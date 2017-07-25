/*********************************************************************/
/*                                                                   */
/*  This Program Written by Paul Edwards, 3:711/934@fidonet.         */
/*  Released to the Public Domain                                    */
/*                                                                   */
/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*  reczm - zmodem receive                                           */
/*  this program is designed to receive a file using zmodem          */
/*                                                                   */
/*********************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include <pdcomm.h>
#include <crcxm.h>
#include <crc32.h>
#include <error.h>

#ifndef MSDOS
#include <os2.h>
#else
#include <dos.h>
#endif


void extModemClearInbound(void);
void extModemResponseSent(void);
void extModemGetBlock(void *buf, size_t max, size_t *actual);
void extModemRegisterBad(void);
void extModemRegisterGood(void);
void extModemGetBlockImm(void *buf, size_t max, size_t *actual);
void extModemWriteBlock(void *buf, size_t max);
void extFileSetInfo(unsigned char *filename, 
                    unsigned char *fileinfo,
                    long *offset,
                    int *skip);
void extFileWriteData(void *buf, size_t bytes);
void extFileFinish(void);


#define ZPAD 0x2a
#define ZDLE 0x18
#define ZBIN 0x41
#define ZHEX 0x42
#define ZBIN32 0x43
#define ZRQINIT 0x00
#define ZRINIT 0x01
#define CANFDX 0x01
#define CANOVIO 0x02
#define CANFC32 0x20
#define ZFILE 0x04
#define ZSKIP 0x05
#define ZCRCW 0x6b
#define ZRPOS 0x09
#define ZDATA 0x0a
#define ZCRCG 0x69
#define ZEOF 0x0b
#define ZFIN 0x08

static unsigned char hexdigit[] = 
    "\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x61\x62\x63\x64\x65\x66";

void receiveZmodem(void);
static void receiveFile(void);
static void receiveData(void);
static int posMatch(void);
static void getData(void);
static void getData16(void);
static void getData32(void);
static void getFileData(void);
static void getZMHeader(void);
static void getHexHeader(void);
static void getBinaryHeader(void);
static void getBin32Header(void);
static void getNextHexCh(void);
static void getNextDLECh(void);
static void getNextCh(void);
static void sendHeader(void);
static void sendHexHeader(void);
static void sendBinHeader(void);
static void sendBin32Header(void);
static void sendChar(void);
static void sendDLEChar(void);
static void sendHexChar(void);
static void sendZRINIT(void);
static void sendZSKIP(void);
static void sendZRPOS(void);
static void sendZFIN(void);
static void getOO(void);

#define ZMODEM_INIT "ZMD001 Remote didn't accept my ZRINIT\n"
#define ZMODEM_POS "ZMD002 Remote failed to respond to my ZRPOS\n"
#define ZMODEM_ZDATA "ZMD003 Remote failed to sync to correct position\n"
#define ZMODEM_CRCXM "ZMD004 CRCXM error, ours = %x, theirs = %x\n"
#define ZMODEM_LONGSP "ZMD005 Error - subpacket too long\n"
#define ZMODEM_CRC32 "ZMD006 CRC32 error - ours = %lx, theirs = %lx\n"
#define ZMODEM_FILEDATA "ZMD007 File Data is out of spec\n"            
#define ZMODEM_BADHEX "ZMD008 Bad hex character %x received\n"
#define ZMODEM_TIMEOUT "ZMD009 Timeout\n"

#define MAXBUF 18000

static PDCOMM pdcomm;
static FILE *fq;

static unsigned char frameType;
static unsigned char headerType;
static unsigned char headerData[4];
static unsigned char recBuf[MAXBUF];
static unsigned char *bufPos;
static unsigned char *bufTop;
static int ch;
static int moreData;
static int gotSpecial;
static long goodOffset;
static unsigned char *fileName;
static unsigned char *fileInfo;
static int skip;
static int gotHeader;

int main(void)
{
    errorDefaults();
    errorInit();
    if (ALLOK)
    {
        pdcommInit(&pdcomm);
#ifndef MSDOS        
        pdcommOpen(&pdcomm, "COM3", 19200L);
#else
        pdcommBuffers(&pdcomm, 2000, 2000);
        pdcommOpenPort(&pdcomm, 0x3e8, 9); /* io port + irq !!!!!! */
        pdcommSetParms(&pdcomm, 19200L, PDCOMM_NO_PARITY, 8, 1);
#endif        
        if (ALLOK)
        {
            receiveZmodem();
        }
#ifndef MSDOS        
        pdcommClose(&pdcomm);
#else
        pdcommClosePort(&pdcomm);
#endif                
        pdcommTerm(&pdcomm);
    }
    errorTerm();
    return (0);
}

void receiveZmodem(void)
{
    int fin, quit, tries;

    bufTop = recBuf + sizeof recBuf;    
    extModemClearInbound();   
    skip = 0;
    fin = 0;
    while (ALLOK && !fin)
    {
        sendZRINIT();           
        quit = 0;
        tries = 0;
        while (ALLOK && !quit)
        {
            getZMHeader();
            if (ALLOK)
            {
                if (headerType == ZFIN)
                {
                    sendZFIN();
                    extModemRegisterGood();
                    getOO();
                    quit = 1;
                    fin = 1;
                }
                else if (headerType == ZRQINIT)
                {    
                    if (tries < 10)
                    {
                        sendZRINIT();
                        tries++;
                    }
                    else
                    {
                        errorSet(ZMODEM_INIT);
                    }
                }
                else if (headerType == ZFILE)
                {
                    skip = 0;
                    extModemRegisterGood();
                    getFileData();
                    if (ALLOK)
                    {
                        receiveFile();
                        quit = 1;
                    }
                }
            }
            else
            {
                if (errorCompare(ZMODEM_TIMEOUT)
                    || errorCompare(ZMODEM_CRCXM)
                    || errorCompare(ZMODEM_CRC32))
                {
                    if (tries < 10)
                    {
                        errorFlush();
                        sendZRINIT();
                        tries++;
                    }
                }
            }
        }
    }
    return;
}

static void receiveFile(void)
{
    int quit;
    int tries;
    
    goodOffset = 0;
    extFileSetInfo(fileName, fileInfo, &goodOffset, &skip);
    if (skip)
    {
        sendZSKIP();
    }    
    else
    {
        sendZRPOS();
        quit = 0;
        tries = 0;
        while (ALLOK && !quit)
        {
            getZMHeader();
            if (ALLOK)
            {
                if (headerType == ZFILE)
                {
                    if (tries < 10)
                    {
                        sendZRPOS();
                        tries++;
                    }
                    else
                    {
                        errorSet(ZMODEM_POS);
                    }
                }
                else if (headerType == ZDATA)
                {
                    extModemRegisterGood();
                    receiveData();
                    quit = 1;
                }
            }
            else
            {
                if (errorCompare(ZMODEM_TIMEOUT) 
                    || errorCompare(ZMODEM_CRCXM)
                    || errorCompare(ZMODEM_CRC32))
                {
                    if (tries < 10)
                    {
                        errorFlush();
                        sendZRPOS();
                        tries++;
                    }
                }
            } 
        }
    }
    return;
}
    
static void receiveData(void)
{
    int quit;
    int tries;
    
    quit = 0;
    tries = 0;
    moreData = 1;
    while (ALLOK && !quit)
    {
        if (moreData)
        {
            getData();
            if (ALLOK)
            {
                extFileWriteData(recBuf, (size_t)(bufPos - recBuf));
                tries = 0;
            }
        }
        else
        {
            getZMHeader();
            if (ALLOK)
            {
                if (headerType == ZDATA)
                {
                    if (posMatch())
                    {
                        extModemRegisterGood();
                        moreData = 1;
                    }
                }
                else if (headerType == ZEOF)
                {
                    if (posMatch())
                    {
                        extModemRegisterGood();
                        extFileFinish();
                        quit = 1;
                    }
                }
            }
        }
        if (!ALLOK)
        {
            if (errorCompare(ZMODEM_TIMEOUT)
                || errorCompare(ZMODEM_LONGSP)
                || errorCompare(ZMODEM_CRCXM)
                || errorCompare(ZMODEM_CRC32))
            {
                if (tries < 10)
                {
                    errorFlush();
                    sendZRPOS();
                    tries++;
                }
            }
        }
    }
    return;
}

static int posMatch(void)
{
    long templ;
    int ret;
    
    templ = headerData[3];
    templ = (templ << 8) | headerData[2];
    templ = (templ << 8) | headerData[1];
    templ = (templ << 8) | headerData[0];
    if (templ == goodOffset)
    {
        ret = 1;
    }
    else
    {
        ret = 0;
    }
    return (ret);
}

static void getData(void)
{
    if (frameType == ZBIN32)
    {
        getData32();
    }
    else
    {
        getData16();
    }
    if (!ALLOK)
    {
        moreData = 0;
    }
    else
    {
        extModemRegisterGood();
    }
    return;
}

static void getData16(void)
{
    int quit;
    CRCXM crc;
    unsigned int theirCRC;
    
    bufPos = recBuf;
    quit = 0;
    crcxmInit(&crc);
    while (ALLOK && (bufPos < bufTop) && !quit)
    {
        getNextDLECh();
        if (ALLOK)
        {
            if (gotSpecial)
            {
                if (ch != ZCRCG)
                {
                    moreData = 0;
                }
                crcxmUpdate(&crc, ch);
                getNextDLECh();
                if (ALLOK)
                {
                    theirCRC = ch;
                    getNextDLECh();
                }
                if (ALLOK)
                {
                    theirCRC = (theirCRC << 8) | ch;
                    if (crcxmValue(&crc) != theirCRC)
                    {
                        errorSet(ZMODEM_CRCXM, 
                                 crcxmValue(&crc), 
                                 theirCRC);
                    }
                    else
                    {
                        goodOffset += (bufPos - recBuf);
                        quit = 1;
                    }
                }
            }
            else
            {
                crcxmUpdate(&crc, ch);
                *bufPos = (unsigned char)ch;
                bufPos++;
            }
        }
    }
    if (bufPos == bufTop)
    {
        errorSet(ZMODEM_LONGSP);
    }
    return;
}

static void getData32(void)
{
    int quit;
    CRC32 crc;
    unsigned long theirCRC;
    
    bufPos = recBuf;
    quit = 0;
    crc32Init(&crc);
    while (ALLOK && (bufPos < bufTop) && !quit)
    {
        getNextDLECh();
        if (ALLOK)
        {
            if (gotSpecial)
            {
                if (ch != ZCRCG)
                {
                    moreData = 0;
                }
                crc32Update(&crc, ch);
                getNextDLECh();
                if (ALLOK)
                {
                    theirCRC = ch;
                    getNextDLECh();
                }
                if (ALLOK)
                {
                    theirCRC = theirCRC | ((unsigned long)ch << 8);
                    getNextDLECh();
                }
                if (ALLOK)
                {
                    theirCRC = theirCRC | ((unsigned long)ch << 16);
                    getNextDLECh();
                }
                if (ALLOK)
                {
                    theirCRC = theirCRC | ((unsigned long)ch << 24);
                    if (~crc32Value(&crc) != theirCRC)
                    {
                        errorSet(ZMODEM_CRC32,
                                 ~crc32Value(&crc), 
                                 theirCRC);
                    }
                    else
                    {
                        goodOffset += (bufPos - recBuf);
                        quit = 1;
                    }
                }
            }
            else
            {
                crc32Update(&crc, ch);
                *bufPos = (unsigned char)ch;
                bufPos++;
            }
        }
    }
    if (bufPos == bufTop)
    {
        errorSet(ZMODEM_LONGSP);
    }
    return;
}

static void getFileData(void)
{
    unsigned char *pos;
    int problem;
    
    getData();
    if (ALLOK)
    {
        problem = 1;
        fileName = recBuf;
        pos = memchr(recBuf, '\0', sizeof recBuf - 1);
        if (pos != NULL)
        {
            pos++;
            fileInfo = pos;
            pos = memchr(pos, '\0', sizeof recBuf - (pos - recBuf));
            if (pos != NULL)
            {
                if ((pos - recBuf) < 1024)
                {
                    problem = 0;
                }
            }        
        }
        if (problem)
        {
            errorSet(ZMODEM_FILEDATA);
        }
    }
    return;
}

static void getZMHeader(void)
{
    int count;
    
    gotHeader = 0;
    getNextCh();
    while (ALLOK && !gotHeader)
    {
        while (ALLOK && (ch != ZPAD))
        {
            extModemRegisterBad();
            getNextCh();
        }
        if (ALLOK)
        {
            count = 1;
            getNextCh();
            while (ALLOK && (ch == ZPAD))
            {
                count++;
                if (count > 2)
                {
                    extModemRegisterBad();
                }
                getNextCh(); 
            }
            if (ALLOK && (ch == ZDLE))
            {
                getNextCh();
                if (ALLOK)
                {
                    if (ch == ZBIN)
                    {
                        frameType = ZBIN;
                        getBinaryHeader();
                    }
                    else if (ch == ZBIN32)
                    {
                        frameType = ZBIN32;
                        getBin32Header();
                    }
                    else if ((ch == ZHEX) && (count >= 2))
                    {
                        frameType = ZHEX;
                        getHexHeader();
                    }
                }
            }
        }
    }
    if (gotHeader)
    {
        printf("received HeaderType %x\n", headerType);
    }
    return;
}

static void getHexHeader(void)
{
    CRCXM crc;
    unsigned int theirCRC;
    
    getNextHexCh();
    while (ALLOK && !gotHeader)
    {
        crcxmInit(&crc);
        headerType = (unsigned char)ch;
        crcxmUpdate(&crc, ch);
        getNextHexCh();
        if (ALLOK)
        {
            headerData[0] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextHexCh();
        }
        if (ALLOK)
        {
            headerData[1] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextHexCh();
        }
        if (ALLOK)
        {
            headerData[2] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextHexCh();
        }
        if (ALLOK)
        {
            headerData[3] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextHexCh();
        }
        if (ALLOK)
        {
            theirCRC = ch;
            getNextHexCh();
        }
        if (ALLOK)
        {
            theirCRC = (theirCRC << 8) | ch;
            if (crcxmValue(&crc) != theirCRC)
            {
                errorSet(ZMODEM_CRCXM, crcxmValue(&crc), theirCRC);
            }
            else
            {
                gotHeader = 1;
            }
        }
    }
    return;
}

static void getBinaryHeader(void)
{
    CRCXM crc;
    unsigned int theirCRC;

    getNextDLECh();
    while (ALLOK && !gotHeader)
    {
        crcxmInit(&crc);
        headerType = (unsigned char)ch;
        crcxmUpdate(&crc, ch);
        getNextDLECh();
        if (ALLOK)
        {
            headerData[0] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            headerData[1] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            headerData[2] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            headerData[3] = (unsigned char)ch;
            crcxmUpdate(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            theirCRC = ch;
            getNextDLECh();
        }
        if (ALLOK)
        {
            theirCRC = (theirCRC << 8) | ch;
            if (crcxmValue(&crc) != theirCRC)
            {
                errorSet(ZMODEM_CRCXM, crcxmValue(&crc), theirCRC);
            }
            else
            {
                gotHeader = 1;
            }
        }
    }
    return;
}

static void getBin32Header(void)
{
    CRC32 crc;
    unsigned long theirCRC;
    
    getNextDLECh();
    while (ALLOK && !gotHeader)
    {
        crc32Init(&crc);
        headerType = (unsigned char)ch;
        crc32Update(&crc, ch);
        getNextDLECh();
        if (ALLOK)
        {
            headerData[0] = (unsigned char)ch;
            crc32Update(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            headerData[1] = (unsigned char)ch;
            crc32Update(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            headerData[2] = (unsigned char)ch;
            crc32Update(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            headerData[3] = (unsigned char)ch;
            crc32Update(&crc, ch);
            getNextDLECh();
        }
        if (ALLOK)
        {
            theirCRC = (unsigned long)ch;
            getNextDLECh();
        }
        if (ALLOK)
        {
            theirCRC = theirCRC | ((unsigned long)ch << 8);
            getNextDLECh();
        }
        if (ALLOK)
        {
            theirCRC = theirCRC | ((unsigned long)ch << 16);
            getNextDLECh();
        }
        if (ALLOK)
        {
            theirCRC = theirCRC | ((unsigned long)ch << 24);
            if (~crc32Value(&crc) != theirCRC)
            {
                errorSet(ZMODEM_CRC32, ~crc32Value(&crc), theirCRC);
            }
            else
            {
                gotHeader = 1;
            }
        }
    }
    return;
}

static void getNextHexCh(void)
{
    int tempCh;
    
    getNextCh();
    if (ALLOK)
    { 
        if ((ch <= 0x39) && (ch >= 0x30))
        {
            tempCh = (ch - 0x30);
        }
        else if ((ch >= 0x61) && (ch <= 0x66))
        {
            tempCh = (ch - 0x61) + 0x0a;
        }
        else 
        {
            errorSet(ZMODEM_BADHEX, ch);
        }
        if (ALLOK)
        {
            getNextCh();
        }
        if (ALLOK)
        {
            tempCh = tempCh << 4;
            if ((ch <= 0x39) && (ch >= 0x30))
            {
                ch = (ch - 0x30);
            }
            else if ((ch >= 0x61) && (ch <= 0x66))
            {
                ch = (ch - 0x61) + 0x0a;
            }
            else
            {
                errorSet(ZMODEM_BADHEX, ch);
            }
        }
        if (ALLOK)
        {
            ch = ch | tempCh;
        }
    }
    return;
}

static void getNextDLECh(void)
{
    gotSpecial = 0;
    getNextCh();
    if (ALLOK)
    {
        if (ch == ZDLE)
        {
            getNextCh();
            if (ALLOK)
            {
                if (((ch & 0x40) != 0) && ((ch & 0x20) == 0))
                {
                    ch &= 0xbf;
                    gotSpecial = 0;
                }
                else
                {
                    gotSpecial = 1;
                }
            }
        }
    }
    return;
}

static void getNextCh(void)
{
    unsigned char buf[1];
    size_t actual;
    
    extModemGetBlock(buf, 1, &actual);
    if (ALLOK && (actual == 1))
    {
        ch = buf[0];
    }
    return;
}

static void sendHeader(void)
{
    printf("sending header %x\n", headerType);
    if (frameType == ZHEX)
    {
        sendHexHeader();
    }
    else if (frameType == ZBIN)
    {
        sendBinHeader();
    }
    else if (frameType == ZBIN32)
    {
        sendBin32Header();
    }
    if (ALLOK)
    {
        extModemResponseSent();
    }
    return;
}

static void sendHexHeader(void)
{
    CRCXM crc;

    crcxmInit(&crc);    
    ch = ZPAD;
    sendChar();
    if (ALLOK)
    {
        sendChar();
    }
    if (ALLOK)
    {
        ch = ZDLE;
        sendChar();
    }
    if (ALLOK)
    {
        ch = frameType;
        sendChar();
    }
    if (ALLOK)
    {
        ch = headerType;
        crcxmUpdate(&crc, ch);
        sendHexChar();
    }
    if (ALLOK)
    {
        ch = headerData[0];
        crcxmUpdate(&crc, ch);
        sendHexChar();
    }
    if (ALLOK)
    {
        ch = headerData[1];
        crcxmUpdate(&crc, ch);
        sendHexChar();
    }
    if (ALLOK)
    {
        ch = headerData[2];
        crcxmUpdate(&crc, ch);
        sendHexChar();
    }
    if (ALLOK)
    {
        ch = headerData[3];
        crcxmUpdate(&crc, ch);
        sendHexChar();
    }
    if (ALLOK)
    {
        ch = crcxmHighbyte(&crc);
        sendHexChar();
    }
    if (ALLOK)
    {
        ch = crcxmLowbyte(&crc);
        sendHexChar();
    }
    if (ALLOK)
    {
        ch = 0x0d;
        sendChar();
    }
    if (ALLOK)
    {
        ch = 0x0a;
        sendChar();
    }
    if (ALLOK)
    {
        ch = 0x11;
        sendChar();
    }
    return;
}    

static void sendBinHeader(void)
{
    CRCXM crc;

    crcxmInit(&crc);    
    ch = ZPAD;
    sendChar();
    if (ALLOK)
    {
        ch = ZDLE;
        sendChar();
    }
    if (ALLOK)
    {
        ch = frameType;
        sendChar();
    }
    if (ALLOK)
    {
        ch = headerType;
        crcxmUpdate(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[0];
        crcxmUpdate(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[1];
        crcxmUpdate(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[2];
        crcxmUpdate(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[3];
        crcxmUpdate(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = crcxmHighbyte(&crc);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = crcxmLowbyte(&crc);
        sendDLEChar();
    }
    return;
}    

static void sendBin32Header(void)
{
    CRC32 crc;

    crc32Init(&crc);    
    ch = ZPAD;
    sendChar();
    if (ALLOK)
    {
        ch = ZDLE;
        sendChar();
    }
    if (ALLOK)
    {
        ch = frameType;
        sendChar();
    }
    if (ALLOK)
    {
        ch = headerType;
        crc32Update(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[0];
        crc32Update(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[1];
        crc32Update(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[2];
        crc32Update(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = headerData[3];
        crc32Update(&crc, ch);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = (unsigned char)crc32Byte1(&crc);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = (unsigned char)crc32Byte2(&crc);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = (unsigned char)crc32Byte3(&crc);
        sendDLEChar();
    }
    if (ALLOK)
    {
        ch = (unsigned char)crc32Byte4(&crc);
        sendDLEChar();
    }
    return;
}    

static void sendChar(void)
{
    unsigned char buf[1];
    
    buf[0] = (unsigned char)ch;
    extModemWriteBlock(buf, 1);
    return;
}

static void sendDLEChar(void)
{
    int tempCh;
    
    if ((ch == ZDLE) || (ch == 0x10) || (ch == 0x11) || (ch == 0x13)
        || (ch == 0x50) || (ch == 0x51) || (ch == 0x53))
    {
        tempCh = ch;
        ch = ZDLE;
        sendChar();
        if (ALLOK)
        {
            ch = tempCh;
            ch |= 0x40;
            sendChar();
        }
    }
    else
    {
        sendChar();
    }
    return;
}

static void sendHexChar(void)
{
    int tempCh;
    
    tempCh = ch;
    ch = hexdigit[(tempCh >> 4) & 0x0f];
    sendChar();
    if (ALLOK)
    {
        ch = hexdigit[tempCh & 0x0f];
        sendChar();
    }
    return;
}
                         
static void sendZRINIT(void)
{
    frameType = ZHEX;
    headerType = ZRINIT;
    headerData[0] = 0x00;
    headerData[1] = 0x00;
    headerData[2] = 0x00;
    headerData[3] = CANOVIO | CANFDX | CANFC32; 
    sendHeader();
    return;
}

static void sendZSKIP(void)
{
    frameType = ZHEX;
    headerType = ZSKIP;
    headerData[0] = 0x00;
    headerData[1] = 0x00;
    headerData[2] = 0x00;
    headerData[3] = 0x00;
    sendHeader();
    return;
}

static void sendZRPOS(void)
{
    long templ;
    
    frameType = ZHEX;
    headerType = ZRPOS;
    templ = goodOffset;
    headerData[0] = (unsigned char)(templ & 0xff);
    templ = templ >> 8;
    headerData[1] = (unsigned char)(templ & 0xff);
    templ = templ >> 8;
    headerData[2] = (unsigned char)(templ & 0xff);
    templ = templ >> 8;
    headerData[3] = (unsigned char)(templ & 0xff);
    extModemClearInbound();
    sendHeader();
    return;
}

static void sendZFIN(void)
{
    frameType = ZHEX;
    headerType = ZFIN;
    headerData[0] = 0x00;
    headerData[1] = 0x00;
    headerData[2] = 0x00;
    headerData[3] = 0x00;
    sendHeader();
    return;
}

static void getOO(void)
{
    getNextCh();
    if (ALLOK)
    {
        getNextCh();
    }
    if (!ALLOK && errorCompare(ZMODEM_TIMEOUT))
    {
        errorClear();
    }
    return;
}

void extModemClearInbound(void)
{
    unsigned char buf[200];
    
    while (pdcommRecBuf(&pdcomm, buf, sizeof buf) == sizeof buf) ;
    return;
}

void extModemResponseSent(void)
{
    return;
}

void extModemGetBlock(void *buf, size_t max, size_t *actual)
{
    int x;
    
    x = pdcommRecBuf(&pdcomm, buf, max);
    if (x == 0)
    {
#ifndef MSDOS    
        DosSleep(1000);
#else
        sleep(1);
#endif                
        x = pdcommRecBuf(&pdcomm, buf, max);
    }
    if (x == 0)
    {
        errorSet(ZMODEM_TIMEOUT);
    }
    *actual = x;
    return;
}

void extModemRegisterBad(void)
{
    return;
}

void extModemRegisterGood(void)
{
    return;
}

void extModemGetBlockImm(void *buf, size_t max, size_t *actual)
{
    int x;
    
    x = pdcommRecBuf(&pdcomm, buf, max);
    *actual = x;
    return;
}

void extModemWriteBlock(void *buf, size_t max)
{
    pdcommTxBuf(&pdcomm, buf, max);
    return;
}
   
void extFileSetInfo(unsigned char *filename, 
                    unsigned char *fileinfo,
                    long *offset,
                    int *skip)
{
    (void)fileinfo;
    printf("opening file %s\n", filename);
    fq = fopen((char *)filename, "wb");
    if (fq == NULL)
    {
        printf("failed to open file %s\n", filename);
    }
    *offset = 0;
    *skip = 0;
    return;
}

void extFileWriteData(void *buf, size_t bytes)
{
    printf("goodOffset is %ld\n", goodOffset);
    fwrite(buf, bytes, 1, fq);
    return;
}

void extFileFinish(void)
{
    printf("closing file\n");
    fclose(fq);
    return;
}
