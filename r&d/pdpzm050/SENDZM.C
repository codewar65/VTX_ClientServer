/*********************************************************************/
/*                                                                   */
/*  This Program Written by Paul Edwards, 3:711/934@fidonet.         */
/*  Released to the Public Domain                                    */
/*                                                                   */
/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*  newzm - zmodem send                                              */
/*  this program is designed to send a file using zmodem             */
/*                                                                   */
/*********************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include <pdcomm.h>
#include <crcxm.h>
#include <m2x.h>

#include <os2.h>

#define ZPAD 0x2a
#define ZDLE 0x18
#define ZBIN 0x41
#define ZHEX 0x42
#define ZBIN32 0x43
#define ZRINIT 0x01
#define CANFDX 0x01
#define CANOVIO 0x02
#define ZFILE 0x04
#define ZCRCW 0x6b
#define ZDATA 0x0a
#define ZCRCE 0x68
#define ZCRCG 0x69
#define ZEOF 0x0b
#define ZFIN 0x08
#define ZCBIN 0x01

static void sendZmodem(void);
static void sendrz(void);
static void sendZRQINIT(void);
static void getZRINIT(void);
static void sendZFILE(void);
static void sendFILEINFO(void);
static void getZRPOS(void);
static void sendZDATA(void);
static void sendDATA(void);
static void sendZEOF(void);
static void sendZFIN(void);
static void getZFIN(void);
static void sendOO(void);
static void listen(void);

PDCOMM pdcomm;

int main(void)
{
    pdcommInit(&pdcomm);
    pdcommOpen(&pdcomm, "COM2", 19200L);
    sendZmodem();
    pdcommClose(&pdcomm);
    pdcommTerm(&pdcomm);
    return (0);
}

static void sendZmodem(void)
{
    sendrz();
    sendZRQINIT();
    getZRINIT();
    sendZFILE();
    sendFILEINFO();
    getZRPOS();
    sendZDATA();
    sendDATA();
    sendZEOF();
    getZRINIT();
    sendZFIN();
    getZFIN();
    sendOO();
    return;
}

static void sendrz(void)
{
    printf("sending %s\n", m2x("\x72\x7a\x0d", 3));
    pdcommTxBuf(&pdcomm, (unsigned char *)"\x72\x7a\x0d", 3); /* rz\r */
    return;
}

#define ZRQRINIT_STR "\x2a\x2a\x18\x42" \
                 "\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30" \
                 "\x0d\x0a\x11"
                 
static void sendZRQINIT(void)
{
    printf("sending %s\n", m2x(ZRQRINIT_STR, sizeof ZRQRINIT_STR - 1));
    pdcommTxBuf(&pdcomm, 
                (unsigned char *)ZRQRINIT_STR, 
                sizeof ZRQRINIT_STR - 1);
    return;
}

static void getZRINIT(void)
{
    /* while (pdcommRecCh(&pdcomm) != '\x11') ; */
    DosSleep(3000);
    listen();
}

static void sendZFILE(void)
{
    unsigned char buf[500];
    int cnt = 0;
    crcxm crc;
    int x;
    
    buf[cnt++] = ZPAD;
    buf[cnt++] = ZDLE;
    buf[cnt++] = ZBIN;
    buf[cnt++] = ZFILE;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    buf[cnt++] = ZCBIN;
    crcxm_init(crc);
    for (x = 3; x < 8; x++)
    {
        crcxm_update(crc, buf[x]);
    }
    buf[cnt++] = (unsigned char)crcxm_highbyte(crc);
    buf[cnt++] = (unsigned char)crcxm_lowbyte(crc);
    printf("sending %s\n", m2x(buf, cnt));
    pdcommTxBuf(&pdcomm, buf, cnt);
    return;
}

static void sendFILEINFO(void)
{
    unsigned char buf[500];
    int cnt = 0;
    int x;
    crcxm crc;

    sprintf((char *)buf, "temp.txt");
    cnt = strlen((char *)buf) + 1;
/*    sprintf((char *)buf + cnt, "4 5717451676 0"); */
    sprintf((char *)buf + cnt, "3 0 0 0"); 
    cnt = cnt + strlen((char *)buf + cnt) + 1;
    crcxm_init(crc);
    for (x = 0; x < cnt; x++)
    {
        crcxm_update(crc, buf[x]);
    }
    buf[cnt++] = ZDLE;
    buf[cnt++] = ZCRCW;
    crcxm_update(crc, ZCRCW);
    buf[cnt++] = (unsigned char)crcxm_highbyte(crc);
    buf[cnt++] = (unsigned char)crcxm_lowbyte(crc);
    buf[cnt++] = 0x11;
    printf("sending %s\n", m2x(buf, cnt));
    pdcommTxBuf(&pdcomm, buf, cnt);
    return;
}

static void getZRPOS(void)
{
    /* while (pdcommRecCh(&pdcomm) != '\x11') ; */
    DosSleep(3000);
    listen();
    return;
}

static void sendZDATA(void)
{
    unsigned char buf[500];
    int cnt = 0;
    crcxm crc;
    int x;
    
    buf[cnt++] = ZPAD;
    buf[cnt++] = ZDLE;
    buf[cnt++] = ZBIN;
    buf[cnt++] = ZDATA;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    crcxm_init(crc);
    for (x = 3; x < 8; x++)
    {
        crcxm_update(crc, buf[x]);
    }
    buf[cnt++] = (unsigned char)crcxm_highbyte(crc);
    buf[cnt++] = (unsigned char)crcxm_lowbyte(crc);
    printf("sending %s\n", m2x(buf, cnt));
    pdcommTxBuf(&pdcomm, buf, cnt);
    return;
}

static void sendDATA(void)
{
    unsigned char buf[500];
    int cnt = 0;
    int x;
    crcxm crc;
    
    buf[cnt++] = 'A';
    buf[cnt++] = 'B';
    buf[cnt++] = 'C';
    buf[cnt++] = ZDLE;
    buf[cnt++] = ZCRCE;  /* ZCRCG if more data */
    crcxm_init(crc);
    for (x = 0; x < 3; x++)
    {
        crcxm_update(crc, buf[x]);
    }
    crcxm_update(crc, ZCRCE);
    buf[cnt++] = (unsigned char)crcxm_highbyte(crc);
    buf[cnt++] = (unsigned char)crcxm_lowbyte(crc);
    printf("sending %s\n", m2x(buf, cnt));
    pdcommTxBuf(&pdcomm, buf, cnt);
    return;
}

static void sendZEOF(void)
{
    unsigned char buf[500];
    int cnt = 0;
    crcxm crc;
    int x;
    
    buf[cnt++] = ZPAD;
    buf[cnt++] = ZDLE;
    buf[cnt++] = ZBIN;
    buf[cnt++] = ZEOF;
    buf[cnt++] = 0x03;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    crcxm_init(crc);
    for (x = 3; x < 8; x++)
    {
        crcxm_update(crc, buf[x]);
    }
    buf[cnt++] = (unsigned char)crcxm_highbyte(crc);
    buf[cnt++] = (unsigned char)crcxm_lowbyte(crc);
    printf("sending %s\n", m2x(buf, cnt));
    pdcommTxBuf(&pdcomm, buf, cnt);
    return;
}

static void sendZFIN(void)
{
    unsigned char buf[500];
    int cnt = 0;
    crcxm crc;
    int x;
    
    buf[cnt++] = ZPAD;
    buf[cnt++] = ZDLE;
    buf[cnt++] = ZBIN;
    buf[cnt++] = ZFIN;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    buf[cnt++] = 0x00;
    crcxm_init(crc);
    for (x = 3; x < 8; x++)
    {
        crcxm_update(crc, buf[x]);
    }
    buf[cnt++] = (unsigned char)crcxm_highbyte(crc);
    buf[cnt++] = (unsigned char)crcxm_lowbyte(crc);
    printf("sending %s\n", m2x(buf, cnt));
    pdcommTxBuf(&pdcomm, buf, cnt);
    return;
}

static void getZFIN(void)
{
    DosSleep(3000);
    return;
}

static void sendOO(void)
{
    printf("sending %s\n", m2x("OO", 2));
    pdcommTxBuf(&pdcomm, (unsigned char *)"OO", 2);
    return;
}

static void listen(void)
{
    unsigned char buf[5000];
    int cnt;
    
    cnt = pdcommRecBuf(&pdcomm, buf, 5000);
    printf("receive %s\n", m2x(buf, cnt));
    return;
}
