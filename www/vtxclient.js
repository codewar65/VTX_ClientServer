/*

  Copyright (c) 2017, Daniel Mecklenburg Jr.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  VTX Server
  2017-07-20
  vtxserv.pas

    TODO : ( see also TODO in code)

        On Demand Fonts - allow normal font select, but onload of new font,
            force redraw cells with the new font.
    
        Client Ident - fix - currently crashing / not working all time.

        Codes for restore cursor attr, restore page attr

        ATASCII

        Server: http requests out to separate threads

        YModem download : remove CPMEOF's on files w/o filenames/sizes
        

    HOTSPOTATTR

        00000000 00000000 00000000 00000000
        BBBBBBBB FFFFFFFF bbbbbbbb ffffffff

        f : mouseover foreground color
        b : mouseover background color
        F : click foreground color
        B : click background color

        default = 0x0C0F070B

    PAGEATTR

        bbbbbbbb BBBBBBBB

        b : border color
        B : background color

    ROWATTR - numbers stored in conRowAttr[num]

        00000000 00101100 00000000 00000000 - default
   
        00000000 00000000 00000000 00000000  - bits
        ------DD mwwzzzbb ssssssss ffffffff
        00000000 00101100 00000000 00000000
           00       2C       00       00

        f : First Color (0-255)
        s : Second Color (0-255)
        b : Background Pattern
            00 - none
            01 - solid (first color)
            10 - horiz grad (first -> second color)
            11 - vert grad (first -> second color)
        z : height scale
            000 - 25%
            001 - 50%
            010 - 75%
            011 - 100%
            100 - 125%
            101 - 150%
            110 - 175%
            111 - 200%
        w : width scale
            00  - 50%
            01  - 100%
            10  - 150%
            11  - 200%
        m : marquee (0-1)
        D : row display row type
            00 - normal
            01 - concealed
            10 - top half of double height 
            11 - bottom half of double height
        - : unused

    CELLATTRS - numbers stored in conCellAttr[row][col]

        00000000 00000000 00000000 00000000 - bits
        ZZZZosDD fKktuibr BBBBBBBB FFFFFFFF
                 
        F : Foreground Color (0-255) using aixterm palette
        B : Background Color (0-255)  -''-
        r : reversed (0-1)
        b : bold (0-1)
        i : italics (0-1)
        u : underline (0-1)
        t : strikethrough (0-1)
        k : blink slow (0-1)
        K : blink fast (0-1)
        f : faint (0-1)
        D : display (0=normal,1=concealed,2=tophalf,3=bottomhalf)
        s : shadow (0-1)
        o : outlined (0-1)
        Z : font number 0-15 (10=mosaic block, 11=separated block).
     
    CRSRATTRS

        00000000 00000000 00000000 00000000  - bits
        -------- -------- -----ozz cccccccc

        c : color (0-255)
        z : size
            00 : none   0%
            01 : thin  10%
            10 : thick 25%
            11 : full   100%
        o : orientation
            0 : horizontal
            1 : vertical
        - : unused


    ESC / CSI CODES - see vtx.txt

*/
"use strict";

vtx: {
    
// globals
const
    // ansi color lookup table (alteration. color 0=transparent, use 16 for true black`)
    ansiColors = [
        // VGA 0-15 - transparent will switch to #000000 when appropriate
        'transparent',  '#AA0000',      '#00AA00',      '#AA5500',
        '#0000AA',      '#AA00AA',      '#00AAAA',      '#AAAAAA',
        '#555555',      '#FF5555',      '#55FF55',      '#FFFF55',
        '#5555FF',      '#FF55FF',      '#55FFFF',      '#FFFFFF',
        // EXTENDED 16-231
        '#000000', '#00005F', '#000087', '#0000AF', '#0000D7', '#0000FF',
        '#005F00', '#005F5F', '#005F87', '#005FAF', '#005FD7', '#005FFF',
        '#008700', '#00875F', '#008787', '#0087AF', '#0087D7', '#0087FF',
        '#00AF00', '#00AF5F', '#00AF87', '#00AFAF', '#00AFD7', '#00AFFF',
        '#00D700', '#00D75F', '#00D787', '#00D7AF', '#00D7D7', '#00D7FF',
        '#00FF00', '#00FF5F', '#00FF87', '#00FFAF', '#00FFD7', '#00FFFF',
        '#5F0000', '#5F005F', '#5F0087', '#5F00AF', '#5F00D7', '#5F00FF',
        '#5F5F00', '#5F5F5F', '#5F5F87', '#5F5FAF', '#5F5FD7', '#5F5FFF',
        '#5F8700', '#5F875F', '#5F8787', '#5F87AF', '#5F87D7', '#5F87FF',
        '#5FAF00', '#5FAF5F', '#5FAF87', '#5FAFAF', '#5FAFD7', '#5FAFFF',
        '#5FD700', '#5FD75F', '#5FD787', '#5FD7AF', '#5FD7D7', '#5FD7FF',
        '#5FFF00', '#5FFF5F', '#5FFF87', '#5FFFAF', '#5FFFD7', '#5FFFFF',
        '#870000', '#87005F', '#870087', '#8700AF', '#8700D7', '#8700FF',
        '#875F00', '#875F5F', '#875F87', '#875FAF', '#875FD7', '#875FFF',
        '#878700', '#87875F', '#878787', '#8787AF', '#8787D7', '#8787FF',
        '#87AF00', '#87AF5F', '#87AF87', '#87AFAF', '#87AFD7', '#87AFFF',
        '#87D700', '#87D75F', '#87D787', '#87D7AF', '#87D7D7', '#87D7FF',
        '#87FF00', '#87FF5F', '#87FF87', '#87FFAF', '#87FFD7', '#87FFFF',
        '#AF0000', '#AF005F', '#AF0087', '#AF00AF', '#AF00D7', '#AF00FF',
        '#AF5F00', '#AF5F5F', '#AF5F87', '#AF5FAF', '#AF5FD7', '#AF5FFF',
        '#AF8700', '#AF875F', '#AF8787', '#AF87AF', '#AF87D7', '#AF87FF',
        '#AFAF00', '#AFAF5F', '#AFAF87', '#AFAFAF', '#AFAFD7', '#AFAFFF',
        '#AFD700', '#AFD75F', '#AFD787', '#AFD7AF', '#AFD7D7', '#AFD7FF',
        '#AFFF00', '#AFFF5F', '#AFFF87', '#AFFFAF', '#AFFFD7', '#AFFFFF',
        '#D70000', '#D7005F', '#D70087', '#D700AF', '#D700D7', '#D700FF',
        '#D75F00', '#D75F5F', '#D75F87', '#D75FAF', '#D75FD7', '#D75FFF',
        '#D78700', '#D7875F', '#D78787', '#D787AF', '#D787D7', '#D787FF',
        '#D7AF00', '#D7AF5F', '#D7AF87', '#D7AFAF', '#D7AFD7', '#D7AFFF',
        '#D7D700', '#D7D75F', '#D7D787', '#D7D7AF', '#D7D7D7', '#D7D7FF',
        '#D7FF00', '#D7FF5F', '#D7FF87', '#D7FFAF', '#D7FFD7', '#D7FFFF',
        '#FF0000', '#FF005F', '#FF0087', '#FF00AF', '#FF00D7', '#FF00FF',
        '#FF5F00', '#FF5F5F', '#FF5F87', '#FF5FAF', '#FF5FD7', '#FF5FFF',
        '#FF8700', '#FF875F', '#FF8787', '#FF87AF', '#FF87D7', '#FF87FF',
        '#FFAF00', '#FFAF5F', '#FFAF87', '#FFAFAF', '#FFAFD7', '#FFAFFF',
        '#FFD700', '#FFD75F', '#FFD787', '#FFD7AF', '#FFD7D7', '#FFD7FF',
        '#FFFF00', '#FFFF5F', '#FFFF87', '#FFFFAF', '#FFFFD7', '#FFFFFF',
        // GRAYS 232-255
        '#080808', '#121212', '#1C1C1C', '#262626', '#303030', '#3A3A3A',
        '#444444', '#4E4E4E', '#585858', '#626262', '#6C6C6C', '#767676',
        '#808080', '#8A8A8A', '#949494', '#9E9E9E', '#A8A8A8', '#B2B2B2',
        '#BCBCBC', '#C6C6C6', '#D0D0D0', '#DADADA', '#E4E4E4', '#EEEEEE'
    ],

    // commodore colors.
    vic20Colors = [ // vic-20 in 22 column mode
        '#000000', '#FFFFFF', '#782922', '#87D6DD', '#AA5FB6', '#55A049',
        '#40318D', '#BFCE72', '#AA7449', '#EAB489', '#B86962', '#C7FFFF',
        '#EA9FF6', '#94E089', '#8071CC', '#FFFFB2', '#000000' // fake black
    ],
    c64Colors = [   // C64/C128 in 40 column mode
        '#000000', '#FFFFFF', '#68372B', '#70A4B2', '#6F3D86', '#588D43',
        '#352879', '#B8C76F', '#6F4F25', '#433900', '#9A6759', '#444444',
        '#6C6C6C', '#9AD284', '#6C5EB5', '#959595', '#000000' // fake black
    ],
    c128Colors = [  // C128 colors in 80 column mode
        '#000000', '#303030', '#EA311B', '#FC601C', '#36C137', '#77EC7C',
        '#1C49D9', '#4487EF', '#BBC238', '#E9F491', '#D974DA', '#EECFED',
        '#68C8C2', '#B2F0EC', '#BECEBC', '#FFFFFF', '#000000' // fake black
    ],

    // atari tty colors.
    atariColors = [
        '#0141A3', '#64ACFF'
    ],
    
    hex =   '0123456789ABCDEF',
    b64 =   'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=',

    // attributes
    A_CELL_FGCOLOR_MASK     = 0x000000FF,
    A_CELL_BGCOLOR_MASK     = 0x0000FF00,
    A_CELL_REVERSE          = 0x00010000,
    A_CELL_BOLD             = 0x00020000,
    A_CELL_ITALICS          = 0x00040000,
    A_CELL_UNDERLINE        = 0x00080000,
    A_CELL_STRIKETHROUGH    = 0x00100000,
    A_CELL_BLINKSLOW        = 0x00200000,
    A_CELL_BLINKFAST        = 0x00400000,
    A_CELL_FAINT            = 0x00800000,
    A_CELL_DISPLAY_MASK     = 0x03000000,
    A_CELL_DISPLAY_NORMAL   = 0x00000000,
    A_CELL_DISPLAY_CONCEAL  = 0x01000000,
    A_CELL_DISPLAY_TOP      = 0x02000000,
    A_CELL_DISPLAY_BOTTOM   = 0x03000000,
    A_CELL_SHADOW           = 0x04000000,
    A_CELL_OUTLINE          = 0x08000000,
    A_CELL_FONT_MASK        = 0xF0000000,

    A_ROW_COLOR1_MASK       = 0x000000FF, // 0-255
    A_ROW_COLOR2_MASK       = 0x0000FF00, // 0-255
    A_ROW_PATTERN_MASK      = 0x00030000, // normal,solid,horz,vert
    A_ROW_PATTERN_NONE      = 0x00000000, // no background
    A_ROW_PATTERN_SOLID     = 0x00010000, // solid color1
    A_ROW_PATTERN_HORZ      = 0x00020000, // horizontal gradient color1 to color2
    A_ROW_PATTERN_VERT      = 0x00030000, // vertical gradient color1 to color2
    A_ROW_HEIGHT_MASK       = 0x001C0000, // 25,50,75,100,125,150,175,200
    A_ROW_HEIGHT_25         = 0x00000000, // 0 00
    A_ROW_HEIGHT_50         = 0x00040000, // 0 01
    A_ROW_HEIGHT_75         = 0x00080000, // 0 10
    A_ROW_HEIGHT_100        = 0x000C0000, // 0 11
    A_ROW_HEIGHT_125        = 0x00100000, // 1 00
    A_ROW_HEIGHT_150        = 0x00140000, // 1 01
    A_ROW_HEIGHT_175        = 0x00180000, // 1 10
    A_ROW_HEIGHT_200        = 0x001C0000, // 1 11
    A_ROW_WIDTH_MASK        = 0x00600000, // 50,100,150,200
    A_ROW_WIDTH_50          = 0x00000000,
    A_ROW_WIDTH_100         = 0x00200000,
    A_ROW_WIDTH_150         = 0x00400000,
    A_ROW_WIDTH_200         = 0x00600000,
    A_ROW_MARQUEE           = 0x00800000,
    A_ROW_DISPLAY_MASK      = 0x03000000, // normal/conceal/top/bottom
    A_ROW_DISPLAY_NORMAL    = 0x00000000,
    A_ROW_DISPLAY_CONCEAL   = 0x01000000,
    A_ROW_DISPLAY_TOP       = 0x02000000,
    A_ROW_DISPLAY_BOTTOM    = 0x03000000,
    
    A_CRSR_COLOR_MASK       = 0x000000FF, // 0-255
    A_CRSR_STYLE_MASK       = 0x00000300, // none,thin,thick,full
    A_CRSR_STYLE_NONE       = 0x00000000,
    A_CRSR_STYLE_THIN       = 0x00000100,
    A_CRSR_STYLE_THICK      = 0x00000200,
    A_CRSR_STYLE_FULL       = 0x00000300,
    A_CRSR_ORIENTATION      = 0x00000400,   // 0 = horz, 1 = vert

    // special key commands
    DO_CAPLK =          -2,
    DO_NUMLK =          -3,
    DO_SCRLK =          -4,

    // terminal states
    TS_OFFLINE =        -1, // not connected
    TS_NORMAL =         0,  // normal terminal mode. no xfers.
    TS_YMR_START =      1,  // ymodem download started. sending G's.
    TS_YMR_GETPACKET =  2,  // ymodem download packet
    TS_YMS_START =      3,  // ymodem send header of file.
    TS_YMS_PUTPACKET =  4,  // ymodem send packet
    TS_YMS_PUTWAIT =    5,  // ymodem wait for C on send

    // bitspersecond / 100 rates for speed emulation.
    bauds =     [ 0,3,6,12,24,48,96,192,384,576,768,1152 ],

    // ASCII C0 Codes
    _NUL     = 0x00,
    _SOH     = 0x01,
    _STX     = 0x02,
    _ETX     = 0x03,
    _EOT     = 0x04,
    _ENQ     = 0x05,
    _ACK     = 0x06,
    _BEL     = 0x07,
    _BS      = 0x08,
    _HT      = 0x09,
    _LF      = 0x0A,
    _VT      = 0x0B,
    _FF      = 0x0C,
    _CR      = 0x0D,
    _SO      = 0x0E,
    _SI      = 0x0F,
    _DLE     = 0x10,
    _DC1     = 0x11,  //   same
    _XON     = 0x11,  // values
    _DC2     = 0x12,
    _DC3     = 0x13,  //   same
    _XOFF    = 0x13,  // values
    _DC4     = 0x14,
    _NAK     = 0x15,
    _SYN     = 0x16,
    _ETB     = 0x17,
    _CAN     = 0x18,
    _EM      = 0x19,
    _SUB     = 0x1A,  //   same
    _CPMEOF  = 0x1A,  // values
    _ESC     = 0x1B,
    _FS      = 0x1C,
    _GS      = 0x1D,
    _RS      = 0x1E,
    _US      = 0x1F,
    _SPACE   = 0x20,
    _C       = 0x43,
    _G       = 0x47,
    _DEL     = 0x7F,
    _BLANK   = 0x0020,  // characters without a glyph (null etc).
    _SHY     = 0x2010,

    // special char codes and sequences
    NUL =       '\x00',
    ESC =       '\x1B',
    CSI =       '\x1B[',
    CR =        '\x0D',
    LF =        '\x0A',
    CRLF =      '\x0D\x0A',

    // tables for converting byte to UTF16 (unicode)
    codePageAKAs = {
        CP790:      'CP667',
        CP991:      'CP667',
        CP1119:     'CP772',
        CP1118:     'CP774',
        CP900:      'CP866',
        CP895:      'CP866',
        CP65001:    'UTF8',
        CP819:      'ISO8859_1',
        CP28593:    'ISO8859_3',
        VIC20:      'RAW',
        C64:        'RAW',
        C128:       'RAW',
        ATASCII:    'RAW'
    },

    // codepage tables.
    // size     desc
    // 255      glyphs for all points (for doorway mode)
    // 128      80-FF (00-7F from ASCII)
    // 96       A0-FF (00-7F from ASCII, 80-9F from CP437)
    codePageData = {
        ASCII: new Uint16Array([
            _BLANK, 0x263A, 0x263B, 0x2665, 0x2666, 0x2663, 0x2660, 0x2022,
            0x25D8, 0x25CB, 0x25D9, 0x2642, 0x2640, 0x266A, 0x266B, 0x263C,
            0x25BA, 0x25C4, 0x2195, 0x203C, 0x00B6, 0x00A7, 0x25AC, 0x21A8,
            0x2191, 0x2193, 0x2192, 0x2190, 0x221F, 0x2194, 0x25B2, 0x25BC,
            0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027,
            0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F,
            0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
            0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
            0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,
            0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F,
            0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057,
            0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F,
            0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067,
            0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F,
            0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077,
            0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x2302]),
        CP437: new Uint16Array([    // CP437
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
            0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, _BLANK]),
        CP667: new Uint16Array([    // CP667, CP790, CP991
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x0105, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x0107, 0x00C4, 0x0104,
            0x0118, 0x0119, 0x0142, 0x00F4, 0x00F6, 0x0106, 0x00FB, 0x00F9,
            0x015A, 0x00D6, 0x00DC, 0x00A2, 0x0141, 0x00A5, 0x015B, 0x0192,
            0x0179, 0x017B, 0x00F3, 0x00D3, 0x0144, 0x0143, 0x017A, 0x017C,
            0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP668: new Uint16Array([    // CP668
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x0107, 0x00E7,
            0x0142, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x0179, 0x00C4, 0x0106,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x015A,
            0x015B, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x0141, 0x00D3, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x0104, 0x0105, 0x017B, 0x017C,
            0x0118, 0x0119, 0x00AC, 0x017A, 0x0143, 0x0144, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP737: new Uint16Array([    // CP737
            0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 0x0398,
            0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F, 0x03A0,
            0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 0x03A8, 0x03A9,
            0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 0x03B8,
            0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF, 0x03C0,
            0x03C1, 0x03C3, 0x03C2, 0x03C4, 0x03C5, 0x03C6, 0x03C7, 0x03C8,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03C9, 0x03AC, 0x03AD, 0x03AE, 0x03CA, 0x03AF, 0x03CC, 0x03CD,
            0x03CB, 0x03CE, 0x0386, 0x0388, 0x0389, 0x038A, 0x038C, 0x038E,
            0x038F, 0x00B1, 0x2265, 0x2264, 0x03AA, 0x03AB, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP770: new Uint16Array([    // CP770
            0x010C, 0x00FC, 0x0117, 0x0101, 0x00E4, 0x0105, 0x013C, 0x010D,
            0x0113, 0x0112, 0x0119, 0x0118, 0x012B, 0x012F, 0x00C4, 0x0104,
            0x0116, 0x017E, 0x017D, 0x00F5, 0x00F6, 0x00D5, 0x016B, 0x0173,
            0x0123, 0x00D6, 0x00DC, 0x00A2, 0x013B, 0x201E, 0x0161, 0x0160,
            0x0100, 0x012A, 0x0137, 0x0136, 0x0146, 0x0145, 0x016A, 0x0172,
            0x0122, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x012E, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP771: new Uint16Array([    // CP771
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x0104, 0x0105, 0x010C, 0x010D,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
            0x0118, 0x0119, 0x0116, 0x0117, 0x012E, 0x012F, 0x0160, 0x0161,
            0x0172, 0x0173, 0x016A, 0x016B, 0x017D, 0x017E, 0x25A0, 0x00A0]),
        CP772: new Uint16Array([ // CP772, CP1119
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x0104, 0x010C, 0x0118,
            0x0116, 0x2563, 0x2551, 0x2557, 0x255D, 0x012E, 0x0160, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x0172, 0x016A,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x017D,
            0x0105, 0x010D, 0x0119, 0x0117, 0x012F, 0x0161, 0x0173, 0x016B,
            0x017E, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
            0x0401, 0x0451, 0x201E, 0x201C, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP773: new Uint16Array([ // CP773
            0x0106, 0x00FC, 0x00E9, 0x0101, 0x00E4, 0x0123, 0x00E5, 0x0107,
            0x0142, 0x0113, 0x0156, 0x0157, 0x012B, 0x0179, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x014D, 0x00F6, 0x0122, 0x00A2, 0x015A,
            0x015B, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x00D7, 0x00A4,
            0x0100, 0x012A, 0x00F3, 0x017B, 0x017C, 0x017A, 0x201D, 0x00A6,
            0x00A9, 0x00AE, 0x00AC, 0x00BD, 0x00BC, 0x0141, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x0104, 0x0105, 0x010C, 0x010D,
            0x00D3, 0x00DF, 0x014C, 0x0143, 0x00F5, 0x00D5, 0x00B5, 0x0144,
            0x0136, 0x0137, 0x013B, 0x013C, 0x0146, 0x0112, 0x0145, 0x2019,
            0x0118, 0x0119, 0x0116, 0x0117, 0x012E, 0x012F, 0x0160, 0x0161,
            0x0172, 0x0173, 0x016A, 0x016B, 0x017D, 0x017E, 0x25A0, 0x00A0]),
        CP774: new Uint16Array([    // CP774, CP1118
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
            0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x0104, 0x010C, 0x0118,
            0x0116, 0x2563, 0x2551, 0x2557, 0x255D, 0x012E, 0x0160, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x0172, 0x016A,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x017D,
            0x0105, 0x010D, 0x0119, 0x0117, 0x012F, 0x0161, 0x0173, 0x016B,
            0x017E, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x201E, 0x201C, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP775: new Uint16Array([    // CP775
            0x0106, 0x00FC, 0x00E9, 0x0101, 0x00E4, 0x0123, 0x00E5, 0x0107,
            0x0142, 0x0113, 0x0156, 0x0157, 0x012B, 0x0179, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x014D, 0x00F6, 0x0122, 0x00A2, 0x015A,
            0x015B, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x00D7, 0x00A4,
            0x0100, 0x012A, 0x00F3, 0x017B, 0x017C, 0x017A, 0x201D, 0x00A6,
            0x00A9, 0x00AE, 0x00AC, 0x00BD, 0x00BC, 0x0141, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x0104, 0x010C, 0x0118,
            0x0116, 0x2563, 0x2551, 0x2557, 0x255D, 0x012E, 0x0160, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x0172, 0x016A,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x017D,
            0x0105, 0x010D, 0x0119, 0x0117, 0x012F, 0x0161, 0x0173, 0x016B,
            0x017E, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x00D3, 0x00DF, 0x014C, 0x0143, 0x00F5, 0x00D5, 0x00B5, 0x0144,
            0x0136, 0x0137, 0x013B, 0x013C, 0x0146, 0x0112, 0x0145, 0x2019,
            _SHY  , 0x00B1, 0x201C, 0x00BE, 0x00B6, 0x00A7, 0x00F7, 0x201E,
            0x00B0, 0x2219, 0x00B7, 0x00B9, 0x00B3, 0x00B2, 0x25A0, 0x00A0]),
        CP808: new Uint16Array([    // CP808
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
            0x0401, 0x0451, 0x0404, 0x0454, 0x0407, 0x0457, 0x040E, 0x045E,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x2116, 0x20AC, 0x25A0, 0x00A0]),
        CP813: new Uint16Array([    // CP813
            0x00A0, 0x2018, 0x2019, 0x00A3, 0x20AC, _BLANK, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, _BLANK, 0x00AB, 0x00AC, _SHY  , _BLANK, 0x2015,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x0385, 0x0386, 0x0387,
            0x0388, 0x0389, 0x038A, 0x00BB, 0x038C, 0x00BD, 0x038E, 0x038F,
            0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
            0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F,
            0x03A0, 0x03A1, _BLANK, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7,
            0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x03AC, 0x03AD, 0x03AE, 0x03AF,
            0x03B0, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7,
            0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF,
            0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7,
            0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, _BLANK]),
        CP850: new Uint16Array([    // CP850
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x00D7, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
            0x00BF, 0x00AE, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0,
            0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x00E3, 0x00C3,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            0x00F0, 0x00D0, 0x00CA, 0x00CB, 0x00C8, 0x0131, 0x00CD, 0x00CE,
            0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, 0x00A6, 0x00CC, 0x2580,
            0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x00F5, 0x00D5, 0x00B5, 0x00FE,
            0x00DE, 0x00DA, 0x00DB, 0x00D9, 0x00FD, 0x00DD, 0x00AF, 0x00B4,
            _SHY  , 0x00B1, 0x2017, 0x00BE, 0x00B6, 0x00A7, 0x00F7, 0x00B8,
            0x00B0, 0x00A8, 0x00B7, 0x00B9, 0x00B3, 0x00B2, 0x25A0, 0x00A0]),
        CP851: new Uint16Array([    // CP851
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x0386, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x0388, 0x00C4, 0x0389,
            0x038A, _BLANK, 0x038C, 0x00F4, 0x00F6, 0x038E, 0x00FB, 0x00F9,
            0x038F, 0x00D6, 0x00DC, 0x03AC, 0x00A3, 0x03AD, 0x03AE, 0x03AF,
            0x03CA, 0x0390, 0x03CC, 0x03CD, 0x0391, 0x0392, 0x0393, 0x0394,
            0x0395, 0x0396, 0x0397, 0x00BD, 0x0398, 0x0399, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x039A, 0x039B, 0x039C,
            0x039D, 0x2563, 0x2551, 0x2557, 0x255D, 0x039E, 0x039F, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x03A0, 0x03A1,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x03A3,
            0x03A4, 0x03A5, 0x03A6, 0x03A7, 0x03A8, 0x03A9, 0x03B1, 0x03B2,
            0x03B3, 0x2518, 0x250C, 0x2588, 0x2584, 0x03B4, 0x03B5, 0x2580,
            0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD,
            0x03BE, 0x03BF, 0x03C0, 0x03C1, 0x03C3, 0x03C2, 0x03C4, 0x0384,
            _SHY  , 0x00B1, 0x03C5, 0x03C6, 0x03C7, 0x00A7, 0x03C8, 0x0385,
            0x00B0, 0x00A8, 0x03C9, 0x03CB, 0x03B0, 0x03CE, 0x25A0, 0x00A0]),
        CP852: new Uint16Array([    // CP852
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x016F, 0x0107, 0x00E7,
            0x0142, 0x00EB, 0x0150, 0x0151, 0x00EE, 0x0179, 0x00C4, 0x0106,
            0x00C9, 0x0139, 0x013A, 0x00F4, 0x00F6, 0x013D, 0x013E, 0x015A,
            0x015B, 0x00D6, 0x00DC, 0x0164, 0x0165, 0x0141, 0x00D7, 0x010D,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x0104, 0x0105, 0x017D, 0x017E,
            0x0118, 0x0119, 0x00AC, 0x017A, 0x010C, 0x015F, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x011A,
            0x015E, 0x2563, 0x2551, 0x2557, 0x255D, 0x017B, 0x017C, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x0102, 0x0103,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            0x0111, 0x0110, 0x010E, 0x00CB, 0x010F, 0x0147, 0x00CD, 0x00CE,
            0x011B, 0x2518, 0x250C, 0x2588, 0x2584, 0x0162, 0x016E, 0x2580,
            0x00D3, 0x00DF, 0x00D4, 0x0143, 0x0144, 0x0148, 0x0160, 0x0161,
            0x0154, 0x00DA, 0x0155, 0x0170, 0x00FD, 0x00DD, 0x0163, 0x00B4,
            _SHY  , 0x02DD, 0x02DB, 0x02C7, 0x02D8, 0x00A7, 0x00F7, 0x00B8,
            0x00B0, 0x00A8, 0x02D9, 0x0171, 0x0158, 0x0159, 0x25A0, 0x00A0]),
        CP853: new Uint16Array([    // CP853
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x0109, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x0108,
            0x00C9, 0x010B, 0x010A, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x0130, 0x00D6, 0x00DC, 0x011D, 0x00A3, 0x011C, 0x00D7, 0x0135,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x011E, 0x011F,
            0x0124, 0x0125, _BLANK, 0x00BD, 0x0134, 0x015F, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0,
            0x015E, 0x2563, 0x2551, 0x2557, 0x255D, 0x017B, 0x017C, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x015C, 0x015D,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            _BLANK, _BLANK, 0x00CA, 0x00CB, 0x00C8, 0x0131, 0x00CD, 0x00CE,
            0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, _BLANK, 0x00CC, 0x2580,
            0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x0120, 0x0121, 0x00B5, 0x0126,
            0x0127, 0x00DA, 0x00DB, 0x00D9, 0x016C, 0x016D, 0x00B7, 0x00B4,
            _SHY  , _BLANK, 0x2113, 0x0149, 0x02D8, 0x00A7, 0x00F7, 0x00B8,
            0x00B0, 0x00A8, 0x02D9, _BLANK, 0x00B3, 0x00B2, 0x25A0, 0x00A0]),
        CP855: new Uint16Array([    // CP855
            0x0452, 0x0402, 0x0453, 0x0403, 0x0451, 0x0401, 0x0454, 0x0404,
            0x0455, 0x0405, 0x0456, 0x0406, 0x0457, 0x0407, 0x0458, 0x0408,
            0x0459, 0x0409, 0x045A, 0x040A, 0x045B, 0x040B, 0x045C, 0x040C,
            0x045E, 0x040E, 0x045F, 0x040F, 0x044E, 0x042E, 0x044A, 0x042A,
            0x0430, 0x0410, 0x0431, 0x0411, 0x0446, 0x0426, 0x0434, 0x0414,
            0x0435, 0x0415, 0x0444, 0x0424, 0x0433, 0x0413, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x0445, 0x0425, 0x0438,
            0x0418, 0x2563, 0x2551, 0x2557, 0x255D, 0x0439, 0x0419, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x043A, 0x041A,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            0x043B, 0x041B, 0x043C, 0x041C, 0x043D, 0x041D, 0x043E, 0x041E,
            0x043F, 0x2518, 0x250C, 0x2588, 0x2584, 0x041F, 0x044F, 0x2580,
            0x042F, 0x0440, 0x0420, 0x0441, 0x0421, 0x0442, 0x0422, 0x0443,
            0x0423, 0x0436, 0x0416, 0x0432, 0x0412, 0x044C, 0x042C, 0x2116,
            _SHY  , 0x044B, 0x042B, 0x0437, 0x0417, 0x0448, 0x0428, 0x044D,
            0x042D, 0x0449, 0x0429, 0x0447, 0x0427, 0x00A7, 0x25A0, 0x00A0]),
        CP857: new Uint16Array([    // CP857
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x0131, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x0130, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x015E, 0x015F,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x011E, 0x011F,
            0x00BF, 0x00AE, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0,
            0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x00E3, 0x00C3,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            0x00BA, 0x00AA, 0x00CA, 0x00CB, 0x00C8, 0x20AC, 0x00CD, 0x00CE,
            0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, 0x00A6, 0x00CC, 0x2580,
            0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x00F5, 0x00D5, 0x00B5, _BLANK,
            0x00D7, 0x00DA, 0x00DB, 0x00D9, 0x00EC, 0x00FF, 0x00AF, 0x00B4,
            _SHY  , 0x00B1, _BLANK, 0x00BE, 0x00B6, 0x00A7, 0x00F7, 0x00B8,
            0x00B0, 0x00A8, 0x00B7, 0x00B9, 0x00B3, 0x00B2, 0x25A0, 0x00A0]),
        CP858: new Uint16Array([    // CP858
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x00D7, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
            0x00BF, 0x00AE, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0,
            0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x00E3, 0x00C3,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            0x00F0, 0x00D0, 0x00CA, 0x00CB, 0x00C8, 0x20AC, 0x00CD, 0x00CE,
            0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, 0x00A6, 0x00CC, 0x2580,
            0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x00F5, 0x00D5, 0x00B5, 0x00FE,
            0x00DE, 0x00DA, 0x00DB, 0x00D9, 0x00FD, 0x00DD, 0x00AF, 0x00B4,
            _SHY  , 0x00B1, 0x2017, 0x00BE, 0x00B6, 0x00A7, 0x00F7, 0x00B8,
            0x00B0, 0x00A8, 0x00B7, 0x00B9, 0x00B3, 0x00B2, 0x25A0, 0x00A0]),
        CP859: new Uint16Array([    // CP859
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x00D7, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
            0x00BF, 0x00AE, 0x00AC, 0x0153, 0x0152, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0,
            0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x00E3, 0x00C3,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            0x00F0, 0x00D0, 0x00CA, 0x00CB, 0x00C8, 0x20AC, 0x00CD, 0x00CE,
            0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, 0x0160, 0x00CC, 0x2580,
            0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x00F5, 0x00D5, 0x00B5, 0x00FE,
            0x00DE, 0x00DA, 0x00DB, 0x00D9, 0x00FD, 0x00DD, 0x00AF, 0x017D,
            _SHY  , 0x00B1, _BLANK, 0x0178, 0x00B6, 0x00A7, 0x00F7, 0x017E,
            0x00B0, 0x0161, 0x00B7, 0x00B9, 0x00B3, 0x00B2, 0x25A0, 0x00A0]),
        CP860: new Uint16Array([    // CP860
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E3, 0x00E0, 0x00C1, 0x00E7,
            0x00EA, 0x00CA, 0x00E8, 0x00CD, 0x00D4, 0x00EC, 0x00C3, 0x00C2,
            0x00C9, 0x00C0, 0x00C8, 0x00F4, 0x00F5, 0x00F2, 0x00DA, 0x00F9,
            0x00CC, 0x00D5, 0x00DC, 0x00A2, 0x00A3, 0x00D9, 0x20A7, 0x00D3,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
            0x00BF, 0x00D2, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP861: new Uint16Array([    // CP861
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00D0, 0x00F0, 0x00DE, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00FE, 0x00FB, 0x00DD,
            0x00FD, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x20A7, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00C1, 0x00CD, 0x00D3, 0x00DA,
            0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP863: new Uint16Array([    // CP863
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00C2, 0x00E0, 0x00B6, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x2017, 0x00C0, 0x00A7,
            0x00C9, 0x00C8, 0x00CA, 0x00F4, 0x00CB, 0x00CF, 0x00FB, 0x00F9,
            0x00A4, 0x00D4, 0x00DC, 0x00A2, 0x00A3, 0x00D9, 0x00DB, 0x0192,
            0x00A6, 0x00B4, 0x00F3, 0x00FA, 0x00A8, 0x00B8, 0x00B3, 0x00AF,
            0x00CE, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00BE, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP865: new Uint16Array([    // CP865
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x20A7, 0x0192,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
            0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00A4,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP866: new Uint16Array([    // CP866, CP900
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
            0x0401, 0x0451, 0x0404, 0x0454, 0x0407, 0x0457, 0x040E, 0x045E,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x2116, 0x00A4, 0x25A0, 0x00A0]),
        CP867: new Uint16Array([    // CP867, CP895
            0x010C, 0x00FC, 0x00E9, 0x010F, 0x00E4, 0x010E, 0x0164, 0x010D,
            0x011B, 0x011A, 0x0139, 0x00CD, 0x013E, 0x013A, 0x00C4, 0x00C1,
            0x00C9, 0x017E, 0x017D, 0x00F4, 0x00F6, 0x00D3, 0x016F, 0x00DA,
            0x00FD, 0x00D6, 0x00DC, 0x0160, 0x013D, 0x00DD, 0x0158, 0x0165,
            0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x0148, 0x0147, 0x016E, 0x00D4,
            0x0161, 0x0159, 0x0155, 0x0154, 0x00BC, 0x00A7, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        CP869: new Uint16Array([    // CP869
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 0x0386, _BLANK,
            0x00B7, 0x00AC, 0x00A6, 0x2018, 0x2019, 0x0388, 0x2015, 0x0389,
            0x038A, 0x03AA, 0x038C, _BLANK, _BLANK, 0x038E, 0x03AB, 0x00A9,
            0x038F, 0x00B2, 0x00B3, 0x03AC, 0x00A3, 0x03AD, 0x03AE, 0x03AF,
            0x03CA, 0x0390, 0x03CC, 0x03CD, 0x0391, 0x0392, 0x0393, 0x0394,
            0x0395, 0x0396, 0x0397, 0x00BD, 0x0398, 0x0399, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x039A, 0x039B, 0x039C,
            0x039D, 0x2563, 0x2551, 0x2557, 0x255D, 0x039E, 0x039F, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x03A0, 0x03A1,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x03A3,
            0x03A4, 0x03A5, 0x03A6, 0x03A7, 0x03A8, 0x03A9, 0x03B1, 0x03B2,
            0x03B3, 0x2518, 0x250C, 0x2588, 0x2584, 0x03B4, 0x03B5, 0x2580,
            0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD,
            0x03BE, 0x03BF, 0x03C0, 0x03C1, 0x03C3, 0x03C2, 0x03C4, 0x0384,
            _SHY  , 0x00B1, 0x03C5, 0x03C6, 0x03C7, 0x00A7, 0x03C8, 0x0385,
            0x00B0, 0x00A8, 0x03C9, 0x03CB, 0x03B0, 0x03CE, 0x25A0, 0x00A0]),
        CP872: new Uint16Array([    // CP872
            0x0452, 0x0402, 0x0453, 0x0403, 0x0451, 0x0401, 0x0454, 0x0404,
            0x0455, 0x0405, 0x0456, 0x0406, 0x0457, 0x0407, 0x0458, 0x0408,
            0x0459, 0x0409, 0x045A, 0x040A, 0x045B, 0x040B, 0x045C, 0x040C,
            0x045E, 0x040E, 0x045F, 0x040F, 0x044E, 0x042E, 0x044A, 0x042A,
            0x0430, 0x0410, 0x0431, 0x0411, 0x0446, 0x0426, 0x0434, 0x0414,
            0x0435, 0x0415, 0x0444, 0x0424, 0x0433, 0x0413, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x0445, 0x0425, 0x0438,
            0x0418, 0x2563, 0x2551, 0x2557, 0x255D, 0x0439, 0x0419, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x043A, 0x041A,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x20AC,
            0x043B, 0x041B, 0x043C, 0x041C, 0x043D, 0x041D, 0x043E, 0x041E,
            0x043F, 0x2518, 0x250C, 0x2588, 0x2584, 0x041F, 0x044F, 0x2580,
            0x042F, 0x0440, 0x0420, 0x0441, 0x0421, 0x0442, 0x0422, 0x0443,
            0x0423, 0x0436, 0x0416, 0x0432, 0x0412, 0x044C, 0x042C, 0x2116,
            _SHY  , 0x044B, 0x042B, 0x0437, 0x0417, 0x0448, 0x0428, 0x044D,
            0x042D, 0x0449, 0x0429, 0x0447, 0x0427, 0x00A7, 0x25A0, 0x00A0]),
        CP878: new Uint16Array([    // CP878
            0x2500, 0x2502, 0x250C, 0x2510, 0x2514, 0x2518, 0x251C, 0x2524,
            0x252C, 0x2534, 0x253C, 0x2580, 0x2584, 0x2588, 0x258C, 0x2590,
            0x2591, 0x2592, 0x2593, 0x2320, 0x25A0, 0x2219, 0x221A, 0x2248,
            0x2264, 0x2265, 0x00A0, 0x2321, 0x00B0, 0x00B2, 0x00B7, 0x00F7,
            0x2550, 0x2551, 0x2552, 0x0451, 0x2553, 0x2554, 0x2555, 0x2556,
            0x2557, 0x2558, 0x2559, 0x255A, 0x255B, 0x255C, 0x255D, 0x255E,
            0x255F, 0x2560, 0x2561, 0x0401, 0x2562, 0x2563, 0x2564, 0x2565,
            0x2566, 0x2567, 0x2568, 0x2569, 0x256A, 0x256B, 0x256C, 0x00A9,
            0x044E, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433,
            0x0445, 0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E,
            0x043F, 0x044F, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432,
            0x044C, 0x044B, 0x0437, 0x0448, 0x044D, 0x0449, 0x0447, 0x044A,
            0x042E, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413,
            0x0425, 0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E,
            0x041F, 0x042F, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412,
            0x042C, 0x042B, 0x0417, 0x0428, 0x042D, 0x0429, 0x0427, 0x042A]),
        CP912: new Uint16Array([    // CP912
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2518, 0x250C, 0x2588,
            0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x2584, 0x2580,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00AE,
            0x00A0, 0x0104, 0x02D8, 0x0141, 0x00A4, 0x013D, 0x015A, 0x00A7,
            0x00A8, 0x0160, 0x015E, 0x0164, 0x0179, _SHY  , 0x017D, 0x017B,
            0x00B0, 0x0105, 0x02DB, 0x0142, 0x00B4, 0x013E, 0x015B, 0x02C7,
            0x00B8, 0x0161, 0x015F, 0x0165, 0x017A, 0x02DD, 0x017E, 0x017C,
            0x0154, 0x00C1, 0x00C2, 0x0102, 0x00C4, 0x0139, 0x0106, 0x00C7,
            0x010C, 0x00C9, 0x0118, 0x00CB, 0x011A, 0x00CD, 0x00CE, 0x010E,
            0x0110, 0x0143, 0x0147, 0x00D3, 0x00D4, 0x0150, 0x00D6, 0x00D7,
            0x0158, 0x016E, 0x00DA, 0x0170, 0x00DC, 0x00DD, 0x0162, 0x00DF,
            0x0155, 0x00E1, 0x00E2, 0x0103, 0x00E4, 0x013A, 0x0107, 0x00E7,
            0x010D, 0x00E9, 0x0119, 0x00EB, 0x011B, 0x00ED, 0x00EE, 0x010F,
            0x0111, 0x0144, 0x0148, 0x00F3, 0x00F4, 0x0151, 0x00F6, 0x00F7,
            0x0159, 0x016F, 0x00FA, 0x0171, 0x00FC, 0x00FD, 0x0163, 0x02D9]),
        CP915: new Uint16Array([    // CP915
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2518, 0x250C, 0x2588,
            0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x2584, 0x2580,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            0x00A0, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407,
            0x0408, 0x0409, 0x040A, 0x040B, 0x040C, _SHY  , 0x040E, 0x040F,
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
            0x2116, 0x0451, 0x0452, 0x0453, 0x0454, 0x0455, 0x0456, 0x0457,
            0x0458, 0x0459, 0x045A, 0x045B, 0x045C, 0x00A7, 0x045E, 0x045F]),
        CP920: new Uint16Array([    // CP920
            0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, _SHY  , 0x00AE, 0x00AF,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
            0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF,
            0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x011E, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
            0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x0130, 0x015E, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x011F, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
            0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x0131, 0x015F, 0x00FF]),
        CP1117: new Uint16Array([    // CP1117
            0x0106, 0x00FC, 0x00E9, 0x0101, 0x00E4, 0x0123, 0x00E5, 0x0107,
            0x0142, 0x0113, 0x0117, 0x012F, 0x012B, 0x0179, 0x00C4, 0x00C5,
            0x00C9, 0x017B, 0x017C, 0x014D, 0x00F6, 0x0122, 0x016B, 0x015A,
            0x015B, 0x00D6, 0x00DC, 0x0144, 0x013B, 0x0141, 0x00D7, 0x010D,
            0x0100, 0x012A, 0x00F3, 0x0173, 0x0104, 0x0105, 0x017D, 0x017E,
            0x0118, 0x0119, 0x0116, 0x017A, 0x010C, 0x012E, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
            0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x00D3, 0x00DF, 0x014C, 0x0143, 0x00F5, 0x00D5, 0x0160, 0x0161,
            0x0136, 0x0137, 0x016A, 0x0172, 0x013C, 0x0112, 0x0145, 0x0146,
            _SHY  , 0x00B1, 0x00E6, 0x00C6, 0x00B6, 0x00A4, 0x00F7, 0x00F8,
            0x00B0, 0x00D8, 0x00B7, 0x0157, 0x0156, 0x201E, 0x201C, 0x00A0]),
        CPMIK: new Uint16Array([    // CPMIK
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x2563, 0x2551,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2510,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2116, 0x00A7, 0x2557,
            0x255D, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
            0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
            0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
            0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
        WIN1250: new Uint16Array([    // WIN1250
            0x20AC, _BLANK, 0x201A, _BLANK, 0x201E, 0x2026, 0x2020, 0x2021,
            _BLANK, 0x2030, 0x0160, 0x2039, 0x015A, 0x0164, 0x017D, 0x0179,
            _BLANK, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            _BLANK, 0x2122, 0x0161, 0x203A, 0x015B, 0x0165, 0x017E, 0x017A,
            0x00A0, 0x02C7, 0x02D8, 0x0141, 0x00A4, 0x0104, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, 0x015E, 0x00AB, 0x00AC, _SHY  , 0x00AE, 0x017B,
            0x00B0, 0x00B1, 0x02DB, 0x0142, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
            0x00B8, 0x0105, 0x015F, 0x00BB, 0x013D, 0x02DD, 0x013E, 0x017C,
            0x0154, 0x00C1, 0x00C2, 0x0102, 0x00C4, 0x0139, 0x0106, 0x00C7,
            0x010C, 0x00C9, 0x0118, 0x00CB, 0x011A, 0x00CD, 0x00CE, 0x010E,
            0x0110, 0x0143, 0x0147, 0x00D3, 0x00D4, 0x0150, 0x00D6, 0x00D7,
            0x0158, 0x016E, 0x00DA, 0x0170, 0x00DC, 0x00DD, 0x0162, 0x00DF,
            0x0155, 0x00E1, 0x00E2, 0x0103, 0x00E4, 0x013A, 0x0107, 0x00E7,
            0x010D, 0x00E9, 0x0119, 0x00EB, 0x011B, 0x00ED, 0x00EE, 0x010F,
            0x0111, 0x0144, 0x0148, 0x00F3, 0x00F4, 0x0151, 0x00F6, 0x00F7,
            0x0159, 0x016F, 0x00FA, 0x0171, 0x00FC, 0x00FD, 0x0163, 0x02D9]),
        WIN1251: new Uint16Array([    // WIN1251 [1]
            0x0402, 0x0403, 0x201A, 0x0453, 0x201E, 0x2026, 0x2020, 0x2021,
            0x20AC, 0x2030, 0x0409, 0x2039, 0x040A, 0x040C, 0x040B, 0x040F,
            0x0452, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            _BLANK, 0x2122, 0x0459, 0x203A, 0x045A, 0x045C, 0x045B, 0x045F,
            0x00A0, 0x040E, 0x045E, 0x0408, 0x00A4, 0x0490, 0x00A6, 0x00A7,
            0x0401, 0x00A9, 0x0404, 0x00AB, 0x00AC, _SHY  , 0x00AE, 0x0407,
            0x00B0, 0x00B1, 0x0406, 0x0456, 0x0491, 0x00B5, 0x00B6, 0x00B7,
            0x0451, 0x2116, 0x0454, 0x00BB, 0x0458, 0x0405, 0x0455, 0x0457,
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F]),
        WIN1253: new Uint16Array([    // WIN1253
            0x20AC, _BLANK, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021,
            _BLANK, 0x2030, _BLANK, 0x2039, _BLANK, _BLANK, _BLANK, _BLANK,
            _BLANK, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            _BLANK, 0x2122, _BLANK, 0x203A, _BLANK, _BLANK, _BLANK, _BLANK,
            0x00A0, 0x0385, 0x0386, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, _BLANK, 0x00AB, 0x00AC, _SHY  , 0x00AE, 0x2015,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x0384, 0x00B5, 0x00B6, 0x00B7,
            0x0388, 0x0389, 0x038A, 0x00BB, 0x038C, 0x00BD, 0x038E, 0x038F,
            0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
            0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F,
            0x03A0, 0x03A1, _BLANK, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7,
            0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x03AC, 0x03AD, 0x03AE, 0x03AF,
            0x03B0, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7,
            0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF,
            0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7,
            0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, _BLANK]),
        WIN1254: new Uint16Array([    // WIN1254
            0x20AC, _BLANK, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021,
            0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, _BLANK, _BLANK, _BLANK,
            _BLANK, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, _BLANK, _BLANK, 0x0178,
            0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, _SHY  , 0x00AE, 0x00AF,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
            0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF,
            0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x011E, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
            0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x0130, 0x015E, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x011F, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
            0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x0131, 0x015F, 0x00FF]),
        WIN1257: new Uint16Array([    // WIN1257
            0x20AC, _BLANK, 0x201A, _BLANK, 0x201E, 0x2026, 0x2020, 0x2021,
            _BLANK, 0x2030, _BLANK, 0x2039, _BLANK, 0x00A8, 0x02C7, 0x00B8,
            _BLANK, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            _BLANK, 0x2122, _BLANK, 0x203A, _BLANK, 0x00AF, 0x02DB, _BLANK,
            0x00A0, _BLANK, 0x00A2, 0x00A3, 0x00A4, _BLANK, 0x00A6, 0x00A7,
            0x00D8, 0x00A9, 0x0156, 0x00AB, 0x00AC, _SHY  , 0x00AE, 0x00C6,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
            0x00F8, 0x00B9, 0x0157, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00E6,
            0x0104, 0x012E, 0x0100, 0x0106, 0x00C4, 0x00C5, 0x0118, 0x0112,
            0x010C, 0x00C9, 0x0179, 0x0116, 0x0122, 0x0136, 0x012A, 0x013B,
            0x0160, 0x0143, 0x0145, 0x00D3, 0x014C, 0x00D5, 0x00D6, 0x00D7,
            0x0172, 0x0141, 0x015A, 0x016A, 0x00DC, 0x017B, 0x017D, 0x00DF,
            0x0105, 0x012F, 0x0101, 0x0107, 0x00E4, 0x00E5, 0x0119, 0x0113,
            0x010D, 0x00E9, 0x017A, 0x0117, 0x0123, 0x0137, 0x012B, 0x013C,
            0x0161, 0x0144, 0x0146, 0x00F3, 0x014D, 0x00F5, 0x00F6, 0x00F7,
            0x0173, 0x0142, 0x015B, 0x016B, 0x00FC, 0x017C, 0x017E, 0x02D9]),
        KOI8_R: new Uint16Array([   // KOI8-R [2]
            0x2500, 0x2502, 0x250C, 0x2510, 0x2514, 0x2518, 0x251C, 0x2524,
            0x252C, 0x2534, 0x253C, 0x2580, 0x2584, 0x2588, 0x258C, 0x2590,
            0x2591, 0x2592, 0x2593, 0x2320, 0x25A0, 0x2219, 0x221A, 0x2248,
            0x2264, 0x2265, 0x00A0, 0x2321, 0x00B0, 0x00B2, 0x00B7, 0x00F7,
            0x2550, 0x2551, 0x2552, 0x0451, 0x2553, 0x2554, 0x2555, 0x2556,
            0x2557, 0x2558, 0x2559, 0x255A, 0x255B, 0x255C, 0x255D, 0x255E,
            0x255F, 0x2560, 0x2561, 0x0401, 0x2562, 0x2563, 0x2564, 0x2565,
            0x2566, 0x2567, 0x2568, 0x2569, 0x256A, 0x256B, 0x256C, 0x00A9,
            0x044E, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433,
            0x0445, 0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E,
            0x043F, 0x044F, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432,
            0x044C, 0x044B, 0x0437, 0x0448, 0x044D, 0x0449, 0x0447, 0x044A,
            0x042E, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413,
            0x0425, 0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E,
            0x041F, 0x042F, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412,
            0x042C, 0x042B, 0x0417, 0x0428, 0x042D, 0x0429, 0x0427, 0x042A]),
        KOI8_U: new Uint16Array([   // KOI8-U
            0x2500, 0x2502, 0x250C, 0x2510, 0x2514, 0x2518, 0x251C, 0x2524,
            0x252C, 0x2534, 0x253C, 0x2580, 0x2584, 0x2588, 0x258C, 0x2590,
            0x2591, 0x2592, 0x2593, 0x2320, 0x25A0, 0x2219, 0x221A, 0x2248,
            0x2264, 0x2265, 0x00A0, 0x2321, 0x00B0, 0x00B2, 0x00B7, 0x00F7,
            0x2550, 0x2551, 0x2552, 0x0451, 0x0454, 0x2554, 0x0456, 0x0457,
            0x2557, 0x2558, 0x2559, 0x255A, 0x255B, 0x0491, 0x255D, 0x255E,
            0x255F, 0x2560, 0x2561, 0x0401, 0x0404, 0x2563, 0x0406, 0x0407,
            0x2566, 0x2567, 0x2568, 0x2569, 0x256A, 0x0490, 0x256C, 0x00A9,
            0x044E, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433,
            0x0445, 0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E,
            0x043F, 0x044F, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432,
            0x044C, 0x044B, 0x0437, 0x0448, 0x044D, 0x0449, 0x0447, 0x044A,
            0x042E, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413,
            0x0425, 0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E,
            0x041F, 0x042F, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412,
            0x042C, 0x042B, 0x0417, 0x0428, 0x042D, 0x0429, 0x0427, 0x042A]),
        ISO8859_1: new Uint16Array([    // ISO8859_1, CP819
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x2010, 0x00AE, 0x00AF,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
            0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF,
            0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
            0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
            0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF]),
        ISO8859_2: new Uint16Array([    // ISO8859_2 [3]
            0x2500, 0x2502, 0x250C, 0x2510, 0x2514, 0x2518, 0x251C, 0x2524,
            0x252C, 0x2534, 0x253C, 0x2580, 0x2584, 0x2588, 0x258C, 0x2590,
            0x2591, 0x2592, 0x2593, 0x2320, 0x25A0, 0x2219, 0x221A, 0x2248,
            0x2264, 0x2265, 0x00A0, 0x2321, 0x00B0, 0x00B2, 0x00B7, 0x00F7,
            0x00A0, 0x0104, 0x02D8, 0x0141, 0x00A4, 0x013D, 0x015A, 0x00A7,
            0x00A8, 0x0160, 0x015E, 0x0164, 0x0179, 0x2010, 0x017D, 0x017B,
            0x00B0, 0x0105, 0x02DB, 0x0142, 0x00B4, 0x013E, 0x015B, 0x02C7,
            0x00B8, 0x0161, 0x015F, 0x0165, 0x017A, 0x02DD, 0x017E, 0x017C,
            0x0154, 0x00C1, 0x00C2, 0x0102, 0x00C4, 0x0139, 0x0106, 0x00C7,
            0x010C, 0x00C9, 0x0118, 0x00CB, 0x011A, 0x00CD, 0x00CE, 0x010E,
            0x0110, 0x0143, 0x0147, 0x00D3, 0x00D4, 0x0150, 0x00D6, 0x00D7,
            0x0158, 0x016E, 0x00DA, 0x0170, 0x00DC, 0x00DD, 0x0162, 0x00DF,
            0x0155, 0x00E1, 0x00E2, 0x0103, 0x00E4, 0x013A, 0x0107, 0x00E7,
            0x010D, 0x00E9, 0x0119, 0x00EB, 0x011B, 0x00ED, 0x00EE, 0x010F,
            0x0111, 0x0144, 0x0148, 0x00F3, 0x00F4, 0x0151, 0x00F6, 0x00F7,
            0x0159, 0x016F, 0x00FA, 0x0171, 0x00FC, 0x00FD, 0x0163, 0x02D9]),
        ISO8859_3: new Uint16Array([    // ISO8859_3, CP28593
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x0126, 0x02D8, 0x00A3, 0x00A4, _BLANK, 0x0124, 0x00A7,
            0x00A8, 0x0130, 0x015E, 0x011E, 0x0134, 0x2010, _BLANK, 0x017B,
            0x00B0, 0x0127, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x0125, 0x00B7,
            0x00B8, 0x0131, 0x015F, 0x011F, 0x0135, 0x00BD, _BLANK, 0x017C,
            0x00C0, 0x00C1, 0x00C2, _BLANK, 0x00C4, 0x010A, 0x0108, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            _BLANK, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x0120, 0x00D6, 0x00D7,
            0x011C, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x016C, 0x015C, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, _BLANK, 0x00E4, 0x010B, 0x0109, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            _BLANK, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x0121, 0x00F6, 0x00F7,
            0x011D, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x016D, 0x015D, 0x02D9]),
        ISO8859_4: new Uint16Array([    // ISO8859_4, CP28594 [4,13]
            0x2500, 0x2502, 0x250C, 0x2510, 0x2514, 0x2518, 0x251C, 0x2524,
            0x252C, 0x2534, 0x253C, 0x2580, 0x2584, 0x2588, 0x258C, 0x2590,
            0x2591, 0x2592, 0x2593, 0x2320, 0x25A0, 0x2219, 0x221A, 0x2248,
            0x2264, 0x2265, 0x00A0, 0x2321, 0x00B0, 0x00B2, 0x00B7, 0x00F7,
            0x00A0, 0x0104, 0x0138, 0x0156, 0x00A4, 0x0128, 0x013B, 0x00A7,
            0x00A8, 0x0160, 0x0112, 0x0122, 0x0166, 0x2010, 0x017D, 0x00AF,
            0x00B0, 0x0105, 0x02DB, 0x0157, 0x00B4, 0x0129, 0x013C, 0x02C7,
            0x00B8, 0x0161, 0x0113, 0x0123, 0x0167, 0x014A, 0x017E, 0x014B,
            0x0100, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x012E,
            0x010C, 0x00C9, 0x0118, 0x00CB, 0x0116, 0x00CD, 0x00CE, 0x012A,
            0x0110, 0x0145, 0x014C, 0x0136, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
            0x00D8, 0x0172, 0x00DA, 0x00DB, 0x00DC, 0x0168, 0x016A, 0x00DF,
            0x0101, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x012F,
            0x010D, 0x00E9, 0x0119, 0x00EB, 0x0117, 0x00ED, 0x00EE, 0x012B,
            0x0111, 0x0146, 0x014D, 0x0137, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
            0x00F8, 0x0173, 0x00FA, 0x00FB, 0x00FC, 0x0169, 0x016B, 0x02D9]),
        ISO8859_5: new Uint16Array([        // ISO 8859_5 - cyrillic
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407,
            0x0408, 0x0409, 0x040A, 0x040B, 0x040C, 0x2010, 0x040E, 0x040F,
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
            0x2116, 0x0451, 0x0452, 0x0453, 0x0454, 0x0455, 0x0456, 0x0457,
            0x0458, 0x0459, 0x045A, 0x045B, 0x045C, 0x00A7, 0x045E, 0x045F]),
        ISO8859_7: new Uint16Array([        // ISO 8859_7 - greek
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x2018, 0x2019, 0x00A3, 0x20AC, 0x20AF, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, 0x037A, 0x00AB, 0x00AC, 0x2010, _BLANK, 0x2015,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x0384, 0x0385, 0x0386, 0x00B7,
            0x0388, 0x0389, 0x038A, 0x00BB, 0x038C, 0x00BD, 0x038E, 0x038F,
            0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
            0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F,
            0x03A0, 0x03A1, _BLANK, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7,
            0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x03AC, 0x03AD, 0x03AE, 0x03AF,
            0x03B0, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7,
            0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF,
            0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7,
            0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, _BLANK]),
        ISO8859_9: new Uint16Array([        // ISO 8859_9 - turkish
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, _SHY,   0x00AE, 0x00AF,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
            0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF,
            0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x011E, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
            0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x0130, 0x015E, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x011F, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
            0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x0131, 0x015F, 0x00FF]),
        ISO8859_10: new Uint16Array([       // ISO 8859_10 - nordic
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x0104, 0x0112, 0x0122, 0x012A, 0x0128, 0x0136, 0x00A7,
            0x013B, 0x0110, 0x0160, 0x0166, 0x017D, _SHY,   0x016A, 0x014A,
            0x00B0, 0x0105, 0x0113, 0x0123, 0x012B, 0x0129, 0x0137, 0x00B7,
            0x013C, 0x0111, 0x0161, 0x0167, 0x017E, 0x2015, 0x016B, 0x014B,
            0x0100, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x012E,
            0x010C, 0x00C9, 0x0118, 0x00CB, 0x0116, 0x00CD, 0x00CE, 0x00CF,
            0x00D0, 0x0145, 0x014C, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x0168,
            0x00D8, 0x0172, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF,
            0x0101, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x012F,
            0x010D, 0x00E9, 0x0119, 0x00EB, 0x0117, 0x00ED, 0x00EE, 0x00EF,
            0x00F0, 0x0146, 0x014D, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x0169,
            0x00F8, 0x0173, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x0138]),
        ISO8859_13: new Uint16Array([       // ISO 8859_13 - baltic
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x201D, 0x00A2, 0x00A3, 0x00A4, 0x201E, 0x00A6, 0x00A7,
            0x00D8, 0x00A9, 0x0156, 0x00AB, 0x00AC, _SHY,   0x00AE, 0x00C6,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x201C, 0x00B5, 0x00B6, 0x00B7,
            0x00F8, 0x00B9, 0x0157, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00E6,
            0x0104, 0x012E, 0x0100, 0x0106, 0x00C4, 0x00C5, 0x0118, 0x0112,
            0x010C, 0x00C9, 0x0179, 0x0116, 0x0122, 0x0136, 0x012A, 0x013B,
            0x0160, 0x0143, 0x0145, 0x00D3, 0x014C, 0x00D5, 0x00D6, 0x00D7,
            0x0172, 0x0141, 0x015A, 0x016A, 0x00DC, 0x017B, 0x017D, 0x00DF,
            0x0105, 0x012F, 0x0101, 0x0107, 0x00E4, 0x00E5, 0x0119, 0x0113,
            0x010D, 0x00E9, 0x017A, 0x0117, 0x0123, 0x0137, 0x012B, 0x013C,
            0x0161, 0x0144, 0x0146, 0x00F3, 0x014D, 0x00F5, 0x00F6, 0x00F7,
            0x0173, 0x0142, 0x015B, 0x016B, 0x00FC, 0x017C, 0x017E, 0x2019]),
        ISO8859_14: new Uint16Array([       // ISO 8859_14 - celtic
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x1E02, 0x1E03, 0x00A3, 0x010A, 0x010B, 0x1E0A, 0x00A7,
            0x1E80, 0x00A9, 0x1E82, 0x1E0B, 0x1EF2, _SHY,   0x00AE, 0x0178,
            0x1E1E, 0x1E1F, 0x0120, 0x0121, 0x1E40, 0x1E41, 0x00B6, 0x1E56,
            0x1E81, 0x1E57, 0x1E83, 0x1E60, 0x1EF3, 0x1E84, 0x1E85, 0x1E61,
            0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x0174, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x1E6A,
            0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x0176, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x0175, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x1E6B,
            0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x0177, 0x00FF]),
        ISO8859_15: new Uint16Array([       // ISO 8859_15 - western european
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x20AC, 0x00A5, 0x0160, 0x00A7,
            0x0161, 0x00A9, 0x00AA, 0x00AB, 0x00AC, _SHY,   0x00AE, 0x00AF,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x017D, 0x00B5, 0x00B6, 0x00B7,
            0x017E, 0x00B9, 0x00BA, 0x00BB, 0x0152, 0x0153, 0x0178, 0x00BF,
            0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
            0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
            0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF]),
        ISO8859_16: new Uint16Array([       // ISO 8859_16 - southeaster european
            0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
            0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
            0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
            0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
            0x00A0, 0x0104, 0x0105, 0x0141, 0x20AC, 0x201E, 0x0160, 0x00A7,
            0x0161, 0x00A9, 0x0218, 0x00AB, 0x0179, _SHY,   0x017A, 0x017B,
            0x00B0, 0x00B1, 0x010C, 0x0142, 0x017D, 0x201D, 0x00B6, 0x00B7,
            0x017E, 0x010D, 0x0219, 0x00BB, 0x0152, 0x0153, 0x0178, 0x017C,
            0x00C0, 0x00C1, 0x00C2, 0x0102, 0x00C4, 0x0106, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x0110, 0x0143, 0x00D2, 0x00D3, 0x00D4, 0x0150, 0x00D6, 0x015A,
            0x0170, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x0118, 0x021A, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x0103, 0x00E4, 0x0107, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x0111, 0x0144, 0x00F2, 0x00F3, 0x00F4, 0x0151, 0x00F6, 0x015B,
            0x0171, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x0119, 0x021B, 0x00FF]),
        ARMSCII_8: new Uint16Array([    // ArmSCI-8
            0x00a0, 0x058E, 0x0587, 0x0589, 0x0029, 0x0028, 0x00bb, 0x00a8,
            0x2014, 0x00b7, 0x0559, 0x055d, 0x002d, 0x055f, 0x2026, 0x055c,
            0x055b, 0x055e, 0x0531, 0x0561, 0x0532, 0x0562, 0x0533, 0x0563,
            0x0534, 0x0564, 0x0535, 0x0565, 0x0536, 0x0566, 0x0537, 0x0567,
            0x0538, 0x0568, 0x0539, 0x0569, 0x053a, 0x056a, 0x053b, 0x056b,
            0x053c, 0x056c, 0x053d, 0x056d, 0x053e, 0x056e, 0x053f, 0x056f,
            0x0540, 0x0570, 0x0541, 0x0571, 0x0542, 0x0572, 0x0543, 0x0573,
            0x0544, 0x0574, 0x0545, 0x0575, 0x0546, 0x0576, 0x0547, 0x0577,
            0x0548, 0x0578, 0x0549, 0x0579, 0x054a, 0x057a, 0x054b, 0x057b,
            0x054c, 0x057c, 0x054d, 0x057d, 0x054e, 0x057e, 0x054f, 0x057f,
            0x0550, 0x0580, 0x0551, 0x0581, 0x0552, 0x0582, 0x0553, 0x0583,
            0x0554, 0x0584, 0x0555, 0x0585, 0x0556, 0x0586, 0x055a, _BLANK]),
        HAIK8: new Uint16Array([    // HAIK8
            0x00a0, 0x058E, 0x0587, 0x0589, 0x0029, 0x0028, 0x00bb, 0x00a8,
            0x2014, 0x00b7, 0x0559, 0x055d, 0x002d, 0x055f, 0x2026, 0x055c,
            0x055b, 0x055e, 0x0531, 0x0561, 0x0532, 0x0562, 0x0533, 0x0563,
            0x0534, 0x0564, 0x0535, 0x0565, 0x0536, 0x0566, 0x0537, 0x0567,
            0x0538, 0x0568, 0x0539, 0x0569, 0x053a, 0x056a, 0x053b, 0x056b,
            0x053c, 0x056c, 0x053d, 0x056d, 0x053e, 0x056e, 0x053f, 0x056f,
            0x0540, 0x0570, 0x0541, 0x0571, 0x0542, 0x0572, 0x0543, 0x0573,
            0x0544, 0x0574, 0x0545, 0x0575, 0x0546, 0x0576, 0x0547, 0x0577,
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK,
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK,
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK,
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK,
            0x0548, 0x0578, 0x0549, 0x0579, 0x054a, 0x057a, 0x054b, 0x057b,
            0x054c, 0x057c, 0x054d, 0x057d, 0x054e, 0x057e, 0x054f, 0x057f,
            0x0550, 0x0580, 0x0551, 0x0581, 0x0552, 0x0582, 0x0553, 0x0583,
            0x0554, 0x0584, 0x0555, 0x0585, 0x0556, 0x0586, 0x055a, _BLANK]),
        CP1131: new Uint16Array([   // CP1131 - Belarus
            0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
            0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, 0x041f,
            0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
            0x0428, 0x0429, 0x042a, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f,
            0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
            0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e, 0x043f,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
            0x2555, 0x2563, 0x2551, 0x2557, 0x255d, 0x255c, 0x255b, 0x2510,
            0x2514, 0x2534, 0x252c, 0x251c, 0x2500, 0x253c, 0x255e, 0x255f,
            0x255a, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256c, 0x2567,
            0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256b,
            0x256a, 0x2518, 0x250c, 0x2588, 0x2584, 0x258c, 0x2590, 0x2580,
            0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
            0x0448, 0x0449, 0x044a, 0x044b, 0x044c, 0x044d, 0x044e, 0x044f,
            0x0401, 0x0451, 0x0404, 0x0454, 0x0407, 0x0457, 0x040e, 0x045e,
            0x0406, 0x0456, 0x00b7, 0x00a4, 0x0490, 0x0491, 0x2219, 0x00a0]),

        TELETEXT: new Uint16Array([ // TELETEXT
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027,
            0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F,
            0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
            0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
            0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,
            0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F,
            0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057,
            0x0058, 0x0059, 0x005A, 0x2190, 0x00BD, 0x2192, 0x2191, 0x2014,
            0x00A3, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067,
            0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F,
            0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077,
            0x0078, 0x0079, 0x007A, 0x00BC, 0x2016, 0x00BE, 0x00F7, 0x25A0,
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, 
            _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK, _BLANK
        ]),
            
        RAW: new Uint16Array([   // for RAW converted fonts - mapped to ASCII/PETSCII points
            0xE000, 0xE001, 0xE002, 0xE003, 0xE004, 0xE005, 0xE006, 0xE007,
            0xE008, 0xE009, 0xE00A, 0xE00B, 0xE00C, 0xE00D, 0xE00E, 0xE00F,
            0xE010, 0xE011, 0xE012, 0xE013, 0xE014, 0xE015, 0xE016, 0xE017,
            0xE018, 0xE019, 0xE01A, 0xE01B, 0xE01C, 0xE01D, 0xE01E, 0xE01F,
            0xE020, 0xE021, 0xE022, 0xE023, 0xE024, 0xE025, 0xE026, 0xE027,
            0xE028, 0xE029, 0xE02A, 0xE02B, 0xE02C, 0xE02D, 0xE02E, 0xE02F,
            0xE030, 0xE031, 0xE032, 0xE033, 0xE034, 0xE035, 0xE036, 0xE037,
            0xE038, 0xE039, 0xE03A, 0xE03B, 0xE03C, 0xE03D, 0xE03E, 0xE03F,
            0xE040, 0xE041, 0xE042, 0xE043, 0xE044, 0xE045, 0xE046, 0xE047,
            0xE048, 0xE049, 0xE04A, 0xE04B, 0xE04C, 0xE04D, 0xE04E, 0xE04F,
            0xE050, 0xE051, 0xE052, 0xE053, 0xE054, 0xE055, 0xE056, 0xE057,
            0xE058, 0xE059, 0xE05A, 0xE05B, 0xE05C, 0xE05D, 0xE05E, 0xE05F,
            0xE060, 0xE061, 0xE062, 0xE063, 0xE064, 0xE065, 0xE066, 0xE067,
            0xE068, 0xE069, 0xE06A, 0xE06B, 0xE06C, 0xE06D, 0xE06E, 0xE06F,
            0xE070, 0xE071, 0xE072, 0xE073, 0xE074, 0xE075, 0xE076, 0xE077,
            0xE078, 0xE079, 0xE07A, 0xE07B, 0xE07C, 0xE07D, 0xE07E, 0xE07F,
            0xE080, 0xE081, 0xE082, 0xE083, 0xE084, 0xE085, 0xE086, 0xE087,
            0xE088, 0xE089, 0xE08A, 0xE08B, 0xE08C, 0xE08D, 0xE08E, 0xE08F,
            0xE090, 0xE091, 0xE092, 0xE093, 0xE094, 0xE095, 0xE096, 0xE097,
            0xE098, 0xE099, 0xE09A, 0xE09B, 0xE09C, 0xE09D, 0xE09E, 0xE09F,
            0xE0A0, 0xE0A1, 0xE0A2, 0xE0A3, 0xE0A4, 0xE0A5, 0xE0A6, 0xE0A7,
            0xE0A8, 0xE0A9, 0xE0AA, 0xE0AB, 0xE0AC, 0xE0AD, 0xE0AE, 0xE0AF,
            0xE0B0, 0xE0B1, 0xE0B2, 0xE0B3, 0xE0B4, 0xE0B5, 0xE0B6, 0xE0B7,
            0xE0B8, 0xE0B9, 0xE0BA, 0xE0BB, 0xE0BC, 0xE0BD, 0xE0BE, 0xE0BF,
            0xE0C0, 0xE0C1, 0xE0C2, 0xE0C3, 0xE0C4, 0xE0C5, 0xE0C6, 0xE0C7,
            0xE0C8, 0xE0C9, 0xE0CA, 0xE0CB, 0xE0CC, 0xE0CD, 0xE0CE, 0xE0CF,
            0xE0D0, 0xE0D1, 0xE0D2, 0xE0D3, 0xE0D4, 0xE0D5, 0xE0D6, 0xE0D7,
            0xE0D8, 0xE0D9, 0xE0DA, 0xE0DB, 0xE0DC, 0xE0DD, 0xE0DE, 0xE0DF,
            0xE0E0, 0xE0E1, 0xE0E2, 0xE0E3, 0xE0E4, 0xE0E5, 0xE0E6, 0xE0E7,
            0xE0E8, 0xE0E9, 0xE0EA, 0xE0EB, 0xE0EC, 0xE0ED, 0xE0EE, 0xE0EF,
            0xE0F0, 0xE0F1, 0xE0F2, 0xE0F3, 0xE0F4, 0xE0F5, 0xE0F6, 0xE0F7,
            0xE0F8, 0xE0F9, 0xE0FA, 0xE0FB, 0xE0FC, 0xE0FD, 0xE0FE, 0xE0FF]),
        RAWHI: new Uint16Array([   // for RAW converted fonts - mapped to ASCII
            0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087,
            0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F,
            0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
            0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F,
            0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
            0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF,
            0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
            0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
            0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF])
    },

    // http://invisible-island.net/xterm/xterm-function-keys.html
    // http://ansi-bbs.org/ansi-bbs2/index.ssjs
    keysS0C0A0 = {  // Normal keys

        0:  0,                  // windows - ie
        8:  function (){        // backspace
                if (cbm)                return 0x14
                else if (atari)         return 0x7E
                else                    return _BS; },
        9:  function (){        // tab
                if (cbm) {
                    // toggle text / gfx modes
                    conFontNum ^= 0x1;
                    renderAll();
                    return 0;
                } else if (atari)       return 0x7F
                else                    return 0x09; },
        12: 0,                  // clear (numpad5 numlk off)
        13: function () {       // enter
                if (atari)              return 0x9B
                else                    return _CR;
            },
        16: 0,                  // shift
        17: 0,                  // ctrl
        18: 0,                  // alt
        19: 0,                  // pause/break
        20: DO_CAPLK,           // caps lock
        27: function () {       // esc
                if (cbm)                return 0x03 // run/stop 
                else                    return _ESC; },
        32: ' ',                // spacebar
        33: function(){         // pgup
                if (modeDOORWAY)        return '\x00\x49'
                else                    return CSI+'V'; },
        34: function(){         // pgdn
                if (modeDOORWAY)        return '\x00\x51'
                else                    return CSI+'U'; },
        35: function(){         // end
                // text mode in cbm
                if (cbm)                return 0x0E
                else if (modeDOORWAY)   return '\x00\x4F'
                else                    return CSI+'K'; },
        36: function () {       // home
                if (cbm)                return 0x13
                else if (modeDOORWAY)   return '\x00\x47'
                else                    return CSI+'H'; },
        37: function () {       // left arrow
                if (cbm)                return 0x9D
                else if (atari)         return 0x1E
                else if (modeDOORWAY)   return '\x00\x4B'
                else                    return CSI+'D'; },
        38: function () {       // up arrow
                if (cbm)                return 0x91
                else if (atari)         return 0x1C
                else if (modeDOORWAY)   return '\x00\x48'
                else                    return CSI+'A'; },
        39: function () {       // right arrow
                if (cbm)                return 0x1D
                else if (atari)         return 0x1F
                else if (modeDOORWAY)   return '\x00\x4D'
                else                    return CSI+'C'; },
        40: function () {       // down arrow
                if (cbm)                return 0x11
                else if (atari)         return 0x1D
                else if (modeDOORWAY)   return '\x00\x50'
                else                    return CSI+'B'; },
        45: function (){        // insert
                if (cbm)                return 0x94
                else if (atari)         return 0xFF
                else if (modeDOORWAY)   return '\x00\x52'
                else                    return CSI+'@'; },
        46: function (){        // delete
                if (cbm)                return 0x14
                else if (atari)         return 0xFE
                else                    return 0x7f; },
        48: '0',                // 0
        49: '1',                // 1
        50: '2',                // 2
        51: '3',                // 3
        52: '4',                // 4
        53: '5',                // 5
        54: '6',                // 6
        55: '7',                // 7
        56: '8',                // 8
        57: '9',                // 9
        59: ';',                // ;: - firefox
        61: '=',                // =+ - firefox
        65: 'a',                // a
        66: 'b',                // b
        67: 'c',                // c - browser copy
        68: 'd',                // d
        69: 'e',                // e
        70: 'f',                // f
        71: 'g',                // g
        72: 'h',                // h
        73: 'i',                // i
        74: 'j',                // j
        75: 'k',                // k
        76: 'l',                // l
        77: 'm',                // m
        78: 'n',                // n - browser new window
        79: 'o',                // o
        80: 'p',                // p
        81: 'q',                // q
        82: 'r',                // r
        83: 's',                // s
        84: 't',                // t - browser new tab
        85: 'u',                // u
        86: 'v',                // v - browser paste
        87: 'w',                // w - browser close window
        88: 'x',                // x
        89: 'y',                // y
        90: 'z',                // z
        91: 0,                  // left win
        92: 0,                  // right win
        93: 0,                  // select
        96: '0',                // numpad0
        97: '1',                // numpad1
        98: '2',                // numpad2
        99: '3',                // numpad3
       100: '4',                // numpad4
       101: '5',                // numpad5
       102: '6',                // numpad6
       103: '7',                // numpad7
       104: '8',                // numpad8
       105: '9',                // numpad9
       106: '*',                // multiply
       107: '+',                // add (use for enter on VT modes)
       109: '-',                // subtract
       110: '.',                // decimal
       111: '/',                // divide
       112: function(){         // f1
                if (cbm)                return 0x85 
                else if (modeDOORWAY)   return '\x00\x3B'
                else                    return ESC+'OP'; },
       113: function(){         // f2
                if (cbm)                return 0x89
                else if (modeDOORWAY)   return '\x00\x3C'
                else                    return ESC+'OQ'; },
       114: function(){         // f3
                if (cbm)                return 0x86
                else if (modeDOORWAY)   return '\x00\x3D'
                else                    return ESC+'OR'; },
       115: function(){         // f4
                if (cbm)                return 0x8A
                else if (modeDOORWAY)   return '\x00\x3E'
                else                    return ESC+'OS'; },
       116: function(){         // f5 - browser refresh
                if (cbm)                return 0x87
                else if (modeDOORWAY)   return '\x00\x3F'
                else                    return ESC+'Ot'; },
       117: function(){         // f6
                if (cbm)                return 0x8B
                else if (modeDOORWAY)   return '\x00\x40'
                else                    return CSI+'17~'; },
       118: function(){         // f7
                if (cbm)                return 0x88
                else if (modeDOORWAY)   return '\x00\x41'
                else                    return CSI+'18~'; },
       119: function(){         // f8
                if (cbm)                return 0x8C
                else if (modeDOORWAY)   return '\x00\x42'
                else                    return CSI+'19~'; },
       120: function(){         // f9
                if (modeDOORWAY)        return '\x00\x43'
                else                    return CSI+'20~'; },
       121: function(){         // f10
                if (modeDOORWAY)        return '\x00\x44'
                else                    return CSI+'21~'; },
       122: CSI+'23~',          // f11 - browser full screen
       123: CSI+'24~',          // f12

       124: 0,                  // gui F13
       125: 0,                  // gui F14
       126: 0,                  // gui F15 / Help
       127: 0,                  // gui F16 / Do
       128: 0,                  // gui F17
       129: 0,                  // gui F18
       130: 0,                  // gui F19
       131: 0,                  // gui F20
       132: 0,                  // gui F21
       133: 0,                  // gui F22
       134: 0,                  // gui F23
       135: 0,                  // gui F24
       144: DO_NUMLK,           // numlock
       145: DO_SCRLK,           // scrolllock
       173: '-',                // -_ (firefox)
       186: ';',                // ;:
       187: '=',                // =+
       188: ',',                // ,<
       189: '-',                // -
       190: '.',                // .
       191: '/',                // /
       192: '`',                // `
       219: '[',                // [
       220: '\\',               // '\'
       221: ']',                // ]
       222: '\'',               // '
       255: 0,                  // windows - chrome/opera
    },
    keysS1C0A0 = {  // SHIFTed keys.

         9: function(){         // tab
                if (atari)              return 0x9F     // set tabstop
                else if (modeDOORWAY)   return '\x00\x0F'
                else                    return 0; },
        13: function(){         // enter
                return cbm?0x8d:0; },
        32: '\xa0',             // spacebar
        
        35: function(){         // end
                // graphics cbm
                return cbm?0x8E:0; },
        36: function(){         // home
                // clr cbm
                return cbm?0x93:0; },

        45: function (){        // insert
                if (atari)              return 0x9D // insert row
                else                    return 0;; },
        46: function (){        // delete
                if (atari)              return 0x9C // delete row
                else                    return 0; },
        48: ')',                // 0
        49: '!',                // 1
        50: '@',                // 2
        51: '#',                // 3
        52: '$',                // 4
        53: '%',                // 5
        54: '^',                // 6
        55: '&',                // 7
        56: '*',                // 8
        57: '(',                // 9
        59: ':',                // ;: - firefox
        61: '+',                // =+ - firefox
        65: 'A',                // a
        66: 'B',                // b
        67: 'C',                // c - browser copy
        68: 'D',                // d
        69: 'E',                // e
        70: 'F',                // f
        71: 'G',                // g
        72: 'H',                // h
        73: 'I',                // i
        74: 'J',                // j
        75: 'K',                // k
        76: 'L',                // l
        77: 'M',                // m
        78: 'N',                // n - browser new window
        79: 'O',                // o
        80: 'P',                // p
        81: 'Q',                // q
        82: 'R',                // r
        83: 'S',                // s
        84: 'T',                // t - browser new tab
        85: 'U',                // u
        86: 'V',                // v - browser paste
        87: 'W',                // w - browser close window
        88: 'X',                // x
        89: 'Y',                // y
        90: 'Z',                // z
       112: function(){         // f1
                return modeDOORWAY?'\x00\x54':0; },
       113: function(){         // f2
                return modeDOORWAY?'\x00\x55':0; },
       114: function(){         // f3
                return modeDOORWAY?'\x00\x56':0; },
       115: function(){         // f4
                return modeDOORWAY?'\x00\x57':0; },
       116: null,               // f5 - browser refresh
       117: function(){         // f6
                return modeDOORWAY?'\x00\x59':0; },
       118: function(){         // f7
                return modeDOORWAY?'\x00\x5A':0; },
       119: function(){         // f8
                return modeDOORWAY?'\x00\x5B':0; },
       120: function(){         // f9
                return modeDOORWAY?'\x00\x5C':0; },
       121: function(){         // f10
                return modeDOORWAY?'\x00\x5D':0; },
       173: '_',                // -_ (firefox)
       186: ':',                // ;:
       187: '+',                // =+
       188: '<',                // ,<
       189: '_',                // -
       190: '>',                // .
       191: '?',                // /
       192: '~',                // `
       219: '{',                // [
       220: '|',                // '\'
       221: '}',                // ]
       222: '"'                 // '
    },
    keysS0C1A0 = {  // CTRLed keys.
         9: function(){         // tab
                if (atari)              return 0x9E // clear tabstop
                else                    return 0; },
        33: function(){         // pgup
                return modeDOORWAY?'\x00\x84':0; },
        34: function(){         // pgdn
                return modeDOORWAY?'\x00\x76':0; },
        35: function(){         // end
                return modeDOORWAY?'\x00\x75':0; },
        36: function(){         // home
                return modeDOORWAY?'/x00/x77':0; },
        37: function(){         // left
                return modeDOORWAY?'\x00\x73':0; },
        39: function(){         // right
                return modeDOORWAY?'\x00\x74':0; },
        48: function(){         // 0
                // rev off
                return cbm?0x92:0; },
        49: function(){          // 1
                // black
                return cbm?0x90:0; },
        50: function(){         // 2
                // white
                return cbm?0x05:0; },
        51: function(){         // 3
                // red
                return cbm?0x1C:0; },
        52: function(){         // 4
                // cyan
                return cbm?0x9f:0; },
        53: function(){         // 5
                // purple
                return cbm?0x9c:0; },
        54: function(){         // 6
                // green
                return cbm?0x1e:0; },
        55: function(){         // 7
                // blue
                return cbm?0x1f:0; },
        56: function(){         // 8
                // yellow
                return cbm?0x9e:0; },
        57: function(){         // 9
                // rev on
                return cbm?0x12:0; },
        65: 0x01,               // a
        66: 0x02,               // b
        67: null,               // c - browser copy
        68: 0x04,               // d
        69: 0x05,               // e
        70: 0x06,               // f
        71: 0x07,               // g
        72: 0x08,               // h
        73: 0x09,               // i
        74: 0x0a,               // j
        75: 0x0b,               // k
        76: 0x0c,               // l
        77: 0x0d,               // m
        78: null,               // n - browser new window
        79: 0x0f,               // o
        80: 0x10,               // p
        81: 0x11,               // q
        82: 0x12,               // r
        83: 0x13,               // s
        84: null,               // t - browser new tab
        85: 0x15,               // u
        86: null,               // v - browser paste
        87: null,               // w - browser close window
        88: 0x18,               // x
        89: 0x19,               // y
        90: 0x1a,               // z
       112: function(){         // f1
                return modeDOORWAY?'\x00\x5E':0; },
       113: function(){         // f2
                return modeDOORWAY?'\x00\x5F':0; },
       114: function(){         // f3
                return modeDOORWAY?'\x00\x60':0; },
       115: function(){         // f4
                return modeDOORWAY?'\x00\x61':0; },
       116: null,               // f5 - browser refresh
       117: function(){         // f6
                return modeDOORWAY?'\x00\x63':0; },
       118: function(){         // f7
                return modeDOORWAY?'\x00\x64':0; },
       119: function(){         // f8
                return modeDOORWAY?'\x00\x65':0; },
       120: function(){         // f9
                return modeDOORWAY?'\x00\x66':0; },
       121: function(){         // f10
                return modeDOORWAY?'\x00\x67':0; },
       219: 0x1b,               // [
       220: 0x1c,               // '\'
       221: 0x1d,               // ]
    },
    keysS1C1A0 = {  // SHIFT+CTRL keys. (currently empty)
    },
    keysS0C0A1 = {  // ALTed keys.
        49: function(){             // 1
                // orange
                return cbm?0x81:0; },
        50: function(){             // 2
                // brown
                return cbm?0x95:0; },
        51: function(){             // 3
                // lt red
                return cbm?0x96:0; },
        52: function(){             // 4
                // dk gray
                return cbm?0x97:0; },
        53: function(){             // 5
                // gray
                return cbm?0x98:0; },
        54: function(){             // 6
                // lt green
                return cbm?0x99:0; },
        55: function(){             // 7
                // lt blue
                return cbm?0x9a:0; },
        56: function(){             // 8
                // lt gray
                return cbm?0x9b:0; },
       112: function(){             // F1
                return modeDOORWAY?'\x00\x68':0; },
       113: function(){             // F2
                return modeDOORWAY?'\x00\x69':0; },
       114: function(){             // F3
                return modeDOORWAY?'\x00\x6A':0; },
       115: function(){             // F4
                return modeDOORWAY?'\x00\x6B':0; },
       116: function(){             // F5
                return modeDOORWAY?'\x00\x6C':0; },
       117: function(){             // f6
                return modeDOORWAY?'\x00\x6D':0; },
       118: function(){             // f7
                return modeDOORWAY?'\x00\x6E':0; },
       119: function(){             // f8
                return modeDOORWAY?'\x00\x6F':0; },
       120: function(){             // f9
                return modeDOORWAY?'\x00\x70':0; },
       121: function(){             // f10
                return modeDOORWAY?'\x00\x71':0; },
    },
    keysS1C0A1 = {  // SHIFT+ALTed keus. (currently empty)
    },
    keysS0C1A1 = {  // CTRL+ALTed keys.
        46: null        // delete
    },
    keysS1C1A1 = {  // SHIFT+CTRL+ALTed keys.  (currently empty)
    },

    keyVals = [
        keysS0C0A0, keysS1C0A0, keysS0C1A0, keysS1C1A0,
        keysS0C0A1, keysS1C0A1, keysS0C1A1, keysS1C1A1 ],

    // all available to vtx client
    vtxFonts = [
        'UVGA16', 'MICROKNIGHT', 'MICROKNIGHTPLUS',
        'MOSOUL', 'P0TNOODLE', 'TOPAZ', 'TOPAZPLUS',
        'VIC200', 'VIC201', 'C640', 'C641', 'C1280',
        'C1281', 'ATARI', 'TI994' ],
        
    // telnet commands
    TN_IS           = 0x00,
    TN_SEND         = 0x01,
    TN_BIN          = 0x00,
    TN_ECHO         = 0x01,
    TN_RECONNECT    = 0x02,
    TN_SGA          = 0x03,
    TN_STATUS       = 0x05,
    TN_TMARK        = 0x06,
    TN_SLOC         = 0x17,
    TN_TTYPE        = 0x18,
    TN_NAWS         = 0x1F,
    TN_TSPEED       = 0x20,
    TN_RFC          = 0x21,
    TN_LM           = 0x22,
    TN_XLOC         = 0x23,
    TN_EVARS        = 0x24,
    TN_AUTH         = 0x25,
    TN_NEWE         = 0x27,
    TN_SE           = 0xF0,
    TN_NOP          = 0xF1,
    TN_DM           = 0xF2,
    TN_BRK          = 0xF3,
    TN_IP           = 0xF4,
    TN_AO           = 0xF5,
    TN_AYT          = 0xF6,
    TN_EC           = 0xF7,
    TN_EL           = 0xF8,
    TN_GA           = 0xF9,
    TN_SB           = 0xFA,
    TN_WILL         = 0xFB,
    TN_WONT         = 0xFC,
    TN_DO           = 0xFD,
    TN_DONT         = 0xFE,
    TN_IAC          = 0xFF,

    // TELNET states for Q method
    // option is enabled ONLY IF state is TNS_YES
    TNQ_NO          = 0,
    TNQ_YES         = 1,
    TNQ_WANTNO      = 2,
    TNQ_WANTYES     = 3,
    TNQ_WANTNO_OP   = 4,
    TNQ_WANTYES_OP  = 5,

    // for ymodem crc16 
    CRC_POLY        = 0x1021;
    
let
    // strings that get transmogrified by the HTTP server.
    // only change these if you are not using the VTX HTTP server.
    codePage = vtxdata.codePage,  
    wsConnect = vtxdata.wsConnect,
    crtCols = vtxdata.crtCols,   
    crtRows = vtxdata.crtRows,
    xScale = vtxdata.xScale,     
    term = vtxdata.term,         
    cbm = (vtxdata.term == 'PETSCII'),
    atari = (vtxdata.term == 'ATASCII'),
    initStr = vtxdata.initStr,   

    cbmColors,                      // the correct color palette for a PETSCII

    ws = null,                  // websocket connection.

    // timers / intevals
    irqWriteBuffer = null,      // print buffer (33ms)
    irqCheckResize = null,
    irqCursor = null,
    irqBlink = null,

    fontName,                   // font used
    fontSize,                   // font size to use
    vtxFontsLoaded,             // bools of loaded vtxFonts

    
    rowSize,                    // character size
    colSize,                    // cell width in pixels
    crtWidth,                   // crt width in pixels
    pageWidth,                  // with of html in pixels

    pagePos,
    pageLeft,                   // left position of page div.
    pageTop,                    // top position of page div.

    elPage = document.getElementsByTagName('html')[0],
    crsr,                       // cursor element
    crsrRow,                    // cursor position
    crsrCol,                    // ''
    crsrSaveRow = 0,            // saved position
    crsrSaveCol = 0,            // ''
    cellSaveAttr = 0,           // save attribute (ESC 7 / ESC 8)
    pageAttr,                   // current page attributes
    crsrAttr,                   // color of cursor (only fg used)
    crsrBlink,                  // cursor blink state
    crsrSkipTime,               // skip cursor draws on heavy character output
    cellAttr,                   // current active attributes
    cellBlinkSlow,              // text blink states
    cellBlinkFast,
    defPageAttr,                // default page
    defCrsrAttr,                // default crsr
    defCellAttr,                // default cell attributes.
    defRowAttr = 0x002c0000,    // def row attr
    lastChar,                   // last printable character outputed.
    lastHotSpot = null,         // last mouseover hotspot
    hotspotAttr = 0x0C0F040B,   // hotspot colors
    conBaud = 0,                // baud emulation speed.
    audio,                      // audio element

    termState,                  // TS_...

    pageDiv = null,             // page contents div
    ctrlDiv = null,             // controls panel
    textDiv = null,             // text plane
    soundBell = null,           // bell sound
    textPos = null,             // ul x,y of textdiv

    // ansi parsing vars
    parms = '',                 // parameters for CSI
    interm = '',                // intermediate for CSI
    ansiState = 0,

    // mode switches
    modeVTXANSI = false,        // CSI ?50 h/l to switch out of old ANSI.SYS mode.
    modeBlinkBright = false,    // CSI ?33 h/l to switch blink for bright background.
    modeCursor = true,          // CSI ?25 h/l to turn cursor on / off.
    modeBoldFont = false,       // CSI ?31 h/l to use font 1 for bold.
    modeNoBold = false,         // CSI ?32 h/l to disallow bold.
    modeBlinkFont = false,      // CSI ?34 h/l to use font 2 for blink.
    modeNoBlink = false,        // CSI ?35 h/l to disallow blink.
    modeCBMShift = true,        // PETSCII shift enabled
    modeDOORWAY = false,        // DOORWAY mode
    modeAutoWrap = true,        // Autowrap Mode
    modeNextGlyph = false,      // if DOORWAY mode, print glyph associated with this byte!
    modeRegionOrigin,           // origin in region?

    // scroll region info.
    regionTopRow,               // top row of scroll region.
    regionBottomRow,            // bottom row of scroll region.

    // display buffer.
    conBuffer = '',             // console output buffer.

    // Attrs are integer arrays, base 0 (i.e.: row 1 = index 0)
    conRowAttr  = [],           // row attributes array of number
    conCellAttr = [],           // character attributes array of array or number
    conText = [],               // raw text - array of string
    conHotSpots = [],           // clickable hotspots
    spriteDefs = [],            // sprite definitions - contains url / or data url
    audioDefs = [],             // audio definitions - type 0 coded. 1,2 TODO
    conTabStop = [],            // programmable horizontal tab stops. (used in ATASCII for now)
    
    // array 0..15 (0-9:ANSI selectable,10/11:special,12-15:reserved)
    conFont = [],               // the 10 fonts used for CSI 10-19 m
    conFontCP = [],             // associated code page for font.
    conFontNum = 0,             // current font being used.

    ovl = {},                   // overlay dialog stuff for file transfers

    // keyboard meta key states
    shiftState, ctrlState, altState,
    numState, capState, scrState,

    tnState,        // telnet read state (IAC cmds)
    tnCmd,          // current telnet command
    tnQUs   = new Uint8Array(256),  // tables for telnet q method
    tnQHim  = new Uint8Array(256),

    // YModem rigmarole
    ymTimer,
    ymCCount,
    ymPacketSize,               // current size of packet.
    ymPacketPos,                // current position in buffer.
    ymPacketBuff = [],          // buffer for send / receive.
    ymFileName,
    ymFileSize,                 // file size. -1 if unknown.
    ymModifiedDate,             // secs since 1/1/1970 in octal
    ymFilePos,                  // current position in file.
    ymFileData = new Blob([], {type: 'application/octet-stream'}),
    ymEOTCount,
    ymNakCount,
    ymNextBlock,
    ymSendStartTime;            // timer for when to abort

    
// add event listener
function addListener(obj, eventName, listener) {
    if(obj.addEventListener)
        obj.addEventListener(eventName, listener, false)
    else
        obj.attachEvent("on" + eventName, listener);
}

// create an element
function domElement(type, options, styles, txt) {
    var
        e = document.createElement(type),
        i;

    if (options)
        for (i in options)
            e[i] = options[i];
    if (styles)
        for (i in styles)
            e.style[i] = styles[i];
    if (txt)
        e.appendChild(document.createTextNode(txt));
    return e;
}

// load fonts and boot
function bootVTX() {
    var
        i, j, l, str,
        el,
        t = document.getElementsByTagName('title')[0],
        hd = document.getElementsByTagName('head')[0],
        bootFonts,
        bootTimer,
        bootDiv = [],
        testDiv;

    // load fonts
    // set fonts to be loaded based on term
    vtxFontsLoaded = new Array(vtxFonts.length);
    vtxFontsLoaded.fill(false);
    bootFonts = [];
    switch (vtxdata.term) {
        case 'PETSCII':
            switch (vtxdata.codePage) {
                case 'VIC20':
                    bootFonts.push('VIC200');
                    bootFonts.push('VIC201');
                    break;
                
                
                case 'C128':
                    bootFonts.push('C1280');
                    bootFonts.push('C1281');
                    break;
                
                case 'C64':
                default:
                    bootFonts.push('C640');
                    bootFonts.push('C641');
                    break;
            }
            break;
        
        case 'ATASCII':
            bootFonts.push('ATARI');
            break;
            
        default:
            bootFonts.push('UVGA16');
            break;
    }
    
    // inject @font-faces
    str = '';
    for (i = 0, l = bootFonts.length; i < l; i++) 
        str += '@font-face {\r\n '
            + '  font-family: "' + bootFonts[i] + '"; '
            + '  src: url("' + bootFonts[i] + '.woff") format("woff"); }\r\n';
    el = domElement(
        'style', 
        {   type:   'text/css',
            id:     'vtxfonts'
        }, {}, str );
    hd.appendChild(el);

    // loop through until all fonts needed at boot.
    for (i = 0, l = bootFonts.length; i < l; i++) {
        bootDiv[i] = domElement('div',{},
                    {   fontFamily:         bootFonts[i] + ', AdobeBlank',
                        fontSize:           '24px',
                        fontWeight:         'normal',
                        color:              'transparent',
                        display:            'inline-block',
                        border:             '0px',
                        padding:            '0px',
                        margin:             '0px' },
                    'Test 1..2..3..');
        document.body.appendChild(bootDiv[i]);
    }
    bootTimer = setInterval(
        function(){
            var
                i,
                count = bootFonts.length;

            for (i = bootFonts.length - 1; i >= 0; i--)
                if (bootDiv[i].offsetWidth > 12) {
                    // mark as font loaded.
                    for (j = 0; j < vtxFonts.length; j++)
                        if (vtxFonts[j] == bootFonts[i]) {
                            vtxFontsLoaded[j] = true;
                            break;
                        }
                    count--;
                }

            if (count == 0) {
                for (i = bootFonts.length - 1; i >= 0; i--)
                    document.body.removeChild(bootDiv[i]);
                clearInterval(bootTimer);
                bootVTX2();
            }
        }, 50);
}

function loadSingleFont(fname){
    var 
        fn,
        el,
        str,
        hd = document.getElementsByTagName('head')[0],
        fontTimer,
        testDiv;
        
    // return if it's already loaded.
    fn = vtxFonts.indexOf(fname);
    if ((fn >= 0) && !vtxFontsLoaded[fn]) {
        // inject new @font-faces
        str = '@font-face {\r\n '
            + '  font-family: "' + fname + '"; '
            + '  src: url("' + fname + '.woff") format("woff"); }\r\n';
            
            
        el = document.getElementById('vtxfonts');
        if (el)
            el.innerHTML += str
        else
            el = domElement('style', { type: 'text/css' }, {}, str );
        hd.appendChild(el);

        testDiv = domElement('div',{},
                    {   fontFamily:         fname + ', AdobeBlank',
                        fontSize:           '24px',
                        fontWeight:         'normal',
                        color:              'transparent',
                        display:            'inline-block',
                        border:             '0px',
                        padding:            '0px',
                        margin:             '0px' },
                    'Test 1..2..3..');
        document.body.appendChild(testDiv);

        fontTimer = setInterval(
            function(){
                if (testDiv.offsetWidth > 12) {
                    // mark as font loaded.
                    vtxFontsLoaded[fn] = true;
                    document.body.removeChild(testDiv);
                    clearInterval(fontTimer);
                
                    // redraw everything using this fomt.
                    redrawFont(fname);
                }
            }, 50);
    }
}

// global redraw a font change based on font name (i.e. VIC200, etc)
function redrawFont(fname) {
    var
        fn,
        tfn,
        r, c;

    // convert fname to font number
    fn = conFont.indexOf(fname);
    if (fn >= 0) {
        for (r = conRowAttr.length-1; r >= 0; r--)
            for (c = conCellAttr[r].length - 1; c >= 0; c--) {
                tfn = (conCellAttr[r][c] & A_CELL_FONT_MASK) >>> 28;
                if (tfn == fn) 
                    renderCell(r, c);
            }
    }
}

function bootVTX2() {
    var
        t = document.getElementsByTagName('title')[0],
        hd = document.getElementsByTagName('head')[0];

    // wait for the required data.
    while (!vtxdata){};

    // format the TITLE tag - only if empty or missing.
    if (!t)
        hd.appendChild(domElement('title',{},{},vtxdata.sysName));
    else {
        if (!t.innerText || (t.innerText == ''))
            t.innerText = vtxdata.sysName
    }

    // when all fonts loaded call initDisplay
    window.setTimeout(initDisplay, 100);
}

// string splice - why this is not standard is beyond me.
// ECMA-262 / 15.4.4.12 - prototype.splice
if (!String.prototype.splice) {
    String.prototype.splice = function (start, deleteCount, item) {
        if (start < 0) {
            start = this.length + start;
            if (start < 0)
                start = 0;
        }
        return this.slice(0, start) + (item || '') + this.slice(start + deleteCount);
    }
}

// which row is the mouse on?
function getMouseCell(e) {
    var
        x, y,
        size, width, c, rh,
        ty, dt, i;

    //ty = document.documentElement.scrollTop || document.body.scrollTop;
    ty = 0;
    x = e.clientX;
    y = e.clientY + ty;
    dt = textPos.top;
    if ((y >= dt) && (x >= textPos.left) && (x < textPos.left + crtWidth)) {
        // on the page. find row
        for (i = 0; i < conRowAttr.length; i++) {
            size = getRowAttrHeight(conRowAttr[i]) / 100;
            width = getRowAttrWidth(conRowAttr[i]) / 100;
            rh = Math.round(fontSize * size);
            if ((y >= dt) && (y < (dt + rh))) {
                // on this row. get col
                c = (x - textPos.left) / (xScale * colSize * width);
                return { row: i, col: Math.floor(c) };
            }
            dt += rh;
        }
    }
    // off the console
    return null;
}

// get the hotspot under x, y
function getHotSpot(e) {
    var
        i, mpos, hs;

    mpos = getMouseCell(e);
    if (mpos) {
        // adjust to base-1 ansi coords
        for (i = 0; i < conHotSpots.length; i++) {
            hs = conHotSpots[i];
            if ((mpos.row >= hs.row)
                && (mpos.row < hs.row + hs.height)
                && (mpos.col >= hs.col)
                && (mpos.col < hs.col + hs.width))
                return conHotSpots[i];
        }
    }
    return null;
}

function mouseUp(e) {
    // for now, just fix meta key states
    e = e || window.event;
    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;
}

function mouseDown(e) {
    // for now, just fix meta key states
    e = e || window.event;
    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;
}

function mouseMove(e) {
    var
        x, y,
        hs;

    // for now, just fix meta key states
    e = e || window.event;
    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;

    if (termState > TS_NORMAL) return;

    // check if over a hotspot
    hs = getHotSpot(e);
    if (hs) {
        if (lastHotSpot != hs) {
            if (lastHotSpot) {
                // erase old
                for (y = 0; y < lastHotSpot.height; y++)
                    for (x = 0; x < lastHotSpot.width; x++)
                        renderCell(lastHotSpot.row+y, lastHotSpot.col+x);
            }
            // draw this one
            for (y = 0; y < hs.height; y++)
                for (x = 0; x < hs.width; x++)
                    renderCell(hs.row+y, hs.col+x, hs.hilite?1:0);
        }
        document.body.style['cursor'] = 'pointer'
    } else {
        if (lastHotSpot) {
            // erase old
            for (y = 0; y < lastHotSpot.height; y++)
                for (x = 0; x < lastHotSpot.width; x++)
                    renderCell(lastHotSpot.row+y, lastHotSpot.col+x);
        }
        document.body.style['cursor'] = 'default';
    }
    lastHotSpot = hs;
}

function click(e) {
    var
        x, y,
        win,
        hs;

    // for now, just fix meta key states
    e = e || window.event;
    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;

    if (termState > TS_NORMAL) return;

    hs = getHotSpot(e);
    if (hs) {
        // draw this one
        for (y = 0; y < hs.height; y++)
            for (x = 0; x < hs.width; x++)
                renderCell(hs.row+y, hs.col+x, hs.hilite?2:0);

        // clicked on hotspot.
        switch (hs.type) {
            case 0:
                sendData(hs.val);
                break;

            case 1:
                // url
                win = window.open(hs.val, '_blank');
                win.focus();
                break;
        }
    }
}

// process keyups (function, arrows, etc)
function keyUp(e) {
    var
        kc;

    e = e || window.event;
    kc = e.keyCode || e.which;
    crsr.style['display'] = 'block';

    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;
}

// process keydowns (function, arrows, etc)
function keyDown(e) {
    var
        stateIdx,
        kc, ka;

    e = e || window.event;
    kc = e.keyCode || e.which;
    crsr.style['display'] = 'block';

    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;

    if (termState > TS_NORMAL) return;

    stateIdx = (shiftState ? 1 : 0) + (ctrlState ? 2 : 0) + (altState ? 4 : 0);

    // translate for capslock
    if ((kc >= 65) && (kc <= 90) && (stateIdx < 2) && capState)
        stateIdx ^= 1;

    // reverse for PETSCII
    if (cbm) {
        if ((kc >= 65) && (kc <= 90))
            stateIdx ^= 1;
    }

    ka = keyVals[stateIdx][kc];
    if (ka == null) {
        // let browser handle it.
        return (e.returnValue = true);

    } else if (typeof ka == 'function') {
        ka = ka();
        if (ka == null)
            return (e.returnValue = true);
    }

    if (typeof ka == 'string') {
        // send string to console.
        sendData(ka);
        e.preventDefault();
        return (e.returnValue = false); // true

    } else if (typeof ka == 'number')
    {
        if (ka == 0) {
            // never do anything.
            e.preventDefault();
            return (e.returnValue = true);

        } else if (ka < 0) {
            // perform special action.
            switch (ka) {
                case DO_CAPLK:
                    capState = !capState;
                    setBulbs();
                    break;

                case DO_NUMLK:
                    numState = !numState;
                    setBulbs();
                    break;

                case DO_SCRLK:
                    scrState = !scrState;
                    setBulbs();
                    break;

                default:
                    // unknown action - pass to keyPress
                    return;
            }
            e.preventDefault();
            return (e.returnValue = false);

        } else if (ka > 0) {
            // send ascii if online, send to server. if offline, localecho
            sendData(ka);
            e.preventDefault();
            return (e.returnValue = false);
        }
    }
}

// process keypresses (alphas, numerics, etc)
function keyPress(e) {
    // normally, send to websocket server. only echo what is returned.
    var
        cc;

    e = e || window.event;
    cc = e.charCode;

    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;

    capState = (((cc >= 65) && (cc <= 90) && !shiftState)
            || ((cc >= 97) && (cc <= 112) && shiftState));
}

// delete row from storage and element from html
// TODO - check scroll region
function delRow(rownum) {
    var
        els, p;

    expandToRow(rownum);
    els = document.getElementsByClassName('vtx');
    p = els[rownum - 1].parentNode;
    p.removeChild(els[rownum]);
    conRowAttr.splice(rownum, 1);
    conText.splice(rownum, 1);
    conCellAttr.splice(rownum, 1);

    // move all hotspots below up one. remove hotspots on this row.
    clearHotSpotsRow(rownum, 0, 999);
    moveHotSpotsRows(rownum + 1, conRowAttr.length, -1);
}

// insert row into storage and element into html
// TODO - check scroll region
function insRow(rownum) {
    var
        els, p;

    els = document.getElementsByClassName('vtx');
    p = els[rownum].parentNode;
    p.insertBefore(createNewRow(), els[rownum]);
    conRowAttr.splice(rownum, 0, defRowAttr);
    conText.splice(rownum, 0, '');
    conCellAttr.splice(rownum, 0, []);

    // move all hotspots on this row and below down one.
    moveHotSpotsRows(rownum, conRowAttr.length, +1);
    trimHistory();
}

// remove excess rows from top.
function trimHistory(){
    var
        els,
        p,
        i,
        hs;

    while (conRowAttr.length > vtxdata.crtHistory) {
        clearHotSpotsRow(0, 0, 999);
        els = document.getElementsByClassName('vtx');
        p = els[0].parentNode;
        p.removeChild(els[0]);
        conRowAttr.splice(0, 1);
        conText.splice(0, 1);
        conCellAttr.splice(0, 1);
        crsrRow--;
    }
}

// delete a character at position
function delChar(rownum, colnum) {
    expandToRow(rownum);
    expandToCol(rownum, colnum);
    conText[rownum] = conText[rownum].splice(colnum, 1);
    conCellAttr[rownum].splice(colnum, 1);
    moveHotSpotsRow(rownum, colnum, 999, -1);
    redrawRow(rownum);
}

// insert a character at position. also sets attr to def
function insChar(rownum, colnum, chr) {
    expandToRow(rownum);
    expandToCol(rownum, colnum);
    conText[rownum] = conText[rownum].splice(colnum, 0, String.fromCharCode(chr));
    conCellAttr[rownum].splice(colnum, 0, defCellAttr);
    moveHotSpotsRow(rownum, colnum, 999, +1);
    redrawRow(rownum);
}

// create blank row
function createNewRow() {
    var
        el = domElement('div', { className: 'vtx' }),
        cnv = domElement('canvas');

    //el.appendChild(cnv);
    return el;
}

// is this character a printable?
function isprint(chr) {
    if (chr < 32) return false;                 // C0 controls
    if (chr >= 128 && chr < 160) return false;   // C1 controls
    return true;
}

// compute number of visible cells on this row.
function colsOnRow(rownum) {
    var
        cols = crtCols,
        width = getRowAttrWidth(conRowAttr[rownum]) / 100;

    cols *= 1 / width;
    return cols;
}

// clamp value to range. with optional fallback if out of bounds.
function minMax(v, min, max, fallback) {
    if (fallback == null) {
        if (v < min) v = min;
        if (v > max) v = max;
    } else {
        if (v < min) v = fallback;
        if (v > max) v = fallback;
    }
    return v;
}

// flush out parameter array with defaults.
// truncate to match defs length
function fixParams(parm, defs) {
    var
        i;
    for (i = parm.length; i < defs.length; i++)
        parm[i] = defs[i];
    return parm.slice(0, defs.length);
}

// get actual document position of element
function getElementPosition(obj) {
    var
        lpos = obj.offsetLeft,
        tpos = obj.offsetTop;

    while (obj.offsetParent) {
        obj = obj.offsetParent;
        lpos += obj.offsetLeft;
        tpos += obj.offsetTop;
    }
    return { left: lpos, top: tpos };
}

// redraw the cursor. - attempt to scroll
function crsrDraw(force) {
    var
        row, rpos,
        csize,
        dt, wh;

    force = force || false;

    expandToRow(crsrRow);
    row = getRowElement(crsrRow);
    if (row == null) return;

    // position of row in relation to parent.
    rpos = getElementPosition(row);

    // position of doc
    dt = document.documentElement.scrollTop || document.body.scrollTop;

    wh = "innerHeight" in window
        ? window.innerHeight
        : document.documentElement.offsetHeight;

    // character size for this row.
    csize = getRowFontSize(crsrRow);

    // set cursor siz / pos
    crsr.style['top'] =     (rpos.top - pageTop) + 'px';
    crsr.style['left'] =    (xScale * crsrCol * csize.width) + 'px';
    crsr.style['width'] =   (xScale * csize.width) + 'px';
    crsr.style['height'] =  csize.height + 'px';

    if (rpos.top < dt) {
        // cursor is above page - scroll up
        window.scrollTo(0, rpos.top - 8);
    } else if ((rpos.top + csize.height) > (dt + wh)) {
        // cursor is below page - scroll down
        window.scrollTo(0, rpos.top + csize.height + 8);
    }
}

// get default font size for page
function getDefaultFontSize() {
    // look for font-width: in body
    var
        cs, font, textSize, h, w,
        x, y, d, i,
        txtTop, txtBottom, txtLeft, txtRight,
        testString = '',
        data, ctx, canvas,
        bmpw = 2000,
        bmph = 64;

    testString += '\u2588\u2584\u2580\u2590\u258c\u2591\u2592\u2593';
    for (i = 32; i < 128; i++)
        testString += String.fromCharCode(i);
    testString += '\u2588\u2584\u2580\u2590\u258c\u2591\u2592\u2593';

    //cs = document.defaultView.getComputedStyle(document.body, null);
    cs = document.defaultView.getComputedStyle(pageDiv, null);
    fontName = cs['font-family'];
    fontSize = parseInt(cs['font-size']);
    font = fontSize + 'px ' + fontName;

    // interrogate font
    canvas = domElement(
        'canvas',
        {   width:  bmpw,
            height: bmph });
    ctx = canvas.getContext('2d');
    ctx.font = font;
    ctx.textBaseline = 'top';
    ctx.textAlign = 'left';
    ctx.fillStyle = '#FFF';
    ctx.fillText(testString, 0, 0);

    data = ctx.getImageData(0, 0, bmpw, bmph).data;
    txtTop = txtLeft = bmpw;
    txtRight = txtBottom = 0;
    for (y = 0; y < bmph; y++)
        for (x = 0; x < bmpw; x++) {
            d = data[(y * bmpw+ x) * 4];
            if (d != 0) {
                if (y < txtTop)
                    txtTop = y;
                if (y > txtBottom)
                    txtBottom = y;
                if (x < txtLeft)
                    txtLeft = x;
                if (x > txtRight)
                    txtRight = x;
            }
        }
    // middle ground between Mozilla and Chromium
    colSize = Math.round(((txtRight - txtLeft) / testString.length) - 0.25);
    rowSize = Math.floor(txtBottom - txtTop) + 1;
}

// compute size of text based on text and font.
function getTextWidth(text, font) {
    // re-use canvas object for better performance
    // probably not called enough to make a difference.
    var
        canvas = document.createElement('canvas'),
        context, metrics;

    context = canvas.getContext('2d');
    context.font = font;
    context.textBaseline='alphabetic';
    context.textAlign='left';
    metrics = context.measureText(text);
    canvas = null;
    return [metrics.width, metrics.height];
}

// get maximum row on document.
function getMaxRow() {
    return conRowAttr.length - 1;
}

// get row length for a row
function getMaxCol(rownum) {
    return conText[rownum].length - 1;
}

// home cursor
function crsrHome() {
    crsrRow = crsrCol = 0; // base 0
    crsrDraw();
}

// move cursor
function crsrMove(rownum, colnum) {
    crsrRow = rownum;
    crsrCol = colnum;
    crsrDraw();
}

// cursor up
function crsrUp() {
    if (crsrRow > 0)
        crsrRow--;
    crsrDraw();
}

// cursor down
function crsrDown() {
    crsrRow++;
    crsrDraw();
}

// cursor left
function crsrLeft() {
    if (crsrCol > 0)
        crsrCol--;
    crsrDraw();
}

// cursor right
function crsrRight() {
    crsrCol++;
    crsrDraw();
}

// return element for row
function getRowElement(row) {
    var
        els = document.getElementsByClassName('vtx');

    if (row > els.length)
        return null
    return els[row];
}

// called often to fix cursor on zoom / page resize
function doCheckResize() {
    // page resize?
    textPos = textDiv.getBoundingClientRect();
    if (elPage.clientWidth != pageWidth) {
        pageWidth = elPage.clientWidth;
        crsrDraw(true);
    }
    ctrlDiv.style['left'] = (6 + textPos.left + (crtWidth*xScale)) + 'px';
}

// blink cursor (533ms is cursor blink speed based on DOS VGA).
function doCursor() {
    if (cbm)
        crsr.firstChild.style['background-color'] =
            ((crsrBlink = !crsrBlink) || (!modeCursor)) ?
            'transparent' :
            cbmColors[cellAttr & 0xF]
    else if(atari)
        crsr.firstChild.style['background-color'] =
            ((crsrBlink = !crsrBlink) || (!modeCursor)) ?
            'transparent' :
            atariColors[cellAttr & 0x1]
    else
        crsr.firstChild.style['background-color'] =
            ((crsrBlink = !crsrBlink) || (!modeCursor)) ?
            'transparent' :
            ansiColors[getCrsrAttrColor(crsrAttr)];
}

// animate blink (533ms)
function doBlink(){
    var
        r, c, y, rh;

    if (!modeNoBlink && !modeBlinkBright) {
        // y of first row.
        y = textDiv.getBoundingClientRect().top;
        for (r = 0; r < conRowAttr.length; r++) {
            // check if row is visible
            rh = rowSize * getRowAttrHeight(conRowAttr[r]) / 100;
            if ((y + rh > 0) && (y < window.innerHeight)) {
                // look for blink
                // refresh blinkable text.
                for (c = 0; c < conCellAttr[r].length; c++) {
                    if (conCellAttr[r][c] & (A_CELL_BLINKSLOW | A_CELL_BLINKFAST))
                        renderCell(r, c);
                }
            }
            y += rh;
        }
    }
    cellBlinkFast = !cellBlinkFast;
    if (cellBlinkFast)
        cellBlinkSlow = !cellBlinkSlow;
}

// compute font size (width) for row - figure in scale (row is dom element)
function getRowFontSize(rownum) {
    var
        w, h, rattr;

    rattr = conRowAttr[rownum];
    w = colSize * (getRowAttrWidth(rattr) / 100);
    h = rowSize * (getRowAttrHeight(rattr) / 100);
    return { width: w, height:h };
}

// convert 4 digit base64 str to 24bit int
function btoi(str) {
    var
        digits = str.split('');
    return  (b64.indexOf(digits[0]) << 18) +
            (b64.indexOf(digits[1]) << 12) +
            (b64.indexOf(digits[2]) <<  6) +
            (b64.indexOf(digits[3]));
}

// convert 24bit int to 4 digit base64
function itob(val) {
    var 
        str = b64c[val & 0x3f];
        
    val >>= 6;
    str = b64c[val & 0x3f] + str;
    val >>= 6;
    str = b64c[val & 0x3f] + str;
    val >>= 6;
    return b64c[val & 0x3f] + str;
}

// concert integer to hex string.
function itoh(i, d) {
    var
        str = '';

    d = d || 0;
    for (; i; str = hex.charAt(i & 0x0F) + str, i >>= 4){};
    if (d > 0)
        while (str.length < d) str = '0' + str;
    return str;
}

// convert hex string to integer
function htoi(h) {
    var
        i, l, v;
        
    h = h || '0';
    for (h = h.toUpperCase(), v = 0, i = 0, l = h.length; i < l; i++) {
        v <<= 4;
        v += hex.indexOf(h.charAt(i));
    }
    return v;
}

// create row attribute
function makeRowAttr(c1, c2, bp, size, width, marquee) {

    bp = bp || 0;
    size = size || 100;
    width = width || 100;
    marquee = marquee || false;

    var v;
    
    // round size to nearest valid 25%
    size = Math.round(size / 25) - 1;
    if (size < 0) size = 0;
    if (size > 7) size = 7;
    size <<= 18;

    // round width to nearest valid 50%
    width = Math.round(width / 50) - 1;
    if (width < 0) width = 0;
    if (width > 3) width = 3;
    width <<= 21;

    v = (c1 & 0xFF)
        | ((c2 & 0xFF) << 8)
        | bp
        | size
        | width
        | (marquee ? A_ROW_MARQUEE : 0);

    return v;
}

function setPageAttrBorder(attr, color) {
    return (attr & 0x00FF) | ((color & 0xFF) << 8);
}
function setPageAttrBackground(attr, color) {
    return (attr & 0xFF00) | (color & 0xFF);
}

// set row attribute parts
function setRowAttrColor1(attr, color1) {
    return (attr & ~A_ROW_COLOR1_MASK) | (color1 & 0xFF);
}
function setRowAttrColor2(attr, color2) {
    return (attr & ~A_ROW_COLOR2_MASK) | ((color2 & 0xFF) << 8);
}
function setRowAttrPattern(attr, pattern) {
    return (attr & ~A_ROW_PATTERN_MASK) | pattern;
}
function setRowAttrHeight(attr, height) {
    // round size to nearest valid 25%
    height = Math.round(height / 25) - 1;
    if (height < 0) height = 0;
    if (height > 7) height = 7;
    height <<= 18;
    return (attr & ~A_ROW_HEIGHT_MASK) | height;
}
function setRowAttrWidth(attr, width) {
    // round width to nearest valid 50%
    width = Math.round(width / 50) - 1;
    if (width < 0) width = 0;
    if (width > 3) width = 3;
    width <<= 21;

    return (attr & ~A_ROW_WIDTH_MASK) | width;
}
function setRowAttrMarquee(attr, marquee) {
    return (attr & ~A_ROW_MARQUEE) | (marquee ? A_ROW_MARQUEE : 0);
}

// get row attribute parts
function getRowAttrColor1(attr) { return attr & 0xFF; }
function getRowAttrColor2(attr) { return (attr >> 8) & 0xFF; }
function getRowAttrPattern(attr) { return attr & A_ROW_PATTERN_MASK; }
function getRowAttrHeight(attr) { return (((attr & A_ROW_HEIGHT_MASK) >> 18) + 1) * 25.0; }
function getRowAttrWidth(attr) { return (((attr & A_ROW_WIDTH_MASK) >> 21) + 1) * 50.0; }
function getRowAttrMarquee(attr) { return (attr & A_ROW_MARQUEE) != 0; }

// create cell attribute
function makeCellAttr(fg, bg, bold, italics, underline, blinkslow, shadow,
    strikethrough, outline, blinkfast, faint, font) {

    fg = fg || 7;
    bg = bg || 0;
    bold = bold || false;
    italics = italics || false;
    underline = underline || false;
    blinkslow = blinkslow || false;
    shadow = shadow || false;
    strikethrough = strikethrough || false;
    outline = outline || false;
    blinkfast = blinkfast || false;
    faint = faint || false;
    font = (font || 0) & 0xF;
    return (fg & 0xFF)
        | ((bg & 0xFF) << 8)
        | (bold ? A_CELL_BOLD : 0)
        | (italics ? A_CELL_ITALICS : 0)
        | (underline ? A_CELL_UNDERLINE : 0)
        | (blinkslow ? A_CELL_BLINKSLOW : 0)
        | (shadow ? A_CELL_SHADOW : 0)
        | (strikethrough ? A_CELL_STRIKETHROUGH : 0)
        | (outline ? A_CELL_OUTLINE : 0)
        | (blinkfast ? A_CELL_BLINKFAST : 0)
        | (faint ? A_CELL_FAINT : 0)
        | (font << 28)
        ;
}

// set cell attribute parts
function setCellAttrFG(attr, color) {
    return (attr & ~A_CELL_FGCOLOR_MASK) | (color & 0xFF);
}
function setCellAttrBG(attr, color) {
    return (attr & ~A_CELL_BGCOLOR_MASK) | ((color & 0xFF) << 8);
}
function setCellAttrBold(attr, bold) {
    return (attr & ~A_CELL_BOLD) | (bold ? A_CELL_BOLD : 0);
}
function setCellAttrItalics(attr, italics) {
    return (attr & ~A_CELL_ITALICS) | (italics? A_CELL_ITALICS : 0);
}
function setCellAttrUnderline(attr, underline) {
    return (attr & ~A_CELL_UNDERLINE) | (underline? A_CELL_UNDERLINE : 0);
}
function setCellAttrBlinkSlow(attr, blink) {
    return (attr & ~A_CELL_BLINKSLOW) | (blink ? A_CELL_BLINKSLOW : 0);
}
function setCellAttrBlinkFast(attr, blink) {
    return (attr & ~A_CELL_BLINKFAST) | (blink ? A_CELL_BLINKFAST : 0);
}
function setCellAttrShadow(attr, shadow) {
    return (attr & ~A_CELL_SHADOW) | (shadow ? A_CELL_SHADOW : 0);
}
function setCellAttrStrikethrough(attr, strikethrough) {
    return (attr & ~A_CELL_STRIKETHROUGH) | (strikethrough ? A_CELL_STRIKETHROUGH : 0);
}
function setCellAttrOutline(attr, outline) {
    return (attr & ~A_CELL_OUTLINE) | (outline ? A_CELL_OUTLINE: 0);
}
function setCellAttrReverse(attr, reverse) {
    return (attr & ~A_CELL_REVERSE) | (reverse ? A_CELL_REVERSE : 0);
}
function setCellAttrDisplay(attr, display) {
    return (attr & ~A_CELL_DISPLAY_MASK) | (display & A_CELL_DISPLAY_MASK);
}
function setCellAttrFaint(attr, faint) {
    return (attr & ~A_CELL_FAINT) | (faint ? A_CELL_FAINT : 0);
}
function setCellAttrFont(attr, font) {
    return (attr & ~A_CELL_FONT_MASK) | ((font & 0xF) << 28);
}

// get cell attribute parts
function getCellAttrFG(attr) { return attr & 0xFF; }
function getCellAttrBG(attr) { return (attr >> 8) & 0xFF; }
function getCellAttrBold(attr) { return (attr & A_CELL_BOLD) != 0; }
function getCellAttrItalics(attr) { return (attr & A_CELL_ITALICS) != 0; }
function getCellAttrUnderline(attr) { return (attr & A_CELL_UNDERLINE) != 0; }
function getCellAttrBlinkSlow(attr) { return (attr & A_CELL_BLINKSLOW) != 0; }
function getCellAttrBlinkFast(attr) { return (attr & A_CELL_BLINKFAST) != 0; }
function getCellAttrShadow(attr) { return (attr & A_CELL_SHADOW) != 0; }
function getCellAttrStrikethrough(attr) { return (attr & A_CELL_STRIKETHROUGH) != 0; }
function getCellAttrOutline(attr) { return (attr & A_CELL_OUTLINE) != 0;}
function getCellAttrReverse(attr) { return (attr & A_CELL_REVERSE) != 0; }
function getCellAttrDisplay(attr) { return (attr & A_CELL_DISPLAY) != 0; }
function getCellAttrFaint(attr) { return (attr & A_CELL_FAINT) != 0; }
function getCellAttrFont(attr) { return (attr & A_CELL_FONT_MASK) >> 28; }

// create cursor attributes
function makeCrsrAttr(color, size, orientation){

    color = color || 7;
    size = size || 2;
    orientation = orientation || 0;
    color &= 0xFF;
    size &= 0x03;
    orientation = (orientation ? 1 : 0)
    return (color & 0xFF) | (size << 8) | (orientation << 10);
}

// set cursor attributes
function setCrsrAttrColor(attr, color) {
    return (attr & ~A_CRSR_COLOR_MASK) | (color & 0xFF);
}
function setCrsrAttrSize(attr, size) {
    return (attr & ~A_CRSR_STYLE_MASK) | ((size << 8) & 0x0300);
}
function setCrsrAttrOrientation(attr, orient) {
    return (attr & ~A_CRSR_ORIENTATION) | (orient ? A_CRSR_ORIENTATION : 0x0000);
}

// get cursor attributes
function getCrsrAttrColor(attr) { return attr & 0xFF; }
function getCrsrAttrSize(attr) { return (attr & A_CRSR_STYLE_MASK) >> 8; }
function getCrsrAttrOrientation(attr) { return (attr & A_CRSR_ORIENTATION) >> 10; }

// if row size has changed, resize canvas, redraw row.
function adjustRow(rownum) {
    var
        row, size, width, h, w, x, cnv, i, nw;

    row = getRowElement(rownum);
    size = getRowAttrHeight(conRowAttr[rownum]) / 100;  // .25 - 2
    width = getRowAttrWidth(conRowAttr[rownum]) / 100;  // .5 - 2
    h = rowSize * size;     // height of char
    w = colSize * width;    // width of char

    // get current size
    cnv = row.firstChild;
    if (!cnv){
        // create it.
        cnv = document.createElement('canvas');
        row.appendChild(cnv);
    }

    if (conRowAttr[rownum] & A_ROW_MARQUEE) {
        // marquee are normal width min or text length max
        nw = Math.max(conText[rownum].length * w, crtCols * colSize)
    } else {
        nw= crtCols * colSize;
    }

    if ((cnv.height != (h + 16)) || (cnv.width != nw)) {
        // adjust for new height.
        row.style['height'] = size + 'em';
        cnv.width = nw * xScale;
        cnv.height = (h + 16);
    }
    // redraw this entire row
    redrawRow(rownum);
}

function redrawRow(rownum){
    var
        cnv, ctx, row, size, width, w, h, x, y, i, l;

    expandToRow(rownum);    // in case..

    // redraw this entire row
    l = conText[rownum].length;
    for (i = 0; i < l; i++)
        renderCell(rownum, i);

    // clear end of row
    row = getRowElement(rownum);
    size = getRowAttrHeight(conRowAttr[rownum]) / 100;  // .25 - 2
    width = getRowAttrWidth(conRowAttr[rownum]) / 100;  // .5 - 2
    w = xScale * colSize * width;   // width of char
    h = fontSize * size;            // height of char
    x = w * l;                      // left pos of char on canv

    cnv = row.firstChild;
    if (!cnv) {
        cnv = domElement(
            'canvas',
            {   width:  crtCols * w,
                height: (h + 16) },
            {   zIndex: '50' });
        row.appendChild(cnv);
    }
    ctx = cnv.getContext('2d');
    ctx.clearRect(x, 0, cnv.width - x, cnv.height);
}

// redraw all characters
function renderAll() {
    var
        r, c;
    for (r = 0; r < conRowAttr.length; r++)
        for (c = 0; c < conCellAttr[r].length; c++)
            renderCell(r, c);
}

// render an individual row, col. if forcerev, invert (twice if need be)
// This is the only place we need to convert a character to unicode for
// display!!
// hilight = 0:none, 1:mouse over, 2:mouse click

// force draw cells below on draw of top on tall cells. set bottom true.
// bottom = true if this is the bottom half of the row above.
function renderCell(rownum, colnum, hilight, bottom) {
    var
        row,        // row element drawing to
        size,       // size factor (25%-200%)
        width,      // width factor (50%-200%)
        w, h,       // width / height of this character
        x,          // position on canvas
        drawtxt,    // flag for if text is drawn this cell
        cnv,        // canvas to draw upon
        ctx,        // canvas context
        attr,       // attributes for this character
        ch,         // character to draw
        tfg,        // this cell foreground color
        tbg,        // this cell background color
        tbold,      // this cell bold?
        stroke,     // thickness of underline / strikethrough
        tmp,
        tblinks,    // this cell blinks?
        tfnt,       // font number for this cell
        i, j, l,    // index / length
        rowadj,     // row index adjustment used for double height bottoms
        tall, 
        teletext,
        xskew,      // skew amount for italics
        xadj,       // x adjustment to render character
        yadj,       // y adjustment to render character
        yScale;     // y scale for tall characters

    hilight = hilight || 0;     // flag for mouse over drawing. 0=normal,
                                // 1=mouseover,2=click
    bottom = bottom || false;   // force draw bottoms of double. if this is set
                                // draw the bottom haft of character in row 
                                // above this row.
    rowadj = bottom?-1:0;       // adjustment for retreiving character info.
    
    // quick range check
    if (rownum + rowadj > conRowAttr.length)         return;
    if (colnum >= conText[rownum + rowadj].length)   return;

    // get size of this row
    size = getRowAttrHeight(conRowAttr[rownum + rowadj]) / 100; // .25 - 2
    width = getRowAttrWidth(conRowAttr[rownum + rowadj]) / 100; // .5 - 2
    
    w = xScale * colSize * width;   // width of char
    x = w * colnum;                 // left pos of char on canv

    // don't render off page unless marquee
    if ((x > w * crtCols) && !(conRowAttr[rownum] & A_ROW_MARQUEE))
        return;

    // compute height    
    row = getRowElement(rownum);
    h = fontSize * size;            // height of char
    stroke = h * 0.1;               // underline/strikethrough size
    cnv = row.firstChild;           // get canvas
    if (!cnv) {
        // create new canvas if nonexistant
        cnv = domElement(
            'canvas',
            {   width:  crtCols * w,
                height: (h+16) },
            {   zIndex: '50' });
        row.appendChild(cnv);
    }
    ctx = cnv.getContext('2d');

    // get char and attributes
    ch = conText[rownum + rowadj].charAt(colnum);
    attr = conCellAttr[rownum + rowadj][colnum];

    // extract colors and font to use
    tfg = (attr & 0xFF);
    tbg = (attr >> 8) & 0xff;
    tfnt = ((attr & A_CELL_FONT_MASK) >> 28) & 0xF;

    // get bold and adust if not used
    tbold  = attr & A_CELL_BOLD;
    if (modeNoBold) // CSI ?32 h / l
        tbold = 0;

    // get blink attributes
    tblinks = attr & (A_CELL_BLINKSLOW | A_CELL_BLINKFAST);

    // move bold / blink to proper font if mode is on
    if (modeBoldFont && tbold)
        tfnt = 1;
    if (modeBlinkFont && tblinks)
        tfnt = 2;
    if (modeBoldFont && tbold && modeBlinkFont && tblinks)
        tfnt = 3;

    // convert to proper glyph # for font used by this char
    ch = String.fromCharCode(getUnicode(conFontCP[tfnt], ch.charCodeAt(0)));

    // reverse fg / bg for reverse on
    if (attr & A_CELL_REVERSE) {
        tmp = tfg;
        tfg = tbg;
        tbg = tmp;
    }

    // adjust fg color if in BBS ANSI and turn off bold.
    if (!cbm && !atari && !modeVTXANSI) {
        if (tbold && (tfg < 8)) {
            tfg += 8;
        }
        tbold = false;
    }

    // fix iCE colors
    if (modeBlinkBright
        && (tbg < 8)
        && tblinks) {
        // high intensity background / force blink off
        tbg += 8;
        attr &= ~(A_CELL_BLINKSLOW | A_CELL_BLINKFAST);
        tblinks = 0;
    }

    // force highlight (for mouse selections)
    if (hilight == 1) {
        tbg = (hotspotAttr >>  8) & 0xFF;
        tfg = (hotspotAttr      ) & 0xFF;
    } else if (hilight == 2) {
        // TODO : fix for sign.
        tbg = (hotspotAttr >>> 24) & 0xFF;
        tfg = (hotspotAttr >> 16) & 0xFF;
    }

    // fix transparents
    if (!cbm && !atari) {
        if (tfg == 0) 
            tfg = 16;
        if ((tbg == 0) && !modeVTXANSI) 
            tbg = 16;
    }

    // fix stupid ansi
    if (ch.charCodeAt(0) == 0x2588) {
        ch = ' ';
        tbg = tfg;
    }

    // set clipping region for this cell.
    ctx.save();
    ctx.beginPath();
    ctx.rect(x, 0, w, h);
    ctx.clip();

    // clear cell with background color
    if (cbm) {
        // PETSCII colors
        ctx.fillStyle = cbmColors[tbg]
        ctx.fillRect(x, 0, w, h);
    }  else if (atari) {
        ctx.fillStyle = atariColors[tbg]; // this should be 0 or 1
        ctx.fillRect(x, 0, w, h);
    } else {
        // ANSI colors
        if (tbg > 0) {
            ctx.fillStyle = ansiColors[tbg];
            ctx.fillRect(x, 0, w, h);
        } else
            ctx.clearRect(x, 0, w, h);
    }

    drawtxt = true;
   
    // don't draw if in blink state
    if (((attr & A_CELL_BLINKSLOW) && cellBlinkSlow)  
        || ((attr & A_CELL_BLINKFAST) && cellBlinkFast)) {
        if (!modeNoBlink)
            drawtxt = false;
    }

    // row display takes precidence over cell
    // adjust scaling for row display type (normal,conceal,top,bottom)
    i = conRowAttr[rownum] & A_ROW_DISPLAY_MASK;
    switch (i) {
        case A_ROW_DISPLAY_NORMAL:
            // adjust scaling for cell display type (normal,conceal,top,bottom)
            j = attr & A_CELL_DISPLAY_MASK;
            switch (j) {
                case A_CELL_DISPLAY_NORMAL:
                    yadj = 0;
                    yScale = 1.0;
                    break;
            
                case A_CELL_DISPLAY_CONCEAL:
                    // don't draw if row is concealed.
                    drawtxt = false;
                    break;
            
                case A_CELL_DISPLAY_TOP:
                    yadj = 0;
                    yScale = 2.0;
                    break;
            
                case A_CELL_DISPLAY_BOTTOM:
                    yadj = -h;
                    yScale = 2.0;
                    break;
            }
            break;
            
        case A_ROW_DISPLAY_CONCEAL:
            // don't draw if row is concealed.
            drawtxt = false;
            break;
            
        case A_ROW_DISPLAY_TOP:
            yadj = 0;
            yScale = 2.0;
            break;
            
        case A_ROW_DISPLAY_BOTTOM:
            yadj = -h;
            yScale = 2.0;
            break;
    }
    
    if (drawtxt) {
        // select text color
        if (attr & A_CELL_FAINT)
            // darken faint color
            ctx.fillStyle = brightenRGB(ansiColors[tfg], -0.33);
        else if (cbm)
            // PETSCII color
            ctx.fillStyle = cbmColors[tfg]
        else if (atari)
            // ATASCII monochrome color
            ctx.fillStyle = atariColors[tfg]
        else
            // ANSI color
            ctx.fillStyle = ansiColors[tfg];

        // swap for special fonts.
        teletext = -1;
        if ((tfnt == 10) || (tfnt == 11)) {
            // special teletext block font
            if (((ch >= ' ') && (ch <= '?')) || 
                ((ch >= '`') && (ch <= '\x7f'))) {
                teletext = tfnt - 10
            } else
                // use normal if not a block
                ctx.font = (tbold ? 'bold ' : '') + fontSize + 'px ' + conFont[0];
        } else {
            if (cbm)
                // PETSCII : render all text using conFontNum
                ctx.font = (tbold ? 'bold ' : '') + fontSize + 'px ' + conFont[conFontNum]
            else
                ctx.font = (tbold ? 'bold ' : '') + fontSize + 'px ' + conFont[tfnt];
        }
        
        ctx.textAlign = 'start';
        ctx.textBaseline = 'top';

        // set shadowing for shadow effect
        if (attr & A_CELL_SHADOW) {
            ctx.shadowColor = '#000000';
            ctx.shadowOffsetX = h / rowSize;
            ctx.shadowOffsetY = h / rowSize;
            ctx.shadowBlur = 0;
        } else {
            ctx.shadowBlur = 0;
        }

        // set skew for italics
        xskew = 0;
        xadj = 0;
        if (attr & A_CELL_ITALICS) {
            xskew = -0.125;
            xadj  = 1;
        }

        // if drawomg the bottoms of teletext double tall, adjust
        //if (attr & A_CELL_DOUBLE)
        if (bottom) {
            yadj = -h;
            yScale = 2.0;
        }

        // transmogrify
        ctx.setTransform(
            xScale * width,             // x scale
            0,                          // y skew
            xScale * xskew,             // x skew
            yScale * size,              // y scale
            x + (xScale * xadj),        // x adj
            yadj);                      // y adj
            
        if (attr & A_CELL_OUTLINE) {
            ctx.strokeStyle = ansiColors[tfg];
            ctx.lineWidth = 1;
            ctx.strokeText(ch, 0, 0);
        } else {
            if (teletext >= 0)
                drawMosaicBlock(ctx, ch.charCodeAt(0), w, h, teletext)
            else
                ctx.fillText(ch, 0, 0);
        }

        // draw underline / strikethough manually
        if (attr & A_CELL_UNDERLINE) {
            ctx.fillRect(0, h - stroke, w, stroke);
        }
        if (attr & A_CELL_STRIKETHROUGH) {
            ctx.fillRect(0, (h + stroke) / 2, w, stroke);
        }
    }
    ctx.restore();
}

// draw teletext style block graphic
// 0x20-0x3f/0x60-0x7f
function drawMosaicBlock(ctx, ch, w, h, separated) {
    var
        b = (ch<0x60)?(ch-0x20):(ch-0x40),
        x, y,
        bit = 0x01,
        bw = Math.floor(w / 2),
        bh = Math.floor(h / 3),
        bx, by = 0,
        sep = (separated ? -1 : 0),
        xadj = w - (bw * 2),
        yadj = h - (bh * 3);

    for (y = 0; y < 3; y++) {
        bx = 0;
        for (x = 0; x < 2; x++) {
            if (b & bit) {
                // draw this mini block
                ctx.fillRect(
                    bx, by,
                    bw + (x ? xadj : 0) + sep,
                    bh + ((y == 2)? yadj : 0) + sep);
            }
            bx += bw;
            bit <<= 1;
        }
        by += bh;
    }
}

// brighten / darken a color. color is a 24bit value (#RRGGBB)
function brightenRGB(colorstr, factor) {
    var
        r, g, b, rgb;

    // catch transparent
    if (colorstr == 'transparent')
        return colorstr;

    rgb = htoi(colorstr.substring(1));
    r = (rgb >> 16) & 0xFF;
    g = (rgb >>  8) & 0xFF;
    b =  rgb        & 0xFF;
    if (factor < 0) {
        factor += 1.0;
        r *= factor;
        g *= factor;
        b *= factor;
    } else {
        r = (255 - r) * factor + r;
        g = (255 - g) * factor + g;
        b = (255 - b) * factor + b;
    }
    r = Math.floor(r) & 0xFF;
    g = Math.floor(g) & 0xFF;
    b = Math.floor(b) & 0xFF;
    return  '#' + itoh((r << 16) + (g << 8) + b, 6);
}

// grow this row to col
function expandToCol(rownum, colnum) {
    while (conText[rownum].length < colnum)
        conText[rownum] += ' ';
    while (conCellAttr[rownum].length < colnum)
        conCellAttr[rownum][conCellAttr[rownum].length] = defCellAttr;
}

// grow total rows to rownum. needs to add html dom elements as well.
function expandToRow(rownum) {
    if (rownum >= conRowAttr.length) {
        while (rownum > getMaxRow()) {
            textDiv.appendChild(createNewRow());

            conRowAttr[conRowAttr.length] = 0x002C0000;
            conCellAttr[conCellAttr.length] = [];
            conText[conText.length] = '';
        }
        trimHistory();
    }
}

// adjust a <div><img src='...svg'></div> element so svg fills dimensions of div
// this is an onload event for svg images inside a div
function fitSVGToDiv(e) {
    var
        str,
        r1, w1, h1, w2, h2,
        p = this.parentNode;

    r1 = p.getBoundingClientRect();     // size of bounding div
    w1 = r1.width;
    h1 = r1.height;
    w2 = this.naturalWidth;             // ration of image
    h2 = this.naturalHeight;
    str = 'scale(' + (w1 / h2) + ',' + (h1 / w2) + ')';
    this.style['transform-origin'] = 'top left';
    this.style['transform'] = str;
    this.style['visibility'] = 'visible';
}

// set indicators
function setBulbs() {
    var 
        el;

    // set the online indicator button        
    el = document.getElementById('osbulb');
    if (termState == TS_OFFLINE) {
        el.src = 'os0.png';
        el.title = 'Connect';
    } else {
        el.src = 'os1.png';
        el.title = 'Disconnect';
    }

    // set the caps/num/scr lock indicators
    document.getElementById('clbulb').src = (capState ? 'cl1':'cl0') + '.png';
    document.getElementById('nlbulb').src = (numState ? 'nl1':'nl0') + '.png';
    document.getElementById('slbulb').src = (scrState ? 'sl1':'sl0') + '.png';

    // set the ul/dl buttons
    if (termState == TS_NORMAL) {
        el = document.getElementById('ulbtn');
        el.src = 'ul1.png';
        el.style['visibility'] = 'visible';
        el = document.getElementById('dlbtn');
        el.src = 'dl1.png';
        el.style['visibility'] = 'visible';
    } else if (termState == TS_OFFLINE) {
        el = document.getElementById('ulbtn');
        el.src = 'ul0.png';
        el.style['visibility'] = 'visible';
        el = document.getElementById('dlbtn');
        el.src = 'dl0.png';
        el.style['visibility'] = 'visible';
    } else {
        // buttons not visible in file transfer mode.
        document.getElementById('ulbtn').style['visibility'] = 'hidden';
        document.getElementById('dlbtn').style['visibility'] = 'hidden';
    }
}

// set event for page resize check and cursor blink
function setTimers(onoff) {
    if (onoff) {
        if (irqWriteBuffer == null)
            irqWriteBuffer = setInterval(doWriteBuffer, 33);
        if (irqCheckResize == null)
            irqCheckResize = setInterval(doCheckResize, 10);
        if (irqCursor == null)
            irqCursor = setInterval(doCursor, 533);
        if (irqBlink == null)
            irqBlink = setInterval(doBlink, 533);
    } else {
        if (irqWriteBuffer != null)
            clearInterval(irqWriteBuffer);
        if (irqCheckResize != null)
            clearInterval(irqCheckResize);
        if (irqCursor != null)
            clearInterval(irqCursor);
        if (irqBlink != null)
            clearInterval(irqBlink);
        irqWriteBuffer = null;
        irqCheckResize = null;
        irqCursor = null;
        irqBlink = null;
    }
}

// event functions for ctrl+v text to console
function beforePaste(e) {
    e.returnValue = false;
}

// event functions for ctrl+v text to console
function paste(e) {
    var
        clipboardData;

    e = e || window.event;

    // Stop data actually being pasted into div
    e.stopPropagation();
    e.preventDefault();

    clipboardData = e.clipboardData || window.clipboardData;
    conBufferOut(clipboardData.getData('Text'));
}

// update cursor from crsrAttr values
function newCrsr() {
    const
        sizes = [ '0%', '10%', '25%', '100%' ];
    var
        o, c, z, sz, ax1, ax2;

    if (crsr == null) {

        crsr = domElement(
            'div', {},
            {   position:   'absolute',
                display:    'block',
                zIndex:     '999' })

        o = domElement(
            'div',
            {   id:         'crsr' },
            {   position:   'absolute',
                display:    'block',
                bottom:     '0px',
                left:       '0px' });

        crsr.appendChild(o);
        textDiv.appendChild(crsr);
    } else
        o = crsr.firstChild;

    z = getCrsrAttrOrientation(crsrAttr);
    sz = getCrsrAttrSize(crsrAttr)
    switch (z) {
        case 0:
            o.style['width'] = '100%';
            o.style['height'] = sizes[sz];
            break;
        default:
            o.style['height'] = '100%';
            o.style['width'] = sizes[sz];
            break;
    }
    c = getCrsrAttrColor(crsrAttr);
    if (cbm)
        o.style['background-color'] = cbmColors[c & 0xF]
    else if (atari)
        o.style['background-color'] = atariColors[c & 0x1]
    else
        o.style['background-color'] = ansiColors[c];
}

// connect / disconnect. called from connect UI element.
function termConnect() {
    if (termState == TS_OFFLINE) {
        tnState = 0;
        ws = new WebSocket(wsConnect, ['telnet']);
        ws.binaryType = "arraybuffer";
        ws.onmessage = function(e) {
            // binary data in.
            var
                i, j, str, data;

            data = new Uint8Array(e.data);
            //dump(data,0,data.length);

            // do telnet handshaking negotiations - return new data w/o IAC...'s
            if (vtxdata.telnet == 1) 
                data = tnNegotiate(data);

            // convert data to string
            str = '';
            switch (codePage){
                case 'UTF8':
                    str = UFT8ArrayToStr(data);
                    break;

                case 'UTF16':
                    break;

                default:
                    // straight string
                    for (i = 0; i < data.length; i++)
                        str += String.fromCharCode(data[i]);
                    break;
            }

            switch (termState) {
                case TS_NORMAL:
                    // convert from codepage
                    conBufferOut(str);
                    break;

                case TS_YMR_START:
                case TS_YMR_GETPACKET:
                    data = ymRStateMachine(data);
                    if (data.length > 0) {
                        // transfer ended midway. output the rest.
                        conBufferOut(str);
                    }
                    break;

                case TS_YMS_START:
                case TS_YMS_PUTPACKET:
                case TS_YMS_PUTWAIT:
                    data = ymSStateMachine(data);
                    if (data.length > 0) {
                        // transfer ended midway. output the rest.
                        conBufferOut(str);
                    }
                    break;
            }
        }
        ws.onopen = function() {
            if (vtxdata.telnet)
                tnInit();
            termState = TS_NORMAL;
            setBulbs();
        }
        ws.onclose = function() {
            conBaud = 0;
            conBufferOut(
                cbm
                ?'\r\rDISCONNECTED.\r'
                :'\r\n\r\n\x1b[#9\x1b[0;91mDisconnected.\r\n');
            document.body.style['cursor'] = 'default';
            if (audio) {
                audio.pause();
                pageDiv.removeChild(audio);
            }
            termState = TS_OFFLINE;
            setBulbs();
        }
        ws.onerror = function(error) {
            conBufferOut(
                cbm
                ?'\r\rERROR : ' + error.reason.toUpper() + '\r'
                :'\r\n\r\n\x1b[#9\x1b[0;91mError : ' + error.reason + '\r\n');
            setBulbs();
        }
    } else {
        ws.close();
    }
}
function encodeAscii(str) {
    var i,l,strout = '';
    l=str.length;
    for (i=0;i<l;i++)
        strout+=';'+str.charCodeAt(i).toString();
    return strout.substring(1);
}

// setup the crt and cursor
function initDisplay() {
    var
        i, o, p, pos,
        cssel,
        fsize,
        style,
        css,
        head,
        defattrs;

//testArrayStuff();

    // find the page / text div
    pageDiv = document.getElementById('vtxpage');
    if (!pageDiv)
        return; // couldn't find it.
    textDiv = document.getElementById('vtxtext');
    if (!textDiv)
        return; // couldn't find it.

    // determine standard sized font width in pixels
    getDefaultFontSize(); // get fontName, colSize, rowSize
    crtWidth = colSize * crtCols;

    // codepage from AKAs - abort if invalid
    if (!codePageData[codePage]) {
        if (codePageAKAs[codePage]) 
            codePage = codePageAKAs[codePage];
    }

    // set fonts.
    if (cbm) {
        conFontNum = 0;
        switch (vtxdata.codePage) {
            case 'VIC20':
                conFont[0] = 'VIC200';
                conFont[1] = 'VIC201';
                cbmColors = vic20Colors;
                break;

            case 'C64':
                conFont[0] = 'C640';
                conFont[1] = 'C641';
                cbmColors = c64Colors;
                break;

            case 'C128':
                conFont[0] = 'C1280';
                conFont[1] = 'C1281';
                cbmColors = c128Colors;
                break;
        }
        conFontCP[0] = 'RAW';
        conFontCP[1] = 'RAW';
    } else if (atari) {
        conFontNum = 0;
        conFont[0] = 'ATARI';
        conFontCP[0] = 'RAW';
    } else {
        conFontNum = 0;                 // current font being used.
        for (i = 0; i < 16; i++) {      // set default font selects.
            conFont[i] = fontName;
            conFontCP[i] = codePage;
        }
    }

    pageDiv.style['width'] = (crtWidth*xScale) + 'px';
    pagePos = getElementPosition(pageDiv);
    pageLeft = pagePos.left;
    pageTop = pagePos.top;

    // build marquee CSS
    style = document.createElement('style');
    style.type = 'text/css';
    css = '.marquee { animation: marquee 12s linear infinite; } '
        + '@keyframes marquee { '
        + '0% { transform: translate( ' + (xScale * crtCols * colSize) + 'px, 0); } '
        + '100% { transform: translate(-100%, 0); }}';
    if (style.styleSheet)
        style.styleSheet.cssText = css
    else
        style.appendChild(document.createTextNode(css));
    head = document.head || document.getElementsByTagName('head')[0];
    head.appendChild(style);

    defCellAttr = vtxdata.defCellAttr;
    defCrsrAttr = vtxdata.defCrsrAttr;
    defPageAttr = vtxdata.defPageAttr;
    cellAttr =  defCellAttr;
    crsrAttr = defCrsrAttr;
    pageAttr = defPageAttr;

    // create cursor
    newCrsr();
    crsrSkipTime = 0;

    // load bell sound
    soundBell = new Audio();
    soundBell.src = 'bell.mp3';
    soundBell.type = 'audio/mp3';
    soundBell.volume = 1;
    soundBell.preload = 'auto';
    soundBell.load();

    // set page attributes
    p = pageDiv.parentNode;
    if (cbm) {
        p.style['background-color'] = cbmColors[(pageAttr >> 8) & 0xF];
        pageDiv.style['background-color'] = cbmColors[pageAttr & 0xF];
    } else if (atari) {
        p.style['background-color'] = atariColors[(pageAttr >> 8) & 0x1];
        pageDiv.style['background-color'] = atariColors[pageAttr & 0x1];
    } else {
        p.style['background-color'] = ansiColors[(pageAttr >> 8) & 0xFF];
        pageDiv.style['background-color'] = ansiColors[pageAttr & 0xFF];
    }
    pageWidth = elPage.clientWidth;

    // set initial states.
    shiftState = ctrlState = altState
        = capState = numState = scrState = false;
    crsrBlink = false;  // cursor blink state for intervaltimer
    cellBlinkSlow = true;
    cellBlinkFast = true;
    crsrRow = crsrCol = 0;
    crsrDraw();

    // set event for page resize check and cursor blink
    elPage = document.getElementsByTagName('html')[0];
    setTimers(true);

    crsrHome();
    textPos = textDiv.getBoundingClientRect();
    termState = TS_OFFLINE; // set for standard terminal mode, not in file xfer mode

    // indicators and controls
    ctrlDiv = domElement(
        'div',
        {   id:             'ctrls' },
        {   width:          '24px',
            height:         '164px',
            position:       'fixed',
            top:            textPos.top + 'px',
            left:           (6 + textPos.left + (crtWidth * xScale)) + 'px'});

    pos = 0;
    ctrlDiv.appendChild(domElement(
        'img',
        {   src:        'os0.png',
            id:         'osbulb',
            onclick:    termConnect,
            width:      24,
            height:     24,
            title:      'Connect/Disconnect' },
        {   cursor:     'pointer'}));

    ctrlDiv.appendChild(domElement(
        'img',
        {   src:        'cl0.png',
            id:         'clbulb',
            width:      24,
            height:     24,
            title:      'CapsLk' }));

    ctrlDiv.appendChild(domElement(
        'img',
        {   src:        'nl0.png',
            id:         'nlbulb',
            width:      24,
            height:     24,
            title:      'NumLk' }));

    ctrlDiv.appendChild(domElement(
        'img',
        {   src:        'sl0.png',
            id:         'slbulb',
            width:      24,
            height:     24,
            title:      'ScrLk' }));

    ctrlDiv.appendChild(domElement(
        'img',
        {   src:        'ul0.png',
            id:         'ulbtn',
            onclick:    ymSendStart,
            width:      24,
            height:     24,
            title:      'YModem Upload' },
        {   cursor:     'pointer'}));

    ctrlDiv.appendChild(domElement(
        'img',
        {   src:        'dl0.png',
            id:         'dlbtn',
            onclick:    ymRecvStart,
            width:      24,
            height:     24,
            title:      'YModem Download' },
        {   cursor:     'pointer'}));

    pageDiv.appendChild(ctrlDiv);

    // add audio element for sound.
    audio = domElement(
        'audio',
        {   id:         'vtxaudio',
            preload:    'none',
            volume:     '0.25',
            src:        '/;' },
        {   width:      '0px',
            height:     '0px' });
    pageDiv.appendChild(audio);

    conBufferOut(initStr);

    if (vtxdata.autoConnect && (vtxdata.autoConnect > 0))
        // websocket connect
        termConnect()
    else {
        // let user know how to connect. heh-
        switch (vtxdata.term) {
            case 'PETSCII':
                conStrOut('\x0Evtx tERMINAL cLIENT\r');
                conStrOut('vERSION 0.9\r');
                conStrOut('2017 dAN mECKLENBURG jR.\r');
                conStrOut('GITHUB.COM/CODEWAR65/vtx\xA4cLIENTsERVER\r');
                conStrOut('\rcLICK cONNECT...\r');
                break;
                
            case 'ATASCII':
                conStrOut('VTX Terminal Client\x9B');
                conStrOut('Version 0.9\x9B');
                conStrOut('2017 Dan Mecklenburg Jr.\x9B');
                conStrOut('github.com/codewar65/VTX_ClientServer\x9B');
                conStrOut('\x9BClick Connect...\x9B');
                break;
                
            default:
                conStrOut('\n\r\x1b#6\x1b[38;5;46;58mVTX\x1b[32;78m Terminal Client\n\r');
                conStrOut('\x1b#6\x1b[38;5;46;59mVTX\x1b[32;79m Version 0.9 Beta\n\r');
                conStrOut('\n\r\x1b[m2017 Dan Mecklenburg Jr.\n\r');
                conStrOut('Visit \x1b[1;45;1;1;' 
                    + encodeAscii('https://github.com/codewar65/VTX_ClientServer') 
                    + '\\https://github.com/codewar65/VTX_ClientServer for more info.\n\r');
                conStrOut('\n\r\x1b[38;5;46mCLICK CONNECT...\x1b[m\n\r');
                break;
        }
    }

    setBulbs();

    // key events
    addListener(document, 'keydown', keyDown);
    addListener(document, 'keyup', keyUp);
    addListener(document, 'keypress', keyPress);
    // mouse events
    addListener(document, 'mouseup', mouseUp);
    addListener(document, 'mousedown', mouseDown);
    addListener(document, 'click', click);
    addListener(document, 'mousemove', mouseMove);
    // copy paste events
    addListener(document, 'beforepaste', beforePaste);
    addListener(document, 'paste', paste);
}

function UFT8ArrayToStr(array) {
    var
        out, i, len, c,
        char2, char3;

    out = '';
    len = array.length;
    i = 0;
    while(i < len) {
        c = array[i++];
        switch(c >> 4) {
            case 0: case 1: case 2:
            case 3: case 4: case 5: case 6: case 7:
                // 0xxxxxxx
                out += String.fromCharCode(c);
                break;

            case 12: case 13:
                // 110x xxxx   10xx xxxx
                char2 = array[i++];
                out += String.fromCharCode(((c & 0x1F) << 6) | (char2 & 0x3F));
                break;

            case 14:
                // 1110 xxxx  10xx xxxx  10xx xxxx
                char2 = array[i++];
                char3 = array[i++];
                out += String.fromCharCode(((c & 0x0F) << 12) |
                       ((char2 & 0x3F) << 6) |
                       ((char3 & 0x3F) << 0));
                break;
        }
    }
    return out;
}

function fadeScreen(fade) {
    var
        el, fntfm, cs;

    if (!fade) {
        pageDiv.classList.remove('fade');
        if (ovl['dialog'])
            document.body.removeChild(ovl['dialog']);
        ovl = {};

        // enable keys / cursor
        document.getElementById('ulbtn').style['visibility'] = 'visible';
        document.getElementById('dlbtn').style['visibility'] = 'visible';
        setTimers(true);
        document.body.style['cursor'] = 'default';
    } else {
        pageDiv.classList.add('fade')


        //cs = window.getComputedStyle(el) || el.currentStyle;
        fntfm = 'san-serif'; //cs['font-family'];

        // add control overlay
        ovl['dialog'] = domElement(
            'div',
            {   className:  'noselect' },
            {   width:      '256px',
                height:     '128px',
                display:    'block',
                background: '#CCC',
                color:      '#000',
                border:     '2px outset #888',
                position:   'fixed',
                top:        ((window.innerHeight - 128) / 2) + 'px',
                left:       ((pageWidth - 256) / 2) + 'px',
                cursor:     'default',
                fontFamily: fntfm,
                fontSize:   '12px' });
        document.body.appendChild(ovl['dialog']);

        // add cancel button
        ovl['dialog'].appendChild(domElement(
            'input',
            {   type:       'button',
                onclick:    ymCancel,
                value:      'Cancel' },
            {   width:      '96px',
                height:     '24px',
                position:   'absolute',
                bottom:     '8px',
                right:      '8px' }));

        // dialog label
        ovl['title'] = domElement(
            'span',
            {   className:  'noselect' },
            {   width:      '228px',
                height:     '20px',
                background: '#039',
                border:     '1px inset #888',
                position:   'absolute',
                top:        '4px',
                left:       '4px',
                padding:    '0px 8px',
                fontSize:   '14px',
                fontWieght: 'bold',
                color:      'white' },
            'YModem Download' );
        ovl['dialog'].appendChild(ovl['title']);

        // filename
        ovl['dialog'].appendChild(domElement(
            'span',
            {   className:  'noselect' },
            {   position:   'absolute',
                top:        '32px',
                left:       '12px' },
            "File:" ));
        ovl['filename'] = domElement(
            'span', {},
            {   position:       'absolute',
                top:            '32px',
                left:           '80px',
                width:          '172px',
                overflow:       'hidden',
                textOverflow:   'ellipsis'
            },
            '{filename}' );
        ovl['dialog'].appendChild(ovl['filename']);

        // filesize
        ovl['dialog'].appendChild(domElement(
            'span',
            {   className:  'noselect' },
            {   position:   'absolute',
                top:        '52px',
                left:       '12px' },
            "Size:" ));
        ovl['filesize'] = domElement(
            'span', {},
            {   position:       'absolute',
                top:            '52px',
                left:           '80px',
                width:          '172px',
                overflow:       'hidden',
                textOverflow:   'ellipsis'
            },
            '{filesize}' );
        ovl['dialog'].appendChild(ovl['filesize']);

        // filesize
        ovl['dialog'].appendChild(domElement(
            'span',
            {   className:  'noselect' },
            {   position:   'absolute',
                top:        '72px',
                left:       '12px' },
            "Transferred:" ));
        ovl['transferred'] = domElement(
            'span', {},
            {   position:       'absolute',
                top:            '72px',
                left:           '80px',
                width:          '172px',
                overflow:       'hidden',
                textOverflow:   'ellipsis'
            },
            '{transferred}' );
        ovl['dialog'].appendChild(ovl['transferred']);

        // disable keys / cursor
        setBulbs();
        document.getElementById('ulbtn').style['visibility'] = 'hidden';
        document.getElementById('dlbtn').style['visibility'] = 'hidden';
        setTimers(false);
        document.body.style['cursor'] = 'wait';
    }
}

function updateCRC16(crcIn, byteIn) {
    var
        crc = crcIn,
        b = byteIn | 0x100;

    do {
        crc <<= 1;
        b <<= 1;
        if (b & 0x100)
            crc++;
        if (crc & 0x10000)
            crc ^= CRC_POLY;
    } while (!(b & 0x10000));
    return crc & 0xFFFF;
}

function calcCRC16(data, start, size) {
    var
        pos, end,
        crc = 0;

    pos = start;
    end = start + size;
    for (; pos < end; pos++)
        crc = updateCRC16(crc, data[pos]);
    crc = updateCRC16(crc, 0);
    crc = updateCRC16(crc, 0);
    return crc & 0xFFFF;
}

// concatinate two bytearrays
function combineArrays(first, second) {
    var
        firstLength = first.length,
        result = new Uint8Array(firstLength + second.length);

    result.set(first);
    result.set(second, firstLength);

    return result;
}

// https://stackoverflow.com/questions/23451726/saving-binary-data-as-file-using-javascript-from-a-browser
function saveAs(blob, fileName) {
    var
        el,
        url = window.URL.createObjectURL(blob);

    el = domElement(
        'a',
        {   href:       url,
            download:   fileName },
        {   display:    'none' });

    document.body.appendChild(el);
    el.click();
    document.body.removeChild(el);

    // On Edge, revokeObjectURL should be called only after
    // a.click() has completed, atleast on EdgeHTML 15.15048
    setTimeout( function() {
        window.URL.revokeObjectURL(url);
    }, 1000);
}

// data in / out is Uint8Array
// the idea is to process one byte at a time. pass in chunks of string data
// and process all of it until done. return any remaining string data if
// terminates before end of string.
function ymRStateMachine(data){
    var
        i, j, result = [],
        block, block2,
        crc16A, crc16B,
        tmpblob,
        str, b, tmp, bytes = [];

    if (!data) return result;

    for (i = 0; i < data.length; i++) {
        b = data[i];
        switch (termState) {
            case TS_YMR_START:
                // start recieving packets. if SOH, get 128. if STX then 1024
                // need SOH 00 FF foo.c NUL[123] CRC CRC
                ymPacketPos = 0;
                switch (b) {
                    case _SOH:
                        clearInterval(ymTimer);
                        ymEOTCount = 0;
                        termState = TS_YMR_GETPACKET;   // advance to get line num
                        ymPacketSize = 128 + 4;         // 128 bytes + line, line inv + crc16
                        break;

                    case _STX:
                        clearInterval(ymTimer);
                        ymEOTCount = 0;
                        termState = TS_YMR_GETPACKET;   // advance to get line num
                        ymPacketSize = 1024 + 4;        // 128 bytes + line, line inv + crc16
                        break;

                    case _EOT:
                        // end of transmittion. save file.
                        clearInterval(ymTimer);
                        ymEOTCount++;
                        if (ymEOTCount == 1) {
                            sendData(_ACK);

                            termState = TS_NORMAL;
                            fadeScreen(false);

                            if (ymFileSize > -1) {
                                ymFileData = ymFileData.slice(0, ymFileSize);
                            } else {
                                // TODO : count CPMEOFs on end (work on this)
                                throw 'EOT look for CMPEOF';
//                              j = ymFileData.size - 1;
//                              while ((ymFileData[j] == _CPMEOF) && (j > 0))
//                                 j--;
//                              ymFileData = ymFileData.slice(0, j + 1);
                            }
                            saveAs(ymFileData, ymFileName);
                            result = data.slice(i + 1);
                            ymCancel();
                            break;
                        }
                        sendData(_ACK);
                        break;

                    case _CAN:
                        // cancel from remote.
                        clearInterval(ymTimer);
                        ymEOTCount = 0;
                        termState = TS_NORMAL;
                        fadeScreen(false);
                        ymFileData = new Blob([], {type: 'application/octet-stream'});
                        result = data.slice(i+1);
                        break;
                }
                break;

            case TS_YMR_GETPACKET:
                // read ymPacketSize bytes for packet
                ymPacketBuff[ymPacketPos++] = b;
                if (ymPacketPos == ymPacketSize) {
                    // entire packet recieved. test lines and crc16
                    block =     ymPacketBuff[0];
                    block2 =    ymPacketBuff[1];

                    // out of sequence
                    if (block != ymNextBlock) {
                        if (block == (ymNextBlock - 1))
                            // resending last block.
                            sendData(_ACK)
                        else
                            sendData(_NAK);
                        termState = TS_YMR_START;
                        break;
                    }

                    // check block number
                    if (block != (~block2 & 0xFF)) {
                        sendData(_NAK);
                        termState = TS_YMR_START;
                        break;
                    }
                    // check crc16
                    // uncertain the order for crc16 - swap if needed.
                    crc16A = (ymPacketBuff[ymPacketPos - 2] << 8) |
                              ymPacketBuff[ymPacketPos - 1];
                    crc16B = calcCRC16(ymPacketBuff, 2, ymPacketSize - 4);

                    if (crc16A != crc16B) {
                        //dump(ymPacketBuff, 2, ymPacketSize - 4);
                        sendData(_NAK);
                        termState = TS_YMR_START;
                        break;
                    }
                    if ((block == 0) && (ymFileData.size == 0)) {
                        // first packet. get filename and optional filesize
                        //dump(ymPacketBuff, 0, ymPacketBuff.length);
                        ymFileName = '';
                        j = 2;
                        while (ymPacketBuff[j])
                            ymFileName += String.fromCharCode(ymPacketBuff[j++]);
                        ovl['filename'].innerHTML = ymFileName;

                        j++; // skip over null.

                        // check for filesize
                        if (ymPacketBuff[j]) {
                            str = '';
                            while ((ymPacketBuff[j] != _SPACE) && (ymPacketBuff[j] != _NUL))
                                str += String.fromCharCode(ymPacketBuff[j++]);
                            ymFileSize = parseInt(str); // known size
                            ovl['filesize'].innerHTML = formatSize(ymFileSize);
                        } else {
                            ymFileSize = -1;            // unknown size
                            ovl['filesize'].innerHTML = 'Unknown';
                        }

                        // initialize blob to empty
                        ymFileData =  new Blob([], {type: 'application/octet-stream'});
                        ovl['transferred'].innerHTML = '0 bytes';

                        // send ACK + C
                        sendData(_ACK);
                        sendData(_C);
                        termState = TS_YMR_START;

                    } else {
                        // append data to blob
                        tmp = new Uint8Array(ymPacketSize - 4);
                        for (j = 0; j < ymPacketSize - 4; j++)
                            tmp[j] = ymPacketBuff[2 + j];
                        ymPacketBuff = [];
                        tmpblob = new Blob([ymFileData, tmp]);
                        ymFileData = tmpblob;
                        ovl['transferred'].innerHTML = formatSize(ymFileData.size);

                        // send ACK
                        sendData(_ACK);
                        termState = TS_YMR_START;
                    }
                    ymNextBlock = (block + 1) & 0xFF;
                }
                break;
        }
        if (termState == TS_NORMAL)
            break;
    }
    return result;
}

// return human readable text for filesize
function formatSize(size) {
    var
        mag,
        mags = [ 'B','KB','MB','GB' ];

    for (mag = 0; size >= 1000; size /= 1000, mag++){};
    return size.toFixed(2) + ' ' + mags[mag];
}

// cancel ymodem transfer
function ymCancel() {
    clearInterval(ymTimer);
    sendData(_CAN);
    sendData(_CAN);
    sendData(_CAN);
    sendData(_CAN);
    sendData(_CAN);
    sendData(_CAN);
    sendData(_CAN);
    sendData(_CAN);
    termState = TS_NORMAL;
    fadeScreen(false);
}

// send C's to remote to start download.
function ymSendC() {
    sendData(_C);
    ymCCount++;
    if (ymCCount > 8) {
        // abort after 8 tries.
        termState = TS_NORMAL;
        clearInterval(ymTimer);
        fadeScreen(false);
    }
}

// receive file from remote.
function ymRecvStart() {

    // already have session started.
    if (ovl.length) return;

    // send starting C's
    termState = TS_YMR_START;

    // fade terminal - build file xfer ui.
    fadeScreen(true);
    ovl['title'].innerHTML = 'YModem Download';
    ovl['filename'].innerHTML = 'Waiting for Remote...';
    ovl['filesize'].innerHTML = '';
    ovl['transferred'].innerHTML = '';

    ymNextBlock = 0;
    ymCCount = 0;
    ymSendC();
    if (termState != TS_NORMAL)
        ymTimer = setInterval(ymSendC, 3000);
}

// wait for ack / nak. if no response, resend last packet.
function ymTimeOut() {
    sendData(ymPacketBuff);
    ymNakCount++;
}

function twosCompliment(v) {
    return (255 - v) & 0xff;
}

function highByte(v) {
    return (v >> 8) & 0xff;
}

function lowByte(v) {
    return v & 0xff;
}

// state machine for sending.
// entire file in ymFileData blob
// current
function ymSStateMachine(data) {
    var
        tmp = [],
        str,
        crc16,
        i, j,
        pos,
        packetSize,
        b;

    for (i = 0; i < data.length; i++) {
        clearInterval(ymTimer);
        b = data[i];
        switch (termState) {
            case TS_YMS_START:
                // wait for 'C'
                if (b == _C) {
                    // build header packet

                    // 128 byte header
                    packetSize = 128;

                    ymPacketBuff = new Uint8Array(5 + packetSize);
                    ymPacketBuff.fill(_NUL);

                    // block packet prefix - size / block num
                    ymPacketBuff[0] = ((packetSize == 128) ? _SOH : _STX);
                    ymPacketBuff[1] = ymNextBlock;        // block number 0
                    ymPacketBuff[2] = 255 - ymNextBlock;  // inverse block number

                    // filename charactes are ascii 33-126 via cleanFileName()
                    str =   ymFileName + NUL +
                            ymFileSize.toString() + ' ' +
                            ymModifiedDate + ' 0 0';     // mode + serial #
                    for (j = 0; j < str.length; j++)
                        ymPacketBuff[3 + j] = str.charCodeAt(j);

                    // block packet suffix - crc16
                    crc16 = calcCRC16(ymPacketBuff, 3, packetSize);
                    ymPacketBuff[3 + packetSize] = highByte(crc16);
                    ymPacketBuff[4 + packetSize] = lowByte(crc16);

                    console.log('Header Packet');
                    dump(ymPacketBuff, 0, ymPacketBuff.length);
                    sendData(ymPacketBuff);

                    termState = TS_YMS_PUTPACKET;
                    ymFilePos = 0;
                    ymNakCount = 0;
                    ymEOTCount = 0;
                    ymNextBlock = (ymNextBlock + 1) & 0xFF;
                    // now need ACK + C

                } else {
                    // wait for 10 seconds before abort
                    if (new Date().getTime() > ymSendStartTime + 10000)
                        ymCancel();
                }
                break;

            case TS_YMS_PUTWAIT:
                // need a C to get past header ACK
                if (b == _C) {
                    ymNakCount = 0;

                    // send first block
                    packetSize = 1024;

                    ymPacketBuff = new Uint8Array(5 + packetSize);
                    ymPacketBuff.fill(_CPMEOF);

                    // block packet prefix - size / block num
                    ymPacketBuff[0] = ((packetSize == 128) ? _SOH : _STX);
                    ymPacketBuff[1] = ymNextBlock;        // block number 0
                    ymPacketBuff[2] = 255 - ymNextBlock;  // inverse block number

                    // copy chunk of file data
                    pos = ymFilePos;
                    for (j = 0; j < packetSize; j++, pos++) {
                        if (pos < ymFileSize)
                            ymPacketBuff[3 + j] = ymFileData[pos]
                        else
                            break;
                    }

                    // block packet suffix - crc16
                    crc16 = calcCRC16(ymPacketBuff, 3, packetSize);
                    ymPacketBuff[3 + packetSize] = highByte(crc16);
                    ymPacketBuff[4 + packetSize] = lowByte(crc16);

                    // send data
                    sendData(ymPacketBuff);
                    ymFilePos += packetSize;
                    if (ymFilePos > ymFileSize) ymFilePos = ymFileSize;

                    termState = TS_YMS_PUTPACKET;
                    ymNextBlock = (ymNextBlock + 1) & 0xFF;
                    // back to normal data-> + ACK/NAK<-

                } else
                    // ?!
                break;

            case TS_YMS_PUTPACKET:
                if (b == _NAK) {
                    // resend last packet.
                    sendData(ymPacketBuff);
                    ymNakCount++;
                    // abort after 10 NAKs
                    if (ymNakCount > 10)
                        ymCancel();
                } else if (b == _ACK) {
                    // last packet good.
                    ymNakCount = 0;
                    if ((ymFilePos == 0) && (ymNextBlock == 1)) {
                        // wait for C.
                        ovl['transferred'].innerHTML = 'Sent header.';
                        termState = TS_YMS_PUTWAIT
                    } else if (ymFilePos == ymFileSize) {
                        // send EOT
                        ovl['transferred'].innerHTML = formatSize(ymFileSize);
                        if (ymEOTCount) {
                            // done. transferred.
                            termState = TS_NORMAL;
                            ymCancel();
                        }
                        ymEOTCount++;
                        ymPacketBuff = new Uint8Array(1);
                        ymPacketBuff[0] = _EOT;
                        sendData(ymPacketBuff);

                    } else {
                        // send data packet
                        ovl['transferred'].innerHTML = formatSize(ymFilePos);

                        // send data packet 2+
                        packetSize = 1024;

                        ymPacketBuff = new Uint8Array(5 + packetSize);
                        ymPacketBuff.fill(_CPMEOF);

                        // block packet prefix - size / block num
                        ymPacketBuff[0] = ((packetSize == 128) ? _SOH : _STX);
                        ymPacketBuff[1] = ymNextBlock;        // block number 0
                        ymPacketBuff[2] = 255 - ymNextBlock;  // inverse block number

                        // copy chunk of file data
                        pos = ymFilePos;
                        for (j = 0; j < packetSize; j++, pos++) {
                            if (pos < ymFileSize)
                                ymPacketBuff[3 + j] = ymFileData[pos]
                            else
                                break;
                        }

                        // block packet suffix - crc16
                        crc16 = calcCRC16(ymPacketBuff, 3, packetSize);
                        ymPacketBuff[3 + packetSize] = highByte(crc16);
                        ymPacketBuff[4 + packetSize] = lowByte(crc16);

                        sendData(ymPacketBuff);
                        ymFilePos += packetSize;
                        if (ymFilePos > ymFileSize) ymFilePos = ymFileSize;

                        ymNextBlock = (ymNextBlock + 1) & 0xFF;
                    }
                } else if (b == _CAN) {
                    ymCancel();
                } else {
                    // ?!
                }
                break;
        }
        if (termState == TS_NORMAL)
            break;
    }
    return [];
}

// file loaded. commence upload.
function ymFileLoaded(e) {
    var
        str,
        i, pos;

    ymFileData = new Uint8Array(e.target.result);
    ymFileSize = ymFileData.byteLength;
    ovl['filesize'].innerHTML = formatSize(ymFileSize);
    ovl['transferred'].innerHTML = formatSize(0);
    termState = TS_YMS_START;       // wait for 'G'.
    ymNextBlock = 0;
    ymFilePos = 0;
    ymSendStartTime = new Date().getTime();
}

// file selected. load it up.
function ymFileLoad(e) {
    var
        i,
        file,
        reader = new FileReader();

    file = e.target.files[0],
    name = file.name;
    i = name.lastIndexOf('/');
    if (i == -1) i = name.lastIndexOf('\\');
    ymFileName = cleanFileName(name.substring(i + 1));
    ymModifiedDate = Math.floor(file.lastModified / 1000).toString(8);
    ovl['filename'].innerHTML = ymFileName;
    reader.onload = ymFileLoaded;
    reader.readAsArrayBuffer(file);
}

// remove spaces and stuff.
function cleanFileName(str) {
    var
        i, l, c, ch,
        strout = '',
        badchrs = '/\\?%*:|"<> ';

    for (i = 0, l = str.length; i < l; i++){
        c = str.charAt(i);
        ch = str.charCodeAt(i);
        if ((ch > 32) && (ch < 127) &&
            (badchrs.indexOf(c) == -1))
            strout += c;
    }
    if (!strout.length)
        strout = 'untitled';
    return strout;
}

// send file to remove
function ymSendStart() {
    var
        el;

    // already have session started.
    if (ovl.length) return;

    // fade terminal - build file xfer ui.
    fadeScreen(true);
    ovl['title'].innerHTML = 'YModem Upload';
    ovl['filename'].innerHTML = '';
    ovl['filesize'].innerHTML = '';
    ovl['transferred'].innerHTML = '';

    // fetch file.
    el = domElement(
        'input',
        {   type:       'file',
            onchange:   ymFileLoad },
        {   position:   'relative',
            left:       '1px',
            top:        '1px',
            width:      '1px',
            height:     '1px'
        });

    ovl['dialog'].appendChild(el);
    el.click();
    ovl['dialog'].removeChild(el);
}

function dump(buff, start, len){
    var
        i,
        str,
        alpha,
        count = 0;

    str = itoh(start, 4) + ':';
    alpha = '';
    for (i = start; i < start + len; i++) {

        str += ' ' + itoh(buff[i], 2);
        if ((buff[i] >= 32) && (buff[i] < 127))
            alpha += String.fromCharCode(buff[i])
        else
            alpha += '.';

        count = (count + 1) & 0xf;
        if (!count) {
            str += ': ' + alpha + '\r\n' + itoh(i + 1, 4) + ':';
            alpha = '';
        }
    }
    for (i = count; i < 16; i++) str += '   ';
    str += ': ' + alpha;
    console.log(str);
}
function nop(){};

// convert character ch (0-255) to unicode
function getUnicode(cp, ch) {

    if (cp == 'UTF8') return ch;

    var
        d = 0,
        cplut = codePageData[cp] || codePageData[codePageAKAs[cp]];

    if (cplut) {
        switch (cplut.length) {
            case 256:
                d = cplut[ch];
                break;

            case 128:
                d = (ch < 128) ? codePageData['ASCII'][ch] : cplut[ch - 128];
                break;

            case 96:
                d = (ch < 160) ? codePageData['ASCII'][ch] : cplut[ch - 160];
                break;

            default:
                // default to 437 if not found.
                d = codePageData['CP437'][ch];
                break;
        }
    }
    return d;
}

// send data to remote (or echo local if not connected)
// only send arraybuffer data
// add convert from native to node's codepage.
// need to escape 0xFF if in telnet mode!
function sendData(data) {
    var
        i, l, 
        str,
        pos,
        len,
        tmp;

    if (typeof data === 'string') {
        // string to aray buffer
        str = data;
        l = str.length;
        data = new Uint8Array(l);
        for (i = 0; i < l; i++)
            data[i] = str.charCodeAt(i);
    } else if (typeof data === 'number') {
        // byte/character to aray buffer
        str = String.fromCharCode(data);
        data = new Uint8Array([data]);
    }
    
    if (!(data instanceof Uint8Array))
        throw 'Invalid data in sendData().';

    if (vtxdata.telnet) {
        // escape 0xFF's
        if (data.indexOf(0xFF) >= 0) {
            // spilt up / recombine
            tmp = [];
            len = 0;
            while ((pos = data.indexOf(0xFF)) >= 0) {
                tmp.push(data.slice(0, pos));
                len += pos;
                tmp.push(new Uint8Array([0xFF,0xFF]));
                len += 2;
                data = data.slice(pos + 1);
            }
            len += data.length;
            tmp.push(data);
        
            // join tmp into new data
            data = new Uint8Array(len);
            for (pos = 0, i = 0; i < tmp.length; i++){
                data.set(tmp[i], pos);
                pos += tmp[i].length;
            }
        }
    }
    
    // check code page conversions.
    if (ws && (ws.readyState == 1) && (termState != TS_OFFLINE)) {
        ws.send(data.buffer);
    } else {
        conBufferOut(str);
        crsrDraw();
    }
}

// poke a character
function conPutChar(rownum, colnum, chr, attr) {
    // expand if needed
    expandToRow(rownum);
    expandToCol(rownum, colnum);
    conText[rownum] = conText[rownum].splice(colnum, 1, String.fromCharCode(chr));
    conCellAttr[rownum][colnum] = attr;
    renderCell(rownum, colnum);
}

// erase hotspots on row from col1 to col2
function clearHotSpotsRow(row, col1, col2) {
    var
        hs, i;
    for (i = conHotSpots.length - 1; i >= 0; i--) {
        hs = conHotSpots[i];
        if ((hs.row == row) && (hs.col >= col1) && (hs.col <= col2))
            conHotSpots.splice(i,1);
    }
}

// erase hotspots from row1 to row2
function clearHotSpotsRows(row1, row2) {
    var
        hs, i;
    for (i = conHotSpots.length - 1; i >= 0; i--) {
        hs = conHotSpots[i];
        if ((hs.row >= row1) && (hs.row <= row2))
            conHotSpots.splice(i,1);
    }
}

// move hotspots on row from col1 to col2 by coladj cols
function moveHotSpotsRow(row, col1, col2, coladj) {
    var
        hs, i;
    for (i = conHotSpots.length - 1; i >= 0; i--) {
        hs = conHotSpots[i];
        if ((hs.row == row) && (hs.col >= col1) && (hs.col <= col2))
            hs.col += coladj;
    }
}

// move hotspots from row1 to row2 by rowadj rows
function moveHotSpotsRows(row1, row2, rowadj) {
    var
        hs, i;
    for (i = conHotSpots.length - 1; i >= 0; i--) {
        hs = conHotSpots[i];
        if ((hs.row >= row1) && (hs.row <= row2))
            hs.row += rowadj;
    }
}

// scroll screen up 1 row
function scrollUp() {
    var
        fromRow, toRow,
        j, hs;

    if (modeRegionOrigin) {
        fromRow = regionTopRow;
        toRow = regionBottomRow;
    } else {
        fromRow = 0;
        toRow = conRowAttr.length - 1;
    }

    expandToRow(crtRows);
    for (j = fromRow; j < toRow; j++) {
        conText[j] = conText[j + 1];
        conCellAttr[j] = conCellAttr[j + 1];
        redrawRow(j);
    }
    // clear bottow row
    conText[toRow] = '';
    conCellAttr[toRow] = [];
    redrawRow(toRow);

    // move / clear hotspots
    clearHotSpotsRow(fromRow, 0, 999);
    moveHotSpotsRows(fromRow + 1, toRow, -1);
}

// scroll screen down 1 row
function scrollDown() {
    var
        fromRow, toRow,
        j, hs;

    if (modeRegionOrigin) {
        fromRow = regionTopRow;
        toRow = regionBottomRow;
    } else {
        fromRow = 0;
        toRow = conRowAttr.length - 1;
    }

    expandToRow(crtRows);
    for (j = toRow; j > fromRow; j--) {
        conText[j] = conText[j - 1];
        conCellAttr[j] = conCellAttr[j - 1];
        redrawRow(j);
    }
    // clear top row
    conText[fromRow] = '';
    conCellAttr[fromRow] = [];
    redrawRow(fromRow);

    // move / clear hotspots
    clearHotSpotsRow(toRow, 0, 999);
    moveHotSpotsRows(fromRow, toRow - 1, +1);
}

// output character using current attribute at cursor position.
function conPrintChar(chr) {
    if (isprint(chr)) {
        crsrSkipTime = new Date().getTime();
        conPutChar(crsrRow, crsrCol, chr, cellAttr);
        crsrCol++;
        if (!modeAutoWrap) {
            if (crsrCol >= crtCols)
                crsrCol--;
        } else {
            if (crsrCol == colsOnRow(crsrRow)) {
                crsrCol = 0;
                crsrRow++
            }
        }
        lastChar = chr;
    }
}

function resetTerminal() {
    var
        i;

    modeVTXANSI = false;
    modeBlinkBright = false;
    modeCursor = true;
    modeBoldFont = false;
    modeNoBold = false;
    modeBlinkFont = false;
    modeNoBlink = false;
    conBaud = 0;
    modeDOORWAY = false;
    cellAttr = defCellAttr;
    pageAttr = defPageAttr;
    crsrAttr = defCrsrAttr;

    pageDiv.parentNode.style['background-color'] = ansiColors[(pageAttr >> 8) & 0xFF];
    pageDiv.style['background-color'] = ansiColors[pageAttr & 0xFF];
    conFontNum = 0;                 // current font being used.
    for (i = 0; i < 16; i++) {      // set default font selects.
        conFont[i] = fontName;
        conFontCP[i] = codePage;
    }
}

// the big function - ansi sequence state machine. ###CALL conBufferOut!###
function conCharOut(chr) {
    var
        def,
        i, j, l,            // generic idx, length
        r, c, v,            // row, col idx
        crsrrender = false, // redraw cursor?
        doCSI = false,      // execute compiled CSI at end?
        parm,               // parameters
        str,
        els, div, img,      // for svg sprite creation
        fromRow, toRow,     // indexes for scrolling.
        hs = {},            // hotspot
        row, c1, c2,
        rpos,
        csize,
        spriteTop,
        spriteLeft,
        el;

    if (cbm) {
        // PETSCII ------------------------------------------------------------
        switch (chr) {
            case 3:  // run/stop
                // ignore
                break;

            case 5:  // white
                cellAttr = setCellAttrFG(cellAttr, 1);
                break;

            case 7:     // bell
                soundBell.pause();
                soundBell.play();
                break;

            case 8:  // shift disable
                modeCBMShift = false;
                break;

            case 9:  // shift enable
                modeCBMShift = true;
                break;

            case 13: // cr
            case 141: // lf
                crsrCol = 0;
                crsrRow++;
                crsrrender = true;
                cellAttr = setCellAttrReverse(cellAttr, false);
                break;

            case 14: // text mode
                conFontNum = 1;
                renderAll();
                break;

            case 17: // down
                crsrRow++;
                crsrrender = true;
                break;

            case 18: // rev on
                cellAttr = setCellAttrReverse(cellAttr, true);
                break;

            case 19: // home
                crsrRow = 0;
                crsrCol = 0;
                crsrrender = true;
                break;

            case 20: // del
                if (crsrCol > 0)
                    crsrCol--;
                delChar(crsrRow, crsrCol);
                redrawRow(crsrRow);
                crsrrender = true;
                break;

            case 28: // red
                cellAttr = setCellAttrFG(cellAttr, 2);
                crsrAttr = setCrsrAttrColor(2);
                break;

            case 29: // right
                crsrCol++;
                if (crsrCol > crtCols){
                    crsrCol = 0;
                    crsrRow++;
                }
                crsrrender = true;
                break;

            case 30: // green
                cellAttr = setCellAttrFG(cellAttr, 5);
                crsrAttr = setCrsrAttrColor(5);
                break;

            case 31: // blue
                cellAttr = setCellAttrFG(cellAttr, 6);
                crsrAttr = setCrsrAttrColor(6);
                break;

            case 129: // orange
                cellAttr = setCellAttrFG(cellAttr, 8);
                crsrAttr = setCrsrAttrColor(8);
                break;

            case 133: case 134: case 135: case 136:
            case 137: case 138: case 139: case 140:
                // function keys.
                // ignore
                break;

            case 142: // graphics mode
                conFontNum = 0;
                renderAll();
                break;

            case 144: // black
                cellAttr = setCellAttrFG(cellAttr, 0);
                crsrAttr = setCrsrAttrColor(0);
                break;

            case 145: // up
                if (crsrRow > 0)
                    crsrRow--;
                crsrrender = true;
                break;

            case 146: // rev off
                cellAttr = setCellAttrReverse(cellAttr, false);
                break;

            case 147: // clr
                // clear entire screen.
                // remove html rows
                els = document.getElementsByClassName('vtx');
                for (l = els.length, r = l - 1; r >= 0; r--)
                    els[r].parentNode.removeChild(els[r]);
                conRowAttr = [];
                conCellAttr = [];
                conText = [];
                document.body.style['cursor'] = 'default';
                crsrRow = crsrCol = 0;
                crsrrender = true;
                break;

            case 148: // insert
                insChar(crsrRow, crsrCol, 0x20);
                redrawRow(crsrRow);
                break;

            case 149: // brown
                cellAttr = setCellAttrFG(cellAttr, 9);
                crsrAttr = setCrsrAttrColor(9);
                break;

            case 150: // lt red
                cellAttr = setCellAttrFG(cellAttr, 10);
                crsrAttr = setCrsrAttrColor(10);
                break;

            case 151: // dk gray
                cellAttr = setCellAttrFG(cellAttr, 11);
                crsrAttr = setCrsrAttrColor(11);
                break;

            case 152: // gray
                cellAttr = setCellAttrFG(cellAttr, 12);
                crsrAttr = setCrsrAttrColor(12);
                break;

            case 153: // lt green
                cellAttr = setCellAttrFG(cellAttr, 13);
                crsrAttr = setCrsrAttrColor(13);
                break;

            case 154: // lt blue
                cellAttr = setCellAttrFG(cellAttr, 14);
                crsrAttr = setCrsrAttrColor(14);
                break;

            case 155: // lt gray
                cellAttr = setCellAttrFG(cellAttr, 15);
                crsrAttr = setCrsrAttrColor(15);
                break;

            case 156: // purple
                cellAttr = setCellAttrFG(cellAttr, 4);
                crsrAttr = setCrsrAttrColor(4);
                break;

            case 157: // left
                if (crsrCol > 0)
                    crsrCol--;
                crsrrender = true;
                break;

            case 158: // yellow
                cellAttr = setCellAttrFG(cellAttr, 7);
                crsrAttr = setCrsrAttrColor(7);
                break;

            case 159: // cyan
                cellAttr = setCellAttrFG(cellAttr, 3);
                crsrAttr = setCrsrAttrColor(3);
                break;

            default:
                // try to print it.
                if ((chr & 0x7F) >= 0x20) {
                    conPrintChar(chr);
                    crsrrender = true;
                }
                break;
        }
    } else if (atari) {
        // ATASCII ------------------------------------------------------------
        switch (chr) {
            case 0x1C: // cursor up.
                if (crsrRow > 0)
                    crsrRow--;
                crsrrender = true;
                break;
                
            case 0x1D: // cursor down.
                crsrRow++;
                crsrrender = true;
                break;
                
            case 0x1E: // cursor left.
                if (crsrCol > 0)
                    crsrCol--;
                crsrrender = true;
                break;
                
            case 0x1F: // cursor right.
                crsrCol++;
                if (crsrCol > crtCols){
                    crsrCol = 0;
                    crsrRow++;
                }
                crsrrender = true;
                break;
                
            case 0x7D: // clear screen.
                // clear entire screen.
                // remove html rows
                els = document.getElementsByClassName('vtx');
                for (l = els.length, r = l - 1; r >= 0; r--)
                    els[r].parentNode.removeChild(els[r]);
                conRowAttr = [];
                conCellAttr = [];
                conText = [];
                document.body.style['cursor'] = 'default';
                crsrRow = crsrCol = 0;
                crsrrender = true;
                break;

            case 0x7E: // backspace - TODO : find out if destructive
                if (crsrCol > 0)
                    crsrCol--;
                crsrrender = true;
                break;
                
            case 0x7F: // tab
                // find next tabstop
                for (i = crsrCol+1; i < crtCols; i++){ 
                    if (conTabStop[i]) {
                        crsrCol = i;
                        crsrrender = true;
                        break;
                    }
                }
                break;
            
            case 0x9B: // return
                crsrCol = 0;
                crsrRow++;
                crsrrender = true;
                cellAttr = setCellAttrReverse(cellAttr, false);
                break;
            
            case 0x9C: // delete row
                delRow(crsrRow);
                break;
                
            case 0x9D: // insert row
                insRow(crsrRow);
                break;
                
            case 0x9E: // clear tabstop
            case 0x9F: // set tabstop
                conTabStop[crsrCol] = (chr == 0x9F);
                break;
                
            case 0xFD: // buzzer
                soundBell.pause();
                soundBell.play();
                break;
                
            case 0xFE: // delete char
                expandToRow(crsrRow);
                expandToCol(crsrRow, crsrCol);
                delChar(crsrRow, crsrCol);
                redrawRow(crsrRow);
                crsrrender = true;
                break;
                    
            case 0xFF: // insert char
                break;
            
            default:
                // set reverse on off for each character in atari mode
                cellAttr = setCellAttrReverse(cellAttr, (chr & 0x80));
                conPrintChar(chr & 0x7F);
                crsrrender = true;
                break;
        }
    } else {
        // ANSI ---------------------------------------------------------------

        if (modeNextGlyph){
            // print glyph AS IS
            conPrintChar(chr);
            crsrrender = true;
            modeNextGlyph = false;
        } else {
            // do all normal ctrls first
            switch (chr) {
                case _NUL:     // null
                    if (modeDOORWAY)
                        modeNextGlyph = true;
                    break;

                case _BEL:     // bell
                    soundBell.pause();
                    soundBell.play();
                    break;

                case _BS:     // backspace
                    if (crsrCol > 0) {
                        expandToRow(crsrRow);
                        expandToCol(crsrRow, crsrCol);
                        crsrCol--;
                        //delChar(crsrRow, crsrCol);  // backspace is not destructive
                        crsrrender = true;
                    }
                    break;

                case _HT:     // horz tab
                    crsrCol = ((crsrCol >> 3) + 1) << 3;
                    if (crsrCol > colsOnRow(crsrRow))
                        crsrCol = colsOnRow(crsrRow);
                    crsrrender = true;
                    break;

                case _LF:    // linefeed
                    if (!modeVTXANSI)  // LF dont CR!  lol
                        crsrCol = 0;    // for BBS/ANSI.SYS mode
                    crsrRow++;
                    crsrrender = true;
                    break;

                case _CR:    // carriage return
                    crsrCol = 0;
                    crsrrender = true;
                    break;

                case _DEL:   // delete (not on font 10 or 11)
                    if (conFontNum < 10) {
                        expandToRow(crsrRow);
                        expandToCol(crsrRow, crsrCol);
                        delChar(crsrRow, crsrCol);
                        redrawRow(crsrRow);
                        crsrrender = true;
                    } else {
                        conPrintChar(chr);
                        crsrrender = true;
                    }
                    break;

                default:
                    switch (ansiState) {
                        case 0:
                            // not in an sequence.
                            if (chr == _ESC)
                                ansiState = 1
                            else {
                                // convert to codepage
                                conPrintChar(chr);
                                crsrrender = true;
                            }
                            break;

                        case 1:
                            // start of ansi sequence
                            if (chr == 0x5B) {
                                // ESC [ - CSI
                                parms = '';
                                interm = '';
                                ansiState = 2
                            }
                            else if (chr == 0x23)
                                // ESC # - row attr
                                ansiState = 3
                            else if (chr == 0x37) {
                                // ESC 7 - Save crsr pos and attrs
                                crsrSaveRow = crsrRow;
                                crsrSaveCol = crsrCol;
                                cellSaveAttr = cellAttr;
                                ansiState = 0;
                            } else if (chr == 0x38) {
                                // ESC 8 - restore crsr pos and attrs
                                crsrRow = crsrSaveRow;
                                crsrCol = crsrSaveCol;
                                cellAttr = cellSaveAttr;
                                ansiState = 0;
                            } else if (chr == 0x44) {
                                // ESC D - Scroll up 1
                                scrollUp();
                                ansiState = 0;
                            } else if (chr == 0x45) {
                                // ESC E - Move to next line
                                crsrRow++;
                                crsrrender = true;
                                ansiState = 0;
                            } else if (chr == 0x4D) {
                                // ESC M - Scroll down 1
                                scrollDown();
                                ansiState = 0;
                            } else if (chr == 0x63) {
                                // ESC c - reset terminal
                                resetTerminal();
                            } else
                                // unrecognized - abort sequence
                                ansiState = 0;
                            break;

                        case 2:
                            // start of CSI (ESC [)
                            // collect parameters until either intermediate or final
                            if ((chr >= 0x30) && (chr <= 0x3F))
                                parms += String.fromCharCode(chr)
                            else if ((chr >= 0x20) && (chr <= 0x2F)) {
                                // intermediate byte
                                interm = String.fromCharCode(chr);
                                ansiState = 5;
                            } else if ((chr >= 0x40) && (chr <= 0x7E)) {
                                // final byte
                                ansiState = 0;
                                doCSI = true;
                            } else
                                // unrecognized - abort sequence
                                ansiState = 0;
                            break

                        case 3:
                            // start of row attr (ESC #)
                            // get single byte (0,1,9)
                            switch (chr) {
                                case 0x30:
                                    // ESC # 0 - reset row
                                    conRowAttr[crsrRow] = defRowAttr;
                                    break;
                                
                                case 0x31:
                                    // ESC # 1 - single wide, double high, top
                                    conRowAttr[crsrRow] &=
                                        ~(A_ROW_DISPLAY_MASK | A_ROW_WIDTH_MASK);
                                    conRowAttr[crsrRow] |= (A_ROW_DISPLAY_TOP | A_ROW_WIDTH_100);
                                    break;
                                    
                                case 0x32:
                                    // ESC # 2 - single wide, double high, bottom
                                    conRowAttr[crsrRow] &=
                                        ~(A_ROW_DISPLAY_MASK | A_ROW_WIDTH_MASK);
                                    conRowAttr[crsrRow] |= (A_ROW_DISPLAY_BOTTOM | A_ROW_WIDTH_100);
                                    break;
                                    
                                case 0x33:
                                    // ESC # 3 - double wide/high, top
                                    conRowAttr[crsrRow] &=
                                        ~(A_ROW_DISPLAY_MASK | A_ROW_WIDTH_MASK);
                                    conRowAttr[crsrRow] |= (A_ROW_DISPLAY_TOP | A_ROW_WIDTH_200);
                                    break;
                                    
                                case 0x34:
                                    // ESC # 4 - double wide/high, bottom
                                    conRowAttr[crsrRow] &=
                                        ~(A_ROW_DISPLAY_MASK | A_ROW_WIDTH_MASK);
                                    conRowAttr[crsrRow] |= (A_ROW_DISPLAY_BOTTOM | A_ROW_WIDTH_200);
                                    break;
                                    
                                case 0x35:
                                    // ESC # 5 - single wide/high
                                    conRowAttr[crsrRow] &=
                                        ~(A_ROW_DISPLAY_MASK | A_ROW_WIDTH_MASK);
                                    conRowAttr[crsrRow] |= (A_ROW_WIDTH_100);
                                    break;
                                    
                                case 0x36:
                                    // ESC # 6 - single high / double wide
                                    conRowAttr[crsrRow] &=
                                        ~(A_ROW_DISPLAY_MASK | A_ROW_WIDTH_MASK);
                                    conRowAttr[crsrRow] |= (A_ROW_WIDTH_200);
                                    break;
                                    
                                case 0x37:
                                    // marquee off
                                    // ESC # 7
                                    conRowAttr[crsrRow] &= ~A_ROW_MARQUEE;
                                    getRowElement(crsrRow).firstChild.classList.remove('marquee')
                                    break;
                                    
                                case 0x38:
                                    // marquee on
                                    // ESC # 8
                                    conRowAttr[crsrRow] |= A_ROW_MARQUEE;
                                    getRowElement(crsrRow).firstChild.classList.add('marquee');
                                    break;
                            }
                            adjustRow(crsrRow);
                            ansiState = 0;
                            break;

                        case 5:
                            // collecting intermediate bytes
                            if ((chr >= 0x20) && (chr <= 0x2F))
                                interm += String.fromCharCode(chr)
                            else if ((chr >= 0x40) && (chr <= 0x7E)) {
                                // command?
                                ansiState = 0;
                                doCSI = true;
                            } else
                                // unrecognized - abort sequence
                                ansiState = 0;
                            break;
                    }
                    break;
            }
        }

        if (doCSI) {
            // chr = command / final byte
            // params = optional parameters
            // interm = optional intermediate (not using any for this term emulation - ignore)

            parm = parms.split(';');
            if (parm[0] == '') parm = [];

            // for our purposes, all parameters are integers. if not, leave as string.
            l = parm.length;
            for (i = 0; i < l; i++) {
                v = parseInt(parm[i]);
                if (v.toString() == parm[i])
                    parm[i] = v;
            }
            
            switch (chr) {
                case 0x40:  // @ - ICH - insert characters
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++)
                        insChar(crsrRow, crsrCol, 32);
                    redrawRow(crsrRow);
                    break;

                case 0x41:  // A - Cursor Up
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    crsrRow -= parm[0];
                    if (modeRegionOrigin) {
                        if (crsrRow < regionTopRow)
                            crsrRow = regionTopRow;
                    } else {
                        if (crsrRow < 0)
                            crsrRow = 0;
                    }
                    crsrrender = true;
                    break;

                case 0x42:  // B - Cursor Down
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    crsrRow += parm[0];
                    if (modeRegionOrigin) {
                        if (crsrRow > regionBottomRow)
                            crsrRow = regionBottomRow;
                    }
                    crsrrender = true;
                    break;

                case 0x43:  // C - Cursor Forward
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    crsrCol += parm[0];
                    if (crsrCol >= colsOnRow(crsrRow) - 1)
                        crsrCol = colsOnRow(crsrRow) - 1;
                    crsrrender = true;
                    break;

                case 0x44:  // D - Cursor Backward / Font Selection
                    if (interm == ' ') {
                        // set font
                        parm = fixParams(parm, [0, 0]);
                        switch (parm[1]) {
                            case  0: // Codepage 437 English
                            case 26: // Codepage 437 English, (thin)
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'CP437';
                                break;

                            case  5: // Codepage 866 (c) Russian
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'CP866';
                                break;

                            case 17: // Codepage 850 Multilingual Latin I, (thin)
                            case 18: // Codepage 850 Multilingual Latin I
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'CP850';
                                break;

                            case 25: // Codepage 866 Russian
                            case 27: // Codepage 866 (b) Russian
                            case 29: // Ukrainian font cp866u
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'CP866';
                                break;

                            case 19: // Codepage 885 Norwegian, (thin)
                            case 28: // Codepage 885 Norwegian
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'CP885';
                                break;

                            case 31: // Codepage 1131 Belarusian, (swiss)
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'CP1131';
                                break;

                            case  1: // Codepage 1251 Cyrillic, (swiss)
                            case 20: // Codepage 1251 Cyrillic
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'WIN1251';
                                break;

                            case  2: // Russian koi8-r
                            case 12: // Russian koi8-r (b)
                            case 22: // Russian koi8-r (c)
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'KOI8_R';
                                break;

                            case  9: // Ukrainian font koi8-u
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'KOI8_U';
                                break;

                            case 24: // ISO-8859-1 West European
                            case 30: // ISO-8859-1 West European, (thin)
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ISO8859_1';
                                break;

                            case  3: // ISO-8859-2 Central European
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ISO8859_2';
                                break;

                            case  4: // ISO-8859-4 Baltic wide (VGA 9bit mapped)
                            case 11: // ISO-8859-4 Baltic (VGA 9bit mapped)
                            case 13: // ISO-8859-4 Baltic wide
                            case 23: // ISO-8859-4 Baltic
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ISO8859_4';
                                break;

                            case 14: // ISO-8859-5 Cyrillic
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ISO8859_5';
                                break;

                            case 21: // ISO-8859-7 Greek
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ISO8859_7';
                                break;

                            case  6: // ISO-8859-9 Turkish
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ISO8859_9';
                                break;

                            case 10: // ISO-8859-15 West European, (thin)
                            case 16: // ISO-8859-15 West European
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ISO8859_15';
                                break;

                            case 15: // ARMSCII-8 Character set
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'ARMSCII_8';
                                break;

                            case  7: // haik8 codepage (use only with armscii8 screenmap)
                                conFont[parm[0]] = fontName;
                                conFontCP[parm[0]] = 'HAIK8';
                                break;

                            //case  8: // ISO-8859-8 Hebrew

                            // CONVERTED FONTS - USE E000-E0FF for map
                            case 32: // Commodore 64 (UPPER)
                                conFont[parm[0]] =  'C640';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 33: // Commodore 64 (Lower)
                                conFont[parm[0]] =  'C641';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 34: // Commodore 128 (UPPER)
                                conFont[parm[0]] =  'C1280';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 35: // Commodore 128 (Lower)
                                conFont[parm[0]] =  'C1281';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 36: // Atari
                                conFont[parm[0]] =  'ATARI';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 37: // P0T NOoDLE (Amiga)
                                conFont[parm[0]] = 'P0TNOODLE';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 38: // mO'sOul (Amiga)
                                conFont[parm[0]] = 'MOSOUL';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 39: // MicroKnight Plus (Amiga)
                                conFont[parm[0]] = 'MICROKNIGHTPLUS';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 40: // Topaz Plus (Amiga)
                                conFont[parm[0]] = 'TOPAZPLUS';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 41: // MicroKnight (Amiga)
                                conFont[parm[0]] = 'MICROKNIGHT';
                                conFontCP[parm[0]] = 'RAW';
                                break;

                            case 42: // Topaz (Amiga)
                                conFont[parm[0]] = 'TOPAZ';
                                conFontCP[parm[0]] = 'RAW';
                                break;
                                
                            case 100: // TI994 (TI-99/4)
                                conFont[parm[0]] = 'TI994';
                                conFontCP[parm[0]] = 'RAWHI';
                                break;
                        }
                        // load it if it's needed.
                        loadSingleFont(conFont[parm[0]]);
                        
                    } else {
                        // move backwards
                        parm = fixParams(parm, [1]);
                        parm[0] = minMax(parm[0], 1, 999);
                        crsrCol -= parm[0];
                        if (crsrCol < 0)
                            crsrCol = 0;
                        crsrrender = true;
                    }
                    break;

                case 0x45:  // E - Next Line
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    crsrCol = 0;
                    crsrRow += parm[0];
                    crsrrender = true;
                    break;

                case 0x46:  // F - Previous Line
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    crsrCol = 0;
                    crsrRow -= parm[0];
                    if (crsrRow < 0)
                        crsrRow = 0;
                    crsrrender = true;
                    break;

                case 0x47:  // G - To Column
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    crsrCol = parm[0] - 1;
                    crsrrender = true;
                    break;

                    // H - see f

                case 0x49:  // I - CFT - forward tab
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++) {
                        crsrCol = (((crsrCol >> 3) + 1) << 3);
                        if (crsrCol > colsOnRow(crsrRow)) {
                            crsrCol = colsOnRow(crsrRow);
                            break;
                        }
                    }
                    crsrrender = true;
                    break;

                case 0x4A:  // J - Erase in Screen (0=EOS,1=SOS,2=ALL)
                    parm = fixParams(parm, [0]);
                    parm[0] = minMax(parm[0], 0, 2, 0);
                    expandToRow(crsrRow);
                    expandToCol(crsrRow, crsrCol);
                    switch (parm[0]) {
                        case 0:
                            // clear EOL first
                            clearHotSpotsRow(crsrRow, crsrCol, 999);
                            conCellAttr[crsrRow].length = crsrCol;
                            conText[crsrRow] = conText[crsrRow].substring(0, crsrCol);
                            // clear EOS
                            clearHotSpotsRows(crsrRow + 1, conRowAttr.length);
                            for (r = getMaxRow(); r > crsrRow; r--) {
                                row = getRowElement(r);
                                row.parentNode.removeChild(row);
                                conRowAttr.length = crsrRow + 1;
                                conCellAttr.length = crsrRow + 1;
                                conText.length = crsrRow + 1;
                            }
                            break;

                        case 1:
                            // clear SOL first
                            clearHotSpotsRow(crsrRow, 0, crsrCol-1);
                            for (c = 0; c <= crsrCol; c++)
                                conPutChar(crsrRow, c, 32, defCellAttr);
                            redrawRow(crsrRow);

                            // clear SOS
                            clearHotSpotsRows(0, crsrRow-1);
                            for (r = 0; r < crsrRow; r++) {
                                conRowAttr[r] = defRowAttr;
                                conCellAttr[r] = [];
                                conText[r] = '';
                                adjustRow(crsrRow);
                                redrawRow(crsrRow);
                            }
                            break;

                        case 2:
                            // clear entire screen.
                            // remove html rows
                            els = document.getElementsByClassName('vtx');
                            for (l = els.length, r = l - 1; r >= 0; r--)
                                els[r].parentNode.removeChild(els[r]);
                            // remove sprites
                            els = document.getElementsByClassName('sprite');
                            for (l = els.length, r = l - 1; r >= 0; r--)
                                els[r].parentNode.removeChild(els[r]);
                            // reset console
                            conRowAttr = [];
                            conCellAttr = [];
                            conText = [];
                            conHotSpots = [];
                            lastHotSpot = null;
                            document.body.style['cursor'] = 'default';
                            if (!modeVTXANSI) {
                                crsrRow = crsrCol = 0   // BBS / ANSI.SYS
                                crsrrender = true;
                            } else {
                                expandToRow(crsrRow);   // ECMA-048 complient
                                expandToCol(crsrRow, crsrCol);
                            }
                            if (modeVTXANSI)
                                cellAttr = defCellAttr;
                            break;
                    }
                    break;

                case 0x4B:  // K - Erase in Line
                    parm = fixParams(parm, [0]);
                    parm[0] = minMax(parm[0], 0, 2, 0);
                    expandToRow(crsrRow);
                    expandToCol(crsrRow, crsrCol);
                    switch (parm[0]) {
                        case 0:
                            // clear EOL
                            clearHotSpotsRow(crsrRow, crsrCol, 999);
                            conCellAttr[crsrRow].length = crsrCol;
                            conText[crsrRow] = conText[crsrRow].substring(0, crsrCol);
                            redrawRow(crsrRow);
                            break;

                        case 1:
                            // clear SOL
                            clearHotSpotsRow(crsrRow, 0, crsrCol);
                            for (c = 0; c <= crsrCol; c++)
                                conPutChar(crsrRow, c, 32, defCellAttr);
                            redrawRow(crsrRow);
                            break;

                        case 2:
                            // clear row.
                            clearHotSpotsRow(crsrRow, 0, 999);
                            conText[crsrRow] = '';
                            conCellAttr[crsrRow] = [];
                            redrawRow(crsrRow);
                            break;
                    }
                    break;

                case 0x4C:  // L - EL - insert lines
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++)
                        insRow(crsrRow);
                    break;
 
                case 0x4D:  // M - DL - delete lines
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++)
                        delRow(crsrRow);
                    break;
 
                case 0x50:  // P - DCH - delete character
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++)
                        delChar(crsrRow, crsrCol);
                    redrawRow(crsrRow);
                    break;
 
                case 0x53:  // S - Scroll Up (SU). Scroll up.
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++)
                        scrollUp();
                    break;
 
                case 0x54:  // T - Scroll Down (SD). Scroll down.
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++)
                        scrollDown();
                    break;
 
                case 0x58:  // X - ECH - erase n characters
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++) {
                        conPutChar(crsrRow, crsrCol + i, 0x20, defCellAttr);
                    }
                    break;
 
                case 0x5A:  // Z - CBT - back tab
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++) {
                        crsrCol = (((crsrCol >> 3) + 1) << 3) - 16;
                        if (crsrCol <= 0) {
                            crsrCol = 0;
                            break;
                        }
                    }
                     crsrrender = true;
                     break;
 
                /* special VTX sequences start */
                case 0x5B:  // [ - Row Size
                    parm = fixParams(parm, [3,1]);
                    parm[0] = minMax(parm[0], 0, 7, 3);
                    parm[1] = minMax(parm[1], 0, 3, 1);
                    conRowAttr[crsrRow] = setRowAttrHeight(conRowAttr[crsrRow],
                        (parm[0] + 1) * 25);
                    conRowAttr[crsrRow] = setRowAttrWidth(conRowAttr[crsrRow],
                        (parm[1] + 1) * 50);
                    adjustRow(crsrRow);
                    crsrrender = true;
                    break;
 
                case 0x5C:  // \ - hotspots
                    if (l < 2) {
                        // reset all hotspots
                        conHotSpots = [];
                    } else {
                        switch (parm[0]) {
                            case 0: // string binds
                            case 1: // url binds
                                if (l >= 4) {
                                    // need all the parts.
                                    hs = {
                                        type:   parm[0],
                                        row:    crsrRow,
                                        col:    crsrCol,
                                        width:  parm[1],
                                        height: parm[2],
                                        hilite: parm[3],
                                        val:    ''
                                    };
                                    for (i = 4; i < l; i++)
                                        hs.val += String.fromCharCode(parm[i]);
                                    conHotSpots.push(hs);
                                }
                                break;
                        }
                    }
                    break;
 
                case 0x5D:  // ] - Row Modes / background
                    parm = fixParams(parm, [0,0,0]);
                    parm[0] = minMax(parm[0], 0, 255, 0);
                    parm[1] = minMax(parm[1], 0, 255, 0);
                    parm[2] = minMax(parm[2], 0, 3, 0);
                    conRowAttr[crsrRow] = setRowAttrColor1(conRowAttr[crsrRow], parm[0]);
                    conRowAttr[crsrRow] = setRowAttrColor2(conRowAttr[crsrRow], parm[1]);
                    conRowAttr[crsrRow] = setRowAttrPattern(conRowAttr[crsrRow], parm[2] << 16);
 
                    // set row attrs here
                    row = getRowElement(crsrRow);
                    c1 = ansiColors[parm[0]];
                    c2 = ansiColors[parm[1]];
                    switch (parm[2] << 16) {
                        case A_ROW_PATTERN_SOLID:
                            row.style['background'] = c1;
                            break;
 
                        case A_ROW_PATTERN_HORZ:
                            row.style['background'] = 'linear-gradient(to bottom,' + c1 + ',' + c2 + ')';
                            break;
 
                        case A_ROW_PATTERN_VERT:
                            row.style['background'] = 'linear-gradient(to right,' + c1 + ',' + c2 + ')';
                            break;
                    }
                    break;
 
                case 0x5E:  // ^ - Cursor / Page Modes
                    if (!parm.length){
                        // no paremeters - reset cursor to default
                    } else {
                        switch (parm[0]) {
                            case 0:// cursor color
                                i = (parm[1] & 0xFF);
                                crsrAttr = setCrsrAttrColor(crsrAttr, i);
                                newCrsr();
                                break;
 
                            case 1:// cursor size
                                i = (parm[1] & 0x03);
                                crsrAttr = setCrsrAttrSize(crsrAttr, i);
                                newCrsr();
                                break;
 
                            case 2:// cursor orientation
                                i = (parm[1] ? A_CRSR_ORIENTATION : 0);
                                crsrAttr = setCrsrAttrOrientation(crsrAttr, i);
                                newCrsr();
                                break;
 
                            case 3:// page border color
                                i = (parm[1] & 0xFF);
                                pageAttr = setPageAttrBorder(pageAttr, i);
                                pageDiv.parentNode.style['background-color'] = ansiColors[(pageAttr >> 8) & 0xFF];
                                break;
 
                            case 4:// page background color
                                i = (parm[1] & 0xFF);
                                pageAttr = setPageAttrBackground(pageAttr, i);
                                pageDiv.style['background-color'] = ansiColors[pageAttr & 0xFF];
                                break;
 
                            case 5: // CSI '5' ; f ; b '^' : Set hotspot mouseover colors.
                                hotspotAttr &= 0xFFFF0000;
                                hotspotAttr |= (
                                    (parm[1] & 0xFF) |
                                    ((parm[2] & 0xFF) << 8)
                                );
                                break;
 
                            case 6: // CSI '6' ; f ; b '^' : Set hotspot click colors.
                                hotspotAttr &= 0x0000FFFF;
                                hotspotAttr |= (
                                    ((parm[1] & 0xFF) << 16) |
                                    ((parm[2] & 0xFF) << 24)
                                );
                                break;
                        }
                    }
                    crsrrender = true;
                    break;
 
                case 0x5F:  // _ - VTX Media Codes. Sprites & Audio
                    if (parms.length == 0) {
                        
                        // syncronet got here how?
                        console.log('CSI '+ parms + interm + String.fromCharCode(chr));
                        break;
                    }
                    if (parm[0] == 0) {
                        // sprite commands
                        switch (parm[1]) {
                            case 0:
                                // define / clear sprite object.
                                // parm2 = num
                                // parm3 = type
                                // parm4.. = hex3 data (rejoin with ;)
                                if (l == 2) {
                                    // clear all sprite objects.
                                    spriteDefs = [];
                                } else if (l == 3) {
                                    // clear single sprite object.
                                    spriteDefs[parm[2]] = null;
                                } else if (l > 4) {
                                    // define a sprite object
                                    str = '';
                                    switch (parm[3]){
                                        case 0:
                                            // url : unicode encoded characters 
                                            for (i = 4; i < l; i++) 
                                                str += String.fromCharCode(parseInt(parm[i]));
                                            spriteDefs[parm[2]] = stripNL(str);
                                            break;
                                             
                                        case 1:
                                            // UTF8 url : hex3 encoded
                                            for (i = 4; i < l; i++)
                                                str += ';'+parm[i];
                                            str = str.substring(1);
                                            spriteDefs[parm[2]] = stripNL(UFT8ArrayToStr(decodeHex3(str)));
                                            break;
                                             
                                        case 2:
                                            // raw UTF8 svg : hex3 encoded
                                            for (i = 4; i < l; i++)
                                                str += ';'+parm[i];
                                            str = str.substring(1);
                                            spriteDefs[parm[2]] = 'data:image/svg+xml;charset=utf-8,'
                                                + encodeURIComponent(stripNL(UFT8ArrayToStr(decodeHex3(str))));
                                            break;
 
                                        case 3:
                                            // raw UTF8 svg deflated : hex3 encoded
                                            for (i = 4; i < l; i++)
                                                str += ';'+parm[i];
                                            str = str.substring(1);
                                            spriteDefs[parm[2]] = 'data:image/svg+xml;charset=utf-8,'
                                                + encodeURIComponent(stripNL(UFT8ArrayToStr(inflateRaw(decodeHex3(str)))));
                                            break;
                                             
                                        case 4:
                                            // raw UTF8 svg Base64: hex3 encoded
                                            for (i = 4; i < l; i++)
                                                str += ';'+parm[i];
                                            str = str.substring(1);
                                            spriteDefs[parm[2]] = 'data:image/svg+xml;base64,' 
                                                + stripNL(UFT8ArrayToStr(decodeHex3(str)));
                                            break;
                                    }
                                }
                                break;
 
                            case 1:
                                // display / remove sprite from display.
                                if (l == 2) {
                                    // remove all sprints from display
                                    els = document.getElementsByClassName('sprite');
                                    for (i = els.length - 1; i >= 0; i--)
                                        els[i].parentNode.removeChild(els[i]);
                                } else if (l == 3) {
                                    // remove sprite s from display
                                    div = document.getElementById('sprite' + parm[2]);
                                    if (div != null)
                                        div.parentNode.removeChild(div);
                                } else if (l == 7) {
                                    // display a new sprite
                                    // remove old one if it exists first
                                    div = document.getElementById('sprite' + parm[2]);
                                    if (div != null)
                                        div.parentNode.removeChild(div);
 
                                    rpos = getElementPosition(getRowElement(crsrRow));
                                    csize = getRowFontSize(crsrRow);
                                    spriteTop = rpos.top - pageTop;
                                    spriteLeft = crsrCol * csize.width;
 
                                    // make a new one.
                                    div = domElement(
                                        'div',
                                        {   className:  'sprite',
                                            row :       crsrRow,
                                            col :       crsrCol,
                                            id :        'sprite' + parm[2] },
                                        {   position:   'absolute',
                                            left:       spriteLeft + 'px',
                                            top:        spriteTop + 'px',
                                            backgroundColor: 'green',
                                            width:      (colSize * parm[4]) + 'px',
                                            height:     (rowSize * parm[5]) + 'px',
                                            overflow:   'hidden'});
 
                                    img = domElement(
                                        'img',
                                        {   onload:     fitSVGToDiv,
                                            src:        spriteDefs[parm[3]] },
                                        {   visibility: 'hidden',
                                            position:   'relative',
                                            left:'0px',
                                            top:'0px'
                                        });
                                        
                                    div.appendChild(img);
                                    if (parm[6] == 0)
                                        pageDiv.insertBefore(div, textDiv)
                                    else
                                        textDiv.appendChild(div);
                                }                                
                                break;
                            
                            case 2:
                                // move sprite to new r c.
                                el = document.getElementById('sprite'+parm[2]);
                                if ((el) && (l == 6))
                                    moveSprite(el, parm[3], parm[4], parm[5]);
                                break;
                                
                            case 3:
                                // move sprite to new z.
                                // TODO
                                /*
                                    CSI 0 ; 3 ; s ; z '_' : Move sprite s to new z-plane.
                                        s : Sprite number to assign to this sprite. {0}
                                        z : Z-plane. 0 = below text plane, 1 : above text plane. {0}
                                */
                                break;
                        }
                        
                    } else if (parm[0] == 1) {
                        // audio commands
                        switch (parm[1]) {
                            case 0:
                                // define / clear audio object.
                                // parm2 = num
                                // parm3 = type
                                // parm4.. = hex3 data (rejoin with ;)
                                if (l == 2) {
                                    // clear all audio objects.
                                    audioDefs = [];
                                } else if (l == 3) {
                                    // clear audio object num
                                    audioDefs[parm[2]] = null;
                                } else if (l > 4) {
                                    // define audio object
                                    str = '';
                                    switch (parm[3]) {
                                        case 0:
                                            // url : unicode encoded characters 
                                            for (i = 4; i < l; i++) 
                                                str += String.fromCharCode(parseInt(parm[i]));
                                            audioDefs[parm[2]] = str;
                                            break;
                                            
                                        case 1:
                                            // UTF8 url : hex3 encoded
                                            for (i = 4; i < l; i++)
                                                str += ';'+parm[i];
                                            str = str.substring(1);
                                            audioDefs[parm[2]] = UFT8ArrayToStr(decodeHex3(str))
                                            break;
                                            
                                        case 2:
                                            // raw mp3 : hex3 encoded
                                            // TODO
                                            audioDefs[parm[2]] = 
                                                'data:audio/mp3;base64,' + 
                                                btoa(decodeHex3(str));
                                            break;
                                            
                                        case 3:
                                            // raw mp3 deflated : hex3 encoded
                                            // TODO
                                            audioDefs[parm[2]] = 
                                                'data:audio/mp3;base64,' + 
                                                btoa(inflateRaw(decodeHex3(str)));
                                            break;
                                            
                                        case 4:
                                            // raw MP3 Base64: hex3 encoded
                                            // TODO
                                            audioDefs[parm[2]] = 'data:audio/mp3;base64,' +
                                                + stripNL(UFT8ArrayToStr(decodeHex3(str)));
                                            break;
                                    }
                                }
                                break;
                                
                            case 1:
                                // select audio object to player.
                                if (l > 2) {
                                    audio.src = audioDefs[parm[2]];
                                }
                                break;
                                
                            case 2:
                                // play / pause / stop-rewind
                                switch (parm[2]) {
                                    case 0:
                                        // stop/rewind
                                        audio.pause();
                                        audio.load();
                                        break;
                                        
                                    case 1:
                                        // play
                                        audio.play();
                                        break;
                                        
                                    case 2:
                                        // pause
                                        audio.pause();
                                        break;
                                }
                                break;
                                
                            case 3:
                                // set volume (0-100)
                                audio.volume = ((parm[2]!=null)?(parm[2]/100):0.25);
                                break;
                        }
                    }
                    break;
                /* special VTX sequences end */
                
                case 0x62:  // b - repeat last char
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    for (i = 0; i < parm[0]; i++) {
                        conPrintChar(lastChar);
                    }
                    crsrrender = true;
                    break;

                case 0x63:  // c - device attributes
                    parm = fixParams(parm, [0]);
                    if (parm[0] == 0) {
                        // request device
                        sendData(CSI + '?50;86;84;88c'); // reply for VTX
                    }
                    break;

                case 0x66:  // f - Cursor Position
                case 0x48:  // H - Cursor Position
                    // set missing to defaults of 1
                    parm = fixParams(parm, [ 1, 1 ]);
                    parm[0] = minMax(parm[0], 1, 999);
                    parm[1] = minMax(parm[1], 1, 999);
                    while (l < 2)
                        parm[l++] = 1;
                    if (modeRegionOrigin)
                        crsrRow = regionTopRow + parm[0] - 1
                    else
                        crsrRow = parm[0] - 1;
                    crsrCol = parm[1] - 1;
                    expandToRow(crsrRow);
                    crsrrender = true;
                    break;

                case 0x68:  // h - set mode
                case 0x6C:  // l - reset mode
                    parm[0] = parm[0].toString();
                    switch (parm[0]) {
                        case '?6':
                            // origin in region?
                            modeRegionOrigin = (chr == 0x68);
                            break;

                        case '?7':
                            // autowrap mode
                            modeAutoWrap = (chr == 0x68);
                            break;

                        case '?25':
                            // hide / show cursor
                            modeCursor = (chr == 0x68);
                            break;

                        case '?31':
                            // bright as font 1
                            modeBoldFont = (chr == 0x68);
                            break;

                        case '?32':
                            // bright enable/disable
                            modeNoBold = (chr == 0x68);
                            break;

                        case '?33':
                            // blink to high intensity background
                            modeBlinkBright = (chr == 0x68);
                            break;

                        case '?34':
                            //  blink as font 2
                            modeBlinkFont = (chr == 0x68);
                            break;

                        case '?35':
                            // '?35' : blink disabled
                            modeNoBlink = (chr == 0x68);
                            break;

                        case '?50':
                            // VTX / ANSIBBS mode flip
                            modeVTXANSI = (chr == 0x68);
                            break;

                        case '=255':
                            // =255 : DOORWAY mode
                            modeDOORWAY = (chr == 0x68);
                            break;
                    }
                    break;

                case 0x6D:  // m - Character Attr
                    // don't use fixparms. variable parameters.
                    if (l == 0) parm[l++] = 0; 
                    parm[0] = minMax(parm[0], 0, 255);
                    for (i = 0; i < l; i++) {
                        switch (parm[i]) {
                            case 0:     // reset
                                cellAttr = defCellAttr;
                                break;

                            case 1:     // bold on / off
                            case 21:
                                cellAttr =
                                    setCellAttrBold(cellAttr, (parm[i] < 20));
                                break;

                            case 2:     // faint on / off
                            case 22:
                                cellAttr =
                                    setCellAttrFaint(cellAttr, (parm[i] < 20));
                                break;

                            case 3:     // italics on/off
                            case 23:
                                cellAttr =
                                    setCellAttrItalics(cellAttr, (parm[i] < 20));
                                break;

                            case 4:     // underline
                            case 24:
                                cellAttr =
                                    setCellAttrUnderline(cellAttr, (parm[i] < 20));
                                break;

                            case 5:     // blink slow
                                cellAttr &= ~(A_CELL_BLINKSLOW | A_CELL_BLINKFAST);
                                cellAttr |= A_CELL_BLINKSLOW;
                                break;

                            case 6:     // blink fast
                                cellAttr &= ~(A_CELL_BLINKSLOW | A_CELL_BLINKFAST);
                                cellAttr |= A_CELL_BLINKFAST;
                                break;

                            case 25:    // all blink off
                            case 26:    // all blink off (reserved but unblink)
                                cellAttr &= ~(A_CELL_BLINKSLOW | A_CELL_BLINKFAST);
                                break;

                            case 7:     // reverse video
                            case 27:
                                cellAttr =
                                    setCellAttrReverse(cellAttr, (parm[i] < 20));
                                break;

                            case 8:     // conceal
                            case 28:
                                cellAttr = setCellAttrDisplay(cellAttr, 
                                    (parm[i] < 20) ? A_CELL_DISPLAY_CONCEAL : A_CELL_DISPLAY_NORMAL);
                                break;

                            case 9:     // strikethrough
                            case 29:
                                cellAttr =
                                    setCellAttrStrikethrough(cellAttr, (parm[i] < 20));
                                break;

                            // select font
                            case 10: case 11: case 12: case 13: case 14:
                            case 15: case 16: case 17: case 18: case 19:
                                cellAttr = setCellAttrFont(cellAttr, (parm[i] - 10));
                                break;

                            case 56:    // outline
                            case 76:
                                cellAttr =
                                    setCellAttrOutline(cellAttr, (parm[i] < 70));
                                break;

                            case 57:    // shadow
                            case 77:
                                cellAttr =
                                    setCellAttrShadow(cellAttr, (parm[i] < 70));
                                break;

                            case 58:    // top half
                            case 78:
                                cellAttr = setCellAttrDisplay(cellAttr, 
                                    (parm[i] < 70) ? A_CELL_DISPLAY_TOP : A_CELL_DISPLAY_NORMAL);
                                break;                                
                                
                            case 59:    // bottom half
                            case 79:
                                // turn this character attribute off.
                                cellAttr = setCellAttrDisplay(cellAttr, 
                                    (parm[i] < 70) ? A_CELL_DISPLAY_BOTTOM : A_CELL_DISPLAY_NORMAL);
                                break;

                            // special built in fonts
                            case 80: // teletext blocks 0x20-0x5F
                            case 81: // teletext blocks 0x20-0x5F
                            case 82: // reserved
                            case 83: // reserved
                            case 84: // reserved
                            case 85: // reserved
                                cellAttr =
                                    setCellAttrFont(cellAttr, (parm[i] - 70));
                                break;

                            // text foreground colors
                            case 30: case 31: case 32: case 33:
                            case 34: case 35: case 36: case 37:
                                // foreground color (0-7)
                                cellAttr = setCellAttrFG(cellAttr, parm[i] - 30)
                                break;

                            case 38:
                                // check for 5 ; color
                                if (++i < l)
                                    if (parm[i] == 5)
                                        if (++i < l) {
                                            parm[i] = minMax(parm[i], 0, 255, 7);
                                            cellAttr = setCellAttrFG(cellAttr, parm[i]);
                                        }
                                break;

                            case 39:
                                // default
                                cellAttr =
                                    setCellAttrFG(cellAttr, getCellAttrFG(defCellAttr));
                                break;

                            case 90: case 91: case 92: case 93:
                            case 94: case 95: case 96: case 97:
                                // foreground color (8-15)
                                cellAttr = setCellAttrFG(cellAttr, parm[i] - 90 + 8);
                                break;

                            // text background colors
                            case 40: case 41: case 42: case 43:
                            case 44: case 45: case 46: case 47:
                                // background color (0-7)
                                cellAttr = setCellAttrBG(cellAttr, parm[i] - 40)
                                break;

                            case 48:
                                // check for 5 ; color
                                if (++i < l)
                                    if (parm[i] == 5)
                                        if (++i < l) {
                                            parm[i] = minMax(parm[i], 0, 255, 7);
                                            cellAttr = setCellAttrBG(cellAttr, parm[i]);
                                        }
                                break;

                            case 49:
                                // default
                                cellAttr =
                                    setCellAttrBG(cellAttr, getCellAttrBG(defCellAttr));
                                break;

                            case 100: case 101: case 102: case 103:
                            case 104: case 105: case 106: case 107:
                                // background color (8-15)
                                cellAttr = setCellAttrBG(cellAttr, parm[i] - 100 + 8);
                                break;
                        }
                    }
                    break;

                case 0x6E:  // n DSR - device status report
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    if (parm[0] == 6) {
                        // request cursor position
                        sendData(CSI + (crsrRow+1) + ';' + (crsrCol+1) + 'R');
                    }
                    break;
                case 0x72:  // r
                    if (interm == '*') {
                        // *r - emulate baud
                        // assuming if p1 < 2 then use p2, else reset to full speed.
                        // ps1 : nil,0,1 = host transmit, 2=host recieve, 3=printer
                        //      4=modem hi, 5=modem lo
                        // ps2 : nil,0=full speed, 1=300, 2=600,3=1200,4=2400,5=4800,
                        //      6=9600,7=19200,8=38400,9=57600,10=76800,11=115200
                        parm = fixParams(parm, [ 0, 0 ]);
                        if (parm[0] < 2) {
                            conBaud = bauds[parm[1]] * 100;
                        }
                    } else if (interm == '') {
                        // CSI t ; b 'r' : Set scroll window (DECSTBM).
                        if (parm.length == 0) {
                            regionTopRow = 0;
                            regionBottomRow = crtRows - 1;
                        } else {
                            parm = fixParams(parm, [ 1, 1 ]);
                            parm[0] = minMax(parm[0], 1, crtRows);
                            parm[1] = minMax(parm[1], parm[0], crtRows);
                            regionTopRow = parm[0] - 1;
                            regionBottomRow = parm[1] - 1;
                        }
                    }
                    break;

                case 0x73:  // s - Save Position
                    crsrSaveRow = crsrRow;
                    crsrSaveCol = crsrCol;
                    break;

                case 0x75:  // u - Restore Position
                    crsrRow = crsrSaveRow;
                    crsrCol = crsrSaveCol;
                    crsrrender = true;
                    break;

                default:
                    // unsupported - ignore
                    //console.log('unsupported ansi : CSI ' + String.fromCharCode(chr));
                    break;
            }
        }
    }
    if (crsrrender)
        crsrDraw();
}

// animate move sprite to nr, nc over t time.
function moveSprite(el, nr, nc, t){
    var
        rpos = getElementPosition(getRowElement(nr)),
        csize = getRowFontSize(nr),
        cx = parseInt(el.style['left']),
        cy = parseInt(el.style['top']),
        steps, tmr, 
        sel,
        nx, ny;
        
    if (t < 0) t = 0;
    steps = Math.round(t / 15);
    nx = nc * csize.width;
    ny = rpos.top - pageTop;
  
    // generate new keyframe 
    sel = domElement('style',{},{},
        '@keyframes ' + el.id + ' { '
        + 'from { left: ' + cx + 'px; top: ' + cy + 'px; } ' 
        + 'to { left : ' + nx + 'px; top: ' + ny + 'px;}} ');
    el.appendChild(sel);
    el.style['animation-duration'] = t + 'ms';
    el.style['animation-name'] = el.id;
    el.style['animation-iteration-count'] = '1';
    el.style['animation-fill-mode'] = 'forwards';
    setTimeout(function(){
        el.style['left'] = nx + 'px';
        el.style['top'] = ny + 'px';
        el.style['animation-name'] = null;
        el.style['animation-duration'] = null;
        el.style['animation-iteration-count'] = null;
        el.style['animation-fill-mode'] = null;
        el.removeChild(sel);
    }, t + 5);
}

// remove all NL from string. (https://www.chromestatus.com/features/5735596811091968)
function stripNL(strin) {
    var strout = strin.replace(/\n/g,' ').replace(/\s+/g, ' ');
    return strout;
}

// decode hex3 string, return as Uint8Array
// remove whitespaces. Note: string data in Hex3 is UTF8
function decodeHex3(strin) {
    var
        ret,    // decoded result
        i, l,   // idx / length
        p;      // ptr into strin
        
    strin = strin.replace(/\s/g,'');
    ret = new Uint8Array(strin.length >> 1);
    for (p = 0, i = 0, l = ret.length; i < l; i++) {
        ret[i] = ((strin.charCodeAt(p    ) & 0x0F) << 4)
               | ((strin.charCodeAt(p + 1) & 0x0F));
        p += 2;
    }
    return ret;
}

// call once every 33 ms
function doWriteBuffer() {
    var
        strOut,
        bytes;

    if (conBuffer.length > 0) {
        // how many bytes to send since last call.
        if (conBaud == 0) {
            strOut = conBuffer;
            conBuffer = '';
        } else {
            bytes = conBaud / 300;
            if (conBuffer.length < bytes){
                strOut = conBuffer;
                conBuffer = '';
            } else {
                strOut = conBuffer.substring(0,bytes);
                conBuffer = conBuffer.substring(bytes);
            }
        }
        conStrOut(strOut);
    }
}

// do all writing here.
function conBufferOut(data) {
    conBuffer += data;
    if (conBaud == 0) {
        conStrOut(conBuffer);
        conBuffer = '';
    }
}

// write string using current attributes at cursor position. ###CALL conBufferOut!###
function conStrOut(str) {
    var
        oldSpeed,
        l, i;

    str = str || '';
    l = str.length;
    for (i = 0; i < l; i++) {
        oldSpeed = conBaud;
        conCharOut(str.charCodeAt(i));
        if (conBaud != oldSpeed) {
            // move rest back to buffer!
            conBuffer = str.substring(i+1) + conBuffer;
            break;
        }
    }
}

// perform telnet handshaking - return data with telnet commands removed.
// must be state machine.
// data is Uint8Array. return UInt8Array
function tnNegotiate(data) {
    var
        v0, v1, v2, v3,
        i, l, b, outdata, outp;
    
    outdata = new Uint8Array(data.length);  
    outp = 0;   // pointer into output
    l = data.length;
    for (i = 0; i < l; i++) {
        b = data[i];
        switch (tnState) {
            case 0:
                // not in any state. looking for IAC's
                if (b == TN_IAC)
                    tnState++
                else
                    outdata[outp++] = b;
                break;
                
            case 1:
                // recieved a TN_IAC
                switch (b) {
                    case TN_IAC:
                        // escaped 0xFF.
                        outdata[outp++] = b;
                        tnState = 0;
                        break;
                        
                    case TN_SE:    // subneg end
                    case TN_NOP:   // no operation
                    case TN_DM:    // data mark
                    case TN_BRK:   // break
                    case TN_IP:    // interrupt process
                    case TN_AO:    // abort output
                    case TN_AYT:   // are you there? try responding with NUL
                    case TN_EC:    // erase char
                    case TN_EL:    // erase line
                    case TN_GA:    // go ahead
                        // ignore for now.
                        tnState = 0;
                        break;
                        
                    case TN_SB:
                        // subneg begin
                        tnState = 3;
                        break;
                        
                    case TN_WILL:
                    case TN_WONT:
                    case TN_DO:
                    case TN_DONT:
                        tnCmd = b;
                        tnState = 2;
                        break;

                    default:
                        // ??
                        outdata[outp++] = b;
                        tnState = 0;
                        break;
                }
                break;
                
            case 2:
                // have IAC + cmd so far
                switch (tnCmd) {
                    case TN_WILL:
                        // server would like to do something. send DO or DONT
                        if (tnQUs[b] == TNQ_WANTYES)
                            tnQUs[b] = TNQ_YES
                        else {
                            switch (b) {
                                case TN_BIN:    // binary
                                case TN_SGA:    // suppress go ahead
                                case TN_ECHO:   // echo
                                case TN_NAWS:   // negotiate about window size
                                case TN_TTYPE:  // terminal type
                                case TN_TSPEED: // terminal speed
                                    tnSendCmd(TN_DO, b);
                                    tnQHim[b] = TNQ_YES;
                                    break;
                                
                                default:
                                    tnSendCmd(TN_DONT, b);
                                    tnQHim[b] = TNQ_NO;
                                    break;
                            }
                        }
                        break;
                   
                    case TN_WONT:
                        if ((tnQUs[b] == TNQ_WANTYES) || (tnQUs[b] == TNQ_WANTNO))
                            // response to my request to do or dont
                            tnQUs[b] = TNQ_NO
                        else {
                            // server wants to not do
                            tnSendCmd(TN_DONT, b);
                            tnQHim[b] = TNQ_NO;
                        }
                        break;                            
                    
                    case TN_DO:
                        if (tnQUs[b] == TNQ_WANTYES) {
                            // response to my request to will
                            tnQUs[b] = TNQ_YES;
                            
                            // send some SB stuff now
                            if (b == TN_NAWS) {
                                v0 = crtCols & 0xFF;
                                v1 = (crtCols >> 8) & 0xFF;
                                v2 = crtRows & 0xFF;
                                v3 = (crtRows >> 8) & 0xFF;
                                tnSendCmd(TN_SB, TN_NAWS, v1, v0, v3, v2);
                                tnSendCmd(TN_SE);
                            }
                        } else {
                            // server wants us to do
                            switch (b) {
                                case TN_BIN:
                                case TN_SGA:
                                case TN_ECHO:
                                case TN_NAWS:
                                case TN_TTYPE:
                                case TN_TSPEED:
                                    tnSendCmd(TN_WILL, b);
                                    tnQUs[b] = TNQ_YES;
                                    break;
                                    
                                default:
                                    tnSendCmd(TN_WONT, b);
                                    tnQUs[b] = TNQ_NO;
                                    break;
                            }
                        }
                        break;
                        
                    case TN_DONT:
                        // server wants me to not do something. send WONT
                        if (tnQUs[b] == TNQ_WANTYES)
                            // response to my request to will
                            tnQUs[b] = TNQ_NO
                        else {
                            // server wants us to not do. respond WONT
                            tnSendCmd(TN_WONT, b);
                            tnQUs[b] = TNQ_NO;
                        }
                        break;
                }
                tnState = 0;
                break;
            
            case 3:
                // have IAC SB
                tnCmd = b;
                tnState = 4;
                break;
            
            case 4:
                // this should be SEND (1)
                if (b == TN_SEND) 
                    switch (tnCmd) {
                        case TN_TTYPE:
                            // will neg term type;
                            tnSendCmd(TN_SB, tnCmd, TN_IS, vtxdata.term);
                            tnSendCmd(TN_SE);
                            break;
                            
                        case TN_TSPEED:
                            // will neg terminal speed
                            tnSendCmd(TN_SB, tnCmd, TN_IS, '921600,921600');
                            tnSendCmd(TN_SE);
                            break;
                            
                        case TN_NEWE:
                            // new environment
                            tnSendCmd(TN_SB, tnCmd, TN_IS, 0);
                            tnSendCmd(TN_SE);
                            break;
                            
                        default:
                            // ? why are we being asked this? server on drugs?
                            tnSendCmd(TN_DONT, tnCmd);
                            outdata[outp++] = b;
                            break;
                    }
                tnState = 0;
                break;
        }
    }
    // truncate outdata at outp.
    return outdata.slice(0, outp);
}

// send telnet commands.. send bytes or strings as parameters
function tnSendCmd() {
    var
        i, j, 
        l1, l2,
        outbuff,
        bytebuff = [];
    
    bytebuff.push(TN_IAC);
    l1 = arguments.length;
    for (i = 0; i < l1; i++) 
        if (typeof arguments[i] === 'string') {
            // put string into bytebuff
            l2 = arguments[i].length;
            for (j = 0; j < l2; j++)
                bytebuff.push(arguments[i].charCodeAt(j));
        } else if (typeof arguments[i] === 'number') 
            // put this byte into bytebuff
            bytebuff.push(arguments[i]);
    
    // send bytebuff
    l1 = bytebuff.length;
    outbuff = new Uint8Array(l1);
    for (i = 0; i < l1; i++)
        outbuff[i] = bytebuff[i];
    ws.send(outbuff);
}

// send initial barrage of settings.
function tnInit() {
    var
        i;
    
    tnQHim.fill(TNQ_NO);
    tnQUs.fill(TNQ_NO);
    
    // set initial telnet options
    tnQUs[TN_BIN] =     TNQ_WANTYES;
    tnQHim[TN_BIN] =    TNQ_WANTYES;
    tnQUs[TN_SGA] =     TNQ_WANTYES;
    tnQHim[TN_SGA] =    TNQ_WANTYES;
    tnQHim[TN_ECHO] =   TNQ_WANTYES;
    tnQUs[TN_NAWS] =    TNQ_WANTYES;
    tnQUs[TN_TTYPE] =   TNQ_WANTYES;
    tnQUs[TN_TSPEED] =  TNQ_WANTYES;
    
    for (i = 0; i < 256; i++) {
        if (tnQUs[i] == TNQ_WANTYES) 
            tnSendCmd(TN_WILL, i)
        else if (tnQUs[i] == TNQ_WANTNO)
            tnSendCmd(TN_WONT, i);
        
        if (tnQHim[i] == TNQ_WANTYES)
            tnSendCmd(TN_DO, i)
        else if (tnQHim[i] == TNQ_WANTNO)
            tnSendCmd(TN_DONT, i);
    }
}

addListener(window, 'load', bootVTX);

// insert an typed array into an typed array
function insertInto(thisArr, pos, insertArr) {
    if (thisArr.constructor === insertArr.constructor) {
        var tl = thisArr.length, 
            il = insertArr.length;
        if (il > 0) {
            var newArr = new thisArr.constructor(tl + il);
            if (pos > tl) pos = tl;
            if (pos < 0) pos = 0;
            newArr.set(thisArr.subarray(0, pos), 0);
            newArr.set(insertArr, pos);
            newArr.set(thisArr.subarray(pos), pos + il);
            return newArr;
        } else
            return thisArr;
    } else {
        throw new Error('TypedArray mismatch in insertInto().');
        die();
    }
}

// insert an typed array into an typed array
function deleteFrom(thisArr, start, end) {
    if (end > start) {
        var tl = thisArr.length, 
        end = end || tl;
        var len = end - start,
            newArr;
        if (end > tl) end = tl;
        newArr = new thisArr.constructor(tl - len);
        newArr.set(thisArr.subarray(0, start), 0);
        newArr.set(thisArr.subarray(end), start);
        return newArr;
    } else {
        throw new Error('End is before start in deleteFrom().');
        die();
    }
}

};
