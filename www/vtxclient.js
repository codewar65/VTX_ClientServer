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

    TODO :
        Client Ident

        codes for restore cursor attr, restore page attr

        finish PETSCII if term = 'PETSCII' / use c64Colors palette

CBM keys
    Left CTRL is the Commodore key
    ESC and Caps lock keys are the Run/stop key
    Tab key is the CTRL key
    ` (single quote) key is the Left arrow key
    \ (backslash) key is the Pound sign key
    Home key is the CLR HOME key
    Page up key is the RESTORE key
    Arrow keys represent the CRSR keys



    PAGEATTR

        bbbbbbbb BBBBBBBB

        b : border color
        B : background color


    ROWATTR - numbers stored in conRowAttr[num]

        00000000 00000000 00000000  - bits
        mwwzzzbb ssssssss ffffffff

        f : First Color (0-255)
        s : Second Color (0-255)
        b : Background Pattern
            00 - none
            01 - solid (first color)
            10 - horiz grad (first -> second color)
            11 - vert grad (first -> second color)
        z : overall size
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


    CELLATTRS - numbers stored in conCellAttr[row][col]

        00000000 00000000 00000000 00000000 - bits
        ZZZZfKcr gotdkuib BBBBBBBB FFFFFFFF

        F : Foreground Color (0-255) using aixterm palette
        B : Background Color (0-255)  -''-
        b : bold (0-1)
        i : italics (0-1)
        u : underline (0-1)
        k : blink slow (0-1)
        d : drop shadow (0-1)
        t : strikethrough (0-1)
        o : outlined (0-1)
        g : glow (0-1)
        r : reversed
        c : concealed
        K : blink fast
        f : faint
        Z : font number 0-9 (10=mosaic block, 11=separated block).
        - : unused


    CRSRATTRS

        00000000 00000000 00000000  - bits
        -------- -----ozz cccccccc

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

// globals
{//REGION Globals
var
    // ansi color lookup table (alteration. color 0=transparent, use 16 for true black`)
    ansiColors = [
        // VGA 0-15 - transparent will switch to #000000 when appropriate
        'transparent',  '#AA0000',      '#00AA00',      '#AA5500',
        '#0000AA',      '#AA00AA',      '#00AAAA',      '#AAAAAA',
        '#555555',      '#FF5555',      '#55FF55',      '#FFFF55',
        '#5555FF',      '#FF55FF',      '#55FFFF',      '#FFFFFF',
        // EXTENDER 16-231
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
    cbmColors,
    // commodore colors
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

    // strings that get transmogrified by the HTTP server.
    // only change these if you are not using the VTX HTTP server.
    codePage = vtxdata.codePage,    // '@CodePage@',
    wsConnect = vtxdata.wsConnect,  // 'ws://@InternetIP@:@WSPort@',
    crtCols = vtxdata.crtCols,      // @Columns@,        // columns side of row on crt.
    xScale = vtxdata.xScale,        // @XScale@,          // scale everything this much on x.
    term = vtxdata.term,            // '@Terminal@',        // ANSI or PETSCII
    cbm = (vtxdata.term == 'PETSCII'),
    initStr = vtxdata.initStr,      // '@Initialize@',   // terminal initialize

    ws = null,                  // websocket connection.

    irqWriteBuffer = null,      // print buffer (33ms)
    irqCheckResize = null,
    irqCursor = null,
    irqBlink = null,

    hex =   '0123456789ABCDEF',
    b64 =   'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=',
    fontName,                   // font used
    fontSize,                   // font size to use
    rowSize,                    // character size
    colSize,                    // cell width in pixels
    crtWidth,                   // crt width in pixels
    pageWidth,                  // with of html in pixels

    pageLeft,                   // left position of page div.
    pageTop,                    // top position of page div.

    elPage = document.getElementsByTagName('html')[0],
    crsr,                       // cursor element
    crsrRow,                    // cursor position
    crsrCol,
    crsrSaveRow = 0,            // saved position
    crsrSaveCol = 0,
    lastCrsrRow,
    lastCrsrCol,
    pageAttr,                   // current page attributes
    crsrAttr,                   // color of cursor (only fg used)
    crsrBlink,                  // cursor blink state
    crsrSkipTime,               // skip cursor draws on heavy character output
    cellAttr,                   // current active attributes
    cellBlinkSlow,              // text blink states
    cellBlinkFast,
    defPageAttr,
    defCrsrAttr,
    defCellAttr,                // default cell attributes.
    lastChar,                   // last printable character outputed.
    lastHotSpot = null,         // last mouseover hotspot

    termState,                  // TERMSTATE_...

    pageDiv = null,             // page contents div
    ctrlDiv = null,             // controls panel
    textDiv = null,             // text plane
    soundBell = null,           // bell sound
    textPos = null,             // ul x,y of textdiv

    // ansi parsing vars
    parms = '',                 // parameters for CSI
    interm = '',                // intermediate for CSI
    apcstr = '',                // string data for APC
    ansiState = 0,

    // mode switches
    modeVTXANSI = false,        // CSI ?50 h/l to switch out of old ANSI.SYS mode.
    modeBlinkBright = false,    // CSI ?33 h/l to switch blink for bright background.
    modeCursor = true,          // CSI ?25 h/l to turn cursor on / off.
    modeBoldFont = false,       // CSI ?31 h/l to use font 1 for bold.
    modeNoBold = false,         // CSI ?32 h/l to disallow bold.
    modeBlinkFont = false,      // CSI ?34 h/l to use font 2 for blink.
    modeNoBlink = false,        // CSI ?35 h/l to disallow blink.
    modeSpeed = 0,              // baud emulation speed.
    modeCBMShift = true,        // PETSCII shift enabled
    modeDOORWAY = false,        // DOORWAY mode
    modeAutoWrap = true,        // Autowrap Mode

    modeNextGlyph = false,      // if DOORWAY mode, print glyph associated with this byte!
    
    // display buffer.
    conBuffer = '',             // console output buffer. (use string for now).

    // Attrs are integer arrays, base 0 (i.e.: row 1 = index 0)
    conRowAttr  = [],           // row attributes array of number
    conCellAttr = [],           // character attributes array of array or number
    conText = [],               // raw text - array of string
    conHotSpots = [],           // clickable hotspots
    spriteDefs = [],            // sprite definitions

    // array 0..9
    conFont = [],               // the 10 fonts used for CSI 10-19 m
    conFontCP = [],             // associated code page for font.
    conFontNum = 0,             // current font being used.

    // attribute masks
    A_CELL_FG_NASK =        0x000000FF,
    A_CELL_BG_MASK =        0x0000FF00,
    A_CELL_FONT_MASK =      0xF0000000,

    A_ROW_COLOR1_MASK =     0x0000FF,
    A_ROW_COLOR2_MASK =     0x00FF00,
    A_ROW_PATTERN_MASK =    0x030000,
    A_ROW_SIZE_MASK =       0x1C0000,
    A_ROW_WIDTH_MASK =      0x600000,

    A_CRSR_COLOR_MASK =     0x0000FF,
    A_CRSR_STYLE_MASK =     0x000300,

    // attribute flags
    A_CELL_BOLD =           0x00010000,
    A_CELL_ITALICS =        0x00020000,
    A_CELL_UNDERLINE =      0x00040000,
    A_CELL_STRIKETHROUGH =  0x00080000,
    A_CELL_BLINKSLOW =      0x00100000,
    A_CELL_SHADOW =         0x00200000,
    A_CELL_OUTLINE =        0x00400000,
    A_CELL_GLOW =           0x00800000,
    A_CELL_REVERSE =        0x01000000,
    A_CELL_CONCEAL =        0x02000000,
    A_CELL_BLINKFAST =      0x04000000,
    A_CELL_FAINT =          0x08000000,

    A_ROW_NONE =            0x000000,
    A_ROW_SOLID =           0x010000,
    A_ROW_HORZ =            0x020000,
    A_ROW_VERT =            0x030000,
    A_ROW_MARQUEE =         0x800000,

    A_CRSR_NONE =           0x000000,
    A_CRSR_THIN =           0x000100,
    A_CRSR_THICK =          0x000200,
    A_CRSR_FULL =           0x000300,
    A_CRSR_ORIENTATION =    0x000400,

    // key commands
    DO_CAPLK =          -2,
    DO_NUMLK =          -3,
    DO_SCRLK =          -4,

    // terminal states
    TS_NORMAL =         0,  // normal terminal mode. no xfers.
    TS_YMR_START =      1,  // ymodem download started. sending G's.
    TS_YMR_GETPACKET =  2,  // ymodem download packet
    TS_YMS_START =      3,  // ymodem send header of file.
    TS_YMS_PUTPACKET =  4,  // ymodem send packet
    TS_YMS_PUTWAIT =    5,  // ymodem wait for C on send

    ovl = {},               // overlay dialog stuff for file transfers

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
            0x0000, 0x263A, 0x263B, 0x2665, 0x2666, 0x2663, 0x2660, 0x2022,
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
            0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0]),
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
            0x00A0, 0x2018, 0x2019, 0x00A3, 0x20AC, _NUL  , 0x00A6, 0x00A7,
            0x00A8, 0x00A9, _NUL  , 0x00AB, 0x00AC, _SHY  , _NUL  , 0x2015,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x0385, 0x0386, 0x0387,
            0x0388, 0x0389, 0x038A, 0x00BB, 0x038C, 0x00BD, 0x038E, 0x038F,
            0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
            0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F,
            0x03A0, 0x03A1, _NUL  , 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7,
            0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x03AC, 0x03AD, 0x03AE, 0x03AF,
            0x03B0, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7,
            0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF,
            0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7,
            0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, _NUL ]),
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
            0x038A, _NUL  , 0x038C, 0x00F4, 0x00F6, 0x038E, 0x00FB, 0x00F9,
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
            0x0124, 0x0125, _NUL  , 0x00BD, 0x0134, 0x015F, 0x00AB, 0x00BB,
            0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0,
            0x015E, 0x2563, 0x2551, 0x2557, 0x255D, 0x017B, 0x017C, 0x2510,
            0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x015C, 0x015D,
            0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
            _NUL  , _NUL  , 0x00CA, 0x00CB, 0x00C8, 0x0131, 0x00CD, 0x00CE,
            0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, _NUL  , 0x00CC, 0x2580,
            0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x0120, 0x0121, 0x00B5, 0x0126,
            0x0127, 0x00DA, 0x00DB, 0x00D9, 0x016C, 0x016D, 0x00B7, 0x00B4,
            _SHY  , _NUL  , 0x2113, 0x0149, 0x02D8, 0x00A7, 0x00F7, 0x00B8,
            0x00B0, 0x00A8, 0x02D9, _NUL  , 0x00B3, 0x00B2, 0x25A0, 0x00A0]),
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
            0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x00F5, 0x00D5, 0x00B5, _NUL  ,
            0x00D7, 0x00DA, 0x00DB, 0x00D9, 0x00EC, 0x00FF, 0x00AF, 0x00B4,
            _SHY  , 0x00B1, _NUL  , 0x00BE, 0x00B6, 0x00A7, 0x00F7, 0x00B8,
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
            _SHY  , 0x00B1, _NUL  , 0x0178, 0x00B6, 0x00A7, 0x00F7, 0x017E,
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
            _NUL  , _NUL  , _NUL  , _NUL  , _NUL  , _NUL  , 0x0386, _NUL  ,
            0x00B7, 0x00AC, 0x00A6, 0x2018, 0x2019, 0x0388, 0x2015, 0x0389,
            0x038A, 0x03AA, 0x038C, _NUL  , _NUL  , 0x038E, 0x03AB, 0x00A9,
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
            0x20AC, _NUL  , 0x201A, _NUL  , 0x201E, 0x2026, 0x2020, 0x2021,
            _NUL  , 0x2030, 0x0160, 0x2039, 0x015A, 0x0164, 0x017D, 0x0179,
            _NUL  , 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            _NUL  , 0x2122, 0x0161, 0x203A, 0x015B, 0x0165, 0x017E, 0x017A,
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
            _NUL  , 0x2122, 0x0459, 0x203A, 0x045A, 0x045C, 0x045B, 0x045F,
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
            0x20AC, _NUL  , 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021,
            _NUL  , 0x2030, _NUL  , 0x2039, _NUL  , _NUL  , _NUL  , _NUL  ,
            _NUL  , 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            _NUL  , 0x2122, _NUL  , 0x203A, _NUL  , _NUL  , _NUL  , _NUL  ,
            0x00A0, 0x0385, 0x0386, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
            0x00A8, 0x00A9, _NUL  , 0x00AB, 0x00AC, _SHY  , 0x00AE, 0x2015,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x0384, 0x00B5, 0x00B6, 0x00B7,
            0x0388, 0x0389, 0x038A, 0x00BB, 0x038C, 0x00BD, 0x038E, 0x038F,
            0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
            0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F,
            0x03A0, 0x03A1, _NUL  , 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7,
            0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x03AC, 0x03AD, 0x03AE, 0x03AF,
            0x03B0, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7,
            0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF,
            0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7,
            0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, _NUL ]),
        WIN1254: new Uint16Array([    // WIN1254
            0x20AC, _NUL  , 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021,
            0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, _NUL  , _NUL  , _NUL  ,
            _NUL  , 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, _NUL  , _NUL  , 0x0178,
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
            0x20AC, _NUL  , 0x201A, _NUL  , 0x201E, 0x2026, 0x2020, 0x2021,
            _NUL  , 0x2030, _NUL  , 0x2039, _NUL  , 0x00A8, 0x02C7, 0x00B8,
            _NUL  , 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
            _NUL  , 0x2122, _NUL  , 0x203A, _NUL  , 0x00AF, 0x02DB, _NUL  ,
            0x00A0, _NUL  , 0x00A2, 0x00A3, 0x00A4, _NUL  , 0x00A6, 0x00A7,
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
            0x00A0, 0x0126, 0x02D8, 0x00A3, 0x00A4, _NUL,   0x0124, 0x00A7,
            0x00A8, 0x0130, 0x015E, 0x011E, 0x0134, 0x2010, _NUL,   0x017B,
            0x00B0, 0x0127, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x0125, 0x00B7,
            0x00B8, 0x0131, 0x015F, 0x011F, 0x0135, 0x00BD, _NUL,   0x017C,
            0x00C0, 0x00C1, 0x00C2, _NUL,   0x00C4, 0x010A, 0x0108, 0x00C7,
            0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
            _NUL,   0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x0120, 0x00D6, 0x00D7,
            0x011C, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x016C, 0x015C, 0x00DF,
            0x00E0, 0x00E1, 0x00E2, _NUL,   0x00E4, 0x010B, 0x0109, 0x00E7,
            0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
            _NUL,   0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x0121, 0x00F6, 0x00F7,
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
            0x00A8, 0x00A9, 0x037A, 0x00AB, 0x00AC, 0x2010, _NUL,   0x2015,
            0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x0384, 0x0385, 0x0386, 0x00B7,
            0x0388, 0x0389, 0x038A, 0x00BB, 0x038C, 0x00BD, 0x038E, 0x038F,
            0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
            0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 0x039E, 0x039F,
            0x03A0, 0x03A1, _NUL,   0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7,
            0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x03AC, 0x03AD, 0x03AE, 0x03AF,
            0x03B0, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7,
            0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF,
            0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7,
            0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, _NUL]),
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
            0x0554, 0x0584, 0x0555, 0x0585, 0x0556, 0x0586, 0x055a, _NUL ]),
        HAIK8: new Uint16Array([    // HAIK8
            0x00a0, 0x058E, 0x0587, 0x0589, 0x0029, 0x0028, 0x00bb, 0x00a8,
            0x2014, 0x00b7, 0x0559, 0x055d, 0x002d, 0x055f, 0x2026, 0x055c,
            0x055b, 0x055e, 0x0531, 0x0561, 0x0532, 0x0562, 0x0533, 0x0563,
            0x0534, 0x0564, 0x0535, 0x0565, 0x0536, 0x0566, 0x0537, 0x0567,
            0x0538, 0x0568, 0x0539, 0x0569, 0x053a, 0x056a, 0x053b, 0x056b,
            0x053c, 0x056c, 0x053d, 0x056d, 0x053e, 0x056e, 0x053f, 0x056f,
            0x0540, 0x0570, 0x0541, 0x0571, 0x0542, 0x0572, 0x0543, 0x0573,
            0x0544, 0x0574, 0x0545, 0x0575, 0x0546, 0x0576, 0x0547, 0x0577,
            _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,
            _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,
            _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,
            _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,   _NUL,
            0x0548, 0x0578, 0x0549, 0x0579, 0x054a, 0x057a, 0x054b, 0x057b,
            0x054c, 0x057c, 0x054d, 0x057d, 0x054e, 0x057e, 0x054f, 0x057f,
            0x0550, 0x0580, 0x0551, 0x0581, 0x0552, 0x0582, 0x0553, 0x0583,
            0x0554, 0x0584, 0x0555, 0x0585, 0x0556, 0x0586, 0x055a, _NUL ]),
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
            0xE0F8, 0xE0F9, 0xE0FA, 0xE0FB, 0xE0FC, 0xE0FD, 0xE0FE, 0xE0FF])
    },

    // http://invisible-island.net/xterm/xterm-function-keys.html
    // http://ansi-bbs.org/ansi-bbs2/index.ssjs
    keyvals = {

        //  null,       let browser handle key
        //  < 0,        special action
        //  = 0,        attempt to block completely
        //  > 0,        send ascii
        //  string,     send string
        //  function,   execute

        //    NORMAL  SHIFT    CTRL        C+S     ALT     S+A     C+A    C+S+A
         0: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // windows - ie
         8: [ function (){
                return cbm?0x14:0x08;
            },          0,      0,          0,      0,      0,      0,      0 ], // backspace
         9: [ function (){ // toggle text / gfx modes
                if (cbm) { 
                    conFontNum ^= 0x1;
                    renderAll();
                    return 0;
                } else 
                    return x09;
            },          function(){ 
                            return modeDOORWAY?'\x00\x0F':0;
                        },      0,          0,      0,      0,      0,      0 ], // tab
        12: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // clear (numpad5 numlk off)
        13: [ CR,       function () {
                            return cbm?0x8d:0;
                        },      0,          0,      0,      0,      0,      0 ], // enter
        16: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // shift
        17: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // ctrl
        18: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // alt
        19: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // pause/break
        20: [ DO_CAPLK, 0,      0,          0,      0,      0,      0,      0 ], // caps lock
        27: [ function () { // run/stop
                return cbm?0x03:0x1B;
            },          0,      0,          0,      0,      0,      0,      0 ], // esc
        32: [ ' ',      '\xa0', 0,          0,      0,      0,      0,      0 ], // spacebar
        33: [ function(){
                return modeDOORWAY?'\x00\x49':CSI+'V';
            },          0,      function(){
                                    return modeDOORWAY?'\x00\x84':0;
                                },          0,      0,      0,      0,      0 ], // pgup
        34: [ function(){
                return modeDOORWAY?'\x00\x51':CSI+'U';
            },          0,      function(){ 
                                    return modeDOORWAY?'\x00\x76':0;
                                },          0,      0,      0,      0,      0 ], // pgdn
        35: [ function(){ // text
                return modeDOORWAY?'\x00\x4F':(cbm?0x0E:CSI+'K');
            },          function(){ // graphics
                            return cbm?0x8E:0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x75':0;
                                },          0,      0,      0,      0,      0 ], // end
        36: [ function () { // home
                return modeDOORWAY?'\x00\x47':(cbm?0x13:CSI+'H');
            },          function(){ // clr
                            return cbm?0x93:0;
                        },      function(){ 
                                    return modeDOORWAY?'/x00/x77':0;
                                },          0,      0,      0,      0,      0 ], // home
        37: [ function () {
                return modeDOORWAY?'\x00\x4B':(cbm?0x9D:CSI+'D');
            },          0,      function(){
                                    return modeDOORWAY?'\x00\x73':0;
                                },          0,      0,      0,      0,      0 ], // left
        38: [ function () {
                return modeDOORWAY?'\x00\x48':(cbm?0x91:CSI+'A');
            },          0,      0,          0,      0,      0,      0,      0 ], // up
        39: [ function () {
                return modeDOORWAY?'\x00\x4D':(cbm?0x1D:CSI+'C');
            },          0,      function(){
                                    return modeDOORWAY?'\x00\x74':0;
                                },          0,      0,      0,      0,      0 ], // right
        40: [ function () {
                return modeDOORWAY?'\x00\x50':(cbm?0x11:CSI+'B');
            },          0,      0,          0,      0,      0,      0,      0 ], // down
        45: [ function (){
                return modeDOORWAY?'\x00\x52':(cbm?0x94:CSI+'@');
            },          0,      0,          0,      0,      0,      0,      0 ], // insert
        46: [ function (){
                return cbm?0x14:0x7f;
            },          0,      0,          0,      0,      0,      null,   0 ], // delete
        48: [ '0',      ')',    function(){ // rev off
                                    return cbm?0x92:0;
                                },          0,      0,      0,      0,      0 ], // 0
        49: [ '1',      '!',    function(){ // black
                                    return cbm?0x90:0;
                                },          0,      function(){ // orange
                                                        return cbm?0x81:0;
                                                    },      0,      0,      0 ], // 1
        50: [ '2',      '@',    function(){ // white
                                    return cbm?0x05:0;
                                },          0,      function(){ // brown
                                                        return cbm?0x95:0;
                                                    },      0,      0,      0 ], // 2
        51: [ '3',      '#',    function(){ // red
                                    return cbm?0x1C:0;
                                },          0,      function(){ // lt red
                                                        return cbm?0x96:0;
                                                    },      0,      0,      0 ], // 3
        52: [ '4',      '$',    function(){ // cyan
                                    return cbm?0x9f:0;
                                },          0,      function(){ // dk gray
                                                        return cbm?0x97:0;
                                                    },      0,      0,      0 ], // 4
        53: [ '5',      '%',    function(){ // purple
                                    return cbm?0x9c:0;
                                },          0,      function(){ // gray
                                                        return cbm?0x98:0;
                                                    },      0,      0,      0 ], // 5
        54: [ '6',      '^',    function(){ // green
                                    return cbm?0x1e:0;
                                },          0,      function(){ // lt green
                                                        return cbm?0x99:0;
                                                    },      0,      0,      0 ], // 6
        55: [ '7',      '&',    function(){ // blue
                                    return cbm?0x1f:0;
                                },          0,      function(){ // lt blue
                                                        return cbm?0x9a:0;
                                                    },      0,      0,      0 ], // 7
        56: [ '8',      '*',    function(){ // yellow
                                    return cbm?0x9e:0;
                                },          0,      function(){ // lt gray
                                                        return cbm?0x9b:0;
                                                    },      0,      0,      0 ], // 8
        57: [ '9',      '(',    function(){ // rev on
                                    return cbm?0x12:0;
                                },          0,      0,      0,      0,      0 ], // 9
        59: [ ';',      ':',    0,          0,      0,      0,      0,      0 ], // ;: - firefox
        61: [ '=',      '+',    0,          0,      0,      0,      0,      0 ], // =+ - firefox
        65: [ 'a',      'A',    0x01,       0,      0,      0,      0,      0 ], // a
        66: [ 'b',      'B',    0x02,       0,      0,      0,      0,      0 ], // b
        67: [ 'c',      'C',    null,       0,      0,      0,      0,      0 ], // c - browser copy
        68: [ 'd',      'D',    0x04,       0,      0,      0,      0,      0 ], // d
        69: [ 'e',      'E',    0x05,       0,      0,      0,      0,      0 ], // e
        70: [ 'f',      'F',    0x06,       0,      0,      0,      0,      0 ], // f
        71: [ 'g',      'G',    0x07,       0,      0,      0,      0,      0 ], // g
        72: [ 'h',      'H',    0x08,       0,      0,      0,      0,      0 ], // h
        73: [ 'i',      'I',    0x09,       0,      0,      0,      0,      0 ], // i
        74: [ 'j',      'J',    0x0a,       0,      0,      0,      0,      0 ], // j
        75: [ 'k',      'K',    0x0b,       0,      0,      0,      0,      0 ], // k
        76: [ 'l',      'L',    0x0c,       0,      0,      0,      0,      0 ], // l
        77: [ 'm',      'M',    0x0d,       0,      0,      0,      0,      0 ], // m
        78: [ 'n',      'N',    null,       0,      0,      0,      0,      0 ], // n - browser new window
        79: [ 'o',      'O',    0x0f,       0,      0,      0,      0,      0 ], // o
        80: [ 'p',      'P',    0x10,       0,      0,      0,      0,      0 ], // p
        81: [ 'q',      'Q',    0x11,       0,      0,      0,      0,      0 ], // q
        82: [ 'r',      'R',    0x12,       0,      0,      0,      0,      0 ], // r
        83: [ 's',      'S',    0x13,       0,      0,      0,      0,      0 ], // s
        84: [ 't',      'T',    null,       0,      0,      0,      0,      0 ], // t - browser new tab
        85: [ 'u',      'U',    0x15,       0,      0,      0,      0,      0 ], // u
        86: [ 'v',      'V',    null,       0,      0,      0,      0,      0 ], // v - browser paste
        87: [ 'w',      'W',    null,       0,      0,      0,      0,      0 ], // w - browser close window
        88: [ 'x',      'X',    0x18,       0,      0,      0,      0,      0 ], // x
        89: [ 'y',      'Y',    0x19,       0,      0,      0,      0,      0 ], // y
        90: [ 'z',      'Z',    0x1a,       0,      0,      0,      0,      0 ], // z
        91: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // left win
        92: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // right win
        93: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // select
        96: [ '0',      0,      0,          0,      0,      0,      0,      0 ], // numpad0
        97: [ '1',      0,      0,          0,      0,      0,      0,      0 ], // numpad1
        98: [ '2',      0,      0,          0,      0,      0,      0,      0 ], // numpad2
        99: [ '3',      0,      0,          0,      0,      0,      0,      0 ], // numpad3
       100: [ '4',      0,      0,          0,      0,      0,      0,      0 ], // numpad4
       101: [ '5',      0,      0,          0,      0,      0,      0,      0 ], // numpad5
       102: [ '6',      0,      0,          0,      0,      0,      0,      0 ], // numpad6
       103: [ '7',      0,      0,          0,      0,      0,      0,      0 ], // numpad7
       104: [ '8',      0,      0,          0,      0,      0,      0,      0 ], // numpad8
       105: [ '9',      0,      0,          0,      0,      0,      0,      0 ], // numpad9
       106: [ '*',      0,      0,          0,      0,      0,      0,      0 ], // multiply
       107: [ '+',      0,      0,          0,      0,      0,      0,      0 ], // add (use for enter on VT modes)
       109: [ '-',      0,      0,          0,      0,      0,      0,      0 ], // subtract
       110: [ '.',      0,      0,          0,      0,      0,      0,      0 ], // decimal
       111: [ '/',      0,      0,          0,      0,      0,      0,      0 ], // divide
       112: [ function(){
                return modeDOORWAY?'\x00\x3B':(cbm?0x85:ESC+'OP');
            },          function(){
                            return modeDOORWAY?'\x00\x54':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x5E':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x68':0;
                                                    },      0,      0,      0 ], // f1
       113: [ function(){
                return modeDOORWAY?'\x00\x3C':(cbm?0x89:ESC+'OQ');
            },          function(){
                            return modeDOORWAY?'\x00\x55':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x5F':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x69':0;
                                                    },      0,      0,      0 ], // f2
       114: [ function(){
                return modeDOORWAY?'\x00\x3D':(cbm?0x86:ESC+'OR');
            },          function(){
                            return modeDOORWAY?'\x00\x56':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x60':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x6A':0;
                                                    },      0,      0,      0 ], // f3
       115: [ function(){
                return modeDOORWAY?'\x00\x3E':(cbm?0x8A:ESC+'OS');
            },          function(){
                            return modeDOORWAY?'\x00\x57':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x61':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x6B':0;
                                                    },      0,      0,      0 ], // f4
       116: [ function(){
                return modeDOORWAY?'\x00\x3F':(cbm?0x87:ESC+'Ot');
            },          null,   null,       0,      function(){
                                                        return modeDOORWAY?'\x00\x6C':0;
                                                    },      0,      0,      0 ], // f5 - browser refresh
       117: [ function(){
                return modeDOORWAY?'\x00\x40':(cbm?0x8B:CSI+'17~');
            },          function(){
                            return modeDOORWAY?'\x00\x59':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x63':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x6D':0;
                                                    },      0,      0,      0 ], // f6
       118: [ function(){
                return modeDOORWAY?'\x00\x41':(cbm?0x88:CSI+'18~');
            },          function(){
                            return modeDOORWAY?'\x00\x5A':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x64':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x6E':0;
                                                    },      0,      0,      0 ], // f7
       119: [ function(){
                return modeDOORWAY?'\x00\x42':(cbm?0x8C:CSI+'19~');
            },          function(){
                            return modeDOORWAY?'\x00\x5B':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x65':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x6F':0;
                                                    },      0,      0,      0 ], // f8
       120: [ function(){
                return modeDOORWAY?'\x00\x43':CSI+'20~';
            },          function(){
                            return modeDOORWAY?'\x00\x5C':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x66':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x70':0;
                                                    },      0,      0,      0 ], // f9
       121: [ function(){
                return modeDOORWAY?'\x00\x44':CSI+'21~';
            },          function(){
                            return modeDOORWAY?'\x00\x5D':0;
                        },      function(){
                                    return modeDOORWAY?'\x00\x67':0;
                                },          0,      function(){
                                                        return modeDOORWAY?'\x00\x71':0;
                                                    },      0,      0,      0 ], // f10
       122: [ CSI+'23~',0,      0,          0,      0,      0,      0,      0 ], // f11 - browser full screen
       123: [ CSI+'24~',0 ,     0,          0,      0,      0,      0,      0 ], // f12
       124: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F13
       125: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F14
       126: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F15 / Help
       127: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F16 / Do
       128: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F17
       129: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F18
       130: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F19
       131: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F20
       132: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F21
       133: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F22
       134: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F23
       135: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // gui F24
       144: [ DO_NUMLK, 0,      0,          0,      0,      0,      0,      0 ], // numlock
       145: [ DO_SCRLK, 0,      0,          0,      0,      0,      0,      0 ], // scrolllock
       173: [ '-',      '_',    0,          0,      0,      0,      0,      0 ], // -_ (firefox)
       186: [ ';',      ':',    0,          0,      0,      0,      0,      0 ], // ;:
       187: [ '=',      '+',    0,          0,      0,      0,      0,      0 ], // =+
       188: [ ',',      '<',    0,          0,      0,      0,      0,      0 ], // ,<
       189: [ '-',      '_',    0,          0,      0,      0,      0,      0 ], // -
       190: [ '.',      '>',    0,          0,      0,      0,      0,      0 ], // .
       191: [ '/',      '?',    0,          0,      0,      0,      0,      0 ], // /
       192: [ '`',      '~',    0,          0,      0,      0,      0,      0 ], // `
       219: [ '[',      '{',    0x1b,       0,      0,      0,      0,      0 ], // [
       220: [ '\\',     '|',    0x1c,       0,      0,      0,      0,      0 ], // '\'
       221: [ ']',      '}',    0x1d,       0,      0,      0,      0,      0 ], // ]
       222: [ '\'',     '"',    0,          0,      0,      0,      0,      0 ], // '
       255: [ 0,        0,      0,          0,      0,      0,      0,      0 ] // windows - chrome/opera
    },
    shiftState, ctrlState, altState,
    numState, capState, scrState;
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
        fontNames = [ 
            'UVGA16', 'MICROKNIGHT', 'MICROKNIGHTPLUS', 
            'MOSOUL', 'P0TNOODLE', 'TOPAZ', 'TOPAZPLUS',
            'VIC200', 'VIC201', 'C640', 'C641', 'C1280', 
            'C1281', 'ATARI' 
        ],
        i, l, str,
        el, hd,
        testDiv;
        
    // load fonts
    // inject @font-faces
    hd = document.getElementsByTagName('head')[0];
    str = '';
    for (i = 0, l = fontNames.length; i < l; i++) {
        str += '@font-face {\r\n '
            + '  font-family: "' + fontNames[i] + '"; '
            + '  src: url("' + fontNames[i] + '.woff") format("woff"); }\r\n';
    }
    el = domElement('style', { type: 'text/css' }, {}, str );
    hd.appendChild(el);
        
    // loop through until ALL fonts have been loaded.
    for (i = 0, l = fontNames.length; i < l; i++) {
        testDiv = domElement('div',
        {},{
            fontFamily:         fontNames[i] + ', AdobeBlank',
            fontSize:           '12px',
            color:              'red',
            backgroundColor:    'black',
            display:            'inline-block',
            border:             '0px',
            padding:            '0px',
            margin:             '0px' },
            'Test 1..2..3..');
        document.body.appendChild(testDiv);
        while (testDiv.clientWidth < 24);
        document.body.removeChild(testDiv);
    }
    // wait for the required data.
    while (!vtxdata);

    // format the TITLE tag - only if empty or missing.
    var t = document.getElementsByTagName('title')[0];
    if (!t)
        hd.appendChild(domElement('title',{},{},vtxdata.sysName));
    else {
        if (!t.innerText || (t.innerText == ''))
            t.innerText = vtxdata.sysName
    }
        
    
    // when all fonts loaded call initDisplay
    window.setTimeout(initDisplay, 500);
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

// add event listener
function addListener(obj, eventName, listener) {
    if(obj.addEventListener)
        obj.addEventListener(eventName, listener, false)
    else
        obj.attachEvent("on" + eventName, listener);
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
            size = getRowAttrSize(conRowAttr[i]) / 100;
            width = getRowAttrWidth(conRowAttr[i]) / 100;
            rh = Math.round(fontSize * size);
            if ((y >= dt) && (y < (dt + rh))) {
                // on this row. get col
                c = (x - textPos.left) / (xScale * colSize * size * width);
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

    if (termState != TS_NORMAL) return;

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
                    renderCell(hs.row+y, hs.col+x, hs.hilite);
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
        hs;

    // for now, just fix meta key states
    e = e || window.event;
    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;

    if (termState != TS_NORMAL) return;

    hs = getHotSpot(e);
    if (hs) {
        // clicked on hotspot.
        switch (hs.type) {
            case 0:
                sendData(hs.val);
                break;

            case 1:
                // url
                var win = window.open(hs.val, '_blank');
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

    if (termState != TS_NORMAL) return;

    stateIdx = (shiftState ? 1 : 0) + (ctrlState ? 2 : 0) + (altState ? 4 : 0);

    // translate for capslock
    if ((kc >= 65) && (kc <= 90) && (stateIdx < 2) && capState)
        stateIdx ^= 1;

    // reverse for PETSCII
    if (cbm) {
        if ((kc >= 65) && (kc <= 90))
            stateIdx ^= 1;
    }
    
    ka = keyvals[kc][stateIdx];
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
    i = conHotSpots.length;
    while (i--) {
        hs = conHotSpot[i];
        if (hs.row == rownum)
            conHotSpots.splice(i,1)
        else if (hs.row > rownum)
            conHotSpots.row--;
    }
}

// insert row into storage and element into html
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
    i = conHotSpots.length;
    while (i--) {
        hs = conHotSpot[i];
        if (hs.row >= rownum)
            conHotSpots.row++;
    }
    
    // remove excess
    var els, p;
    while (conRowAttr.length > vtxdata.crtHistory) {
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
}

// insert a character at position. also sets attr to def
function insChar(rownum, colnum, chr) {
    expandToRow(rownum);
    expandToCol(rownum, colnum);
    conText[rownum] = conText[rownum].splice(colnum, 0, String.fromCharCode(chr));
    conCellAttr[rownum].splice(colnum, 0, defCellAttr);
}

// create blank HTML row
function createNewRow() {
    return domElement('div', { className: 'vtx' });
}

// is this character a printable? (add )
function isprint(chr) {
    if (chr < 32) return false;                 // C0 controls
    if (chr > 126 && chr < 160) return false;   // C1 controls
    return true;
}

// compute number of visible cells on this row.
function colsOnRow(rownum) {
    var
        cols = crtCols,
        size = getRowAttrSize(conRowAttr[rownum]) / 100,
        width = getRowAttrWidth(conRowAttr[rownum]) / 100;

    cols *= 1 / size;
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
    crsr.style['top'] =     rpos.top + 'px';
    crsr.style['left'] =    (rpos.left + (xScale * crsrCol * csize.width)) + 'px';
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
    var x, y, d;
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
            cbmColors[cellAttr & 0xFF]
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
            rh = rowSize * getRowAttrSize(conRowAttr[r]) / 100;
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
        w, h, rattr, size;

    rattr = conRowAttr[rownum];
    size = getRowAttrSize(rattr);
    w = colSize * (size / 100) * (getRowAttrWidth(rattr) / 100);
    h = rowSize * (size / 100);
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
    var str = b64c[val & 0x3f];
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
    for (; i; str = hex.charAt(i & 0x0F) + str, i >>= 4);
    if (d > 0)
        while (str.length < d) str = '0' + str;
    return str;
}

// convert hex string to integer
function htoi(h) {
    var
        i, l,
        v;
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

    var v = (c1 & 0xFF)
        | ((c2 & 0xFF) << 8)
        | bp
        | size
        | width
        | (marquee ? A_ROW_MARQUEE : 0);

    var tmp = itoh(v, 6);
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
function setRowAttrSize(attr, size) {
    // round size to nearest valid 25%
    size = Math.round(size / 25) - 1;
    if (size < 0) size = 0;
    if (size > 7) size = 7;
    size <<= 18;
    return (attr & ~A_ROW_SIZE_MASK) | size;
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
function getRowAttrSize(attr) { return (((attr & A_ROW_SIZE_MASK) >> 18) + 1) * 25.0; }
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
    return (attr & ~A_CELL_FG_NASK) | (color & 0xFF);
}
function setCellAttrBG(attr, color) {
    return (attr & ~A_CELL_BG_MASK) | ((color & 0xFF) << 8);
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
function setCellAttrGlow(attr, glow) {
    return (attr & ~A_CELL_GLOW) | (glow ? A_CELL_GLOW : 0);
}
function setCellAttrReverse(attr, reverse) {
    return (attr & ~A_CELL_REVERSE) | (reverse ? A_CELL_REVERSE : 0);
}
function setCellAttrConceal(attr, conceal) {
    return (attr & ~A_CELL_CONCEAL) | (conceal ? A_CELL_CONCEAL : 0);
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
function getCellAttrGlow(attr) { return (attr & A_CELL_GLOW) != 0; }
function getCellAttrReverse(attr) { return (attr & A_CELL_REVERSE) != 0; }
function getCellAttrConceal(attr) { return (attr & A_CELL_CONCEAL) != 0; }
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
    size = getRowAttrSize(conRowAttr[rownum]) / 100;    // .25 - 2
    width = getRowAttrWidth(conRowAttr[rownum])/ 100;   // .5 - 2
    h = rowSize * size;             // height of char
    w = colSize * size * width;     // width of char

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

        // redraw this entire row
        redrawRow(rownum);
    }
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
    size = getRowAttrSize(conRowAttr[rownum]) / 100;    // .25 - 2
    width = getRowAttrWidth(conRowAttr[rownum])/ 100;   // .5 - 2
    w = xScale * colSize * size * width;     // width of char
    x = w * l;                  // left pos of char on canv
    cnv = row.firstChild;
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
function renderCell(rownum, colnum, forcerev) {
    var
        row, size, width, w, h, x, cnv, drawtxt,
        ctx, attr, ch, tfg, tbg, tbold, stroke, tmp,
        tblinks, tfnt;

    // quick range check
    if (rownum > conRowAttr.length)         return;
    if (colnum >= conText[rownum].length)   return;

    forcerev = forcerev || false;
    size = getRowAttrSize(conRowAttr[rownum]) / 100;    // .25 - 2
    width = getRowAttrWidth(conRowAttr[rownum])/ 100;   // .5 - 2
    w = xScale * colSize * size * width;     // width of char
    x = w * colnum;                 // left pos of char on canv

    // don't render off page unless marquee
    if ((x > w * crtCols) && !(conRowAttr[rownum] & A_ROW_MARQUEE))
        return;

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

    ch = conText[rownum].charAt(colnum);
    attr = conCellAttr[rownum][colnum];
    
    tfg = (attr & 0xFF);
    tbg = (attr >> 8) & 0xff;
    tfnt = ((attr & A_CELL_FONT_MASK) >> 28) & 0xF;

    tbold  = attr & A_CELL_BOLD;
    if (modeNoBold) // CSI ?32 h / l
        tbold = 0;
    
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
    
    if (attr & A_CELL_REVERSE) {
        tmp = tfg;
        tfg = tbg;
        tbg = tmp;
    }

    if (!modeVTXANSI) {
        if (tbold && (tfg < 8)) {
            tfg += 8;
        }
        tbold = false;
    }

    // force highlight (for mouse selections)
    if (forcerev) {
        tbg = 4;
        tfg = 15;
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

    // fix transparents
    if (tfg == 0) tfg = 16;
    if ((tbg == 0) && !modeVTXANSI) tbg = 16;

    // fix stupid ansi
    if (ch.charCodeAt(0) == 0x2588) {
        ch = ' ';
        tbg = tfg;
    }

    ctx.save();
    if (cbm) {
        ctx.fillStyle = cbmColors[tbg]
        ctx.fillRect(x, 0, w, h);
    } else {
        if (tbg > 0) {
            ctx.fillStyle = ansiColors[tbg];
            ctx.fillRect(x, 0, w, h);
        } else
            ctx.clearRect(x, 0, w, h);
    }

    drawtxt = true;
    if (attr & A_CELL_CONCEAL)  // don't draw this char if concealed.
        drawtxt = false;
    if (((attr & A_CELL_BLINKSLOW) && cellBlinkSlow)  // don't draw if in blink state
        || ((attr & A_CELL_BLINKFAST) && cellBlinkFast)) {
        if (!modeNoBlink)
            drawtxt = false;
    }

    if (drawtxt) {
        // not concealed or not in blink state
        if (attr & A_CELL_FAINT)
            ctx.fillStyle = brightenRGB(ansiColors[tfg], -0.33);
        else if (cbm)
            ctx.fillStyle = cbmColors[tfg]
        else
            ctx.fillStyle = ansiColors[tfg];

        // swap for special fonts.
        var teletext = -1;
        if ((tfnt == 10) || (tfnt == 11)) {
            if ((ch >= ' ') && (ch <= "_")) {
                teletext = tfnt - 10
            } else
                ctx.font = (tbold ? 'bold ' : '') + fontSize + 'px ' + conFont[0];
        } else {
            if (cbm) 
                // render all text using conFontNum
                ctx.font = (tbold ? 'bold ' : '') + fontSize + 'px ' + conFont[conFontNum]
            else
                ctx.font = (tbold ? 'bold ' : '') + fontSize + 'px ' + conFont[tfnt];
        }
        ctx.textAlign = 'start';
        ctx.textBaseline = 'top';

        if (attr & A_CELL_GLOW) {
            // how does this work on scaled? test
            ctx.shadowColor = brightenRGB(ansiColors[tfg], 0.25);
            ctx.shadowOffsetX = 0;
            ctx.shadowOffsetY = 0;
            ctx.shadowBlur = 7;
        } else if (attr & A_CELL_SHADOW) {
            // how does this work on scaled? test
            ctx.shadowColor = '#000000';
            ctx.shadowOffsetX = h / rowSize;
            ctx.shadowOffsetY = h / rowSize;
            ctx.shadowBlur = 0;
        } else {
            ctx.shadowBlur = 0;
        }

        // use less of a skew on italics due to character clipping
        var xskew = 0;
        var xadj = 0;
        if (attr & A_CELL_ITALICS) {
            xskew = -0.125;
            xadj  = 1;
        }
        ctx.setTransform(
            xScale * size * width,      // x scale
            0,                          // y skew
            xScale * xskew,             // x skew
            size,                       // y scale
            (x + xScale * xadj),        // x adj
            0);                         // y adj
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
function drawMosaicBlock(ctx, ch, w, h, separated) {
    var
        b = ch - 32,
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
        
        // remove excess
        var els, p;
        while (conRowAttr.length > vtxdata.crtHistory) {
            els = document.getElementsByClassName('vtx');
            p = els[0].parentNode;
            p.removeChild(els[0]);
            conRowAttr.splice(0, 1);
            conText.splice(0, 1);
            conCellAttr.splice(0, 1);
            crsrRow--;
        }
    }
}

// adjust a <div><img src='...svg'></div> element so svg fills dimensions of div
// this is an onload event for svg images inside a div
function fitSVGToDiv(e) {
    var
        nw, nh,
        pw, ph,
        p = this.parentNode;

    e = e || window.event;
    this.style['transform-origin'] = 'top left';
    pw = p.clientWidth; //parseInt(p.style['width']);
    nw = pw / this.getBoundingClientRect().width;
    ph = p.clientHeight; //parseInt(p.style['height']);
    nh = ph / this.getBoundingClientRect().height;
    this.style['transform'] = 'scale(' + nw + ',' + nh + ')';
    this.style['visibility'] = 'visible';
}

// set indicators
function setBulbs() {
    if (!ws) return;

    document.getElementById('osbulb').src = ((ws.readyState == 1) ? 'os1':'os0') + '.png';
    document.getElementById('clbulb').src = (capState ? 'cl1':'cl0') + '.png';
    document.getElementById('nlbulb').src = (numState ? 'nl1':'nl0') + '.png';
    document.getElementById('slbulb').src = (scrState ? 'sl1':'sl0') + '.png';

    if (termState == TS_NORMAL){
        document.getElementById('ulbtn').style['visibility'] = 'visible';
        document.getElementById('dlbtn').style['visibility'] = 'visible';
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
        o.style['background-color'] = cbmColors[c]
    else
        o.style['background-color'] = ansiColors[c];
}

// setup the crt and cursor
function initDisplay() {
    var
        i, o, p, pos,
        cssel,
        fsize,
        defattrs;

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
        if (!codePageAKAs[codePage]) {
            if ((codePage != 'UTF8') && (codePage != 'UTF16')) {
                //document.write('Invalid code page.');
                //return;
            }
        } else
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
    } else {
        conFontNum = 0;                 // current font being used.
        for (i = 0; i < 16; i++) {      // set default font selects.
            conFont[i] = fontName;
            conFontCP[i] = codePage;
        }
    }

    pageDiv.style['width'] = (crtWidth*xScale) + 'px';
    var pagePos = getElementPosition(pageDiv);
    pageLeft = pagePos.left;
    pageTop = pagePos.top;

    // build marquee CSS
    var style = document.createElement('style');
    style.type = 'text/css';
    var css = '.marquee { animation: marquee 12s linear infinite; } '
        + '@keyframes marquee { '
        + '0% { transform: translate( ' + (xScale * crtCols * colSize) + 'px, 0); } '
        + '100% { transform: translate(-100%, 0); }}';
    if (style.styleSheet)
        style.styleSheet.cssText = css
    else
        style.appendChild(document.createTextNode(css));
    var head = document.head || document.getElementsByTagName('head')[0];
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
    soundBell.src = 'bell.ogg';
    soundBell.type = 'audio/ogg';
    soundBell.volume = 1;
    soundBell.preload = 'auto';
    soundBell.load();

    // set page attributes
    p = pageDiv.parentNode;
    if (cbm) {
        p.style['background-color'] = cbmColors[(pageAttr >> 8) & 0xFF];
        pageDiv.style['background-color'] = cbmColors[pageAttr & 0xFF];
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
    termState = TS_NORMAL; // set for standard terminal mode, not in file xfer mode

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
            width:      24,
            height:     24,
            title:      'Online Status' }));

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
        {   src:        'ul.png',
            id:         'ulbtn',
            onclick:    ymSendStart,
            width:      24,
            height:     24,
            title:      'YModem Upload' },
        {   cursor:     'pointer'}));

    ctrlDiv.appendChild(domElement(
        'img',
        {   src:        'dl.png',
            id:         'dlbtn',
            onclick:    ymRecvStart,
            width:      24,
            height:     24,
            title:      'YModem Download' },
        {   cursor:     'pointer'}));

    pageDiv.appendChild(ctrlDiv);

    // test websocket connect
    ws = new WebSocket(wsConnect, ['telnet']);
    ws.binaryType = "arraybuffer";
    ws.onmessage = function(e) {
        // binary data in.
        var
            i, j, str, data;

        data = new Uint8Array(e.data);
        //dump(data,0,data.length);

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
        setBulbs();
    }
    ws.onclose = function() {
        modeSpeed = 0;
        conBufferOut(
            cbm
            ?'\r\rDISCONNECTED.\r'
            :'\r\n\r\n\x1b[#9\x1b[0;91mDisconnected.\r\n');
        document.body.style['cursor'] = 'default';
        setBulbs();
    }
    ws.onerror = function(error) {
        conBufferOut(
            cbm
            ?'\r\rERROR : ' + error.reason.toUpper() + '\r'
            :'\r\n\r\n\x1b[#9\x1b[0;91mError : ' + error.reason + '\r\n');
        setBulbs();
    }
    conBufferOut(initStr);
    
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

// light it up
addListener(window, 'load', bootVTX);
//addListener(document, 'DOMContentLoaded', bootVTX);

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

// YModem rigmarole
var
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
    ymSendStartTime,            // timer for when to abort
    CRC_POLY = 0x1021;

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
                                // count CPMEOFs on end (work on this)
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
                        var tmpblob = new Blob([ymFileData, tmp]);
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

    for (mag = 0; size >= 1000; size /= 1000, mag++);
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

// only convert Uint8Array to string
// FUNCTION NOT USED ANYMORE
function XYZtoUTF16(data) {
    var
        c, d, l, i, outstr, encstr;

    if (data instanceof Uint8Array) {
        l = data.length;
        outstr = '';
        if (codePage == 'UTF8') {

            encstr = String.fromCharCode.apply(null, data);
            outstr = decodeURIComponent(escape(encstr));

        } else if (codePage == 'UTF16') {
            outstr = String.fromCharCode.apply(data);

        } else {
            for (i = 0; i < l; i++) {
                d = data[i];

                // need to keep controls unless in DOORWAY
                if (d >= 32)
                    c = getUnicode(codePage, d)
                else
                    c = d;
                outstr += String.fromCharCode(c);
            }
        }
        return outstr;
    } else
        throw 'Invalid data type in toUTF16.';
}

// send data to remote (or echo local if not connected)
// only send arraybuffer data
// add convert from native to node's codepage.
function sendData(data) {
    var
        unsent, i, l, str;

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

    // check code page conversions.
    if (ws && (ws.readyState == 1)) {
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

// output character using current attribute at cursor position.
function conPrintChar(chr) {
    if (isprint(chr)) {
        crsrSkipTime = new Date().getTime();
        conPutChar(crsrRow, crsrCol, chr, cellAttr);
        crsrCol++;
        if (modeAutoWrap) {
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

// the big function - ansi sequence state machine. ###CALL conBufferOut!###
function conCharOut(chr) {
    var
        def,
        i, l,               // generic idx, length
        r, c, v,            // row, col idx
        crsrrender = false, // redraw cursor?
        doCSI = false,      // execute compiled CSI at end?
        doAPC = false,      // execute compiled APC at end?
        parm,
        els, div, img;      // for svg sprite creation

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
                //if (modeCBMShift) {
                    conFontNum = 1;
                    renderAll();
                //}
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
                //if (modeCBMShift) {
                    conFontNum = 0;
                    renderAll();
                //}
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
    } else {
        // ANSI ---------------------------------------------------------------
        // do all normal ctrls first
        
        if (modeNextGlyph){
            // print glyph AS IS
            conPrintChar(chr);
            crsrrender = true;
            modeNextGlyph = false;
        } else {
            switch (chr) {
                case 0:     // nul
                    if (modeDOORWAY)
                        modeNextGlyph = true;
                    break;
                
                case 7:     // bell
                    soundBell.pause();
                    soundBell.play();
                    break;

                case 8:     // backspace
                    if (crsrCol > 0) {
                        expandToRow(crsrRow);
                        expandToCol(crsrRow, crsrCol);
                        crsrCol--;
                        delChar(crsrRow, crsrCol);
                        crsrrender = true;
                    }
                    break;
    
                case 9:     // horz tab
                    crsrCol = ((crsrCol >> 3) + 1) << 3;
                    if (crsrCol > colsOnRow(crsrRow))
                        crsrCol = colsOnRow(crsrRow);
                    crsrrender = true;
                    break;
    
                case 10:    // linefeed
                    if (!modeVTXANSI)  // LF dont CR!  lol
                        crsrCol = 0;    // for BBS/ANSI.SYS mode
                    crsrRow++;
                    crsrrender = true;
                    break;
    
                case 13:    // carriage return
                    crsrCol = 0;
                    crsrrender = true;
                    break;
    
                case 127:   // delete
                    expandToRow(crsrRow);
                    expandToCol(crsrRow, crsrCol);
                    delChar(crsrRow, crsrCol);
                    redrawRow(crsrRow);
                    crsrrender = true;
                    break;
    
                default:
                    switch (ansiState) {
                        case 0:
                            // not in an sequence.
                            if (chr == 27)
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
                            else if (chr == 0x5F) {
                                // ESC _ - sprite def
                                apcstr = '';
                                ansiState = 4
                            }
                            else
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
                            if (chr == 0x30 || chr == 0x31) {
                                // marquee off/on
                                if (chr == 0x30) {
                                    conRowAttr[crsrRow] &= ~A_ROW_MARQUEE;
                                    getRowElement(crsrRow).firstChild.classList.remove('marquee')
                                } else {
                                    conRowAttr[crsrRow] |= A_ROW_MARQUEE;
                                    getRowElement(crsrRow).firstChild.classList.add('marquee');
                                }
                            } else if (chr == 0x39) {
                                // reset row.
                                conRowAttr[crsrRow] = defRowAttr;
                            } // else unrecognized
                            adjustRow(crsrRow);
                            ansiState = 0;
                            break;

                        case 4:
                            // start of sprite def APC (ESC _)
                            // read until ST (ESC \)
                            // '0' [ ; n [ ; base64 ]] ST
                            if ((chr >= 0x20) && (chr <= 0x7E))
                                apcstr += String.fromCharCode(chr)
                            else if (chr == 0x1B)
                                // advance to finish reading string terminator (ST)
                                ansiState = 6
                            else
                                // unrecognized - abort sequence
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

                        case 6:
                            // confirm ST on APC sprite def
                            if (chr == 0x5C) {
                                // valid ST - process
                                doAPC = true;
                            }
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
            for (i = 0; i < l; i++){
                v = parseInt(parm[i]);
                if (!isNaN(v))
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
                    if (crsrRow < 0)
                        crsrRow = 0;
                    crsrrender = true;
                    break;

                case 0x42:  // B - Cursor Down
                    parm = fixParams(parm, [1]);
                    parm[0] = minMax(parm[0], 1, 999);
                    crsrRow += parm[0];
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

                            //case  7: // haik8 codepage (use only with armscii8 screenmap)
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
                        }
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
                            conCellAttr[crsrRow].length = crsrCol;
                            conText[crsrRow] = conText[crsrRow].substring(0, crsrCol);
                            // clear EOS
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
                            for (c = 0; c <= crsrCol; c++)
                                conPutChar(crsrRow, c, 32, defCellAttr);
                            redrawRow(crsrRow);

                            // clear SOS
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
                            }
                            else {
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
                            // clear EOL first
                            conCellAttr[crsrRow].length = crsrCol;
                            conText[crsrRow] = conText[crsrRow].substring(0, crsrCol);
                            redrawRow(crsrRow);
                            break;

                        case 1:
                            // clear SOL first
                            for (c = 0; c <= crsrCol; c++)
                                conPutChar(crsrRow, c, 32, defCellAttr);
                            redrawRow(crsrRow);
                            break;

                        case 2:
                            // clear row.
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
                    conRowAttr[crsrRow] = setRowAttrSize(conRowAttr[crsrRow],
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
                                    var hs = {
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
                    var row = getRowElement(crsrRow);
                    var c1 = ansiColors[parm[0]];
                    var c2 = ansiColors[parm[1]];
                    switch (parm[2] << 16) {
                        case A_ROW_SOLID:
                            row.style['background'] = c1;
                            break;

                        case A_ROW_HORZ:
                            row.style['background'] = 'linear-gradient(to bottom,' + c1 + ',' + c2 + ')';
                            break;

                        case A_ROW_VERT:
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
                                var p = pageDiv.parentNode;
                                p.style['background-color'] = ansiColors[(pageAttr >> 8) & 0xFF];
                                break;

                            case 4:// page background color
                                i = (parm[1] & 0xFF);
                                pageAttr = setPageAttrBackground(pageAttr, i);
                                pageDiv.style['background-color'] = ansiColors[pageAttr & 0xFF];
                                break;
                        }
                    }
                    crsrrender = true;
                    break;

                case 0x5F:  // _ - Display/Hide Sprite | CSI '0'; s ; n ; w ; h ; z _
                    parm = fixParams(parm, [ 0, 1, 1, 1, 1, 0 ]);
                    parm[1] = minMax(parm[1], 1, 64, 1);    // sprint #
                    parm[2] = minMax(parm[2], 1, 64, 1);    // def #
                    parm[3] = minMax(parm[3], 1, 999, 1);   // w
                    parm[4] = minMax(parm[4], 1, 999, 1);   // h
                    parm[5] = minMax(parm[5], 0, 1, 0);     // z
                    if (parm[0] == 0) {
                        // '0' - sprite display / remove commands
                        if (l == 1) {
                            // remove all sprites
                            els = document.getElementsByClassName('sprite');
                            for (i = els.length - 1; i >= 0; i--)
                                els[i].parentNode.removeChild(els[i]);
                        } else if (l == 2) {
                            // remove one sprite
                            div = document.getElementById('sprite' + parm[1]);
                            if (div != null)
                                div.parentNode.removeChild(div);
                        } else {
                            // display a new sprite
                            // remove old one if it exists first
                            div = document.getElementById('sprite' + parm[1]);
                            if (div != null)
                                div.parentNode.removeChild(div);

                            var rpos = getElementPosition(getRowElement(crsrRow));
                            var csize = getRowFontSize(crsrRow);
                            var spriteTop = rpos.top;
                            var spriteLeft = rpos.left + (crsrCol * csize.width)

                            // make a new one.
                            div = domElement(
                                'div',
                                {   className:  'sprite',
                                    id :        'sprite' + parm[1] },
                                {   position:   'absolute',
                                    left:       spriteLeft + 'px',
                                    top:        spriteTop + 'px',
                                    width:      (colSize * parm[3]) + 'px',
                                    height:     (rowSize * parm[4]) * 'px',
                                    overflow:   'hidden'});

                            img = domElement(
                                'img',
                                {   onload: fitSVGToDiv,
                                    src:    spriteDefs[parm[2]] },
                                {   visibility: 'hidden',
                                    width: (colSize * parm[3]) + 'px',
                                    height: (rowSize * parm[4]) * 'px' });

                            div.appendChild(img);
                            if (parm[5] == 0)
                                pageDiv.insertBefore(div, textDiv)
                            else
                                textDiv.appendChild(div);
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
                    parm[0] = minMax(parm[0], 0, 999);
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
                    crsrRow = parm[0] - 1;
                    crsrCol = parm[1] - 1;
                    expandToRow(crsrRow);
                    crsrrender = true;
                    break;

                case 0x68:  // h - set mode
                case 0x6C:  // l - reset mode
                    parm[0] = parm[0].toString();
                    switch (parm[0]) {
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

                        case '255':
                            // 255 : DOORWAY mode
                            modeDOORWAY = (chr == 0x68);
                            break;
                    }
                    break;

                case 0x6D:  // m - Character Attr
                    if (l < 1) parm[0] = 0; // don't use fixparms. variable parameters.
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
                                cellAttr =
                                    setCellAttrConceal(cellAttr, (parm[i] < 20));
                                break;

                            case 9:     // strikethrough
                            case 29:
                                cellAttr =
                                    setCellAttrStrikethrough(cellAttr, (parm[i] < 20));
                                break;

                            case 10: case 11: case 12: case 13: case 14:
                            case 15: case 16: case 17: case 18: case 19:
                                cellAttr =
                                    setCellAttrFont(cellAttr, (parm[i] - 10));
                                break;

                            case 50:    // glow
                            case 70:
                                cellAttr =
                                    setCellAttrGlow(cellAttr, (parm[i] < 70));
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
                            modeSpeed = bauds[parm[1]] * 100;
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
console.log('unsupported ansi : CSI ' + String.fromCharCode(chr));
                    break;
            }
        } else if (doAPC) {
            // apcstr = string command - define sprites
            // string = 0;n;base64...
            parm = apcstr.split(';');
            if (parm.length > 0) {
                if (parm[0] == '0') {
                    switch (parm.length) {
                        case 1:
                            // clear all definitions
                            spriteDefs = [];
                            break;

                        case 2:
                            // clear a single sprite
                            spriteDefs[parm[1]] = null;
                            break;

                        default:
                            // define a sprite
                            def = '';
                            for (i = 2; i < parm.length; i++)
                                def += parm[i] + ';';
                            def = def.substring(0, def.length - 1);
                            spriteDefs[parm[1]] = def;
                            break;
                    } // else ignore
                }
            }
        }
    }
    if (crsrrender)
        crsrDraw();
}

// call once every 33 ms
function doWriteBuffer() {
    var
        strOut,
        bytes;

    if (conBuffer.length > 0) {
        // how many bytes to send since last call.
        if (modeSpeed == 0) {
            strOut = conBuffer;
            conBuffer = '';
        } else {
            bytes = modeSpeed / 300;
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
    if (modeSpeed == 0) {
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
        oldSpeed = modeSpeed;
        conCharOut(str.charCodeAt(i));
        if (modeSpeed != oldSpeed) {
            // move rest back to buffer!
            conBuffer = str.substring(i+1) + conBuffer;
            break;
        }
    }
}
