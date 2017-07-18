/*
    VTX - Mark2
    (c)2017 Dan Mecklenburg - codewar65


    TODO :
        Client Ident
        
        work on server.
        find scripting engine to use for modules. (javascript / lua)

    NOTES:  APC split on ';'  - SVG info has a ; so parts need to be 
        reconcatenated.

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
    ------cr gotdkuib BBBBBBBB FFFFFFFF

    F : Foreground Color (0-255) using aixterm palette
    B : Background Color (0-255)  -''-
    b : bold (0-1)
    i : italics (0-1)
    u : underline (0-1)
    k : blink (0-1)
    d : drop shadow (0-1)
    t : strikethrough (0-1)
    o : outlined (0-1)
    g : glow (0-1)
    r : reversed
    c : concealed
    - : unused

    

    CRSRATTRS - various globals

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



    ESC / CSI CODES

    ESC [ + (0-?)... (sp-/)... (@-~)

    undeclared sequences to use for odd ball stuff
        CSI ... ... [
        CSI ... ... \
        CSI ... ... ]
        CSI ... ... ^
        CSI ... ... _
        CSI ... ... j
        CSI ... ... k
        CSI ... ... {

    MODE SETS / RESETS
        'Real ANSI' on      CSI ?50 h
        BBS/ANSI.SYS        CSI ?50 l

    CURSOR
        Position            CSI r ; c H  or  CSI r ; c f
        Up                  CSI n A
        Down                CSI n B
        Forward             CSI n C
        Backward            CSI n D
        Save Position       CSI s
        Restore Position    CSI u
        Next Line           CSI n E
        Previous Line       CSI n F
        To Column           CSI n G

    CLEARING
        Display             CSI n J
                                n : 0=EOS,1=SOS,2=ALL
        Row                 CSI n K
                                n : 0=EOL,1=SOL,2=ALL

    SPRITES (ESC _ = APC - Application Program Command)
        Define Sprite       ESC _ '0' ; n ; text ST
                            n = sprite def number
                            txt = base64 svg file
        Clear Sprite        ESC _ '0' ; n ST
        Clear All Sprites   ESC _ '0' ST
        
        Display Sprite      CSI s ; n ; r ; c ; w ; h ; z _
                                s = sprite number
                                n = sprite def number
                                r = row
                                c = col
                                w = width (in 100% cell size)
                                h = height (in 100% cell size)
                                z = zpos, 0=under text, 1=above text
        Hide Sprite         CSI n _
        Hide All Sprites    CSI _

    CURSOR ATTRIBUTES
        Color               CSI '0' ; c ^
                                c = color (0-255) 7 is default
        Size                CSI '1' ; s ^
                                s : 0=none,1=thin,2=thick,3=full 2 is default
        Orientation         CSI '2' ; o ^ 0 is default
                                o : 0=horz, 1=vert

    PAGE ATTRIBUTES

    ROW ATTRIBUTES
        Row Size:           CSI s ; w [ (*** new)
                            s = size, 0=25%,1=50%,...7=200%; 3=default
                            w = width, 0=50%,1=100%,..3=200%; 1=default
        Marquee off :       ESC # 0 (*** new)
        Marquee on :        ESC # 1 (*** new)
        Row Background :        CSI c ; d ; s ] (*** new)
                            c = color1, (0-255) 0=default
                            d = color2, (0-255) 0=default
                            s = style, 0=none,1=solid color1,2=horz grad,3=vert grad; 0=def
        Reset Row :         ESC # 9   reset above to default (*** new)

    CHARACTER ATTRIBUTES
    Char Attributes :   CSI n ; ... m
                            0   Clear all attributes
                            1   Bold on
                            3   Italics on
                            4   Underline on
                            5   Slow blink on
                            7   Reverse video
                            8   Concealed on (text not displayed)
                            9   Strikethrough on
                            21  Bold off
                            23  Italics off
                            24  Underline off
                            25  Slow blink off
                            27  Normal video
                            28  Concealed off
                            29  Strikethrough off
                            30  Black foreground
                            31  Red foreground
                            32  Green foreground
                            33  Yellow foreground
                            34  Blue foreground
                            35  Magenta foreground
                            36  Cyan foreground
                            37  White foreground
                            38;5;n  Set foreground color to entry in special palette
                            39  Default foreground color
                            40  Black background
                            41  Red background
                            42  Green background
                            43  Yellow background
                            44  Blue background
                            45  Magenta background
                            46  Cyan background
                            47  White background
                            48;5;n  Set background color to entry in special palette
                            49  Default background color
                            50  Glow on (*** new)
                            56  Outline on (*** new)
                            57  Shadow on (*** new)
                            70  Glow off (*** new)
                            76  Outline off (*** new)
                            77  Shadow off (*** new)
                            90  Bold Black foreground
                            91  Bold Red foreground
                            92  Bold Green foreground
                            93  Bold Yellow foreground
                            94  Bold Blue foreground
                            95  Bold Magenta foreground
                            96  Bold Cyan foreground
                            97  Bold White foreground
                            100 Bold Black background
                            101 Bold Red background
                            102 Bold Green background
                            103 Bold Yellow background
                            104 Bold Blue background
                            105 Bold Magenta background
                            106 Bold Cyan background
                            107 Bold White background

*/

// ansi color lookup table (alteration. color 0=transparent, use 16 for true black`)
var
    clut = [
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

    ws = null,              // websocket connection.

    irqCheckResize,
    irqCursor,
    
    hex =   '0123456789ABCDEF',
    b64 =   'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=',
    fontName,               // font used
    rowSize,                // character size
    colSize,                // cell width in pixels
    crtWidth,               // crt width in pixels
    crtCols = 80,           // columns side of row on crt.
    pageWidth,              // with of html in pixels
    elPage = document.getElementsByTagName('html')[0],
    crsr,                   // cursor element
    crsrRow,                // cursor position
    crsrCol,
    crsrSaveRow = 0,        // saved position
    crsrSaveCol = 0,
    lastCrsrRow,
    lastCrsrCol,
    pageAttr,               // current page attributes
    crsrAttr,               // color of cursor (only fg used)
    crsrBlink,              // cursor blink state
    crsrSkipTime,           // skip cursor draws on heavy character output
    cellAttr,               // current active attributes
    cellBlink,              // text blink state
    defCellAttr,            // default cell attributes.
    lastChar,               // last printable character outputed.

    pageDiv = null,         // page contents div
    textDiv = null,         // text plane
    soundBell = null,       // bell sound
    
    // ansi parsing vars
    parms = '',             // parameters for CSI
    interm = '',            // intermediate for CSI
    apcstr = '',            // string data for APC
    ansiState = 0,

    // mode switches
    modeRealANSI = false,   // CSI ?50 h / CSI ?50 l to switch out of old ANSI.SYS mode

    // Attrs are integer arrays, base 0 (i.e.: row 1 = index 0)
    conRowAttr  = [],       // row attributes array of number
    conCellAttr = [],       // character attributes array of array or number
    conText = [],           // raw text - array of string

    spriteDefs = [],        // sprite definitions
    
    // attribute masks
    A_CELL_FG_NASK =        0x000000FF,
    A_CELL_BG_MASK =        0x0000FF00,

    A_ROW_COLOR1_MASK =     0x0000FF,
    A_ROW_COLOR2_MASK =     0x00FF00,
    A_ROW_PATTERN_MASK =    0x030000,
    A_ROW_SIZE_MASK =       0x1C0000,
    A_ROW_WIDTH_MASK =      0x600000,

    A_CRSR_COLOR_MASK =     0x0000FF,
    A_CRSR_STYLE_MASK =     0x000300,

    // attribute flags
    A_CELL_BOLD =           0x010000,
    A_CELL_ITALICS =        0x020000,
    A_CELL_UNDERLINE =      0x040000,
    A_CELL_STRIKETHROUGH =  0x080000,
    A_CELL_BLINK =          0x100000,
    A_CELL_SHADOW =         0x200000,
    A_CELL_OUTLINE =        0x400000,
    A_CELL_GLOW =           0x800000,
    A_CELL_REVERSE =        0x1000000,
    A_CELL_CONCEAL =        0x2000000,

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
    DO_POPUP =          -1,
    DO_CAPLK =          -2,
    DO_NUMLK =          -3,
    DO_SCRLK =          -4,

    // special char codes and sequences
    ESC =       '\x1B',
    CSI =       '\x1B[',
    CR =        '\x0D',
    LF =        '\x0A',
    CRLF =      '\x0D\x0A',

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
         8: [ '\x08',   0,      0,          0,      0,      0,      0,      0 ], // backspace
         9: [ '\x09',   0,      0,          0,      0,      0,      0,      0 ], // tab
        12: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // clear (numpad5 numlk off)
        13: [ CRLF,     0,      0,          0,      0,      0,      0,      0 ], // enter
        16: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // shift
        17: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // ctrl
        18: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // alt
        19: [ 0,        0,      0,          0,      0,      0,      0,      0 ], // pause/break
        20: [ DO_CAPLK, 0,      0,          0,      0,      0,      0,      0 ], // caps lock
        27: [ '\x1B',   0,      0,          0,      0,      0,      0,      0 ], // esc
        32: [ ' ',      '\xa0', 0,          0,      0,      0,      0,      0 ], // spacebar
        33: [ CSI+'V',  0,      0,          0,      0,      0,      0,      0 ], // pgup
        34: [ CSI+'U',  0,      0,          0,      0,      0,      0,      0 ], // pgdn
        35: [ CSI+'K',  0,      0,          0,      0,      0,      0,      0 ], // end
        36: [ CSI+'H',  0,      0,          0,      0,      0,      0,      0 ], // home
        37: [ CSI+'D',  0,      0,          0,      0,      0,      0,      0 ], // left
        38: [ CSI+'A',  0,      0,          0,      0,      0,      0,      0 ], // up
        39: [ CSI+'C',  0,      0,          0,      0,      0,      0,      0 ], // right
        40: [ CSI+'B',  0,      0,          0,      0,      0,      0,      0 ], // down
        45: [ CSI+'@',  0,      0,          0,      0,      0,      0,      0 ], // insert
        46: [ 0x7F,     0,      0,          0,      0,      0,      null,   0 ], // delete
        48: [ '0',      ')',    0,          0,      0,      0,      0,      0 ], // 0
        49: [ '1',      '!',    0,          0,      0,      0,      0,      0 ], // 1
        50: [ '2',      '@',    0,          0,      0,      0,      0,      0 ], // 2
        51: [ '3',      '#',    0,          0,      0,      0,      0,      0 ], // 3
        52: [ '4',      '$',    0,          0,      0,      0,      0,      0 ], // 4
        53: [ '5',      '%',    0,          0,      0,      0,      0,      0 ], // 5
        54: [ '6',      '^',    0,          0,      0,      0,      0,      0 ], // 6
        55: [ '7',      '&',    0,          0,      0,      0,      0,      0 ], // 7
        56: [ '8',      '*',    0,          0,      0,      0,      0,      0 ], // 8
        57: [ '9',      '(',    0,          0,      0,      0,      0,      0 ], // 9
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
       112: [ ESC+'OP', 0,      0,          0,      0,      0,      0,      0 ], // f1
       113: [ ESC+'OQ', 0,      0,          0,      0,      0,      0,      0 ], // f2
       114: [ ESC+'OR', 0,      0,          0,      0,      0,      0,      0 ], // f3
       115: [ ESC+'OS', 0,      0,          0,      0,      0,      0,      0 ], // f4
       116: [ ESC+'Ot', null,   null,       0,      0,      0,      0,      0 ], // f5 - browser refresh
       117: [ CSI+'17~',0,      0,          0,      0,      0,      0,      0 ], // f6
       118: [ CSI+'18~',0,      0,          0,      0,      0,      0,      0 ], // f7
       119: [ CSI+'19~',0,      0,          0,      0,      0,      0,      0 ], // f8
       120: [ CSI+'20~',0,      0,          0,      0,      0,      0,      0 ], // f9
       121: [ CSI+'21~',0,      0,          0,      0,      0,      0,      0 ], // f10
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

/**
 * JavaScript Client Detection
 * (C) viazenetti GmbH (Christian Ludwig)
 */
(function (window) {
    {
        var unknown = '-';

        // screen
        var screenSize = '';
        if (screen.width) {
            width = (screen.width) ? screen.width : '';
            height = (screen.height) ? screen.height : '';
            screenSize += '' + width + " x " + height;
        }

        // browser
        var nVer = navigator.appVersion;
        var nAgt = navigator.userAgent;
        var browser = navigator.appName;
        var version = '' + parseFloat(navigator.appVersion);
        var majorVersion = parseInt(navigator.appVersion, 10);
        var nameOffset, verOffset, ix;

        // Opera
        if ((verOffset = nAgt.indexOf('Opera')) != -1) {
            browser = 'Opera';
            version = nAgt.substring(verOffset + 6);
            if ((verOffset = nAgt.indexOf('Version')) != -1) {
                version = nAgt.substring(verOffset + 8);
            }
        }
        // Opera Next
        if ((verOffset = nAgt.indexOf('OPR')) != -1) {
            browser = 'Opera';
            version = nAgt.substring(verOffset + 4);
        }
        // Edge
        else if ((verOffset = nAgt.indexOf('Edge')) != -1) {
            browser = 'Microsoft Edge';
            version = nAgt.substring(verOffset + 5);
        }
        // MSIE
        else if ((verOffset = nAgt.indexOf('MSIE')) != -1) {
            browser = 'Microsoft Internet Explorer';
            version = nAgt.substring(verOffset + 5);
        }
        // Chrome
        else if ((verOffset = nAgt.indexOf('Chrome')) != -1) {
            browser = 'Chrome';
            version = nAgt.substring(verOffset + 7);
        }
        // Safari
        else if ((verOffset = nAgt.indexOf('Safari')) != -1) {
            browser = 'Safari';
            version = nAgt.substring(verOffset + 7);
            if ((verOffset = nAgt.indexOf('Version')) != -1) {
                version = nAgt.substring(verOffset + 8);
            }
        }
        // Firefox
        else if ((verOffset = nAgt.indexOf('Firefox')) != -1) {
            browser = 'Firefox';
            version = nAgt.substring(verOffset + 8);
        }
        // MSIE 11+
        else if (nAgt.indexOf('Trident/') != -1) {
            browser = 'Microsoft Internet Explorer';
            version = nAgt.substring(nAgt.indexOf('rv:') + 3);
        }
        // Other browsers
        else if ((nameOffset = nAgt.lastIndexOf(' ') + 1) < (verOffset = nAgt.lastIndexOf('/'))) {
            browser = nAgt.substring(nameOffset, verOffset);
            version = nAgt.substring(verOffset + 1);
            if (browser.toLowerCase() == browser.toUpperCase()) {
                browser = navigator.appName;
            }
        }
        // trim the version string
        if ((ix = version.indexOf(';')) != -1) version = version.substring(0, ix);
        if ((ix = version.indexOf(' ')) != -1) version = version.substring(0, ix);
        if ((ix = version.indexOf(')')) != -1) version = version.substring(0, ix);

        majorVersion = parseInt('' + version, 10);
        if (isNaN(majorVersion)) {
            version = '' + parseFloat(navigator.appVersion);
            majorVersion = parseInt(navigator.appVersion, 10);
        }

        // mobile version
        var mobile = /Mobile|mini|Fennec|Android|iP(ad|od|hone)/.test(nVer);

        // cookie
        var cookieEnabled = (navigator.cookieEnabled) ? true : false;

        if (typeof navigator.cookieEnabled == 'undefined' && !cookieEnabled) {
            document.cookie = 'testcookie';
            cookieEnabled = (document.cookie.indexOf('testcookie') != -1) ? true : false;
        }

        // system
        var os = unknown;
        var clientStrings = [
            {s:'Windows 10', r:/(Windows 10.0|Windows NT 10.0)/},
            {s:'Windows 8.1', r:/(Windows 8.1|Windows NT 6.3)/},
            {s:'Windows 8', r:/(Windows 8|Windows NT 6.2)/},
            {s:'Windows 7', r:/(Windows 7|Windows NT 6.1)/},
            {s:'Windows Vista', r:/Windows NT 6.0/},
            {s:'Windows Server 2003', r:/Windows NT 5.2/},
            {s:'Windows XP', r:/(Windows NT 5.1|Windows XP)/},
            {s:'Windows 2000', r:/(Windows NT 5.0|Windows 2000)/},
            {s:'Windows ME', r:/(Win 9x 4.90|Windows ME)/},
            {s:'Windows 98', r:/(Windows 98|Win98)/},
            {s:'Windows 95', r:/(Windows 95|Win95|Windows_95)/},
            {s:'Windows NT 4.0', r:/(Windows NT 4.0|WinNT4.0|WinNT|Windows NT)/},
            {s:'Windows CE', r:/Windows CE/},
            {s:'Windows 3.11', r:/Win16/},
            {s:'Android', r:/Android/},
            {s:'Open BSD', r:/OpenBSD/},
            {s:'Sun OS', r:/SunOS/},
            {s:'Linux', r:/(Linux|X11)/},
            {s:'iOS', r:/(iPhone|iPad|iPod)/},
            {s:'Mac OS X', r:/Mac OS X/},
            {s:'Mac OS', r:/(MacPPC|MacIntel|Mac_PowerPC|Macintosh)/},
            {s:'QNX', r:/QNX/},
            {s:'UNIX', r:/UNIX/},
            {s:'BeOS', r:/BeOS/},
            {s:'OS/2', r:/OS\/2/},
            {s:'Search Bot', r:/(nuhk|Googlebot|Yammybot|Openbot|Slurp|MSNBot|Ask Jeeves\/Teoma|ia_archiver)/}
        ];
        for (var id in clientStrings) {
            var cs = clientStrings[id];
            if (cs.r.test(nAgt)) {
                os = cs.s;
                break;
            }
        }

        var osVersion = unknown;

        if (/Windows/.test(os)) {
            osVersion = /Windows (.*)/.exec(os)[1];
            os = 'Windows';
        }

        switch (os) {
            case 'Mac OS X':
                osVersion = /Mac OS X (10[\.\_\d]+)/.exec(nAgt)[1];
                break;

            case 'Android':
                osVersion = /Android ([\.\_\d]+)/.exec(nAgt)[1];
                break;

            case 'iOS':
                osVersion = /OS (\d+)_(\d+)_?(\d+)?/.exec(nVer);
                osVersion = osVersion[1] + '.' + osVersion[2] + '.' + (osVersion[3] | 0);
                break;
        }

        // flash (you'll need to include swfobject)
        /* script src="//ajax.googleapis.com/ajax/libs/swfobject/2.2/swfobject.js" */
        var flashVersion = 'no check';
        if (typeof swfobject != 'undefined') {
            var fv = swfobject.getFlashPlayerVersion();
            if (fv.major > 0) {
                flashVersion = fv.major + '.' + fv.minor + ' r' + fv.release;
            }
            else  {
                flashVersion = unknown;
            }
        }
    }

    window.jscd = {
        screen: screenSize,
        browser: browser,
        browserVersion: version,
        browserMajorVersion: majorVersion,
        mobile: mobile,
        os: os,
        osVersion: osVersion,
        cookies: cookieEnabled,
        flashVersion: flashVersion
    };
}(this));
    
function isMicrosoft() {
    var 
        uA = window.navigator.userAgent,
        onlyIEorEdge = /msie\s|trident\/|edge\//i.test(uA) 
            && !!( document.uniqueID || window.MSInputMethodContext),
        checkVersion = (onlyIEorEdge 
            && +(/(edge\/|rv:|msie\s)([\d.]+)/i.exec(uA)[2])) || NaN;

    return !isNaN(checkVersion);
}

if (jscd.browser.split(' ')[0] == 'Microsoft') {
    document.write('VTX is not compatible with Microsoft browsers.<br>');
    document.write('Please use a Mozilla or Chromium browser.');
    exit;
}

// add event listener
function addListener(obj, eventName, listener) {
    if(obj.addEventListener)
        obj.addEventListener(eventName, listener, false)
    else
        obj.attachEvent("on" + eventName, listener);
}

function mouseAction(e) {
    // for now, just fix meta key states
    e = e || window.event;
    shiftState = e.shiftKey;
    ctrlState = e.ctrlKey;
    altState = e.altKey;
    //setBulbs();
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
    //setBulbs();
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
    //setBulbs();

    stateIdx = (shiftState ? 1 : 0) + (ctrlState ? 2 : 0) + (altState ? 4 : 0);

    // translate for capslock
    if ((kc >= 65) && (kc <= 90) && (stateIdx < 2) && capState)
        stateIdx ^= 1;

    ka = keyvals[kc][stateIdx];
    if (ka == null) {
        // let browser handle it.
        return (e.returnValue = true);

    } else if (typeof ka == 'function') {
        ka();
    } else if (typeof ka == 'string') {
        // send string to console.
        if (ws.readyState == 1) 
            ws.send(ka)
        else
            conStrOut(ka);

        e.keyCode = 0;
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
            if (ws.readyState == 1) 
                ws.send(ka)
            else
                conCharOut(ka);
            
            e.keyCode = 0;
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
    //setBulbs();
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
    var
        row = document.createElement('div');
    row.setAttribute('class', 'vtx');
    return row;
}

// is this character a printable? (add )
function isprint(chr) {
    if (chr < 32) return false;                 // C0 controls
    if (chr > 126 && chr < 160) return false;   // C1 controls
    if (chr == 0xAD) return false;              // soft hyphen (173)
    return true;
}

// compute number of visible cells on this row.
function colsOnRow(rownum) {
    var
        cols = crtCols,
        size = getRowAttrSize(conRowAttr[rownum]) / 100;
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
        if (crsrCol == colsOnRow(crsrRow)) {
            crsrCol = 0;
            crsrRow++
        }
        lastChar = chr;
    }
}

// the big function - ansi sequence state machine.
function conCharOut(chr) {
    var
        i, l,               // generic idx, length
        r, c,               // row, col idx
        crsrrender = false, // redraw cursor?
        doCSI = false,      // execute compiled CSI at end?
        doAPC = false,      // execute compiled APC at end?
        parm,
        div, img;           // for svg sprite creation

    // do all normal ctrls first
    switch (chr) {
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
            if (!modeRealANSI)  // LF dont CR!  lol
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
            crsrrender = true;
            break;

        default:
            switch (ansiState) {
                case 0:
                    // not in an sequence.
                    if (chr == 27)
                        ansiState = 1
                    else {
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
                        conRowAttr[crsrRow] = setRowAttrMarquee(conRowAttr[crsrRow], (chr - 0x30));
                    } else if (chr == 0x39) {
                        // reset row.
                        conRowAttr[crsrRow] = defRowAttr;
                    } // else unrecognized
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
                crsrrender = true;
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

            case 0x44:  // D - Cursor Backward
                parm = fixParams(parm, [1]);
                parm[0] = minMax(parm[0], 1, 999);
                crsrCol -= parm[0];
                if (crsrCol < 0)
                    crsrCol = 0;
                crsrrender = true;
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
                        // clear SOS
                        for (r = 0; r < crsrRow; r++) {
                            conRowAttr[r] = defRowAttr;
                            conCellAttr[r] = [];
                            conText[r] = '';
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
                        if (!modeRealANSI) {
                            crsrRow = crsrCol = 0   // BBS / ANSI.SYS
                            crsrrender = true;
                        }
                        else {
                            expandToRow(crsrRow);   // ECMA-048 complient
                            expandToCol(crsrRow, crsrCol);
                        }
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
                        break;

                    case 1:
                        // clear SOL first
                        for (c = 0; c <= crsrCol; c++)
                            conPutChar(crsrRow, c, 32, defCellAttr);
                        break;

                    case 2:
                        // clear row.
                        conText[crsrRow] = '';
                        conCellAttr[crsrRow] = [];
                        break;
                }
                break;

            case 0x4C:  // L - EL - insert lines
                parm = fixParams(parm, [1]);
                parm[0] = minMax(parm[0], 1, 999);
                for (i = 0; i < parm[0]; i++)
                    insRow(crsrRow);
                crsrrender = true;
                break;
                
            case 0x4D:  // M - DL - delete lines
                parm = fixParams(parm, [1]);
                parm[0] = minMax(parm[0], 1, 999);
                for (i = 0; i < parm[0]; i++)
                    delRow(crsrRow);
                crsrrender = true;
                break;
                
            case 0x50:  // P - DCH - delete character
                parm = fixParams(parm, [1]);
                parm[0] = minMax(parm[0], 1, 999);
                for (i = 0; i < parm[0]; i++)
                    delChar(crsrRow, crsrCol);
                break;
                
            case 0x58:  // X - ECH - erase n characters
                parm = fixParams(parm, [1]);
                parm[0] = minMax(parm[0], 1, 999);
                for (i = 0; i < parm[0]; i++) {
                    conPutChar(crsrRow, crsrCol + i, 0x32, defCellAttr);
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

                        // make a new one.
                        div = document.createElement('div');
                        img = document.createElement('img');
                        div.className = 'sprite';
                        div.id = 'sprite' + parm[1];    // sprite number

                        var rpos = getElementPosition(getRowElement(crsrRow));
                        var csize = getRowFontSize(crsrRow);
                        var spriteTop = rpos.top;
                        var spriteLeft = rpos.left + (crsrCol * csize.width)

                        div.style['position'] = 'absolute';
                        div.style['left'] = spriteLeft + 'px';
                        div.style['top'] = spriteTop + 'px';
                        div.style['width'] = (colSize * parm[3]) + 'px';
                        div.style['height'] = (rowSize * parm[4]) * 'px';
                        div.style['overflow'] = 'hidden';

                        img.onload = fitSVGToDiv;
                        img.style['visibility'] = 'hidden';
                        img.style['width'] = (colSize * parm[3]) + 'px';
                        img.style['height'] = (rowSize * parm[4]) * 'px';

                        img['src'] = spriteDefs[parm[2]];
                        div.appendChild(img);
                        if (parm[5] == 0)
                            pageDiv.insertBefore(div, textDiv)
                        else
                            textDiv.appendChild(div);
                    }
                }
                break;
                
            case 0x5E:  // ^ - Cursor / Page Modes
                if (parm.length == 0){
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
                            p = pageDiv.parentNode;
                            p.style['background-color'] = clut[(pageAttr >> 8) & 0xFF];
                            break;
                            
                        case 4:// page background color
                            i = (parm[1] & 0xFF);
                            pageAttr = setPageAttrBackground(pageAttr, i);
                            pageDiv.style['background-color'] = clut[pageAttr & 0xFF];
                            break;
                    }
                }
                crsrrender = true;
                break;

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
                var c1 = clut[parm[0]];
                var c2 = clut[parm[1]];
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
                    ws.send(CSI + '?50;86;84;88c'); // reply for VTX
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
                    case '?50':
                        modeRealANSI = (chr == 0x68);
                        break;
                    // else ignore
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
                                setCellAttrBold(cellAttr, (parm[i] == 1));
                            break;

                        case 3:     // italics on/off
                        case 23:
                            cellAttr =
                                setCellAttrItalics(cellAttr, (parm[i] == 3));
                            break;

                        case 4:     // underline
                        case 24:
                            cellAttr =
                                setCellAttrUnderline(cellAttr, (parm[i] == 4));
                            break;

                        case 5:     // blink
                        case 6:     // blink
                        case 25:
                        case 26:
                            cellAttr =
                                setCellAttrBlink(cellAttr, (parm[i] < 20));
                            break;

                        case 7:     // reverse video
                        case 27:
                            cellAttr =
                                setCellAttrReverse(cellAttr, (parm[i] == 7));
                            break;

                        case 8:     // conceal
                        case 28:
                            cellAttr =
                                setCellAttrConceal(cellAttr, (parm[i] == 8));
                            break;

                        case 9:     // strikethrough
                        case 29:
                            cellAttr =
                                setCellAttrStrikethrough(cellAttr, (parm[i] == 9));
                            break;

                        case 50:    // glow
                        case 70:
                            cellAttr =
                                setCellAttrGlow(cellAttr, (parm[i] == 50));
                            break;

                        case 56:    // outline
                        case 76:
                            cellAttr =
                                setCellAttrOutline(cellAttr, (parm[i] == 56));
                            break;

                        case 57:    // shadow
                        case 77:
                            cellAttr =
                                setCellAttrShadow(cellAttr, (parm[i] == 57));
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
                    ws.Send(CSI + crsrRow + ';' + crsrCol + 'R');
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
    if (crsrrender)
        crsrDraw();
}

// write string using current attributes at cursor position
function conStrOut(str) {
    var
        l = str.length,
        i;

    setTimers(false);
    for (i = 0; i < l; i++)
        conCharOut(str.charCodeAt(i));
    setTimers(true);
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
    
    if ((new Date().getTime() > crsrSkipTime + 5) && !force)
        return;

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
    crsr.style['left'] =    (rpos.left + (crsrCol * csize.width)) + 'px';
    crsr.style['width'] =   csize.width + 'px';
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
        w, cs, fsz, f;
    
    cs = document.defaultView.getComputedStyle(document.body, null);
    fsz = parseInt(cs['font-size']);
    fontName = cs['font-family'];
    f = '1000px ' + fontName;
    fw = pageDiv.getAttribute('fontwidth');
    if (fw != null) {
        w = parseFloat(fw);
    } else {    
        w  = (getTextWidth(b64, f) / b64.length);
        w = (w / 1000) * fsz;
    }
    colSize = Math.round(w);
    rowSize = fsz;
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
    return metrics.width;
}

// get maximum row on document.
function getMaxRow() {
    return conRowAttr.length - 1;
    //return document.getElementsByClassName('vtx').length - 1;
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
    if (elPage.clientWidth != pageWidth) {
        pageWidth = elPage.clientWidth;
        crsrDraw(true);
    }
}

// blink cursor (533ms is cursor blink speed based on DOS VGA).
function doCursor() {
    crsr.firstChild.style['background-color'] = 
        (crsrBlink = !crsrBlink) ? 'transparent' : clut[getCrsrAttrColor(crsrAttr)];
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
function makeRowAttr(
    c1,
    c2,
    bp,
    size,
    width,
    marquee) {

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
function makeCellAttr(
    fg,             // foreground color
    bg,             // background color
    bold,
    italics,
    underline,
    blink,
    shadow,
    strikethrough,
    outline) {

    fg = fg || 7;
    bg = bg || 0;
    bold = bold || false;
    italics = italics || false;
    underline = underline || false;
    blink = blink || false;
    shadow = shadow || false;
    strikethrough = strikethrough || false;
    outline = outline || false;

    return (fg & 0xFF)
        | ((bg & 0xFF) << 8)
        | (bold ? A_CELL_BOLD : 0)
        | (italics ? A_CELL_ITALICS : 0)
        | (underline ? A_CELL_UNDERLINE : 0)
        | (blink ? A_CELL_BLINK : 0)
        | (shadow ? A_CELL_SHADOW : 0)
        | (strikethrough ? A_CELL_STRIKETHROUGH : 0)
        | (outline ? A_CELL_OUTLINE : 0)
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
function setCellAttrBlink(attr, blink) {
    return (attr & ~A_CELL_BLINK) | (blink ? A_CELL_BLINK : 0);
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

// get cell attribute parts
function getCellAttrFG(attr) { return attr & 0xFF; }
function getCellAttrBG(attr) { return (attr >> 8) & 0xFF; }
function getCellAttrBold(attr) { return (attr & A_CELL_BOLD) != 0; }
function getCellAttrItalics(attr) { return (attr & A_CELL_ITALICS) != 0; }
function getCellAttrUnderline(attr) { return (attr & A_CELL_UNDERLINE) != 0; }
function getCellAttrBlink(attr) { return (attr & A_CELL_BLINK) != 0; }
function getCellAttrShadow(attr) { return (attr & A_CELL_SHADOW) != 0; }
function getCellAttrStrikethrough(attr) { return (attr & A_CELL_STRIKETHROUGH) != 0; }
function getCellAttrOutline(attr) { return (attr & A_CELL_OUTLINE) != 0;}
function getCellAttrGlow(attr) { return (attr & A_CELL_GLOW) != 0; }
function getCellAttrReverse(attr) { return (attr & A_CELL_REVERSE) != 0; }
function getCellAttrConceal(attr) { return (attr & A_CELL_CONCEAL) != 0; }

// create cursor attributes
function makeCrsrAttr(
    color,          // 0-255
    size,           // 0-3
    orientation){   // 0-1

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
        row, size, width, h, w, x, cnv, i;
        
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
    
    if (cnv.height != (h + 16)) {
        // adjust for new height.
        row.style['height'] = h + 'px';
        cnv.width = 80 * colSize;
        cnv.height = (h + 16);
        // redraw this entire row
        for (i = 0; i < conText[rownum].length; i++)
            renderCell(rownum, i);
    }
}

// animate blink and marquee
function doBlink(){
    var
        r, c, y, rh;

    // y of first row.
    y = textDiv.getBoundingClientRect().top;
    for (r = 0; r < conRowAttr.length; r++) {
        // check if row is visible
        rh = rowSize * getRowAttrSize(conRowAttr[r]) / 100;
        if ((y + rh > 0) && (y < window.innerHeight)) {

            // look for blink
            // refresh blinkable text.
            for (c = 0; c < conCellAttr[r].length; c++) 
                if (conCellAttr[r][c] & A_CELL_BLINK) 
                    renderCell(r, c);
        
            // look for marquee
            if (conRowAttr[r] & A_ROW_MARQUEE) {
                // marquee this row
            }
        }
        y += rh;
    }
    cellBlink = !cellBlink;
}

// render an individual row, col
function renderCell(rownum, colnum) {
    var
        row, size, width, w, h, x, y,
        ctx, attr, ch, tfg, tbg, tbold, stroke, tmp;
        
    row = getRowElement(rownum);
    size = getRowAttrSize(conRowAttr[rownum]) / 100;    // .25 - 2
    width = getRowAttrWidth(conRowAttr[rownum])/ 100;   // .5 - 2
    w = colSize * size * width;     // width of char
    h = rowSize * size;             // height of char
    x = w * colnum;                 // left pos of char on canv
    y = 0;

    cnv = row.firstChild;           // get canvas
    if (!cnv) {
        // create new canvas if nonexistant
        cnv = document.createElement('canvas');
        cnv.style['z-index'] = '50';
        row.appendChild(cnv);
        cnv.width = 80 * w;
        cnv.height = (h + 16);
    }
    ctx = cnv.getContext('2d');
    
    attr = conCellAttr[rownum][colnum];
    ch = conText[rownum].charAt(colnum);
    tfg = (attr & 0xFF);
    tbg = (attr >> 8) & 0xff;
    tbold  = attr & A_CELL_BOLD;
    stroke = h * 0.1;  // underline/strikethrough size
    
    if (attr & A_CELL_REVERSE) {
        tmp = tfg;
        tfg = tbg;
        tbg = tmp;
        if (tfg == 0)
            tfg = 16;
    }
    if (!modeRealANSI) {
        if (tbold && (tfg < 8)) {
            tfg += 8;
        } 
        tbold = false;
    }
    
    if (tbg > 0) {
        ctx.fillStyle = clut[tbg];
        ctx.fillRect(x, 0, w, h);
    } else
        ctx.clearRect(x, 0, w, h);

    if (!(attr & A_CELL_CONCEAL) && !((attr & A_CELL_BLINK) && cellBlink)) {
        // not concealed or in blink state
        ctx.fillStyle = clut[tfg];
        ctx.font = ((attr & A_CELL_ITALICS) ? 'italic ' : '') 
            + (tbold ? 'bold ' : '') 
            + rowSize + 'px ' + fontName;
        ctx.textAlign = 'start';
        ctx.textBaseline = 'top';
        
        if (attr & A_CELL_GLOW) {
            // how does this work on scaled? test
            ctx.shadowColor = '#' + brightenRGB(clut[tfg], 0.25);
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
        
        if ((size != 1) || (width != 1)) {
            ctx.save();
            ctx.scale(size * width, size);
            ctx.translate(x / (size * width), 0);
            if (attr & A_CELL_OUTLINE) {
                ctx.strokeStyle = clut[tfg];
                ctx.lineWidth = 1;q
                ctx.strokeText(ch, 0, y);
            } else {
                ctx.fillText(ch, 0, y);
            }
            ctx.restore();
        } else {
            if (attr & A_CELL_OUTLINE) {
                ctx.strokeStyle = clut[tfg];
                ctx.lineWidth = 1;
                ctx.strokeText(ch, x, y);
            } else {
                ctx.fillText(ch, x, y);
            }
        }
    
        // draw underline / strikethough manually
        if (attr & A_CELL_UNDERLINE) {
            ctx.fillRect(x, h - stroke, w, stroke);
        }
        if (attr & A_CELL_STRIKETHROUGH) {
            ctx.fillRect(x, (h + stroke) / 2, w, stroke);
        }
    }
}

// get codepoint from string
if (!String.prototype.codePointAt) {
    String.prototype.codePointAt = function (pos) {
        pos = isNaN(pos) ? 0 : pos;
        var str = String(this),
            code = str.charCodeAt(pos),
            next = str.charCodeAt(pos + 1);
        // If a surrogate pair
        if (0xD800 <= code && code <= 0xDBFF && 0xDC00 <= next && next <= 0xDFFF) {
            return ((code - 0xD800) * 0x400) + (next - 0xDC00) + 0x10000;
        }
        return code;
    };
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

// massage string to make HTML friendly
function htmlEncode(s) {
    var
        el = document.createElement("div");

    el.innerText = el.textContent = s;
    return el.innerHTML;
}

// brighten / darken a color. color is a 24bit value (0xRRGGBB)
function brightenRGB(colorstr, factor) {
    var
        r, g, b,
        rgb = htoi(colorstr);

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
    return  itoh((r << 16) + (g << 8) + b, 6);
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
            //document.body.appendChild(createNewRow());
            textDiv.appendChild(createNewRow());
            conRowAttr[conRowAttr.length] = 0x002C0000;
            conCellAttr[conCellAttr.length] = [];
            conText[conText.length] = '';
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
    document.getElementById('osbulb').src = ((ws.readyState == 1) ? 'os':'off') + '.png';
    document.getElementById('clbulb').src = (capState ? 'cl':'off') + '.png';
    document.getElementById('nlbulb').src = (numState ? 'nl':'off') + '.png';
    document.getElementById('slbulb').src = (scrState ? 'sl':'off') + '.png';
}

// setup the crt and cursor
function initDisplay() {
    var
        o, pos,
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
    crtWidth = colSize * 80;

    pageDiv.style['width'] = crtWidth + 'px';
    
    // add indicators for online/capslk/numlk/scrlk
    pos = 0;
    o = document.createElement('img');
    o.src = 'off.png';   o.id = 'osbulb';
    o.width = 16;       o.height = 28;
    o.style['position'] = 'absolute';
    o.style['top'] = (2 + (pos++ * 30))+'px';
    o.style['right'] = '2px';
    document.body.appendChild(o);
    o = document.createElement('img');
    o.src = 'off.png';  o.id = 'clbulb';
    o.width = 16;       o.height = 28;
    o.style['position'] = 'absolute';
    o.style['top'] = (2 + (pos++ * 30))+'px';
    o.style['right'] = '2px';
    document.body.appendChild(o);
    o = document.createElement('img');
    o.src = 'off.png';  o.id = 'nlbulb';
    o.width = 16;       o.height = 28;
    o.style['position'] = 'absolute';
    o.style['top'] = (2 + (pos++ * 30))+'px';
    o.style['right'] = '2px';
    document.body.appendChild(o);
    o = document.createElement('img');
    o.src = 'off.png';  o.id = 'slbulb';
    o.width = 16;       o.height = 28;
    o.style['position'] = 'absolute';
    o.style['top'] = (2 + (pos++ * 30))+'px';
    o.style['right'] = '2px';
    document.body.appendChild(o);

    // get default attributes for page from <div id='vtxpage'...> 
    // stored as hex values in cellattr, crsrattr, and pageattr
    defCellAttr = htoi(pageDiv.getAttribute('cellattr') || '00000007');
    cellAttr =  defCellAttr;
    crsrAttr = htoi(pageDiv.getAttribute('crsrattr') || '00000207');
    pageAttr = htoi(pageDiv.getAttribute('pageattr') || '00000000');
    
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
    soundBell.addEventListener('ended', function() {
        soundBellCount--;
        if (soundBellCount) {
            this.play();
        }
    });
    
    // set page attributes
    p = pageDiv.parentNode;
    p.style['background-color'] = clut[(pageAttr >> 8) & 0xFF];
    pageDiv.style['background-color'] = clut[pageAttr & 0xFF];
    
    // set initial states.
    shiftState = ctrlState = altState 
        = capState = numState = scrState = false;
    crsrBlink = false;  // cursor blink state for intervaltimer
    cellBlink = true;
    crsrRow = crsrCol = 0;
    crsrDraw();

    // set event for page resize check and cursor blink
    elPage = document.getElementsByTagName('html')[0];
    setTimers(true);

    // one time refresh
    crsrHome();
    
    // test websocket connect
    ws = new WebSocket('ws://@InternetIP@:@WSPort@', ['vtx']);
    ws.onopen = function() { 
        setBulbs();
    }
    ws.onclose = function() { 
        conStrOut('\r\n\r\n\x1b[#9\x1b[0;91mDisconnected from server.\r\n');
        setBulbs();
    }
    ws.onmessage = function(e) { 
        conStrOut(e.data);
    }
    ws.onerror = function(error) { 
        conStrOut('\r\n\r\n\x1b[#9\x1b[0;91mError : ' + error + '\r\n');
        setBulbs();
    }
    return;
}

// set event for page resize check and cursor blink
function setTimers(onoff) {
    if (onoff) {
        irqCheckResize = setInterval(doCheckResize, 10);
        irqCursor = setInterval(doCursor, 533);
        irqBlink = setInterval(doBlink, 533);
    } else {
        clearInterval(irqCheckResize);
        clearInterval(irqCursor);
        clearInterval(irqBlink);
    }
}

addListener(window, 'load', initDisplay);
addListener(document, 'keydown', keyDown);
addListener(document, 'keyup', keyUp);
addListener(document, 'keypress', keyPress);
addListener(document, 'mouseup', mouseAction);
addListener(document, 'mousedown', mouseAction);
addListener(document, 'mouseclick', mouseAction);
addListener(document, 'beforepaste', beforePaste);
addListener(document, 'paste', paste);

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
    conStrOut(clipboardData.getData('Text'));
}

// update cursor from crsrAttr values
function newCrsr() {
    const
        sizes = [ '0%', '10%', '25%', '100%' ];
    var
        o, c, z, sz, ax1, ax2;

    if (crsr == null) {
        crsr = document.createElement('div');
        crsr.style['position'] = 'absolute';
        crsr.style['display'] = 'block';
        crsr.style['z-index'] = '999';
        o = document.createElement('div');
        o.id = 'crsr';
        o.style['position'] = 'absolute';
        o.style['display'] = 'block';
        o.style['bottom'] = '0px';
        o.style['left'] = '0px';
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
    o.style['background-color'] = clut[c];
}

/*
alert(
    'OS: ' + jscd.os +' '+ jscd.osVersion + '\n' +
    'Browser: ' + jscd.browser +' '+ jscd.browserMajorVersion +
      ' (' + jscd.browserVersion + ')\n' + 
    'Mobile: ' + jscd.mobile + '\n' +
    'Flash: ' + jscd.flashVersion + '\n' +
    'Cookies: ' + jscd.cookies + '\n' +
    'Screen Size: ' + jscd.screen + '\n\n' +
    'Full User Agent: ' + navigator.userAgent
);
*/
