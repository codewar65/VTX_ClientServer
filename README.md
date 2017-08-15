# VTX_ClientServer

updated: 15-AUG-2017


![VTX Client Screenshot](https://raw.githubusercontent.com/codewar65/VTX_ClientServer/master/work/vtx_screen1.png "Logo Title Text 1")

## Intro

VTX consists of a web / websocket server written in FreePascal, and a browser 
based javascript client.  The client can be run as a stand alone item if the
operator has their own webserver and access to a websocket proxy.

The server software listens to two separate ports while running. A port for 
HTTP, and a separate port for Web Sockets.

A 'user' connects to the web server via HTTP. The web server delivers a client 
payload (html, css, javascript client, web font) to the 'user's web browser.

The client software then connects to the server via a web socket connection. 
Upon a 'user' connection to the web socket server, the server will wither spawn 
a 'sysop' defined application ('node' software) or connect to a telnet server. 
The 'node' software pipes raw binary text to / from the server via StdIn / 
StdOut. The server then relays this stream to the 'user' through the web socket 
connection. Alternately, VTX server can be configured to connect to a remote 
telnet server (bbs or other service).

The server console is written in Free Pascal.

The client is written in javascript. The client can be run independant of the 
server if you have your own web and websocket proxy. (See Client Only Setup 
below.)

The sample 'node' is written in Free Pascal.

Lazarus project files are included.

The www directory contains files sent via HTTP.

The node directory contains files needed for the 'node' applications.

The work directory is various unneeded stuff I use for web graphics and testing.


## Client Features

Any column width display by any length page. Using webfont specified in html/css 
template file (in the www path).

ANSI: 256 colors (base 8 + high intensity + 6x6x6 color cube + grays) - see 
https://en.wikipedia.org/wiki/ANSI_escape_code PETSCII: Commodore color palettes.

Transparent black in VTX mode.

Variable row sizing, row background effects, and marquee text rows in VTX ANSI 
mode.

Support up to 64 SVG sprites from 64 SVG sprite definitions. 

See https://github.com/codewar65/VTX_ClientServer/blob/master/vtx.txt for ANSI 
code sequences supported.

Y-Modem 1K upload and download. 

Build in audio player for streaming audio with ANSI sequences to set source, 
volume, play, stop.


## PETSCII Mode

VIC 20, Commodore 64 or 128 font support. All PETSCII controls are emulated. 
Color palette depends on CodePage.

Keys mapped to:
* Backspace   = BACK/DEL
* Del         = BACK/DEL
* ESC         = RUN/STOP
* END         = Text Mode
* Shift END   = Graphics Mode
* HOME        = HOME
* Shift HOME  = CLR
* Arrows      = Arrows
* Ctrl 1      = Black
* Ctrl 2      = White
* Ctrl 3      = Red
* Ctrl 4      = Cyan
* Ctrl 5      = Purple
* Ctrl 6      = Green
* Ctrl 7      = Blue
* Ctrl 8      = Yellow
* Ctrl 9      = Reverse Off
* Ctrl 0      = Reverse On
* Alt 1       = Orange
* Alt 2       = Brown
* Alt 3       = Lt Red
* Alt 4       = Dk Gray
* Alt 5       = Gray
* Alt 6       = Lt Green
* Alt 7       = Lt Blue
* Alt 8       = Lt Gray
* F1-F8       = F1-F8
* Tab         = Toggle Text / Graphics


## ATASCII Mode

ATARI 8bit font support. All ATASCII controls are emulated. Only two color support (0/1).

Keys mapped to:
* ESC         = Escape
* Arrows      = Cursor move
* Shift HOME  = Clear Screen 
* Backspace   = Backspace
* Tab         = Tab
* SHIFT Tab   = Set Tabstop
* CTRL Tab    = Clear Tabstop
* Return      = End Line
* SHIFT DEL   = Delete row
* SHIFT INS   = Insert Row
* CTRL 2      = Bell
* DEL         = Delete
* INS         = Insert


## Compiling

There is no make procedure at this time. It currently only builds on Windows. 
Basically, snag a copy of the latest Lazarus / Free Pascal, open the project 
file and build.

I have ported the current project to console more so it will be easier to port 
to other platforms.

Larazus : http://www.lazarus-ide.org/

Free Pascal : https://www.freepascal.org/

You will need the Synapse package (synapse40) to compile at this time until I 
get around to replacing them with my own routines.


## Setup

The vtxserv.ini file contains settings for configuring the server.

SystemName : the name of the server / node processes. Typically the name of of a 
bulletin board system. The name here is embedded into the HTML sent to the 
client from the VTX web server.

SystemIP : The IP address that the server needs to bind to. If you are behind a 
firewall / router, this would be your internal LAN IP.

InternetIP : The address that 'user's out in internet land will need to visit to 
hit your system (via port forwarding, etc.)

HTTPPort : The port number the web server will be listening to HTTP requests on.

WSPort : The websocket port number.

WSSecure : 0 = use ws, 1 = use wss

NodeType : 'ExtProc' or 'Telnet'

Terminal : Terminal type. PETSCII, ATASCII or other..

CodePage: The codepage that is running on the node process telnet service. 
Currently supported Code Pages:
ARMSCII_8, ATARI, C128, C64, CP437, CP667, CP668, CP737, CP770, CP771, CP772, 
CP773, CP774, CP775, CP790, CP808, CP813, CP819, CP850, CP851, CP852, CP853, 
CP855, CP857, CP858, CP859, CP860, CP861, CP863, CP865, CP866, CP867, CP869, 
CP872, CP878, CP895, CP900, CP912, CP915, CP920, CP991, CP1117, CP1118, CP1119, 
CP1131, CP28593, CP65001, CPMIK, HAIK8, ISO8859_1, ISO8859_10, ISO8859_13, 
ISO8859_14, ISO8859_15, ISO8859_16, ISO8859_2, ISO8859_3, ISO8859_4, ISO8859_5, 
ISO8859_7, ISO8859_9, KOI8_R, KOI8_U, TELETEXT, UTF8, UTF16, VIC20, WIN1250, 
WIN1251, WIN1253, WIN1254, WIN1257

ExtProcess : the name of the node process that is launched for a 'user' 
connection. This will be the main process that the 'user' interacts with. The 
program needs to communicate with the server via StdIn / StdOut UTF-8 IO pipe 
streams. These streams need to be character based and not line based to function 
properly.

TelnetIP : IP address that the server needs to connect to the telnet server.

TelnetPort : The port number the telnet server is running on.

MaxConnections : maximum number of websocket connections allowed to operate at
the same time.

AutoConnect : 0 = don't autoconnect on lauch, 1 = connect on launch.


## Running VTXServ

Type vtxserv from the command line. Once the server is running you can enter
commands like:

START ALL : start up the web server, websocket server, and the bridge process.

STOP ALL : stop all processes.

QUIT : close all 'nodes', connections, servers, and exit the program.

LIST : list connections.

KICK n : will hang up on a connection

HELP : displays help with other commands.

CLS : clear the console window.

STATUS : show server status.

LOADCFG : stop listeners, reload INI settings, and restart.


## Node Software

Sample node software is in the node directory. Actual BBS software is not in the
scope of this project, but feel free to tinker, or retrofit existing packages
to work with VTX.


## Client Only Setup.

To run the client without using the VTX server software, all you need is a web page and access to a websocket proxy server.

### Files Needed:

*   vtxdata.js          (see below for customizing)
*   vtxclient.js        (or vtxclient.min.js)
*   *.woff              (terminal fonts needed for the client. include ALL of these or the client will not boot.)
*   *.png               (24x24px images for the UI. customize if you want to replace these.)
*   bell.mp3            (bing! the bell sound. customize if you want to replace this.)

### HTML additions.

In the HTML that will contain the client, in the <HEAD>, include:

```html
    <script type='text/javascript' src='https://cdn.jsdelivr.net/pako/1.0.3/pako.min.js'></script>
    <script type='text/javascript' src='vtxdata.js'></script>
    <script type='text/javascript' src='vtxclient.min.js'></script>
```

In the <BODY> of your page, place something like (the outer 2 divs are the border area and one to center the terminal on the page. you can tinker with these to suit your website):

```html
        <!-- =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- -->
        <!-- center term on page -->
        <div style='text-align:center'> 
            <div id='vtxclient' style='text-align:center;margin:0 auto;display:inline-block;padding:34px;'>
                <!-- VTX client will appears in here -->
            </div>
        </div>
        <!-- =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- -->
```


The vtxpage div element will house the client once the vtxclient.js has booted up.

The CSS file can be customized a little too to help make the client fit your style. Refer to vtxclient.css for info.

Place the two javascript files on your webserver so the HTML can get at them.

### vtxdata.js

The contents of the vtxdata is information the client needs to connect to your system.    

**sysName** will be place in the <TITLE> of the page if it is missing or blank.

**wsConnect** is the [ws/wss]://url:port to the websocket service that will direct you to your system.

**term** is the terminal type that gets reported to the telnet server. Use PETSCII if you
are connecting to a Commodore style board.

**codePage** is the default codepage of the system.

**crtCols**, **crtRows** are the dimensions of the a normal terminal screen. Rows may grow as upto **crtHistory**.
Lines of data beyond **crtHistory** get appended to the bottom, and rows from the top of the terminal
get truncated.

**xScale** is a scaling factor for displaying the terminal. A value of 1 is no scaling, values larger than 1 would double the size of the terminal horizontally.

**initStr** contains optional ANSI codes that can be sent to the term prior to connection
to set it into whatever modes the system operator desires.

**defPageAttr** is a value that defines the colors of the page, bits 7-0 are the colors of the page,
bits 15-8 is the border color (the parent container outside of the main vtxpage div). Colors
are 0x00 - 0xFF (ANSI colors whereas color 0 = transparent).

**defCrsrAttr** is the default cursor attributes or how the cursor will be displayed. Bits 7-0 are
the color (0x00 - 0xFF), bit 9-8 define the style (0=none, 1=thin, 2=thick, 3=full block),
bit 10 defines the orientation (0=horizontal, 1=vertical)

**defCellAttr** defines the default character attributes (what CSI 0 m reverts to). Bits 7-0 are
the foreground color, bits 15-8 are the background color. Other bits can be set as well. See
the documention at the head of vtxclient.js on GitHub.

**telnet** lets the client know if the server is connecting to a telnet service if set to 1. If the value
is set to 0, no telnet handshaking negotiations will take place once a connection is made.

**autoConnect** 0 = don't autoconnect on launch, 1 = autoconnect.

** fontName** is the default font to use. UVGA16 should be specified in most cases. 

** fontSize** the default font size to use. 16px unscaled default size. Larger sizes will increase the required size of the client.

```javascript
var vtxdata = {
  sysName:     "VTX Home System",
  wsConnect:   "ws://142.105.247.156:7003",
  term:        "ANSI",
  telnet:      1,
  autoConnect: 0,
  codePage:    "CP437",
  fontName:    'UVGA16',
  fontSize:    '16px',
  crtCols:     80,
  crtRows:     25,
  crtHistory:  500,
  xScale:      1,
  initStr:     "",
  defPageAttr: 0xF410,
  defCrsrAttr: 0x0207,
  defCellAttr: 0x0007
};
```

## To Do($) / To Fix(!) / Investigate(?)

$ HTTP requests as new thread.

? WSS / certificate support in server.

$ Redo website.

? thin version of uvga16.

$ Reduce the server to web server / websocket proxy only with not mangling of web files.
