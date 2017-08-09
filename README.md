# VTX_ClientServer

updated: 08-AUG-2017


## Intro

VTX consists of a web / websocket server written in FreePascal, and a browser 
based javascript client. 

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

The client is written in javascript.

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

VIC 20, Commodore 64 or 128 font support.

All PETSCII controls are emulated.

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

NodeType : 'ExtProc' or 'Telnet'

CodePage: The codepage that is running on the node process telnet service. 
Currently supported Code Pages:
ARMSCII_8, ATASCII, C128, C64, CP437, CP667, CP668, CP737, CP770, CP771, CP772, 
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


## Roadmap

Investigate ability for websocket server to accept non-'user' connections from
other VTX servers for passing private network information like mail or 
echo-style messages.

Migrate away from Synape TCP libraries to low level sockets in HTTP and WS 
servers.

Migrate telnet negotiation handshaking to the client so the VTX client can be 
used as stand alone websocket telnet client.

Encapsulate vtx client functions and variables into a vtx object. (vtx = {})

Softfonts - Built in bitmap font to vector font converter using method in 
bdf2svg (in utils).


## To Do($) / To Fix(!) / Investigate(?)

! Client handshake with server.

$ Telnet negotiation moved from server to client.

$ Encapsulate all vtx functions / variables into vtx object.

$ Move sprite commands.

$ HTTP requests as new thread.

? thin version of uvga16.

? WSS / certificate &| poor-mans client<->server encryption with rotating keys.

? VT100 compatibily mode.

$ Redo website.

$ Test.test.test.test. 
