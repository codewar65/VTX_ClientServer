# VTX_ClientServer

updated: 24-JUL-2017


## Intro

VTX consists of a web / websocket server written in FreePascal, and a browser 
based javascript client. 

The server software listens to two separate ports while running. A port for
HTTP, and a separate port for Web Sockets.

A 'user' connects to the web server via HTTP. The web server delivers a client
payload (html, css, javascript client, web font) to the 'user's web browser.

The client software then connects to the server via a web socket connection. 
Upon a 'user' connection to the web socket server, the server will spawn a 
'sysop' defined application ('node' software). The 'node' software pipes 
UTF-8 ANSI text to / from the server via StdIn / StdOut. The server then relays
this stream to the 'user' through the web socket connection. Alternately,
VTX server can be configured to connect to a remote telnet server (bbs or other
service).

The server console is written in Free Pascal.

The client is written in javascript.

The sample 'node' is written in Free Pascal.

Lazarus project files are included.

The www directory contains files sent via HTTP.

The node directory contains files needed for the 'node' applications.

The work directory is various unneeded stuff I use for web graphics and testing.


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

SystemName : the name of the server / node processes. Typically the name of
of a bulletin board system. The name here is embedded into the HTML sent to
the client from the VTX web server.

SystemIP : The IP address that the server needs to bind to. If you are behind
a firewall / router, this would be your internal LAN IP.

InternetIP : The address that 'user's out in internet land will need to visit 
to hit your system (via port forwarding, etc.)

HTTPPort : The port number the web server will be listening to HTTP requests on.

WSPort : The websocket port number.

NodeType : 'ExtProc' or 'Telnet'

ExtProcess : the name of the node process that is launched for a 'user' 
connection. This will be the main process that the 'user' interacts with. The
program needs to communicate with the server via StdIn / StdOut UTF-8 IO pipe 
streams. These streams need to be character based and not line based to
function properly.

TelnetIP : IP address that the server needs to connect to the telnet server.

TelnetPort : The port number the telnet server is running on.

TelnetCP : The codepage that is running on the eternal telnet service.

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

CONV : convert an ANSI text file to UTF-8

CPS : list available code pages for use with CONV.

CLS : clear the console window.

STATUS : show server status.


## Node Software

Sample node software is in the node directory. Actual BBS software is not in the
scope of this project, but feel free to tinker, or retrofit existing packages
to work with VTX.


## Roadmap

Stablize server software - fully crashproof and able to always terminate phantom
nodes.

Add additional tool to server console for convert SVG files to Base64 for use
in node software. Possibly remove these tools to separate command line utilities.

Clean up demo node software. Develop ncurses compatible functions and procedures.

Add new escape sequences to define clickable hotspots for sending a key or string
to the server. Additionally, define hotspot for external URLs links that open in 
an new page.

Investigate ability for websocket server to accept non-'user' connections from
other VTX servers for passing private network information like mail or echo-style
messages.

Migrate away from Synape TCP libraries to low level sockets in HTTP and WS 
servers.


## To Do / To Fix / Bugs

Testing YModem download. Write YModem upload.

Merge row size, row background, and page/cursor attributes into one ESC sequence
group.

Add codes to reset page and cursor attributes to default.
