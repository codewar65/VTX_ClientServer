# VTX_ClientServer

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
this stream to the 'user' through the web socket connection.

The server console is written in Free Pascal.

The client is written in javascript.

The sample 'node' is written in Free Pascal.

Lazarus project files are included.

The www directory contains files sent via HTTP.

The node directory contains files needed for the 'node' applications.

The work directory is various unneeded stuff I use for web graphics and testing.

