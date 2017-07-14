program vtxserv;

{$mode objfpc}{$H+}
{$apptype console}

uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}

  Classes,
  Process,
  Pipes,
  SysUtils,
  BlckSock,
  Sockets,
  Synautil,
  Laz_Synapse,
  WebSocket2,
  CustomServer2,
  crt;

const
  CRLF = #13#10;

type
  TvtxHTTPServer = 		class;	// very basic web server for dishing out client
	TvtxWSServer = 			class;	// quasi robust websocket server
  TvtxIOBridge = 			class;
  TvtxWSConnection = 	class;	// websocket connection spawns consoles per connection
  TvtxProcessNanny = 	class;

  TvtxProgressEvent = procedure(msg: string) of object;

  { TvtxHTTPServer : for basic webserver front end for dishing out client }
  TvtxHTTPServer = class(TThread)
	  private
  	  fProgress : 	string;
    	fOnProgress : TvtxProgressEvent;
	    procedure 		DoProgress;

	  protected
  	  procedure 		Execute; override;
      procedure 		AttendConnection(ASocket : TTCPBlockSocket);
      function 			SendFile(
                			ASocket : TTCPBlockSocket;
                    	ContentType : string;
                    	Filename : string) : integer;
      function 			SendImage(
                    	ASocket : TTCPBlockSocket;
                    	ContentType : string;
                    	Filename : string) : integer;

	  public
  	  constructor 	Create(CreateSuspended: boolean);
	    destructor 		Destroy; override;
  	  property 			OnProgress : TvtxProgressEvent
											read fOnProgress
                      write fOnProgress;
  end;

  { TvtxIOBridge : these are for the background worker that routes stdout from
  	client processes to websocket output and input from websocket to stdin of
   	client processes. }
  TvtxIOBridge = class(TThread)
	  private
  	  fProgress : 		string;
    	fOnProgress: 		TvtxProgressEvent;
	    procedure 			DoProgress;

	  protected
  	  procedure 			Execute; override;
      procedure 			PipeToConn(
                      	input : TInputPipeStream;
                        conn : TvtxWSConnection);

	  public
  	  constructor 		Create(CreateSuspended : boolean);
	    destructor 			Destroy; override;
  	  property 				OnProgress : TvtxProgressEvent
                      	read fOnProgress
                        write fOnProgress;
  end;

  { TvtxWSServer : websocket server class object }
	TvtxWSServer = class(TWebSocketServer)
  	public
    	function GetWebSocketConnectionClass(
      					Socket: TTCPCustomConnectionSocket;
					      Header: TStringList;
					      ResourceName, sHost, sPort, Origin, Cookie: string;
			      out HttpResult: integer;
			      var Protocol, Extensions: string) : TWebSocketServerConnections; override;
  end;

  { TvtxProcessNanny : thread for spawning connection console. terminate
  	connection on exit }
  TvtxProcessNanny = class(TThread)
	  private
  	  fProgress : string;
    	fOnProgress : TvtxProgressEvent;
	    procedure DoProgress;

	  protected
  	  procedure Execute; override;

	  public
      serverCon : TvtxWSConnection;
  	  constructor Create(CreateSuspended: boolean);
	    destructor 	Destroy; override;
  	  property 		OnProgress: TvtxProgressEvent read fOnProgress write fOnProgress;
  end;

  { TvtxWSConnection : Websocket connection class. }
  TvtxWSConnection = class(TWebSocketServerConnection)
    public
			ExtNanny	 :	TvtxProcessNanny;	// the TThread that runs below ExtProcess
      ExtProcess : 	TProcess;					// the TProcess spawned board

	    property ReadFinal: boolean read fReadFinal;
	    property ReadRes1: boolean read fReadRes1;
	    property ReadRes2: boolean read fReadRes2;
	    property ReadRes3: boolean read fReadRes3;
	    property ReadCode: integer read fReadCode;
	    property ReadStream: TMemoryStream read fReadStream;

  	  property WriteFinal: boolean read fWriteFinal;
	    property WriteRes1: boolean read fWriteRes1;
    	property WriteRes2: boolean read fWriteRes2;
  	  property WriteRes3: boolean read fWriteRes3;
	    property WriteCode: integer read fWriteCode;
    	property WriteStream: TMemoryStream read fWriteStream;
  end;

	{ TvtxBBSInfo : Board Inofo record }
  TvtxBBSInfo = record
    BBSName : 		string;		// NAME OF BBS - WEBPAGE TITLE
    IPAddress : 	string;		// IP ADDRESS OF THIS HOST
    HTTPPort : 		string;		//PORT NUMBER FOR HTTP FRONT END
    WSPort : 			string;		// PORT NUMBER FOR WS BACK END
  end;

  { TvtxApp : Main application class }
  TvtxApp = class
    procedure StartHTTP;
    procedure StartWS;
    procedure StartBridge;
    procedure StopHTTP;
    procedure StopWS;
    procedure StopBridge;
    procedure WriteCon(msg : string); register;
    procedure WSBeforeAddConnection(Server : TCustomServer; aConnection : TCustomConnection;var CanAdd : boolean); register;
    procedure WSAfterAddConnection(Server : TCustomServer; aConnection : TCustomConnection); register;
    procedure WSBeforeRemoveConnection(Server : TCustomServer; aConnection : TCustomConnection); register;
    procedure WSAfterRemoveConnection(Server : TCustomServer; aConnection : TCustomConnection); register;
    procedure WSSocketError(Server: TCustomServer; Socket: TTCPBlockSocket); register;
    procedure WSOpen(aSender: TWebSocketCustomConnection);
    procedure WSRead(aSender : TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3 : boolean; aCode :integer; aData :TMemoryStream);
    procedure WSWrite(aSender :	TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3 :	boolean; aCode :integer; aData :TMemoryStream);
    procedure WSClose(aSender: TWebSocketCustomConnection; aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure WSTerminate(Sender : TObject); register;

    procedure BridgeTerminate(Sender: TObject);
    procedure NannyTerminate(Sender: TObject);
    procedure HTTPTerminate(Sender : TObject); register;
  end;

  TServices = ( HTTP, WS, Bridge, All, Unknown );
  TServSet = set of TServices;


var
  Done : boolean;
	l : string;
  app : TvtxApp;

  serverWS : 		TvtxWSServer;		// ws server.
  serverHTTP :	TvtxHTTPServer;	// http srever.
	bridgeWS :		TvtxIOBridge;		// bridge streams.

  runningWS,
	runningHTTP,
  runningBridge: boolean;

  BBSInfo : 		TvtxBBSInfo =   (
  	BBSName : 		'NWB 0.0.1';
    IPAddress : 	'192.168.0.2';
    //IPAddress : '142.105.247.156';
    HTTPPort : 		'7001';
    WSPort : 			'7003'; );

{ TvtxIOBridge }

constructor TvtxIOBridge.Create(CreateSuspended: boolean);
  begin
    fProgress := '';
    FreeOnTerminate := True;
    inherited Create(CreateSuspended);
  end;

destructor TvtxIOBridge.Destroy;
  begin
    inherited Destroy;
  end;

procedure TvtxIOBridge.DoProgress;
  begin
    if Assigned(FOnProgress) then
    begin
      FOnProgress(fProgress);
    end;
  end;

procedure TvtxIOBridge.PipeToConn(input : TInputPipeStream; conn : TvtxWSConnection);
  var
    i, bytes : integer;
    b : byte;
    str : ansistring;
  begin
  	bytes := input.NumBytesAvailable;
    if bytes > 0 then
    begin
      for i := 0 to bytes - 1 do
      begin
  			b := input.ReadByte;
        str += char(b);
      end;
      conn.SendText(str);
    end;
  end;

procedure TvtxIOBridge.Execute;
  var
    i : integer;
    conn : TvtxWSConnection;
  begin
    // for each connect that has process, send input, read output
    repeat
      if not serverWS.CheckTerminated then
      begin
      	for i := 0 to serverWS.Count - 1 do
  	  	begin
      		conn := TvtxWSConnection(serverWS.Connection[i]);

        	if not conn.Closed then
        	begin
          	if (conn.ExtProcess <> nil) and conn.ExtProcess.Running then
   	      	begin

              // send console stdout to websocket connection
              if conn.ExtProcess.Output <> nil then
                PipeToConn(conn.ExtProcess.Output, conn);

              // send console stderr to websocket connection
              if conn.ExtProcess.Stderr <> nil then
                PipeToConn(conn.ExtProcess.Stderr, conn);

    	      end;
  		  	end;
       	end;
  	  end;

      // keep this thread from smoking the system
    	sleep(25);

    	if Terminated then
      	break;

  	until false;
    fProgress := 'Bridge Terminating.';
    Synchronize(@DoProgress);
  end;


{ TvtxHTTPServer }

constructor TvtxHTTPServer.Create(CreateSuspended: boolean);
begin
	fProgress := '';
	FreeOnTerminate := True;
	inherited Create(CreateSuspended);
end;

destructor TvtxHTTPServer.Destroy;
begin
  inherited Destroy;
end;

procedure TvtxHTTPServer.DoProgress;
begin
  if Assigned(FOnProgress) then
  begin
    FOnProgress(fProgress);
  end;
end;

// main http listener loop
procedure TvtxHTTPServer.Execute;
var
  ListenerSocket,
  ConnectionSocket: TTCPBlockSocket;
begin
  begin
    ListenerSocket := TTCPBlockSocket.Create;
    ConnectionSocket := TTCPBlockSocket.Create;

    ListenerSocket.CreateSocket;
    ListenerSocket.SetLinger(true, 10);
    ListenerSocket.Bind(BBSInfo.IPAddress, BBSInfo.HTTPPort);
    ListenerSocket.Listen;

    repeat
      if ListenerSocket.CanRead(1000) then
      begin
        ConnectionSocket.Socket := ListenerSocket.accept;

        if ConnectionSocket.LastError <> 0 then
        begin
					fProgress:='Error HTTP accepting socket.';
          Synchronize(@DoProgress);
        end
        else
					AttendConnection(ConnectionSocket);
        ConnectionSocket.CloseSocket;
      end;

      if Terminated then
        break;

    until false;
    ListenerSocket.Free;
    ConnectionSocket.Free;
  end;
end;

// send an image from www directory
function TvtxHTTPServer.SendImage(
          ASocket : TTCPBlockSocket;
          ContentType : string;
          Filename : string) : integer;
var
  fin : TFileStream;
  size : integer;
  buff : pbyte;
	expires : TTimeStamp;

begin
  result := 200;
  Filename := 'www' + Filename;
  if FileExists(Filename) then
  begin
    expires := DateTimeToTimeStamp(now);
    expires.Date += 30;  // + 30 days

		fin := TFileStream.Create(Filename, fmShareDenyNone);
	  size := fin.Size;
	  buff := getmemory(fin.Size);
	  fin.ReadBuffer(buff^, Size);
	  fin.Free;

	  ASocket.SendString(
  	 	  'HTTP/1.0 200' + CRLF
			+ 'Pragma: public' + CRLF
      + 'Cache-Control: max-age=86400' + CRLF
      + 'Expires: ' + Rfc822DateTime(TimeStampToDateTime(expires)) + CRLF
			+ 'Content-Type: ' + ContentType + CRLF
	    + '' + CRLF);
	  ASocket.SendBuffer(buff, size);
	  freememory(buff);
  end
  else
  	result := 404;
end;

// send text file from www directory
function TvtxHTTPServer.SendFile(
          ASocket : TTCPBlockSocket;
          ContentType : string;
          Filename : string) : integer;
var
  fin : TFileStream;
  size : integer;
  buff : pbyte;

begin
  result := 200;
  Filename := 'www' + Filename;
  if FileExists(Filename) then
  begin
		fin := TFileStream.Create(Filename, fmShareDenyNone);
	  size := fin.Size;
	  buff := getmemory(fin.Size);
	  fin.ReadBuffer(buff^, Size);
	  fin.Free;

	  ASocket.SendString(
  	 	  'HTTP/1.0 200' + CRLF
			+ 'Content-Type: ' + ContentType + CRLF
	    + 'Content-Length: ' + IntToStr(size) + CRLF
	    + 'Connection: close' + CRLF
	    + 'Date: ' + Rfc822DateTime(now) + CRLF
	    + 'Server: VTX Mark-II' + CRLF + CRLF
	    + '' + CRLF);
	  ASocket.SendBuffer(buff, size);
	  freememory(buff);
  end
  else
  	result := 404;
end;

// fulfill the request.
procedure TvtxHTTPServer.AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout,
  code : 			integer;
  s,
  ext,
  method,
  uri,
  protocol : 	string;

begin
  timeout := 120000;

  //read request line
  s := ASocket.RecvString(timeout);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  protocol := fetch(s, ' ');

  //read request headers
  repeat
    s := ASocket.RecvString(Timeout);
  until s = '';

  code := 200;
  if uri = '/' then
	  code := SendFile(ASocket, 'text/html', '/index.html')
  else
  begin
   	ext := ExtractFileExt(uri);
		case ext of
      '.css':		code := SendFile(ASocket, 'text/css', uri);
      '.js':		code := SendFile(ASocket, 'text/javascript', uri);
      '.png':		code := SendImage(ASocket, 'image/png', uri);
      // add others here as needed
			else			code := 404;
    end;
  end;

  if code <> 200 then
	  ASocket.SendString(
	    	'HTTP/1.0 ' + IntToStr(code) + CRLF
      + httpCode(code) + CRLF);
end;


{ TvtxProcessNanny }

// thread launched that launches tprocess and waits for it to terminate.
// closes clients connection at end.
constructor TvtxProcessNanny.Create(CreateSuspended: boolean);
begin
  fProgress := '';
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

destructor TvtxProcessNanny.Destroy;
begin
  inherited Destroy;
end;

procedure TvtxProcessNanny.DoProgress;
begin
  if Assigned(FOnProgress) then
  begin
    FOnProgress(fProgress);
  end;
end;

procedure TvtxProcessNanny.Execute;
begin
  // for each connect that has process, send input, read output
  if serverWS <> nil then
  begin

    fProgress := 'Spawning process for ' + serverCon.Socket.GetRemoteSinIP + '.';
    Synchronize(@DoProgress);

		serverCon.ExtProcess := TProcess.Create(nil);
    serverCon.ExtProcess.Executable := 'cscript.exe';
    serverCon.ExtProcess.Parameters.Add('//Nologo');
		serverCon.ExtProcess.Parameters.Add('//I');
    serverCon.ExtProcess.Parameters.Add('test.js');
    serverCon.ExtProcess.Parameters.Add(serverCon.Socket.GetRemoteSinIP);
    serverCon.ExtProcess.Options := [
    		poWaitOnExit,
      	poUsePipes,
        poStderrToOutPut,
      	poNoConsole,
        poDefaultErrorMode
      ];

		// go run.
    serverCon.ExtProcess.Execute;

    serverCon.ExtProcess.Free;

		// diconnect afterwards.
		serverCon.Close(wsCloseNormal, 'Good bye');

    fProgress := 'Finished process for ' + serverCon.Socket.GetRemoteSinIP + '.';
    Synchronize(@DoProgress);

  end;
  //fProgress := 'Process Terminating.';
  //Synchronize(@DoProgress);
end;


{ TvtxWSServer }

function TvtxWSServer.GetWebSocketConnectionClass(
          Socket: TTCPCustomConnectionSocket;
		      Header: TStringList;
		      ResourceName, sHost, sPort, Origin, Cookie: string;
      out HttpResult: integer;
      var Protocol, Extensions: string): TWebSocketServerConnections;
begin
  result := TvtxWSConnection;
end;


{ TvtxApp - main application stuffz }

procedure TvtxApp.BridgeTerminate(Sender: TObject);
begin
  WriteCon('Bridge Terminated.');
end;

procedure TvtxApp.NannyTerminate(Sender: TObject);
begin
	//WriteCon('Process Terminated.');
end;

procedure TvtxApp.WSBeforeAddConnection(
  		Server : 			TCustomServer;
      aConnection :	TCustomConnection;
  var CanAdd : 			boolean); register;
begin
	WriteCon('Before Add WS Connection.');
end;

procedure TvtxApp.WSAfterAddConnection(
      Server : TCustomServer;
      aConnection : TCustomConnection); register;
var
  con : TvtxWSConnection;
begin
	con := TvtxWSConnection(aConnection);

	WriteCon('WS Connection from ' + con.Socket.GetRemoteSinIP + '.');

  con.OnOpen :=  @WSOpen;
  con.OnRead :=  @WSRead;
  con.OnWrite := @WSWrite;
  con.OnClose := @WSClose;

//  con.SendText('Test!');
	// spawn a new process for this connection
  con.ExtNanny := TvtxProcessNanny.Create(true);
  con.ExtNanny.serverCon := con;
  con.ExtNanny.FreeOnTerminate := true;
  con.ExtNanny.OnProgress := @WriteCon;
  con.ExtNanny.OnTerminate := @NannyTerminate;
	con.ExtNanny.Start;

end;

procedure TvtxApp.WSOpen(aSender: TWebSocketCustomConnection);
begin
	//WriteCon('Open WS Connection.');
end;

procedure TvtxApp.WSRead(
      aSender :	TWebSocketCustomConnection;
      aFinal,
      aRes1,
      aRes2,
      aRes3 :		boolean;
      aCode :		integer;
      aData :		TMemoryStream);
var
  i, bytes : integer;
  str : ansistring;
  con : TvtxWSConnection;
begin
	//WriteCon('Read WS Connection.');

  // send aData to process - doesn't work in bridge so do it here.
	con := TvtxWSConnection(aSender);
  if (con.ExtProcess <> nil) and con.ExtProcess.Running then
  begin
    bytes := aData.Size;
    if bytes > 0 then
    begin
      for i := 0 to bytes - 1 do
      begin
        str += char(aData.ReadByte);
      end;
			con.ExtProcess.Input.WriteAnsiString(str);
    end;
  end;
end;

procedure TvtxApp.WSWrite(
      aSender :	TWebSocketCustomConnection;
      aFinal,
      aRes1,
  		aRes2,
      aRes3 :		boolean;
      aCode :		integer;
      aData :		TMemoryStream);
begin
	//WriteCon('Write WS Connection.');
end;

procedure TvtxApp.WSClose(
      aSender: TWebSocketCustomConnection;
  		aCloseCode: integer;
      aCloseReason: string;
      aClosedByPeer: boolean);
begin
	//WriteCon('Close WS Connection.');
end;

procedure TvtxApp.WSBeforeRemoveConnection(
  		Server : 			TCustomServer;
      aConnection :	TCustomConnection); register;
begin
	//WriteCon('Before Remove WS Connection.');
end;

procedure TvtxApp.WSAfterRemoveConnection(
      Server : TCustomServer;
      aConnection : TCustomConnection); register;
begin
	//WriteCon('After Remove WS Connection.');
end;

procedure TvtxApp.WSSocketError(
      Server: TCustomServer;
      Socket: TTCPBlockSocket); register;
begin
	WriteCon('WS Error for ' + Socket.GetRemoteSinIP + '. Error : '
  	+ Socket.LastErrorDesc + '.');
end;

procedure TvtxApp.WSTerminate(Sender : TObject); register;
begin
	//WriteCon('Terminated WS Connection.');
end;

procedure TvtxApp.WriteCon(msg : string); register;
begin
  case WhereX of
    1:		writeln(msg);
    2:		writeln(#8 + msg);
    else	writeln(#10#13+msg);
    end;
end;

procedure TvtxApp.StartHTTP;
begin
  // start http server
  if not runningHTTP then
  begin
    WriteCon(format('HTTP server starting on port %s.', [BBSInfo.HTTPPort]));
  	serverHTTP := TvtxHTTPServer.Create(true);
	  serverHTTP.OnProgress := @WriteCon;
  	serverHTTP.OnTerminate := @HTTPTerminate;
	  serverHTTP.Start;
    runningHTTP := true;
  end
  else
	  WriteCon('HTTP server already running.');
end;

procedure TvtxApp.StartWS;
begin
  // create websocket server
  if not runningWS then
  begin
    WriteCon(format('WS server starting on port %s.', [BBSInfo.WSPort]));
	  serverWS := TvtxWSServer.Create(BBSInfo.IPAddress, BBSInfo.WSPort);
	  serverWS.MaxConnectionsCount := 4;
	  serverWS.OnBeforeAddConnection := @WSBeforeAddConnection;
	  serverWS.OnAfterAddConnection := @WSAfterAddConnection;
	  serverWS.OnBeforeRemoveConnection := @WSBeforeRemoveConnection;
	  serverWS.OnAfterRemoveConnection := @WSAfterRemoveConnection;
	  serverWS.OnSocketError := @WSSocketError;
	  serverWS.OnTerminate := @WSTerminate;
	  serverWS.SSL := false;
	  serverWS.Start;
    runningWS := true;
  end
  else
  	WriteCon('WS server already running.');
end;

procedure TvtxApp.StartBridge;
begin
  // create bridge process to transmit stdin/stdout from client TProcesses
  if not runningBridge then
  begin
    WriteCon('IOBridge starting.');
	  bridgeWS := TvtxIOBridge.Create(true);
  	bridgeWS.OnProgress := @WriteCon;
	  bridgeWS.OnTerminate := @BridgeTerminate;
		bridgeWS.Start;
    runningBridge := true;
  end
  else
  	WriteCon('IOBridge is already running.');
end;

procedure TvtxApp.StopHTTP;
var
  i : integer;
  conn : TvtxWSConnection;

begin
  if runningHTTP then
  begin
  	WriteCon('HTTP server terminating.');
  	serverHTTP.Terminate;
    runningHTTP := false;
  end;
end;

procedure TvtxApp.StopWS;
var
  i : integer;
  conn : TvtxWSConnection;

begin
  if runningWS then
  begin
  	for i := 0 to serverWS.Count - 1 do
	  begin
			conn := TvtxWSConnection(serverWS.Connection[i]);
	    if (conn.ExtProcess <> nil) and conn.ExtProcess.Running then
  	    conn.ExtProcess.Terminate(0);
	  end;
  	WriteCon('WS server terminating.');
	  serverWS.TerminateThread;
    runningWS := false;
  end;
end;

procedure TvtxApp.StopBridge;
var
  i : integer;
  conn : TvtxWSConnection;

begin
  if runningBridge then
  begin
  	WriteCon('IOBridge server terminating.');
  	bridgeWS.Terminate;
    runningBridge := false;
  end;
end;

procedure TvtxApp.HTTPTerminate(Sender : TObject); register;
begin
	WriteCon('HTTP Terminated.');
end;

function GetServFromWords(word : TStringArray) : TServSet;
var
  i : integer;
begin
  result := [];
  for i := 1 to Length(word) - 1 do
  begin
		case upcase(word[i]) of
    	'HTTP':		result += [ HTTP ];
      'WS':			result += [ WS ];
      'BRIDGE':	result += [ Bridge ];
      'ALL' :		result += [ All ];
      else			result += [ Unknown ];
    end;
  end;
end;

var
	linein : 	string = '';
  key : 		char;
  keycode : integer;
  i : 			integer;
  word : 		TStringArray;
  serv :		TServSet;

{$R *.res}

begin
  write('VTX Server Console.' + CRLF
  		+ '(c) 2017 Dan Mecklenburg Jr.' + CRLF
      + CRLF
      + 'Type Help for commands, Quit to exit.' + CRLF + CRLF);

  app := 						TvtxApp.Create;
  runningHTTP := 		false;
  runningWS := 			false;
  runningBridge := 	false;
  Done := 					false;

  linein := '';
  repeat
    if wherex = 1 then
	    write(']');

    if KeyPressed then
    begin
			key := ReadKey;
      keycode := ord(key);
      if key = #0 then
      	keycode := -ord(ReadKey)
			else
      begin
        case key of

          #13: // enter
		        begin
            	write(CRLF);

              // parse command
              if linein.length > 0 then
              begin
	              word := linein.Split(' ');
			  				case upcase(word[0]) of
	  		      		'START':	// start a servive.
	                  {$region}
	                  begin
											serv := GetServFromWords(word);
	                    if Unknown in serv then
	                    	app.WriteCon('Unknown service specified.')
	                    else if serv = [] then
		                  	app.WriteCon('No service specified.')
	                    else
	                    begin
	                      if All in serv then
	                      begin
	    	                  app.StartHTTP;
	  	                    app.StartWS;
		                      app.StartBridge;
	                      end
	                      else
												begin
	                      	if HTTP in serv then
	                      		app.StartHTTP;
	                      	if WS in serv then
	                      		app.StartWS;
	                      	if Bridge in serv then
	                      		app.StartBridge;
												end;
	                    end;
	                  end;
	                	{$endregion}

			    		    'STOP':	// stop a service.
	                  {$region}
	                  begin
											serv := GetServFromWords(word);
	                    if Unknown in serv then
	                    	app.WriteCon('Unknown service specified.')
	                    else if serv = [] then
		                  	app.WriteCon('No service specified.')
	                    else
	                    begin
	                      if All in serv then
	                      begin
	    	                  app.StopHTTP;
	  	                    app.StopWS;
		                      app.StopBridge;
	                      end
	                      else
												begin
	                      	if HTTP in serv then
	                      		app.StopHTTP;
	                      	if WS in serv then
	                      		app.StopWS;
	                      	if Bridge in serv then
	                      		app.StopBridge;
												end;
	                    end;
	                  end;
	   	             	{$endregion}

                  'STATUS':
                  	begin
                      linein := '';
                    	if runningHTTP   	then linein += 'HTTP, ';
                    	if runningWS     	then linein += 'WS, ';
                    	if runningBridge 	then linein += 'Bridge, ';
											if linein = '' 		then linein += 'None, ';
                      linein := LeftStr(linein, linein.length - 2);
                      app.WriteCon('Currently running services: ' + linein);
                    end;

                  'QUIT':		Done := true;

									'HELP':
	                	{$region}
	                  begin
	                  	app.WriteCon('Commands: START serv [serv ..]  - Start one or more service.');
	                  	app.WriteCon('          STOP serv [serv ..]  - Stop one or more service');
                      app.WriteCon('          STATUS  - Display what''s running.');
	                  	app.WriteCon('          HELP  - You''re soaking in it.');
	                  	app.WriteCon('          QUIT  - Stop all services and exit.');
                      app.WriteCon('');
                      app.WriteCon('          serv = HTTP, WS, Bridge, or ALL');
	                  end
	                	{$endregion}
	                else			writeln('Unknown command.');
			  	    	end;
              end;
    		      write(']');
        		  linein := '';
            end;

          #8: // backspace
            begin
              if linein.length > 0 then
              begin
                write(#8' '#8);
                linein := LeftStr(linein, linein.length - 1);
              end;
            end;
          else
            begin
              write(key);
              linein += key;
            end;
        end
      end;
    end;

    CheckSynchronize;
  until Done;
	app.StopHTTP;
  app.StopWS;
  app.StopBridge;
  app.Free;
end.

