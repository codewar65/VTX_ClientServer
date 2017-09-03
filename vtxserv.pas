{

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
  2017-09-04
  vtxserv.pas

}

program vtxserv;

{ mini todo:
    client:
      hotspots
      zmodem xfer
      highlight / copy to clipboard
}

{$codepage utf8}
{$mode objfpc}{$H+}
{$apptype console}

{ $define DEBUG}

uses
  cmem,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
  {$ENDIF}{$ENDIF}

  // pascal / os stuff
  {$IFDEF WINDOWS}
    Windows, winsock2,
  {$ENDIF}
  Classes, Process, Pipes, DateUtils, SysUtils,
  IniFiles, Crt, LConvEncoding, Variants,

  // network stuff
  sockets, BlckSock, Synautil,
  WebSocket2, CustomServer2;

{$define BUFFSIZE:=16768}

{ *************************************************************************** }

Const
  Version = '0.95 beta';

{ TYPES }

{$region TYPES }
type

  TvtxWSConnection =  class;

  // status update prototype for threads
  TvtxProgressEvent = procedure(ip, msg: string) of object;

  { Node process type. }
  TvtxNodeType = ( ExtProc, Telnet );

  { TvtxSystemInfo : Board Inofo record }
  TvtxSystemInfo = record
    // [server] info
    SystemIP :        string;   // ip address of this host - needed to bind to socket.
    WSPort :          string;   // port number for ws/wss back end.
    WSSecure :        boolean;  // use wss instead of ws?
    MaxConnections :  integer;

    // [client] info
    NodeType :        TvtxNodeType;
    ExtProc :         string;   // string to launch node process
    TelnetIP :        string;   // connection info for internal telnet client
    TelnetPort :      string;
    CodePage :        string;   // CP437, PETSCII64, etc
  end;

  // available net services
  TServices = ( WS, All, Unknown );
  TServSet = set of TServices;

  { TvtxNodeProcess : thread for spawning connection console. terminate
    connection on exit }
  TvtxNodeProcess = class(TThread)
    private
      fProgress :   string;
      fProgressIP : string;
      fOnProgress : TvtxProgressEvent;
      procedure     DoProgress;

    protected
      procedure     Execute; override;
      procedure     PipeToConn;
      procedure     PipeToLocal;
      function      Negotiate(buf : array of byte; len : integer) : rawbytestring;
      function      SendBuffer(buf : pbyte; len : integer) : integer;

    public
      serverCon :   TvtxWSConnection;
      constructor   Create(CreateSuspended: boolean);
      destructor    Destroy; override;
      property      OnProgress: TvtxProgressEvent read fOnProgress write fOnProgress;
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

  { TvtxWSConnection : Websocket connection class. }
  TvtxWSConnection = class(TWebSocketServerConnection)
    public
      ExtProcType : TvtxNodeType;

      // needed for ExtProc
      ExtNode :     TvtxNodeProcess;  // the TThread that runs below ExtProcess
      ExtProc :     TProcess;         // the TProcess spawned board

      tnsock :      longint;
      tnserver :    sockaddr_in;
      tnbuf :       array [0..BUFFSIZE] of byte;
      tnlive :      boolean;
      tnstate :     integer;          // telnet read state (IAC cmds)
      tncmd :       byte;             // current telnet command
      tnqus :       array [0..255] of byte;
      tnqhim :      array [0..255] of byte;

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

  { TvtxApp : Main application class }
  TvtxApp = class
    procedure StartWS;
    procedure StopWS;
    procedure WriteCon(ip, msg : string); register;
    procedure WSBeforeAddConnection(Server : TCustomServer; aConnection : TCustomConnection;var CanAdd : boolean); register;
    procedure WSAfterAddConnection(Server : TCustomServer; aConnection : TCustomConnection); register;
    procedure WSBeforeRemoveConnection(Server : TCustomServer; aConnection : TCustomConnection); register;
    procedure WSAfterRemoveConnection(Server : TCustomServer; aConnection : TCustomConnection); register;
    procedure WSSocketError(Server: TCustomServer; Socket: TTCPBlockSocket); register;
    procedure WSOpen(aSender: TWebSocketCustomConnection);
    procedure WSRead(aSender : TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3 : boolean; aCode :integer; aData :TMemoryStream);
    procedure WSWrite(aSender : TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3 : boolean; aCode :integer; aData :TMemoryStream);
    procedure WSClose(aSender: TWebSocketCustomConnection; aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure WSTerminate(Sender : TObject); register;
    procedure CloseAllNodes;
    procedure CloseNode(n : integer);
    procedure NodeTerminate(Sender: TObject);
  end;
{$endregion}

{ *************************************************************************** }
{ CONSTANTS }
{$region CONSTANTS}
const
  // names of net services
  ProcessType : array [0..1] of string = ('ExtProc', 'Telnet' );

  CRLF = #13#10;

{$endregion}

{ *************************************************************************** }
{ GLOBALS }
{$region GLOBALS}
var
  app :           TvtxApp;        // main app class
  SystemInfo :    TvtxSystemInfo; // system about this vtx
  lastaction :    TDateTime;      // last time activity for hibernation
  serverWS :      TvtxWSServer;   // ws server.
  runningWS :     boolean;
  cmdbuff :       string = '';    // console linein buffer
  logout :        Text;           // server logfile.
{$endregion}

{ *************************************************************************** }
{ SUPPORT PROCEDURES / FUNCTIONS }
{$region SUPPORT ROUTINES}

function InList(str : string; list : array of string) : integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to length(list) - 1 do
    if upCase(str) = upCase(list[i]) then
    begin
      result := i;
      break;
    end;
end;

function GetIP(strName : string) : string;
var
  phe : pHostEnt;
begin
  //Convert the name to a cstring since 'gethostbyname' expects a cstring
  phe := gethostbyname(pchar(strName));
  if phe = nil then
    result := '0.0.0.0'
  else
  begin
    result := format('%d.%d.%d.%d', [
      byte(phe^.h_addr^[0]),
      byte(phe^.h_addr^[1]),
      byte(phe^.h_addr^[2]),
      byte(phe^.h_addr^[3])]);
  end;
 end;

{ read the vtxserv.ini file for settings }
procedure LoadSettings;
var
  iin :         TIniFile;

const
  server = 'Server';
  client = 'Client';

begin
  iin := TIniFile.Create('vtxserv.ini');

  // [server] info
  SystemInfo.SystemIP :=    iin.ReadString(server, 'SystemIP',    'localhost');
  SystemInfo.WSPort :=      iin.ReadString(server, 'WSPort',      '7003');
  SystemInfo.WSSecure :=    iin.ReadBool(server,   'WSSecure',    false);
  SystemInfo.MaxConnections := iin.ReadInteger(server, 'MaxConnections',  32);
  SystemInfo.NodeType :=    TvtxNodeType(InList(
                              iin.ReadString(server, 'NodeType',  'ExtProc'),
                              ProcessType));
  // [client] info
  SystemInfo.ExtProc :=     iin.ReadString(client, 'ExtProc',     'vtxtest.exe @UserIP@');
  SystemInfo.TelnetIP :=    iin.ReadString(client, 'TelnetIP',    'localhost');
  SystemInfo.TelnetPort :=  iin.ReadString(client, 'TelnetPort',  '7002');

  SystemInfo.CodePage :=    iin.ReadString(client, 'CodePage',    'UTF8');

  // resolve possible named IPs to IP addresses.
  SystemInfo.SystemIP := GetIP(SystemInfo.SystemIP);
  SystemInfo.TelnetIP := GetIP(SystemInfo.TelnetIP);

  iin.Free;
end;

{ get services associated with words in list 1 .. end }
function GetServFromWords(word : TStringArray) : TServSet;
var
  i : integer;

begin
  result := [];
  for i := 1 to Length(word) - 1 do
  begin
    case upcase(word[i]) of
      'WS':     result += [ WS ];
      'ALL' :   result += [ All ];
      else      result += [ Unknown ];
    end;
  end;
end;

{ read a console line, returns '' if none entered }
function ConsoleLineIn : string;
var
  key :     char;

begin
  result := '';
  if wherex = 1 then
    write(']' + cmdbuff);
  if keypressed then
  begin
    lastaction := now;
    key := readkey;
    if key <> #0 then
    begin
      case key of
        #13:
          begin
            write(CRLF);
            result := cmdbuff;
            cmdbuff := '';
          end;

        #8: // backspace
          begin
            if length(cmdbuff) > 0 then
            begin
              write(#8' '#8);
              cmdbuff := LeftStr(cmdbuff, cmdbuff.length - 1);
            end;
          end;
        else
          begin
            write(key);
            cmdbuff += key;
          end;
      end;
    end
    else
    begin
      // special key
      key := readkey;
      case ord(key) of
        $4B:  // left arrow
          begin
            if length(cmdbuff) > 0 then
            begin
              write(#8' '#8);
              cmdbuff := LeftStr(cmdbuff, cmdbuff.length - 1);
            end;
          end;
        else  beep;
      end;
    end;
  end;
end;

{ replace @codes@ with system values }
function ReplaceAtCodes(str : string) : string;
begin
  str := str.Replace('@SystemIP@', SystemInfo.SystemIP);
  str := str.Replace('@TelnetIP@', SystemInfo.TelnetIP);
  str := str.Replace('@TelnetPort@', SystemInfo.TelnetPort);
  str := str.Replace('@WSPort@', SystemInfo.WSPort);
  str := str.Replace('@CodePage@', SystemInfo.CodePage);
  result := str;
end;

{$endregion}

{ *************************************************************************** }
{ TvtxNodeProcess }

// copy input to websocket
procedure TvtxNodeProcess.PipeToConn;
  var
    i, bytes : integer;
    b : byte;
    str : ansistring;
begin

  str := '';
  try
    bytes := serverCon.ExtProc.Output.NumBytesAvailable;
  except
    bytes := 0;
  end;

  if bytes > 0 then
  begin
    lastaction := now;
    for i := 0 to bytes - 1 do
    begin
      try
        b := serverCon.ExtProc.Output.ReadByte;
      finally
        str += char(b);
      end;
    end;
    serverCon.SendBinary(TStringStream.Create(str));
  end;
end;

// copy input to console
procedure TvtxNodeProcess.PipeToLocal;
var
  i, bytes : integer;
  b : byte;
  str : ansistring;

begin
  try
    bytes := serverCon.ExtProc.StdErr.NumBytesAvailable;
  except
    bytes := 0;
  end;

  str := '';
  if bytes > 0 then
  begin
    lastaction := now;
    for i := 0 to bytes - 1 do
    begin
      try
        b := serverCon.ExtProc.StdErr.ReadByte;
      finally
        str += char(b);
      end;
    end;
    fProgress := str;
    Synchronize(@DoProgress);
  end;
end;

// thread launched that launches tprocess and waits for it to terminate.
// closes clients connection at end.
constructor TvtxNodeProcess.Create(CreateSuspended: boolean);
begin
  fProgressIP := '';
  fProgress := '';
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

destructor TvtxNodeProcess.Destroy;
begin
  inherited Destroy;
end;

procedure TvtxNodeProcess.DoProgress;
begin
  if Assigned(FOnProgress) then
  begin
    FOnProgress(fProgressIP, fProgress);
  end;
end;

// send data to node process
function TvtxNodeProcess.SendBuffer(buf : pbyte; len : longint) : integer;
begin
  result := -1;
  case SystemInfo.NodeType of

    ExtProc:
      begin
        if (serverCon.ExtProc <> nil) and serverCon.ExtProc.Running then
          result := serverCon.ExtProc.Input.Write(buf[0], len);
      end;

    Telnet:
      begin
        if serverCon.tnlive then
        begin
          result := fpsend(serverCon.tnsock, buf, len, 0);
        end;
      end;
  end;
end;

// process str for telnet commands, return string with commands stripped.
function TvtxNodeProcess.Negotiate(
    buf : array of byte;
    len : integer) : rawbytestring;
var
  strout : rawbytestring;
  b :       byte;
  i :       integer;

begin
  strout := '';
  for i := 0 to len - 1 do
  begin
    b := byte(buf[i]);
    strout := strout + char(b);
  end;
  result := strout;
end;

// node process thread. launch extproc process or do a telnet session.
procedure TvtxNodeProcess.Execute;
var
  parms :   TStringArray;
  i :       integer;
  he :      PHostEnt;
  status :  integer;
  rv :      integer;
  linger :  TLinger;
  str :     rawbytestring;
  mode :    longint;

begin
  // for each connect that has process, send input, read output
  fProgressIP := serverCon.Socket.GetRemoteSinIP;
  if serverWS <> nil then
  begin
    case SystemInfo.NodeType of

      ExtProc:
        begin
          // execute a spawned node session.
          fProgress := 'Spawning process.';
          Synchronize(@DoProgress);

          parms := SystemInfo.ExtProc.Split(' ');
          for i := 0 to length(parms) - 1 do
            parms[i] := ReplaceAtCodes(parms[i]);
          parms[i] := parms[i].Replace('@UserIP@', serverCon.Socket.GetRemoteSinIP);

          serverCon.ExtProc := TProcess.Create(nil);
          serverCon.ExtProc.CurrentDirectory:= 'node';
          serverCon.ExtProc.FreeOnRelease;
          serverCon.ExtProc.Executable := 'node\' + parms[0];
          for i := 1 to length(parms) - 1 do
            serverCon.ExtProc.Parameters.Add(parms[i]);

          serverCon.ExtProc.Options := [
              //poWaitOnExit,
              poUsePipes,
              poNoConsole,
              poDefaultErrorMode,
              poNewProcessGroup
            ];

          // go run. wait on exit.
          try
            serverCon.ExtProc.Execute;

            while not serverCon.IsTerminated and serverCon.ExtProc.Running do
            begin
              // pipe strout to websocket.
              if serverCon.ExtProc.Output <> nil then
                PipeToConn;

              // pipe strerr to local console.
              if serverCon.ExtProc.Stderr <> nil then
                PipeToLocal;

            end;

          except
            fProgress := '** Error on Node Process. ExtProc type.';
            Synchronize(@DoProgress);
          end;
        end;

      Telnet:
        begin
          // execute a telnet node session.

          // create socket
          serverCon.tnlive := false;
          serverCon.tnsock := fpsocket(AF_INET, SOCK_STREAM, 0);
          if serverCon.tnsock = -1 then
          begin
            fProgress := 'Unable to create telnet client socket.';
            Synchronize(@DoProgress);
            exit;
          end;

          // build address
          ZeroMemory(@serverCon.tnserver, sizeof(sizeof(TSockAddrIn)));
          he := gethostbyname(@(SystemInfo.TelnetIP[1]));
          if he = nil then
          begin
            fProgress := 'Unable to resolve telnet address.';
            Synchronize(@DoProgress);
            exit;
          end;
          serverCon.tnserver.sin_addr.S_addr := inet_addr(he^.h_name);
          serverCon.tnserver.sin_family := AF_INET;
          serverCon.tnserver.sin_port := htons(StrToInt(SystemInfo.TelnetPort));

          // connect
          status := fpconnect(
                      serverCon.tnsock,
                      @serverCon.tnserver,
                      sizeof(TSockAddrIn));
          if status < 0 then
          begin
            fProgress := 'Unable to connect to telnet server.';
            Synchronize(@DoProgress);
            exit;
          end;
          serverCon.tnlive := true;

          // linger set to 1 sec.
          linger.l_linger := 1;
          linger.l_onoff := 1;
          setsockopt(
            serverCon.tnsock,
            SOL_SOCKET,
            SO_LINGER,
            @linger,
            sizeof(TLinger));

          // set non-blocking mode
          mode := 1;
          ioctlsocket(serverCon.tnsock, longint(FIONBIO), @mode);

          // connected!!! loop
          while true do
          begin

            if serverCon.tnsock = -1 then
            begin
              fProgress := 'Invalid socket.';
              Synchronize(@DoProgress);
              break;
            end;

            // stuff to read
            rv := fprecv(serverCon.tnsock, @serverCon.tnbuf, BUFFSIZE, 0);
            if rv = 0 then
            begin
              fProgress := 'Telnet closed by remote.';
              Synchronize(@DoProgress);
              break;
            end
            else if rv < 0 then
            begin

              // if not wouldblock, disconnect
              if WSAGetLastError <> WSAEWOULDBLOCK then
              begin
                fProgress := 'Telnet closed by remote.';
                Synchronize(@DoProgress);
                break;
              end;

            end;

            if not serverCon.Closed then
            begin
              if rv > 0 then
              begin
                // handle telnet negotiations
                str := negotiate(serverCon.tnbuf, rv);
                if length(str) > 0 then
                begin
                  serverCon.SendBinary(TStringStream.Create(str));
                end;
              end
            end
            else
            begin
              fProgress := 'Websocket closed during telnet.';
              Synchronize(@DoProgress);
              break;
            end;
          end;

          fpshutdown(serverCon.tnsock, 2);
          serverCon.tnlive := false;
          fProgress := 'Closed socket.';
          Synchronize(@DoProgress);
        end;
    end;

    // disconnect afterwards.
    if not serverCon.Closed then
      serverCon.Close(wsCloseNormal, 'Good bye');
  end;

  fProgress := 'Node Terminating.';
  Synchronize(@DoProgress);
end;


{ *************************************************************************** }
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


{ *************************************************************************** }
{ TvtxApp - main application stuffz }

procedure TvtxApp.NodeTerminate(Sender: TObject);
begin
end;

procedure TvtxApp.WSBeforeAddConnection(
      Server :      TCustomServer;
      aConnection : TCustomConnection;
  var CanAdd :      boolean); register;
begin
end;

procedure TvtxApp.WSAfterAddConnection(
      Server : TCustomServer;
      aConnection : TCustomConnection); register;
var
  con : TvtxWSConnection;
begin
  con := TvtxWSConnection(aConnection);

  con.OnOpen :=  @WSOpen;
  con.OnRead :=  @WSRead;
  con.OnWrite := @WSWrite;
  con.OnClose := @WSClose;

  // spawn a new process for this connection
  con.ExtNode := TvtxNodeProcess.Create(true);
  con.ExtNode.fProgressIP := aConnection.Socket.GetRemoteSinIP;
  con.ExtNode.serverCon := con;
  con.ExtNode.OnProgress := @WriteCon;
  con.ExtNode.OnTerminate := @NodeTerminate;
  try
    con.ExtNode.Start;
  except
    WriteCon('', '** Error on Node Create.');
  end;

end;

procedure TvtxApp.WSOpen(aSender: TWebSocketCustomConnection);
begin
  lastaction := now;
  WriteCon(aSender.Socket.GetRemoteSinIP, 'Open WS Connection.');
end;

// send wbsocket incoming data to process
procedure TvtxApp.WSRead(
      aSender : TWebSocketCustomConnection;
      aFinal,
      aRes1,
      aRes2,
      aRes3 :   boolean;
      aCode :   integer;
      aData :   TMemoryStream);
var
  bytes : longint;
  con :   TvtxWSConnection;
  buf :   array [0..2049] of byte;

//const
//  hsresp : string = #27'[?50;86;84;88c';

begin
  lastaction := now;
  //WriteCon('Read WS Connection.');

  con := TvtxWSConnection(aSender);

  bytes := aData.Size;
  if bytes > 2048 then
    bytes := 2048;

  if bytes > 0 then
  begin
    aData.ReadBuffer(buf[0], bytes);

    if con.ExtNode.SendBuffer(@buf, bytes) < 0 then
    begin
//      app.WriteCon(aSender.Socket.GetRemoteSinIP, 'Error sending to node process.');
//      con.Close(wsCloseNormal, 'Good bye');
    end;
  end;
end;

procedure TvtxApp.WSWrite(
      aSender : TWebSocketCustomConnection;
      aFinal,
      aRes1,
      aRes2,
      aRes3 :   boolean;
      aCode :   integer;
      aData :   TMemoryStream);
begin
  lastaction := now;
end;

procedure TvtxApp.WSClose(
      aSender: TWebSocketCustomConnection;
      aCloseCode: integer;
      aCloseReason: string;
      aClosedByPeer: boolean);
begin
  WriteCon(aSender.Socket.GetRemoteSinIP, 'Close WS Connection.');
end;

procedure TvtxApp.WSBeforeRemoveConnection(
      Server :      TCustomServer;
      aConnection : TCustomConnection); register;
var
  con : TvtxWSConnection;
begin
  con := TvtxWSConnection(aConnection);

  case SystemInfo.NodeType of

    ExtProc:
      begin
        if (con.ExtProc <> nil) and con.ExtProc.Running then
        begin
          WriteCon(con.Socket.GetRemoteSinIP, 'Force Terminate Node Processes');
          try
            con.ExtProc.Terminate(0);
          except
            WriteCon('', '** Error terminating node process.');
          end;
        end;
      end;

    Telnet:
      if con.tnlive then
      begin
        fpshutdown(con.tnsock, 4);
        con.tnlive := false;
      end;

  end;
end;

procedure TvtxApp.WSAfterRemoveConnection(
      Server : TCustomServer;
      aConnection : TCustomConnection); register;
begin
end;

procedure TvtxApp.WSSocketError(
      Server: TCustomServer;
      Socket: TTCPBlockSocket); register;
begin
  WriteCon(Socket.GetRemoteSinIP, 'WS Error : ' + Socket.LastErrorDesc + '.');
end;

procedure TvtxApp.WSTerminate(Sender : TObject); register;
begin
  WriteCon('', 'WS server Terminated.');
end;

procedure TvtxApp.WriteCon(ip, msg : string); register;
var
  ips : array of string;
begin
  write(#13);

  TextColor(LightCyan);
  write(#13);
  Write(
    format('%4.2d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d ',
    [
      YearOf(now),    // year
      MonthOf(now),   // month
      DayOf(now),     // day
      HourOf(now),    // 24hr
      MinuteOf(now),  // min
      SecondOf(Now) // sec
    ]));

  if ip <> '' then
  begin
    ips := ip.split('.');
    TextColor(Yellow);
    Write(
      format('(%3.3d.%3.3d.%3.3d.%3.3d) ',
      [
        strtoint(ips[0]),
        strtoint(ips[1]),
        strtoint(ips[2]),
        strtoint(ips[3])
      ]));
  end;

  TextColor(LightGreen);
  writeln(msg);
end;

procedure TvtxApp.StartWS;
begin
  // create websocket server
  if not runningWS then
  begin
    WriteCon('', format('WS server starting on port %s.', [SystemInfo.WSPort]));
    serverWS := TvtxWSServer.Create(SystemInfo.SystemIP, SystemInfo.WSPort);
    serverWS.FreeOnTerminate := true;
    serverWS.MaxConnectionsCount := SystemInfo.MaxConnections;
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
    WriteCon('', 'WS server already running.');
end;

{ Call CloseAllConnections prior to calling. }
procedure TvtxApp.StopWS;
begin
  if runningWS then
  begin
    WriteCon('', 'WS server terminating.');
    if serverWS <> nil then
      serverWS.Terminate;
    runningWS := false;
  end;
end;

{ Terminate all end all client node processes, close all connections }
procedure TvtxApp.CloseAllNodes;
var
  i : integer;
begin
  // terminate all node processes.
  if runningWS then
  begin
    i := serverWS.Count - 1;
    while i >= 0 do
    begin
      CloseNode(i);
      dec(i);
    end;
  end;
end;

procedure TvtxApp.CloseNode(n : integer);
var
  con : TvtxWSConnection;
begin
  if not serverWS.Connection[n].Finished then
  begin
    con := TvtxWSConnection(serverWS.Connection[n]);
    case SystemInfo.NodeType of

      ExtProc:
        begin
          if (con.ExtProc <> nil) and con.ExtProc.Running then
            try
              con.ExtProc.Terminate(1);
            finally
              WriteCon('', 'Node Process terminated.');
            end;
        end;

      Telnet:
        if con.tnlive then
        begin
          fpshutdown(con.tnsock, 4);
          con.tnlive := false;
        end;

    end;
  end;
end;

{$R *.res}

{$region Main}
var
  linein :      string;
  word :        TStringArray;
  serv :        TServSet;
  Done :        boolean;
  Hybernate :   boolean;
  i :           integer;
  count :       integer;
  ThreadRan :   boolean;
  WsaData :     TWSAData;
  t2 :          boolean;

const
  WSAVersion : integer = $0202;

begin

  {$IFDEF WINDOWS}
    SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}

  assign(logout, 'vtxserv.log');
  if FileExists('vtxserv.log') then
    append(logout)
  else
    rewrite(logout);

  TextColor(LightGreen);
  write('VTX WS Server Console.' + CRLF
      + 'Version ' + Version + CRLF
      + '2017 Dan Mecklenburg Jr.' + CRLF
      + CRLF
      + 'Type HELP for commands, QUIT to exit.' + CRLF + CRLF);

  app :=            TvtxApp.Create;
  serverWS :=       nil;
  runningWS :=      false;
  Done :=           false;
  Hybernate :=      false;

  LoadSettings;

  if SystemInfo.NodeType = Telnet then
    WSAStartup(WSAVersion, WsaData);

  lastaction := now;

  linein := '';
  repeat
    linein := ConsoleLineIn;
    if linein <> '' then
    begin
      // parse command
        word := linein.Split(' ');
        case upcase(word[0]) of
        'START':  // start a servive.
            begin
              serv := GetServFromWords(word);
              if Unknown in serv then
                app.WriteCon('', 'Unknown service specified.')
              else if serv = [] then
                app.WriteCon('', 'No service specified.')
              else
              begin
                if All in serv then
                begin
                  app.StartWS;
                end
                else
                begin
                  if WS in serv then
                    app.StartWS;
                end;
              end;
            end;

        'STOP': // stop a service.
            begin
              serv := GetServFromWords(word);
              if Unknown in serv then
                app.WriteCon('', 'Unknown service specified.')
              else if serv = [] then
                app.WriteCon('', 'No service specified.')
              else
              begin
                if All in serv then
                begin
                  app.CloseAllNodes();
                  app.StopWS;
                end
                else
                begin
                  if WS in serv then
                  begin
                    app.CloseAllNodes();
                    app.StopWS;
                  end;
                end;
              end;
            end;

        'STATUS':
          begin
            linein := '';
            if runningWS      then linein += 'WS, ';
            if linein = ''    then linein += 'None, ';
            linein := LeftStr(linein, linein.length - 2);
            app.WriteCon('', 'Currently running services: ' + linein);
            if runningWS then
             app.WriteCon('', 'Current WS connections: ' + inttostr(serverWS.Count));
          end;

        'QUIT':   Done := true;

        'CLS':    ClrScr;

        'LIST':   // list connection
          begin
            if runningWS then
            begin
              count := 0;
              for i := 0 to serverWS.Count - 1 do
              begin
                if not serverWS.Connection[i].IsTerminated then
                begin
                  app.WriteCon('', format('%d) %s', [
                    i + 1,
                    serverWS.Connection[i].Socket.GetRemoteSinIP ]));
                  inc(count);
                end;
              end;
              if count = 0 then
                app.WriteCon('', 'No active connections.');
            end
            else
              app.WriteCon('', 'WS service is not running.');
          end;

        'KICK':   // kick a connection
          begin
            if runningWS then
            begin
              if length(word) = 2 then
              begin
                i := strtoint(word[1]) - 1;
                if (i >= 0) and (i < serverWS.Count) then
                begin
                  if not serverWS.Connection[i].IsTerminated then
                  begin
                    app.CloseNode(i);
                  end
                  else
                  begin
                    app.WriteCon('', 'Connection already closed..');
                  end;
                end
                else
                  app.WriteCon('', 'No such connection.');
              end
              else
                app.WriteCon('', 'Specify a connection number. (See LIST)');
            end
            else
              app.WriteCon('', 'WS service is not running.');
          end;

        'LOADCFG':
          begin
            t2 := runningWS;
            if runningWS then
            begin
              app.CloseAllNodes();
              app.StopWS;
              while not serverWS.Finished do;
            end;
            LoadSettings;
            sleep(2000);
            if t2 then
              app.StartWS;
          end;

        'HELP':
          begin
            app.WriteCon('', 'Commands: START <serv> [<serv> ..]  - Start one or more service.');
            app.WriteCon('', '          STOP <serv> [<serv> ..]  - Stop one or more service.');
            app.WriteCon('', '          STATUS  - Display what''s running.');
            app.WriteCon('', '          LIST  - List current WS connections.');
            app.WriteCon('', '          KICK <connum>  - Disconnect a WS connection.');
            app.WriteCon('', '          CLS  - Clear console screen.');
            app.WriteCon('', '          HELP  - You''re soaking in it.');
            app.WriteCon('', '          QUIT  - Stop all services and exit.');
            app.WriteCon('', '          LOADCFG - force reload config.');
            app.WriteCon('', '');
            app.WriteCon('', '          serv = WS or ALL');
          end
          else
            app.WriteCon('', 'Unknown command.');
      end;
    end;

    // tickle the threads.
    ThreadRan := CheckSynchronize;
    if ThreadRan then
    begin
      app.WriteCon('', 'A thread ran.');
    end;

    // hybernate mode?
    if SecondsBetween(now, lastaction) > 120 then
    begin
      if not Hybernate then
        app.WriteCon('', 'Hybernating.... zzz...');

      Hybernate := true;
      sleep(100);
    end
    else
    begin
      if Hybernate then
        app.WriteCon('', 'Waking up! O.O');

      Hybernate := false;
    end;

  until Done;

  app.CloseAllNodes;
  app.StopWS;
  sleep(2000);

  if SystemInfo.NodeType = Telnet then
    WSACleanup;

  closefile(logout);
end.
{$endregion}

