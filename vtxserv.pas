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
  2017-07-20
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

{$define DEBUG}

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
{ TYPES }
{$region TYPES }
type

  TvtxWSConnection =  class;

  // status update prototype for threads
  TvtxProgressEvent = procedure(ip, msg: string) of object;

  // available code pages to translate to / from.
  TCodePages = (
		CP437, CP667, CP668, CP737, CP770, CP771, CP772, CP773,
    CP774, CP775, CP790, CP808, CP813, CP850, CP851, CP852,
    CP853, CP855, CP857, CP858, CP859, CP860, CP861, CP863,
    CP865, CP866, CP867, CP869, CP872, CP878, CP895, CP900,
    CP912, CP915, CP920, CP991, CP1117, CP1118, CP1119, CP65001,
    CPMIK, UTF8, UTF16, WIN1250, WIN1251, WIN1253, WIN1254, WIN1257
	);

  // code page converter prototypes
//  TCodePageConverter =    function(const s: string): string;
//  TCodePageUnconverter =  function(const s: string; SetTargetCodePage: boolean = false): RawByteString;

  { Node process type. }
  TvtxNodeType = ( ExtProc, Telnet );

  { TvtxSystemInfo : Board Inofo record }
  TvtxSystemInfo = record
    SystemName :      string;   // name of bbs - webpage title.
    SystemIP :        string;   // ip address of this host - needed to bind to socket.
    InternetIP :      string;   // ip address as seen from internet.
    HTTPPort :        string;   // port number for http front end.
    WSPort :          string;   // port number for ws back end.
    MaxConnections :  integer;
    Columns :					integer;	// columns for term console.
    XScale :					real;			// scale everything by this on the X axis.
    Terminal :				string;		// ANSI, PETSCII

    NodeType :        TvtxNodeType;
    CodePage :				TCodePages;

    // string to launch node preocess
    ExtProc :         string;

    // connection info for internal telnet client
    TelnetIP :        string;
    TelnetPort :      string;
  end;

  // available net services
  TServices = ( HTTP, WS, All, Unknown );
  TServSet = set of TServices;

  { TvtxNodeProcess : thread for spawning connection console. terminate
    connection on exit }
  TvtxNodeProcess = class(TThread)
    private
      fProgress :   string;
      fOnProgress : TvtxProgressEvent;
      procedure     DoProgress;

    protected
      procedure     Execute; override;
      procedure     PipeToConn;
      procedure     PipeToLocal;
      function      Negotiate(buf : array of byte; len : integer) : rawbytestring;
      function      SendBuffer(buf : pbyte; len : integer) : integer;
      function      SendCmd(stuff : array of Variant) : integer;

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
    procedure StartHTTP;
    procedure StartWS;
    procedure StopHTTP;
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
    procedure HTTPTerminate(Sender : TObject); register;
  end;

  { TvtxHTTPServer : for basic webserver front end for dishing out client }
  TvtxHTTPServer = class(TThread)
    private
      fProgress :   string;
      fOnProgress : TvtxProgressEvent;
      procedure     DoProgress;

    protected
      procedure     Execute; override;
      procedure     AttendConnection(ASocket : TTCPBlockSocket);
      function      SendText(
                      ASocket : TTCPBlockSocket;
                      ContentType : string;
                      Filename : string) : integer;
      function      SendBinary(
                      ASocket : TTCPBlockSocket;
                      ContentType : string;
                      Filename : string) : integer;

    public
      constructor   Create(CreateSuspended: boolean);
      destructor    Destroy; override;
      property      OnProgress : TvtxProgressEvent
                      read fOnProgress
                      write fOnProgress;
  end;
{$endregion}

{ *************************************************************************** }
{ CONSTANTS }
{$region CONSTANTS}
const
  // code page stuff
  FirstCP : TCodePages = CP437;
  LastCP : 	TCodePages = WIN1257;

  CodePageNames : array [ CP437 .. WIN1257 ] of string = (
		'CP437', 'CP667', 'CP668', 'CP737', 'CP770', 'CP771', 'CP772', 'CP773',
    'CP774', 'CP775', 'CP790', 'CP808', 'CP813', 'CP850', 'CP851', 'CP852',
    'CP853', 'CP855', 'CP857', 'CP858', 'CP859', 'CP860', 'CP861', 'CP863',
    'CP865', 'CP866', 'CP867', 'CP869', 'CP872', 'CP878', 'CP895', 'CP900',
    'CP912', 'CP915', 'CP920', 'CP991', 'CP1117', 'CP1118', 'CP1119', 'CP65001',
  	'CPMIK', 'UTF8', 'UTF16', 'WIN1250', 'WIN1251', 'WIN1253', 'WIN1254', 'WIN1257'
);

  // names of net services
  ProcessType : array [0..1] of string = ('ExtProc', 'Telnet' );

  CRLF = #13#10;

  // telnet commands
  TN_IS           = $00;
  TN_SEND         = $01;

  TN_BIN          = $00;
  TN_ECHO         = $01;
  TN_RECONNECT    = $02;
  TN_SGA          = $03;
  TN_STATUS       = $05;
  TN_TMARK        = $06;
  TN_SLOC         = $17;
  TN_TTYPE        = $18;
  TN_NAWS         = $1F;
  TN_TSPEED       = $20;
  TN_RFC          = $21;
  TN_LM           = $22;
  TN_XLOC         = $23;
  TN_EVARS        = $24;
  TN_AUTH         = $25;
  TN_NEWE         = $27;
  TN_SE           = $F0;
  TN_NOP          = $F1;
  TN_DM           = $F2;
  TN_BRK          = $F3;
  TN_IP           = $F4;
  TN_AO           = $F5;
  TN_AYT          = $F6;
  TN_EC           = $F7;
  TN_EL           = $F8;
  TN_GA           = $F9;
  TN_SB           = $FA;
  TN_WILL         = $FB;
  TN_WONT         = $FC;
  TN_DO           = $FD;
  TN_DONT         = $FE;
  TN_IAC          = $FF;

  TnCmds : array [0..255] of string = (
  	'BIN/IS', 'ECHO/SEND','RECONNECT','SGA','MSGSZ','STATUS','TMARK','RECHO',	// 0-7
    'LWID','PAGSZ','CRDIS','HRZTAB','HRZDIS','FFDIS','VRTTAB','VRTDIS',	// 8-15
    'LFDIS','XASCII','LOGOUT,','BYTEMACRO','DET','SUPDUP','SUPDUPOUT','SLOC', // 16-23
    'TTYPE','EOR','USERIDENT','OMARK','TLOCNUM','TN3270','X3PAD','NAWS', // 24-31
    'TSPEED','RFC','LM','XLOC','EVARS','AUTH','ENCRY','NEWE', // 32-39
		'TN3270E','XAUTH','CHARSET','RSP','CPCOPT','SUPLECHO','STARTTLS','KERMIT', // 40-47
    'SENDURL','FORWARDX','32','33','34','35','36','37', // 48-
    '38','39','3A','3B','3C','3D','3E','3F',
		'40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
		'50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
		'60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
		'70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
		'80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
		'90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
		'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
		'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
		'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
		'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
		'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
		'SE','NOP','DM','BRK','IP','AO','AYT','EC','EL','GA','SB','WILL','WONT','DO','DONT','IAC'
	);

  // TELNET states for Q method
  // option is enabled ONLY IF state is TNS_YES
  TNQ_NO          = 0;
  TNQ_YES         = 1;
  TNQ_WANTNO      = 2;
  TNQ_WANTYES     = 3;
  TNQ_WANTNO_OP   = 4;
  TNQ_WANTYES_OP  = 5;
{$endregion}

{ *************************************************************************** }
{ GLOBALS }
{$region GLOBALS}
var
  app :           TvtxApp;				// main app class
  SystemInfo :    TvtxSystemInfo;	// system about this vtx
  lastaction :    TDateTime;      // last time activity for hibernation
  serverWS :      TvtxWSServer;   // ws server.
  serverHTTP :    TvtxHTTPServer; // http srever.
  runningWS :     boolean;
  runningHTTP :   boolean;
  cmdbuff :       string = '';    // console linein buffer
  logout :				Text;						// server logfile.
{$endregion}

{ *************************************************************************** }
{ SUPPORT PROCEDURES / FUNCTIONS }
{$region SUPPORT ROUTINES}

{ Get a socket error description. }
function GetSocketErrorMsg(errno : integer) : string;
begin
  result := 'Unknown error.';
  case errno of
    6:  result := 'Specified event object handle is invalid.';
    8:  result := 'Insufficient memory available.';
    87: result := 'One or more parameters are invalid.';
    995:  result := 'Overlapped operation aborted.';
    996:  result := 'Overlapped I/O event object not in signaled state.';
    997:  result := 'Overlapped operations will complete later.';
    10004:  result := 'Interrupted function call.';
    10009:  result := 'File handle is not valid.';
    10013:  result := 'Permission denied.';
    10014:  result := 'Bad address.';
    10022:  result := 'Invalid argument.';
    10024:  result := 'Too many open files.';
    10035:  result := 'Resource temporarily unavailable.';
    10036:  result := 'Operation now in progress.';
    10037:  result := 'Operation already in progress.';
    10038:  result := 'Socket operation on nonsocket.';
    10039:  result := 'Destination address required.';
    10040:  result := 'Message too long.';
    10041:  result := 'Protocol wrong type for socket.';
    10042:  result := 'Bad protocol option.';
    10043:  result := 'Protocol not supported.';
    10044:  result := 'Socket type not supported.';
    10045:  result := 'Operation not supported.';
    10046:  result := 'Protocol family not supported.';
    10047:  result := 'Address family not supported by protocol family.';
    10048:  result := 'Address already in use.';
    10049:  result := 'Cannot assign requested address.';
    10050:  result := 'Network is down.';
    10051:  result := 'Network is unreachable.';
    10052:  result := 'Network dropped connection on reset.';
    10053:  result := 'Software caused connection abort.';
    10054:  result := 'Connection reset by peer.';
    10055:  result := 'No buffer space available.';
    10056:  result := 'Socket is already connected.';
    10057:  result := 'Socket is not connected.';
    10058:  result := 'Cannot send after socket shutdown.';
    10059:  result := 'Too many references.';
    10060:  result := 'Connection timed out.';
    10061:  result := 'Connection refused.';
    10062:  result := 'Cannot translate name.';
    10063:  result := 'Name too long.';
    10064:  result := 'Host is down.';
    10065:  result := 'No route to host.';
    10066:  result := 'Directory not empty.';
    10067:  result := 'Too many processes.';
    10068:  result := 'User quota exceeded.';
    10069:  result := 'Disk quota exceeded.';
    10070:  result := 'Stale file handle reference.';
    10071:  result := 'Item is remote.';
    10091:  result := 'Network subsystem is unavailable.';
    10092:  result := 'Winsock.dll version out of range.';
    10093:  result := 'Successful WSAStartup not yet performed.';
    10101:  result := 'Graceful shutdown in progress.';
    10102:  result := 'No more results.';
    10103:  result := 'Call has been canceled.';
    10104:  result := 'Procedure call table is invalid.';
    10105:  result := 'Service provider is invalid.';
    10106:  result := 'Service provider failed to initialize.';
    10107:  result := 'System call failure.';
    10108:  result := 'Service not found.';
    10109:  result := 'Class type not found.';
    10110:  result := 'No more results.';
    10111:  result := 'Call was canceled.';
    10112:  result := 'Database query was refused.';
    11001:  result := 'Host not found.';
    11002:  result := 'Nonauthoritative host not found.';
    11003:  result := 'This is a nonrecoverable error.';
    11004:  result := 'Valid name, no data record of requested type.';
    11005:  result := 'QoS receivers.';
    11006:  result := 'QoS senders.';
    11007:  result := 'No QoS senders.';
    11008:  result := 'QoS no receivers.';
    11009:  result := 'QoS request confirmed.';
    11010:  result := 'QoS admission error.';
    11011:  result := 'QoS policy failure.';
    11012:  result := 'QoS bad style.';
    11013:  result := 'QoS bad object.';
    11014:  result := 'QoS traffic control error.';
    11015:  result := 'QoS generic error.';
    11016:  result := 'QoS service type error.';
    11017:  result := 'QoS flowspec error.';
    11018:  result := 'Invalid QoS provider buffer.';
    11019:  result := 'Invalid QoS filter style.';
    11020:  result := 'Invalid QoS filter type.';
    11021:  result := 'Incorrect QoS filter count.';
    11022:  result := 'Invalid QoS object length.';
    11023:  result := 'Incorrect QoS flow count.';
    11024:  result := 'Unrecognized QoS object.';
    11025:  result := 'Invalid QoS policy object.';
    11026:  result := 'Invalid QoS flow descriptor.';
    11027:  result := 'Invalid QoS provider-specific flowspec.';
    11028:  result := 'Invalid QoS provider-specific filterspec.';
    11029:  result := 'Invalid QoS shape discard mode object.';
    11030:  result := 'Invalid QoS shaping rate object.';
    11031:  result := 'Reserved policy QoS element type.';
  end
end;

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
  iin : TIniFile;

const
  sect = 'Config';

begin
  iin := TIniFile.Create('vtxserv.ini');
  SystemInfo.SystemName :=  iin.ReadString(sect, 'SystemName',  'A VTX Board');
  SystemInfo.SystemIP :=    iin.ReadString(sect, 'SystemIP',    'localhost');
  SystemInfo.InternetIP :=  iin.ReadString(sect, 'InternetIP',  '142.105.247.156');
  SystemInfo.HTTPPort :=    iin.ReadString(sect, 'HTTPPort',    '7001');
  SystemInfo.WSPort :=      iin.ReadString(sect, 'WSPort',      '7003');
  SystemInfo.Columns :=			iin.ReadInteger(sect, 'Columns', 80);
  SystemInfo.XScale :=			iin.ReadFloat(sect, 	'XScale', 1.0);
  SystemInfo.Terminal :=		iin.ReadString(sect, 'Terminal',		'ANSI');

  SystemInfo.NodeType :=    TvtxNodeType(InList(
                              iin.ReadString(sect, 'NodeType', 'ExtProc'),
                              ProcessType));

  SystemInfo.CodePage :=    TCodePages(
                              InList(
                                iin.ReadString(sect, 'CodePage', 'CP437'),
                                CodePageNames));

  SystemInfo.ExtProc :=     iin.ReadString(sect, 'ExtProc',     'cscript.exe //Nologo //I test.js @UserIP@');

  SystemInfo.TelnetIP :=    iin.ReadString(sect, 'TelnetIP', '192.168.0.2');
  SystemInfo.TelnetPort :=  iin.ReadString(sect, 'TelnetPort',  '7002');

  SystemInfo.MaxConnections := iin.ReadInteger(sect, 'MaxConnections',  32);

  // resolve possible named IPs to IP addresses.
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
      'HTTP':   result += [ HTTP ];
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
  str := str.Replace('@SystemName@', SystemInfo.SystemName);
  str := str.Replace('@SystemIP@', SystemInfo.SystemIP);
  str := str.Replace('@InternetIP@', SystemInfo.InternetIP);
  str := str.Replace('@HTTPPort@', SystemInfo.HTTPPort);
  str := str.Replace('@TelnetIP@', SystemInfo.TelnetIP);
  str := str.Replace('@TelnetPort@', SystemInfo.TelnetPort);
  str := str.Replace('@WSPort@', SystemInfo.WSPort);
  str := str.Replace('@CodePage@', CodePageNames[SystemInfo.CodePage]);
  str := str.Replace('@Columns@', IntToStr(SystemInfo.Columns));
  str := str.Replace('@XScale@', FloatToStr(SystemInfo.XScale));
  str := str.Replace('@Terminal@', SystemInfo.Terminal);
  result := str;
end;

{$endregion}

{ *************************************************************************** }
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
    FOnProgress('', fProgress);
  end;
end;

// main http listener loop
procedure TvtxHTTPServer.Execute;
var
  ListenerSocket,
  ConnectionSocket: TTCPBlockSocket;
  errno : integer;
begin
  begin
    ListenerSocket := TTCPBlockSocket.Create;
    ConnectionSocket := TTCPBlockSocket.Create;
    // not designed to run on high performance production servers
    // give users all the time they need.
    try
      ListenerSocket.CreateSocket;
      ListenerSocket.SetTimeout(10000);
      ListenerSocket.SetLinger(true, 10000);
      ListenerSocket.Bind(SystemInfo.SystemIP, SystemInfo.HTTPPort);
      ListenerSocket.Listen;
    finally
      repeat
        if ListenerSocket.CanRead(1000) then
        begin
          lastaction := now;  // there are active connections
          // todo: thread this out.
          ConnectionSocket.Socket := ListenerSocket.Accept;
          errno := ConnectionSocket.LastError;
          if errno <> 0 then
          begin
            fProgress := Format(
                '** Error HTTP accepting socket. #%d : %s',
                [ errno, GetSocketErrorMsg(errno) ]);
            Synchronize(@DoProgress);
          end
          else
          begin
            AttendConnection(ConnectionSocket);
            ConnectionSocket.CloseSocket;
          end;
        end;
        if Terminated then
          break;
      until false;
    end;
    ListenerSocket.Free;
    ConnectionSocket.Free;
  end;
end;

// send an binary file from www directory
function TvtxHTTPServer.SendBinary(
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
      try
      ASocket.SendString(
          'HTTP/1.0 200' + CRLF
        + 'Pragma: public' + CRLF
        + 'Cache-Control: max-age=86400' + CRLF
        + 'Expires: ' + Rfc822DateTime(TimeStampToDateTime(expires)) + CRLF
        + 'Content-Type: ' + ContentType + CRLF
        + '' + CRLF);
      ASocket.SendBuffer(buff, size);
    except
      fProgress:='** Error on HTTPServer.SendBinary.';
      Synchronize(@DoProgress);
    end;
    freememory(buff);
  end
  else
    result := 404;
end;

// send text file from www directory
// swap @ codes in text files.
function TvtxHTTPServer.SendText(
          ASocket : TTCPBlockSocket;
          ContentType : string;
          Filename : string) : integer;
var
  fin : TextFile;
  instr, str : string;

begin
  result := 200;
  Filename := 'www' + Filename;
  if FileExists(Filename) then
  begin
    assignfile(fin, Filename);
    reset(fin);
    str := '';
    while not eof(fin) do
    begin
      readln(fin, instr);
			instr := ReplaceAtCodes(instr);
      instr := instr.Replace('@UserIP@', ASocket.GetRemoteSinIP);
      str += instr + CRLF;
    end;
    closefile(fin);
    try
      ASocket.SendString(
          'HTTP/1.0 200' + CRLF
        + 'Content-Type: ' + ContentType + CRLF
        + 'Content-Length: ' + IntToStr(length(str)) + CRLF
        + 'Connection: close' + CRLF
        + 'Date: ' + Rfc822DateTime(now) + CRLF
        + 'Server: VTX Mark-II' + CRLF + CRLF
        + '' + CRLF);
      ASocket.SendString(str);
    except
      fProgress := '** Error on HTTPServer.SendTextFile';
      Synchronize(@DoProgress);
    end;
  end
  else
    result := 404;
end;

// fulfill the request.
procedure TvtxHTTPServer.AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout :   integer;
  code :      integer;
  s:          string;
  ext:        string;
  //method :    string;
  uri :       string;
  //protocol :  string;

begin
  timeout := 120000;

  //read request line
  s := ASocket.RecvString(timeout);
  fetch(s, ' '); //method := fetch(s, ' ');
  uri := fetch(s, ' ');
  fetch(s, ' '); //protocol := fetch(s, ' ');

  //read request headers
  repeat
    s := ASocket.RecvString(Timeout);
  until s = '';

  code := 200;
  if uri = '/' then
    code := SendText(ASocket, 'text/html', '/index.html')
  else
  begin
    ext := ExtractFileExt(uri);
    case ext of
      '.css':   code := SendText(ASocket, 'text/css', uri);
      '.js':    code := SendText(ASocket, 'text/javascript', uri);
      '.png':   code := SendBinary(ASocket, 'image/png', uri);
      '.ico':		code := SendBinary(ASocket, 'image/x-icon', uri);
      '.eot':   code := SendBinary(ASocket, 'application/vnd.ms-fontobject', uri);
      '.svg':   code := SendBinary(ASocket, 'image/svg+xml', uri);
      '.ttf':   code := SendBinary(ASocket, 'application/font-sfnt', uri);
      '.woff':  code := SendBinary(ASocket, 'application/font-woff', uri);
      '.ogg':   code := SendBinary(ASocket, 'audio/ogg', uri);
      '.wav':   code := SendBinary(ASocket, 'audio/vnd.wav', uri);
      '.mp3':   code := SendBinary(ASocket, 'audio/mpeg', uri);
      // add others here as needed
      else      code := 404;
    end;
  end;
    if code <> 200 then
      ASocket.SendString(
        'HTTP/1.0 ' + IntToStr(code) + CRLF
        + httpCode(code) + CRLF);
end;


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
    //serverCon.SendText(str);
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
    FOnProgress('', fProgress);
  end;
end;

function BufferDump(buf : pbyte; len : integer) : string;
var
  i : integer;
  str : string;
begin
  str := '';
  for i := 0 to len-1 do
  	str += ' ' + IntToHex(buf[i], 2);
  result := str.substring(1);
end;

function BufferDump(strin : rawbytestring) : string;
var
  i : integer;
  str : string;
begin
  str := '';
  for i := 1 to length(strin) do
  	str += ' ' + IntToHex(byte(strin[i]), 2);
  result := str.substring(1);
end;

// send data to node process
function TvtxNodeProcess.SendBuffer(buf : pbyte; len : longint) : integer;
var
  newbuf,
  pin,
  pout : pbyte;
  b : byte;
  i,
  lenout : longint;
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
          // need to escape out IAC's
          newbuf := GetMemory(len * 2);	// worse case scenario. all IAC's
  				pout := newbuf;
          lenout := 0;

          for i := 0 to len - 1 do
          begin
            b := buf[i];
            pout^ := b;
            inc(pout);
            inc(lenout);

            // escape TN_IAC
            if b = TN_IAC then
            begin
              pout^ := b;
              inc(pout);
              inc(lenout);
            end;
          end;

  	  	  //result := fpsend(serverCon.tnsock, buf, len, 0);
	  	  	result := fpsend(serverCon.tnsock, newbuf, lenout, 0);
    			FreeMemory(newbuf);
				end;
      end;
  end;
end;

// send a telnet iac command
function TvtxNodeProcess.SendCmd(stuff : array of Variant) : integer;
var
  i, j :    integer;
  str :     rawbytestring;
  tmpstr :  string;
  len : integer;
	debug : string;
begin
  debug := '';
  case SystemInfo.NodeType of

    ExtProc:
      result := -1;

  	Telnet:
		  begin
	  		str := char(TN_IAC);
        debug += TnCmds[TN_IAC] + ' ';
			  for i := 0 to length(stuff) - 1 do
			  begin
		    	case VarType(stuff[i]) of
		  	    varString:
			        begin
		      	    tmpstr := stuff[i];
		    	      for j := 0 to length(tmpstr) - 1 do
                begin
		  	          str += char(tmpstr.Chars[j]);
                  debug += char(tmpstr.Chars[j]);
                end;
                debug += ' ';
              end;

		     		else
            begin
	    	    	str += char(byte(stuff[i]));
              debug += TnCmds[byte(stuff[i])] + ' ';
            end;
          end;
		  	end;
	  		len := length(str);
	  		result := fpsend(serverCon.tnsock, pointer(str), len, 0);
{$ifdef DEBUG}
writeln(logout, 'telnetout: ' + debug);
{$endif}
  		end;

  end;
end;

// process str for telnet commands, return string with commands stripped.
function TvtxNodeProcess.Negotiate(
    buf : array of byte;
    len : integer) : rawbytestring;
var
  strout : rawbytestring;
  b : 			byte;
  i : 			integer;
	debug : 	string;

begin
  strout := '';
  for i := 0 to len - 1 do
  begin
    b := byte(buf[i]);
    case serverCon.tnstate of
      0:  // not in a telnet state
        begin
          if b = TN_IAC then
            serverCon.tnstate := 1
          else
            strout := strout + char(b);
        end;

      1:  // recieved a TN_IAC
        begin
          case b of
            TN_IAC:   // escaped $FF
              begin
              	strout := strout + char(b);
                serverCon.tnstate := 0;
              end;

            // ignore for now.
            TN_SE,    // subneg end
            TN_NOP,   // no operation
            TN_DM,    // data mark
            TN_BRK,   // break
            TN_IP,    // interrupt process
            TN_AO,    // abort output
            TN_AYT,   // are you there? try responding with NUL
            TN_EC,    // erase char
            TN_EL,    // erase line
            TN_GA:    // go ahead
              serverCon.tnstate := 0;

            TN_SB:    // subneg begin
              serverCon.tnstate := 3;

            TN_WILL,
            TN_WONT,
            TN_DO,
            TN_DONT:
              begin
                serverCon.tncmd := b;
                serverCon.tnstate := 2;
              end;

            else
              begin
                strout := strout + char(b);
                serverCon.tnstate := 0;
              end;
          end;
        end;

      2:  // have IAC + cmd so far
        begin
          case serverCon.tncmd of
            TN_WILL:
              // server would like to do something. send DO or DONT
              begin
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC WILL ' + TnCmds[b]);
{$endif}
                if serverCon.tnqus[b] = TNQ_WANTYES then
                begin
                  // response to my request to do
                  serverCon.tnqus[b] := TNQ_YES;
                end
                else
                begin
                  case b of
                    TN_BIN,
                    TN_SGA,
                    TN_ECHO,
                    TN_NAWS,
                    TN_TTYPE,
                    TN_TSPEED:
                      begin
                        SendCmd([ TN_DO, b ]);
                        serverCon.tnqhim[b] := TNQ_YES;
                      end;

                    else
                      begin
                        SendCmd([ TN_DONT, b ]);
                        serverCon.tnqhim[b] := TNQ_NO;
                      end;
                  end;
                end;
              end;

            TN_WONT:
              // server would like to not do something. send DONT
              begin
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC WONT ' + TnCmds[b]);
{$endif}
                if (serverCon.tnqus[b] = TNQ_WANTYES)
                  or (serverCon.tnqus[b] = TNQ_WANTNO) then
                begin
                  // response to my request to do or dont
                  serverCon.tnqus[b] := TNQ_NO;
                end
                else
                begin
                  // server wants to not do
                  SendCmd([ TN_DONT, b ]);
                  serverCon.tnqhim[b] := TNQ_NO;
                end;
              end;

            TN_DO:
              // server wants me to do something. send WILL or WONT
              begin
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC DO ' + TnCmds[b]);
{$endif}
                if serverCon.tnqus[b] = TNQ_WANTYES then
                begin
                  // response to my request to will
                  serverCon.tnqus[b] := TNQ_YES;

                  // send some SB stuff now
                  if b = TN_NAWS then
                  begin
                    SendCmd([ TN_SB, TN_NAWS, 0, 80, 0, 25 ]);
                    SendCmd([ TN_SE ]);
                  end;
                end
                else
                begin
                  // server wants us to do
                  case b of
                    TN_BIN,
                    TN_SGA,
                    TN_ECHO,
                    TN_NAWS,
                    TN_TTYPE,
                    TN_TSPEED:
                      begin
                        SendCmd([ TN_WILL, b ]);
                        serverCon.tnqus[b] := TNQ_YES;
                      end;

                    else
                      begin
                        SendCmd([ TN_WONT, b ]);
                        serverCon.tnqus[b] := TNQ_NO;
                      end;
                  end;
                end;
              end;

            TN_DONT:
              begin
                // server wants me to not do something. send WONT
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC DONT ' + TnCmds[b]);
{$endif}
                if serverCon.tnqus[b] = TNQ_WANTYES then
                begin
                  // response to my request to will
                  serverCon.tnqus[b] := TNQ_NO;
                end
                else
                begin
                  // server wants us to not do. respond WONT
                  SendCmd([ TN_WONT, b ]);
                  serverCon.tnqus[b] := TNQ_NO;
                end;
              end;
          end;
          serverCon.tnstate := 0;
        end;

      3:  // have IAC SB
        begin
          serverCon.tncmd    := b;
          serverCon.tnstate  := 4;
        end;

      4: // have IAC SB cmd
        begin
          // this should be SEND (1)
          if b = TN_SEND then
          begin
            case serverCon.tncmd of
              TN_TTYPE:
                // will neg term type;
                begin
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC SB TTYPE ' + TnCmds[b]);
{$endif}
                  SendCmd([ TN_SB, serverCon.tncmd, TN_IS, SystemInfo.Terminal ]);
                  SendCmd([ TN_SE ]);
                end;

              TN_TSPEED:
                // will neg terminal speed
                begin
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC SB TSPEED ' + TnCmds[b]);
{$endif}
                  SendCmd([ TN_SB, serverCon.tncmd, TN_IS, '921600,921600' ]);
                  SendCmd([ TN_SE ]);
                end;

              TN_NEWE:
                begin
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC SB NEWE ' + TnCmds[b]);
{$endif}
                  SendCmd([ TN_SB, serverCon.tncmd, TN_IS, 0 ]);
                  SendCmd([ TN_SE ]);
                end;

              else
                begin
                  // ? why are we being asked this? server on drugs?
{$ifdef DEBUG}
writeln(logout, 'telnetin : IAC SB ' + TnCmds[serverCon.tncmd] + TnCmds[b]);
{$endif}
                  SendCmd([ TN_DONT, serverCon.tncmd ]);
                  strout := strout + char(b);
                end;
            end;
          end;
          serverCon.tnstate := 0;
        end;
    end;
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
  if serverWS <> nil then
  begin

    case SystemInfo.NodeType of

	    ExtProc:
        begin
          // execute a spawned node session.
          fProgress := 'Spawning process for ' + serverCon.Socket.GetRemoteSinIP + '.';
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
            fProgress:='** Error on Node Process.Execute';
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

          // clear tn options
          FillMemory(@serverCon.tnqhim, 256, TNQ_NO);
          FillMemory(@serverCon.tnqus, 256, TNQ_NO);

          // send telnet setups.
          // set initial telnet options
          serverCon.tnqus[TN_BIN] :=    TNQ_WANTYES;
          serverCon.tnqhim[TN_BIN] :=		TNQ_WANTYES;
          serverCon.tnqus[TN_SGA] :=    TNQ_WANTYES;
          serverCon.tnqhim[TN_SGA] :=   TNQ_WANTYES;
          serverCon.tnqhim[TN_ECHO] :=  TNQ_WANTYES;
          serverCon.tnqus[TN_NAWS] :=   TNQ_WANTYES;
          serverCon.tnqus[TN_TTYPE] :=  TNQ_WANTYES;
          serverCon.tnqus[TN_TSPEED] := TNQ_WANTYES;

          // send initial barrage of telnet settings
          for i := 0 to 255 do
          begin
            case serverCon.tnqus[i] of
              TNQ_WANTYES:
                begin
                	SendCmd([ TN_WILL, i ]);
                end;

              TNQ_WANTNO:
                begin
                	SendCmd([ TN_WONT, i ]);
                end;
            end;

            case serverCon.tnqhim[i] of
              TNQ_WANTYES:
                begin
	                SendCmd([ TN_DO, i ]);
                end;

              TNQ_WANTNO:
                begin
                	SendCmd([ TN_DONT, i ]);
                end;
            end;
          end;

          // connected!!! loop
          while true do
          begin

            if serverCon.tnsock = -1 then
            begin
              fProgress := 'invalid socket.';
              Synchronize(@DoProgress);
              break;
            end;

            // stuff to read
            rv := fprecv(serverCon.tnsock, @serverCon.tnbuf, BUFFSIZE, 0);
            if rv = 0 then
            begin
              fProgress := 'telnet closed by remote.';
              Synchronize(@DoProgress);
              break;
            end;

            // handle negotiations
            if not serverCon.Closed then
            begin
              if rv > 0 then
              begin
                str := negotiate(serverCon.tnbuf, rv);
                if length(str) > 0 then
                begin
                  //serverCon.SendText(ConvertFromCP(Systeminfo.TelnetCP, str));

//                  fProgress := 'RECV: ' + BufferDump(str);
//                  Synchronize(@DoProgress);

                  //serverCon.SendText(str);
                  serverCon.SendBinary(TStringStream.Create(str));
                end;
              end;
            end
            else
            begin
              fProgress := 'websocket closed during telnet.';
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

    fProgress := 'Finished node process for.';
    Synchronize(@DoProgress);
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

procedure TvtxApp.HTTPTerminate(Sender : TObject); register;
begin
  WriteCon('', 'HTTP Terminated.');
end;

procedure TvtxApp.NodeTerminate(Sender: TObject);
begin
  WriteCon('', 'Node Terminated.');
end;

procedure TvtxApp.WSBeforeAddConnection(
      Server :      TCustomServer;
      aConnection : TCustomConnection;
  var CanAdd :      boolean); register;
begin
  WriteCon('', 'Before Add WS Connection.');
end;

procedure TvtxApp.WSAfterAddConnection(
      Server : TCustomServer;
      aConnection : TCustomConnection); register;
var
  con : TvtxWSConnection;
begin
  con := TvtxWSConnection(aConnection);

  WriteCon('', 'WS Connection.');

  con.OnOpen :=  @WSOpen;
  con.OnRead :=  @WSRead;
  con.OnWrite := @WSWrite;
  con.OnClose := @WSClose;

  // spawn a new process for this connection
  con.ExtNode := TvtxNodeProcess.Create(true);
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
  WriteCon(TvtxWSConnection(aSender).Socket.GetRemoteSinIP, 'Open WS Connection.');
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
      app.WriteCon('','error sending to node process.');
     	con.Close(wsCloseNormal, 'Good bye');
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
  //WriteCon('Write WS Connection.');
end;

procedure TvtxApp.WSClose(
      aSender: TWebSocketCustomConnection;
      aCloseCode: integer;
      aCloseReason: string;
      aClosedByPeer: boolean);
begin
  WriteCon(TvtxWSConnection(aSender).Socket.GetRemoteSinIP, 'Close WS Connection.');
end;

procedure TvtxApp.WSBeforeRemoveConnection(
      Server :      TCustomServer;
      aConnection : TCustomConnection); register;
var
  con : TvtxWSConnection;
begin
  WriteCon('', 'Before Remove WS Connection 1.');
  con := TvtxWSConnection(aConnection);

  case SystemInfo.NodeType of

    ExtProc:
      begin
        if (con.ExtProc <> nil) and con.ExtProc.Running then
        begin
          WriteCon(con.Socket.GetRemoteSinIP,
            'Force Terminate Node Processes');
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
  WriteCon('', 'Before Remove WS Connection 2.');
end;

procedure TvtxApp.WSAfterRemoveConnection(
      Server : TCustomServer;
      aConnection : TCustomConnection); register;
begin
  WriteCon(aConnection.Socket.GetRemoteSinIP, 'After Remove WS Connection.');
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
  if ip = '' then
  begin
    HighVideo;
    Write(
      format(#13'%4.2d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d ',
      [
        YearOf(now),    // year
        MonthOf(now),   // month
        DayOf(now),     // day
        HourOf(now),    // 24hr
        MinuteOf(now),  // min
        SecondOf(Now) // sec
      ]))
  end
  else
  begin
    ips := ip.split('.');
    LowVideo;
    Write(
      format(#13'%4.2d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d (%3.3d.%3.3d.%3.3d.%3.3d) ',
      [
        YearOf(now),    // year
        MonthOf(now),   // month
        DayOf(now),     // day
        HourOf(now),    // 24hr
        MinuteOf(now),  // min
        SecondOf(Now),  // sec
        strtoint(ips[0]),
        strtoint(ips[1]),
        strtoint(ips[2]),
        strtoint(ips[3])
      ]));
  end;
  NormVideo;
  writeln(msg);
end;

procedure TvtxApp.StartHTTP;
begin
  // start http server
  if not runningHTTP then
  begin
    WriteCon('', format('HTTP server starting on port %s.', [SystemInfo.HTTPPort]));
    serverHTTP := TvtxHTTPServer.Create(true);
    serverHTTP.OnProgress := @WriteCon;
    serverHTTP.OnTerminate := @HTTPTerminate;
    serverHTTP.Start;
    runningHTTP := true;
  end
  else
    WriteCon('', 'HTTP server already running.');
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

procedure TvtxApp.StopHTTP;
begin
  if runningHTTP then
  begin
    WriteCon('', 'HTTP server terminating.');
    if serverHTTP <> nil then
      serverHTTP.Terminate;
    runningHTTP := false;
  end;
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
      		if con.ExtProc.Running then
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
  cp :          TCodePages;
  str :         string;
  count :       integer;
  ThreadRan :   boolean;
  WsaData :     TWSAData;
  t1, t2 :			boolean;

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

  write('VTX Server Console.' + CRLF
      + '2017 Dan Mecklenburg Jr.' + CRLF
      + CRLF
      + 'Type HELP for commands, QUIT to exit.' + CRLF + CRLF);

  app :=            TvtxApp.Create;
  serverWS :=       nil;
  runningHTTP :=    false;
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
                  app.StartHTTP;
                  app.StartWS;
                end
                else
                begin
                  if HTTP in serv then
                    app.StartHTTP;
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
                  app.StopHTTP;
                  app.CloseAllNodes();
                  app.StopWS;
                end
                else
                begin
                  if HTTP in serv then
                    app.StopHTTP;
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
            if runningHTTP    then linein += 'HTTP, ';
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
            t1 := runningHttp;
            t2 := runningWS;
        		if runningHTTP then
            begin
            	app.StopHTTP;
              while not serverHTTP.Finished do;
						end;
            if runningWS then
            begin
	            app.CloseAllNodes();
        			app.StopWS;
              while not serverWS.Finished do;
            end;
            LoadSettings;
            sleep(2000);
            if t1 then
	            app.StartHTTP;
            if t2 then
            	app.StartWS;
          end;

        'CPS':
          begin
            str := 'Supported codepages : ';
            for cp := FirstCP to LastCP do
              str += CodePageNames[cp] + ', ';
            str := LeftStr(str, length(str) - 2) + '.';
            app.WriteCon('', str);
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
            app.WriteCon('', '          CPS  - List supported codepages available for CONV.');
            app.WriteCon('', '          LOADCFG - force reload config.');
            app.WriteCon('', '');
            app.WriteCon('', '          serv = HTTP, WS, or ALL');
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
  app.StopHTTP;
  app.StopWS;
  sleep(2000);

  if SystemInfo.NodeType = Telnet then
    WSACleanup;

	closefile(logout);
end.
{$endregion}

