unit VTXNodeUtils;

{$mode objfpc}{$H+}
interface

uses
  {$ifdef WINDOWS}
  Windows,
  {$endif}
  Classes, Pipes, SysUtils, Crt, DateUtils, LazUTF8, strutils;

type
  TRowStyles = (None, Solid, HorzGrad, VertGrad);

const
  CR =                    #13;
  LF =                    #10;
  CRLF =                  CR + LF;
  ESC =                   #27;
  CSI =                   ESC + '[';
  CLS =                   CSI + '2J';
  HOME =                  CSI + 'f';
  APC =                   ESC + '_';
  ST =                    ESC + '\';

  VTXMODE =               CSI + '?50h';
  BBSMODE =               CSI + '?50l';

  ROW_MARQUEE =           ESC + '#1';
  ROW_MARQUEEOFF =        ESC + '#0';
  ROW_RESET =             ESC + '#9';

  // SGR values
  SGR_RESET =             0;
  SGR_BOLD =              1;
  SGR_ITALICS =           3;
  SGR_UNDERLINE =         4;
  SGR_BLINK =             5;
  SGR_REVERSE =           7;
  SGR_CONCEAL =           8;
  SGR_STRIKETHROUGH =     9;
  SGR_BOLDOFF =           21;
  SGR_ITALICSOFF =        23;
  SGR_UNDERLINEOFF =      24;
  SGR_BLINKOFF =          25;
  SGR_REVERSEOFF =        27;
  SGR_CONCEALOFF =        28;
  SGR_STRIKETHROUGHOFF =  29;
  SGR_GLOW =              50;
  SGR_OUTLINE =           56;
  SGR_SHADOW =            57;
  SGR_GLOWOFF =           70;
  SGR_OUTLINEOFF =        76;
  SGR_SHADOWOFF =         77;

  // base colors
  ANSI_TRANSPARENT =      0;
  ANSI_RED =              1;
  ANSI_GREEN =            2;
  ANSI_BROWN =            3;
  ANSI_BLUE =             4;
  ANSI_MAGENTA =          5;
  ANSI_CYAN =             6;
  ANSI_LTGRAY =           7;
  ANSI_DKGRAY =           8;
  ANSI_LTRED =            9;
  ANSI_LTGREEN =          10;
  ANSI_YELLOW =           11;
  ANSI_LTBLUE =           12;
  ANSI_LTMAGENTA =        13;
  ANSI_LTCYAN =           14;
  ANSI_WHITE =            15;
  ANSI_BLACK =            16;

procedure Init;
procedure Finish;

procedure Print(str : string); inline;
procedure PrintLn;
procedure PrintLn(str : string);
procedure Error(str : string); inline;

function InStr : string;
function GetKey : string;
procedure PrintANSI(filename : string);

procedure MoveCursor(r, c : integer);
procedure MoveUp(n : integer);
procedure MoveDown(n : integer);
procedure MoveRight(n : integer);
procedure MoveLeft(n : integer);

function ROWSIZE(s, w : integer) : string;
function ROWCOLOR(color1, color2 : integer; style : TRowStyles) : string;
function UP(n : integer) : string;
function DOWN(n : integer) : string;
function RIGHT(n : integer) : string;
function LEFT(n : integer) : string;
function MOVETO(r, c : integer) : string;

function UNICODE(n : integer) : string;

function URL(w, h, l : integer; urlstr : string) : string;
function HOTSPOT(w, h, l : integer; hotstr : string) : string;

function SOUNDDEFURL(n : integer; urlstr : utf8string) : string;
function SOUNDSET(n : integer) : string;
function SOUNDPLAY : string;
function SOUNDSTOP : string;
function SOUNDVOL(v : integer) : string;

function RGBToANSI(red, green, blue : byte) : integer;
function SGR(fgcolor : integer) : string;
function SGR(fgcolor, bgcolor : integer) : string;
function SGR(vals : array of integer) : string;
function SGR(fgcolor : integer; vals : array of integer) : string;
function SGR(fgcolor, bgcolor : integer; vals : array of integer) : string;

implementation

var
  pin :   TInputPipeStream;
  pout,
  perr:   TOutputPipeStream;
  lastaction : TDateTime;
  PrevBreakHandler : TCtrlBreakHandler;

//------------------ SUPPORT FUNCTIONS

{ Remove last character from string. }
function TrimRightOne(str : string) : string;
begin
  result := str.substring(0,length(str)-1);
end;

{ Find closest color in ANSI palette that matches RGB }
function RGBToANSI(red, green, blue : byte) : integer;
const
  BaseColor : array [0..255] of integer = (
    // VGA 0-15 - transparent will switch to #000000 when appropriate
    $000000, $AA0000, $00AA00, $AA5500,
    $0000AA, $AA00AA, $00AAAA, $AAAAAA,
    $555555, $FF5555, $55FF55, $FFFF55,
    $5555FF, $FF55FF, $55FFFF, $FFFFFF,

    // EXTENDER 16-231
    $000000, $00005F, $000087, $0000AF, $0000D7, $0000FF,
    $005F00, $005F5F, $005F87, $005FAF, $005FD7, $005FFF,
    $008700, $00875F, $008787, $0087AF, $0087D7, $0087FF,
    $00AF00, $00AF5F, $00AF87, $00AFAF, $00AFD7, $00AFFF,
    $00D700, $00D75F, $00D787, $00D7AF, $00D7D7, $00D7FF,
    $00FF00, $00FF5F, $00FF87, $00FFAF, $00FFD7, $00FFFF,
    $5F0000, $5F005F, $5F0087, $5F00AF, $5F00D7, $5F00FF,
    $5F5F00, $5F5F5F, $5F5F87, $5F5FAF, $5F5FD7, $5F5FFF,
    $5F8700, $5F875F, $5F8787, $5F87AF, $5F87D7, $5F87FF,
    $5FAF00, $5FAF5F, $5FAF87, $5FAFAF, $5FAFD7, $5FAFFF,
    $5FD700, $5FD75F, $5FD787, $5FD7AF, $5FD7D7, $5FD7FF,
    $5FFF00, $5FFF5F, $5FFF87, $5FFFAF, $5FFFD7, $5FFFFF,
    $870000, $87005F, $870087, $8700AF, $8700D7, $8700FF,
    $875F00, $875F5F, $875F87, $875FAF, $875FD7, $875FFF,
    $878700, $87875F, $878787, $8787AF, $8787D7, $8787FF,
    $87AF00, $87AF5F, $87AF87, $87AFAF, $87AFD7, $87AFFF,
    $87D700, $87D75F, $87D787, $87D7AF, $87D7D7, $87D7FF,
    $87FF00, $87FF5F, $87FF87, $87FFAF, $87FFD7, $87FFFF,
    $AF0000, $AF005F, $AF0087, $AF00AF, $AF00D7, $AF00FF,
    $AF5F00, $AF5F5F, $AF5F87, $AF5FAF, $AF5FD7, $AF5FFF,
    $AF8700, $AF875F, $AF8787, $AF87AF, $AF87D7, $AF87FF,
    $AFAF00, $AFAF5F, $AFAF87, $AFAFAF, $AFAFD7, $AFAFFF,
    $AFD700, $AFD75F, $AFD787, $AFD7AF, $AFD7D7, $AFD7FF,
    $AFFF00, $AFFF5F, $AFFF87, $AFFFAF, $AFFFD7, $AFFFFF,
    $D70000, $D7005F, $D70087, $D700AF, $D700D7, $D700FF,
    $D75F00, $D75F5F, $D75F87, $D75FAF, $D75FD7, $D75FFF,
    $D78700, $D7875F, $D78787, $D787AF, $D787D7, $D787FF,
    $D7AF00, $D7AF5F, $D7AF87, $D7AFAF, $D7AFD7, $D7AFFF,
    $D7D700, $D7D75F, $D7D787, $D7D7AF, $D7D7D7, $D7D7FF,
    $D7FF00, $D7FF5F, $D7FF87, $D7FFAF, $D7FFD7, $D7FFFF,
    $FF0000, $FF005F, $FF0087, $FF00AF, $FF00D7, $FF00FF,
    $FF5F00, $FF5F5F, $FF5F87, $FF5FAF, $FF5FD7, $FF5FFF,
    $FF8700, $FF875F, $FF8787, $FF87AF, $FF87D7, $FF87FF,
    $FFAF00, $FFAF5F, $FFAF87, $FFAFAF, $FFAFD7, $FFAFFF,
    $FFD700, $FFD75F, $FFD787, $FFD7AF, $FFD7D7, $FFD7FF,
    $FFFF00, $FFFF5F, $FFFF87, $FFFFAF, $FFFFD7, $FFFFFF,
    // GRAYS 232-255
    $080808, $121212, $1C1C1C, $262626, $303030, $3A3A3A,
    $44FFF4, $4E4E4E, $585858, $626262, $6C6C6C, $767676,
    $808080, $8A8A8A, $949494, $9E9E9E, $A8A8A8, $B2B2B2,
    $BCBCBC, $C6C6C6, $D0D0D0, $DADADA, $E4E4E4, $EEEEEE );
var
  i : integer;
  dr, dg, db, d, mind, c : integer;
begin
  c := -1;
  mind := 99999;
  for i := 0 to 255 do
  begin
    dr := ((BaseColor[i] shr 16) and $FF) - red;
    dg := ((BaseColor[i] shr  8) and $FF) - green;
    db := ((BaseColor[i]       ) and $FF) - blue;
    dr *= dr;
    dg *= dg;
    db *= db;
    d := dr + dg + db;
    if d < mind then
    begin
      mind := d;
      c := i;
    end;
  end;
  result := c;
end;

{ Create SGR text for foreground color }
function FGColorSGR(fgcolor : integer) : string;
begin
  if (fgcolor < 0) or (fgcolor > 255) then
    fgcolor := 0
  else if fgcolor < 8 then
    result := inttostr(fgcolor + 30)
  else if fgcolor < 16 then
    result := inttostr(fgcolor + 90 - 8)
  else
    result := '38;5;' + inttostr(fgcolor);
end;

{ Create SGR text for background color }
function BGColorSGR(bgcolor : integer) : string;
begin
  if (bgcolor < 0) or (bgcolor > 255) then
    bgcolor := 0
  else if bgcolor < 8 then
    result := inttostr(bgcolor + 40)
  else if bgcolor < 16 then
    result := inttostr(bgcolor + 100 - 8)
  else
    result := '48;5;' + inttostr(bgcolor);
end;

{ Create SGR text for attribute list }
function AttrSGR(vals : array of integer) : string;
var
  i : integer;
begin
  result := '';
  for i := 0 to length(vals) - 1 do
    result += inttostr(vals[i]) + ';';
  result := TrimRightOne(result);
end;

{ Create SGR codes for foreground color only }
function SGR(fgcolor : integer) : string;
begin
  result := CSI + FGColorSGR(fgcolor) + 'm';
end;

{ Create SGR codes for foreground with background color }
function SGR(fgcolor, bgcolor : integer) : string;
begin
  // get other attributes first. remove the 'm', append a ';'
  result := CSI + FGColorSGR(fgcolor) + ';' + BGColorSGR(bgcolor) + 'm';
end;

function SGR(vals : array of integer) : string;
begin
  result := CSI + AttrSGR(vals) + 'm';
end;

function SGR(fgcolor : integer; vals : array of integer) : string;
begin
  result := CSI + AttrSGR(vals) + ';' + FGColorSGR(fgcolor) + 'm';
end;

function SGR(fgcolor, bgcolor : integer; vals : array of integer) : string;
begin
  result := CSI + AttrSGR(vals) + ';' + FGColorSGR(fgcolor) + ';'
    + BGColorSGR(bgcolor) + 'm';
end;

function UTF8Bytes(const s: UTF8String): TBytes;
begin
  Assert(StringElementSize(s)=1);
  SetLength(Result, Length(s));
  if Length(Result)>0 then
    Move(s[1], Result[0], Length(s));
end;

procedure PrintLn;
begin
  Print(CRLF);
end;

procedure PrintLn(str : string);
begin
  Print(str + CRLF);
end;

procedure Print(str : string);
var
  i : integer;
  b : byte;
  barray : TBytes;
  hex : string;
  utf : string;
  val : integer;
begin
  {$ifdef LOCAL}
    write(str);
  {$else}
    lastaction := now;
    barray := UTF8Bytes(str);
    i := 0;
    while i < length(barray) do
    begin
      // look out for #0 - convert to unicode
      b := barray[i];
      if b = 0 then
      begin
        hex := char(barray[i + 1])
          + char(barray[i + 2])
          + char(barray[i + 3])
          + char(barray[i + 4]);
        val := Hex2Dec(hex);
        utf := UnicodeToUTF8(val);
        pout.WriteAnsiString(utf);
        inc(i, 4);
      end
      else
        pout.WriteByte(b);
      inc(i);
    end;
    setlength(barray, 0);
  {$endif}
end;

procedure Error(str : string); inline;
var
  i : integer;
  barray : TBytes;
begin
  {$ifdef LOCAL}
    write(str);
  {$else}
    lastaction := now;
    barray := UTF8Bytes(str);
    for i := 0 to length(barray) - 1 do
      perr.WriteByte(barray[i]);
    setlength(barray, 0);
  {$endif}
end;

function InStr : string;
var
  i,
  bytes : integer;
  b : 		byte;
begin
  {$ifdef LOCAL}
    if keypressed then
      result := readkey
    else
      result := '';
  {$else}
    result := '';
	  bytes := pin.NumBytesAvailable;
    if bytes > 0 then
    begin
      lastaction := now;
  	  for i := 0 to bytes - 1 do
	    begin
      	b := pin.ReadByte;
    	  if b > 0 then
					result += char(b);
	    end;
    end
    else
    begin
      // hybernate after 1 minute
      if SecondsBetween(now, lastaction) > 60 then
        sleep(100);
    end;
  {$endif}
end;

function GetKey : string;
var
  key : string;
begin
  repeat
    key := Instr;
  until key <> '';
  result := key;
end;

{ Print a UTF-8 encoded ANSI file. }
procedure PrintANSI(filename : string);
var
  fin : TFileStream;
  size : integer;
  buff : pbyte;
begin
  if fileexists(filename) then
  begin
    // read it all in as is (UTF8)
    fin := TFileStream.Create(filename, fmOpenRead + fmShareDenyNone);
    size := fin.Size;
    buff := getmemory(size);
    fin.ReadBuffer(buff[0], size);
    pout.WriteBuffer(buff[0], size);
    freememory(buff);
    fin.Free;
  end
  else
    Error('Unable to find file ' + filename);
end;

procedure MoveCursor(r, c : integer);
begin
  Print(format(CSI + '%d;%df', [ r, c ]));
end;

procedure MoveUp(n : integer);
begin
  Print(CSI + inttostr(n) + 'A');
end;

procedure MoveDown(n : integer);
begin
  Print(CSI + inttostr(n) + 'B');
end;

procedure MoveRight(n : integer);
begin
  Print(CSI + inttostr(n) + 'C');
end;

procedure MoveLeft(n : integer);
begin
  Print(CSI + inttostr(n) + 'D');
end;

function ROWSIZE(s, w : integer) : string;
begin
  s := Round(s / 25) - 1;
  w := Round(w / 50) - 1;
  if s < 0 then s := 0;
  if s > 7 then s := 7;
  if w < 0 then w := 0;
  if w > 3 then w := 3;
  result := CSI + IntToStr(s) + ';' + IntToStr(w) + '[';
end;

function ROWCOLOR(color1, color2 : integer; style : TRowStyles) : string;
begin
  result := CSI + inttostr(color1) + ';' + inttostr(color2) + ';'
    + inttostr(ord(style)) + ']';
end;

function UP(n : integer) : string;
begin
  result := CSI + inttostr(n) + 'A';
end;

function DOWN(n : integer) : string;
begin
  result := CSI + inttostr(n) + 'B';
end;

function RIGHT(n : integer) : string;
begin
  result := CSI + inttostr(n) + 'C';
end;

function LEFT(n : integer) : string;
begin
  result := CSI + inttostr(n) + 'D';
end;

function MOVETO(r, c : integer) : string;
begin
  result := CSI + inttostr(r) + ';' + inttostr(c) + 'H';
end;

function UNICODE(n : integer) : string;
begin
  //result := UnicodeToUTF8(n);
  result := #0 + IntToHex(n, 4);
end;

function URL(w, h, l : integer; urlstr : string) : string;
var
  str : string;
  i : integer;
  c : integer;
begin
	str := '';
  for i := 0 to length(urlstr)-1 do
  begin
    c := ord(urlstr.Chars[i]);
  	str += ';' + inttostr(c);
  end;
  result := CSI + '1;' + inttostr(w)
  	+ ';' + inttostr(h)
  	+ ';' + inttostr(l) + str + '\';
end;

function HOTSPOT(w, h, l : integer; hotstr : string) : string;
var
  str : string;
  i : integer;
  c : integer;
begin
	str := '';
  for i := 0 to length(hotstr)-1 do
  begin
    c := ord(hotstr.Chars[i]);
  	str += ';' + inttostr(c);
  end;
  result := CSI + '0;' + inttostr(w)
  	+ ';' + inttostr(h)
  	+ ';' + inttostr(l) + str + '\';
end;

function Hex3Encode(val : utf8string) : string;
var
  l, i : integer;
  bytes : TBytes;
begin
  result := '';
	l := length(val);
	bytes := UTF8Bytes(val);
  for i := 0 to l - 1 do
    result += char($30 + ((bytes[i] and $F0) shr 4))
    				+ char($30 + ((bytes[i] and $0F)      ));
end;

function SOUNDDEFURL(n : integer; urlstr : utf8string) : string;
begin
  result := CSI + '1;0;' + inttostr(n) + ';1;' + Hex3Encode(urlstr) + '_';
end;

function SOUNDSET(n : integer) : string;
begin
  result := CSI + '1;1;' + inttostr(n) + '_';
end;

function SOUNDPLAY : string;
begin
  result := CSI + '1;2;1_';
end;

function SOUNDSTOP : string;
begin
  result := CSI + '1;2;0_';
end;

function SOUNDPAUSE : string;
begin
  result := CSI + '1;2;2_';
end;

function SOUNDVOL(v : integer) : string;
begin
  result := CSI + '1;3;' + inttostr(v) + '_';
end;

function VTXCtrlBreak(CtrlBreak : boolean) : boolean;
begin
  if CtrlBreak then
  begin
    // stuff in here to do before exit?
  end;
  result := true;
end;

procedure Init;
begin
  // setup
  {$ifndef LOCAL}
  pin := TInputPipeStream.Create(StdInputHandle);
  pout := TOutputPipeStream.Create(StdOutputHandle);
  perr := TOutputPipeStream.Create(StdErrorHandle);

  {$endif}
  PrevBreakHandler := SysSetCtrlBreakHandler(@VTXCtrlBreak);

  lastaction := now;
end;

procedure Finish;
begin
  // setdown
  {$ifndef LOCAL}
  // don't leave until streams are sent.
  sleep(2000);

  pin.Free;
  pout.Free;
  perr.Free;
  {$endif}
  SysSetCtrlBreakHandler(PrevBreakHandler);

end;

begin
end.

