program testvtx;

uses
  SysUtils, VTXNodeUtils;

var
  vol : integer = 25;
  play : boolean = false;

function Menu : string;
var
  key : string;
begin

  Print(#27'[0m'#27'[1;0*r');
  Print(#27'[?25h'#27'[?32l'#27'[?33l'#27'[?35l' + VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' VTX Server / Client / Node Demo');
  PrintLn;
  PrintLn(SGR(ANSI_LTRED) + ' Demo Menu');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'1')+'['
  	+ SGR(ANSI_YELLOW) + '1' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Console Layout');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'2')+'['
  	+ SGR(ANSI_YELLOW) + '2' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Cursor / Page Attributes');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'3')+'['
  	+ SGR(ANSI_YELLOW) + '3' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Line Attributes');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'4')+'['
  	+ SGR(ANSI_YELLOW) + '4' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Character Attributes');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'5')+'['
  	+ SGR(ANSI_YELLOW) + '5' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Colors');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'6')+'['
  	+ SGR(ANSI_YELLOW) + '6' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Cursor / Edit Controls');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'7')+'['
  	+ SGR(ANSI_YELLOW) + '7' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Sprites.');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'8')+'['
  	+ SGR(ANSI_YELLOW) + '8' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Sample ANSI art.');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'9')+'['
  	+ SGR(ANSI_YELLOW) + '9' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Clickable Hotspots.');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'A')+'['
  	+ SGR(ANSI_YELLOW) + 'A' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Block Characters.');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'B')+'['
  	+ SGR(ANSI_YELLOW) + 'B' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Font Selection.');

  PrintLn(SGR(ANSI_BROWN) + ' '+HOTSPOT(3,1,0,'B')+'['
  	+ SGR(ANSI_YELLOW) + 'C' + SGR(ANSI_BROWN)
    + ']' + SGR(ANSI_LTBLUE) + ' Audio.');

  Print(UP(11) + SGR(ANSI_LTGREEN));
  PrintLn(RIGHT(30) + 'This is a demonstration of the VTX web and');
  PrintLn(RIGHT(30) + 'websocket server, VTX web browser client, and');
  PrintLn(RIGHT(30) + 'user node processes.');
  PrintLn;
  PrintLn(RIGHT(30) + 'The web server delivers the client that then');
  PrintLn(RIGHT(30) + 'connects to the websocket server, which then');
  PrintLn(RIGHT(30) + 'spawns node processes for each client connection.');
  PrintLn;
  PrintLn(RIGHT(30) + 'This demo will display the ANSI code sequences');
  PrintLn(RIGHT(30) + 'supported by the VTX client.');

  printLn(DOWN(2) + SGR(ANSI_YELLOW) + ' Now with '#27'[6mAudio! '
  	+ #27'[5;26mPress ' + HOTSPOT(3,1,1,'P') + '[P] to play/stop, '
  	+	HOTSPOT(3,1,1,'+') + '[+]/' + HOTSPOT(3,1,1,'-') + '[-] for volume. '+SGR(ANSI_LTRED)+'(Dawn Radio)');
  println;

  Print(SGR(ANSI_GREEN) + ' Select Screen or ' + SGR(ANSI_BROWN)
  	+ HOTSPOT(3,1,0,'Q')+'[' + SGR(ANSI_YELLOW) + 'Q'
    + SGR(ANSI_BROWN) + ']' + SGR(ANSI_GREEN)
    + 'uit : ');


  result := '';
  repeat
    key := upCase(GetKey);

    if key = 'P' then
    begin
    	if play then
      	Print(SOUNDSTOP)
   		else
        Print(SOUNDPLAY);
      play := not play;
    end
    else if key = '+' then
   	begin
    	vol += 5;
	    if vol > 100 then vol := 100;
    	Print(SOUNDVOL(vol));
    end
    else if key = '-' then
    begin
    	vol -= 5;
	    if vol < 0 then vol := 0;
    	Print(SOUNDVOL(vol));
    end;

    if Pos(key, 'Q123456789ABC') <> 0 then
      result := key;
  until result <> '';
end;

procedure Page1;
var
  key : string;
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Console Layout');
  PrintLn;

  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' The layout of a page is zero or more rows of text.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' Each row of text contains zero or more columns of characters.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' Rows are numbers from 1. Columns are numbered from 1.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' Each character has attributes to alter their appearance.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' The default font size is such that there are 80 columns of text on a row.');
  PrintLn(RIGHT(3) + 'There is no guarantee of the exact number of columns, the width, or height');
  PrintLn(RIGHT(3) + 'of text smaller or larget than the default size.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' Rows can have attributes to change the size of the text on that row,');
  PrintLn(RIGHT(3) + 'define the background color for text on that row, or if the row is displayed');
  PrintLn(RIGHT(3) + 'in marquee mode.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' If the cursor is in the last visible column of a row and a character is');
  PrintLn(RIGHT(3) + 'displayed, the cursor will advance to column 1 of the next row.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' On insert, text is moved right from the cursor. Text pushed '
    + SGR([SGR_ITALICS]) + '''off the screen''' + SGR([SGR_ITALICSOFF]));
  PrintLn(RIGHT(3) + 'are not lost but are not displayed. If the cursor is moved right '
    + SGR([SGR_ITALICS]) + '''off the');
  PrintLn(RIGHT(3) + 'screen''' + SGR([SGR_ITALICSOFF]) + ', text can be wrtten to off screen. Only text with the 80');
  PrintLn(RIGHT(3) + 'column default font size area are physically displayed.');
  PrintLn(SGR(ANSI_YELLOW) + UNICODE($2660) + SGR(ANSI_LTCYAN)
    + ' All text sent and received to and from the server, uses UTF-8 encoding.');
  PrintLn;
  Print(SGR(ANSI_GREEN) + 'Press '+HOTSPOT(3,1,0,'Q')+'[' + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN)
    + '] when done : ');

  repeat
    key := upCase(GetKey);
  until key = 'Q';
end;

procedure Page2;
var
  key : string;
  cc, cs, co : integer;
  b, bg : integer;

  procedure PrintSyntaxLn(startcode, parameters, endcode, notes, vals: string);
  begin
    PrintLn(SGR(ANSI_YELLOW) + startcode
      + SGR([SGR_ITALICS]) + ' ' + parameters + ' '
      + SGR([SGR_ITALICSOFF]) + endcode
      + SGR(ANSI_LTGRAY) + ' : '
      + SGR(ANSI_LTCYAN) + notes + ' '
      + SGR(ANSI_CYAN) + vals);
  end;

begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Cursor / Page Attributes');
  PrintLn;

  PrintLn(SGR(ANSI_LTCYAN) + 'The appearance of the cursor and page can be altered using special ANSI code');
  PrintLn('sequences. ' + SGR([SGR_ITALICS]) + '(CSI = Escape ''['').' + SGR([SGR_ITALICSOFF]));
  PrintLn;
  PrintLn(SGR(ANSI_YELLOW) + 'Cursor Codes ' + SGR(ANSI_LTRED) + 'Alters the cursor appearance.');
  PrintLn;
  PrintSyntaxLn('CSI 0 ;', 'n', '^', 'Set Cursor Color.', 'n = (0-255:color). Def=7.');
  PrintLn;
  PrintSyntaxLn('CSI 1 ;', 'n', '^', 'Set Cursor Size.', 'n = (0:none, 1:thin, 2:thick, 3:full) Def=2.');
  PrintLn;
  PrintSyntaxLn('CSI 2 ;', 'n', '^', 'Set Cursor Orientation.', 'n = (0:horizontal, 1:vertical). Def=0.');
  PrintLn;
  PrintLn(SGR(ANSI_YELLOW) + 'Page Codes ' + SGR(ANSI_LTRED) + 'Alters the appearance of the page.');
  PrintLn;
  PrintSyntaxLn('CSI 3 ;', 'n', '^', 'Set Page Border Color.', 'n = (0-255:color). Def=0.');
  PrintLn;
  PrintSyntaxLn('CSI 4 ;', 'n', '^', 'Set Page Background Color.', 'n = (0-255:color). Def=0.');
  PrintLn;

  Print(SGR(ANSI_GREEN) + 'Press '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'0')+'0' + SGR(ANSI_GREEN) + ', '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'1')+'1' + SGR(ANSI_GREEN) + ', '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'2')+'2' + SGR(ANSI_GREEN) + ', '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'3')+'3' + SGR(ANSI_GREEN) + ', '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'4')+'4' + SGR(ANSI_GREEN)
    + ' to advance attribute value, '+HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN)
    + ']uit when done : ');

  cc := 7;
  cs := 2;
  co := 0;
  b := 0;
  bg := 0;
  repeat
    key := upCase(GetKey);
    case key of
      '0':
        begin
          inc(cc);
          if cc > 16 then cc := 1;
          Print(CSI + '0;' + inttostr(cc) + '^');
        end;
      '1':
        begin
          inc(cs);
          if cs > 3 then cc := 0;
          Print(CSI + '1;' + inttostr(cs) + '^');
        end;
      '2':
        begin
          inc(co);
          if co > 1 then co := 0;
          Print(CSI + '2;' + inttostr(co) + '^');
        end;
      '3':
        begin
          inc(b);
          if b > 16 then b := 1;
          Print(CSI + '3;' + inttostr(b) + '^');
        end;
      '4':
        begin
          inc(bg);
          if bg > 16 then bg := 1;
          Print(CSI + '4;' + inttostr(bg) + '^');
        end;
    end;
  until key = 'Q';
  Print(CSI + '0;7^' + CSI + '1;2^' + CSI + '2;0^' + CSI + '3;16^' + CSI + '4;16^');
end;

procedure PrintSyntaxLn(startcode, parameters, endcode, notes, vals: string);
begin
  PrintLn(SGR(ANSI_YELLOW) + startcode
    + SGR([SGR_ITALICS]) + ' ' + parameters + ' '
    + SGR([SGR_ITALICSOFF]) + endcode
    + SGR(ANSI_LTGRAY) + ' : '
    + SGR(ANSI_LTCYAN) + notes + ' '
    + SGR(ANSI_CYAN) + vals);
end;

procedure Page3;
var
  key : string;
  sz, w : integer;
  c1, c2, s, m : integer;

begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Line Attributes');
  PrintLn;

  PrintLn(SGR(ANSI_LTCYAN) + 'Each row of text can have individual attributes that effect its appearence.');
  PrintLn('The row containing the cursor will be altered when these ANSI code sequences');
  PrintLn('are sent. Backgrounding does not work well in BBS/ANSI.SYS mode as there is');
  PrintLn('no access to a transparent text background color.');
  PrintLn;
  PrintSyntaxLn('CSI', 's ; w', '[', 'Set Row Size.', 's = overall size (0:25%,..3:100%,..7=200%) Def=3.');
  PrintLn(Right(15) + 'w = width (0:50%,1:100%,2:150%,3:200%) Def=1.');
  PrintLn;
  PrintSyntaxLn('CSI', 'c ; d ; s', ']', 'Set Row Background.', 'c = color1 (0..255) Def=0,');
  PrintLn(Right(15) + 'd = color2 (0..255) Def=0, s = style (0:none, 1:solid color1,');
  PrintLn(Right(15) + '2:horizontal gradient, 3:vertical gradient) Def=0.');
  PrintLn;
  PrintSyntaxLn('ESC # 0', #8, #8, 'Marquee Mode Off.', 'Turns off marquee mode.');
  PrintLn;
  PrintSyntaxLn('ESC # 1', #8, #8, 'Marquee Mode On.', 'Will scroll the text right to left.');
  PrintLn;
  PrintSyntaxLn('ESC # 9', #8, #8, 'Reset All Row Attributes.', 'Returns row to default size and background.');
  PrintLn(Down(3) + 'SAMPLE ROW OF TEXT TO ILLUSTRATE THE EFFECTS OF ROW ATTRIBUTES.');

  Print(Up(3) + SGR(ANSI_GREEN) + 'Press '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'+')+'+/'+HOTSPOT(1,1,0,'-')+'-' + SGR(ANSI_GREEN) + ' alter size, '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'B')+'B' + SGR(ANSI_GREEN) + ' random background, '
    + SGR(ANSI_YELLOW) + HOTSPOT(1,1,0,'M')+'M' + SGR(ANSI_GREEN) + ' toggle maquee, '
    + HOTSPOT(3,1,0,'Q')+'[' + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');

  sz := 3;
  w := 1;
  m := 0;
  repeat
    key := upCase(GetKey);
    case key of
      '+':
        begin
          inc(sz);
          if sz > 7 then
          begin
            sz := 0;
            inc(w);
            if w > 3 then
              w := 0;
          end;
          Print(Down(2) + CSI + inttostr(sz) + ';' + inttostr(w) + '[' + Up(2));
        end;

      '-':
        begin
          dec(sz);
          if sz < 0 then
          begin
            sz := 7;
            dec(w);
            if w < 0 then
              w := 3;
          end;
          Print(Down(2) + CSI + inttostr(sz) + ';' + inttostr(w) + '[' + Up(2));
        end;

      'B':
        begin
          c1 := random(255) + 1;
          c2 := random(255) + 1;
          s := random(3) + 1;
          Print(Down(2) + CSI + inttostr(c1) + ';' + inttostr(c2) + ';'
            + inttostr(s) + ']' + Up(2));
        end;

      'M':
        begin
          inc(m);
          if m > 1 then m := 0;
          Print(Down(2) + ESC + '#' + inttostr(m) + Up(2));
        end;
    end;
  until key = 'Q';
end;

procedure Page4;
var
  key : string;

begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Character Attributes');
  PrintLn;

  PrintLn(SGR(ANSI_LTCYAN) + 'Individual characters on the page can be stylized using these ANSI sequences.');
  PrintLn;
  PrintSyntaxLn('CSI', 'n ; ...', 'm', 'Set Graphics Rendition (SGR).', 'n = values below. Def=0.');
  PrintLn;

  PrintLn(#27'[0;91m  0      '#27'[94m: Reset all to default values.');
  PrintLn(#27'[0;91m  1 / 21 '#27'[94m: '#27'[1mBold'#27'[21m on / off. (bright foreground color in BBS/ANSI.SYS mode).');
  PrintLn(#27'[0;91m  3 / 23 '#27'[94m: '#27'[3mItalics'#27'[23m on / off.');
  PrintLn(#27'[0;91m  4 / 24 '#27'[94m: '#27'[4mUnderline'#27'[24m on / off.       '#27'[91m 9 / 29 '#27'[94m: '#27'[9mStrikethrough'#27'[29m on / off.');
  PrintLn(#27'[0;91m  5 / 25 '#27'[94m: '#27'[5mBlink'#27'[25m on / off.           '#27'[91m50 / 70 '#27'[94m: '#27'[50mGlow'#27'[70m on / off.');
  PrintLn(#27'[0;91m  6 / 26 '#27'[94m: '#27'[6mBlink'#27'[26m on / off.           '#27'[91m 2 / 22 '#27'[94m: '#27'[2mFaint'#27'[22m on / off.');
  PrintLn(#27'[0;91m  7 / 27 '#27'[94m: '#27'[7mReverse video'#27'[27m on / off.   '#27'[91m56 / 76 '#27'[94m: '#27'[56mOutline'#27'[76m on / off.');
  PrintLn(#27'[0;91m  8 / 28 '#27'[94m: Concealed on / off.       '#27'[91m57 / 77 '#27'[94m: '#27'[100;57mShadow'#27'[40;77m on / off.');
  PrintLn;
  PrintLn(#27'[0;91m 30 - 37 '#27'[94m: Set foreground color 0-7. '#27'[91m40 / 47 '#27'[94m: Set background color 0-7.');
  PrintLn(#27'[0;91m 38;5; n '#27'[94m: Foreground color 0-255.   '#27'[91m48;5; n '#27'[94m: Background color 0-255.');
  PrintLn(#27'[0;91m 39      '#27'[94m: Default foreground color. '#27'[91m49      '#27'[94m: Default background color.');
  PrintLn(#27'[0;91m 90 - 97 '#27'[94m: Foreground color 8-15.    '#27'[91m100-107 '#27'[94m: Background color 8-15.');
  PrintLn;

  Print(SGR(ANSI_GREEN) + 'Press '+ HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');
  repeat
    key := upCase(GetKey);
  until key = 'Q';

end;


procedure Page5;
var
  c, i, j : integer;
  key : string;
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' ANSI Colors');
  PrintLn;

  Print(SGR(ANSI_LTCYAN) + 'Colors 0 - 7 are low intensity colors.    ');
  for i := 0 to 7 do
    Print(SGR(i) + ' ' + UNICODE($2586) + UNICODE($2586) + UNICODE($2586));
  PrintLn;

  Print(SGR(ANSI_LTCYAN) + 'Colors 8 - 15 are high intensity colors.  ');
  for i := 8 to 15 do
    Print(SGR(i) + ' ' + UNICODE($2586) + UNICODE($2586) + UNICODE($2586));
  PrintLn;
  PrintLn;

  PrintLn(SGR(ANSI_LTCYAN) + 'Colors 16 - 231 are based on a 6 x 6 x 6 color cube. They are as follows.');
  PrintLn;
  for i := 0 to 8 do
  begin
    for j := 0 to 23 do
    begin
      c := 16 + (i * 24) + j;
      Print(#27'[38;5;' + inttostr(c) + 'm ' + UNICODE($2586) + '' + UNICODE($2586));
    end;
    PrintLn;
  end;
  PrintLn;

  PrintLn(SGR(ANSI_LTCYAN) + 'Colors 16 - 231 are based on a 6 x 6 x 6 color cube. They are as follows.');
  PrintLn;
  for j := 0 to 23 do
  begin
    c := 232 + j;
    Print(SGR(c) + ' ' + UNICODE($2586) + UNICODE($2586));
  end;
  PrintLn;
  PrintLn;

  Print(SGR(ANSI_GREEN) + 'Press ' + HOTSPOT(3,1,0,'Q') + '['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');
  repeat
    key := upCase(GetKey);
  until key = 'Q';
end;


procedure Page6;
var
  key : string;
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Cursor / Editing Controls');
  PrintLn;

  PrintLn(SGR(ANSI_LTCYAN) + 'Movement ANSI code currently supported are:');
  PrintLn;
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m A '#27'[92m: Up.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m J '#27'[92m: Erase page. n=(0:sop, 1=eop, 2=all).');
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m B '#27'[92m: Down.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m K '#27'[92m: Erase row. n=(0:sol, 1=eol, 2=all).');
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m C '#27'[92m: Forward.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m L '#27'[92m: Insert n rows. Def=1.');
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m D '#27'[92m: Backward.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m M '#27'[92m: Delete n rows. Def=1.');
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m E '#27'[92m: Next row.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m P '#27'[92m: Delete n characters. Def=1.');
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m F '#27'[92m: Prvious row.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m X '#27'[92m: Erase n characters. Def=1.');
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m G '#27'[92m: Move to column.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m b '#27'[92m: Repeat last character n times. Def=1.');
  Print(#27'[0;93mCSI '#27'[3mr;c'#27'[23m H '#27'[92m: Move to r, c.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m @ '#27'[92m: Insert n characters. Def=1.');
  Print(#27'[0;93mCSI '#27'[3mn'#27'[23m I '#27'[92m: Right n tabs.');
  PrintLn(#27'[30G'#27'[93mCSI '#27'[3mn'#27'[23m Z '#27'[92m: Left n tabs.');
  Print(#27'[0;93mCSI s '#27'[92m: Save cursor r, c.');
  PrintLn(#27'[30G'#27'[93mCSI u '#27'[92m: Restore cursor r, c.');
  PrintLn(#27'[0;93mCSI '#27'[3mr;c'#27'[23m f '#27'[92m: Move to r, c.');
  PrintLn;

  Print(#27'[0;93mCSI ?50 h '#27'[92m: Set VTX mode on.');
  PrintLn(#27'[30G'#27'[93mCSI ?50 l '#27'[92m: Set BBS/ANSI.SYS mode on.');
  PrintLn;

  PrintLn(#27'[0;93mCSI 6 n '#27'[92m: Request cursor position. Reply is '#27'[93mCSI r;c R'#27'[92m.');
  PrintLn;

  PrintLn(#27'[0;93mCSI 0 c '#27'[92m: Request terminal ident. Reply is '#27'[93mCSI ?50;86;84;88 c'#27'[92m.');
  PrintLn;

  Print(SGR(ANSI_GREEN) + 'Press '+HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');
  repeat
    key := upCase(GetKey);
  until key = 'Q';
end;

procedure Page7;
var
  key : string;
  r, c, w, h, z : integer;
const
  testsvg : string = 'data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYXBoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+DQo8IS0tIENyZWF0b3I6IENvcmVsRFJBVyBYNiAtLT4NCjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWw6c3BhY2U9InByZXNlcnZlIiB3aWR0aD0iMy45ODYzNmluIiBoZWlnaHQ9IjMuOTg2MzZpbiIgdmVyc2lvbj0iMS4xIiBzdHlsZT0ic2hhcGUtcmVuZGVyaW5nOmdlb21ldHJpY1ByZWNpc2lvbjsgdGV4dC1yZW5kZXJpbmc6Z2VvbWV0cmljUHJlY2lzaW9uOyBpbWFnZS1yZW5kZXJpbmc6b3B0aW1pemVRdWFsaXR5OyBmaWxsLXJ1bGU6ZXZlbm9kZDsgY2xpcC1ydWxlOmV2ZW5vZGQiDQp2aWV3Qm94PSIwIDAgMjAyNyAyMDI3Ig0KIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj4NCiA8ZGVmcz4NCiAgPHN0eWxlIHR5cGU9InRleHQvY3NzIj4NCiAgIDwhW0NEQVRBWw0KICAgIC5maWwxIHtmaWxsOmJsYWNrfQ0KICAgIC5maWwyIHtmaWxsOiNBQTAwMDB9DQogICAgLmZpbDAge2ZpbGw6I0ZGRkY1NX0NCiAgIF1dPg0KICA8L3N0eWxlPg0KIDwvZGVmcz4NCiA8ZyBpZD0iTGF5ZXJfeDAwMjBfMSI+DQogIDxtZXRhZGF0YSBpZD0iQ29yZWxDb3JwSURfMENvcmVsLUxheWVyIi8+DQogIDxjaXJjbGUgY2xhc3M9ImZpbDAiIGN4PSIxMDEzIiBjeT0iMTAxMyIgcj0iMTAxMyIvPg0KICA8cGF0aCBjbGFzcz0iZmlsMSIgZD0iTTEwMTMgMTAxM2w0NyAwYzEzLDIwIDMzLDM4IDU4LDU0bDQxIDI2YzgsNSAxNCwxMCAxOCwxNyA0LDYgNiwxNCA2LDIzIDAsOSAtMiwxNyAtOCwyMyAtNSw2IC0xMiw5IC0yMSw5IC0xNSwwIC0zMiwtOCAtNTAsLTI1IC0xOCwtMTYgLTMzLC0zOCAtNDUsLTY2bC00MiAxODljNTMsMjggMTEyLDQyIDE3NSw0MiAxMjQsMCAxODYsLTYzIDE4NiwtMTkwIDAsLTI5IC01LC01NCAtMTQsLTc1IC00LC0xMCAtOCwtMTkgLTE0LC0yN2wxMzIgMCAwIDI4NCAxNzEgMCAwIC0yODQgMjk2IDBjMCw1MTcgLTQxOSw5MzYgLTkzNiw5MzZsMCAtOTM2eiIvPg0KICA8cGF0aCBjbGFzcz0iZmlsMSIgZD0iTTEwMTMgMTAxM2wtNjEgMCAwIC03MCAtMTA0IDAgMCAtNzAgMTQ5IDAgMCAtMTQyIC0zMTkgMCAwIDI4MiAtMTM0IDAgMCAtNTMgODYgMCAwIC0yMjkgLTM0MSAwIDAgMjI5IDg1IDAgMCA1MyAtMjk2IDBjMCwtNTE2IDQxOSwtOTM1IDkzNSwtOTM1bDAgOTM1eiIvPg0KICA8cGF0aCBjbGFzcz0iZmlsMiIgZD0iTTEwNjAgMTAxM2wyOTAgMGMtNiwtOSAtMTQsLTE4IC0yMiwtMjUgLTE1LC0xNCAtMzYsLTI5IC02MywtNDQgLTI3LC0xNSAtNDQsLTI2IC01MSwtMzMgLTcsLTcgLTEwLC0xNiAtMTAsLTI4IDAsLTggMiwtMTUgOCwtMjEgNSwtNSAxMywtOCAyMSwtOCAxOCwwIDM1LDggNTIsMjIgMTYsMTUgMzEsMzUgNDQsNjJsNDMgLTE3OGMtNDgsLTI1IC05OSwtMzggLTE1NSwtMzggLTEyMywwIC0xODQsNjQgLTE4NCwxOTIgMCwzNSA3LDY1IDIwLDg5IDIsNCA0LDcgNywxMHptNDIyIDBsMTcxIDAgMCAtNTMgODUgMCAwIC0yMjkgLTM0MSAwIDAgMjI5IDg1IDAgMCA1M3oiLz4NCiAgPHBhdGggY2xhc3M9ImZpbDIiIGQ9Ik05NTIgMTAxM2wtMjc0IDAgMCAyODQgMzE5IDAgMCAtMTQyIC0xNDkgMCAwIC03MCAxMDQgMCAwIC03MnptLTQwOCAwbC0xNzAgMCAwIDI4NCAxNzAgMCAwIC0yODR6Ii8+DQogPC9nPg0KPC9zdmc+DQo=';
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Sprites');
  PrintLn;

  // clear all and define a single test sprite.
  Print(APC + '0' + ST + APC + '0;1;' + testsvg + ST);

  PrintLn(#27'[94mSprites are small SVG clipart sent from the server to the client. This');
  PrintLn('is done in two steps. The first step is to define the sprite on the client.');
  PrintLn('The second is to display a sprite on the page. Unlike most ANSI sequences,');
  PrintLn('Sprite definition commands use APC (Application Program Command : ESC _) to');
  PrintLn('begin the definition and end with a ST (String Termination : ESC \).');
  PrintLn;
  PrintLn('Sprite definition sequences are as follows:');
  PrintLn;
  PrintLn(#27'[93mAPC 0 ST '#27'[94m: Clear all sprite definitions.');
  PrintLn(#27'[93mAPC 0;'#27'[3ms'#27'[23m ST '#27'[94m: Clear a sprite definition number s (1-64).');
  PrintLn(#27'[93mAPC 0;'#27'[3ms;svgdata'#27'[23m ST '#27'[94m: Define sprite s with svgdata (Base64 SVG file text).');
  PrintLn;
  PrintLn('Once a sprite has been defined client side, it can be displayed on the page');
  PrintLn('using:');
  PrintLn;
  PrintLn(#27'[93mCSI 0;'#27'[3mn;s;w;h;z'#27'[23m _ '#27'[94m: Display a sprite number n (1-64) defined as s at the');
  PrintLn('    current cursor position with size w and h (measured in default character');
  PrintLn('    sizes, A width of 1 equals 1 character width at default size, A height of');
  PrintLn('    1 equals 1 character height). Z = 0 if the sprite is to appear under the');
  PrintLn('    text, 1 if on top.');
  PrintLn(#27'[93mCSI 0;'#27'[3mn'#27'[23m _ '#27'[94m: Remove sprite n.');
  PrintLn(#27'[93mCSI 0 _ '#27'[94m: Remove all sprites.');
  PrintLn;

  Print(SGR(ANSI_GREEN) + 'Press ' + SGR(ANSI_YELLOW) + HOTSPOT(5,1,0,' ') + 'Space'
    + SGR(ANSI_GREEN)+' to randomly display a sprite, '+HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');
  repeat
    key := upCase(GetKey);
    if key = ' ' then
    begin
      w := random(16) + 3;
      h := random(10) + 3;
      r := random(20) + 1;
      c := random(80 - w) + 1;
      z := random(2);
      Print(CSI + 's' + MOVETO(r,c) + CSI + '0;1;1;' + inttostr(w) + ';'
        + inttostr(h) + ';' + inttostr(z) + '_' + CSI + 'u');
    end;
  until key = 'Q';
end;

procedure Page8;
var
  key : string;
	filenames : array [0..9] of string = (
		'ansi0.5.utf',
		'ansi0.utf',
		'ansi1.utf',
		'ansi2.utf',
		'ansi3.utf',
		'ansi4.utf',
		'ansi5.utf',
		'ansi6.utf',
		'ansi7.utf',
		'ansi8.utf'
  );
  i : integer;

begin
  i := 0;

  Print(BBSMODE);
  Print(#27'[1;10*r');

  repeat
  begin
    Print(CLS);

		PrintANSI(filenames[i]);
    inc(i); if i >= length(filenames) then i := 0;

    Print(CRLF + #27'[0m' + SGR(ANSI_GREEN) + 'Press any key for next or '
      + HOTSPOT(3,1,0,'Q') + '[' + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN)
      + ']uit when done: ');

    key := upCase(GetKey);
  end
  until key = 'Q';

  Print(#27'[1;0*r');
  Print(CLS + VTXMODE);
end;



procedure Page9;
var
  key : string;
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Hotspots');
  PrintLn;

  PrintLn(#27'[94mHotspots can be defined on a screen that can be clicked by the users');
  PrintLn('mouse. There are two types of hotspots, string and url. A string hotspot will');
  PrintLn('send text to the terminal if clicked. These are good for menu items and such.');
  PrintLn('The second type of hotspot are URL links. These will open in a new page if');
  PrintLn('clicked.');
  PrintLn;
  PrintLn('Sprite definition sequences are as follows:');
  PrintLn;
  PrintLn(#27'[93mCSI '#27'[3mt ; w ; h ; l ; a ; ... ; a'#27'[23m / '#27'[94m: Define a hotspot.');
  PrintLn('    t = type. 0 = string, 1 = url.');
  PrintLn('    w = width in columns.');
  PrintLn('    h = height in rows.');
  PrintLn('    l = highlight. 0 = none, 1 = on mouseover.');
  PrintLn('    a = unicode ascii character(s).');
  PrintLn;
  PrintLn('Samples:  Click ' + HOTSPOT(4,1,1,'Q') + #27'[95mTHIS'#27'[94m to return to main menu (defined as Q).');
  PrintLn;
  PrintLn('          Click ' + URL(4,1,1,'http://www.vtxemu.com') + #27'[95mTHIS'#27'[94m to open VTX home page.');
  PrintLn;

  Print(SGR(ANSI_GREEN) + 'Press '+HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');

  repeat
    key := upCase(GetKey);
  until key = 'Q';
end;

procedure PageA;
var
  key : string;
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Teletext Mosiac Block Mode');
  PrintLn;

  PrintLn(#27'[94mSpecial 2 x 3 block graphic characters as used on the BBC Micro, TRS-80, and');
  PrintLn('Teletext style screens can be accessed with special font select commands. While');
  PrintLn('this mode is on, characters SPACE through _ are rendered as block characters.');
  PrintLn('Full block and separated blocks are available in the two modes. Characters');
  PrintLn('outside this range are rendered using font 0.');
  PrintLn;
  PrintLn('Normal characters:');
  PrintLn;
  PrintLn(#27'[97m    ! " # $ % & '' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?');
  PrintLn('  @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ / ] ^ _');
  PrintLn;
  PrintLn(#27'[93mCSI 80 m'#27'[94m: Turn full block mode on (10 will turn if off).');
  PrintLn;
  PrintLn(#27'[97;80m    ! " # $ % & '' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?');
  PrintLn('  @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ / ] ^ _'#27'[10m');
  PrintLn;
  PrintLn(#27'[93mCSI 81 m'#27'[94m: Turn separated  block mode on (10 will turn if off).');
  PrintLn;
  PrintLn(#27'[97;81m    ! " # $ % & '' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?');
  PrintLn('  @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ / ] ^ _'#27'[10m');
  PrintLn;

  Print(SGR(ANSI_GREEN) + 'Press '+HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');

  repeat
    key := upCase(GetKey);
  until key = 'Q';
end;

procedure PageB;
var
  key : string;
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Font Support');
  PrintLn;

  Print(#27'[1;33 D'#27'[2;36 D'#27'[3;37 D'#27'[4;38 D'#27'[5;39 D'#27'[6;40 D'#27'[7;41 D'#27'[8;42 D');
  PrintLn(#27'[94mFont Select command allows the VTX client switch to alternate defined');
  PrintLn('fonts / codepage combinations.');
  PrintLn;

  PrintLn(#27'[93mCSI n ; f space D'#27'[94m: Define which font selection to be available for use.');
  PrintLn('    n : 0 - 9. Represents the font slot to define.');
  PrintLn('    f : the font number preset. 0 = default codepage defined at start of client');
  PrintLn('        (typically CP437). Others are listed in '+URL(9,1,1,'http://cvs.synchro.net/cgi-bin/viewcvs.cgi/*checkout*/src/conio/cterm.txt')+'cterm.txt.');
  PrintLn;
  PrintLn(#27'[93mCSI 10-19 m'#27'[94m: Select font to begin using for text.'#27'[93m');
  PrintLn;
  PrintLn('    '#27'[11mCommodore 64            '#27'[12mAtari');
  PrintLn;
  PrintLn('    '#27'[13mP0T-NOoDLE              '#27'[14mmOsOul');
  PrintLn;
  PrintLn('    '#27'[15mMicroKnight Plus        '#27'[16mTopaz Plus');
  PrintLn;
  PrintLn('    '#27'[17mMicroKnight             '#27'[18mTopaz');
  PrintLn;

  Print(#27'[10m'+SGR(ANSI_GREEN) + 'Press '+HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');

  repeat
    key := upCase(GetKey);
  until key = 'Q';
end;

procedure PageC;
var
  key : string;
begin
  Print(VTXMODE + CLS + HOME + SGR(ANSI_LTCYAN, [SGR_RESET]));
  PrintLn(RowColor(ANSI_BLUE, ANSI_BLACK, HorzGrad) + RowSize(200, 50)
    + ' Audio Support');
  PrintLn;

  PrintLn(#27'[94mAudio commands allow the VTX client to play either an audio object');
  PrintLn('or a streaming URL.');
  PrintLn;

  PrintLn('Audio definition sequences are as follows:');
  PrintLn;
  PrintLn(#27'[93mAPC 1 ST '#27'[94m: Clear all audio definitions.');
  PrintLn(#27'[93mAPC 1;'#27'[3ma'#27'[23m ST '#27'[94m: Clear a audio definition number a (1-64).');
  PrintLn(#27'[93mAPC 1;'#27'[3ma;data'#27'[23m ST '#27'[94m: Define audio object a with data (Base64 file text).');
  PrintLn;

  PrintLn('To play audio:');
  PrintLn;
  PrintLn(#27'[93mCSI 1 ; 0 ; n _'#27'[94m: Loads an audio object into the audio player. These are set');
  PrintLn('    with the APC 1 ST commands above.');
  PrintLn('    n : Audio object defined with APC 1 ST command.');
  PrintLn(#27'[93mCSI 1 ; 1 ; a ; ... ; a _'#27'[94m: Loads the audio player with online audio.');
  PrintLn('    a : unicode ascii characters to url.');
  PrintLn(#27'[93mCSI 1 ; 2 ; v _'#27'[94m: Set the volume. (0-100). Default=25');
  PrintLn('    a : unicode ascii characters to url.');
  PrintLn(#27'[93mCSI 1 ; 3 _'#27'[94m: Play.');
  PrintLn(#27'[93mCSI 1 ; 4 _'#27'[94m: Stop/Pause.');

  PrintLn;

  Print(#27'[10m'+SGR(ANSI_GREEN) + 'Press '+HOTSPOT(3,1,0,'Q')+'['
    + SGR(ANSI_YELLOW) + 'Q' + SGR(ANSI_GREEN) + ']uit when done: ');

  repeat
    key := upCase(GetKey);
  until key = 'Q';
end;

var
  selection : string;
  loop : boolean;

begin
  Init;

  randomize;

  Print(SOUNDVOL(25) + SOUNDSET('http://dirtydelilah.ds.sparrowindustries.net:7838/;'));

  loop := true;
  while loop do
  begin

    selection := Menu;
    case selection of
      '1':  Page1;
      '2':  Page2;
      '3':  Page3;
      '4':  Page4;
      '5':  Page5;
      '6':  Page6;
      '7':  Page7;
      '8':  Page8;
      '9':  Page9;
      'A':	PageA;
      'B':	PageB;
      'C':	PageC;

      'Q':
        begin
          MoveCursor(1,1);
          Print(CLS + 'Bye.' + CRLF);
          loop := false;
       end;
    end;
  end;

  Finish;
end.

