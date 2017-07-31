program ans2utf;
// convert ansi codepaged file to utf8
//
//	ans2utf codepage inputfile.ans outputfile.utf

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  StrUtils, Classes, SysUtils, LConvEncoding, Variants;

type
  // Code page stuff.
  TCodePages = (
	  CP1250, CP1251, CP1252, CP1253,
    CP1254, CP1255, CP1256, CP1257,
    CP1258, CP437, CP850, CP852,
    CP866, CP874, CP932, CP936,
    CP949, CP950, MACINTOSH, KOI8 );
	TCodePageConverter = function(const s: string): string;
  TCodePageUnconverter =  function(const s: string; SetTargetCodePage: boolean = false): RawByteString;

const
  FirstCP : TCodePages = CP1250;
  LastCP : TCodePages = KOI8;

  CodePageNames : array [ CP1250 .. KOI8 ] of string = (
    'CP1250', 'CP1251', 'CP1252', 'CP1253',
    'CP1254', 'CP1255', 'CP1256', 'CP1257',
    'CP1258', 'CP437', 'CP850', 'CP852',
    'CP866', 'CP874', 'CP932', 'CP936',
    'CP949', 'CP950', 'MACINTOSH', 'KOI8' );

	CodePageConverters : array [ CP1250 .. KOI8 ] of TCodePageConverter = (
    @CP1250ToUTF8, @CP1251ToUTF8, @CP1252ToUTF8, @CP1253ToUTF8,
    @CP1254ToUTF8, @CP1255ToUTF8, @CP1256ToUTF8, @CP1257ToUTF8,
    @CP1258ToUTF8, @CP437ToUTF8, @CP850ToUTF8, @CP852ToUTF8,
    @CP866ToUTF8, @CP874ToUTF8, @CP932ToUTF8, @CP936ToUTF8,
    @CP949ToUTF8, @CP950ToUTF8, @MACINTOSHToUTF8, @KOI8ToUTF8 );

  CodePageUnconverters : array [ CP1250 .. KOI8 ] of TCodePageUnconverter = (
    @UTF8ToCP1250, @UTF8ToCP1251, @UTF8ToCP1252, @UTF8ToCP1253,
    @UTF8ToCP1254, @UTF8ToCP1255, @UTF8ToCP1256, @UTF8ToCP1257,
    @UTF8ToCP1258, @UTF8ToCP437, @UTF8ToCP850, @UTF8ToCP852,
    @UTF8ToCP866, @UTF8ToCP874, @UTF8ToCP932, @UTF8ToCP936,
    @UTF8ToCP949, @UTF8ToCP950, @UTF8ToMACINTOSH, @UTF8ToKOI8 );

  CRLF = #13#10;

function Convert(cp : TCodePages; filename : string) : utf8string; register;
var
  fin : TextFile;
  linein : RawByteString;
  str : utf8string;
begin
  result := '';
  str := '';
  if fileexists(filename) then
  begin
    assign(fin, filename);
    reset(fin);
    while not eof(fin) do
    begin
      readln(fin, linein);
      str += CodePageConverters[cp](linein) + CRLF;
    end;
    closefile(fin);
    result := str;
  end;
end;

{ Convert strin to UTF-8 }
function ConvertFromCP(cp : TCodePages; strin : string) : string; register;
begin
  result := CodePageConverters[cp](strin);
end;

{ Convert strin to codepage }
function ConvertToCP(cp : TCodePages; strin : string) : RawByteString; register;
begin
  result := CodePageUnconverters[cp](strin);
end;

procedure help;
begin
  writeln('ans2utf -C codepage -I inputfile -O outputfile');
end;

var
  found : boolean;
  i : integer;
  codepage,
  infname,
 	outfname : string;
  cp : TCodePages;
  fout : Text;
  data : utf8string;
  arg : string;
  state : integer;

begin
  writeln('ans2utf - convert ansi file to utf8.' + CRLF
  	+ '(c) 2017 Dan Mecklenburg Jr.' + CRLF);

  {$region Get args }
  // get args
  codepage := 'CP437';
  infname := '';
  outfname := '';
  for i := 1 to argc - 1 do
  begin
    	arg := argv[i];
      case upcase(arg) of
        '--?','-?','/?','-H':
          begin
            help;
  	      	exit;
          end;

        '-I':	state := 1;
        '-O':	state := 2;
	      '-C': state := 3;
        else
          case state of
            0:
          		begin
          			writeln('Illegal argument : ' + arg);
            		exit;
          		end;

            1:
              begin
  	            // get input name
	              infname := arg;
	            	state := 0;
              end;

            2:
              begin
  	            // get output name
	              outfname := arg;
	            	state := 0;
              end;

            3:
              begin
  	            // get codepage
	              codepage := arg;
	            	state := 0;
              end;
          end;
      end;
  end;
	{$endregion}

  {$region Check args }
	// check args
  if (infname = '') then
  begin
    writeln('No input file specified.');
    exit;
  end;
  if not fileexists(infname) then
  begin
    writeln('Unable to locate file ' + infname + '.');
    exit;
  end;

  if (outfname = '') then
  begin
    writeln('No output file specified.');
    exit;
  end;

  // check codepages
  found := false;
	for cp := FirstCP to LastCP do
  begin
    if Upcase(codepage) = Upcase(CodePageNames[cp]) then
    begin
      found := true;
      break;
    end;
  end;
  if not found then
  begin
    writeln('Unknown codepage ' + codepage + '.');
    exit;
  end;
	{$endregion}

  // do it.
  data := Convert(cp, infname);
  assign(fout, outfname);
  rewrite(fout);
	write(fout, data);
  close(fout);

  writeln('Done.');
end.

