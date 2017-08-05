program needed;

// read unicode numbers from list of required glyphs
// compare to glyph defs in bdf file
// output list of mission glyphs in bdf

uses
	strutils, sysutils;

var
  listin : 	Text;
  bdfin : 	Text;
  needout : Text;
  line : 		String;
  chrnum : 	LongInt;
  len : 		Integer;
  glyphs : 	array of LongInt;
  i : 			Integer;
  found : 	Boolean;

begin

  WriteLn('needed.');
  WriteLn('// read unicode numbers from list of required glyphs');
  WriteLn('// compare to glyph defs in bdf file');
  WriteLn('// output list of mission glyphs in bdf');


  // read in what glyphs we have devined in bdf
  Assign(bdfin, 'u_vga16.bdf');
  Reset(bdfin);
  writeln(#10#13'Reading defined glyphs');
  SetLength(glyphs, 0);
	while not EOF(bdfin) do
  begin
    ReadLn(bdfin, line);
    // look for ENCODING <decimal valeu>
    if Upcase(copy(line, 1, 9)) = 'ENCODING ' then
    begin
      chrnum := StrToInt(copy(line, 10));
      len := length(glyphs);
      setlength(glyphs, len +1);
      glyphs[len] := chrnum;
		end;
  end;
  writeln(inttostr(length(glyphs)) + ' read from bdf.');
  closefile(bdfin);

  Assign(listin, 'needed.txt');
  Reset(listin);
  Assign(needout, 'needed.out');
  Rewrite(needout);
  writeln(#10#13'Reading needed.txt');
	while not EOF(listin) do
  begin
    ReadLn(listin, line);
    chrnum := -1;
    if Length(line) > 0 then
	  begin
	    if Copy(line, 1, 2) = '0x' then
        chrnum := Hex2Dec(Copy(line, 3))
	    else if copy(line, 1, 1) = '$' then
        chrnum := Hex2Dec(Copy(line, 2))
	    else
        chrnum := StrToInt(line);

			if chrnum >= 0 then
  	  begin
	      // look in list
  	    found := false;
    	  for i := 0 to length(glyphs) - 1 do
      		if glyphs[i] = chrnum then
        	begin
        		found := true;
	          break;
  	      end;
				if not found then
      		writeln(needout, inttohex(chrnum, 4));
      end;
    end;
  end;
  closefile(listin);
  closefile(needout);

end.

