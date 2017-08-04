program bdf2svg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  FileUtil, strutils, Classes, SysUtils;

const
  MULT = 32;

type

  TCorner = ( UpperRight, LowerRight, LowerLeft, UpperLeft, EndPoint );

  TStick = record
    x1, y1 : integer;
    corner1 : TCorner;
	  x2, y2 : integer;
  	corner2 : TCorner;
  end;

  TStickArray = array of TStick;


const
  CRLF = #13#10;

var
  pile : 			TStickArray;				// the pile
  startbit : 	integer;						// starting bit mask
  img : 			array of integer;		// current glyph image
	w, h : 			integer;						// current glyph size


// delete a stick from the pile.
procedure RemoveStick(
  var list : TStickArray;
  i : integer);

var
  len : integer;

begin
  // delete a stick.
  len := length(list);
  if (i >= 0) and (i < len) then
  begin
	  move(list[i + 1], list[i], sizeof(TStick) * (len - i));
  	setlength(list, len - 1);
  end
end;

// add a stick to the pile.
procedure AddStick(
  var list : TStickArray;
  x1, y1 : integer;
  corner1 : TCorner;
  x2, y2 : integer;
  corner2 : TCorner);

var
  i, len : integer;

begin
	// add a stick.
  // if already in list, don't add and remove the one that matches.
  if list = nil then
  	setlength(list, 0);

	len := length(list);
  for i := 0 to len - 1 do
  begin
  	if (((x1 = list[i].x1) and (y1 = list[i].y1) and
  	 		 (x2 = list[i].x2) and (y2 = list[i].y2)) or
  	    ((x1 = list[i].x2) and (y1 = list[i].y2) and
  			 (x2 = list[i].x1) and (y2 = list[i].y1))) then
      begin
        // remove this stick.
        RemoveStick(list, i);
        exit;
      end;
  end;
  // add new stick
  setlength(list, len + 1);
  list[len].x1 := x1;
  list[len].y1 := y1;
  list[len].corner1 := corner1;
  list[len].x2 := x2;
  list[len].y2 := y2;
  list[len].corner2 := corner2;
end;

procedure AddStick(
  var list : TStickArray;
  stick : TStick);
begin
  AddStick(
  	list,
    stick.x1, stick.y1, stick.corner1,
    stick.x2, stick.y2, stick.corner2);
end;

function VGA9Char(chr : integer) : boolean;
begin
  // unicode character ranges that carries bit 8 to 9
  result :=
  		((chr >= 9472) and (chr <= 9616)) or
  		((chr >= 9620) and (chr <= 9621)) or
  		((chr >= 9698) and (chr <= 9701)) or
      ((chr >= 57696) and (chr <= 57823));
end;


procedure help;
begin
  writeln('-h : help' + CRLF
  	+ '-i <input bdf file>' + CRLF
    + '-r <input raw file (8x16 CP437)>' + CRLF
    + '-o <output svg file>' + CRLF
    + '-9 : 9 bits wide from 8 bit set' + CRLF
    + '-s <start glyph num>' + CRLF
    + '-e <end glyph num>'  + CRLF
  	+ '-c : commodore encoding / PETSCII' + CRLF
    + '-d : double height (8x8 -> 8x16)' + CRLF
    + '-a : raw encoding (0-255 = $e000-$e0ff)' + CRLF
    );
end;


// return if pixel set in img. 0 = no, !0 = yes
function GetPixel(x, y : integer) : boolean;
begin
  if (x < 0) or (x >= w) or (y < 0) or (y >= h) then
    result := true
  else
  	result := (img[y] and (startbit >> x)) <> 0;
end;


// remove quotes from string
function Unquote(str : string) : string;
begin
	result := str.Replace('''', '');
	result := str.Replace('"', '');
end;

var
  also : integer;
  imgchecksum : 		integer;
  i : 							integer;
  arg : 						string;
  bdfname,
  svgname : 				string;
  rawname : 				string;
  state : 					integer;
  startglyph,
  endglyph : 				integer;
	fin : 						textfile;				// bdf file input
  bin : 						tfilestream;
  fout : 						textfile;				// svg font output
  linein : 					string;
  vals : 						TStringArray;
  charname : 				string;					// current glyph name
  enc : 						integer;				// current glyph code point
	poly : 						TStickArray;
  font_name,
  weight_name,
  slant,
  style_name,
  face_name : 			string;
  fname :						string;
  font_ascent,
  font_descent : 		integer;
  default_char,
  cap_height,
  x_height : 				integer;
  dx, dy : 					integer;
  height : 					integer;
  vga9 : 						boolean;				// switch for carry over to bit 9 on special characters
  double : 					boolean;				// switch for expanding 8x8 to 8x16
  truewidth : 			integer;
  chr : 						integer;
  fontbank : 				integer;				// bank # in raw file
  cbmmapping :			boolean;				// map characters as commodore. E000-E0FF PETSCII
  rawmapping :			boolean; 				// map to E000-E0FF
  seekto :					longint;
  base : integer;
  allglyphs : 			TStringList;

const
  cp437 : array [0..255] of integer = (
  	$0000, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
    $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
    $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
    $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
    $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
    $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
    $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
    $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
    $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
    $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
    $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
    $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
    $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
    $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
    $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
    $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
    $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
    $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0
  );

// generate svg font glyph for chr using current info.
function generate(truewidth : integer; enc : integer) : string;
var
  i, j : 						integer;
  x, y : 						integer;
  pixw : 						integer;
  sx, sy :					integer;
  ex, ey : 					integer;
  sc, ec : 					TCorner;
  c0, c1, c2, c3 : 	boolean;
  path : 						string;
  removed :					boolean;

begin
	// convert to sticks
  path := '';
  if (enc >= startglyph) and (enc <= endglyph) then
  begin
 		setlength(pile, 0);
    for y := 0 to h - 1 do
    begin
    	for x := 0 to w - 1 do
      begin
 				if GetPixel(x, y) then
        begin

          pixw := 1;
          if vga9 and VGA9Char(enc) and (x = 7) then
            pixw := 2;

          //  a 0 b
          // 3 [ ] 1   corners
          //  d 2 c
          // pixel on. add sticks.
          c0 := GetPixel(x, y - 1);
          c1 := GetPixel(x + 1, y);
          c2 := GetPixel(x, y + 1);
          c3 := GetPixel(x - 1, y);

          //   -X-
          //  |   |
          //   ---
          sc := LowerRight;
          if c3 or c0 then
            sc := EndPoint;
          ec := LowerLeft;
          if c0 or c1 then
            ec := EndPoint;
 					AddStick(pile, x, y, sc, x + pixw, y, ec);

           //   ---
           //  |   X
           //   ---
           sc := LowerLeft;
           if c0 or c1 then
             sc := EndPoint;
           ec := UpperLeft;
           if c1 or c2 then
             ec := EndPoint;
           AddStick(pile, x + pixw, y, sc, x + pixw, y + 1, ec);

           //   ---
           //  |   |
           //   -X-
           sc := UpperLeft;
           if c1 or c2 then
             sc := EndPoint;
           ec := UpperRight;
           if c2 or c3 then
             ec := EndPoint;
           AddStick(pile, x + pixw, y + 1, sc, x, y + 1, ec);

           //   ---
           //  X   |
           //   ---
           sc := UpperRight;
           if c2 or c3 then
             sc := EndPoint;
           ec := LowerRight;
             if c3 or c0 then
           ec := EndPoint;
           AddStick(pile, x, y + 1, sc, x, y, ec);
      	end;
    	end; // for x
		end; // for y
		// all sticks added.

    // write start of this chardef.
		if enc = default_char then
 			path := '<missing-glyph '
         + 'horiz-adv-x=''' + inttostr(truewidth * MULT) + ''' '
         + 'd='''
    else
    begin
//      if enc < 256 then
// 	     	path := '<glyph unicode=''' + Format('&#x%2.2x;', [enc])
//           + ''' horiz-adv-x=''' + inttostr(truewidth * MULT) + ''' '
//           + 'd='''
//      else
 	      path := '<glyph unicode=''' + Format('&#x%4.4x;', [enc])
 	        + ''' horiz-adv-x=''' + inttostr(truewidth * MULT) + ''' '
           + 'd=''';
    end;

    // pluck out polygons until sticks in pile are gone.
    // mind the corners
    while length(pile) > 0 do
    begin
    	// get a stick off top of pile
      setlength(poly, 0);

      sx := pile[0].x1;
      sy := pile[0].y1;
      sc := pile[0].corner1;

      ex := pile[0].x2;
      ey := pile[0].y2;
      ec := pile[0].corner2;

      AddStick(poly, pile[0]);
      RemoveStick(pile, 0);

      // find connecting sticks
      repeat
        i := length(pile) - 1;
        while i >= 0 do
   	  	begin

 					if (ex = pile[i].x1) and (ey = pile[i].y1) and (ec = pile[i].corner1) then
 	      	begin
          	// adjust end point.
          	ex := pile[i].x2;
          	ey := pile[i].y2;
          	ec := pile[i].corner2;

            // move to poly.
            AddStick(poly, pile[i]);
            RemoveStick(pile, i);
            break;
          end;

          if (ex = pile[i].x2) and (ey = pile[i].y2) and (ec = pile[i].corner2) then
          begin
            // adjust end point
            ex := pile[i].x1;
            ey := pile[i].y1;
            ec := pile[i].corner1;

 						// move reverse to poly.
            AddStick(poly,
              pile[i].x2, pile[i].y2, pile[i].corner2,
              pile[i].x1, pile[i].y1, pile[i].corner1);
            RemoveStick(pile, i);
            break;
          end;
          dec(i);
        end;
      until (sx = ex) and (sy = ey);

     	// remove midpoints
     	repeat
        removed := false;
        for i := 0 to length(poly) - 2 do
        begin
 					for j := i + 1 to length(poly) - 1 do
          begin
          	if (poly[i].x1 = poly[i].x2) and
					 		(poly[i].x2 = poly[j].x1) and
              (poly[j].x1 = poly[j].x2) and
              (poly[i].y2 = poly[j].y1) then
            begin
            	// combine
              poly[i].x2 := poly[j].x2;
              poly[i].y2 := poly[j].y2;
              poly[i].corner2 := poly[j].corner2;
              RemoveStick(poly, j);
              removed := true;
            end;

            if (poly[i].y1 = poly[i].y2) and
 							(poly[i].y2 = poly[j].y1) and
              (poly[j].y1 = poly[j].y2) and
              (poly[i].x2 = poly[j].x1) then
            begin
              // combine
              poly[i].x2 := poly[j].x2;
              poly[i].y2 := poly[j].y2;
              poly[i].corner2 := poly[j].corner2;
              RemoveStick(poly, j);
              removed := true;
            end;
            if removed then break;
          end;
         	if removed then break;
       	end;
     	until not removed;

 			// output this poly path
     	//    from V----------------V to
     	//<path d="M0,0h200v200h-200z"/></missing-glyph>
     	// logic for using H and V instead of L
     	dy := poly[0].y2 - poly[0].y1;
     	case poly[0].corner1 of
         EndPoint:
           path += 'M' + format('%d,%d',   [ (poly[0].x1 * MULT),     ((h - poly[0].y1) * MULT) ]);

         UpperLeft:
           if dy = 0 then
             path += 'M' + format('%d,%d', [ (poly[0].x1 * MULT) - 1, ((h - poly[0].y1) * MULT) ])
           else
             path += 'M' + format('%d,%d', [ (poly[0].x1 * MULT), 	   ((h - poly[0].y1) * MULT) + 1 ]);

         UpperRight:
           if dy = 0 then
             path += 'M' + format('%d,%d', [ (poly[0].x1 * MULT) + 1, ((h - poly[0].y1) * MULT) ])
           else
             path += 'M' + format('%d,%d', [ (poly[0].x1 * MULT),     ((h - poly[0].y1) * MULT) + 1 ]);

         LowerRight:
         	if dy = 0 then
             path += 'M' + format('%d,%d', [ (poly[0].x1 * MULT) + 1, ((h - poly[0].y1) * MULT) ])
           else
             path += 'M' + format('%d,%d', [ (poly[0].x1 * MULT),     ((h - poly[0].y1) * MULT) - 1 ]);

         LowerLeft:
           if dy = 0 then
             path += 'M' + format('%d,%d',
               [ (poly[0].x1 * MULT) - 1, ((h - poly[0].y1) * MULT) ])
           else
             path += 'M' + format('%d,%d',
 	      			[ (poly[0].x1 * MULT),     ((h - poly[0].y1) * MULT) - 1 ]);
     	end;

 			for i := 0 to length(poly) - 1 do
     	begin
       	dy := poly[i].y2 - poly[i].y1;
       	case poly[i].corner2 of
           EndPoint:
             path += format(' L%d,%d',
               [ (poly[i].x2 * MULT),     ((h - poly[i].y2) * MULT) ]);

           UpperLeft:
             if dy = 0 then
 	            path += format(' L%d,%d L%d,%d',
 		            [ (poly[i].x2 * MULT) - 1, ((h - poly[i].y2) * MULT),
 		              (poly[i].x2 * MULT),     ((h - poly[i].y2) * MULT) + 1 ])
             else
 	            path += format(' L%d,%d L%d,%d',
 		           	[ (poly[i].x2 * MULT), 		 ((h - poly[i].y2) * MULT) + 1,
 	  	         		(poly[i].x2 * MULT) - 1, ((h - poly[i].y2) * MULT) ]);

           UpperRight:
             if dy = 0 then
 	            path += format(' L%d,%d L%d,%d',
 		           	[ (poly[i].x2 * MULT) + 1, ((h - poly[i].y2) * MULT),
 		           		(poly[i].x2 * MULT),     ((h - poly[i].y2) * MULT) + 1 ])
             else
 	            path += format(' L%d,%d L%d,%d',
 		           	[ (poly[i].x2 * MULT), 		 ((h - poly[i].y2) * MULT) + 1,
 	  	         		(poly[i].x2 * MULT) + 1, ((h - poly[i].y2) * MULT) ]);

           LowerRight:
             if dy = 0 then
 	            path += format(' L%d,%d L%d,%d',
 		           	[ (poly[i].x2 * MULT) + 1, ((h - poly[i].y2) * MULT),
 		         		  (poly[i].x2 * MULT),     ((h - poly[i].y2) * MULT) - 1 ])
             else
 	            path += format(' L%d,%d L%d,%d',
 		            [ (poly[i].x2 * MULT), 		 ((h - poly[i].y2) * MULT) - 1,
 	  	            (poly[i].x2 * MULT) + 1, ((h - poly[i].y2) * MULT) ]);

           LowerLeft:
             if dy = 0 then
 	            path += format(' L%d,%d L%d,%d',
 		            [ (poly[i].x2 * MULT) - 1, ((h - poly[i].y2) * MULT),
 		              (poly[i].x2 * MULT),     ((h - poly[i].y2) * MULT) - 1 ])
             else
 	            path += format(' L%d,%d L%d,%d',
 		            [ (poly[i].x2 * MULT), 		 ((h - poly[i].y2) * MULT) - 1,
 	  	            (poly[i].x2 * MULT) - 1, ((h - poly[i].y2) * MULT) ]);
         end;
       end;

     	// remove the last L#,# and make it autoclose
 			for i := length(path) - 1 downto 0 do
     	begin
       	if path.Chars[i] = 'L' then
       	begin
         	dx := i;
         	break;
       	end;
     	end;
     	path := path.substring(0, dx) + 'Z';
		end;
 	  // write end of chardef.
 		if enc = default_char then
 			path := '''></missing-glyph>'
 	  else
 		  path += '''></glyph>';
  end;
  result := path;
end;

begin
  writeln('bdf2svg - convert bsd bitmap font to svg vector font.' + CRLF
  	+ '(c) 2017 Dan Mecklenburg Jr.' + CRLF);

  {$region Get args }
  // get args
  state := 0;
  bdfname := 'u_vga16.bdf';
  svgname := 'test.svg';
  startglyph := 0;
  endglyph := 65535;
  vga9 := false;
  double := false;
  fontbank := 0;
  cbmmapping := false;
  rawmapping := false;
  for i := 1 to argc - 1 do
  begin
    	arg := argv[i];
      case upcase(arg) of
      	'-H':
          begin
            help;
  	      	exit;
          end;
        '-I':	state := 1;			// input file
        '-R': state := 5;			// raw files 0-31 -> $2400 rest go 32+ (256 glyphs)
        '-F': state := 6;			// font bank (n x font size is start)
        '-O':	state := 2;			// output file
	      '-S': state := 3;			// start
				'-E': state := 4;			// end
        '-9': vga9 := true;  	// 9 pixels wide
        '-D': double := true;	// double height of 8x8 font
        '-C': cbmmapping := true;
        '-A': rawmapping := true;
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
                rawname := '';
	              bdfname := arg;
	            	state := 0;
              end;

            2:
              begin
  	            // get output name
	              svgname := arg;
	            	state := 0;
              end;

            3:
              begin
  	            // get start
	              startglyph := strtoint(arg);
	            	state := 0;
              end;

            4:
              begin
  	            // get end
	              endglyph := strtoint(arg);
	            	state := 0;
              end;

            5:
              begin
  	            // get input name
                bdfname := '';
	              rawname := arg;
	            	state := 0;
              end;
            6:
              begin
                // font bank
                fontbank := strtoint(arg);
                state := 0;
              end;
          end;
      end;
  end;
	{$endregion}

  {$region Check args }
	// check args
  if (bdfname = '') and (rawname = '') then
  begin
    writeln('No input file specified.');
    exit;
  end;
  if not fileexists(bdfname) and not fileexists(rawname) then
  begin
    writeln('Unable to locate file ' + bdfname + '.');
    exit;
  end;
  if (svgname = '') then
  begin
    writeln('No output file specified.');
    exit;
  end;
  if startglyph > endglyph then
  begin
    writeln('Start is greater then end.');
    exit;
  end;
	{$endregion}

  // do it.
  assign(fout, svgname);
  rewrite(fout);

  writeln(fout, '<?xml version="1.0" standalone="yes"?>');
  writeln(fout, '<svg width="100%" height="100%" version="1.1" xmlns = "http://www.w3.org/2000/svg">');
  writeln(fout, '  <defs>');

  x_height := 16;
  height := 16;
  cap_height := 16;
  font_ascent := 16;
  font_descent := 0;
  weight_name := 'normal';
  style_name := 'normal';
  default_char := -1;

  if bdfname <> '' then
  begin
  	assign(fin, bdfname);
    reset(fin);

	  while not eof(fin) do
	  begin
	   	readln(fin, linein);
	    if linein <> '' then
	    begin
	    	vals := linein.split([' '], '"');
	      case upcase(vals[0]) of
	        'ENDPROPERTIES':
	          begin
	            // figure out the numbers!
	            writeln(fout, '    <font id="font" horiz-adv-x="0">');
	            writeln(fout, '      <font-face');
	            writeln(fout, '        font-family="' + face_name + '"');
	            writeln(fout, '        font-weight="' + weight_name + '"');
	            writeln(fout, '        font-style="' + style_name + '"');
	            writeln(fout, '        units-per-em="' + inttostr(height * MULT) + '"');
							writeln(fout, '        cap-height="' + inttostr(height * MULT) + '"');
							writeln(fout, '        x-height="' + inttostr(height * MULT) + '"');
							writeln(fout, '        ascent="' + inttostr(height * MULT) + '"');
							writeln(fout, '        descent="' + inttostr(0 * MULT) + '">');
	            writeln(fout, '        <font-face-src>');
	            writeln(fout, '          <font-face-name name="' + face_name + '"/>');
	            writeln(fout, '        </font-face-src>');
	            writeln(fout, '      </font-face>');
	          end;

//	        'SIZE':
//	          height := strtoint(vals[1]);

	        'X_HEIGHT':
	          x_height := strtoint(vals[1]);

	        'CAP_HEIGHT':
	          cap_height := strtoint(vals[1]);

					'DEFAULT_CHAR':
	          default_char := strtoint(vals[1]);

	        'FONT':
	          font_name := vals[1];

	        'WEIGHT_NAME':
	          weight_name := Unquote(vals[1]);

	        'SLANT':
	          begin
	          	slant := Unquote(vals[1]);
							if slant = 'I' then
	            	style_name := 'italics'
	            else
	          		style_name := 'normal';
	          end;

	        'FACE_NAME':
	          face_name := Unquote(vals[1]);

	        'FONT_ASCENT':
	        	font_ascent := strtoint(vals[1]);

	        'FONT_DESCENT':
	        	font_descent := strtoint(vals[1]);

	        'STARTCHAR':
	          charname := vals[1];

	        'ENCODING':
            begin
	          	enc := strtoint(vals[1]);
		      	  if enc < 32 then
  							enc := $2400 + enc
            end;

	        'BBX':
	          begin
	  	      	h := strtoint(vals[2]);
		          w := strtoint(vals[1]);
	            truewidth := w;
	            if vga9 then
	            	if w = 8 then
	              	inc(truewidth)
	              else
	                vga9 := false;
	            startbit := 128 << (((w-1) >> 3) * 8);
	          end;

	        'BITMAP':
	          begin
	            writeln('Character : ' + inttostr(enc));

	            // read bitmap in
	            setlength(img, h);
	            for i := 0 to h - 1 do
	            begin
	            	readln(fin, linein);
	            	img[i] := hex2dec(linein);
	            end;
	            // bitmap image for glyph loaded.

							writeln(fout, generate(truewidth, enc));

	          end;
	      end;
	    end;
	  end;
    // close bdf input.
    close(fin);
  end
  else
  begin
		// raw binary file (256 characters)
    bin := tfilestream.create(rawname, fmOpenRead);
		fname := ExtractFileNameWithoutExt(svgname);

    // skip to fontbank
    seekto := fontbank * 16 * 256;
    if double then seekto := seekto div 2;
    bin.Seek(seekto, soFromBeginning);

    height := 16;
    x_height := 16;
    cap_height := 16;
    font_name := rawname;
    weight_name := 'Medium';
    slant := 'R';
    face_name := rawname;
    font_ascent := 16;
    font_descent := 0;
    h := 16;
    w := 8;

    truewidth := w;
    if vga9 then
    	if w = 8 then
      	inc(truewidth)
      else
        vga9 := false;
    startbit := 128 << (((w-1) >> 3) * 8);

    writeln(fout, '    <font id="font" horiz-adv-x="0">');
    writeln(fout, '      <font-face');
    writeln(fout, '        font-family="' + fname + '"');
    //writeln(fout, '        font-family="' + face_name + '"');
    writeln(fout, '        font-weight="' + weight_name + '"');
    writeln(fout, '        font-style="' + style_name + '"');
    writeln(fout, '        units-per-em="' + inttostr(height * MULT) + '"');
		writeln(fout, '        cap-height="' + inttostr(height * MULT) + '"');
  	writeln(fout, '        x-height="' + inttostr(height * MULT) + '"');
  	writeln(fout, '        ascent="' + inttostr(height * MULT) + '"');
  	writeln(fout, '        descent="' + inttostr(0 * MULT) + '">');
    writeln(fout, '        <font-face-src>');
    writeln(fout, '          <font-face-name name="' + fname + '"/>');
    //writeln(fout, '          <font-face-name name="' + face_name + '"/>');
    writeln(fout, '        </font-face-src>');
    writeln(fout, '      </font-face>');

    default_char := -1;
		writeln(fout, '<missing-glyph '
       + 'horiz-adv-x=''' + inttostr(truewidth * MULT) + ''' '
       + 'd=''''></missing-glyph>');

    allglyphs := TStringList.Create();

    for chr := 0 to 255 do
		begin

      also := -1;
     	base := -1;
      if cbmmapping then
      begin
        // map commodore rom chars to petscii codepoints
        // 	move 0x00 - 0x1F  to  0x40 - 0x5F
        //	move 0x20 - 0x3F	to	0x20 - 0x3F
        //	move 0x40 - 0x5F	to	0x60 - 0x7F / 0xC0 - 0xDF
        //	move 0x60 - 0x7F	to 	0xA0 - 0xBF / 0xE0 - 0xFF
        case chr of
          $00..$1F:
            begin
    					base := chr + $40;
              enc := chr + $E040;
            end;
          $20..$3f:
            begin
    					base := chr;
              enc := chr + $E000;
            end;
         	$40..$5F:
            begin
              base := chr - $40 + $60;
              enc := chr - $40 + $E060;
              also := chr - $40 + $E0C0;
            end;
          $60..$7F:
            begin
              enc := chr - $60 + $E0A0;
              also := chr - $60 + $E0E0;
            end;
          else
            // don't need reversed glyphs
            break;
        end;

			end
      else if rawmapping then
      begin
        case chr of
          $00..$1F:
            begin
              enc := chr + $E000;
            end;
          $20..$7F:
            begin
    					base := chr;
              enc := chr + $E000;
            end;
          $80..$FF:
            begin
              enc := chr + $E000;
            end;
        end;
      end
      else
      begin
    	  if (chr >= 32) then
					enc := chr
        else
	   	 		enc := chr + $2400;
      end;

      writeln('Character : ' + inttostr(enc));

      // read bitmap in
      imgchecksum := 0;
      setlength(img, h);
      if double then
	      for i := 0 to 7 do
		    begin
  		   	img[i*2] := bin.readbyte;
  		   	img[i*2+1] := img[i*2];
    		  imgchecksum += img[i];
	      end
      else
	      for i := 0 to 15 do
  	    begin
    	   	img[i] := bin.readbyte;
      	  imgchecksum += img[i];
        end;

      allglyphs.Add(generate(truewidth, enc));
      if base >= 0 then
	      allglyphs.Add(generate(truewidth, base));
      if also >= 0 then
      	allglyphs.Add(generate(truewidth, also));
    end;

		allglyphs.Sort;

    for i := 0 to allglyphs.Count-1 do
    	writeln(fout, allglyphs[i]);

    bin.free;

  end;

	// end of svg font file.
  writeln(fout, '    </font>');
  writeln(fout, '  </defs>');
  writeln(fout, '</svg>');
  close(fout);

  writeln('Done.');
end.

