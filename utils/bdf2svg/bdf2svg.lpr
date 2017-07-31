program bdf2svg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  strutils, Classes, SysUtils;

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
    + '-o <output svg file>' + CRLF
    + '-9 : 9 bits wide from 8 bit set' + CRLF
    + '-s <start glyph num>' + CRLF
    + '-e <end glyph num>');
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
  i, j : integer;
  arg : string;
  bdfname, svgname : string;
  state : integer;
  startglyph, endglyph : integer;
	fin : textfile;						// bdf file input
  fout : textfile;					// svg font output
  linein : string;
  vals : TStringArray;
  charname : string;				// current glyph name
  enc : integer;						// current glyph code point
  x, y : integer;
  sx, sy, ex, ey : integer;
  sc, ec : TCorner;
  c0, c1, c2, c3 : boolean;
	poly : TStickArray;
  removed : boolean;
  font_name, weight_name, slant, style_name, face_name : string;
  font_ascent, font_descent : integer;
  default_char, cap_height, x_height : integer;
	path : string;
  dx, dy : integer;
  height : integer;
  vga9 : boolean;		// switch for carry over to bit 9 on special characters
  truewidth : integer;
  pixw : integer;		// last column pixel width

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
  for i := 1 to argc - 1 do
  begin
    	arg := upcase(argv[i]);
      case arg of
      	'-H':
          begin
            help;
  	      	exit;
          end;
        '-I':	state := 1;
        '-O':	state := 2;
	      '-S': state := 3;
				'-E': state := 4;
        '-9': vga9 := true;
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
          end;
      end;
  end;
	{$endregion}

  {$region Check args }
	// check args
  if (bdfname = '') then
  begin
    writeln('No input file specified.');
    exit;
  end;
  if not fileexists(bdfname) then
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

	assign(fin, bdfname);
  reset(fin);

  x_height := 16;
  height := 16;
  cap_height := 16;
  font_ascent := 16;
  font_descent := 0;
  weight_name := 'normal';
  style_name := 'normal';
  default_char := -1;

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

//            writeln(fout, '        cap-height="' + inttostr(cap_height * MULT) + '"');
//            writeln(fout, '        x-height="' + inttostr(x_height * MULT) + '"');
//            writeln(fout, '        ascent="' + inttostr(font_ascent * MULT) + '"');
//            writeln(fout, '        descent="' + inttostr(font_descent * MULT) + '">');
writeln(fout, '        cap-height="' + inttostr(height * MULT) + '"');
writeln(fout, '        x-height="' + inttostr(height * MULT) + '"');
writeln(fout, '        ascent="' + inttostr(height * MULT) + '"');
writeln(fout, '        descent="' + inttostr(0 * MULT) + '">');

            writeln(fout, '        <font-face-src>');
            writeln(fout, '          <font-face-name name="' + face_name + '"/>');
            writeln(fout, '        </font-face-src>');
            writeln(fout, '      </font-face>');
          end;

        'SIZE':
          height := strtoint(vals[1]);

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
          enc := strtoint(vals[1]);

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

            // convert to sticks
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
								write(fout, '      <missing-glyph '
              		+ 'horiz-adv-x=''' + inttostr(truewidth * MULT) + ''' '
                	+ 'd=''')
              else
              begin
                if enc < 256 then
		              write(fout, '      <glyph unicode=''' + Format('&#x%2.2x;', [enc])
	              		+ ''' horiz-adv-x=''' + inttostr(truewidth * MULT) + ''' '
                    + 'd=''')
                else
		              write(fout, '      <glyph unicode=''' + Format('&#x%4.4x;', [enc])
  	              	+ ''' horiz-adv-x=''' + inttostr(truewidth * MULT) + ''' '
                    + 'd=''');
              end;
              path := '';

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
								writeln(fout, path + '''></missing-glyph>')
              else
	              writeln(fout, path + '''></glyph>');

            end;
          end;
      end;
    end;
  end;
  // close bdf input.
  close(fin);

	// end of svg font file.
  writeln(fout, '    </font>');
  writeln(fout, '  </defs>');
  writeln(fout, '</svg>');
  close(fout);

  writeln('Done.');
end.

