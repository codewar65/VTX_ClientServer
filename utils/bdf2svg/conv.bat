@echo off

REM - convert to SVG
bdf2svg -a -r MicroKnightPlus_v1.0.raw  -o MICROKNIGHTPLUS.svg
bdf2svg -a -r MicroKnight_v1.0.raw      -o MICROKNIGHT.svg
bdf2svg -a -r mO'sOul_v1.0.raw          -o MOSOUL.svg
bdf2svg -a -r P0T-NOoDLE_v1.0.raw       -o P0TNOODLE.svg
bdf2svg -a -r TopazPlus_a1200_v1.0.raw  -o TOPAZPLUS.svg
rem bdf2svg -r TopazPlus_a500_v1.0.raw   -o TOPAZPLUS_A500.svg
bdf2svg -a -r Topaz_a1200_v1.0.raw      -o TOPAZ.svg
rem bdf2svg -r Topaz_a500_v1.0.raw       -o TOPAZ_A500.svg
bdf2svg -i u_vga16.bdf               -o UVGA16.svg
bdf2svg -c -d -f 0 -r c64.bin      -o C640.svg
bdf2svg -c -d -f 1 -r c64.bin      -o C641.svg
bdf2svg -c -d -f 0 -r c128.bin     -o C1280.svg
bdf2svg -c -d -f 1 -r c128.bin     -o C1281.svg
bdf2svg -c -d -f 0 -r vic20.bin    -o VIC200.svg
bdf2svg -c -d -f 1 -r vic20.bin    -o VIC201.svg
rem bdf2svg -c -d -f 0 -r superpet.bin -o superpet_0.svg
rem bdf2svg -c -d -f 1 -r superpet.bin -o superpet_1.svg
rem bdf2svg -c -d -f 0 -r pet-1.bin    -o pet-1.svg
rem bdf2svg -i atari.bdf               -o ATARI.svg
bdf2svg -a -d -f 0 -r atari.bin    -o ATARI.svg

REM - convert to WOFF
echo Open($1) > conv.pe
rem echo Generate($1:r+'.ttf') >> conv.pe
echo Generate($1:r+'.woff') >> conv.pe

"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe MICROKNIGHTPLUS.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe MICROKNIGHT.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe MOSOUL.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe P0TNOODLE.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe TOPAZPLUS.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe TOPAZPLUS_A500.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe TOPAZ.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe TOPAZ_A500.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe UVGA16.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe C640.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe C641.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe C1280.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe C1281.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe VIC200.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe VIC201.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe superpet_0.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe superpet_1.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe pet-1.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe ATARI.svg

del *.svg
del conv.pe
