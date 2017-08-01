@echo off

bdf2svg -r MicroKnightPlus_v1.0.raw  -o MicroKnightPlus_v1.0.svg
bdf2svg -r MicroKnight_v1.0.raw      -o MicroKnight_v1.0.svg
bdf2svg -r mO'sOul_v1.0.raw          -o mO'sOul_v1.0.svg
bdf2svg -r P0T-NOoDLE_v1.0.raw       -o P0T-NOoDLE_v1.0.svg
bdf2svg -r TopazPlus_a1200_v1.0.raw  -o TopazPlus_a1200_v1.0.svg
bdf2svg -r TopazPlus_a500_v1.0.raw   -o TopazPlus_a500_v1.0.svg
bdf2svg -r Topaz_a1200_v1.0.raw      -o Topaz_a1200_v1.0.svg
bdf2svg -r Topaz_a500_v1.0.raw       -o Topaz_a500_v1.0.svg
bdf2svg -i u_vga16.bdf               -o u_vga16.svg

echo Open($1) > conv.pe
echo Generate($1:r+'.ttf') >> conv.pe
echo Generate($1:r+'.woff') >> conv.pe

"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe MicroKnightPlus_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe MicroKnight_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe mO'sOul_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe P0T-NOoDLE_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe TopazPlus_a1200_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe TopazPlus_a500_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe Topaz_a1200_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe Topaz_a500_v1.0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe u_vga16.svg

del conv.pe
