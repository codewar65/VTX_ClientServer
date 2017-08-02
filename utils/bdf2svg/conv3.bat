@echo off

bdf2svg -i atari.bdf               -o atari.svg

echo Open($1) > conv.pe
echo Generate($1:r+'.ttf') >> conv.pe
echo Generate($1:r+'.woff') >> conv.pe

"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe atari.svg

del conv.pe
