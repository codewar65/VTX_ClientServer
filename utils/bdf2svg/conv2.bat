@echo off

bdf2svg -c -d -f 0 -r c64.bin      -o c64_0.svg
bdf2svg -c -d -f 1 -r c64.bin      -o c64_1.svg
bdf2svg -c -d -f 0 -r c128.bin     -o c128_0.svg
bdf2svg -c -d -f 1 -r c128.bin     -o c128_1.svg
bdf2svg -c -d -f 0 -r vic20.bin    -o vic20_0.svg
bdf2svg -c -d -f 1 -r vic20.bin    -o vic20_1.svg
bdf2svg -c -d -f 0 -r superpet.bin -o superpet_0.svg
bdf2svg -c -d -f 1 -r superpet.bin -o superpet_1.svg
bdf2svg -c -d -f 0 -r pet-1.bin    -o pet-1.svg

echo Open($1) > conv.pe
echo Generate($1:r+'.ttf') >> conv.pe
echo Generate($1:r+'.woff') >> conv.pe

"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe c64_0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe c64_1.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe c128_0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe c128_1.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe vic20_0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe vic20_1.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe superpet_0.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe superpet_1.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe pet-1.svg

del conv.pe
