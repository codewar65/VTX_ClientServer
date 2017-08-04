@echo off

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
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe vic20_0.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe vic20_1.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe superpet_0.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe superpet_1.svg
rem "c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe pet-1.svg
"c:\program files (x86)\FontForgeBuilds\bin\fontforge.exe" -script conv.pe ATARI.svg

del conv.pe
