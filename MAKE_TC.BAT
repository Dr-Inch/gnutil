tcc -c -mc command.c
tcc -c -mc f_scan.c
tcc -c -mc gauss.c
tcc -c -mc newton.c
tlink c:\tc\lib\c0c command f_scan gauss newton,gnutil.exe,dummy,c:\tc\lib\mathc.lib c:\tc\lib\cc.lib c:\tc\lib\emu.lib

