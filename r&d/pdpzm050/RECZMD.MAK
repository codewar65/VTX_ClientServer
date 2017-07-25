CC = bcc 
COPTS = -I. -c -w -DMSDOS

reczm.exe: reczm.obj pdcommd.obj pdcommda.obj error.obj uart.obj lldos.obj
  $(CC) reczm.obj pdcommd.obj pdcommda.obj error.obj uart.obj lldos.obj
  
reczm.obj: reczm.c pdcomm.h crcxm.h
  $(CC) $(COPTS) reczm.c 
  
pdcommd.obj: pdcommd.h pdcommd.c error.h
  $(CC) $(COPTS) pdcommd.c
  
pdcommda.obj: pdcommda.h pdcommda.c
  $(CC) $(COPTS) pdcommda.c

uart.obj: uart.h uart.c
  $(CC) $(COPTS) uart.c
  
lldos.obj: lldos.h lldos.c
  $(CC) $(COPTS) lldos.c  
    
error.obj: error.h
  $(CC) $(COPTS) error.c
