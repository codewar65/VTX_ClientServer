CC = bcc 
COPTS = -I. -c -w -DOS2

reczm.exe: reczm.obj pdcommo.obj error.obj 
  $(CC) reczm.obj pdcommo.obj error.obj 
  
reczm.obj: reczm.c pdcomm.h crcxm.h
  $(CC) $(COPTS) reczm.c 
  
pdcommo.obj: pdcommo.h pdcommo.c error.h
  $(CC) $(COPTS) pdcommo.c
  
error.obj: error.h
  $(CC) $(COPTS) error.c
