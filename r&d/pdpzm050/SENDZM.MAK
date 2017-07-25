CC = bcc 
COPTS = -I. -c -w -DOS2

sendzm.exe: sendzm.obj pdcommo.obj error.obj m2x.obj
  $(CC) sendzm.obj pdcommo.obj error.obj m2x.obj
  
sendzm.obj: sendzm.c pdcomm.h crcxm.h
  $(CC) $(COPTS) sendzm.c 
  
pdcommo.obj: pdcommo.h pdcommo.c error.h
  $(CC) $(COPTS) pdcommo.c
  
error.obj: error.h
  $(CC) $(COPTS) error.c

m2x.obj: m2x.h
  $(CC) $(COPTS) m2x.c
