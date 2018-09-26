CC=gcc
CFLAGS=

OBJ = readout.o

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

readout.exe: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean

clean:
	-rm -f *.o *.exe