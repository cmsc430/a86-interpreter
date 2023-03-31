UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	format=macho64
	CC=arch -x86_64 gcc
else
	format=elf64
	CC=gcc
endif

%.run: %.o runtime.o
	$(CC) $^ -o $@

runtime.o: set_regs.o print_regs.o main.o
	ld -r $^ -o $@

.c.o:
	$(CC) -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

.PHONY: clean-all clean

clean-all: clean
	-rm *.run *.log *.out

clean:
	-rm *.o
