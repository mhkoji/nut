SRCS := runtime-impl.c runtime.h test.c nut.c

.PHONY: all format clean

all: test

test: test.o runtime.o nut.o
	gcc $(^) -o $@

runtime.o: runtime-impl.c
	gcc -c $(<) -o $@

test.o: test.c
	gcc -c $(<) -o $@

# nut.o: nut.c
# 	gcc -c $(<) -o $@

nut.o: nut.asm
	nasm -f elf64 $(<) -o $@

clean:
	rm -rf *.o test

format: $(SRCS)
	$(foreach src, $(SRCS), clang-format -i $(src);)
