SRCS := runtime-impl.c runtime.h test.c nut.c

.PHONY: all format clean

all: test

test: test.o runtime.o nut.o
	gcc $(^) -o $@ -lm -no-pie

runtime.o: runtime-impl.c
	gcc -c $(<) -o $@

test.o: test.c
	gcc -c $(<) -o $@

# nut.o: nut.c
# 	gcc -c $(<) -o $@

# nut.o: nut.asm
# 	nasm -f elf64 $(<) -o $@

nut.o: nut.s
	nasm -f elf64 $(<) -o $@

nut.s: compile.lisp
	sbcl --load compile.lisp --eval "(nut.compiler:main)" --quit

clean:
	rm -rf *.o nut.s test

format: $(SRCS)
	$(foreach src, $(SRCS), clang-format -i $(src);)
