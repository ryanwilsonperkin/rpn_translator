CC=gfortran
CFLAGS=-std=f95

build:
	$(CC) $(CFLAGS) rpn.f95

test: build
	./a.out < test/input > output 2> /dev/null
	diff output test/expected_output
