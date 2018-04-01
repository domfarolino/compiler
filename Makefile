CXX=g++
PROGRAMS=main
UNAME := $(shell uname)
CFLAGS=-std=c++11

all: $(PROGRAMS)

main: main.cpp lexer/Lexer.cpp
	$(CXX) $^ -o $@ $(CFLAGS)

clean:
	rm -r $(PROGRAMS)
