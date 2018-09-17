CXX=g++
PROGRAMS=main
UNAME := $(shell uname)
CFLAGS=-std=c++11 -g

all: $(PROGRAMS)

main: main.cpp lexer/Lexer.cpp parser/Parser.cpp
	$(CXX) $^ -o $@ $(CFLAGS)

clean:
	rm -r $(PROGRAMS)
