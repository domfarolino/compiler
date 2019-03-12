CXX=g++
PROGRAMS=main
UNAME := $(shell uname)
# TODO(domfarolino): Maybe make this use some environment variable as an LLVM-path prefix?
CFLAGS= `/usr/local/opt/llvm/bin/llvm-config --cppflags --ldflags --libs all --system-libs core` -std=c++11

all: $(PROGRAMS)

main: main.cpp lexer/Lexer.cpp parser/Parser.cpp
	$(CXX) $^ -o $@ $(CFLAGS)

clean:
	rm -r $(PROGRAMS)
