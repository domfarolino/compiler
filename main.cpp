#include <iostream>
#include <fstream>

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "You must provide exactly one argument (an input program) to the compiler" << std::endl;
    return 1;
  }

  std::ifstream source;
  source.open(argv[1], std::ifstream::in);
  if (!source.is_open()) {
    std::cerr << "Failed to open " << argv[1] << std::endl;
  }

  source.close();
  return 0;
}
