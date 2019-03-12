import os
import filecmp
import subprocess
from glob import glob

# Pardon my py, I don't write a lot of Python these days :)
# MEANT TO BE RUN FROM THE ROOT FOLDER OF THIS REPOSITORY, because
# that is where the `main` compiler binary is.

in_programs = []

for x in os.walk("."):
  for y in glob(os.path.join(x[0], '*')):
    if y.endswith(".in"):
      in_programs.append(y)

for idx,program in enumerate(in_programs):
  out_file = open(program[:-2] + "out", "wb")
  subprocess.Popen(["./main", program, "--no-codegen"], stdout=out_file, stderr=out_file)
  out_file.close()

  # Run the compiler in symbol-insight mode for valid programs
  if "valid" in program and "invalid" not in program:
    symbol_out_file = open(program[:-2] + "sym.out", "wb")
    subprocess.Popen(["./main", program, "--symbol-insight", "--no-codegen"], stdout=symbol_out_file, stderr=symbol_out_file)
    symbol_out_file.close()
