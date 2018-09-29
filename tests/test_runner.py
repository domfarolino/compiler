import os
import filecmp
import subprocess
from glob import glob

# Pardon my py, I don't write a lot of Python these days :)
# MEANT TO BE RUN FROM THE ROOT FOLDER OF THIS REPOSITORY

in_programs = []
out_programs = []

for x in os.walk("."):
  for y in glob(os.path.join(x[0], '*')):
    if y.endswith(".in"):
      in_programs.append(y)
    elif y.endswith(".out"):
      out_programs.append(y)

for idx,program in enumerate(in_programs):
  out_file = open(out_programs[idx], "wb")
  subprocess.Popen(["./main", program], stdout=out_file, stderr=out_file)
  out_file.close()
