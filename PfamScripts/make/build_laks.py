#! /usr/bin/env python3

import os
from shutil import copy2
import sys


alns_filepath = None

#Check directory containging the alignments is supplied on the command line
if len(sys.argv) == 1:
    sys.exit(f"Need to pass directory containing alignments on the command line\nE.g. {sys.argv[0]} <dir>\n")
else:
    alns_filepath = sys.argv[1]

if not os.path.isdir(alns_filepath):
    sys.exit(f"[{alns_filepath}] does not exist")


#Get a list of files in the directory
files = os.listdir(alns_filepath)

#Loop through each alignment, create a new dir and run liftover_alignment, create_alignment, pfbuild and pqc-overlap-rdb on each one
for filename in files: #EAD9.fa
  name, ext = os.path.splitext(filename) #EAD9, fa

  fasta = os.path.join(alns_filepath, filename) #/hps/nobackup/production/xfam/jaina/Laks/alignments/EAD9.fa

  #Make dir and cp fasta across
  os.mkdir(name)
  copy2(fasta, name)
  os.chdir(name)

  #Reformat afa to pfam format, then use grep to remove STOCKHOLM tag, blank lines and // lines
  os.system(f"esl-reformat pfam {filename}  | grep -v '^#\|^\s*$\|\/\/' > seed");

  #Replace any | in the accession with _, as downstream scripts don't like | in the accession name (ie 000|HEA09671.1 => 000_HEA09671.1)
  os.system(f"sed -i -r 's/\|/_/' seed");

  #Submit liftover_alignment, create_alignment, pfbuild and pqc-overlap-rdb to farm
  os.system(f"bsub -q standard -M 5000 -R \"rusage[mem=5000]\" -o farm.log -J{name} 'liftover_alignment.pl -local -align seed && create_alignment.pl -fasta seed.phmmer -mu > SEED && pfbuild -withpfmake -local && cd .. && pqc-overlap-rdb {name} '");

  os.chdir('../')

