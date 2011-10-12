======
Design
======

History
=======

ADDA's used to be a mixture of C++ executables, python and perl scripts,
all controlled by a Makefile and distributed over several directories.
This caused all sorts of dependency and installation problems.

Version 2 was refactored to use cython and ruffus. ADDA is now controlled
by a single script, :file:`adda.py` and runs within a single directory.
All scripts and C++ code are part of the same distribution and can be
installed via :file:`setup.py`.
