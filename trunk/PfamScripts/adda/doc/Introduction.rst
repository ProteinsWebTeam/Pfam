============
Introduction
============

Synopsis
========

ADDA is a method to find protein domains in protein sequences.
Briefly, ADDA attempts to split domains into segments that 
correspond as closely as possible to all-on-all pairwise
alignments. A detailed description of the method can be found
in

   Heger A, Holm L. (2003) Exhaustive enumeration of protein domain families.
   J Mol Biol. 2003 May 2;328(3):749-67.
   PMID: `12706730 <http://www.ncbi.nlm.nih.gov/pubmed/12706730>`_ 

Installation
============

Getting ADDA
-------------

Adda is available from http://sourceforge.net/projects/adda. The code
is available from the subversion repository::

   svn co https://adda.svn.sourceforge.net/svnroot/adda/trunk adda

Requirements
------------

ADDA requires the following software:

  * python_ (2.6) 
  * alignlib_ (install from subversion) 
  * matplotlib_ (0.98 or higher) 
  * numpy_ (1.3.0 or higher) 
  * cython_ (0.11.1 or higher) 
  * ruffus_ (2.0.8 or higher) 
  * BLAT_ (for :ref:`Mapping`)

Installation notes
++++++++++++++++++

  * python_ 3 will not work as boost.python support is not there yet.
  * python_ 2.6 is required for the multiprocessing_ module. If python_ 2.5
      is used, install it separately.
  * The python extensions are available on pypi_ and can be installed using::

     easy_install numpy matplotlib cython ruffus

Installation
------------

The following command installs ADDA::

   python setup.py install

This will install the script :file:`adda.py` and the ADDA python libraries.

Citing ADDA
===========

If you use ADDA it in your work, please cite

   Heger A, Holm L. (2003) Exhaustive enumeration of protein domain families.
   J Mol Biol. 2003 May 2;328(3):749-67.
   PMID: `12706730 <http://www.ncbi.nlm.nih.gov/pubmed/12706730>`_ 

Acknowledgements
================

ADDA uses code from the following projects:

   * The gzstream_ library. The gzstream library is available from http://www.cs.unc.edu/Research/compgeom/gzstream/ libray
     under the LPGL licence. Gzstream has been written by Deepak Bandyopadhyay and Lutz Kettner at the
     Computational Geometry Group at UNC Chapel Hill

.. _python: http://www.python.org
.. _matplotlib: http://matplotlib.sourceforge.net
.. _numpy: http://numpy.scipy.org
.. _cython: http://www.cython.org
.. _ruffus: http://code.google.com/p/ruffus
.. _alignlib: http://sourceforge.net/projects/alignlib
.. _pypi: http://pypi.python.org/pypi
.. _ADDA: http://wwwfgu.anat.ox.ac.uk/~andreas/adda/html
.. _PFAM: http://pfam.sanger.ac.uk
.. _multiprocessing: http://code.google.com/p/python-multiprocessing
.. _BLAT: http://genome-test.cse.ucsc.edu/~kent/exe
.. _gzstream: http://www.cs.unc.edu/Research/compgeom/gzstream
