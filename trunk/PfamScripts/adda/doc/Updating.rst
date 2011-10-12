=============
Updating ADDA
=============

This chapter contains specific details for running
ADDA updates. See :ref:`Usage` for general usage 
instructions

Updating ADDA from pairsdb_
===========================

ADDA can be run from the pairdb dump files 
that are output by the pairsdb database at the CSC.
The script :file:`pairsdb2adda.py` assists with the
conversion.

In particular, the following table dumps are required 

+------------------------------+-------------------------------+---------------------------------+
|*table*                       |*filename*                     |*contents*                       |
+------------------------------+-------------------------------+---------------------------------+
|nrdb40                        |nrdb40.dump.gz                 |nrdb40                           |
+------------------------------+-------------------------------+---------------------------------+
|nrdb                          |nrdb.dump.gz                   |sequence information             |
|                              |                               |                                 |
+------------------------------+-------------------------------+---------------------------------+
|nrdb40_scop_domains_nr        |nrdb40_scop_domains_nr.dump.gz |scop domains (for calibration)   |
|                              |                               |                                 |
+------------------------------+-------------------------------+---------------------------------+
|nrdb_interpro_domains         |nrdb_interpro_domains.dump.gz  |pfam domains (for benchmarking)  |
|                              |                               |                                 |
+------------------------------+-------------------------------+---------------------------------+
|pairsdb_100x40                |pairsdb_100x40.dump.gz         |map between nrdb40 to nrdb       |
+------------------------------+-------------------------------+---------------------------------+

The last two files are only required in the post-processing and annotation
stage.

Updating ADDA from simap_
=========================

.. _pairsdb: http://pairsdb.csc.fi
.. _simap: http://boinc.bio.wzw.tum.de/boincsimap







