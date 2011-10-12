.. _Usage:

=======
Usage
=======

Overview
========

ADDA is controlled with the script :file:`adda.py`. This script expects a file 
:file:`adda.ini` with configuration options in the directory from which it is called. 
An example :file:`adda.ini` is in the directory :file:`test`.

Input data
==========

ADDA requires three input files

   * a file with sequences (SequenceFile_)

   * a file with a pairwise similarity matrix resulting from all-on-all
     BLAST searches (PairsdbFile_)

   * a file with domain assignments from a reference database (ReferenceFile_)

   * a configuration file (AddaIni_)

A toy example with these files can be found at http://genserv.anat.ox.ac.uk/downloads/contrib/adda.

These files are set in the ``[input]`` section of :file:`adda.ini`

.. _SequenceFile:

Sequence file
-------------

The sequence file should be in simple :term:`fasta` format with a single one-line header
followed by the sequence. For example::

   fasta=nrdb.fasta.gz

Only sequences appearing in this file will be used by ADDA. Note that ADDA has only
been tested on non-redundant sequence data, usually filtered down to 40% identity
(Park_ et al 2000).

.. _PairsdbFile:

Pairsdb file
------------

For example::

   graph=pairsdb.links.gz

A list of pairwise alignments. These have been obtained by
running BLASTP (Altschul_ et al. 1997) all-on-all and parsed into a tab-separated 
table. The columns are:

query
   identifier of the query sequence
sbjct
   identifier of the sbjct sequence
evalue
   natural log of the E-Value
query_start
   first aligned residue in query (0-based)
query_end
   last aligned residue + 1 in query
query_ali
   compressed alignment of the query
sbjct_start
   first aligned residue in sbjct (0-based)
sbjct_end
   last aligned residue + 1 in sbjct
sbjct_ali
   compressed alignment of the sbjct
alignment score 
   (not used)
percent identity 
   (not used)

The alignment coordinates are inclusive/exclusive 0-based coordinates "[)".
The alignment is stored in compressed form as alternating integer numbers
with the prefix "+" and "-". Positive numbers signify character emissions 
and negative numbers insertions. For example, "+3-3+2" with the sequence 
ABCDE will result in "ABC---DE".

ADDA can read alternative formats. See the command line option ``--alignment-format``.

This file can be compressed (filenames ending in suffix ".gz") and/or split into
several parts. 

The file should be sorted by query and the results for a query should
all be within the same file.

.. _ReferenceFile:

Reference file
==============

The reference file contains a list of domains for a subset of sequences
in the `SequenceFile`_.

For example::

    reference.domains.gz

Typically, the domains in are from structural domain definitions in 
SCOP (Andreeva_ et al. 2008).

This file can be supplied in compressed form (gzip, suffix ".gz").

.. _AddaIni:

adda.ini
--------

The :file:`adda.ini` should be present in the working directory. It contains
various configuration options for the pipeline and is grouped into sections.

Of interest should be the section ``[files]`` which lists the input and 
output filenames. In particular,

input_graph
   the file or files of the BLAST results (`PairsdbFile`_)

input_fasta
   the file with sequence information (`SequenceFile`_)

input_reference
   the file with reference domain information (`ReferenceFile`_)

A sample :file:`adda.ini` can be found in the :file:`test` directory.

Running ADDA
============

ADDA is controlled through a single script :file:`adda.py`. In order to
run the full build process, type::

   adda.py make build

within the build directory.

Parallel runs
-------------

ADDA can use several CPUs/cores for steps that are embarrassingly parallel.
These steps will create several output files with numeric suffixes that 
will be later merged into a single file.

Aborted runs
------------

ADDA will try to pick up from aborted runs and continue without re-computing previously
computed steps. It will check if a step has run to completion by examining the file
contents and not time stamps. In particular, it will check if a file ends with the
token ''#\\''.

Output
======

Filenames for ADDA's intermediate and output files can be set in
the ``[output]`` section of :file:`adda.ini` though it is best to leave 
them unchanged.

Result files
------------

.. glossary::


   adda.result
      The main ADDA result. This :term:`domainfile` lists for 
      each sequence domains and their domain families. This file
      includes all ADDA domains plus all :term:`singletons`.

   adda.families
      Summary information on all domain families defined in
      :term:`adda.result`.
      
      +------------------------------+--------------------------------------------+
      |**column**                    |**content**                                 |
      +------------------------------+--------------------------------------------+
      |family                        |family name                                 |
      +------------------------------+--------------------------------------------+
      |nunits                        |number of domains in sequences              |
      +------------------------------+--------------------------------------------+
      |nsequences                    |number of sequences with this domain        |
      +------------------------------+--------------------------------------------+
      |nresidues                     |number of residues in this domain family    |
      +------------------------------+--------------------------------------------+
      |length                        |average domain length                       |
      +------------------------------+--------------------------------------------+
      |length_median                 |median domain length                        |
      +------------------------------+--------------------------------------------+
      |length_stddev                 |standard deviation of domain length         |
      +------------------------------+--------------------------------------------+
   
   adda.stats
      Some statistics on the input graph.

   adda.summary
      This file contains for each stage summary information
      on the number of sequences and domains that have been
      processed.

Intermediate files
------------------

ADDA creates a few intermediate files. These can get quite large
and are not deleted automatically. Approximately in order of appearance
there are:

.. glossary::

   adda.nids
      This tab-separated table contains the sequence information
      and the map between a :term:`pid` to an :term:`nid`.

   adda.graph
      A compressed and processed version of the pairwise alignment
      graph (PairsdbFile_). 

   adda.graph.idx
      The index for :term:`adda.graph`.

   adda.fasta
      A :term:`fasta` formatted file of all sequences used by
      ADDA. Sequences are stored on a single line to allow
      quick random access.

   adda.idx
      The index for :term:`adda.idx`

   adda.fit
      The results of the fitting procedure.

   adda.fit.data
      Auxiliary data file for fitting.   

   adda.fit.transfer
      Histgoram of :term:`transfer` values.

   adda.fit.overhang
      Histogram of :term:`overhang` values.

   adda.segments
      The segmentation trees. Each sequence is split 
      hierarchically into smaller and smaller segments.
      The result is a tree structure that is presented
      in this file in tabular form. The columns are:

      +------------------------------+---------------------------------------+
      |column                        |content                                |
      +------------------------------+---------------------------------------+
      |nid                           |the sequence identifier (:term:`nid`)  |
      |                              |                                       |
      +------------------------------+---------------------------------------+
      |node                          |the current node within the tree       |
      +------------------------------+---------------------------------------+
      |parent                        |the parent node                        |
      +------------------------------+---------------------------------------+
      |level                         |the depth of the current node          |
      +------------------------------+---------------------------------------+
      |start                         |first residue of segment               |
      +------------------------------+---------------------------------------+
      |end                           |last residue+1 of segment              |
      +------------------------------+---------------------------------------+

   adda.domains
       The result of the optimisation step. This is a :term:`domainfile`
       without the family information.

   adda.domaingraph.gz
       The pairwise domain alignment graph. Alignments between sequences are converted into 
       alignments between domains.

       +------------------------------+------------------------------+
       |column                        |content                       |
       +------------------------------+------------------------------+
       |qdomain                       |a query domain                |
       +------------------------------+------------------------------+
       |sdomain                       |a sbjct domain                |
       +------------------------------+------------------------------+
       |weight                        |the edge weight               |
       +------------------------------+------------------------------+
       |...                           |additional columns            |
       +------------------------------+------------------------------+

   adda.mst
       The minimum spanning tree. The domain graph is clustered by single linkage
       and the resulting edges are in adda.mst. This file has the same format as
       :term:`adda.domaingraph.gz`.

   adda.align
       Each edge in :term:`adda.mst` is checked via profile-profile alignment.
       The alignment results are in this tab-separated file:

       +-------------------+------------------------------------------------------+
       |column             |content                                               |
       +-------------------+------------------------------------------------------+
       |qdomain            |a query domain                                        |
       +-------------------+------------------------------------------------------+
       |sdomain            |a sbjct domain                                        |
       +-------------------+------------------------------------------------------+
       |weight             |the edge weight                                       |
       +-------------------+------------------------------------------------------+
       |passed             |``+`` if alignment is *significant*, ``-`` otherwise  |
       +-------------------+------------------------------------------------------+
       |qstart             |the first aligned residue on the query                |
       |                   |                                                      |
       |                   |                                                      |
       +-------------------+------------------------------------------------------+
       |qend               |the last aligned+1 residue on the query               |
       +-------------------+------------------------------------------------------+
       |qali               |the query alignment in ``pairsdb`` format             |
       +-------------------+------------------------------------------------------+
       |sstart             |the first aligned residue on the sbjct                |
       +-------------------+------------------------------------------------------+
       |ssend              |the last aligned+1 residue on the sbjct               |
       +-------------------+------------------------------------------------------+
       |sali               |the sbjctalignment in ``pairsdb`` format              |
       +-------------------+------------------------------------------------------+
       |score              |the alignment score                                   |
       +-------------------+------------------------------------------------------+
       |naligned           |the number of aligned residues                        |
       +-------------------+------------------------------------------------------+
       |ngaps              |the number of gaps in the alignment                   |
       +-------------------+------------------------------------------------------+
       |zscore             |the zcore of the alignment (``na`` if not computed)   |
       +-------------------+------------------------------------------------------+

   adda.clusters
      ADDA domains after the computing connected components on the edges in
      :term:`adda.align` that have passed the alignment score threshold and
      other alignment quality criteria. This file is in :term:`domainfile`
      format.

References
==========

.. _Altschul:

Altschul SF, Madden TL, Schäffer AA, Zhang J, Zhang Z, Miller W, Lipman DJ.
(1997) Gapped BLAST and PSI-BLAST: a new generation of protein database search program
Nucleic Acids Res. Sep 1;25(17):3389-402.

.. _Park:

Park J, Holm L, Heger A, Chothia C. (2000) RSDB: representative protein 
sequence databases have high information content. Bioinformatics. May;16(5):458-64.

.. _Andreeva:

Andreeva A, Howorth D, Chandonia JM, Brenner SE, Hubbard TJ, Chothia C, Murzin AG.
(2008) Data growth and its impact on the SCOP database: new development
Nucleic Acids Res. Jan;36(Database issue):D419-25. Epub 2007 Nov 13.
