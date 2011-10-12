.. _Mapping:

====================
Mapping ADDA to PFAM
====================

The section describes how to map ADDA domains onto the latest PFAM_
dataset. Briefly, the sequences in the latest ADDA and PFAM_ releases
are compared. Sequences present in PFAM_ but not in ADDA are
mapped using BLAT_ onto sequences within ADDA.

Preparation
===========

For mapping, a file called :file:`adda.ini` needs
to be present in the working directory. In this file,
the following variables need to be set in the section
``[map]``:

.. glossary::

   filename_adda_sequences
       filename with adda sequences, for example :file:`adda.fasta.gz`. This
       is a :term:`fasta` formatted file. Sequences are labeled by :term:`nid`. The latest
       version of this file can be downloaded from the `adda update server`_.

   filename_domains
       filename with adda domains, for example :file:`adda.results.gz`. This is
       a tab-separated file with four columns: (numerical) sequence identifier (:term:`nid`),
       start, end and family. Sequence coordinates are 0-based half-open. 
       The first line is a header. For example::

	   nid     start   end     family
	   27      0       101     AD092610
	   30      0       147     AD005437
	   42      0       115     AD211211
	   43      0       474     AD004688
	   44      0       150     AD123296
	   55      0       320     AD074056
	   55      320     440     AD054547

       The latest version of this file can be downloaded from the `adda update server`_.

   filename_target_sequences
       filename with the target (PFAM_) sequences in :term:`fasta` format, for example 
       :file:`pfam.fasta.gz`. The first characters up to the first
       whitespace will be used as the sequence identifiers.

   chunksize
       size of BLAT_ jobs. :term:`chunksize` queries are collected and
       run together in one batch.

   min_identity
       minimum percent identity. Only alignments of :term:`min_identity`
       will be used for mapping domains.

An example of the :file:`adda.ini` is on the `adda update server`_.

The mapping procedure requires the following executables to be in
the :envvar:`PATH`:

* blat: the BLAT_ search program
* gunzip: gzip utility

Running
=======

The mapping pipeline is run via the command::

    adda.py make map

.. note::

   The current implementation sends jobs to the cluster assuming
   that sge_ and drmaapython_ are installed. Cluster options like
   queue and priority can be supplied as command line options.

Results
=======

The map command creates the following files in the working directory:

.. glossary::

   all.domains
      A file with ADDA domains mapped to the sequences in 
      :term:`filename_target_sequences`. The format is identical
      to :term:`filename_domains`.

   direct.domains
      A file with ADDA domains in sequences that are part of
      :term:`filename_target_sequences`. The format is identical
      to :term:`filename_domains`.

   mapped.domains
      A file with ADDA domains that have been mapped via BLAT
      onto :term:`filename_target_sequences`. The format is identical
      to :term:`filename_domains`.

   indirect.domains
      A file with ADDA domains that have been mapped via BLAT
      onto redundant :term:`filename_target_sequences`. The format 
      is identical to :term:`filename_domains`.
   
   mapping.summary
      Summary statistics of the mapping process.

   mapping.coverage
      Table delineating the sequence coverage of each sequence in
      :term:`filename_target_sequences`. 

   mapping.coverage_residuecoverage.table
      Distribution of the residue coverage. The distribution is computed from
      the proportion of residues in each sequence in :term:`filename_target_sequences`
      that are within ADDA domains.

   mapping.coverage_sequencecoverage.table
      Distribution of sequence coverages. The sequence coverage describes the number
      of domains that map to a sequence in :term:`filename_target_sequences`.

   mapping.coverage_coverage.png
      A plot showing the distribution in :term:`mapping.coverage_residuecoverage.table`
      and :term:`mapping.coverage_sequencecoverage.table`.

.. _ADDA update server: http://genserv.anat.ox.ac.uk/downloads/adda/current
.. _BLAT: http://genome-test.cse.ucsc.edu/~kent/exe
.. _PFAM: http://pfam.sanger.ac.uk
.. _sge: http://gridengine.sunsource.net
.. _drmaapython: http://code.google.com/p/drmaa-python
