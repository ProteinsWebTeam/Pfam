========
Glossary
========

.. glossary::
   :sorted:

   pid 
      a protein identifier. The protein identifier can be any alphanumerical sequence.
   
   nid
      a numerical sequence identifier. ADDA refers to sequences by a positive integer
      (i.e., excluding 0)

   hid
      a hash identifier

   fasta
      a fasta_ formatted sequence file. The format is::

         >Identifier Description
	 SEQUENCE

      The identifier is delimited by the first white-space character. 
      The description is optional and is ignored. The sequence can span multiple lines.

   domainfile
      a list of domains as a tab separated table. The domainfile contains
      the following four columns 

      +------------------------------+------------------------------+
      |column                        |content                       |
      +------------------------------+------------------------------+
      |nid                           |the sequence identifier       |
      +------------------------------+------------------------------+
      |start                         |start of the domain (0-based) |
      +------------------------------+------------------------------+
      |end                           |last residue+1 of the domain  |
      +------------------------------+------------------------------+
      |family                        |the domain family             |
      +------------------------------+------------------------------+

.. _fasta: http://www.ncbi.nlm.nih.gov/blast/fasta.shtml
