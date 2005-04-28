How to update the Rfam genome hit tables
----------------------------------------

1. Get txt files of accession from EMBL:

   wget http://www.ebi.ac.uk/genomes/eukaryota.txt
   wget http://www.ebi.ac.uk/genomes/bacteria.txt
   wget http://www.ebi.ac.uk/genomes/archaea.txt

2. Work out which are cons entries and fudge the hit lists as before,
   but also calculating chromosome coordinates.

3. Retrieve those that are bona fide EMBL entries and grab
   precalulated results.

4. Run rfam_scan.pl on stuff that isn't precalculated.

5. Grab wgs entries and run search over those aswell?

