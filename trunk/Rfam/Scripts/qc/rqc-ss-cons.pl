#!/usr/bin/env perl
#
# Calculate statistics for how well the structure annotation corresponds 
# with the SEED sequences.
#
use strict;
use warnings;
use File::Touch;
use Cwd;

use Bio::Rfam::QC;
use Bio::Rfam::FamilyIO;

my $pwd = getcwd;

my $fam = '.';
if($ARGV[0]){
  $fam = shift @ARGV;
}

if(! -d $fam or !-e "$fam/SEED"){
  warn "$fam does not look like a Rfam family directory\n";
  help();
}

if(-e "$fam/ssstats"){
  unlink("$fam/ssstats");
}

my $io     = Bio::Rfam::FamilyIO->new;
my $familyObj = $io->loadRfamFromLocalFile( $fam, $pwd );
my $error = Bio::Rfam::QC::ssStats($familyObj, "$pwd/$fam");
if($error){
  warn "There is a problem with your SS stats. Please fix.\n";
  exit(1);
}else{
  touch("$fam/ssstats");
}

sub help {
  print STDERR <<EOF;

rqc-ss-con.pl - calculate statistics for how well the structure annotation 
                corresponds with the SEED sequences.

Usage:    rqc-ss-con.pl <directory>
Options:
  -n             no log file
  -f <stkfile>   calculate statistics on \47stkfile\47 instead of SEED

Email discussion with Eric Nawrocki:

Use a rel-entropy measure also?:
> cmbuild --prior nearzero.prior --enone my.cm ../testsuite/srp-euk.sto
#                                                                          rel entropy
#                                                                         ------------
#  aln  cm idx  name           nseq  eff_nseq    alen   clen   bps  bifs     CM    HMM
# ----  ------  ---------  --------  --------  ------  -----  ----  ----  -----  -----
     1       1  srp-euk-1        37     37.00     344    297    74     4  0.892  0.691

The 'rel entropy' 'HMM' column is the information that's due to
primary sequence conservation, so in this case 0.691/0.892 =~ 77% of
the information is from the primary sequence and 23% is from the
structure - more specifically from modelling the consensus pairs
emissions as joint probabilities instead of independently as if they
were single stranded like an HMM could do.

> cmbuild -F my.cm ../testsuite/srp-euk.sto
#                                                                          rel entropy
#                                                                         ------------
#  aln  cm idx  name           nseq  eff_nseq    alen   clen   bps  bifs     CM    HMM
# ----  ------  ---------  --------  --------  ------  -----  ----  ----  -----  -----
     1       1  srp-euk-1        37      8.90     344    297    74     4  0.590  0.385

probabilities of the CM as long as you don't go to below 0.59 bits per
position (for models > 90 consensus positions), or the corresponding
number for models < 90 consensus positions.

\$X = 6.* (eX + log((double) ((clen * (clen+1)) / 2)) / log(2.))    / (double)(2*clen + 4);

if (0.59 > \$relEntropy && \$length >= 90){
       print "BAD MODEL MAGIC! The model is now non-specific, delete dodgy sequences.\n";
}
elsif (\$X > \$relEntropy && \$length < 90){
       print "BAD MODEL MAGIC! The model is now non-specific, delete dodgy sequences.\n";
}


EOF

exit;
}