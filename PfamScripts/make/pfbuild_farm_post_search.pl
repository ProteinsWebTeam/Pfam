#!/software/bin/perl

use strict;
use warnings;
use Bio::Pfam::HMM::HMMResultsIO;

my $HMMResultsIO = Bio::Pfam::HMM::HMMResultsIO->new;  
$HMMResultsIO->convertHMMSearch( "OUTPUT" );

