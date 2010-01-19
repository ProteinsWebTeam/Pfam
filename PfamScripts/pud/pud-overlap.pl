#!/usr/local/bin/perl -w

#
# Checks all-vs-all overlap 
# from ALIGN files across a Pfam directory system
#
use strict;
use Bio::Pfam::PfamQC;

my $dir = shift;
opendir(DIR,"$dir");
my @families = grep{ $_ ne ".." and $_ ne "." }readdir(DIR);
closedir(DIR);

if( $#families == -1 ){
    print<<EOF;
pud-overlap - call as pud-overlap <directory>

checks all Pfam overlap of these directories on ALIGN files
EOF
}

my $error = &Bio::Pfam::PfamQC::local_dirs_overlap( $dir, \@families );

print "Number of overlap errors $error\n";


