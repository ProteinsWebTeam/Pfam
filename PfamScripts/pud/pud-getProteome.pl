#!/usr/local/bin/perl -w

# This script is designed to get the latest proteomes from Intergr8 at the ebi.
# This data is processed by another script, this simply downloaded.
# Author: mm1
# Maintainer: rdf


use strict;

my $output_dir = shift;

chdir "$output_dir" or die "couln't change dir";

my $url = "ftp://ftp.ebi.ac.uk/pub/databases/SPproteomes/fasta/proteomes/";


system("wget $url") and die "wget failed";

print STDERR "Getting list of genome fasta files\n";
 
open(_URL, "index.html") or die "Could not open index.html";

while(<_URL>) {
  if ($_ =~ /(\d+\.\S+\.FASTA\.gz)/i) {
   
    my $file = $1;
   
    my $new_url = $url . $file;
    print "getting $new_url\n";
    system ("wget  $new_url") and die "couln't download $new_url";
    
  }
}

close(_URL);

# Need to get the report so that we know which genome corresponds to which file
print STDERR "Getting proteome report\n";
system("wget ftp://ftp.ebi.ac.uk/pub/databases/SPproteomes/proteome_report.txt") and die "couldn't get proteome_report.txt" ;
