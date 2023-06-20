#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

my $config = Bio::Pfam::Config->new;

my ($help, $revision, $rev_ver);
GetOptions(
  "help" => \$help,
  "rev"  => \$revision,
  "ver=i"=> \$rev_ver
 );

help() if($help);

unless ( $ARGV[0] ) {
  help();
}

my $family = shift;

if ( $family !~ /^(PF\d{5})$/ ) {
 if($config->location eq 'WTSI' or $config->location eq 'EBI'){
  my $connect = $config->pfamlive;
  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
    %{ $connect }
  );
  my $pfamAcc = $pfamDB->id2acc($family);
  unless($pfamAcc =~ /PF\d{5}/){
    warn "You passed in something that did not look like an accession.\n"; 
    warn "Because you are at WTSI/EBI, tried to map it to an accession, but failed.\n";
    help();
  }
  $family = $pfamAcc;   
 }else{
  warn "Looks like you have passed in an id rather than accession, [$family]\n";
  help();
 }
}

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
$client->checkFamilyExists($family, $config);
$client->log($family) if($revision);
$client->catFile( $family, "DESC", undef, $rev_ver );

sub help {

  print<<EOF;

usage: $0 <PFAM ACCESSION>

Now just prints the DESC file.

-rev :Prints the SVN revision history for the family.
-ver <REVISION VERSION> :Prints the DESC file at the svn revision version provided.

If you are WTSI/EBI, you can use family ids.

EOF
 

exit;  
}
