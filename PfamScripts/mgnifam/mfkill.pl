#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my ($message, $forward, $nforward);
GetOptions('m=s' => \$message,
  "f=s"  => \$forward,
  "nf"   => \$nforward);


my $family = shift;

unless($family) {
  die "Need to specify family on the command line\nEg $0 MGYF00003\n";
}

#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
unless($pfamDB->{database} eq "mgnifam") {
  die "Need to use the config for mgnifam\n";
}

#Check the family is in mgnifam table
my $mgnifam=$pfamDB->getSchema->resultset('Mgnifam')->find( { mgnifam_acc => $family });
unless($mgnifam) {
  die "Cannot find $family in the database\n";
}

#Get message if not specified
unless($message) {
  print "Please give a comment for killing family [$family]\n";
  print "Finish comment by a . on the line by itself\n";
  my @comment;
  while (<STDIN>) {
    chomp;
    /^\s*\.\s*$/ && last;
    push( @comment, $_ );
  }
  $message = join( " ", @comment );
}

#Get user
my $user = $ENV{USER};

my $error;
unless ( $nforward or $forward ) {
  print "Please give the mgnifam/pfam accession for the family to forward to\n";
  print "Finish with a . on the line by itself\n";

  while ( ( $_ = <STDIN> ) !~ /^\.$/ ) {
    chop;
    if(/^(PF\d{5})/) {
      $forward = $1;
    }
    elsif(/^(MGYF\d+)/) {
      my $acc= $1;
      my $is_in_db=$pfamDB->getSchema->resultset('Mgnifam')->find( { mgnifam_acc => $acc });
      print STDERR "$1 is not in the mgnifam database" unless($is_in_db);
      $error=1;
      $forward = $1;
    }
    else {
      warn "$_ does not look like a Pfam/Mgnifam accession\n";
    }
  }
}

if($error) {
  exit;
}

#Delete from mgnifam
  $pfamDB->getSchema->resultset('Mgnifam')->find( { mgnifam_acc => $family } )->delete;

#Add to dead_family table
$pfamDB->getSchema->resultset('DeadMgnifam')->create(
    {
      mgnifam_id   => $mgnifam->mgnifam_id,
      mgnifam_acc  => $mgnifam->mgnifam_acc,
      comment    => $message,
      forward_to => $forward,
      user       => $user,
      killed     => \'NOW()'
    }
);
print STDERR "$family has been killed\n";
