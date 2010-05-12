#!/usr/local/bin/perl

use strict;
use warnings;
use LWP::UserAgent;
use File::Temp qw(tempfile);
use Cwd; 
use Getopt::Long;
use HTTP::Request::Common;


use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

my ( @ignore, $endpoints_opt, $help, $add_to_clan, $remove_from_clan );

&GetOptions(
  "i=s@" => \@ignore,
  "help" => \$help
);

my $family = shift;

if ( !$family ) {
  print STDERR "ERROR:You have not specified the family\n";
  help();
}

if ( $family =~ /^(\S+)\/$/ ) {
  chop $family;
}

help() if ($help);

my %ignore;    #Hash used to ignore duplicating entries in @ignore
foreach my $fam (@ignore) {
  $ignore{$fam} = 1;
}

my $config = Bio::Pfam::Config->new;
my ( $fh, $filename ) = tempfile();
my $familyIO = Bio::Pfam::FamilyIO->new;

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if ( !( -d "$pwd/$family" ) ) {
  die
"$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if ( !-w "$pwd/$family" ) {
  die
    "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

#-------------------------------------------------------------------------------

my $famObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded $family through middleware\n";

foreach my $seq ( $famObj->SEED->each_seq ) {
  my $id;
  if ( $seq->id =~ /(\S+)\.\d+/ ) {
    $id = $1;
  }
  else {
    $id = $seq->id;
  }

  my $string = join(
    "\t",
    (
      "SEED", $id, $seq->start, $seq->end,
      ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
      ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" )
    )
  );
  print $fh $string."\n";
}

#Then for the full, use the scores file as this contains tha alignment co-ordinates.
#We now allow overlaps between envelopes.
foreach my $seq ( keys %{ $famObj->scores->regions } ) {
  my $id;
  if ( $seq =~ /(\S+)\.\d+/ ) {
    $id = $1;
  }
  else {
    $id = $seq;
  }
  foreach my $fullReg ( @{ $famObj->scores->regions->{$id} } ) {
    my $string = join(
      "\t",
      (
        "FULL",
        $id,
        $fullReg->{aliStart},
        $fullReg->{aliEnd},
        ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
        ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" )
      )
    );
    print $fh $string."\n";
  }
}
close($fh);
# Create a user agent object

my $ua = LWP::UserAgent->new;
$ua->agent("PfamOverlap/0.1 ");

# Create a request
#my $req = HTTP::Request->new( );

# Pass request to the user agent and get a response back
my $res = $ua->request(POST 'https://pfamsvn.sanger.ac.uk/cgi-bin/overlap.cgi',
                              Content_Type => 'form-data', 
                              Content => [ file => [$filename]]);

# Check the outcome of the response
if ( $res->is_success ) {
  print $res->content;
}
else {
  print $res->status_line, "\n";
}

my $overlaps;
if ($overlaps) {
  exit(1);
}
else {
  exit(0);
}

sub help {
  print STDERR << "EOF";

This script runs checks for overlaps between a Pfam family and the
current Pfam database (pfamlive).


Usage:

    $0 <family>

  

Example:

    $0 AAA -remove_from_clan CL0023

EOF

  exit(0);
}
