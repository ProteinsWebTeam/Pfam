#!/usr/local/bin/perl
#
# Script to find overlaps between a family (on disk) and the live curation MySQL database!
#
use strict;
use warnings;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Clan::ClanQC;

my ( @ignore, $endpoints_opt, $help, $add_to_clan, $remove_from_clan );

&GetOptions(
  "i=s@"               => \@ignore,
  "e"                  => \$endpoints_opt,
  "add_to_clan=s"      => \$add_to_clan,
  "remove_from_clan=s" => \$remove_from_clan,
  "help"               => \$help
);

my $family = shift;
if ( $family =~ /^(\S+)\/$/ ) {
  chop $family;
}

help() if ($help);
if ( !$family ) {
  print STDERR "ERROR:You have not specified the family\n";
  help();
}
if ( $remove_from_clan and $add_to_clan ) {
  print STDERR
"ERROR: You cannot use the -add_family and -remove_family options together\n";
}

if ($add_to_clan) {
  unless ( $add_to_clan =~ /CL\d\d\d\d/ ) {
    print STDERR
"ERROR: The clan accession you specified [$add_to_clan] is not in the correct format.\n";
    help();
  }
  print STDERR "Performing overlap checks for adding $family to $add_to_clan\n";
}
elsif ($remove_from_clan) {
  unless ( $remove_from_clan =~ /CL\d\d\d\d/ ) {
    print STDERR
"ERROR: The clan accession you specified [$remove_from_clan] is not in the correct format.\n";
    help();
  }
  print STDERR
    "Performing overlap checks for removing $family from $remove_from_clan\n";
}

my %ignore;    #Hash used to ignore duplicating entries in @ignore

foreach my $fam (@ignore) {
  $ignore{$fam} = 1;
}


my $config = Bio::Pfam::Config->new;
my $connect = $config->pfamlive;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{ $connect }
);

#Find out if family is in rdb
my $rdb_family = $pfamDB->getPfamData($family);


#Find out what clan (if any) the family already belongs to
my ( $rdb_clan_acc, $clan_membership );
if ($rdb_family) {
  my $clan_result = $pfamDB->getClanDataByPfam("$family");

  if ($clan_result) {
    $rdb_clan_acc    = $clan_result->clan_acc;
    $clan_membership = $pfamDB->getClanMembership($rdb_clan_acc);

    print STDERR "$family belongs $rdb_clan_acc.\n" unless ($remove_from_clan);

    if ( $add_to_clan and $rdb_clan_acc ) {
      die
"Cannot add $family to $add_to_clan.  $family already belongs to $rdb_clan_acc.\n";
    }
    elsif ( $remove_from_clan and $rdb_clan_acc ne $remove_from_clan ) {
      die
"$family belongs to $rdb_clan_acc and not to $remove_from_clan.  Incorrect clan accession entered on command line.";
    }
    my $num_in_clan = @$clan_membership;
    if ( $remove_from_clan and $num_in_clan == 2 ) {
      warn
"Cannot remove $family from $rdb_clan_acc because there are only two families in this clan and a clan must contain a minimum of two families\n";
    }

  }
}
if ( $add_to_clan and !$rdb_clan_acc ) {    #Family not currently in a clan
  $clan_membership = $pfamDB->getClanMembership($add_to_clan);
}

my $no_nl = 0;
my $no_ne = 0;

#Look for nested domains if there is a DESC file
if ( -e "$family/DESC" ) {
  open( DESC, "$family/DESC" ) or die "Could not open $family/DESC $!";
  while (<DESC>) {
    if (/^AC\s+(PF\d\d\d\d\d)/) {
      my $pfam = $pfamDB->getPfamData($1);
      push( @ignore, $pfam->pfamA_id ) unless ( $ignore{ $pfam->pfamA_id } );
      $ignore{ $pfam->pfamA_id } = 1;
    }
    if (/^NE\s+(PF\d\d\d\d\d)/) {
      my $nested = $pfamDB->getPfamData($1);
      push( @ignore, $nested->pfamA_id )
        unless ( $ignore{ $nested->pfamA_id } );
      $ignore{ $nested->pfamA_id } = 1;
      $no_ne++;
      warn "CAUTION:: am allowing $family to overlap with $1 !!!\n";
    }
    elsif (/^NL\s+/) {
      $no_nl++;
    }
  }
  close(DESC);
}
if ( $no_nl != $no_ne ) {
  warn "$family:Numbers of nested domains and nested locations do not match";
  exit(1);
}


#Now look for overlaps
my ( $overlaps, $clan_seed_overlap );
if ( $add_to_clan or ( $rdb_clan_acc and !$remove_from_clan ) ) {
  foreach my $clan_mem (@$clan_membership) {
    push( @ignore, $clan_mem->pfamA_id )
      unless ( $ignore{ $clan_mem->pfamA_id } );
  }
  
  $overlaps =
    &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \@ignore,
    $endpoints_opt, $pfamDB );
  warn "$family: found $overlaps external overlaps\n";

  #Need to also check for clan seed overlaps
  $clan_seed_overlap =
    Bio::Pfam::Clan::ClanQC::check_seed_overlap( $pfamDB, $family,
    $clan_membership, $pfamDB );
  warn "$family: found $clan_seed_overlap clan seed overlaps\n"
    if ($clan_seed_overlap);
}
else {
  $overlaps =
    &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \@ignore,
    $endpoints_opt, $pfamDB );
  warn "$family: found $overlaps overlaps\n";
}

if ($overlaps) {
  exit(1);
}else {
  exit(0);
}

sub help {
  print STDERR << "EOF";

This script runs checks for overlaps between a Pfam family and the
current Pfam database (pfamlive).


Usage:

    $0 <family>


Addional options:

  -add_to_clan <clan_accession>       :Adds family to clan
  -remove_from_clan <clan_accession>  :Removes family from clan
  -i <family_name>                    :Ignore this family (-i can occur multiple times)
  -e                                  :Find end points of extensions


  

Example:

    $0 AAA -remove_from_clan CL0023

EOF

  exit(0);
}
