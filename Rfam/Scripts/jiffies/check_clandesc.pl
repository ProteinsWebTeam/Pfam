#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;

use Bio::Rfam::ClanIO;

my $rootdir = getcwd;
my $clanIO = Bio::Rfam::ClanIO->new( );

opendir(DIR, ".") or die "Failed to open pwd fro reading.\n";
my @dir = grep{$_ ne ".." and $_ ne "." }readdir(DIR);
foreach my $d (@dir){
  next unless($d =~ /^CL\d{5}/);
  chdir("$rootdir/$d") or die "Could not chdir $rootdir/$d\n";
  eval{
    my $desc = $clanIO->parseDESC( "CLANDESC" );
  };
  if($@){
    print "Error parsing $d: $@\n";
  }
}
