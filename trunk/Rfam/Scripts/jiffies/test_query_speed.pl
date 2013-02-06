#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;

use Bio::Rfam::Config;

my $mode = shift;

my $config = Bio::Rfam::Config->new;
my $rfamdb = $config->rfamlive;

if($mode eq 'dbi'){
  my $rs = $rfamdb->resultset('Rfamseq');
  $rs->result_class('DBIx::Class::ResultClass::HashRefInflator');
  for( my $i = 1; $i <= 10000; $i++){
    my $hashRef = $rs->seqaccToTaxon('AAAA02006309.1');
    print STDERR "." if(($i % 100) == 0);
  }
}elsif($mode eq 'dbh'){
  my $sth = $rfamdb->prepare_seqaccToTaxon;
  for( my $i = 1; $i <= 10000; $i++){
    $sth->execute('AAAA02006309.1');
    my $row = $sth->fetchrow_hashref;
    #p($row);
    print STDERR "." if(($i % 100) == 0);
  }
}else{
  print STDERR "mode not dbi or dbh\n";
}
