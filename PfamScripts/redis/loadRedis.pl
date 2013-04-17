#!/usr/bin/env perl
#
# Redis is used on the pfamsvn machine, to provide a fast mapping between
# pfamseq acceession.version and the surrogate keys (auto_pfamseq).  This takes
# a dump of these from the mysql database and loads redis
#
use strict;
use warnings;
use Redis;

my $file = shift;

if(!defined($file)){
  warn "Acceession mapping file not passed in!\n";
  help();
}

if(! -s $file){
  warn "Could not find file, $file!\n";
  help();
}

#Connect on default localhost and port, 127.0.0.1:6379
my $redis = Redis->new;

my $c =0;
open(my $F, '<', $file) or die "Could not open $file:[$!]\n";
while(<$F>){
  chomp;
  if( /^(\S+\.\d+)\s+(\d+)/){
    
    my $acc = $1;
    my $auto = $2;
    $redis->set($acc => $auto);
    $c++;
  }
  #Something that prints progress.
  if($c % 1000000 == 0){
    print STDERR "$c\n";
  }
}

#This is a really important step, otherwise we may not completely recover.
$redis->save;

sub help {
  
print<<EOF;

usage: $0 <accession.version auto_pfamseq tab file>

Loads the redis server with the key/values contained in the file.

This can be generated using the following SQL statement.

select concat(pfamseq_acc, ".", seq_version), auto_pfamseq from pfamseq

EOF
exit; 
}