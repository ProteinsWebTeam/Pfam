#!/usr/local/bin/perl -w

use strict;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;
use RfamRCS;

my $tmpspace = "/tmp";
chdir "$tmpspace" or die;

my $list;
foreach my $acc ( Rfam::get_allaccs() ) {
    system "cp $Rfam::current_dir/$acc/CM $acc.cm" and die;
    $list .= "$acc.cm ";
}
system "tar -cvf - $list" and die;
