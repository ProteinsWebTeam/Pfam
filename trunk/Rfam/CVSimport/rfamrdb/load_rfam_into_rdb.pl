#!/usr/local/bin/perl 


use Rfam::UpdateRDB;
use Rfam;


use strict;
#my $database = shift;
 my  $rdb;
my $family = shift;

die "need Rfam to upload dopey!! \n" if(!$family);
#if ($database =~ /rfam2/) {
  $rdb  = Rfam->switchover_rdb_update();
#} else {
#  $rdb = Rfam->rdb_update();
#}

my $db = Rfam::default_db();
my $en = $db->get_Entry_by_acc($family );
my $id =  $en->author();

 $rdb->check_in_Entry( $en );

