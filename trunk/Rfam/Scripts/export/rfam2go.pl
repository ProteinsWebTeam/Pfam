#!/usr/local/bin/perl

# produces rfam2go file for GO from rfam_live
# need to add a header to the file e.g.
# # !Mapping of Rfam V11.0 to GO terms
# this could be done automatically if the version table was populated and a released version of the database used instead of live

use warnings;
use strict;

use Cwd;
use Config::General;
use DDP;
use Bio::Rfam::Config;

my $config = Bio::Rfam::Config->new;
my $rfamdb = $config->rfamlive;  

my @results = $rfamdb->resultset('Family')->search(
    { 'database_links.db_id' => 'GO'},
    { join => 'database_links',
    }

);

#to avoid duplicated results
my %annotation;

foreach my $row2 (@results){
    my @link = $row2->database_links;
    foreach my $xref (@link){
        if ($xref->db_id eq 'GO'){
           
            my $line = "Rfam:" . $row2->rfam_acc . " " . $row2->rfam_id . " > " . $xref->db_id . ":" . $xref->other_params . " ; " . $xref->db_id . ":" . $xref->db_link;
           my $value = $row2->rfam_acc . "_" . $xref->db_link;
            $annotation{ $line }=$value;

        }
    }

}

open (OUT, ">rfam2go") or die "Cannot open file to write\n";

foreach my $line (sort { $annotation{$a} cmp $annotation{$b} } keys %annotation){
    print OUT $line . "\n";
}

close OUT;
