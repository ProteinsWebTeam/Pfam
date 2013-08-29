#!/usr/bin/env perl 

use strict;
use warnings;
use LWP::UserAgent;
use File::Temp qw(tempdir);
use IO::Uncompress::Gunzip qw(gunzip $GunzipError);
use IO::Compress::Gzip qw(gzip);
use Data::Printer;
use Log::Log4perl qw( :easy );

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $dir = 

my $proteome = 'complete:yes';
my $url = 'www.uniprot.org';
my $agent = LWP::UserAgent->new;
# Get a list of all taxons below the top node with a complete/reference proteome.
my $query_list = 'http://'.$url."/taxonomy/?query=$proteome&format=list";
my $response_list = $agent->get($query_list);
$logger->logdie( 'Failed, got ' . $response_list->status_line .
  ' for ' . $response_list->request->uri ) unless $response_list->is_success;

# For each taxon, mirror its proteome set in FASTA format.

  my $version = 0;
  for my $taxon (split(/\n/, $response_list->content)) {
    my $file = $taxon . '.fasta';
    my $query_taxon = 'http://'.$url."/uniprot/?query=organism:$taxon&format=fasta&include=yes";
    my $response_taxon = $agent->mirror($query_taxon, $file);
    if ($response_taxon->is_success) {
      $logger->debug("Got file");
    }else{
      $logger->logdie('Failed, got ' . $response_taxon->status_line .' for ' . $response_taxon->request->uri);
    }
  }