#!/usr/bin/env perl 

use strict;
use warnings;
use LWP::UserAgent;
use HTTP::Date;

my $proteome =  'complete:yes';
my $keyword =  'keyword:181';

my $agent = LWP::UserAgent->new;
$agent->env_proxy;

my $output_dir = shift;
chdir "$output_dir" or die "couln't change dir";

# Get a list of all taxons below the top node with a complete/reference proteome.
my $query_list = "http://www.uniprot.org/taxonomy/?query=$proteome&format=list";

my $response_list = $agent->get($query_list);
die 'Failed, got ' . $response_list->status_line .
  ' for ' . $response_list->request->uri . "\n"
  unless $response_list->is_success;


my $summary = "http://www.uniprot.org/taxonomy/?query=$proteome&format=tab";

my $response_sum = $agent->get($summary);
die 'Failed, got ' . $response_sum->status_line .
  ' for ' . $response_sum->request->uri . "\n"
  unless $response_sum->is_success;

open(L,">", "taxonomy_list") or die;
print L $response_sum->content;
close(L);

# For each taxon, mirror its proteome set in FASTA format.
for my $taxon (split(/\n/, $response_list->content)) {
  my $file = $taxon . '.fasta';
  my $query_taxon = "http://www.uniprot.org/uniprot/?query=organism:$taxon+$keyword&format=fasta&include=yes";
  my $response_taxon = $agent->mirror($query_taxon, $file);

  if ($response_taxon->is_success) {
    my $results = $response_taxon->header('X-Total-Results');
    my $release = $response_taxon->header('X-UniProt-Release');
    my $date = sprintf("%4d-%02d-%02d", HTTP::Date::parse_date($response_taxon->header('Last-Modified')));
    print "File $file: downloaded $results entries of UniProt release $release ($date)\n";
  }
  elsif ($response_taxon->code == HTTP::Status::RC_NOT_MODIFIED) {
    print "File $file: up-to-date\n";
  }
  else {
    die 'Failed, got ' . $response_taxon->status_line .
      ' for ' . $response_taxon->request->uri . "\n";
  }
}