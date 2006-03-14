
package PfamWeb::Model::PubMed;

use strict;
use warnings;

use LWP;
use XML::LibXML;

sub new {
  my $class = shift;

  my $this = { URL => "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&id=" };

  bless $this, $class;
  return $this;
}

sub get {
  my $this = shift;
  my $rawId = shift;

  return unless $rawId =~ /^(\d+)$/;
  my $pmid = $1;

  $this->_parseXML( $pmid );
}

sub _fetchXML {
  my $this = shift;
  my $pmid = shift;

  my $ua = LWP::UserAgent->new;
  $ua->proxy( "http", "http://wwwcache.sanger.ac.uk:3128" );

  my $url = $this->{URL}.$pmid;
  my $response = $ua->get( $url );

  # make sure we got something
  die "(EE) ERROR: can't retrieve PubMed entry $pmid: ", $response->status_line
	unless $response->is_success;

  return $response->content;
}

sub _parseXML {
  my $this = shift;
  my $pmid = shift;

  my $parser = XML::LibXML->new();

  # the parser appears to make network connections behind the scenes,
  # and to do that it needs to use the Sanger proxy. Make sure it can
  # find it
  $ENV{http_proxy} = "http://wwwcache.sanger.ac.uk:3128";

  my $xml = $this->_fetchXML( $pmid );
  my $doc = $parser->parse_string( $xml );
  my $root = $doc->documentElement;

  $this->{abstract} = $root->find( "/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/Abstract/AbstractText" );

  print "(ii) abstract: |", $this->{abstract}, "|\n";
}

1;

