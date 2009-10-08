
package Bio::Lit;

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;
use Carp;
use LWP::UserAgent;
use XML::LibXML;
use XML::LibXML::XPathContext;


use Data::Dump qw( dump );

#-------------------------------------------------------------------------------

# validation for the different possible entry types

subtype 'pmid'
  => as 'Num'
  => where { m/^\d+$/ }
  => message { "'$_' is not a valid PubMed ID" };

has '_pmid' => (
  isa => 'pmid',
  is  => 'rw',
);

subtype 'pmcid'
  => as 'Num'
  => where { m/^\d+$/ }
  => message { "'$_' is not a valid PubMed Central ID" };

has '_pmcid' => (
  isa => 'pmcid',
  is  => 'rw',
);

subtype 'pdbid'
  => as 'Str',
  => where { m/^\d[A-Za-z0-9]{3}$/ }
  => message { "'$_' is not a valid PDB ID" };

has '_pdb' => (
  isa => 'pdbid',
  is  => 'rw',
);

subtype 'uniprotid'
  => as 'Str',
  => where { m/^[A-NR-Z][0-9][A-Z][A-Z0-9]{2}[0-9]$/ or 
             m/^[OPQ][0-9][A-Z0-9]{3}[0-9]$/ },
  => message { "'$_' is not a valid UniProtKB ID" };

# validation patten taken from 
#    http://www.uniprot.org/manual/accession_numbers
# 1           2       3           4           5           6
# [A-N,R-Z]   [0-9]   [A-Z]       [A-Z, 0-9]  [A-Z, 0-9]  [0-9]
# [O,P,Q]     [0-9]   [A-Z, 0-9]  [A-Z, 0-9]  [A-Z, 0-9]  [0-9]

has '_uniprot' => (
  isa => 'uniprotid',
  is  => 'rw',
);

#---------------------------------------

# public attributes

has 'entry_type' => (
  isa      => enum( [ qw( pmcid pmid pdb uniprot ) ] ),
  is       => 'rw',
  required => 1,
);
# TODO ignoring 'ncbi', because I can't find a sensible validation pattern for it

has 'id' => (
  isa      => 'Str',
  is       => 'rw',
  required => 1,
);

has 'articles' => (
  is      => 'rw',
  lazy    => 1,
  builder => '_retrieve_articles',
);

# private attributes

has '_ua' => (
  isa     => 'LWP::UserAgent',
  is      => 'rw',
  lazy    => 1,
  default => sub{ 
    my $this = shift;
    my $ua = LWP::UserAgent->new;
    $ua->timeout( 10 );
    $ua->env_proxy;
    $this->_ua( $ua );
  },
);

has '_xml_parser' => (
  isa     => 'XML::LibXML',
  is      => 'rw',
  lazy    => 1,
  default => sub {
    my $this = shift;
    $this->_xml_parser( XML::LibXML->new );
  },
);

has '_biolit_url_root' => (
  is => 'ro',
  default => 'http://biolit.ucsd.edu/ws/rest',
);

#---------------------------------------

# called after the object is created, this method tries to set a private 
# attribute of the appropriate type (based on the "entry_type" setting). 
# It's a way of validating the ID against the correct pattern for its type

sub BUILD {
  shift->_validate_id;
}

sub _validate_id {
  my $this = shift;

  my $attr_name = "_" . $this->entry_type;
  $this->$attr_name( $this->id );
}


#-------------------------------------------------------------------------------

sub set_params {
  my ( $this, @param_list ) = @_;

  my $params = {};
  if ( scalar @param_list > 1 ) {

    if ( scalar( @param_list ) == 2 ) {
      $params->{entry_type} = $param_list[0];
      $params->{id}         = $param_list[1];
    }
    elsif ( scalar( @param_list ) == 4 ) {
      my %param_hash = @param_list;
      $params->{entry_type} = $param_hash{entry_type};
      $params->{id}         = $param_hash{id};
    }
    else {
      croak 'error: wrong number of parameters (supplied ' 
            . scalar @param_list . ' parameters, need 2 or 4)';
    }

  }
  elsif ( scalar @param_list == 1 ) {

    my $param_ref = shift @param_list;

    if ( ref $param_ref eq 'ARRAY' ) {
      $params->{entry_type} = $param_ref->[0];
      $params->{id}         = $param_ref->[1];
    }
    elsif ( ref $param_ref eq 'HASH' ) {
      $params->{entry_type} = $param_ref->{entry_type};
      $params->{id}         = $param_ref->{id};
    }

  }

  croak 'error: must set "entry_type" and "id"'
    unless ( defined $params->{entry_type} and
             defined $params->{id} );

  $this->entry_type( $params->{entry_type} );
  $this->id( $params->{id} );

  $this->_validate_id;
}

#-------------------------------------------------------------------------------

sub _retrieve_articles {
  my $this = shift;

  my $url = $this->_biolit_url_root . '/articles/'
            . $this->entry_type . '/'
            . $this->id . '/metadata';

  my $response = $this->_ua->get( $url );

  croak 'error: failed to retrieve article data from BioLit: ' . $response->status_line
    unless $response->is_success;

  my $context = $this->_xml_parser->parse_string( $response->decoded_content );
  my $xc      = XML::LibXML::XPathContext->new( $context );

  my @articles = ();
  foreach my $article_node ( $xc->findnodes( '//success/data/list/article' ) ) {
    my $article = {};
    push @articles, $article;
    
    $article->{doi}      = $article_node->findvalue('article-meta/article-id[@pub-id-type="doi"]');
    $article->{abstract} = $article_node->findvalue('article-meta/abstract');
    $article->{year}     = $article_node->findvalue('article-meta/pub-date/year');
    $article->{volume}   = $article_node->findvalue('article-meta/volume');
    $article->{fpage}    = $article_node->findvalue('article-meta/fpage');
    $article->{issue}    = $article_node->findvalue('article-meta/issue');
    $article->{title}    = $article_node->findvalue('article-meta/article-title/title-group');
    $article->{authors}  = $article_node->findvalue('article-meta/contrib-group/contrib');
    $article->{journal}  = $article_node->findvalue('journal-meta/journal-title');

    foreach my $meta ( @{ $article_node->findnodes('article-meta/article-id') }){

      # this is a PubMed article, so just get the ID
      if ( $meta->getAttribute('pub-id-type') eq 'pmid' ) {
        $article->{pmid} = $meta->textContent;
      }

      # this is a PubMed Central ID, so go get the figures too
      elsif ( $meta->getAttribute('pub-id-type') eq 'pmcid') {
        my $pmcid = $meta->textContent;
        $article->{pmcid}   = $pmcid;
        $article->{figures} = $this->_retrieve_figures( $pmcid );
        
      }
    }      
  }
  
  return \@articles;
}

#-------------------------------------------------------------------------------

sub _retrieve_figures {
  my ( $this, $id ) = @_;

  my $url = $this->_biolit_url_root . '/files/'
            . 'pmcid/'
            . $id . '/figures';

  my $response = $this->_ua->get( $url );

  unless ( $response->is_success ) {
    warn qq(warning: failed to retrieve figure data for "$id" from BioLit: )
         . $response->status_line;
    return;
  }

  my $context = $this->_xml_parser->parse_string( $response->decoded_content );
  my $xc      = XML::LibXML::XPathContext->new( $context );

  my @figures = ();
  foreach my $figure_node ( $xc->findnodes( '//success/data/list/figures/figure' ) ) {
    my $figure = {};
    push @figures, $figure;

    $figure->{legend} = $figure_node->textContent;

    foreach my $figure_file_node ( $figure_node->findnodes( 'files/file' ) ) {
      if ( $figure_file_node->hasAttribute('type') and
           $figure_file_node->getAttribute('type') eq 'thumbnail' ) {
        $figure->{thumbnail} = $figure_file_node->textContent;
      }
      else {
        $figure->{image} = $figure_file_node->textContent;
      }
    }
  }
  
  return \@figures;
}

#-------------------------------------------------------------------------------

1;
