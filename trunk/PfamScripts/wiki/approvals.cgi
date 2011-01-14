#!/usr/bin/perl

# this is a CGI script to return the list of approved wikipedia
# articles, their current approved revision number, and the
# accessions of Pfam and Rfam families that map to each article.
#
# jt6 20101124 WTSI
#
# $Id$

use strict;
use warnings;

use Config::General;
use DBI;
use PfamDB;
use RfamDB;
use JSON;
use CGI;

use Data::Dumper;

my $q = CGI->new;

# get database connection parameters from the config file
my $cg = Config::General->new( '/pfam/home/config/wiki.conf' );

# parse the config and get the section relevant to the wikipedia stuff
my %config = $cg->getall;
my $wa_conf = $config{WikiApprove};
my $pfam_conf= $config{PfamDB};
my $rfam_conf= $config{RfamDB};

# get database connections
my $wa_dbh = DBI->connect( "dbi:mysql:$wa_conf->{db_name}:$wa_conf->{db_host}:$wa_conf->{db_port}", 
                           $wa_conf->{username}, $wa_conf->{password},
                           { PrintError => 0,
                             RaiseError => 0 } );
# (we're not using the DBIC wrapper for the wiki_approve database, because it's
# more than just a simple table wrapper. It requires MediaWiki::API, which is
# going to be a pain to get installed and working on the SVN machine.)

my $pfam_schema = PfamDB->connect( "dbi:mysql:$pfam_conf->{db_name}:$pfam_conf->{db_host}:$pfam_conf->{db_port}", 
                                   $pfam_conf->{username}, $pfam_conf->{password} );

my $rfam_schema = RfamDB->connect( "dbi:mysql:$rfam_conf->{db_name}:$rfam_conf->{db_host}:$rfam_conf->{db_port}", 
                                   $rfam_conf->{username}, $rfam_conf->{password} );

# if we can't even connect, we're done here. At least put a hint in
# the body of the response though
unless ( $wa_dbh and $pfam_schema and $rfam_schema ) {
  print $q->header( -status => 500 ),
        "Couldn't retrieve approvals list";
  exit;
}

# get the title and approved version for every approved article
my $titles_sth = $wa_dbh->prepare( "SELECT title, approved_revision, pfam_status, rfam_status FROM wikipedia WHERE pfam_status != 'inactive' OR rfam_status != 'inactive'" );
$titles_sth->execute;

my $articles = {};
foreach my $title_row ( @{ $titles_sth->fetchall_arrayref } ) {

  # skip any articles that (for some reason) don't map to any families
  next if ( $title_row->[2] eq 'inactive' and $title_row->[3] eq 'inactive' );

  my $article = {
    approved_revision => $title_row->[1],
    pfam_families     => [],
    rfam_families     => [],
  };

  # retrieve Pfam family accessions for this article
  if ( $title_row->[2] ne 'inactive' ) { # pfam_status

    my @rows = $pfam_schema->resultset( 'Pfama' )
                           ->search( { title => $title_row->[0] },
                                     { join     => { 'pfama_wikis' => 'auto_wiki' },
                                       prefetch => { 'pfama_wikis' => 'auto_wiki' } } );

    # this is a little convuloted, because Pfam allows a many-to-many
    # relationship between families and articles
    foreach my $row ( @rows ) {
      foreach my $pfam ( $row->pfama_wikis ) {
        push @{ $article->{pfam_families} }, $pfam->auto_pfama->pfama_acc;
      }
    }
  }

  # retrieve Rfam family accessions for this article
  if ( $title_row->[3] ne 'inactive' ) { # rfam_status

    my @rows = $rfam_schema->resultset( 'Rfam' )
                           ->search( { title => $title_row->[0] },
                                     { join     => 'auto_wiki' } );

    foreach my $row ( @rows ) {
      push @{ $article->{rfam_families} }, $row->rfam_acc;
    }
  }

  # it's possible that there will be no accession, even though this
  # article is marked as "pending" for one or both of Pfam and Rfam.
  # This is because the article could be mapped to a family in the
  # live databases, rather than the release database. Need to take
  # care of this in the database really...

  # provided we got some accessions, store the article details
  if ( scalar @{ $article->{pfam_families} } or
       scalar @{ $article->{rfam_families} } ) {
    $articles->{$title_row->[0]} = $article;
  }

}

# convert the data structure to JSON and dump it out
my $json = JSON->new;
$json->pretty(1);

print $q->header( -type => 'application/json',
                  -Content_Disposition => 'attachment; filename=article_mapping.json' ),
      $json->encode( $articles );

