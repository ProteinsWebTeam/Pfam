#!/usr/bin/perl

# this is a simple CGI script to return the mapping between Pfam/Rfam
# entries and wikipedia articles.
#
# jt6 20110114 WTSI
#
# $Id$

use strict;
use warnings;

use Config::General;
use DBI;
use JSON;
use CGI;

my $q = CGI->new;

# get database connection parameters from the config file
my $cg = Config::General->new( '/pfam/home/config/wiki.conf' );
# my $cg = Config::General->new( '/nfs/users/nfs_j/jt6/perl/wiki/wiki_testing.conf' );

# parse the config and get the section relevant to the wikipedia stuff
my %config = $cg->getall;
my $conf = $config{WikiApprove};

# get a database connection
my $dsn = "dbi:mysql:$conf->{db_name}:$conf->{db_host}:$conf->{db_port}";

my $dbh = DBI->connect( $dsn, $conf->{username}, $conf->{password},
                        { PrintError => 0,
                          RaiseError => 0 } );

# if we can't even connect, we're done here. At least put a hint in
# the body of the response though
unless ( $dbh ) {
  print $q->header( -status => 500 ),
        "Couldn't retrieve mapping";
  exit;
}

# do we want the Pfam or Rfam mapping, or (the default) both ?
my $source;
my $filename = 'article_mapping.json';
my $query    = 'SELECT accession, title FROM article_mapping';

if ( defined $q->param('db') and
     $q->param('db') eq 'pfam' ) {
  $filename  = 'pfam_article_mapping.json';
  $query    .= ' where db = "pfam"';
}
elsif ( defined $q->param('db') and
        $q->param('db') eq 'rfam' ) {
  $filename  = 'rfam_article_mapping.json';
  $query    .= ' where db = "rfam"';
}

my $data = $dbh->selectcol_arrayref( $query, { Columns => [ 1, 2 ] } );

# (make sure we retrieved *something*)
unless ( $data ) {
  print $q->header( -status => 500 ),
        "Couldn't generate article mapping";
  exit;
}

# convert the array elements straight to a hash and dump it to the 
# response as a JSON string
my %hash = @$data;
 
print $q->header( -type => 'application/json',
                  -Content_Disposition => "attachment; filename=$filename" ),

      to_json( \%hash );
#     to_json( \%hash, { pretty => 1 } ); # to generate a "pretty" string

