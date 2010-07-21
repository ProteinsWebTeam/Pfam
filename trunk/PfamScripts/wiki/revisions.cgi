#!/usr/bin/perl

# this is a simple CGI script to retrieve the list of wikipedia titles
# and their approved revision numbers from the wikipedia tracking DB.
#
# jt6 20100621 WTSI
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

# parse the config and get the section relevant to the wikipedia stuff
my %config = $cg->getall;
my $conf = $config{WikiApprove};

# get a database connection
my $dsn = "dbi:mysql:$conf->{db_name}:$conf->{db_host}:$conf->{db_port}";

my $dbh = DBI->connect( $dsn, $conf->{username}, $conf->{password},
                        { RaiseError => 0 } );

# if we can't even connect, we're done here. At least put a hint in
# the body of the response though
unless ( $dbh ) {
  print $q->header( -status => 500 ),
        "Couldn't retrieve revision list";
  exit;
}

# get the title and approved version for every article. Convert that
# list into a hash and dump it as a JSON string
my $data = $dbh->selectcol_arrayref( "SELECT title, approved_revision FROM wikipedia", 
                                     { Columns => [ 1, 2 ] } );

# (make sure we retrieved *something*)
unless ( $data ) {
  print $q->header( -status => 500 ),
        "Couldn't generate revision list";
  exit;
}

my %hash = @$data;

print $q->header( -type => 'application/json',
                  -Content_Disposition => 'attachment; filename=article_revisions.json' ),
      to_json( \%hash );
#     to_json( \%hash, { pretty => 1 } ); # to generate a "pretty" string

