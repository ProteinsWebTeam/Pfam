#!/nfs/team71/pfam/jt6/server/perl/bin/perl
#
# update_das_sources.pl
# jt6 20060428 WTSI
#
# Update the list of DAS sources in the database. This is intended to
# be run as a cron job, as often as required to keep the database
# table up to date.
#
# The script populates a table that's created as follows:
#   CREATE TABLE feature_das_sources (
#     server_id VARCHAR(40) NOT NULL,
#     system VARCHAR(200) NOT NULL,
#     sequence_type VARCHAR(200) NOT NULL,
#     name VARCHAR(100) NOT NULL,
#     url VARCHAR(200) NOT NULL,
#     helper_url VARCHAR(200) NULL,
#     default_server TINYINT(1) NOT NULL,
#     PRIMARY KEY(server_id, system, sequence_type)
#   );
#
# $Id: update_das_sources.pl,v 1.9 2007-05-16 15:23:07 jt6 Exp $
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

use warnings;
use strict;

use lib qw( /nfs/team71/pfam/jt6/server/pfam-site );

use Bio::Das::Lite;
use Data::Validate::URI qw( is_uri );
use DBI;
use Time::Local;

# config

# Bio::Das::Lite setup
my $DAS_DSN   = "http://das.sanger.ac.uk/das/pfam";
my $DAS_TO    = 100;
my $DAS_PROXY = "http://wwwcache.sanger.ac.uk:3128";

# db setup
my $DB_DSN  = "dbi:mysql:web_user:pfamdb1:3306";
my $DB_USER = "web_user";
my $DB_PASS = "web_user";

# todays date, in seconds since the epoch
my $cd = time;

# default servers
my $defaultServers = { DS_109 => 1, # uniprot
                       DS_120 => 1, # superfamily
                       DS_210 => 1, # SMART
                       DS_311 => 1, # Pfam Other Features
                       DS_327 => 1, # interpro
                     };

# ignore these servers
my $ignoreServers = { DS_241 => 1, # Pfam
                    };

# main

# get a Bio::Das::Lite object that's connected to the specified registry
my $das = Bio::Das::Lite->new( { dsn     => $DAS_DSN,
                                 timeout => $DAS_TO,
                                 proxy   => $DAS_PROXY } );

# get a database handle
my $dbh = DBI->connect( $DB_DSN,
                        $DB_USER,
                        $DB_PASS,
                        { RaiseError => 1,
                          PrintError => 0,
                          AutoCommit => 0 } );

# prepare the queries
my $insertSth = $dbh->prepare( "INSERT INTO feature_das_sources ( server_id, name, url, system, sequence_type, helper_url, default_server ) VALUES( ?, ?, ?, ?, ?, ?, ? )" );

# get the full list of sources
#my $sourcesList = $das->registry_sources( { category => [ "Protein Sequence", "Protein Structure" ] } );
my $sourcesList = $das->registry_sources();
print STDERR "(ii) retrieved " . scalar @$sourcesList . " sources from the registry\n"; 

# decide which sources we want to use
my $chosenList = [ ];
foreach my $source ( @$sourcesList ) {
  
  # check the lease date
  $source->{leaseDate} =~ m/^(\d{4})\-(\d{2})\-(\d{2})T(\d{2}):(\d{2}):(\d{2}).(\d{3})Z$/i;

  # convert the lease date into seconds since the epoch. Note that we need 
  # to subtract 1 from the month, since we need it zero-based but the 
  # registry date comes with it as and month-of-the-year, i.e. 1-based
  my $ld = timelocal( $6, $5, $4, $3, $2 - 1, $1 );

  # delta, in seconds
  my $dd = $cd - $ld;

  # don't add the source if the lease is older than two days
  if( $dd > 172800 ) {
    print STDERR "(ww) skipping \"$source->{nickname}\" ($source->{id}); down for ".int($dd/86400)." days\n";
    next;
  }

  # don't add the source if it's in the "ignore" list
  if( $ignoreServers->{ $source->{id} } ) {
    print STDERR "(ww) ignoring \"$source->{nickname}\" ($source->{id})\n";
    next;
  }

  my $entry = {};
  $entry->{id}        = $source->{id};
  $entry->{name}      = $source->{nickname};
  $entry->{url}       = $source->{url};
  $entry->{helperurl} = $source->{helperurl};

  # trim any trailing slashes off the URLs
  {
    $/ = "/";
    chomp $entry->{url}       if defined $entry->{url};
    chomp $entry->{helperurl} if defined $entry->{helperurl};
  };

  # validate the URLs
  unless( is_uri( $entry->{url} ) ) {
    print STDERR "(ww) skipping \"$source->{nickname}\" ($source->{id}); invalid URL ($source->{url})\n";
    next;
  }

  if( $entry->{helperurl} and not is_uri( $entry->{helperurl} ) ) {
    print STDERR "(ww) skipping \"$source->{nickname}\" ($source->{id}); invalid help URL ($entry->{helperurl})\n";
    next;
  }

  # we're only interested in features
  unless( grep /features/, @{ $source->{capabilities} } ) {
    print STDERR "(ww) skipping \"$source->{nickname}\" ($source->{id}); no features\n";
    next;
  }

  foreach my $coords ( @{ $source->{coordinateSystem} } ) {
    if( defined $entry->{coords}{$coords->{uniqueId}} ) {
      print STDERR "(ww) ignoring duplicate co-ordinate system for $source->{id}\n";
      next;
    }
    $entry->{coords}{ $coords->{uniqueId} } = { system => $coords->{name}, 
                                                type   => $coords->{category} };
  }
  
  if( defined $entry->{coords} ) {
    push @$chosenList, $entry;
  } else {
    print STDERR "(ww) no coordinates found for $source->{id}\n";
  }
}

# make sure we have some sources before going any further
unless( scalar @$chosenList ) {
  print STDERR "(EE) ERROR: no sources retrieved; leaving the table untouched\n";
  exit 1;
}

# update the database
print STDERR "(ii) opening transaction...\n";

# start a transaction...
eval {

  # truncate the table
  print STDERR "(ii) emptying table... ";
  $dbh->do( "DELETE FROM feature_das_sources" );
  print "done\n";

  # insert them
  foreach my $entry( sort { $a->{id} cmp $b->{id} } @$chosenList ) {

    foreach my $coord (values %{ $entry->{coords} } ) {
      print STDERR "(ii) inserting $entry->{name} [$coord->{system},$coord->{type}]... ";
      $insertSth->execute( $entry->{id},
                           $entry->{name},
                           $entry->{url},
                           $coord->{system},
                           $coord->{type},
                           $entry->{helperurl},
                           exists $defaultServers->{ $entry->{id} } ? 1 : 0
                         );
      print STDERR "done\n";
    }
  }

  $dbh->commit;
  print STDERR "(ii) committed changes\n";

}; # end of "eval"

# check for errors in the transaction and roll back if we found any
if( $@ ) {
  print STDERR "\n(EE) ERROR: transaction error: $@; rolling back\n";
  eval { $dbh->rollback; };
} else {
  print STDERR "(ii) transaction successful\n";
}

# done
