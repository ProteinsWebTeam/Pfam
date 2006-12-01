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
#
# CREATE TABLE das_sources (
#   server_id     VARCHAR(40)  PRIMARY KEY,
#   name          VARCHAR(100) NOT     NULL,
#   url           VARCHAR(200) NOT     NULL,
#   system        VARCHAR(200) NOT     NULL,
#   helperurl     VARCHAR(200) DEFAULT NULL,
#   defaultServer BOOLEAN      DEFAULT 0
# );
#
# $Id: update_das_sources.pl,v 1.4 2006-12-01 13:30:26 jt6 Exp $

use warnings;
use strict;

use lib qw( /nfs/team71/pfam/jt6/server/PfamLib );

use Bio::DasLite;
use Data::Dumper;
use Data::Validate::URI qw( is_uri );
use DBI;
use Date::Parse;
use Date::Calc qw( Delta_Days Delta_DHMS );

# config

# Bio::DasLite setup
my $DAS_DSN   = "http://das.sanger.ac.uk/das/pfam";
my $DAS_TO    = 100;
my $DAS_PROXY = "http://wwwcache.sanger.ac.uk:3128";

# db setup
my $DB_DSN  = "dbi:mysql:web_user:pfam:3306";
my $DB_USER = "web_user";
my $DB_PASS = "web_user";

# todays date
my( $s, $m, $h, $day1, $month1, $year1, $z ) = localtime( time );

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

# get a Bio::DasLite object that's connected to the specified registry
my $das = Bio::DasLite->new( { dsn     => $DAS_DSN,
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
my $insertSth = $dbh->prepare( "INSERT INTO das_sources ( server_id, name, url, system, helper_url, default_server ) VALUES( ?, ?, ?, ?, ?, ? )" );

# get the full list of sources
my $sourcesList = $das->registry_sources( { category => [ "Protein Sequence", "Protein Structure" ] } );

# decide which sources we want to use
my $chosenList = {};
foreach my $source ( @$sourcesList ) {

  # print Dumper( $source );

  # check the lease date
  my( $s, $m, $h, $day2, $month2, $year2, $z ) = strptime( $source->{leaseDate} );
  my $since = Delta_Days( $year2, $month2, $day2, $year1, $month1, $day1 );

  # don't add the source if the lease is older than two days
  print STDERR "(ww) dropping \"$source->{nickname}\"; down for $since days\n" and next
	if $since > 2;

  # don't add the source if it's in the "ignore" list
  print STDERR "(ww) ignoring \"$source->{nickname}\"\n" and next
	if $ignoreServers->{ $source->{id} };

  my $featureCount = 0;
  my $sysCount = 0;

  my $entry = {};
  $entry->{id}        = $source->{id};
  $entry->{name}      = $source->{nickname};
  $entry->{url}       = $source->{url};
  $entry->{helperurl} = $source->{helperurl};

  # trim any trailing slashes off the URLs
  {
	$/ = "/";
	chomp $entry->{url};
	chomp $entry->{helperurl};
  };

  # validate the URLs
  unless( is_uri( $entry->{url} ) ) {
	print STDERR "(ww) dropping \"$source->{nickname}\"; invalid URL ($source->{url})\n";
	next;
  }

  if( $entry->{helperurl} and not is_uri( $entry->{helperurl} ) ) {
	print STDERR "(ww) dropping \"$source->{nickname}\"; invalid help URL ($entry->{helperurl})\n";
	next;
  }

  # we're only interested in features
  foreach my $capability ( @{$source->{capabilities}} ) {
	$featureCount++ and last if $capability eq "features";
  }
  next unless $featureCount;

  # convert the list of coordinate systems into a hash, so that we can
  # quickly look up the type of coordinate systems that this source
  # can handle
  my $map = {};
  map { $map->{$_->{name}} = 1 } @{$source->{coordinateSystem}};

  if( exists $map->{UniProt} ) {
	$sysCount++;
	$entry->{system} = "UniProt";
  } elsif( exists $map->{Ensembl} ) {
	$sysCount++;
	$entry->{system} = "Ensembl";
  } elsif( exists $map->{PDBresnum} ) {
	$sysCount++;
	$entry->{system} = "PDBresnum";
  }

  # if we've found at least one acceptable coordinate system, stash
  # this source
  $chosenList->{$entry->{id}} = $entry if $sysCount;

}

# make sure we have some sources before going any further
unless( scalar keys %$chosenList ) {
  print STDERR "(EE) ERROR: no sources retrieved; leaving the table untouched\n";
  exit 1;
}

# update the database
print STDERR "(ii) opening transaction...\n";

# start a transaction...
eval {

  # truncate the table
  print STDERR "(ii) dropping table... ";
  $dbh->do( "DELETE FROM das_sources" );
  print "done\n";

  # insert them
  foreach my $name ( sort keys %$chosenList ) {
	print STDERR "(ii) inserting $chosenList->{$name}->{name}... ";
	$insertSth->execute( $chosenList->{$name}->{id},
						 $chosenList->{$name}->{name},
						 $chosenList->{$name}->{url},
						 $chosenList->{$name}->{system},
						 $chosenList->{$name}->{helperurl},
					     exists $defaultServers->{ $chosenList->{$name}->{id} } ? 1 : 0
					   );

	print STDERR "done\n";
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
