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

=head
CREATE TABLE feature_das_sources (
  server_id VARCHAR(40) NOT NULL,
  system VARCHAR(200) NOT NULL,
  sequence_type VARCHAR(200) NOT NULL,
  name VARCHAR(100) NOT NULL,
  url VARCHAR(200) NOT NULL,
  helper_url VARCHAR(200) NULL,
  default_server TINYINT(1) NOT NULL,
  PRIMARY KEY(server_id, system, sequence_type)
);
=cut
# $Id: update_das_sources.pl,v 1.6 2007-02-07 16:05:56 aj5 Exp $

use warnings;
use strict;

use lib qw( /nfs/team71/pfam/jt6/server/PfamLib );

use Bio::DasLite;
use Data::Dumper;
use Data::Validate::URI qw( is_uri );
use DBI;
use Time::Local;

#use Date::Parse;
#use Date::Calc qw( Delta_Days Delta_DHMS );

# config

# Bio::DasLite setup
my $DAS_DSN   = "http://das.sanger.ac.uk/das/pfam";
my $DAS_TO    = 100;
my $DAS_PROXY = "http://wwwcache.sanger.ac.uk:3128";

# db setup
my $DB_DSN  = "dbi:mysql:web_user:pfam:3306";
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
my $insertSth = $dbh->prepare( "INSERT INTO feature_das_sources ( server_id, name, url, system, sequence_type, helper_url, default_server ) VALUES( ?, ?, ?, ?, ?, ?, ? )" );

# get the full list of sources
#my $sourcesList = $das->registry_sources( { category => [ "Protein Sequence", "Protein Structure" ] } );
my $sourcesList = $das->registry_sources();

# decide which sources we want to use
my $chosenList = [ ];
foreach my $source ( @$sourcesList ) {

#   print Dumper( $source );
#   foreach ( strptime( $source->{leaseDate} ) ) {
# 	print "strptime: $_\n";
#   }

  # check the lease date
  $source->{leaseDate} =~ m/^(\d{4})\-(\d{2})\-(\d{2})T(\d{2}):(\d{2}):(\d{2}).(\d{3})Z$/i;

  # convert the lease date into seconds since the epoch. Note that we
  # need to subtract 1 from the day and month, since we need them
  # zero-based but the registry date comes with them as
  # day-of-the-month and month-of-the-year
  my $ld = timelocal( $6, $5, $4, $3 - 1, $2 - 1, $1 );

  # delta, in seconds
  my $dd = $cd - $ld;

  # don't add the source if the lease is older than two days
  if( $dd > 172800 ) {
	print STDERR "(ww) dropping \"$source->{nickname}\" ($source->{id}); down for ".int($dd/86400)." days\n";
	next;
  }

  # don't add the source if it's in the "ignore" list
  print STDERR "(ww) ignoring \"$source->{nickname}\" ($source->{id})\n" and next
	if $ignoreServers->{ $source->{id} };

  my $entry = {};
  $entry->{id}        = $source->{id};
  $entry->{name}      = $source->{nickname};
  $entry->{url}       = $source->{url};
  $entry->{helperurl} = $source->{helperurl};

  # trim any trailing slashes off the URLs
  {
	$/ = "/";
	chomp $entry->{url} if defined($entry->{url});
	chomp $entry->{helperurl} if defined($entry->{helperurl});
  };

  # validate the URLs
  unless( is_uri( $entry->{url} ) ) {
	print STDERR "(ww) dropping \"$source->{nickname}\" ($source->{id}); invalid URL ($source->{url})\n";
	next;
  }

  if( $entry->{helperurl} and not is_uri( $entry->{helperurl} ) ) {
	print STDERR "(ww) dropping \"$source->{nickname}\" ($source->{id}); invalid help URL ($entry->{helperurl})\n";
	next;
  }

  # we're only interested in features
  next unless grep /features/, @{$source->{capabilities}};
  
  foreach my $coords (@{$source->{coordinateSystem}})
  {
  	if (defined $entry->{coords}{$coords->{uniqueId}}) {
		print STDERR "(ww) ignoring duplicate co-ordinate system for $source->{id}\n";
		next;
	}
	$entry->{coords}{$coords->{uniqueId}} = { system => $coords->{name}, type => $coords->{category} };
  }
  
  push (@$chosenList, $entry) if (defined $entry->{coords});

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
  	foreach my $coord (values %{ $entry->{coords} }) {
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
