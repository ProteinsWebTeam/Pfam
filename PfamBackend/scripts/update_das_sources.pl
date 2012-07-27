#!/usr/local/bin/perl
#
# update_das_sources.pl
#
# Updates the list of DAS feature sources in the database. See the pod at
# the bottom of the script for details.
# jt6 20111003 WTSI
#
# $Id$

use warnings;
use strict;

use Data::Dump qw(dump);
use Log::Log4perl qw(get_logger :levels);
use Getopt::Long;
use Pod::Usage;
use Config::General;
use DBI;
use Bio::Das::Lite;
use Data::Validate::URI qw( is_uri );
use Time::Local;

#-------------------------------------------------------------------------------

# set up logging
my $logger_conf = q(
  log4perl.logger                   = INFO, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );

my $log = get_logger();

#-------------------------------------------------------------------------------

# options

my $config_file = 'conf/wiki_external_crons.conf';
my $help = 0;
my $force = 0;

GetOptions( 'force'    => \$force,
            'config=s' => \$config_file,
            'help|?'   => \$help )
  or pod2usage(2);

pod2usage( -verbose => 2) if $help;

$log->logdie( "ERROR: couldn't read config from '$config_file': $!" )
  unless -e $config_file;


#-------------------------------------------------------------------------------

# configuration

# parse the config
my $cg        = Config::General->new($config_file);
my %config    = $cg->getall;

# Bio::Das::Lite setup
my $DAS_DSN   = $config{WebUser}->{das_dsn};
my $DAS_TO    = $config{WebUser}->{das_to};
my $DAS_PROXY = $config{WebUser}->{das_proxy};

# db setup
my $DB_NAME   = $config{WebUser}->{db_name};
my $DB_HOST   = $config{WebUser}->{db_host};
my $DB_PORT   = $config{WebUser}->{db_port};
my $DB_USER   = $config{WebUser}->{username};
my $DB_PASS   = $config{WebUser}->{password};

# get the lists of default servers and those to ignore entirely
my %default_servers = map { $_ => 1 } @{ $config{update_cron}->{defaultServer} };
my %ignore_servers  = map { $_ => 1 } @{ $config{update_cron}->{ignoreServer} };

#-------------------------------------------------------------------------------

# main

my $DB_DSN = "dbi:mysql:database=$DB_NAME;host=$DB_HOST;port=$DB_PORT";

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

# create a temporary table and populate the results there
eval {
  $dbh->do( "CREATE TEMPORARY TABLE temp LIKE feature_das_sources" );
};
if ( $@ ) {
  $log->logdie( "ERROR: temporary table creation failed: $@" );
}

# get the total number of features sources from the database;
my $feature_sources = $dbh->selectrow_arrayref('SELECT COUNT(*) FROM feature_das_sources');
my $num_total_features  = $feature_sources->[0];

$log->info( "retrieved a total of $num_total_features features" );

# get the list of sources with features capability
my $sources_list = $das->registry_sources( { capability => 'features' } );

$log->info( 'retrieved ' . scalar @$sources_list . ' sources from the registry' );

# todays date, in seconds since the epoch
my $cd = time;

# decide which sources we want to use
my $chosen_list = []; 
my $sources_to_populate = 0;
foreach my $source ( @$sources_list ) {
  
  # parse the properties into a hash. Ignore the fact that there may be multiple
  # values for each key, since there really should be just one "leaseTime"
  my %properties = map { $_->{prop_name} => $_->{prop_value } } @{ $source->{prop} };

  # check the lease date ("2011-09-21T06:21:13+0100")
  $properties{leaseTime} =~ m/^(\d{4})\-(\d{2})\-(\d{2})/;

  # convert the lease date into seconds since the epoch. Note that we need 
  # to subtract 1 from the month, since we need it zero-based but the 
  # registry date comes with it as and month-of-the-year, i.e. 1-based
  my $ld = timelocal( 0,0,0,$3, $2 - 1, $1 );
 
  # delta, in seconds
  my $dd = $cd - $ld;

  # don't add the source if the lease is older than two days
  if ( $dd > $config{update_cron}->{active_time} ) {
    $log->warn( qq|skipping "$source->{title}" ($source->{id}); down for >| . int( $dd / 86400 ) . q| days| );
    next;
  }

  # don't add the source if it's in the "ignore" list
  if ( $ignore_servers{ $source->{id} } ) {
    $log->warn( qq|ignoring "$source->{title}" ($source->{id})| );
    next;
  }

  my $entry = {
    id        => $source->{id},
    name      => $source->{title},
    url       => $source->{url},
    helperurl => $source->{helperurl},
    coords    => {},
  };

  # trim any trailing slashes off the URLs
  {
    $/ = '/';
    chomp $entry->{url}       if defined $entry->{url};
    chomp $entry->{helperurl} if defined $entry->{helperurl};
  };

  # validate the URLs
  unless ( is_uri( $entry->{url} ) ) {
    $log->warn( qq|skipping "$source->{title}" ($source->{id}); invalid URL ($source->{url})| );
    next;
  }

  if ( $entry->{helperurl} and not is_uri( $entry->{helperurl} ) ) {
    $log->warn( qq|skipping "$source->{title}" ($source->{id}); invalid help URL ($entry->{helperurl})| );
    next;
  }

  # collect the coordinate systems
  foreach my $coords ( @{ $source->{coordinateSystem} } ) {
    my $unique_id = $coords->{category} . $coords->{name};
    if ( defined $entry->{coords}{$unique_id} ) {
      $log->warn( qq|ignoring duplicate coordinate system for "$source->{id}"| );
      next;
    }
    $entry->{coords}{$unique_id} = { system => $coords->{name}, 
                                     type   => $coords->{category} };
    $sources_to_populate++;
  }
  
  if ( defined $entry->{coords} ) {
    push @$chosen_list, $entry;
  }
  else {
    $log->warn( qq|no coordinates found for "$source->{id}"| );
  }
}

# make sure we get certain number of sources, or else we exit with an error message

unless ( $force ) {  
  $log->debug( "entering the total_features block with \$num_total_features set to |$num_total_features|" );
  if ( $num_total_features ) {
    my $percentage = ( ( $num_total_features - $sources_to_populate ) / $num_total_features ) * 100;
    my $num_sources = scalar( @$sources_list );
    my $num_chosen  = scalar( @$chosen_list );
    my $threshold   = $config{update_cron}->{threshold};
    if ( $percentage > $threshold ) {
      print STDERR <<EOF_msg1;

The total number of features sources present in the database is $num_total_features.
The total number of features sources retrieved from DAS is $num_sources.
the total sources which succeeds validation is $num_chosen.

Due to multiple coordinate systems for some sources, the number of sources 
to be populated is $sources_to_populate.

$percentage of active DAS sources have been lost (>$threshold%), so we're skipping 
this update.
EOF_msg1
      exit 1;
    }
  }
  else {
    print STDERR <<EOF_msg2;

There are no feature sources present in database.

We cannot calculate a percentage loss or gain in the number of sources, so 
we're skipping this update (run the script with the option "-force" to 
override this check).
EOF_msg2
    exit 1;
  }
}

#-------------------------------------------------------------------------------

# actually update the database

my $insert_sth = $dbh->prepare('INSERT INTO temp ( server_id, name, url, system, sequence_type, helper_url, default_server ) VALUES( ?, ?, ?, ?, ?, ?, ? )');

$log->debug( 'opening transaction...' );

eval {
  
  foreach ( sort { $a->{id} cmp $b->{id} } @$chosen_list ) {
    
    foreach my $coord ( values %{ $_->{coords} } ) {
      $log->debug( 'the coordinates are: ', dump $coord );
      $log->debug( "inserting $_->{name} [$coord->{system}, $coord->{type}]" );
      $insert_sth->execute(
        $_->{id},
        $_->{name},
        $_->{url},
        $coord->{system},
        $coord->{type},
        $_->{helperurl},
        exists $default_servers{ $_->{id} } ? 1 : 0
      );  
    }
  
  }
  
  $dbh->commit;

  $log->debug( 'committed changes to table' );
};
if ($@) {
  $log->logdie( "failed to insert sources into database: $@" );
}

#-------------------------------------------------------------------------------

# before copying the contents of temporary table; look at the diff of both;

# source in temp table but not in database table;
my $diff1 = $dbh->prepare( 'SELECT server_id, system, name FROM temp WHERE server_id NOT IN ( SELECT server_id FROM feature_das_sources )' );

# source in das table but lost in temp table;
my $diff2 = $dbh->prepare( 'SELECT server_id, system, name FROM feature_das_sources WHERE server_id NOT IN ( SELECT server_id FROM temp )' );

eval {
  $diff1->execute();
  $diff2->execute();    
};
if ($@) {
  $log->logdie( "failed to calculate differences between new and old lists: $@" );
}

my ( $gains, $losses );

my ( $id, $system, $name );
while ( ( $id, $system, $name ) = $diff1->fetchrow_array ) {
  $gains  .= "$id\t$system\t$name\n";
}
while ( ( $id, $system, $name ) = $diff2->fetchrow_array ) {
  $losses .= "$id\t$system\t$name\n";
}

#-------------------------------------------------------------------------------

# copy results from temporary table to live one

eval {
  $dbh->do( 'DELETE FROM feature_das_sources' );
  $dbh->do( 'INSERT INTO feature_das_sources ( SELECT * FROM temp )' );
  $dbh->commit;
};
if ($@) {
  $log->logdie( qq(Failed to empty and re-populate "feature_das_sources" table: $@) );
}

my $new_feature_sources = $dbh->selectrow_arrayref( 'SELECT COUNT(*) FROM feature_das_sources' );
my $num_new_total_features  = $new_feature_sources->[0];

print STDERR <<EOF_msg3;

The total number of features sources present before update was $num_total_features.
The total number of features sources present after  update is  $num_new_total_features.

EOF_msg3

if ( $gains ) {
  print STDERR "Gained sources:\n$gains\n";
}
elsif ( $losses ) {
  print STDERR "Lost sources:\n$losses\n";
}
else {
  print STDERR "No sources lost or gained.\n";
}

exit;

#-------------------------------------------------------------------------------

__END__

=head1 NAME

update_das_sources_cron.pl - Update the database table storing active DAS sources

=head1 SYNOPSIS

update_das_sources_cron.pl -c configuration_file.conf [-force]

  Options:
    --config   Config::General-style configuration file
    --force    override the check on the percentage gain/loss in number of sources
    --help     show this help message

=head1 OPTIONS

=over 8

=item B<--config> | B<-c>

Specifies the configuration file for the script. The file must be a
L<Config::General> (Apache-style) configuration file.

=item B<--force> | B<-f>

The script calculates the number of sources that would be gained or lost by an
update and will refuse to update the live database table if the percentage change
is greater than a threshold specified in the configuration file. Use B<--force>
to override this check and update the table regardless of the percentage change 
in number of sources.

=item B<--help> | B<-h>

Prints the help message and exits.

=back

=head1 DESCRIPTION

This script will retrieve the list of currently available DAS sources from the 
DAS registry, check that they are still active and then insert them into the
"feature_das_sources" table in the "web_user" database, for use by the Pfam
website.

Servers are assumed to be dead if their "lease date" is greater than the value
specified in the configuration file (default: 3 days). Dead servers will be 
removed from the table and so become unavailable for use through the DAS
feature viewer in the Pfam website.

=head1 REQUIREMENTS

=head2 Bio::Das::Lite

This script requires a (currently) unreleased version of L<Bio::Das::Lite>, 
which includes a work-around for changes in the output of the DAS registry,
available from:

    https://bio-das-lite.svn.sourceforge.net/svnroot/bio-das-lite

=head2 Database table

The DAS feature source list is stored in a database table. The connection
parameters for the DB should be specified in the appropriate section of
the L<configuration/"Database connection parameters">. The table can be
generated with the following SQL:

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


=head1 CONFIGURATION

The L<Config::General> configuration file must contain the following
sections.

=head2 Database connection parameters

    <WebUser>
      db_name  web_user    # schema name
      db_host  mydbhost    # database server hostname
      db_port  3306
      username mydbuser    # database account
      password mydbpass    # database password
    </WebUser>

=head2 Script configuration

    <update_cron>

      # DasLite connection parameters
      das_dsn   "http://das.sanger.ac.uk/das/pfam"
      das_to    100
      das_proxy "http://<proxy URL>:3128"

      # active time of the das source
      active_time  172800
      threshold    10 # percent of active servers that can be lost in a single update

      # the set of default servers that will be shown by default in DAS features
      # viewer in the Pfam website that reads this list of servers
      defaultServer DS_120  # superfamily
      defaultServer DS_210  # SMART
      defaultServer DS_241  # Pfam
      defaultServer DS_327  # interpro
      defaultServer DS_359  # phobius
      defaultServer DS_544  # Merops
      defaultServer DS_409  # Uniprot
      defaultserver DS_817  # prosite
      defaultserver DS_448  # gene3d
      defaultserver DS_447  # CATH

      # the set of servers to ignore entirely
      # (a single server must be included twice, because of the way that
      # Config::General treats single values as a scalar, whereas we want this to
      # be an array at all times)
      #ignoreServer DS_241
      #ignoreServer DS_241

    </update_cron>

=cut

