#!/software/bin/perl
#
# unknown_function.pl
# jt6 20120731 WTSI
#
# a simple script to keep track of "families of unknown function". Relies on 
# a table in the web_user database, which stores only an accession (either
# Pfam or Rfam) and a database (also either "pfam" or "rfam"). Also lists the
# current set of flagged families as a simple text file.
#
# $Id$

use strict;
use warnings;

use Log::Log4perl;
use Getopt::Long;
use Try::Tiny;
use DateTime;

use Bio::Pfam::Config;
use Bio::Pfam::WebUserDBManager;

#-------------------------------------------------------------------------------
# configure logging

my $log_conf = q(
  log4perl.logger                   = FATAL, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$log_conf );
my $log = Log::Log4perl->get_logger;

#-------------------------------------------------------------------------------
# check database connection parameters

my $config = Bio::Pfam::Config->new;
my $connect_details = $config->webuseradmin;

my $wu_db = Bio::Pfam::WebUserDBManager->new( %$connect_details )
  or die "ERROR: couldn't connect to pfam_live: $!";

my $SCHEMA  = $wu_db->getSchema;

$SCHEMA->storage()->debug( 1 )
  if $log->is_debug;

#-------------------------------------------------------------------------------
# handle arguments

my ( $project, $remove, $help );
my $go = GetOptions( 'project:s' => \$project,
                     'remove'    => \$remove,
                     'help'      => \$help );
unless ( $go ) {
  usage();
  print STDERR "ERROR: invalid option(s)\n";
  exit 1;
}

if ( $help ) {
  usage();
  exit 0;
}

my $acc = uc shift;

unless ( $acc or $project ) {
  print STDERR "ERROR: you must specify an either accession to add/remove, or a project to list\n";
  usage();
  exit 1;
}

if ( $acc and $project ) {
  print STDERR "ERROR: you must specify either an accession to add/remove, or a project to list, but not both\n";
  exit 1;
}

if ( defined $project ) {
  unless ( $project eq 'pfam' or 
           $project eq 'rfam' ) {
    print STDERR "ERROR: 'project' must be either 'pfam' or 'rfam'\n";
    exit 1;
  }
}
else {
  if ( $acc =~ m/^PF\d{5}$/ ) {
    $project = 'pfam';
  }
  elsif ( $acc =~ m/^RF\d{5}$/ ) {
    $project = 'rfam';
  }
  else {
    print STDERR "ERROR: '$acc' is not a valid accession\n";
    exit 1;
  }
}

$log->debug( "\$project = '$project'" );
$log->debug( "\$acc     = '$acc'" );
$log->debug( "\$remove  = '" . ( defined $remove ? 1 : 0 ) . "'" );
                            
#-------------------------------------------------------------------------------
# do what we came here to do...

if ( $acc ) {
  if ( $remove ) {
    remove_row( $acc );
  }
  else {
    add_row( $acc, $project );
  }
}
else {
  list_all( $project );
}

exit;

#-------------------------------------------------------------------------------
# methods ----------------------------------------------------------------------
#-------------------------------------------------------------------------------

# add the specified accession/project to the database

sub add_row {
  my ( $acc, $project ) = @_;

  my $row;
  try {
    $row = $SCHEMA->resultset('UnknownFunction')
                  ->find_or_new( { accession => $acc,
                                   db        => $project },
                                 { key => 'primary' } );
  } catch {
    print STDERR "ERROR: there was a problem finding/inserting the row for '$acc' into the database: $!\n";
    exit 1;
  };

  if ( $row->in_storage ) {
    print "'$acc' is already flagged as being 'of unknown function'\n";
  }
  else {
    $row->insert;
    print "'$acc' has been flagged as 'of unknown function'\n";
  }

}

#-------------------------------------------------------------------------------
# remove the specified accession from the database

sub remove_row {
  my $acc = shift;

  my $row;
  try {
    $row = $SCHEMA->resultset('UnknownFunction')
                  ->find( $acc );
  } catch {
    print STDERR "ERROR: there was a problem finding the row for '$acc' in the database: $!\n";
    exit 1;
  };

  if ( $row ) {
    $row->delete;
    print "removed '$acc'\n";
  }
  else {
    print "'$acc' does not appear to have been flagged as 'of unknown function' yet\n";
  }

}

#-------------------------------------------------------------------------------
# list the current set of flagged accessions for the specified project

sub list_all {
  my $project = shift;

  my $rs;
  try {
    $rs = $SCHEMA->resultset('UnknownFunction')
                 ->search( { db => $project },
                           { order_by => [ 'accession' ] } );
  } catch {
    print STDERR "ERROR: there was a problem getting the list of all rows for '$project': $!\n";
    exit 1;
  };

  print '# ' . ucfirst $project . ' families flagged as having unknown function as of ' 
        . DateTime->now . "\n";
  while ( my $row = $rs->next ) {
    print $row->accession, "\n";
  }

}

#-------------------------------------------------------------------------------

sub usage {
  print <<EOF_help;

usage: $0 [ --project pfam|rfam ] | [ [--remove] <accession> ]

This scripts adds (or removes) a specified family to the list of families of
unknown function. If a project ('pfam' or 'rfam') is specified, the list of
currently flagged families in that project are listed to STDOUT.

parameters:
  --project | -p <project>   list families in <project> (either pfam or rfam)
  --remove | -r <accession>  removes the specified family from the list
  --help | -h                prints this message

You must specify either an accession to add/remove, or a project to list.
The accession must be a Pfam or Rfam accession, i.e. matching the regular
expression [PR]F\\d{5}, e.g. PF12345 or RF98765. The project must be either
"pfam" or "rfam" (without the quotes).

If the family to be added or removed is already present or absent from the
database, a message is printed to that effect, but nothing else is done.

examples:
  # add PF12345 to the list of families with unknown function 
  shell% $0 PF12345
  'PF12345' has been flagged as 'of unknown function'

  # remove PF12345 from the list
  shell% $0 -r PF12345
  removed 'PF12345'

  # list all "families of unknown function" in Pfam
  shell% $0 -p pfam
  # Pfam families flagged as having unknown function as of 2012-07-31T09:55:17
  PF54321
  ...

EOF_help
}
