#!/software/bin/perl
#
# archive_files.pl
# jt6 20120531 WTSI
#
# a simple script to backup a set of named files from lustre into a tar file
#
# $Id$

use strict;
use warnings;

use Log::Log4perl qw( get_logger :levels );
use Getopt::Long;
use Archive::Tar;
use DateTime;

use constant LFS => '/usr/bin/lfs';

#-------------------------------------------------------------------------------

# configure logging
BEGIN {
  Log::Log4perl->init( \<<EOF_log_config
    log4perl.rootLogger=INFO, SCREEN
    log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
    log4perl.appender.SCREEN.mode=append
    log4perl.appender.SCREEN.layout=PatternLayout
    log4perl.appender.SCREEN.layout.ConversionPattern=%d %p %F{1}:%L: %M %m%n
EOF_log_config
  );
}
my $log = get_logger();

# we rely on the "lfs" command
$log->logdie( "ERROR: can't find 'lfs' command as '" . LFS . "'" )
  unless -x LFS;

#---------------------------------------

# parse options and make sure they're sensible

my ( $dest_dir, @files, $help, $src_dir, $uncompressed, $verbose );

my $rv = GetOptions( 'destdir=s'    => \$dest_dir,
                     'files=s'      => \@files,
                     'help'         => \$help,
                     'srcdir=s'     => \$src_dir,
                     'uncompressed' => \$uncompressed,
                     'verbose'      => \$verbose );

usage() and exit if ( $help or
                      ( not defined $src_dir and not defined $dest_dir ) );

if ( $verbose ) {
  $log->level( $DEBUG );
  $log->debug( 'verbose logging enabled' );
}

$log->logdie( 'ERROR: no source directory specified' )                        unless $src_dir;
$log->logdie( "ERROR: '$src_dir' must be a full path (must start with '/')" ) unless $src_dir =~ m|^/|;
$log->logdie( "ERROR: '$src_dir' is not on a lustre filesystem" )             unless $src_dir =~ m/lustre/;
$log->logdie( "ERROR: no such source directory, '$src_dir'" )                 unless -d $src_dir;

$log->debug( "archiving files from '$src_dir'\n" );

$log->logdie( 'ERROR: must specify destination directory for archive' ) unless $dest_dir;
$log->logdie( "ERROR: no such destination directory, '$dest_dir'" )     unless -d $dest_dir;

unless ( @files ) {
 $log->debug( 'setting default list of file names' );
 @files = qw( DESC SEED );
}

#---------------------------------------

$log->debug( 'building archive name');

( my $prefix = $src_dir ) =~ s|/|_|g;
$prefix =~ s/^_//;

( my $now = DateTime->now ) =~ s/:/-/g;

my $archive = "$dest_dir/${prefix}_$now" . ( $uncompressed ? '.tar' : '.tgz' );

$log->logdie( "ERROR: archive '$archive' already exists" ) if -e $archive;

#---------------------------------------

chdir $src_dir;

my $tar = Archive::Tar->new;

foreach my $file ( @files ) {
  $log->info( "adding files named '$file'" );
  find_and_add( $tar, $file );
}

if ( $uncompressed ) {
  $log->info( "writing uncompressed archive '$archive'" );
  $tar->write( $archive, undef, $prefix );
}
else {
  $log->info( "writing compressed archive '$archive'" );
  $tar->write( $archive, COMPRESS_BZIP, $prefix );
}

exit;

#-------------------------------------------------------------------------------
#- subroutines -----------------------------------------------------------------
#-------------------------------------------------------------------------------

# walk down the lustre directory, find all files with the specified names and
# add them to the supplied archive
sub find_and_add {
  my ( $tar, $file ) = @_;

  open ( FIND, LFS  . " find ./ -name $file -print |" )
    or $log->logdie( "ERROR: couldn't run 'lfs find' command: $!" );

  while ( <FIND> ) {
    chomp;
    $log->debug( "adding '$_'" );
    $tar->add_files( $_ );
  }
}

#-------------------------------------------------------------------------------

sub usage {

  print STDERR <<EOF_help;

usage:  $0 --srcdir|-s <dir> --destdir|-d <dir> [--files|-f <filename>] [--compress|-c] [--verbose|-v] [--help|-h]

This script uses the "lfs find" command to search down a lustre directory
structure and add files with specified names to a tar archive.

required parameters: 
  --srcdir | -s <dir>      source directory from which to search downwards
  --destdir | -d <dir>     destination directory for archive
  
optional parameters:
  --files | -f <filename>  name of the file(s) to look for under "srcdir" and
                           archive. Repeat to add more. Defaults to "SEED" 
                           and "DESC"
  --uncompress | -u        don't compress the final tar archive. Default is to
                           compress
  --verbose | -v           enable verbose logging
  --help | -h              show this help message

The directory to search (srcdir) must be specified, as must the destination
directory (destdir) for the archive. Both directory paths must be absolute
(must start with "/"). 

The list of files to archive can be supplied by adding repeated "--files" (or
"-f") switches. The default file names are "SEED" and "DESC".

The final tar archive will be named according to the source directory, appended
with the date and time at which the script is run,

e.g. lustre_scratch_109_sanger_jt6_2012-05-31_19:00:33.tar

If there is already a tar file with the specified name in the destination
directory, the script will throw an error and stop.

The files in the tar archive will have a relative path and the directory
structure will be prefixed with the same pseudo-path as the tar archive itself,

e.g. lustre_scratch109_sanger_jt6/

If the "--uncompressed" switch is given, the tar archive will not be compressed
and given the suffix ".tar". Default is to compress and use the suffix ".tgz".

EOF_help
}

