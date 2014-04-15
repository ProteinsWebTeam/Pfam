#!/usr/bin/perl
#
#We have to set this in thes scripts as they do not assume anyones environment variables.
#
# pre-commit.pl -txn 1388 -repos /Users/finnr/Work/Repository

use strict;
use warnings;

use SVN::Look;
use Getopt::Long;
use Bio::Rfam::Config;
use Bio::Rfam::SVN::Commit;
use Data::Printer;

use Log::Log4perl qw(:easy);
 
 # set up logging
my $logger_conf = q(
  log4perl.logger                         = DEBUG, FileAppender
  log4perl.appender.FileAppender          = Log::Log4perl::Appender::File
  log4perl.appender.FileAppender.filename = /tmp/pre-commit.log
  log4perl.appender.FileAppender.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.FileAppender.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );

my $log = get_logger(); 

my $DEBUG = defined( $ENV{DEBUG} ) ? $ENV{DEBUG} : 0;

my ( $rev, $txn, $repos, $debug, $help );

GetOptions(
  "rev=s"   => \$rev,
  "txn=s"   => \$txn,
  "repos=s" => \$repos
);

$log->debug('successfully processed options');

if ( $rev and $txn ) {
  die "Can not define both and revision and a transaction\n";
}

my %params;
if ($rev) {
  $params{rev} = $rev;
}
else {
  $params{txn} = $txn;
}

$params{repos} = $repos;

$log->debug("got revision number ($rev) and transaction ($txn) from command-line options");

my $txnlook = Bio::Rfam::SVN::Commit->new( \%params );

unless ( $txnlook and $txnlook->isa('SVN::Look') ) {
  die "Failed to get a SVN::Look object for txn:$txn and repos:$repos\n";
}

my $msg = $txnlook->log_msg();
unless ($msg) {
  die "No commit message passed in!\n";
}

$log->debug( qw(got log message from transaction: "$msg") );

my $config  = Bio::Rfam::Config->new;
$log->debug('got config object');

my $rfamdb = $config->rfamlive;
$log->debug('got database connection');

if ($DEBUG) {
  print STDERR "*** $msg ***\n";

  #Now see what has changed
  my @added_files   = $txnlook->added();
  my @updated_files = $txnlook->updated();
  my @deleted_files = $txnlook->deleted();
  my @changed_files = $txnlook->changed();
  foreach my $f (@updated_files) {
    print STDERR "Updated:" . $f . "\n";
  }
  foreach my $f (@added_files) {
    print STDERR "Added:" . $f . "\n";
  }
  foreach my $f (@changed_files) {
    print STDERR "Changed:" . $f . "\n";
  }
  foreach my $f (@deleted_files) {
    print STDERR "Deleted:" . $f . "\n";
  }
}
$log->debug('printed modified file list');

#Make sure the database isn't locked
my $lock = $txnlook->allowCommit($rfamdb);
$log->debug('retrieved lock status');
my $allow_commit;
if ($lock) {

  #If it is locked, check to see if user is allowed to make commits
  if ( ( $lock->locker eq $txnlook->author ) and $lock->allowcommits ) {
    $allow_commit = 1;
  }
  elsif ( $lock->alsoallow ) {
    my @allow = split( /\s+/, $lock->alsoallow );

    foreach my $user (@allow) {
      if ( $user eq $txnlook->author ) {
        $allow_commit = 1;
        last;
      }
    }
  }
}
else {
  $allow_commit = 1;
}
unless ($allow_commit) {
  die "The database is currently locked by " . $lock->locker . "\n";
}

$log->debug('database is not locked (or committer has permission to commit anyway)');

if ( $msg =~ /^CI:/ ) {
  $log->debug('got a commit');
  $txnlook->commitEntry;
  $log->debug('committed');
}
elsif ( $msg =~ /^NEW:/ ) {
  $log->debug('got a new entry');
  $txnlook->commitNewEntry;
  $log->debug('committed');
}elsif( $msg =~ /CIDESC:/){
  $log->debug('got a modified DESC');
  $txnlook->commitEntryDESC;   
  $log->debug('committed');
}elsif ( $msg =~ /^NEWMOV:/ ) {
  $log->debug('got a NEWMOV message');
  ;
}elsif ( $msg =~ /^MOV:/ ) {
  $log->debug('got a move');
  $txnlook->moveFamily();
  $log->debug('committed');
}
elsif ( $msg =~ /^KILL:/ ) {
  $log->debug('got a kill');
  my ( $comment, $forward );
  if ( $msg =~ /^KILL:Comment;(.*)\nKILL:Forward;(.*)/ ) {
    $comment = $1;
    $forward = $2;
  }
  elsif ( $msg =~ /KILL:Comment;(.*)/ ) {
    $comment = $1;
    $forward = '';
  }
  else {
    die "In KILL message, did not parse $msg\n";
  }
  $log->debug( qw(kill comment: "$comment") );
  $log->debug( qw(forward to:   "$forward") );

  #Find out the author so we know who has done this.
  my $author = $txnlook->author();
  $log->debug( qw(kill author:  "$author") );
  #Go and delete the family.
  $txnlook->deleteFamily( $comment, $forward, $author );
  $log->debug('killed family');
}
elsif ( $msg =~ /SEQUP/ ) {
  $log->debug('got a sequence update');
  foreach my $file ( $txnlook->updated() ) {
    $log->debug("changed file: $file");
    unless ( $file =~ /Sequences/ ) {
      $log->debug('dying; not a valid sequence update file');
      die "Got sequence update message, but the file ($file) does not look"
        . " like it has come from the sequence part of the repository\n";
    }
  }
}elsif( $msg =~ /ADMINBYPASS/ ) {
  $log->debug('admin bypass');
  ; 
}
else {
  $log->debug( qw(unknown commit message: "$msg") );
  die "Do not know here this commit has come from, [$msg]!\n";
}

exit(0);

