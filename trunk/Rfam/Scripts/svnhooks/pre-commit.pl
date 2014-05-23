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

use Log::Log4perl qw(get_logger);

# set up logging
my $logger_conf = q(
  log4perl.logger                         = DEBUG, FileAppender
  log4perl.appender.FileAppender          = Log::Log4perl::Appender::File
  log4perl.appender.FileAppender.filename = /tmp/pre-commit.log
  log4perl.appender.FileAppender.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.FileAppender.layout.ConversionPattern = %d %F{1}: %M %L> %m %n
);

Log::Log4perl->init( \$logger_conf );

my $logger = get_logger();

my $DEBUG = defined( $ENV{DEBUG} ) ? $ENV{DEBUG} : 0;

my ( $rev, $txn, $repos, $debug, $help );

GetOptions(
  "rev=s"   => \$rev,
  "txn=s"   => \$txn,
  "repos=s" => \$repos
);

$logger->debug('successfully processed options');

if ( $rev and $txn ) {
  $logger->logdie( "ERROR: you cannot specify both a revision and a transaction" );
}

my %params;
if ($rev) {
  $params{rev} = $rev;
}
else {
  $params{txn} = $txn;
}

$params{repos} = $repos;

$logger->debug("got repository location ($repos) and "
            . ( defined $rev ? "revision number ($rev) " : "" )
            . ( defined $txn ? "transaction ($txn) " : "" )
            . "from command-line options");

my $txnlook = Bio::Rfam::SVN::Commit->new( \%params );

unless ( $txnlook and $txnlook->isa('SVN::Look') ) {
  $logger->logdie( "ERROR: failed to get a SVN::Look object for txn:$txn and repos:$repos" );
}

my $msg = $txnlook->log_msg();
unless ($msg) {
  $logger->logdie( "ERROR: no commit message passed in!" );
}

if ( $logger->is_debug ) {
  my $log_message = $msg;
  chomp $log_message;
  $logger->debug( qq(got log message from transaction: "$log_message") );
}

my $config  = Bio::Rfam::Config->new;
$logger->debug('got config object');

my $rfamdb = $config->rfamlive;
$logger->debug('got database connection');

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
$logger->debug('printed modified file list');

#Make sure the database isn't locked
my $lock = $txnlook->allowCommit($rfamdb);
$logger->debug('retrieved lock status');
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
  $logger->logdie( "ERROR: the database is currently locked by " . $lock->locker );
}

$logger->debug('database is not locked (or committer has permission to commit anyway)');

if ( $msg =~ /^CI:/ ) {
  $logger->debug('parsing msg; got a commit');
  $txnlook->commitEntry;
  $logger->debug('committed');
}
elsif ( $msg =~ /^NEW:/ ) {
  $logger->debug('parsing msg; got a new entry');
  $txnlook->commitNewEntry;
  $logger->debug('committed');
}elsif( $msg =~ /CIDESC:/){
  $logger->debug('parsing msg; got a modified DESC');
  $txnlook->commitEntryDESC;
  $logger->debug('committed');
}elsif ( $msg =~ /^NEWMOV:/ ) {
  $logger->debug('parsing msg; got a NEWMOV message');
  ;
}elsif ( $msg =~ /^MOV:/ ) {
  $logger->debug('parsing msg; got a move');
  $txnlook->moveFamily();
  $logger->debug('committed');
}
elsif ( $msg =~ /^KILL:/ ) {
  $logger->debug('parsing msg; got a kill');
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
    $logger->logdie( "ERROR: In KILL message, did not parse $msg" );
  }
  $logger->debug( qq(kill comment: "$comment") );
  $logger->debug( qq(forward to:   "$forward") );

  #Find out the author so we know who has done this.
  my $author = $txnlook->author();
  $logger->debug( qq(kill author:  "$author") );
  #Go and delete the family.
  $txnlook->deleteFamily( $comment, $forward, $author );
  $logger->debug('killed family');
}
elsif ( $msg =~ /SEQUP/ ) {
  $logger->debug('parsing msg; got a sequence update');
  foreach my $file ( $txnlook->updated() ) {
    $logger->debug("changed file: $file");
    unless ( $file =~ /Sequences/ ) {
      $logger->debug('dying; not a valid sequence update file');
      $logger->logdie( "ERROR: got sequence update message, but the file ($file) does not look"
        . " like it has come from the sequence part of the repository" );
    }
  }
}elsif ( $msg =~ /^CLCI:/ ) {
  $txnlook->commitClan;
}elsif( $msg =~ /ADMINBYPASS/ ) {
  $logger->debug('parsing msg; admin bypass');
}
else {
  $logger->debug( qq(parsing msg; unrecognised commit message: "$msg") );
  $logger->logdie( qq(ERROR: do not know here this commit has come from: "$msg") );
}

exit(0);

