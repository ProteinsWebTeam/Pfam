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

my $DEBUG = defined( $ENV{DEBUG} ) ? $ENV{DEBUG} : 0;

my ( $rev, $txn, $repos, $debug, $help );

GetOptions(
  "rev=s"   => \$rev,
  "txn=s"   => \$txn,
  "repos=s" => \$repos
);

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

my $txnlook = Bio::Rfam::SVN::Commit->new( \%params );

unless ( $txnlook and $txnlook->isa('SVN::Look') ) {
  die "Failed to get a SVN::Look object for txn:$txn and repos:$repos\n";
}

my $msg = $txnlook->log_msg();
unless ($msg) {
  die "No commit message passed in!\n";
}

my $config  = Bio::Rfam::Config->new;
my $rfamdb = $config->rfamlive;

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

#Make sure the database isn't locked
my $lock = $txnlook->allowCommit($rfamdb);
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

if ( $msg =~ /^CI:/ ) {
  $txnlook->commitEntry;
}
elsif ( $msg =~ /^NEW:/ ) {
  $txnlook->commitNewEntry;
}elsif( $msg =~ /CIDESC:/){
  $txnlook->commitEntryDESC;   
}elsif ( $msg =~ /^NEWMOV:/ ) {
  ;
}elsif ( $msg =~ /^MOV:/ ) {
  $txnlook->moveFamily();
}
elsif ( $msg =~ /^KILL:/ ) {
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

  #Find out the author so we know who has done this.
  my $author = $txnlook->author();
  #Go and delete the family.
  $txnlook->deleteFamily( $comment, $forward, $author );
}
elsif ( $msg =~ /SEQUP/ ) {
  foreach my $file ( $txnlook->updated() ) {
    unless ( $file =~ /Sequences/ ) {
      die "Got sequence update message, but the file ($file) does not look"
        . " like it has come from the sequence part of the repository\n";
    }
  }
}elsif( $msg =~ /ADMINBYPASS/ ) {
  ; 
}
else {
  die "Do not know here this commit has come from, [$msg]!\n";
}

exit(0);

