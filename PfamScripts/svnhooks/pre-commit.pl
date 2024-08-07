#!/usr/bin/perl
#
#We have to set this in thes scripts as they do not assume anyones environment variables.
#
# pre-commit.pl -txn 1388 -repos /Users/finnr/Work/Repository

use strict;
use warnings;

use SVN::Look;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::SVN::Commit;
use Data::Dumper;

my $DEBUG = defined( $ENV{DEBUG} ) ? $ENV{DEBUG} : 1;

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

my $txnlook = Bio::Pfam::SVN::Commit->new( \%params );

unless ( $txnlook and $txnlook->isa('SVN::Look') ) {
  die "Failed to get a SVN::Look object for txn:$txn and repos:$repos\n";
}

my $msg = $txnlook->log_msg();
unless ($msg) {
  die "No commit message passed in!\n";
}

my $config  = Bio::Pfam::Config->new;
my $connect = $config->pfamlive;

if ($DEBUG) {
  print STDERR Dumper($connect);
}
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );

if ($DEBUG) {
  print STDERR "*** $msg ***\n";

  #$msg = "$msg";

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
my $lock = $txnlook->allowCommit($pfamDB);
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

if ( $msg =~ /^PFCI:/ ) {
  $txnlook->commitFamily($pfamDB, $msg);
}
elsif ( $msg =~ /PFCIATC:(CL\d{4})\:(PF\d{5})/ ) {
  my ( $clan, $fam );
  $clan = $1;
  $fam  = $2;

  #Add the clan data to the database
  $txnlook->updateClanMembership( $pfamDB, $clan, $fam );

  #Then commit the family
  $txnlook->commitFamily($pfamDB);
}

elsif ( $msg =~ /PFCIRMC:(CL\d{4})\:(PF\d{5})/ ) {
  my ( $clan, $fam );
  $clan = $1;
  $fam  = $2;

  #Remove the clan data to the database
  $txnlook->removeFamilyFromClanMembership( $pfamDB, $clan, $fam );

  #Then commit the family
  $txnlook->commitFamily($pfamDB);
}
elsif ( $msg =~ /PFCICHC:(CL\d{4})\|(CL\d{4})\:(PF\d{5})/ ) {
  my ( $clan_old, $clan_new, $fam );
  $clan_old = $1;
  $clan_new = $2;
  $fam  = $3;

  #Remove the clan data from the database
  $txnlook->removeFamilyFromClanMembership( $pfamDB, $clan_old, $fam );

  #Add the clan data to the database
  $txnlook->updateClanMembership( $pfamDB, $clan_new, $fam );

  #Then commit the family
  $txnlook->commitFamily($pfamDB);
}
elsif ( $msg =~ /^PFNEW:/ ) {
  $txnlook->commitNewFamily($pfamDB);
}elsif( $msg =~ /PFCIDESC:/){
  $txnlook->commitFamily($pfamDB);   
}elsif ( $msg =~ /^CLCI:/ ) {
  $txnlook->commitClan($pfamDB);
}
elsif ( $msg =~ /^CLNEW:/ ) {
  $txnlook->commitNewClan($pfamDB);
}
elsif ( $msg =~ /^CLMOV:/ ) {
  $txnlook->moveClan($pfamDB);
}
elsif ( $msg =~ /^(PF|CL)NEWMOV:/ ) {
  ;
}
elsif ( $msg =~ /^CLNEWACC:/ ) {
  ;
}
elsif ( $msg =~ /^AUTOMB/ ) {
  ;
}
elsif ( $msg =~ /^AUTORMMB/ ) {
  ;
}
elsif ( $msg =~ /^AUTORMCL:/ ) {
  $txnlook->initiateFamilyView($pfamDB);
}
elsif ( $msg =~ /^PFMOV:/ ) {
  $txnlook->moveFamily($pfamDB);
}
elsif ( $msg =~ /^PFKILL:/ ) {
  my ( $comment, $forward );
  if ( $msg =~ /PFKILL:Comment;(.*)\nPFKILL:Forward;(.*)/ ) {
    $comment = $1;
    $forward = $2;
  }
  elsif ( $msg =~ /PFKILL:Comment;(.*)/ ) {
    $comment = $1;
    $forward = '';
  }
  else {
    die "In PFKILL message, did not parse $msg\n";
  }
  if ( $msg =~ /PFKILLRMC:(CL\d{4})\:(PF\d{5})/ ) {
    my ( $clan, $fam );
    $clan = $1;
    $fam  = $2;

    #Remove the family from the clan membership in the database
    $txnlook->removeFamilyFromClanMembership( $pfamDB, $clan, $fam );
  }
  my $author = $txnlook->author();
  $txnlook->deleteFamily( $pfamDB, $comment, $forward, $author );
}
elsif ( $msg =~ /^CLKILL:/ ) {
  my ( $comment, $forward );
  if ( $msg =~ /CLKILL:Comment;(.*)CLKILL:Forward;(.*)/ ) {
    $comment = $1;
    $forward = $2;
  }
  elsif ( $msg =~ /CLKILL:Comment;(.*)/ ) {
    $comment = $1;
    $forward = '';
  }
  else {
    die "In CLKILL message, did not parse $msg\n";
  }

  #Remove the family from the clan membership in the database
  $txnlook->deleteClan( $pfamDB, $comment, $forward );
}
elsif ( $msg =~ /SEQUP/ ) {
  foreach my $file ( $txnlook->updated() ) {
    unless ( $file =~ /Sequences/ ) {
      die "Got sequence update message, but the file ($file) does not look"
        . " like it has come from the sequence part of the repository\n";
    }
  }
}elsif($msg =~ /CLMERGE/ or $msg =~ /CLADD/ or $msg =~ /CLREMOVE/) {
 #let it go through

}elsif( $msg =~ /ADMINBYPASS/){
# let it go through.

}else{
  my @changed = $txnlook->changed();
  unless ( $changed[0] =~ m|Data/Dictionary/dictionary$|){
    die "Do not know where this commit has come from, [$msg]!\n";
  }
}

exit(0);

