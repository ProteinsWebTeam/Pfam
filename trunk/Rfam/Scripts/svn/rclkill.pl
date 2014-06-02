#!/usr/bin/env perl


=head1 rclkill.pl
  
  The purpose of this script is to remove a clan form the database. There 
  are two steps to this approach.  The first is to remove the entry from the 
  mysql database and add information to the dead clans database table.  The
  second stage is to remove the entry from the subversion repository.  Prior
  to th running of this script, all families should have been removed from
  the clan.

=cut

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Rfam::Config;
use Bio::Rfam::ClanIO;
use Bio::Rfam::SVN::Client;

my $config = Bio::Rfam::Config->new;

#------------------------------------------------------
# User options

my ( $comment, $forward, $nforward, $help );

&GetOptions(
  "m=s"  => \$comment,
  "f=s"  => \$forward,
  "help" => \$help,
  "nf"   => \$nforward
);

help() if ($help);

#We expect the clan accession to be passed on the command line
my $entry = shift;
chomp($entry);

unless ($entry) {
  warn "\n***** No entry passed  *****\n\n";
  help();
}

if ( $entry !~ /^CL\d{5}$/ ) {
  warn "Looks like $entry is an identifier, rather than an accession.\n"; 
  
  #Try and map across.
  my $oriEntry = $entry;
 if($config->location eq 'EBI'){
    my $rfamdb = $config->rfamlive;
    $entry = $rfamdb->resultset('Clan')->id2acc($entry);
  }else{
    $entry = undef;
  }
  unless($entry){
    die "Could not find anything that looked like $oriEntry, please try with an accession.\n";    
  }
}

if ( $forward and $forward !~ /^CL\d{5}$/ ) {
  warn
"\n***** The entry to forward to [ $forward ] does not look like a Rfam clan acccession *****\n\n";
  help();
}

#-------------------------------------------------------------------------------
# Now check that the entry that we want to kill exisits
# Now check that the entry we want to kill is part of the SVN repository

my $client = Bio::Rfam::SVN::Client->new;
$client->checkClanExists($entry);

#Now load the remote family
my $clanIO = Bio::Rfam::ClanIO->new;
my $oldClanObj = $clanIO->loadClanFromSVN( $entry, $client );
print STDERR "Successfully loaded SVN copy of $entry through middleware\n";
if(defined($oldClanObj->DESC->MEMB)){
  print "Please remove all families from a clan before killing!\n";
  exit;  
}

#-------------------------------------------------------------------------------
# This bit is based on the old pfkill. Get a comment as to why the entry has been
# killed unless the comment was supplied as a commandline argument.
#
unless ($comment) {
  print "Please give a comment for killing entry [$entry]\n";
  print "Finish comment by a . on the line by itself\n";
  my @comment;
  while (<STDIN>) {
    chomp;
    /^\s*\.\s*$/ && last;
    push( @comment, $_ );
  }
  $comment = join( " ", @comment );
}

unless ( $comment =~ /\S+/ ) {
  die "Please give a reason/comment as to why you are killing this entry\n";
}

#-------------------------------------------------------------------------------
# Try and get a forwarding accession

# Prompt for forwarding accession.
if ($forward) {

  #Now check that the entry we want to kill is part of the SVN repository
  $client->checkClanExists($forward);
}

unless ( $nforward or $forward ) {
  print "Please give the Rfam accession for the entry to forward to\n";
  print "Finish with a . on the line by itself\n";

  while ( ( $_ = <STDIN> ) !~ /^\.$/ ) {
    chop;
    if ( $_ =~ /^(CL\d{5}$)/ ) {
      my $acc = $1;
      eval { $client->checkClanExists($acc); };
      if ($@) {
        warn "Acc $acc does not exist in the database";
        next;
      }
      else {
        $forward = $acc;
      }
    }
    else {
      warn "$_ does not look like a Rfam clan accession\n";
    }
  }
}

#-------------------------------------------------------------------------------
# So we should have enough information to kill off the entry.  Write it to file
# such that the code reference that deals with the SVN log message can grab it.

if ( -s ".default".$$."rclkill" ) {
  unlink(".default".$$."rclkill")
    or die "Could not remove old default check-in message\n";
}

open( M, ">.default".$$."rclkill" ) or die "Could not open message file\n";
print M "CLKILL:Comment;" . $comment . "\n";
print M "CLKILL:Forward;" . $forward . "\n" if ($forward);
close(M);
$client->addRCLKILLLog();
#-------------------------------------------------------------------------------
#If we get here, then great! We can now kill the entry!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->killClan($entry);

#Remove any file containing the check-in message
if ( -s ".default".$$."rclkill" ) {
  unlink(".default".$$."rclkill")
    or die "Could not remove old default check-in message\n";
}

#
if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}

exit(0);

#-------------------------------------------------------------------------------

sub help {

  print <<EOF;

usage $0 <clan accession>

The script will then ask for a reason for killing the entry, and a forwarding accession if appropriate.
If you want to provide these reasons on the commandline use the following flags

-help           - Prints this help message.
-m <message>    - Message/comment as to why the entry is being killed.
-f <accession>  - Forward this entry to the accession.
-nf             - Sometimes there is no accession to forward to. Use this to suppress the request
                  for a forwarding comment.

EOF

  exit(1);

}
