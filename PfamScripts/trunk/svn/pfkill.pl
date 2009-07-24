#!/software/bin/perl

=head1 COPYRIGHT

File: pfkill.pl

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

=head1 pfkill.pl
  
  The purpose of this script is to remove a family form the database. There 
  are two steps to this approach.  The first is to remove the family from the 
  mysql database and add information to the dead families database table.  The
  second stage is to remove the entry from the subversion repository.

=cut

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;

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

#We expect the family accession to be passed on the command line
my $family = shift;
chomp($family);

unless ($family) {
  warn "\n***** No family passed  *****\n\n";
  help();
}

my $config = Bio::Pfam::Config->new;
#Check that the family looks like a pfam accession
unless($family =~ /PF\d{5}/){
  if($config->location eq 'WTSI'){
    my $connect = $config->pfamlive;
    my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
      %{ $connect }
    );
    my $pfamAcc = $pfamDB->id2acc($family);
    unless($pfamAcc =~ /PF\d{5}/){
      warn "You passed in something that did not look like an accession.\n"; 
      warn "Because you are at WTSI, tried to map it to an accession, but failed.\n";
      help();
    }
    $family = $pfamAcc;
  }else{
    print STDERR "\n ***** $family does not look like a family accession *****\n\n";
    help();
  }  
}

unless ( $family =~ /^PF\d{5}$/ ) {
  warn "\n***** $family does not look like an acccession *****\n\n";
  help();
}

if ( $forward and $forward !~ /^PF\d{5}/ ) {
  warn
"\n***** The family to forward to [ $forward ] does not look like an acccession *****\n\n";
  help();
}

#-------------------------------------------------------------------------------
# Now check that the family that we want to kill exisits
# Now check that the family we want to kill is part of the SVN repository

my $client = Bio::Pfam::SVN::Client->new;
$client->checkFamilyExists($family);
my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromSVN( $family, $client );

#-------------------------------------------------------------------------------
# This bit is based on the old pfkill. Get a comment as to why the family has been
# killed unless the comment was supplied as a commandline argument.
#
unless ($comment) {
  print "Please give a comment for killing family [$family]\n";
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
  die "Please give a reason/comment as to why you are killing this family\n";
}

#-------------------------------------------------------------------------------
# Try and get a forwarding accession

# Prompt for forwarding accession.
if ($forward) {

  #Now check that the family we want to kill is part of the SVN repository
  $client->checkFamilyExists($forward);
}

unless ( $nforward or $forward ) {
  print "Please give the pfam accession for the family to forward to\n";
  print "Finish with a . on the line by itself\n";

  while ( ( $_ = <STDIN> ) !~ /^\.$/ ) {
    chop;
    if ( $_ =~ /^(PF\d{5})/ ) {
      my $acc = $1;
      eval { $client->checkFamilyExists($acc); };
      if ($@) {
        warn "Acc $acc does not exist in the database";
        next;
      }
      else {
        $forward = $acc;
      }
    }
    else {
      warn "$_ does not look like a Pfam accession\n";
    }
  }
}

#-------------------------------------------------------------------------------
# So we should have enough information to kill off the family.  Write it to file
# such that the code reference that deals with the SVN log message can grab it.

if ( -s ".defaultpfkill" ) {
  unlink(".defaultpfkill")
    or die "Could not remove old default check-in message\n";
}

open( M, ">.defaultpfkill" ) or die "Could not open message file\n";
print M "Comment;" . $comment . "\n";
print M "PFKILL:Forward;" . $forward . "\n" if ($forward);
if ( $famObj->DESC->CL ) {
  print M "PFKILLRMC:"
    . $famObj->DESC->CL . ":"
    . $famObj->DESC->AC
    . "\n";
}
close(M);
$client->addPFKILLLog();

#-------------------------------------------------------------------------------
#If we get here, then great! We can now kill the family!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->killFamily($family);

#Remove any file containing the check-in message
if ( -s ".defaultpfkill" ) {
  unlink(".defaultpfkill")
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

usage $0 <family accession>

The script will then ask for a reason for killing the family a for a forwarding accession.
If you want to provide these reasons on the commandline use the following flags

-help           - Prints this help message.
-m <message>    - Message/comment as to why the family is being killed.
-f <accession>  - Forward this family to the accession.
-nf             - Sometimes there is no accession to forward to. Use this to suppress the request
                  for a forwarding comment.

EOF

  exit(1);

}
