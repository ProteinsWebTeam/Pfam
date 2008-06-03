#!/usr/bin/perl -w
# cvslog -- Mail a CVS log message to a given address.
#
# Written by Russ Allbery <rra@stanford.edu>
# Copyright 1998 Board of Trustees, Leland Stanford Jr. University
#
# Modified by James Cuff <james@sanger.ac.uk> to work with Ensembl
# cvs repository Thu Dec 20 11:00:35 GMT 2001, and to work with 
# Mail::Send
#
# Modified again by Tim Cutts
#
# This program is designed to run from CVS's loginfo administrative file and
# takes a log message, massaging it and mailing it to the address given on
# the command line.  It's a modified version of the log script that comes
# with CVS, but tries to do less (it doesn't do cvs status).
#
# It should be run from loginfo with something like:
#
#       ^leland         $CVSROOT/CVSROOT/cvslog %s
#       ^leland/dbmodel $CVSROOT/CVSROOT/cvslog -a ls-dbmodel@lists %s
#
# Note that it mails everything to the address configured at the top of this
# file in addition to any other addresses given on the command line with -a.

# Mail all reports to this address.


use POSIX qw(strftime);
use IO::File;
use Getopt::Long;

use strict;

# Grab the address to mail this report to.

my ( @addresses, $message );

GetOptions( 'a=s', \@addresses,
            's=s', \$message );

# The next arguments are from %s; first the relative path in the repository
# and then the list of files modified.

my $files_tmp = $ARGV[0];
my @files=split(" ",$files_tmp);
my $module = $files[0] or die "$0: no module specified\n";

unless (@files) { die "$0: no files specified\n" }

# Figure out who is doing the update.
my $user = (getpwuid $<)[0] || $<;

my $numfiles=$#files;
my $date = strftime ('%A, %B %e, %Y @ %T', localtime time);
my $msg ="";

$message ||= 'CVS commit';
$message .= " : Module $module <";
$message .= ($numfiles == 1) ? '1 file edited>' : "$numfiles files edited>";

# see what addresses we have
my $addresses = join ' ', @addresses;
die "$0: no email addresses supplied\n" if $addresses =~ m/^$/;

# open up a mail object
my $fh = new IO::File;
$fh->open("|/usr/bin/mailx -s \"$message\" $addresses") or
    die "$0: Could not open pipe to /usr/bin/mailx : $!\n";

$_ = <STDIN>;

s/\n//g;
s/Update of //;

$fh->print("CVS: [$date], user: $user edited $_, from ");

# Now comes the path to the local working directory.  Grab it and clean it
# up, and then ignore the next blank line.

$_=<STDIN>;
my $tmp="";
my $local="";
s/In directory //;
($local,$tmp) =split (":",$_); 

$fh->print("$local.\n");
<STDIN>;

while (<STDIN>) {
  last if /^Log Message/;
  if (/Modified Files:\s*/){
      $fh->print("\n$user modified the following files:\n");
      s/Modified Files:\n//;
  }
  $fh->print($_);
}

$fh->print("\nComment:\n\n");

while (<STDIN>){
    $fh->print($_);
}

$fh->close;

# all done
