#!/usr/local/bin/perl -w

#
# author sgj
# heavily borrowed from Ewan's pfam code
#

use lib '/nfs/disk56/sgj/rfam/scripts/Modules';

use strict;
use Getopt::Std;
use Rfam;
use RfamRCS;

use vars qw($opt_m);

&getopt("m:");
my $message = $opt_m;

if( $#ARGV == -1 ) {
    &RfamRCS::show_rcs_help(\*STDOUT);
    exit;
}

## first, check that the database is not locked

my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
if( $locked ) {
    my $allowed;
    foreach my $allow ( @{$allow_ref} ) {
	$allowed = 1 if( $allow eq "rfci" );
    }
    unless( $allowed ) {
	die "rfci aborted: database is locked by $locker";
    } 
}

my $acc = shift;
    
if( $acc =~ /\/$/ ) {
    $acc =~ s/\/$//g;
}

if( !(-d $acc) ) {
    die "rfci: [$acc] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if( ! &RfamRCS::check_family_directory_exists($acc) ) {
    die "rfci: Family [$acc] does not have a directory in RCS_MASTER.\nIf this is a new family, check in a new family with pfnew\n";
}

if( ! &RfamRCS::check_family_exists($acc) ) {
    die "rfci: Bad error Family [$acc] has RCS_MASTER directory, but not the correct files internally.\nPlease contact pfam\@sanger.ac.uk to resolve the error\n";
}

my $name = `whoami`;
chop $name;

my ($haslocked,$peoplelocked) = &RfamRCS::user_has_locked_family($acc,$name);

if( $haslocked == 0 ) {
    die "rfci: This family [$acc] was not locked by you[$name], it was locked by [$peoplelocked]\n";
}

if( ! open(DESC,"./$acc/DESC") ) {
    die "rfci: A bad error - cannot open the desc file [./$acc/DESC]. Yikes! [$!]";
}

my $has_ac;

while(<DESC>) {
    if(/^AC/) {
	$has_ac=1;
    }
}    
close(DESC);

if(!$has_ac) {
    die "rfci: Your DESC file has no AC line. This is bad!\n";
}

if( &RfamRCS::move_files_to_rcs_directory($acc, $acc) == 0 ) {
    die "rfci: Could not move RCS files to directory for family [$acc]. Problem!\n";
}

my $comment;
if( !defined $message ) {
    print "Please give a comment for family [$acc]\n";
    print "Finish comment by a . on the line by itself\n"; 

    while( <STDIN> ) {
	chop;
	/^\s*\.\s*$/ && last;
	$comment .= "$_\n";
    }
} else {
    $comment = $message;
}

if( &RfamRCS::check_in_rcs_files($acc, $comment) == 0 ) {
    die "rfci: Could not check in files for $acc. This is a bad error\n";
}

if( &RfamRCS::update_current_directory($acc) == 0 ) {
    die "rfci: Could not update directory for $acc\n";
}
  
# rdb stuff

#&RfamRCS::make_view_files($acc); 

print STDERR "\n\nChecked in family [$acc]\n";
