#!/usr/local/bin/perl
#
# This script allows one to check in a family to the SVN repository containing Pfam families.
# The family that you wish to checkout should be passed in as a parameter
#

use strict;
use warnings;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::FamilyIO;

use Cwd;

my $family = shift;
chomp($fammily);

if( !(-d $family) ) {
    die "$0: [$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if( !-w $family ) {
    die "$0: I can't write to directory [$family].  Check the permissions.\n";
}

if(!(-d "$family/.svn") ){
  die "$0: [$family] does not look like that it is from the Pfam subversion repository";
}

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
my $familyIO = Bio::Pfam::FamilyIO->new;
$client->checkFamilyExists($family);
my $oldFamily = $familyIO->loadPfamAFromSVN($family, $client);


#Load family up into objects
my $family = $familyIO->loadPfamAFromLocalfile($family, $pwd);

#Check that the accession xrefs with svn
if($family->desc->accession ne $oldfamily->desc->accession){
  die; 
}
if($family->desc->id ne $oldfamily->desc->id){
  die; 
}


if(! $opt_i && &Bio::Pfam::PfamQC::sequence_checker($family)){
	print "pfci: $family contains errors.  You should rebuild this family.\n";
    exit(1);	
}

## missing in full
if(!$opt_i){
	system ("pqc-check $family");
}

if(! $opt_i and -s "$family/missing" and !$ignore_missing_test) {
    die "pfci: Your family appears to be missing sequences, see $family/missing for details\n";
}

## Find potential fragments in the seed
if(! $opt_i and ! -e "$family/seedokay" ) {
    die "pfci: Your family seed has not passed the quality checks, please fix!!\n";
}

if(! $opt_i && &Bio::Pfam::PfamQC::doggy_sequence_in_seed($family) ) {
    die "pfci: $family SEED appears to contain a partial match or fragment in the seed.\n";
}

## ragged seed check
if(! $opt_i and ! -e "$family/seedNotRagged" ) {
    die "pfci: Your family seed has not passed the quality checks, please fix!!\n";
}

if(!$opt_i && &Bio::Pfam::PfamQC::ragged_seed($family) ) {
    die "pfci: $family appears to have a ragged SEED.  This is good, please fix\n";
}

#Skip overlap

#If we get here, then great! We can now check the family in!

my $caught_cntrl_c; 
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

my $comment;
if( !defined $message ) {
    print "Please give a comment for family [$family]\n";
    print "Finish comment by a . on the line by itself\n"; 

    while( <STDIN> ) {
	chop;
	/^\s*\.\s*$/ && last;
	$comment .= "$_\n";
    }
} else {
    $comment = $message;
}


if( $caught_cntrl_c ) {
    print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}


