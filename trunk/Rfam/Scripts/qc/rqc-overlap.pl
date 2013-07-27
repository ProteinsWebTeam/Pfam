#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use File::Touch;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;

#Get the inital set up
my $config   = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;
use Getopt::Long;

my( $quiet,
    $nolog,
    $local_fams,
    @ignore);


&GetOptions("i=s@" => \@ignore,
            "q"    => \$quiet,
            "l"    => \$local_fams,
            "n!"   => \$nolog);
            
if($#ARGV == -1) {
  help();
}
my $family = shift;
my $pwd = getcwd;

#Check that $fam corresponds to a directory.
if(!-d $family){
  help();
}

if(-e "$family/overlap"){
  unlink("$family/overlap");
}

#First check timestamps, because if they are wrong it is not worth doing anything
my $error = 0;

my $familyObj;
if(!$error){
  eval{
    $familyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );
  };
  if($@){
    warn "There was an issues loading the entry from file: $@\n";
    exit(1);
  }
}
my %ignore = map{$_ => 1}@ignore;
$ignore{ $familyObj->DESC->AC }++ if($familyObj->DESC->AC);
$error = Bio::Rfam::QC::overlap($familyObj, $config, \%ignore);

sub help {
  print<<EOF;

$0 - finds the overlap between a Rfam family and the current Rfamlive database\n";

USAGE: $0 <model-directory> <optional - families to compare against>;

If no families given, assummes all

OPTIONS
 -i <family> ignore this family (-i can occur multiple times)
 -q quiet running
 -n  no log file
 -l  look in current directory for target families
  
EOF
  
exit;
}


