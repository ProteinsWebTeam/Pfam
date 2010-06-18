#! /software/bin/perl -w


#
# Loops over all families in a directory 
# and issues a pfci
#

use strict;
use Getopt::Long;

my ($help, $message, $fam_dir, $log_dir);

GetOptions (     "h" => \$help,
		 "message=s" => \$message,
		 "fam_dir=s" => \$fam_dir, 
		 "log_dir=s" => \$log_dir);




&help if($help or !$message or !$fam_dir or !$log_dir);


#Check input parameters are sensible
unless(-d $fam_dir) {
    print STDERR "[$fam_dir] is not a valid directory location\n\n";    
    &help;
}
unless(-d $log_dir) {
    print STDERR "[$log_dir is not a valid directory location\n\n";
    &help;
}

my $ci_file = "$log_dir/checkedIn";
my $clan_file = "$log_dir/families_to_add_to_clans";

my %clan;
open(CLAN, $clan_file) or die "[$clan_file] does not exist\n";
while(<CLAN>) {
    if(/(\S+)/) {
	$clan{$1}=1;
    }
}
close CLAN;
 
my %done;


#Read in fams that have already been checked in (if any)
if(-s "$ci_file") {
    open(FILE, "$ci_file") or die "Couldn't open fh to $ci_file $!";
    while(<FILE>) {
	if(/^(\S+)/) {
	    $done{$1}=1;
	}
    }
    close FILE;

    #Open fh for printing later
    open(FILE, ">>$ci_file") or die "Couldn't open fh to $ci_file $!";
}
else {
    open(FILE, ">$ci_file") or die "Couldn't open fh to $ci_file $!";
}

#Record failed check ins
open(ERROR, ">$log_dir/failedCheckIn") or die "Couldn't open $log_dir/failedCheckIn $!";


opendir(DIR,$fam_dir);
my @families = grep { ! /^\./ } readdir(DIR);
closedir(DIR);

chdir("$fam_dir") or die "Couldn't chdir into $fam_dir $!";


#Loop through each family and check in
foreach my $family ( @families ) {
    next unless(-d $family);

    next if(exists($done{$family}));

    print STDERR "Checking in $family\n";


    #Need to add '-add_to_clan' flag to those that need it
    if(exists($clan{$family})) {
	if ( system("pfci -i -m '$message' -add_to_clan $family") == 0) {
	    print FILE "$family\n";
	}	
	else {
	    print ERROR "$family\n";
	}
    }
    else {
	if( system("pfci -i -m '$message' $family") == 0) {
	    print FILE "$family\n";
	}
	else {
	    print ERROR "$family\n";
	}
    }
}

close FILE;  
close ERROR;




sub help {
    print<<EOF;

Script to check in all families in a directory.  There should be a
file in the log_dir called 'families_to_add_to_clans' which contains a
list of family accessions for which the '-add_to_clan' flag needs to
added to the pfci command.  The script records sucessfully checked in
families in a file named 'log_dir/checkedIn' and failed check ins in a
file named 'log_dir/failedCheckIn'.

Usage:

$0 -fam_dir <fam_dir> -log_dir <log_dir> -message "message text"

Options:
-h : Display this help message


EOF

exit(0);
}

