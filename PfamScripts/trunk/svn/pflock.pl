#! /software/bin/perl -w

use strict;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Data::Dumper;


my ($help, $lock, $unlock, @user_list, $commit, @commit_list, $commit_list, $force);
&GetOptions (     "h" => \$help,
		  "l" => \$lock,
		  "u" => \$unlock, 
   	      "force" => \$force,
           "allow_me" => \$commit,
       "allow_user=s" => \@commit_list);
        
unless($lock or $unlock) {
    help();
}

if($unlock and ($commit or @commit_list or $force or $lock)) {
    die "You have specified options which cannot be used together\n";
}


open(FH, "whoami |") or die "Couldn't open filehandle $!\n";
my $user;
while(<FH>) {
    $user = $1 if(/(\S+)/) ;
}



if(@commit_list) {
    @commit_list = split(/,/,join(',',@commit_list));  #Allow comma-separated lists of values and multiple occurrences of the options
    $commit_list = join(" ", @commit_list);
} else {
    $commit_list = "";
}
$commit = 0 unless($commit);





my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

my @lock_data = $pfamDB->getSchema
    ->resultset('Lock')->search({ 'locked' => 1});

my $lock_data = shift @lock_data; #Should only ever be one row

if($lock_data) {   
    if($lock) {
	die "Database is already locked by " . $lock_data->locker."\n";
    }
    elsif($unlock) {
        if($user ne $lock_data->locker and !$force) {
            die "The database is locked by ".$lock_data->locker.", if you [$user] want to unlock it, you need to use the -force option\n";
	}
        else {
            $lock_data->delete;
	    print STDERR "Database unlocked\n";
	}
    }
}
else {
    if($lock) {
        $pfamDB->getSchema
        ->resultset('Lock')
        ->update_or_create( { locked => 1, locker => $user, allowcommits => $commit,  alsoallow  => $commit_list });
	print STDERR "Database locked\n";
    }
    elsif($unlock) {
	die "Database is already unlocked\n";
    }
}





sub help {
print STDERR << "EOF";

This script can be used to 'lock' and 'unlock' the database.  When the
database is in the 'locked' state, users will be unable to use pfam
scripts to commit data, unless the allow_me or allow_user flags are used.


EXAMPLE:

  $0 -l (locks the database)
  $0 -u (unlocks the database)

OPTIONS:
   -allow_me              : Allows the user running the script to perform commits
   -allow_user  <user_id> : Allow user_id to perform commits (can specify multiple users)

    e.g $0 -l -allow_me -allow_user "rdf,pcc"

EOF

exit (0);
}

