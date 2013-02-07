#!/usr/bin/env perl

=head1 NAME

  rflock - script to perform high level locking of the database.

=head1 DESCRIPTION

This script can be used to 'lock' and 'unlock' the database.  When the
database is in the 'locked' state, users will not be unable to use Rfam
scripts to commit data, unless the allow_me or allow_user flags are used.

Run with -h for more options

=head1 COPYRIGHT

File: rflock.pl 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Feb 7, 2013 3:18:36 PM

=cut

use strict;
use warnings;
use Getopt::Long;
use Bio::Rfam::Config;
use Data::Printer;


my ($help, $lock, $unlock, @user_list, $commit, @commit_list, $commit_list, $force);
$force = 0; #By default do not force unlocks
$commit = 0; #By default do not allow commits
&GetOptions ( "h"            => \$help,
              "l"            => \$lock,
              "u"            => \$unlock,
              "force"        => \$force,
              "allow_me"     => \$commit,
              "allow_user=s" => \@commit_list );

#We need help!
help() if($help);

#Some very loose vanity checking on the options
if($unlock and ($commit or @commit_list or $lock)) {
  warn "You have specified options which cannot be used together, unlock can only be used with force\n";
  help();
}

if($lock and $force){
  warn "You have specified options which cannot be used together, you can not force a lock.\n";
  help();
}

#Get the config and the database connection.
my $config = Bio::Rfam::Config->new;

my $rfamdb = $config->rfamlive;
if($config->location ne 'EBI'){
  die "Lock operartions only permitted when the MySQL database is accessibly, i.e. withing EBI.\n";
}

#Get the current user
my $user = getpwuid( $< );

if(@commit_list) {
    @commit_list = split(/,/,join(',',@commit_list));  #Allow comma-separated lists of values and multiple occurrences of the options
    $commit_list = join(" ", @commit_list);
} else {
    $commit_list = "";
}

if($commit){
  $commit_list = $user." ".$commit_list;
}

if($lock){
  $rfamdb->resultset('Lock')->lockTheDatabase($user, $commit, $commit_list);
}elsif($unlock){
  $rfamdb->resultset('Lock')->unlockTheDatabase($user, $force);
}else{
  my $row = $rfamdb->resultset('Lock')->search()->first;
  if(!defined($row) or $row->locked == 0){
    print "Database is unlocked\n";
  }else{
    print "The database is currently lockeed by:". $row->locker."\n";
    if($row->allowcommits){
      print "Commits are allowed by ".$row->alsoallow;
    }else{
      print "Commits are not permitted."
    }
    print "\n";
  }
}


sub help {
print STDERR << "EOF";

This script can be used to 'lock' and 'unlock' the database.  When the
database is in the 'locked' state, users will not be unable to use Rfam
scripts to commit data, unless the allow_me or allow_user flags are used.

USAGE:

  $0 -l (locks the database)
  $0 -u (unlocks the database)

OPTIONS FOR LOCKING:

   -allow_me              : Allows the user running the script to perform commits
   -allow_user  <user_id> : Allow user_id to perform commits (can specify multiple users)
   -force                 : Can be used to force the unlocking of the database.
   
EXAMPLE:

    $0 -l -allow_me -allow_user "rdf,sb"

EOF

exit (0);
}
