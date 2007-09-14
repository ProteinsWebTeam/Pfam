#!/software/bin/perl -w


=head1 rfadduser

Adds a user to rfam by going over the RCS system
and doing a rcs -a<user-name> over all the files

=cut

use strict;
use Rfam;
use RfamRCS;
use Getopt::Std;

use vars qw($opt_r);

&getopts('rv');
my $remove = $opt_r;

if( $#ARGV == -1 ) {
    print STDERR <<"EOF";
rfadduser <-r> <user>

Adds a user to the access control lists for Rfam. If -r 
is specified, removes the user
EOF

    exit;
}





my @users = @ARGV;

## first, check that the database is not locked

my ($locked, $locker) =  &RfamRCS::check_database_isnot_locked;
die "pfadduser aborted: database is locked by $locker" if $locked; 

my $db = Rfam->default_db();

foreach my $family ( $db->get_allacc() ) {
    if ($remove) {
	&RfamRCS::remove_users_from_family( $family, @users );
    }
    else {
	&RfamRCS::add_users_to_family( $family, @users);
    }
}

if ($remove) {
    &RfamRCS::remove_users_from_system( @users );
}
else {
    &RfamRCS::add_users_to_system( @users );	
}
