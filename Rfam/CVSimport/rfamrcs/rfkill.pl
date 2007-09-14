#!/sotware/bin/perl -w

# Kills an Rfam family
#
# Marks in the accession table that it is a __DEAD_FAMILY__
#
# Moves the family into the RCS_ATTIC directory
#
# Makes a DEAD file in that directory with the previous id placed
# in the DESC file
#


use strict;
use Rfam;
use RfamRCS;
use Rfam::UpdateRDB;

if( $#ARGV == -1 ) {
  &usage();
  exit;
}

## first, check that the database is not locked

my( $locked, $locker, $allow_ref ) = &RfamRCS::check_database_isnot_locked;
if( $locked ) {
    my $allowed;
    foreach my $allow ( @{$allow_ref} ) {
        $allowed = 1 if( $allow eq "rfkill" );
    }
    unless( $allowed ) {
        die "rfkill aborted: database is locked by $locker";
    } 
}

my $acc = shift;

if( $acc =~ /\/$/ ) {
    $acc =~ s/\/$//g;
}

## check that we can connect to the RDB

my $rdb = Rfam->live_rdb_update();
eval { 
    $rdb->connect();
};
if ($@) {
    die "rfkill: error - could not connect to RDB - kill aborted";
}
$rdb->disconnect();

if( &RfamRCS::check_family_directory_exists($acc) != 1 ) {
    print "Family $acc does not exist. Please don't try to kill it!\n";
    exit(-1);
}

my ($islocked,$lockedpeople) = &RfamRCS::check_family_isnot_locked($acc);

if( $islocked ) {
    print("rfkill: Family [$acc] has been locked by [$lockedpeople]\n, cannot kill it\n");
    exit(1);
}

my $db = Rfam->default_db();

if( -e "$Rfam::rcs_attic_dir/$acc" ) {
    print "$acc already exists in the attic. This could be because you've tried to kill the same family twice.\n";
    exit(1);
}

print "Please give a comment for killing family [$acc]\n";
print "Finish comment by a . on the line by itself\n"; 

my @comment;
while( <STDIN> ) {
    chop;
    /^\s*\.\s*$/ && last;
    push(@comment,$_);
}

# Prompt for forward

print "Please give a list of current accs for the family to forward to\n";
print "pressing return after each one. Give '.' on a line on its own\n";
print "to end the list\n";

my @facc;
while (($_ = <STDIN>) !~ /^\.$/) {
    chop;
    if( $_ =~ /^([0-9a-zA-Z\-_]+)/ ) {
	my $facc = $1;
	my $fid;
	eval {
	    $fid = $db->acc2id($facc);
	};
	push( @facc, $facc );
	if ($@) {
	    warn "rfkill: accession $facc does not exist in the database\n";
	    next;
	}
    }
}

my $caught_cntrl_c;
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

# Ok. Attempt to get the lock. If we fail a rather ungraceful exit.

if( my $ret = $db->_get_lock() ) {
    die "rfci: The accession lock has been grabbed by [$ret].\nAccession locking should be short - try again in a couple of minutes\n";
}				 

if( system("rm -rf $Rfam::current_dir/$acc") != 0 ) {
    die("rfkill: could not remove $acc from current! This is bad - tell someone!\n");
}

# move the accession in the db file to dead

$db->_get_lock();
$db->_kill_accession($acc);
$db->_unlock();

if( system("mv $Rfam::rcs_master_dir/$acc $Rfam::rcs_attic_dir/$acc") != 0 ) {
    die("rfkill: could not move $acc from RCS_MASTER to RCS_ATTIC! This is bad - tell someone!\n");
}

open(DEAD,">$Rfam::rcs_attic_dir/$acc/DEAD") || die "Could not open DEAD file $!\n";
print DEAD "AC   $acc\n";
print DEAD "KL   This family has been killed\n";

foreach my $facc ( @facc ) {
    print DEAD "FW   $facc\n";
}

foreach ( @comment ) {
    print DEAD "CC   $_\n";
}

close(DEAD) || die "Could not close DEAD file $!\n";

# finished

print "Killed family $acc\n\n";


#############################################################################
# At this point, everything went fine for the kill. We can now update the 
# relational database. God help us if anything goes wrong
#############################################################################

# The entry is dead, but we need its accession number. Therefore,
# we go through the middleware layer, saying specifically that we
# want a dead entry.

my $en;
eval {
    if (defined $rdb) {
	$en = $db->get_Entry_by_acc( $acc, 1 );
	$rdb->check_in_dead_Entry( $en );
    }
};    
$@ and print STDERR "rfkill: RDB update; Could not update the RDB for the culling of $acc [$@]\n";

if( $caught_cntrl_c ) {
    print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}


sub usage {
    print <<EOF;
rfkill <accession>

Removes family from Rfam database, storing it in the attic

It prompts for comments and potential family redirects
EOF
}
