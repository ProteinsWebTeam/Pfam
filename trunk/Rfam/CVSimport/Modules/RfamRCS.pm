
#
# RfamRCS: some subroutines and globals which are the basis of
# the rfam ci/co system
#
# taken heavily from Ewan's PfamRCS.pm - some day the model here
# can be used in generic fashion for Xfam
#

use lib '/pfam/db/Rfam/scripts/Modules';

package RfamRCS;

use strict;
use FileHandle;
use Rfam;

use vars qw( @ISA 
	     @EXPORT );

@ISA = qw( Exporter );

my $database_lock = "$accession_dir/database_lock";
my $acclog_file   = "$accession_dir/acclog";
my $acc_lock      = "$accession_dir/lock";
my $access_list   = "$accession_dir/access_list";
my $allowed_user_string = "";

my $view_maker = "$scripts_dir/rfamrcs/makerfamview.pl";

open (_LIST, $access_list) or die "Fatal error - could not open the access list file";
while(<_LIST>) {
    /^(\S+)/ and do {
	if ($allowed_user_string) {
	    $allowed_user_string .= ",$1";
	}
	else {
	    $allowed_user_string = "$1";
	}
    };
}  


my $got_lock = 0; # will be set to true when you get the lock
my $db = undef;


sub abort_lock_on_family {
    my($family) = @_;
    my($out,$file);

    $out = 1;

    foreach my $file ( @rcs_file_set ) {
	if( system("rcs -u -q $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: Was unable to abort file [$file] in [$family]... hmmm...\n");
	    $out = 0;
	}
    }

    foreach my $file ( @optional_file_set ) {
	if( !(-e "$rcs_master_dir/$family/$file,v") ) { next; }

	if( system("rcs -u -q $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: Was unable to abort file [$file] in [$family]... hmmm...\n");
	    $out =0;
	}
    }

    if( ! -e "$rcs_master_dir/$family/locked" ) {
	warn("Bad news... you have checked in a family and I have lost my locked file. Ooops!");
    } else {
	unlink("$rcs_master_dir/$family/locked");
    }

    return $out;
}



sub add_users_to_family {
    my $family = shift;
    my @users = @_;

    my $user = join(',', @users);

    foreach my $file ( @rcs_file_set ) {
	if( ! -e "$rcs_master_dir/$family/$file,v" ) {
	    warn("Bad error - for family $family no $file in RCS master!");
	} else {
	    if( system("rcs -a$user $rcs_master_dir/$family/$file,v") ) {
		warn("Bad error - unable to -a$user on $family/$file");
	    }
	}
    }
    foreach my $file ( @optional_file_set ) {
	if( ! -e "$rcs_master_dir/$family/$file,v" ) {
	    next;
	} else {
	    if( system("rcs -a$user $rcs_master_dir/$family/$file,v") ) {
		warn("Bad error - unable to -a$user on $family/$file");
	    }
	}
    }
}



sub add_users_to_system {
    my @users = @_;
    my (%user_hash);

    open(ACCESS,"$access_list") || die "Could not open $access_list ($!) A v. bad error";
    open(ACCESSTEMP,">$access_list.$$") || die "Could not open $access_list.$$ ($!) A v. bad error";

    while( <ACCESS> ) {
	/^(\S+)$/ and do {
	    print ACCESSTEMP;
	    $user_hash{ $1 } = 1;
	};
    }
    close(ACCESS);

    foreach my $user (@users) {
	if ($user_hash{ $user }) {
	    warn("RCS::add_user_to_system - user $user was already there!");
	}
	else {
	    print ACCESSTEMP "$user\n";
	}
    }
    close(ACCESSTEMP);

    if (not rename("$access_list.$$",$access_list)) {
	warn("RCS::add_user_to_system - Could not rename access list file!");
    }
}


    
sub allocate_new_accession {   # hack at the moment
                               # should use accmaps, locks etc
    my $family = shift;
    
    if( !$family ) {
	warn("Cannot allocate a new family without a name");
	return undef;
    }

    open(ACCLOG,"$acclog_file") || die "Could not open $acclog_file ($!) A v. bad error";
    open(ACCTEMP,">$acclog_file.$$") || die "Could not open $acclog_file.$$ ($!) A v. bad error";

    my $line;
    while( <ACCLOG> ) {
	print ACCTEMP;
	$line = $_;
    }

    ($line =~ /^(\S{2}\d+)/) || die "Last line of acclog is not an accession ($_). Big trouble!";

    my $acc = $1;
    $acc++;

    my $me = `whoami`;
    chop $me;
    my $date = gmtime();

    print ACCTEMP "$acc [$family] [$me] [$date]\n";

    close(ACCLOG);
    close(ACCTEMP);

    rename("$acclog_file.$$",$acclog_file);
    return "$acc";
}



sub check_family_directory_exists {
    my ($family) = @_;
    if( !(-e "$rcs_master_dir/$family") ) {
	return 0;
    }    
    return 1;
}


sub check_family_exists {
    my ($family) = @_;
    my $out = 1;

    foreach my $file ( @rcs_file_set ) {
	if( check_family_file_exists($family,"$file") == 0 ) { 
	    warn "could not find $file,v in [$family]"; 
	    $out = 0; 
	}
    }
    return $out;
}


sub check_family_file_exists {
    my ($family,$file) = @_;

    if( !(-e "$rcs_master_dir/$family/$file,v") ) { return 0;}
    else { return 1;}

}


sub check_database_isnot_locked {
    my $islocked = 0;
    my $locker = 0;
    my @allow;

    if ( open(_LOCK, "<$database_lock")) {
	while(<_LOCK>) {
	    /^(\S+): has locked rfam$/ and do {
		$locker = $1;
	    };
	    /^allow (\S+)$/ and do {
		push (@allow, $1);
	    };
	}
    }

    if ($locker) {
	$islocked = 1;
    }
    
    return ($islocked,$locker,\@allow);
}


sub check_family_isnot_locked {
    my ($family) = @_;
    my ($locked, $locker);
    my $islocked = 0;

    if ( open(_LOCK, "<$rcs_master_dir/$family/locked")) {
	while(<_LOCK>) {
	    /^Lock file to indicate lock by user (\S+)/ && do {
		$locker = $1;
	    };
	}
    }

    if ($locker) {
	$islocked = 1;
	$locked = "$locker";
    }

    open(RLOG,"rlog -L -h $rcs_master_dir/$family/SEED|") || die "Could not open rlog pipe!";

    while(<RLOG>) {
	if( /^locks:/ ) {
	    $islocked = 1;
	    while(<RLOG>) {
		chop;
		!/^\s\s*(\w*)/ && last;
		$locked .= " $1";
	    }
	}
    }
    close(RLOG);
    
    return ($islocked,$locked);
}


sub check_in_rcs_files {
    my($family,$comment) = @_;
    my $new_rcs;
    my $out = 1;

    foreach my $file ( @rcs_file_set ) {
	if( system("ci -f -q -m\"$comment\" $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: Was unable to check in $file... hmm...\n");
	    $out = 0;
	}
#	system("chmod g+rw $rcs_master_dir/$family/$file,v");
    }

    foreach my $file ( @optional_file_set ) {
	if( !(-e "$rcs_master_dir/$family/$file") ) { 
	    next; 
	}
	if( !( -e "$rcs_master_dir/$family/$file,v") ) {
	    $new_rcs = 1;
	}

	if( $new_rcs == 1 ) {
	    if( system("rcs -a$allowed_user_string -q -i -t-\"$comment\" $rcs_master_dir/$family/$file") != 0 ) {
		print("RCS: Was unable make new file $file... hmm...\n");
		$out = 0;
	    }	
	}	       
	if( system("ci -f -q -m\"$comment\" $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: Was unable to check in $file... hmm...\n");
	    $out = 0;
	}
#	system("chmod g+rw $rcs_master_dir/$family/$file,v");
    }

    if( ! -e "$rcs_master_dir/$family/locked" ) {
	warn("Bad news... you have checked in a family and I have lost my locked file. Ooops!");
    } else {
	unlink("$rcs_master_dir/$family/locked");
    }

    return $out;
}


    
#
# check_out_family called as check_out_family("egf",\*STDOUT);
#

sub check_out_family {
    my($family,$handle) = @_; 
    my $out = 1;

    foreach my $file ( @rcs_file_set ) {
	if( check_out_file($file,$family,$handle) == 0 ) {
	    $out = 0;
	}
    }
    foreach my $file ( @optional_file_set ) {
	if( -e "$rcs_master_dir/$family/$file,v" ) {
	    if( check_out_file($file,$family,$handle) == 0 ) {
		$out = 0;
	    }
	}
    }

    # make locked file

    open(LOC,">$rcs_master_dir/$family/locked") || die("Bad bug ... could not open locked file $!");

    my $user = `whoami`; chop $user;
    print LOC "Lock file to indicate lock by user $user\n";

    close(LOC);

    return $out;
}

#
# Really a sub for check_out_family
#

sub check_out_file {
    my ($file,$family,$handle) = @_;

    if( !open(CHECK,"co -l -q $rcs_master_dir/$family/$file|") ) {
	print("RCS: Could not open co -l for $file of family $family, $!\n");
	return 0;
    }

    while(<CHECK>) {
	print $handle;
    }

    close(CHECK);
    return 1;
}


#
# Copies the current directory files
# to the new place
# call as (family) - assummes you want to move them to . 
#

sub copy_from_current {
    my($family) = @_;
    my($out) = 1;

    foreach my $file ( @rcs_file_set ) {
	if( system("cp $current_dir/$family/$file ./$file") != 0 ) {
	    $out = 0;
	}
	if( system("chmod u+w ./$file") != 0 ) {
	    $out = 0;
	}
    }

    foreach my $file ( @optional_file_set ) {
	if( -e "$rcs_master_dir/$family/$file,v" ) {
	    if( system("cp $current_dir/$family/$file ./$file") != 0 ) {
		$out = 0;
	    }
	    if( system("chmod u+w ./$file") != 0 ) {
		$out = 0;
	    }
	}
    }
    return $out;
}


sub get_locked_families {
    my (@names);

    if( !opendir(_RFAM_INTERNAL_DIR,$rcs_master_dir) ) {
	print("RCS: A real bad problem, can't see root at $rcs_master_dir\n");
	return @names;
    }

    my @files = readdir _RFAM_INTERNAL_DIR;

    foreach my $file (@files) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if( !(-d "$rcs_master_dir/$file") ) {
	    print("RCS: I don't like this, a non directory file [$file] in the RCS_MASTER. Yuk!\n");
	}
	else {
	    if( -e "$rcs_master_dir/$file/locked" ) {
		push(@names,$file);
	    }
	}

    }

    return @names;
}


sub get_all_family_names {
    my (@names);

    if( !opendir(_RFAM_INTERNAL_DIR,$rcs_master_dir) ) {
	print("RCS: A real bad problem, can't see root at $rcs_master_dir\n");
	return @names;
    }

    my @files = readdir _RFAM_INTERNAL_DIR;

    closedir(_RFAM_INTERNAL_DIR);

    foreach my $file (@files) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if( !(-d "$rcs_master_dir/$file") ) {
	    print("RCS: I don't like this, a non directory file [$file] in the RCS_MASTER. Yuk!\n");
	}
	else {
	    push(@names,$file);
	}

    }

    return @names;
}


sub get_dead_family_names {
    my (@names);

    if( !opendir(_RFAM_INTERNAL_DIR,$rcs_attic_dir) ) {
	print("RCS: A real bad problem, can't see attic at $rcs_attic_dir\n");
	return @names;
    }

    my @files = readdir _RFAM_INTERNAL_DIR;

    closedir(_RFAM_INTERNAL_DIR);

    foreach my $file (@files) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if( !(-d "$rcs_attic_dir/$file") ) {
	    print("RCS: I don't like this, a non directory file [$file] in the RCS_ATTIC. Yuk!\n");
	}
	else {
	    push(@names,$file);
	}
    }

    return @names;
}


sub get_info_on_family {
    my($family,$fileout) = @_;
    my($line, $locker);
    my $locked = "";

    if ( open(_LOCK, "<$rcs_master_dir/$family/locked")) {
	while(<_LOCK>) {
	    /^Lock file to indicate lock by user (\S+)/ && do {
		$locker = $1;
	    };
	}
    }

    if( !open(INFO,"rlog -b $rcs_master_dir/$family/DESC |") ) {
	print("RCS: can't get any info on family [$family]\n");
    }

    while (<INFO>) {
	if( /^locks/ ) {
	    $line = <INFO>;
	    chop $line;
	    if( ($line =~ /^\s*(\w*):/) ) {
		$locked = $1;
	    }
	}
	/^-/ && last;
    }

    if( $locked ) {
	print $fileout "Current rcs lock by $locked (lock file says $locker)\n";
    }

    while (<INFO>) {
	# /^-/ && last;

	print $fileout $_;
    }
    close(INFO);
}
    

sub get_accession_lock {
    my ($me, $line, $name);

    #
    # returns "success" for success and "lockee" for failure... 
    #

    if( open(LOCK,"$acc_lock") ) {
	$line = <LOCK>;
	($name) = split (/:/,$line);
	return $name;
    } else {
	open(LOCK,">$acc_lock") || die "Could not open lock to write. Something is badly wrong. file [$acc_lock]";
	$me = `whoami`;
	chop $me;
	print LOCK "$me: has the accession lock\n";
	close(LOCK);
	system("chmod 0640 $acc_lock");
	$got_lock = 1;
	return "success";
    }
}


sub get_database_lock {
    my @allow = @_;
    my ($me, $line, $name);

    #
    # returns "success" for success and "lockee" for failure... 
    #

    if( open(LOCK,"$database_lock") ) {
	$line = <LOCK>;
	($name) = split (/:/,$line);
	return $name;
    } else {
	open(LOCK,">$database_lock") || die "Could not open lock to write. Something is badly wrong. file [$database_lock]";
	$me = `whoami`;
	chop $me;
	print LOCK "$me: has locked the database\n";
	foreach my $allowed (@allow) {
	    print LOCK "allow $allowed\n";
	}
	close(LOCK);
	system("chmod 0640 $database_lock");

	return "success";
    }
}


sub get_short_info_on_family {
    my($family,$fileout) = @_;
    my($line, $lasttouched, $lastauthor);

    my ($islocked, $locker) = &check_family_isnot_locked( $family ); 

    if( !open(INFO,"rlog -b $rcs_master_dir/$family/DESC |") ) {
	print("RCS: can't get any info on family [$family]\n");
    }

    while (<INFO>) {
	/^-/ && last;
    }

    while (<INFO>) {
	if( /^date:\s+([0-9\/]*\s+[0-9:]+);\s+author:\s+(\w+);/ ) {
	    $lasttouched = $1;
	    $lastauthor  = $2;
	}
	/^-/ && last;
	/^=/ && last;
    }
    close(INFO);

    if( $islocked == 1 ) {
	print $fileout sprintf("[%-15s] Revision $lasttouched [%-10s] Locked [$locker]\n",$family,$lastauthor);
    } else {
	print $fileout sprintf("[%-15s] Revision $lasttouched [%-10s]\n",$family,$lastauthor);
    }

}

 
sub label_family {
    my $family = shift;
    my $label  = shift;
    my $ret = 1;

    if( !defined $label ) {
	warn("label family requires a name");
	return undef;
    }

    foreach my $file ( @rcs_file_set ) {
	if( system("rcs -N$label: $rcs_master_dir/$family/$file") != 0 ) {
	    warn("Could not label $family/$file...");
	    $ret = 0;
	}
    }

    foreach my $file ( @optional_file_set ) {
	if( ! -e "$rcs_master_dir/$family/$file,v" ) {
	    next;
	}

	if( system("rcs -N$label: $rcs_master_dir/$family/$file") != 0 ) {
	    warn("Could not label $family/$file...");
	    $ret = 0;
	}
    }
    return $ret;
}


#
# Returns a hash of label->version
#

sub label_hash {
    my $family = shift;
    my $dead = shift;
    my ($tlabel,$trev,%h);

    if( !defined $family ) {
	warn("should call has_changed as family");
	return undef;
    }

    #
    # open align file with rlog
    #

    my $loc = ($dead)?$rcs_attic_dir:$rcs_master_dir;

    open(RLOG,"rlog $loc/$family/DESC |") || die "Could not open pipe to rlog $!";
    while(<RLOG>) {
	/^symbolic names:/ && last;
    }

    while(<RLOG>) {
	/^comment/ && last;
	/\s+(\S+):\s+(\S+)/ && do {
	    $tlabel = $1;
	    $trev = $2;
	    $h{$tlabel} = $trev;
	};
     
    }

    close(RLOG) || die "Could not handle rlog pipe $! $?";

    return %h;
}



sub lock_family {
    my ($family, $user) = @_;

    my($out) = 1;

    foreach my $file (@rcs_file_set) {
	if( lock_file($file,$family) == 0 ) {
	    $out = 0;
	}
    }
    foreach my $file (@optional_file_set) {
	if( -e "$rcs_master_dir/$family/$file,v" ) {
	    if( lock_file($file,$family) == 0 ) {
		$out = 0;
	    }
	}
    }

    # make locked file

    if (not $user) {
	$user = `whoami`; chop $user;
    }

    open(LOC,">$rcs_master_dir/$family/locked") || die("Bad bug ... could not open locked file $!");
    print LOC "Lock file to indicate lock by user $user\n";
    close(LOC);

    return $out;
}



sub lock_file {
    my($file,$family) = @_;

    system "rcs -l -q $rcs_master_dir/$family/$file" and 
	warn("Major error - could not get lock for $family/$file");

    return 1;
}



sub make_new_rcs_directory {
    my ($newfamily) = @_;
    my($out);

    $out = 1;

    if( mkdir("$rcs_master_dir/$newfamily", 0775) == 0 ) {
	print("RCS: Could not make new rcs directory for [$newfamily] due to $!\n");
	$out = 0;
    }

    if( mkdir("$current_dir/$newfamily", 0775) == 0 ) {
	print("RCS: Could not make new current directory for [$newfamily] due to $!\n");
	$out = 0;
    }
    system("chmod a+rxw $rcs_master_dir/$newfamily");
    system("chmod a+rxw $current_dir/$newfamily");

    return $out;
}



sub make_new_rcs_files {
    my ($family,$comment) = @_;
    my $out = 1;

    foreach my $file (@rcs_file_set) {
	if( system("rcs -a$allowed_user_string -q -i -t-\"$comment\" $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: Was unable make new file $file... hmm...\n");
	    $out = 0;
	}
    }
    foreach my $file (@optional_file_set) {
	if( !(-e "$rcs_master_dir/$family/$file") ) {next;}

	if( system("rcs -a$allowed_user_string -q -i -t-\"$comment\" $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: Was unable make new file $file... hmm...\n");
	    $out = 0;
	}
    }
    return $out;
}


sub make_view_files {
    my $family = shift;

    # if the files are already there, delete them

    foreach my $file ( @view_file_set ) {
	if( -e "$current_dir/$family/$file" ) {
	    unlink("$current_dir/$family/$file");
	}
    }

    umask 002;
    open(TODO,">$current_dir/$family/todo.view") || die "Could not open todo.view file for $family";
    my $me = `whoami`;
    chomp $me;
    print TODO "Set off for $family by $me\n";
    close(TODO);
    
    # get lock on family

    &lock_family($family, "VIEW");

    # submit makeview

    system("bsub -q pfam_slow -R \"hname=pfam\" -o /dev/null $view_maker $family");

    # when the view files have finished being built, the lock will be released

    return 1;
}



sub move_family_to_current {
    my ($family) = @_;
    my $out = 1;

    foreach my $file (@rcs_file_set) {
	if( system("mv $rcs_master_dir/$family/$file ./$file") != 0 ) {
	    $out = 0;
	}
	if( system("chmod a-x ./$file") != 0 ) {
	    $out = 0;
	}
    }

    foreach my $file (@optional_file_set) {
	if( -e "$rcs_master_dir/$family/$file" ) {
	    if( system("mv $rcs_master_dir/$family/$file ./$file") != 0 ) {
		$out = 0;
	    }
	    if( system("chmod a-x ./$file") != 0 ) {
		$out = 0;
	    }
	}
    }
	    
    return $out;
}



sub move_files_to_rcs_directory {
    my ($family,$fromdir) = @_;
    my $out = 1;

    foreach my $file (@rcs_file_set) {
	if( system("cp $fromdir/$file $rcs_master_dir/$family/$file") != 0 ) {
	    $out = 0;
	}
    }

    foreach my $file (@optional_file_set ) {
	if( -e "$fromdir/$file") {
	    if( system("cp $fromdir/$file $rcs_master_dir/$family/$file") != 0 ) {
		$out = 0;
	    }
	}
    }
    return $out;
}
    
#
# Used for pfmove
#
# Moves RCS directory and deletes the old CURRENT, putting in a new current.
#

sub move_rcs_directory {     # not sorted
    my $from = shift;
    my $to   = shift;
    my (%temphash,$acc,$name, $refhash);

    # get out a copy of the database for the accession
    # numbers and lock it.

    
    if( -d "$rcs_master_dir/$to" ) {
	print("RCS: Directory $rcs_master_dir/$to exists. Cannot move!");
	return 1;
    }

    $db = Bio::Pfam::default_db();
    if( ($name = $db->_get_lock()) ) {
	# locks the SDMB file
	print STDERR "Unable to get the lock for the SDBM_file - $name has it\n";
	return 1;
    }

    $refhash = $db->_get_tied_accmap();
    %temphash = reverse %{$refhash};

    if( rename("$rcs_master_dir/$from","$rcs_master_dir/$to") == 0 ) {
	print("RCS: Very bad error. Please talk to adminstator. Unable to move RCS $from to $to $!");
	return 1;
    }

    
    if( system("rm -r -f $current_dir/$from") ){
	print("RCS: Very bad error. Please talk to adminstator. Unable to remove current $from$!");
	return 1;
    }

    if( mkdir("$current_dir/$to", 0775) == 0 ) {
	print("RCS: Could not make new Pfam current directory for [$to] due to $!\n");
	return 1;
    }

    system("chmod a+rxw $current_dir/$to");

    $acc = $temphash{$from};
    $db->_move_accession($acc,$to);

    $db->_unlock();
    
    return 0;
}



sub release_accession_lock {

    if( $got_lock != 1 ) {
	warn("Cannot release lock when you don't have it!");
	return undef;
    }

    if( unlink("$acc_lock") == 0 ) {
	warn("Bad news: could not release lock! This is bad");
	return undef;
    }
    $got_lock = 0;
    return 1;
}


	
sub release_database_lock {
    if( unlink("$database_lock") == 0 ) {
	warn("Bad news: could not release database lock");
	return undef;
    }

    return 1;
}
    

sub remove_users_from_family {
    my $family = shift;
    my @users = @_;

    my $user = join(',', @users);

    foreach my $file ( @rcs_file_set ) {
	if( ! -e "$rcs_master_dir/$family/$file,v" ) {
	    warn("Bad error - for family $family no $file in RCS master!");
	} else {
	    if( system("rcs -e$user $rcs_master_dir/$family/$file,v") ) {
		warn("Bad error - unable to -e$user on $family/$file");
	    }
	}
    }
    foreach my $file ( @optional_file_set ) {
	if( ! -e "$rcs_master_dir/$family/$file,v" ) {
	    next;
	} else {
	    if( system("rcs -e$user $rcs_master_dir/$family/$file,v") ) {
		warn("Bad error - unable to -e$user on $family/$file");
	    }
	}
    }
}



sub remove_users_from_system {
    my @users = @_;

    my $found;

    open(ACCESS,"$access_list") || die "Could not open $access_list ($!) A v. bad error";
    open(ACCESSTEMP,">$access_list.$$") || die "Could not open $access_list.$$ ($!) A v. bad error";

    while( <ACCESS> ) {
	$found = 0;
	foreach my $user (@users) {
	    if (/^$user$/) {
		$found = "true";
	    }
	}
	if (not $found) {
	    print ACCESSTEMP;
	}
    }
    close(ACCESS);
    close(ACCESSTEMP);

    if (not rename("$access_list.$$",$access_list)) {
	warn "RCS::remove_user_from_system - could not rename access_list file!";
    }
}


   
sub show_rcs_help {
    my($out) = @_;
    print $out <<"EOF";

Pfam RCS system (written by Ewan Birney)

USAGE - Program [Domain1] [Domain2] [Domain3] etc
Programs:
      pfco                      Check out (with lock) a family
      pfci -m 'message'         Check in family
      pfabort                   Remove lock on family (revert to previous revision)
      pfnew                     Make a new family
      pfinfo                    Get info on family
      pfupdate                  Update with latest Pfam version (pfco without a lock)
      pfmove <old-id> <new-id>  Move one id to another id
      pfkill <id>               CAUTION: Removes family from Pfam database 
      pfret <label> <id>        Retrieve family from a particular label
      pflock <-l|u>             Lock|unlock the database
EOF

}

    
#
# update_family_label called as check_out_family("egf",label);
#

sub update_family_label {
    my($family, $label, $handle) = @_; 
    my $out = 1;

    foreach my $file (@rcs_file_set) {
	if( update_file_label($file,$family,$handle,$label) == 0 ) {
	    $out = 0;
	}
    }
    foreach my $file (@optional_file_set) {
	if( -e "$rcs_master_dir/$family/$file,v" ) {
	    if( update_file_label($file,$family,$handle,$label) == 0 ) {
		$out = 0;
	    }
	}
    }

    return $out;
}


sub update_file_label {
    my ($file,$family,$handle,$label) = @_;

    if( !open(CHECK,"co -r$label  -q $rcs_master_dir/$family/$file|") ) {
	print("RCS: Could not open co -l for $file of family $family, $!\n");
	return 0;
    }

    while(<CHECK>) {
	print $handle;
    }

    close(CHECK);
    return 1;
}
    


sub update_current_directory {
    my($family) = @_;

    my $out = 1;

    foreach my $file (@rcs_file_set) {
	if( system("co -q $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: For file $file of family $family,\ncould not check (unlocked) out for update\n");
	    $out = 0;
	}
	if( system("mv -f $rcs_master_dir/$family/$file $current_dir/$family/$file") != 0 ) {
	    print("RCS: For file $file of family $family,\ncould not move file from RCS to current for update\n");
	    $out = 0;
	}
	if( system("chmod a-x $current_dir/$family/$file") != 0 ) {
	    $out = 0;
	}
    }

    foreach my $file (@optional_file_set) {
	if( !(-e "$rcs_master_dir/$family/$file,v") ) { 
	    next; 
	}
	if( system("co -q $rcs_master_dir/$family/$file") != 0 ) {
	    print("RCS: For file $file of family $family,\n could not check (unlocked) out for update\n");
	    $out = 0;
	}
	if( system("mv -f $rcs_master_dir/$family/$file $current_dir/$family/$file") != 0 ) {
	    print("RCS: For file $file of family $family,\ncould not move file from RCS to current for update\n");
	    $out = 0;
	}
	if( system("chmod a-x $current_dir/$family/$file") != 0 ) {
	    $out = 0;
	}
    }

    # ok - fine.

    return $out;
}


sub user_has_locked_family {
    my ($family,$user) = @_;
    my($islocked,$locked) = &check_family_isnot_locked($family);

    if( !$islocked) {
	print("RCS: The family [$family] has no lock on it -- user $user cannot unlock!\n");
    }

    if( $locked =~ /^\s*$user\s*/ ) {
	return (1,$locked);
    }
    else {
	return (0,$locked);
    }
}


#
# tell require things that we are ok
#

1;














