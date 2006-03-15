
#
# PfamRCS: some subroutines and globals which are the basis of
# the pfam ci/co system
#
#
#

package Bio::Pfam::PfamRCS;

use strict;
use FileHandle;
use Bio::Pfam;
use HMMResults;

use vars qw(@ISA 
	    @EXPORT
	    $Pfam_rcs_root
	    $Pfam_current_root
	    $Pfam_rcs_attic);


@ISA = ('Exporter');
@EXPORT = qw($Pfam_rcs_root
	     $Pfam_current_root
	     $Pfam_rcs_attic);

$Pfam_rcs_root     = $Bio::Pfam::rcs_master_dir;
$Pfam_current_root = $Bio::Pfam::rcs_current_dir;
$Pfam_rcs_attic    = $Bio::Pfam::rcs_attic_dir;

my @Pfam_Annotation_File_Set = @Bio::Pfam::annotation_file_set;
my @Pfam_Family_File_Set     = @Bio::Pfam::family_file_set;
my @Pfam_View_File_Set       = @Bio::Pfam::view_file_set;
my @Pfam_optional_file       = @Bio::Pfam::optional_file_set;

my $view_maker  = "makepfamview";

my $pfam_accession_dir = "$Bio::Pfam::rcs_accession_dir";
my $pfam_accession_attic_dir = "$Bio::Pfam::rcs_accession_attic_dir";

my $pfam_database_lock = "$pfam_accession_dir/database_lock";
my $pfam_acc_log  = "$pfam_accession_dir/acclog";
my $pfam_acc_lock = "$pfam_accession_dir/lock";
my $pfam_acc_temp = "$pfam_accession_dir/acctemp";
my $pfam_access_list = "$pfam_accession_dir/access_list";
my $pfam_access_temp = "$pfam_accession_dir/access_temp";
my $allowed_user_string = "";

open (_LIST, $pfam_access_list) or die "Fatal error - could not open the access list file";
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

    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( system("rcs -u -q $Pfam_rcs_root/$family/$file") != 0 ) {
	    print("PFAMRCS: Was unable to abort file [$file] in [$family]... hmmm...\n");
	    $out =0;
	}
    }

    foreach my $file (@Pfam_optional_file ) {
	if( !(-e "$Pfam_rcs_root/$family/$file,v") ) { next; }

	if( system("rcs -u -q $Pfam_rcs_root/$family/$file") != 0 ) {
	    print("PFAMRCS: Was unable to abort file [$file] in [$family]... hmmm...\n");
	    $out =0;
	}
    }


    if( ! -e "$Pfam_rcs_root/$family/locked" ) {
	warn("Bad news... you have checked in a family and I have lost my locked file. Ooops!");
    } else {
	unlink("$Pfam_rcs_root/$family/locked");
    }

    return $out;
}



sub add_users_to_family {
    my $family = shift;
    my @users = @_;

    my $user = join(',', @users);

    foreach my $file ( @Pfam_Family_File_Set, @Pfam_Annotation_File_Set ) {
	if( ! -e "$Pfam_rcs_root/$family/$file,v" ) {
	    warn("Bad error - for family $family no $file in RCS master!");
	} else {
	    if( system("rcs -a$user $Pfam_rcs_root/$family/$file,v") ) {
		warn("Bad error - unable to -a$user on $family/$file");
	    }
	}
    }
    foreach my $file ( @Pfam_optional_file ) {
	if( ! -e "$Pfam_rcs_root/$family/$file,v" ) {
	    next;
	} else {
	    if( system("rcs -a$user $Pfam_rcs_root/$family/$file,v") ) {
		warn("Bad error - unable to -a$user on $family/$file");
	    }
	}
    }
}



sub add_users_to_system {
    my @users = @_;
    my (%user_hash);

    open(ACCESS,"$pfam_access_list") || die "Could not open $pfam_access_list ($!) A v. bad error";
    open(ACCESSTEMP,">$pfam_access_temp") || die "Could not open $pfam_access_temp ($!) A v. bad error";

    while( <ACCESS> ) {
	/^(\S+)$/ and do {
	    print ACCESSTEMP;
	    $user_hash{ $1 } = 1;
	};
    }
    close(ACCESS);

    foreach my $user (@users) {
	if ($user_hash{ $user }) {
	    warn("PfamRCS::add_user_to_system - user $user was already there!");
	}
	else {
	    print ACCESSTEMP "$user\n";
	}
    }
    close(ACCESSTEMP);

    if (not rename($pfam_access_temp,$pfam_access_list)) {
	warn("PfamRCS::add_user_to_system - Could not rename access list file!");
    }
}


    
sub allocate_new_pfam_accession {
    my $family = shift;
    my ($me, $date, $db, $refhash, $name, $acc, $line);
    
    if( !$family ) {
	warn("Cannot allocate a new family without a name");
	return undef;
    }

    if( $got_lock != 1 ) {
	warn("Cannot allocate a new family without locking the accession");
	return undef;
    }

    $db = Bio::Pfam::default_db();
    if( ($name = $db->_get_lock()) ) {
	# locks the SDMB file
	print STDERR "Unable to get the lock for the SDBM_file - $name has it\n";
	return undef;
    }

    open(ACCLOG,"$pfam_acc_log") || die "Could not open $pfam_acc_log ($!) A v. bad error";
    open(ACCTEMP,">$pfam_acc_temp") || die "Could not open $pfam_acc_temp ($!) A v. bad error";

    while( <ACCLOG> ) {
	print ACCTEMP;
	$line = $_;
    }

    ($line =~ /^PF(\d+)/) || die "Last line of Pfam acclog is not an accession ($_). Big trouble!";

    $acc = $1;
    $acc++;

    $me = `whoami`;
    chop $me;

    $date = gmtime();

    print ACCTEMP "PF$acc [$family] [$me] [$date]\n";

    close(ACCLOG);
    close(ACCTEMP);

    rename($pfam_acc_temp,$pfam_acc_log);

    # update the hash
    $db->_add_accession("PF$acc",$family);

    $db->_unlock();

    return "PF$acc";
}


sub check_family_directory_exists {
    my ($family) = @_;

    if( !(-e "$Pfam_rcs_root/$family") ) {
	return 0;
    }
    
    return 1;
}

sub check_family_exists {
    my ($family) = @_;
    my $out = 1;


    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( check_family_file_exists($family,"$file") == 0 ) { 
	    warn "could not find $file,v in [$family]"; 
	    $out = 0; 
	}
    }

    return $out;
}




sub check_family_file_exists {
    my ($family,$file) = @_;

    if( !(-e "$Pfam_rcs_root/$family/$file,v") ) { return 0;}
    else { return 1;}

}



sub check_database_isnot_locked {
    my $islocked = 0;
    my $locker = 0;
    my @allow;

    if ( open(_LOCK, "<$pfam_database_lock")) {
	while(<_LOCK>) {
	    /^(\S+): has locked pfam$/ and do {
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

    if ( open(_LOCK, "<$Pfam_rcs_root/$family/locked")) {
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

    open(RLOG,"rlog -L -h $Pfam_rcs_root/$family/SEED|") || die "Could not open rlog pipe!";

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
    my($family,$comment,$annotonly) = @_;
    my($out,$file, $new_rcs);

    $out = 1;

    foreach my $file (@Pfam_Annotation_File_Set) {
	if( system("ci -f -q -m\"$comment\" $Pfam_rcs_root/$family/$file") != 0 ) {
	    print("PFAMRCS: Was unable to check in $file... hmm...\n");
	    $out = 0;
	}
	system("chmod g+rxw $Pfam_rcs_root/$family/$file,v");
    }


    foreach my $file (@Pfam_Family_File_Set) {
	if (not $annotonly) {
	    if( system("ci -f -q -m\"$comment\" $Pfam_rcs_root/$family/$file") != 0 ) {
		print("PFAMRCS: Was unable to check in $file... hmm...\n");
		$out = 0;
	    }
	    system("chmod g+rxw $Pfam_rcs_root/$family/$file,v");
	}
	else {
	    if ( system("rcs -u -q $Pfam_rcs_root/$family/$file") != 0 ) {
		print("PFAMRCS: Was unable to release the lock on [$file] in [$family].. hmm...\n");
		$out = 0;
	    }
	    if (not unlink "$Pfam_rcs_root/$family/$file") {
		print("PFAMRCS: Was unable to remove $Pfam_rcs_root/$family/$file when releasing lock.\n");
		$out = 0;
	    }
	}
    }

    foreach my $file (@Pfam_optional_file) {
	if( !(-e "$Pfam_rcs_root/$family/$file") ) { 
	    next; 
	}
	if( !( -e "$Pfam_rcs_root/$family/$file,v") ) {
	    $new_rcs = 1;
	}

	if (not $annotonly) {
	    if( $new_rcs == 1 ) {
		if( system("rcs -a$allowed_user_string -q -i -t-\"$comment\" $Pfam_rcs_root/$family/$file") != 0 ) {
		    print("PFAMRCS: Was unable make new file $file... hmm...\n");
		    $out = 0;
		}	
	    }	       
	    if( system("ci -f -q -m\"$comment\" $Pfam_rcs_root/$family/$file") != 0 ) {
		print("PFAMRCS: Was unable to check in $file... hmm...\n");
		$out = 0;
	    }
	    system("chmod g+rxw $Pfam_rcs_root/$family/$file,v");
	}
	else {
	    if ( system("rcs -u -q $Pfam_rcs_root/$family/$file") != 0 ) {
		print("PFAMRCS: Was unable to release the lock on [$file] in [$family].. hmm...\n");
		$out = 0;
	    }
	    if (not unlink "$Pfam_rcs_root/$family/$file") {
		print("PFAMRCS: Was unable to remove $Pfam_rcs_root/$family/$file when releasing lock.\n");
		$out = 0;
	    }
	}
	    
    }

    if( ! -e "$Pfam_rcs_root/$family/locked" ) {
	warn("Bad news... you have checked in a family and I have lost my locked file. Ooops!");
    } else {
	unlink("$Pfam_rcs_root/$family/locked");
    }

    return $out;
}


    
#
# check_out_family called as check_out_family("egf",\*STDOUT);
#

sub check_out_family {
    my($family,$handle, $lockonly) = @_; 
    my $out = 1;
	
    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
		if( check_out_file($file,$family,$handle) == 0 ) {
	    	$out = 0;
		}
		if( -e "$Pfam_rcs_root/$family/$file" && $lockonly){
			unlink("$Pfam_rcs_root/$family/$file");
		}
    }
    foreach my $file (@Pfam_optional_file) {
	if( -e "$Pfam_rcs_root/$family/$file,v" ) {
	    if( check_out_file($file,$family,$handle) == 0 ) {
		$out = 0;
	    }
		if( -e "$Pfam_rcs_root/$family/$file" && $lockonly){
			unlink("$Pfam_rcs_root/$family/$file");
		}
	}
    }

    # make locked file

    open(LOC,">$Pfam_rcs_root/$family/locked") || die("Bad bug ... could not open locked file $!");

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

    if( !open(CHECK,"co -l -q $Pfam_rcs_root/$family/$file|") ) {
	print("PFAMRCS: Could not open co -l for $file of family $family, $!\n");
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

    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( system("cp $Pfam_current_root/$family/$file ./$file") != 0 ) {
	    $out = 0;
	}
	if( system("chmod u+w ./$file") != 0 ) {
	    $out = 0;
	}
    }

    foreach my $file (@Pfam_optional_file) {
	if( -e "$Pfam_rcs_root/$family/$file,v" ) {
	    if( system("cp $Pfam_current_root/$family/$file ./$file") != 0 ) {
		$out = 0;
	    }
	    if( system("chmod u+w ./$file") != 0 ) {
		$out = 0;
	    }
	}
    }
	    
    return $out;
}


sub diff_family {
    my $family = shift;
    my $oldrev = shift;
    my $newrev = shift;

    my @files;

    foreach my $file ( @Pfam_Family_File_Set, @Pfam_Annotation_File_Set ) {
	next if $file eq 'scores' or $file =~ /OUTPUT/;
 
	my $fh = &diff_file($family,$file,$oldrev,$newrev);
	my $seen = 0;
	while( <$fh>) {
	    $seen = 1;
	}
	$fh->close();
	# print STDERR "$file $seen $filestr\n";
	if( $seen == 1 ) {
	    if( $file eq 'ALIGN' ){
		push @files, "FULL";
	    } else {
		push @files, "$file";
	    }
	}
    }

    return @files;
}



#
# Provides a stream to a diff 
# between two versions of a file
#
# call as diff_file($family,$file,$oldrev,$newrev)
#
sub diff_file {
    my $family = shift;
    my $file = shift;
    my $oldrev = shift;
    my $newrev = shift;
    my(%revhash,$fh);

    if( !defined $newrev ) {
	warn("Call diff_file as diff_file($family,$file,$oldrev,$newrev)");
	return undef;
    }

    %revhash = &label_hash($family);
    if( !exists($revhash{$oldrev}) || !exists($revhash{$newrev}) ) {
	warn("family $family doesn't have $oldrev or $newrev - can't call diff");
	return undef;
    }

    
    $fh = new FileHandle;
    $fh->open("rcsdiff -q -r$oldrev -r$newrev $Pfam_rcs_root/$family/$file |");
    return $fh;
}
    


sub get_locked_families {
    my (@names);

    if( !opendir(_PFAM_INTERNAL_DIR,$Pfam_rcs_root) ) {
	print("PFAMRCS: A real bad problem, can't see Pfam root at $Pfam_rcs_root\n");
	return @names;
    }

    my @files = readdir _PFAM_INTERNAL_DIR;

    foreach my $file (@files) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if( !(-d "$Pfam_rcs_root/$file") ) {
	    print("PFAMRCS: I don't like this, a non directory file [$file] in the RCS_MASTER. Yuk!\n");
	}
	else {
	    if( -e "$Pfam_rcs_root/$file/locked" ) {
		push(@names,$file);
	    }
	}

    }

    return @names;
}



sub get_all_family_names {
    my (@names);

    if( !opendir(_PFAM_INTERNAL_DIR,$Pfam_rcs_root) ) {
	print("PFAMRCS: A real bad problem, can't see Pfam root at $Pfam_rcs_root\n");
	return @names;
    }

    my @files = readdir _PFAM_INTERNAL_DIR;

    closedir(_PFAM_INTERNAL_DIR);

    foreach my $file (@files) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if( !(-d "$Pfam_rcs_root/$file") ) {
	    print("PFAMRCS: I don't like this, a non directory file [$file] in the RCS_MASTER. Yuk!\n");
	}
	else {
	    push(@names,$file);
	}

    }

    return @names;
}


sub get_dead_family_names {
    my (@names);

    if( !opendir(_PFAM_INTERNAL_DIR,$Pfam_rcs_attic) ) {
	print("PFAMRCS: A real bad problem, can't see Pfam attic at $Pfam_rcs_attic\n");
	return @names;
    }

    my @files = readdir _PFAM_INTERNAL_DIR;

    closedir(_PFAM_INTERNAL_DIR);

    foreach my $file (@files) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if( !(-d "$Pfam_rcs_attic/$file") ) {
	    print("PFAMRCS: I don't like this, a non directory file [$file] in the RCS_ATTIC. Yuk!\n");
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


    if ( open(_LOCK, "<$Pfam_rcs_root/$family/locked")) {
	while(<_LOCK>) {
	    /^Lock file to indicate lock by user (\S+)/ && do {
		$locker = $1;
	    };
	}
    }


    if( !open(INFO,"rlog -b $Pfam_rcs_root/$family/DESC |") ) {
	print("PFAMRCS: can't get any info on family [$family]\n");
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
    


	
sub get_pfam_accession_lock {
    my ($me, $line, $name);

    #
    # returns "success" for success and "lockee" for failure... 
    #


    if( open(LOCK,"$pfam_acc_lock") ) {
	$line = <LOCK>;
	($name) = split (/:/,$line);
	return $name;
    } else {
	open(LOCK,">$pfam_acc_lock") || die "Could not open lock to write. Something is badly wrong. file [$pfam_acc_lock]";
	$me = `whoami`;
	chop $me;
	print LOCK "$me: has locked pfam\n";
	close(LOCK);
	system("chmod 0640 $pfam_acc_lock");
	$got_lock = 1;
	return "success";
    }

}



	
sub get_pfam_database_lock {
    my @allow = @_;
    my ($me, $line, $name);

    #
    # returns "success" for success and "lockee" for failure... 
    #


    if( open(LOCK,"$pfam_database_lock") ) {
	$line = <LOCK>;
	($name) = split (/:/,$line);
	return $name;
    } else {
	open(LOCK,">$pfam_database_lock") || die "Could not open lock to write. Something is badly wrong. file [$pfam_acc_lock]";
	$me = `whoami`;
	chop $me;
	print LOCK "$me: has locked pfam\n";
	foreach my $allowed (@allow) {
	    print LOCK "allow $allowed\n";
	}
	close(LOCK);
	system("chmod 0640 $pfam_database_lock");

	return "success";
    }

}




sub get_short_info_on_family {
    my($family,$fileout) = @_;
    my($line, $lasttouched, $lastauthor);

    my ($islocked, $locker) = &check_family_isnot_locked( $family ); 

    if( !open(INFO,"rlog -b $Pfam_rcs_root/$family/DESC |") ) {
	print("PFAMRCS: can't get any info on family [$family]\n");
    }

    while (<INFO>) {
	#if( /^locks/ ) {
	#    $line = <INFO>;
	#    chop $line;
	#    if( ($line =~ /^\s*(\w*):/) ) {
	#	$locked = $1;
	#	$islocked = 1;
	#    }
	#}
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


 
sub label_accession_mappings {
    my $label = shift;
    
    # copy every file in the accessions directory into the directory of
    # previous releases
    
    die "Directory of accession history $pfam_accession_attic_dir does not exist\n"
	if not -e "$pfam_accession_attic_dir";

    my $frozen_accessions = "$pfam_accession_attic_dir/$label";
    
    if (not -e $frozen_accessions) {
	system("mkdir $frozen_accessions; chmod 775 $frozen_accessions") 
	    and die "Could not make directory $frozen_accessions\n";
    }

    opendir(_ACC, $pfam_accession_dir) 
	or die "Could not archive accession directory $pfam_accession_dir\n";

    foreach my $file (readdir _ACC) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if (-f "$pfam_accession_dir/$file") {
	    system("cp -r $pfam_accession_dir/$file $frozen_accessions/$file")
		and die "Could not copy $file whilst archiving accession directory\n";
	}
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

    foreach my $file ( @Pfam_Annotation_File_Set, @Pfam_Family_File_Set ) {
	if( system("rcs -N$label: $Pfam_rcs_root/$family/$file") != 0 ) {
	    warn("Could not label $family/$file...");
	    $ret = 0;
	}
    }

    foreach my $file ( @Pfam_optional_file ) {
	if( ! -e "$Pfam_rcs_root/$family/$file,v" ) {
	    next;
	}

	if( system("rcs -N$label: $Pfam_rcs_root/$family/$file") != 0 ) {
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

    my $loc = ($dead)?$Pfam_rcs_attic:$Pfam_rcs_root;

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

    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( lock_file($file,$family) == 0 ) {
	    $out = 0;
	}
    }
    foreach my $file (@Pfam_optional_file) {
	if( -e "$Pfam_rcs_root/$family/$file,v" ) {
	    if( lock_file($file,$family) == 0 ) {
		$out = 0;
	    }
	}
    }

    # make locked file

    if (not $user) {
	$user = `whoami`; chop $user;
    }

    open(LOC,">$Pfam_rcs_root/$family/locked") || die("Bad bug ... could not open locked file $!");

    print LOC "Lock file to indicate lock by user $user\n";

    close(LOC);

    return $out;
}



sub lock_file {
    my($file,$family) = @_;

    open(T,"rcs -l -q $Pfam_rcs_root/$family/$file|") or
	warn("Major error - could not get lock for $family/$file");
    while(<T>) {
	;
    }

    return 1;
}



sub make_accession_from_rcs_label {
    my $rcs_label = shift;
    my $root_dir = shift;

    my $dest_dir = "$root_dir/ACCESSION";

    if (-e $dest_dir) {
	my $no_files;
	opendir(TMP, $dest_dir);
	foreach my $en (readdir TMP ) {
	    if( $en =~ /^\.$/ || $en =~ /^\.\.$/ ) { next; }
	    $no_files++;
	}
	
	die "PfamRCS::make_accession_from_rcs_label - you should make sure $dest_dir is empty first\n" 
	    if $no_files;
    }
    else {
	# create it
	system("mkdir $dest_dir; chmod 755 $dest_dir")
	    and die "PfamRCS::make_accession_from_rcs_label - Could not make $dest_dir\n";
    }

    my $release_accession_dir = "$pfam_accession_attic_dir/$rcs_label";

    opendir(_ACC, $release_accession_dir) 
	or die "Could not obtain accession mappings for release $rcs_label [$release_accession_dir]\n";

    foreach my $file (readdir _ACC) {

	if( $file =~ /^\.$/ or $file =~ /^\.\.$/ ) { next; }
	if (-f "$release_accession_dir/$file") {
	    system("cp $release_accession_dir/$file $dest_dir/$file")
		and die "Could not copy $file whilst restoring accession directory\n";
	}
    } 
}



sub make_attic_from_rcs_label {
    my $rcs_label = shift;
    my $root_dir = shift;

    my $dest_dir = "$root_dir/RCS_ATTIC";

    if (-e $dest_dir) {
	my $no_files;
	opendir(TMP, $dest_dir);
	foreach my $en (readdir TMP ) {
	    if( $en =~ /^\.$/ || $en =~ /^\.\.$/ ) { next; }
	    $no_files++;
	}
	
	die "PfamRCS::make_attic_from_rcs_label - you should make sure $dest_dir is empty first\n" 
	    if $no_files;
    }
    else {
	# create it
	system("mkdir $dest_dir; chmod 755 $dest_dir")
	    and die "PfamRCS::make_attic_from_rcs_label - Could not make $dest_dir\n";
    }

    # all dead families without the label were dead last time

    if( !opendir(_PFAM_ATTIC_DIR, $Pfam_rcs_attic) ) {
	print STDERR "PFAMRCS: A real bad problem, can't see Pfam root at $Pfam_rcs_root\n";
    }
    else {
	foreach my $fam (readdir _PFAM_ATTIC_DIR) {
	    if( $fam =~ /^\.$/ || $fam =~ /^\.\.$/ ) { next; }
	    if (-d "$Pfam_rcs_attic/$fam") {
		my %hash = label_hash( $fam, 1 );
		if (not $hash{$rcs_label}) {
		    print STDERR "Family $fam is still dead so restoring to attic\n";
		    # check out and move the family
		 	  
		    if( !(-d "$dest_dir/$fam") ) {
			mkdir "$dest_dir/$fam", 0775  || die "Could not make directory $dest_dir/$fam $!";
		    }
		    
		    system("cp $Pfam_rcs_attic/$fam/DEAD $dest_dir/$fam/DEAD") &&
			print STDERR "PFAMRCS:make_attic_from_rcs_label - could not cp $Pfam_rcs_attic/$fam/DEAD\n";
		    
		}
	    }
	}
    }
}	



# The following function assumes that a skeleton database 
# already exists in $dest_dir, so that the accession map
# can be queried

sub make_current_from_rcs_label {
    my $rcs_label = shift;
    my $root_dir = shift;

    my $dest_dir = "$root_dir/CURRENT";

    if (-e $dest_dir) {
	my $no_files;
	opendir(TMP, $dest_dir);
	foreach my $en (readdir TMP ) {
	    if( $en =~ /^\.$/ || $en =~ /^\.\.$/ ) { next; }
	    $no_files++;
	}
	
	die "PfamRCS::make_current_from_rcs_label - you should make sure $dest_dir is empty first\n" 
	    if $no_files;
    }
    else {
	# create it
	system("mkdir $dest_dir; chmod 755 $dest_dir")
	    and die "PfamRCS::make_current_from_rcs_label - Could not make $dest_dir\n";
    }

    my ($old_acc2id, $current_id2acc);
    eval {
	$current_id2acc = { reverse %{Bio::Pfam->default_db->_get_tied_accmap() } };
	$old_acc2id = Bio::Pfam->local_rcs_db( $root_dir )->_get_tied_accmap();
    };
    $@ and do {
	print STDERR "$@\n";
	# accmap info not available, so do what we can
	my $warn_string = "PFAMRCS:make_current_from_rcs_label ";
	$warn_string .= "could not get accmap data - some families may have names that ";
	$warn_string .= "were different at time of release $rcs_label\n";
	
	warn($warn_string);
	
	($current_id2acc, $old_acc2id) = (undef, undef);
    };

    die "PFAMRCS: A real bad problem, can't see Pfam root at $Pfam_rcs_root\n"
	if not opendir(_PFAM_CURRENT_DIR, $Pfam_rcs_root);

    foreach my $fam (readdir _PFAM_CURRENT_DIR) {
	if( $fam =~ /^\.$/ || $fam =~ /^\.\.$/ ) { next; }
	if (-d "$Pfam_rcs_root/$fam") {
	    my %hash = label_hash( $fam );
	    if ($hash{$rcs_label}) {
		print STDERR "Restoring $fam\n";
		# check out and move the family
		
		if( !(-d "$dest_dir/$fam") ) {
		    mkdir "$dest_dir/$fam", 0775  || die "Could not make directory $dest_dir/$fam $!";
		}
		foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
		    system ("co -q -r$rcs_label $Pfam_rcs_root/$fam/$file") &&
			die "PFAMRCS:make_current_from_rcs_label - Failed to check out $fam/$file\n";
		    
		    system("mv $Pfam_rcs_root/$fam/$file $dest_dir/$fam/$file") && 
			die "PFAMRCS: make_current_from_rcs_label - could not move $Pfam_rcs_root/$fam/$file\n";
		}
		foreach my $file (@Pfam_optional_file) {
		    if( -e "$Pfam_rcs_root/$fam/$file,v" ) {
			system ("co -q -r$rcs_label $Pfam_rcs_root/$fam/$file") && 
			    die "PFAMRCS:make_current_from_rcs_label - Failed to check out $fam/$file\n";
			
			system("mv $Pfam_rcs_root/$fam/$file $dest_dir/$fam/$file") &&
			    die "PFAMRCS:make_current_from_rcs_label - could not mv $Pfam_rcs_root/$fam/$file\n";
		    }
		}
		
		# at this point, we may need to rename the family to the name it had at the time 
		# of release $rcs_label. 
		
		if (defined $current_id2acc and defined $old_acc2id) {
		    if ($current_id2acc->{ $fam }) {
			my $acc = $current_id2acc->{ $fam };
			my $old_fam = $old_acc2id->{ $acc };
			
			if (not $old_fam) {
			    die "It seems there was no entry for $acc at $rcs_label, weird.\n";
			}
			elsif ($old_fam ne $fam) {
			    print STDERR "   Restoring the old name $old_fam...\n";	  	   
			    system("mv $dest_dir/$fam $dest_dir/$old_fam") &&
				die "PFAMRCS:make_current_from_rcs_label - could not rename $dest_dir/$fam $dest_dir/$old_fam\n";
			} 
		    }
		    else {
			die "PFAMRCS:make_current_from_rcs_label - Could not get accession number for family $fam\n";
		    }		    		    
		}
	    }
	}
    }
    
    die "PFAMRCS: A real bad problem, can't see Pfam attic at $Pfam_rcs_attic\n"
	if not opendir(_PFAM_ATTIC_DIR,$Pfam_rcs_attic);

    foreach my $fam (readdir _PFAM_ATTIC_DIR) {
	if( $fam =~ /^\.$/ || $fam =~ /^\.\.$/ ) { next; }
	if (-d "$Pfam_rcs_attic/$fam") {
	    my %hash = label_hash( $fam, 1 );
	    if ($hash{$rcs_label}) {
		print STDERR "Resurrecting $fam...\n";	  	   
		# check out and move the family
		
		if( !(-d "$dest_dir/$fam") ) {
		    mkdir "$dest_dir/$fam", 0775  || die "Could not make directory $dest_dir/$fam $!";
		}
		foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
		    system ("co -q -r$rcs_label $Pfam_rcs_attic/$fam/$file") &&
			    print STDERR "Failed to check out $fam/$file\n";
		    
			system("mv $Pfam_rcs_attic/$fam/$file $dest_dir/$fam/$file") && 
			    print STDERR "PFAMRCS: make_current_from_rcs_label - could not move $Pfam_rcs_attic/$fam/$file\n";
		}
		foreach my $file (@Pfam_optional_file) {
		    if( -e "$Pfam_rcs_root/$fam/$file,v" ) {
			system ("co -q -r$rcs_label $Pfam_rcs_attic/$fam/$file") &&
			    die "PFAMRCS:make_current_from_rcs_label - Failed to check out $fam/$file\n";
			
			system("mv $Pfam_rcs_attic/$fam/$file $dest_dir/$fam/$file") &&
			    die "PFAMRCS:make_current_from_rcs_label - could not mv $Pfam_rcs_attic/$fam/$file\n";
			
		    }
		}

		# at this point, we may need to rename the family to the name it had at the time 
		# of release $rcs_label. 
		
		if (defined $old_acc2id) {
		    # difficult (although not impossible) to get the accession number
		    # of the dead family from the accmap. Look for the accession in the 
		    # DEAD file instead

		    open(_DEAD, "$Pfam_rcs_attic/$fam/DEAD") 
			or die "PFAMRCS:make_current_from_rcs_label - Could not fine DEAD file for dead family $fam\n";
		    
		    my $acc_of_fam;
		    while(<_DEAD>) {
			/^AC\s+(PF\d+)/ and do {
			    $acc_of_fam = 1;
			    last;
			};
		    }
			
		    my $old_fam = $old_acc2id->{ $acc_of_fam };
			
		    if (not $old_fam) {
			die "It seems there was no entry for $acc_of_fam at $rcs_label, weird.\n";
		    }
		    elsif ($old_fam ne $fam) {
			print STDERR "   Restoring the old name $old_fam...\n";	  	   
			system("mv $dest_dir/$fam $dest_dir/$old_fam") &&
			    die "PFAMRCS:make_current_from_rcs_label - could not rename $dest_dir/$fam to $dest_dir/$old_fam\n";
		    } 
		}
		else {
		    die "Could not get accession number for family $fam\n";
		}
	    }
	}	
    }	
}




sub make_new_rcs_directory {
    my ($newfamily) = @_;
    my($out);

    $out = 1;

    if( mkdir("$Pfam_rcs_root/$newfamily", 0775) == 0 ) {
	print("PFAMRCS: Could not make new Pfam rcs directory for [$newfamily] due to $!\n");
	$out = 0;
    }

    if( mkdir("$Pfam_current_root/$newfamily", 0775) == 0 ) {
	print("PFAMRCS: Could not make new Pfam current directory for [$newfamily] due to $!\n");
	$out = 0;
    }
    system("chmod a+rxw $Pfam_rcs_root/$newfamily");
    system("chmod a+rxw $Pfam_current_root/$newfamily");

    return $out;
}



sub make_new_rcs_files {
    my ($family,$comment) = @_;
    my $out = 1;


    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( system("rcs -a$allowed_user_string -q -i -t-\"$comment\" $Pfam_rcs_root/$family/$file") != 0 ) {
	    print("PFAMRCS: Was unable make new file $file... hmm...\n");
	    $out = 0;
	}
    }
    foreach my $file (@Pfam_optional_file) {
	if( !(-e "$Pfam_rcs_root/$family/$file") ) {next;}

	if( system("rcs -a$allowed_user_string -q -i -t-\"$comment\" $Pfam_rcs_root/$family/$file") != 0 ) {
	    print("PFAMRCS: Was unable make new file $file... hmm...\n");
	    $out = 0;
	}
    }


    return $out;
}

sub make_output_from_rdb_dump {
  my($family, $pfamseqsize, $dir) = @_;
    my $rdb = Bio::Pfam->live_rdb();      # only need both of these cause

    my(@regions);; 
    open(_FILE, "$dir/$family") or die "Cant open: $dir/$family as $! \n" ;
    while(<_FILE>) {
      chop($_);
      my($pfamA_acc,$seqid, $st, $en, $mo_st, $mo_en, $bits, $ev, $seq_bits, $seq_eval, $mode) = split(/\t/, $_);
      	   push @regions,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION'  => $pfamA_acc,
						   '-PFAM_ID'         => $family,
						   '-SEQ_ID'          => $seqid,
						   '-FROM'            => $st,
						   '-TO'              => $en,
						   '-MODEL_FROM'      => $mo_st,
						   '-MODEL_TO'        => $mo_en,
						   '-BITS'            => $bits,
						   '-EVALUE'          => $ev,
						   '-MODE'            => $mode,
						   '-SEQUENCE_BITS'   => $seq_bits,
						   '-SEQUENCE_EVALUE' => $seq_eval,
						   );
      
    }
    close(_FILE);

    my $rcs = Bio::Pfam->default_db();    # rdb doesn't have mu and lambda
    my $entrya = $rdb -> get_EntryA_by_id( $family );
    my $tempen = $rcs -> get_EntryA_by_id( $family );
    #my @regions = $entrya -> all_full_annotated_regions();

    my $res_ls = new HMMResults;
    my $res_fs = new HMMResults;

    foreach my $region ( @regions ) {
	my $res;
	if( $region->mode() eq "ls" ) {
	    $res = $res_ls;
	}
	elsif( $region->mode() eq "fs" ) {
	    $res = $res_fs;
	}
	else {
	    die( "can't work out the mode of your region" );
	}
	my( $seqpfam ) = $rdb -> get_Seq_pfamseq( $region -> pfamseq_id, 'id', 0 );
	unless( $res -> getHMMSequence( $seqpfam -> acc ) ) {
	    my $hmmseq = new HMMSequence;
	    $hmmseq ->   bits( $region -> sequence_bits_score );
	    $hmmseq -> evalue( $region -> sequence_evalue_score );
	    $hmmseq ->   name( $seqpfam -> acc );  # key off pfamseq accession
                                                   # may be a more efficient way of getting this without 2nd rdb call
                                                   # eg. add pfamseq_acc to Pfam::PfamRegion or something
	    $hmmseq ->   desc( $region->pfamseq_id." ".$seqpfam->desc );  
	    $res -> addHMMSequence( $hmmseq );
	}
	
	my $hmmunit = new HMMUnit;
	$hmmunit ->      bits( $region -> bits_score );
	$hmmunit ->    evalue( $region -> evalue_score );
	$hmmunit ->   seqname( $seqpfam -> acc );
	$hmmunit -> start_seq( $region -> from );
	$hmmunit ->   end_seq( $region -> to );
	$hmmunit -> start_hmm( $region -> model_from );
	$hmmunit ->   end_hmm( $region -> model_to );

	$res -> addHMMUnit( $hmmunit );
    }

    $res_ls -> calculate_evalues( $tempen->ls_mu, $tempen->ls_kappa, $pfamseqsize );
    $res_fs -> calculate_evalues( $tempen->fs_mu, $tempen->fs_kappa, $pfamseqsize );

    open( O, ">PFAMOUT_ls" ) or die;
    $res_ls -> write_pfam_output( \*O );
    close O;
    open( O, ">PFAMOUT_fs" ) or die;
    $res_fs -> write_pfam_output( \*O );
    close O;

}

sub make_output_from_rdb {
    my $family = shift;
    my $rdb = Bio::Pfam->live_rdb();      # only need both of these cause
    my $rcs = Bio::Pfam->default_db();    # rdb doesn't have mu and lambda

    my $pfamseqsize = $rcs->pfamseq_dbsize;

    my $entrya = $rdb -> get_EntryA_by_id( $family );
    my $tempen = $rcs -> get_EntryA_by_id( $family );
    my @regions = $entrya -> all_full_annotated_regions();

    my $res_ls = new HMMResults;
    my $res_fs = new HMMResults;

    foreach my $region ( @regions ) {
	my $res;
	if( $region -> mode eq "ls" ) {
	    $res = $res_ls;
	}
	elsif( $region -> mode eq "fs" ) {
	    $res = $res_fs;
	}
	else {
	    die( "can't work out the mode of your region" );
	}

	unless( $res -> getHMMSequence( $region -> pfamseq_acc ) ) {
	    my $hmmseq = new HMMSequence;
	    $hmmseq ->   bits( $region -> sequence_bits_score );
	    $hmmseq -> evalue( $region -> sequence_evalue_score );
	    $hmmseq ->   name( $region -> pfamseq_acc );
	    $hmmseq ->   desc( $region->pfamseq_desc );
	    $res -> addHMMSequence( $hmmseq );
	}
	
	my $hmmunit = new HMMUnit;
	$hmmunit ->      bits( $region -> bits_score );
	$hmmunit ->    evalue( $region -> evalue_score );
	$hmmunit ->   seqname( $region -> pfamseq_acc );
	$hmmunit -> start_seq( $region -> from );
	$hmmunit ->   end_seq( $region -> to );
	$hmmunit -> start_hmm( $region -> model_from );
	$hmmunit ->   end_hmm( $region -> model_to );

	$res -> addHMMUnit( $hmmunit );
    }

    $res_ls -> calculate_evalues( $tempen->ls_mu, $tempen->ls_kappa, $pfamseqsize );
    $res_fs -> calculate_evalues( $tempen->fs_mu, $tempen->fs_kappa, $pfamseqsize );

    open( O, ">PFAMOUT_ls" ) or die;
    $res_ls -> write_pfam_output( \*O );
    close O;
    open( O, ">PFAMOUT_fs" ) or die;
    $res_fs -> write_pfam_output( \*O );
    close O;

}


sub make_view_files {
    my $family = shift;

    # if the files are alreadt there, delete them

    my $wc = `wc -l $Pfam_current_root/$family/ALIGN`;
    $wc =~ /(\d+)/;
    $wc= $1;

    my $high_mem;
    if($wc > 20000) { #if ALIGN has > 20000 seq then submit to high memory queue
       print STDERR "Family has greater than 20,000 sequences - submitting view files to bigmem queue\n"; 
       $high_mem = 1;
    }

    foreach my $file ( @Pfam_View_File_Set ) {
	if( -e "$Pfam_current_root/$family/$file" ) {
	    unlink("$Pfam_current_root/$family/$file");
	}
    }

    umask 002;
    open(TODO,">$Pfam_current_root/$family/todo.view") || die "Could not open todo.view file for $family";
    my $me = `whoami`;
    chomp $me;
    print TODO "Set off for $family by $me\n";
    close(TODO);
    
    # get lock on family

    &lock_family($family, "VIEW");

    # submit makeview
    if($high_mem) {
	system("bsub -q bigmem -R'select[type==any && mem>7000]rusage[mem=7000]' -o /dev/null $view_maker $family");
    }
    else {
      system("bsub -q pfam_slow -Ralpha -o /dev/null $view_maker $family");
    }

    # when the view files have finished being built, the lock will be released

    return 1;
}



sub move_family_to_current {
    my ($family) = @_;
    my $out = 1;

    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( system("mv $Pfam_rcs_root/$family/$file ./$file") != 0 ) {
	    $out = 0;
	}
    }

    foreach my $file (@Pfam_optional_file) {
	if( -e "$Pfam_rcs_root/$family/$file" ) {
	    if( system("mv $Pfam_rcs_root/$family/$file ./$file") != 0 ) {
		$out = 0;
	    }
	}
    }
	    

    return $out;

}



sub move_files_to_rcs_directory {
    my ($family,$fromdir) = @_;
    my $out = 1;

    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( system("cp $fromdir/$file $Pfam_rcs_root/$family/$file") != 0 ) {
	    $out = 0;
	}
    }

    foreach my $file (@Pfam_optional_file ) {
	if( -e "$fromdir/$file") {
	    if( system("cp $fromdir/$file $Pfam_rcs_root/$family/$file") != 0 ) {
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

sub move_rcs_directory {
    my $from = shift;
    my $to   = shift;
    my (%temphash,$acc,$name, $refhash);

    # get out a copy of the database for the accession
    # numbers and lock it.

    
    if( -d "$Pfam_rcs_root/$to" ) {
	print("PFAMRCS: Directory $Pfam_rcs_root/$to exists. Cannot move!");
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

    if( rename("$Pfam_rcs_root/$from","$Pfam_rcs_root/$to") == 0 ) {
	print("PFAMRCS: Very bad error. Please talk to adminstator. Unable to move RCS $from to $to $!");
	return 1;
    }

    
    if( system("rm -r -f $Pfam_current_root/$from") ){
	print("PFAMRCS: Very bad error. Please talk to adminstator. Unable to remove current $from$!");
	return 1;
    }

    if( mkdir("$Pfam_current_root/$to", 0775) == 0 ) {
	print("PFAMRCS: Could not make new Pfam current directory for [$to] due to $!\n");
	return 1;
    }

    system("chmod a+rxw $Pfam_current_root/$to");

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

    if( unlink("$pfam_acc_lock") == 0 ) {
	warn("Bad news: could not release lock test! Please talk to ewan");
	return undef;
    }
    $got_lock = 0;
    return 1;
}


	
sub release_pfam_database_lock {

#    if( $got_lock != 1 ) {
#	warn("Cannot release lock when you don't have it!");
#	return undef;
#    }

    if( unlink("$pfam_database_lock") == 0 ) {
	warn("Bad news: could not release database lock");
	return undef;
    }

    return 1;
}
    


sub remove_users_from_family {
    my $family = shift;
    my @users = @_;

    my $user = join(',', @users);

    foreach my $file ( @Pfam_Family_File_Set, @Pfam_Annotation_File_Set ) {
	if( ! -e "$Pfam_rcs_root/$family/$file,v" ) {
	    warn("Bad error - for family $family no $file in RCS master!");
	} else {
	    if( system("rcs -e$user $Pfam_rcs_root/$family/$file,v") ) {
		warn("Bad error - unable to -e$user on $family/$file");
	    }
	}
    }
    foreach my $file ( @Pfam_optional_file ) {
	if( ! -e "$Pfam_rcs_root/$family/$file,v" ) {
	    next;
	} else {
	    if( system("rcs -e$user $Pfam_rcs_root/$family/$file,v") ) {
		warn("Bad error - unable to -e$user on $family/$file");
	    }
	}
    }
}



sub remove_users_from_system {
    my @users = @_;

    my $found;

    open(ACCESS,"$pfam_access_list") || die "Could not open $pfam_access_list ($!) A v. bad error";
    open(ACCESSTEMP,">$pfam_access_temp") || die "Could not open $pfam_access_temp ($!) A v. bad error";

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

    if (not rename($pfam_access_temp,$pfam_access_list)) {
	warn "PfamRCS::remove_user_from_system - could not rename access_list file!";
    }
}






   
sub show_pfamrcs_help {
    my($out) = @_;

    print $out <<"EOF";

Pfam RCS system (written by Ewan Birney)

USAGE - Program [Domain1] [Domain2] [Domain3] etc
Programs:
      pfco                      Check out (with lock) a family
      pfco -notoutput			Check out (with lock) a family, but no output files
      pfco -onlylock			Check out (with lock) a family, no files or directory
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

    foreach my $file (@Pfam_Family_File_Set) {
	if( update_file_label($file,$family,$handle,$label) == 0 ) {
	    $out = 0;
	}
    }
    foreach my $file (@Pfam_optional_file) {
	if( -e "$Pfam_rcs_root/$family/$file,v" ) {
	    if( update_file_label($file,$family,$handle,$label) == 0 ) {
		$out = 0;
	    }
	}
    }

    return $out;
}


sub update_file_label {
    my ($file,$family,$handle,$label) = @_;

    if( !open(CHECK,"co -r$label  -q $Pfam_rcs_root/$family/$file|") ) {
	print("PFAMRCS: Could not open co -l for $file of family $family, $!\n");
	return 0;
    }

    while(<CHECK>) {
	print $handle;
    }

    close(CHECK);
    return 1;
}
    


sub update_current_directory {
    my($family, $annotonly) = @_;
    my($out);

    $out = 1;

    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( system("co -q $Pfam_rcs_root/$family/$file") != 0 ) {
	    print("PFAMRCS: For file $file of family $family,\n could not check (unlocked) out for update\n");
	    $out = 0;
	}
	if( system("mv -f $Pfam_rcs_root/$family/$file $Pfam_current_root/$family/$file") != 0 ) {
	    print("PFAMRCS: For file $file of family $family,\ncould not move file from RCS to current for update\n");
	    $out = 0;
	}
    }

    foreach my $file (@Pfam_optional_file) {
	if( !(-e "$Pfam_rcs_root/$family/$file,v") ) { 
	    next; 
	}
	if( system("co -q $Pfam_rcs_root/$family/$file") != 0 ) {
	    print("PFAMRCS: For file $file of family $family,\n could not check (unlocked) out for update\n");
	    $out = 0;
	}
	if( system("mv -f $Pfam_rcs_root/$family/$file $Pfam_current_root/$family/$file") != 0 ) {
	    print("PFAMRCS: For file $file of family $family,\ncould not move file from RCS to current for update\n");
	    $out = 0;
	}
    }

    # ok - fine.

    return $out;
}



sub user_has_locked_family {
    my ($family,$user) = @_;
    my ($islocked,$locked);


    ($islocked,$locked) = &check_family_isnot_locked($family);

    if( !$islocked) {
	print("PFAMRCS: The family [$family] has no lock on it, certainly user $user cannot!\n");
    }

    if( $locked =~ /^\s*$user\s*/ ) {
	return (1,$locked);
    }
    else {
	return (0,$locked);
    }
}

#
# check_family_acc_vs_id
#

sub check_family_acc_vs_id {
	my $family = shift;
	my $db = Bio::Pfam->default_db();
	open(DESC, "$family/DESC") || die "Could not open DESC file:[$!]\n";	
	my $desc_acc;
	while(<DESC>){
		if(/^AC\s+(PF\d+)/){
		$desc_acc = $1;
		last;
		}
	}
	close(DESC);
	warn "Checking $desc_acc against family id....\n";	
	my $rcs_acc = $db->id2acc($family);
	my $rcs_family = $db->acc2id($desc_acc);
	
	if($rcs_acc ne $desc_acc){
		warn "$family acc:$desc_acc does not match rcs acc:$rcs_acc\n";
		return 0;
	}elsif ($rcs_family ne $family){
		warn "$family does not match rcs family name [expected $rcs_family]\n";
		return 0;
	}else{
		return 1;
	}


}



sub get_current_version {
    my $family = shift;
    my $file = shift;
    
    if( !open(INFO,"rlog -r $Pfam_rcs_root/$family/$file |") ) {
	print("PFAMRCS: can't get any info on family [$family]\n");
    }
    my $version;
    while(<INFO>){
	if(/^revision\s(\d+\.\d+)/){
	    $version = $1;
	    last;
	}
    }
    return $version;
}
#
# tell require things that we are ok
#

1;














