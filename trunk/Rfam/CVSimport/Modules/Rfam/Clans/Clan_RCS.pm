BEGIN {
    # DB_File isn't always available
    # if it isn't then we don't want to bomb
    if( eval "use DB_File" ) {
        warn "DB_File isn't available.\n";
    }
}


package Rfam::Clans::Clan_RCS;

#methods in this package
# new
# id_exists
# get_allacc
# acc2id
# allocate_new_clan_accession
# make_new_rcs_directory
# move_files_to_rcs_directory
# make_new_rcs_files
# check_in_new_rcs_files
# update_current_directory
# lock_clan
# unlock_clan
# check_clan_is_not_locked
# user_has_locked_clan
# check_out_clandesc
# move_clandesc_to_local
# get_locked_families
# get_info_on_clan
# get_short_info_on_clan
# _add_accession
# _get_lock
# _unlock
# _get_tied_accmap
# _accmap_cache

use strict;
use Fcntl;
use Rfam;
use FileHandle;

sub new {
    my ($caller, %params) = @_;
    my $self = bless {}, ref($caller) || $caller;
    
    my( $clan_cur, $clan_index, $lock_file ) =
	( ($params{'-CLAN_CURRENT'}   || $params{'-clan_current'}),
	  ($params{'-CLAN_INDEX'}     || $params{'-clan_index'}) ,
	  ($params{'-LOCK_FILE'}      || $params{'-lock_file'})  );

    $self->{'clan_dir'} = $clan_cur;
    $self->{'clan_index'}=$clan_index;    
    $self->{'rcsdb_lock'}=$lock_file;     
    
    if ( ! -s $clan_index){
	die "Clan index file empty\n"; #first time round I needed to initate this
    } 
    
    if( tie( my %testtie,'DB_File', $clan_index, O_RDONLY,0666) == 0){
     	die("Could not open $clan_index as a DB_file [$!] [$?]");           
 	untie(%testtie);	
    }
    return $self;
}


# can probably compress these down...
#checks the accmap for the given id.
sub id_exists {
    my $id = shift;
    my $clandb = Rfam::clan_db(); #new clan_rcs obj
    my $accmap = $clandb->_get_tied_accmap(); #checks up the accmap
    my %idmap  = reverse %{$accmap};
    if( exists $idmap{$id} ) {
	return $idmap{$id};
    }
    else {
	return 0;
    }
}

#looks up the accmap to get all the accessions
sub get_allacc {
    my $self   = shift;
    my $accmap = $self->_get_tied_accmap();

    my @acc;
    foreach my $acc ( sort keys %{$accmap} ) {
	if( $accmap->{$acc} =~ /\$DEAD_FAMILY_STRING/ ) {
	    next;
	}
	push( @acc, $acc );
    }
    return @acc;
}

#looks up the accmap and gets the id for acc
sub acc2id {
    my $self = shift;
    my $acc  = shift;
    
    my $accmap = $self->_get_tied_accmap();
    my $id = $accmap->{$acc};

    if( not defined $id ) {
	die "Rfam::Clan::Clan_RCS: accession [$acc] doesnot exist";
    }
    return $id;
}


#1 allocate accession and update the accmap
#this locks the RDB while the accmap is updated and then unlocks it again.
#the only bit of this that does anything serious is the update accmap- which is a DB_File hash. acclog is just a simple flat file.
sub allocate_new_clan_accession {
    my ($id) = shift;
 
   if( not defined $id && length($id)==0) {
 	warn("Cannot allocate a new family without a name");
 	return undef;
    }

    my $clandb = Rfam::clan_db();
     if( my $name = $clandb->_get_lock() ) {
         # places a locked file in ACCESSION with your name in it and sets flag on the db obj self->{is locked}=1
         die "Unable to get the lock for the accmap.dat (SDBM_file) - $name has it\n";
     }
    
    #simple text file with clan acc and creation date
    my $clan_acclog_file   = "$Rfam::clan_acclog_file";
    open(ACCLOG,"< $clan_acclog_file") || die "Could not open $clan_acclog_file ($!) A v. bad error\n[$!]";
    open(ACCTEMP,">$clan_acclog_file.$$") || die "Could not open $clan_acclog_file.$$ ($!) A v. bad error\n[$!]";
    
    my $line = "CL00000 [dummy] [pg5] [Tue Oct 20 15:59:05 2009]\n"; #Initialise with a dummy CL00000 entry 
    while( <ACCLOG> ) {
 	print ACCTEMP;
 	$line = $_;
    }
    
    ($line =~ /^(\S{2}\d+)/) || die "Last line of acclog is not an accession ($_). Big trouble!";    
    my $acc = $1;
    $acc++;
    
    my $me = `whoami`;
    chomp $me;
    my $date = gmtime();

    print ACCTEMP "$acc [$id] [$me] [$date]\n";
    print STDERR  "$acc [$id] [$me] [$date]\n";
    
    close(ACCLOG);
    close(ACCTEMP);
    
    rename("$clan_acclog_file.$$",$clan_acclog_file) or die "FATAL: failed to rename [$clan_acclog_file.$$] as [$clan_acclog_file]\n[$!]";
    # update the accmap.dat DB_File hash 
    $clandb->_add_accession( $acc, $id );
    $clandb->_unlock();
    
    return "$acc";
}

#2 make new rcs dir in clan master
sub make_new_rcs_directory {
    my ($clan) = @_;
    my $out=1;

    if( mkdir("$Rfam::clan_rcs_dir/$clan", 0775) == 0 ) {
	print STDERR ("RCS: Could not make new rcs directory for [$clan] due to $!\n");
	$out=0;
    }
    
    if( mkdir("$Rfam::clan_dir/$clan", 0775) == 0 ) {
	print STDERR ("RCS: Could not make new current directory for [$clan] due to $!\n");
	$out=0;
    }
    
    system("chmod a+rxw $Rfam::clan_rcs_dir/$clan");
    system("chmod a+rxw $Rfam::clan_dir/$clan");
    
    return $out;
}


#3 copy new local files to rcs directory
sub move_files_to_rcs_directory{
    my ($clan, $fromdir)=@_;
    my $out=1;

    if( system("cp $fromdir/CLANDESC $Rfam::clan_rcs_dir/$clan/CLANDESC") != 0 ) {
	print STDERR ("RCS: Could notcopy the local file for [$clan] due to $!\n");
	$out=0;
    }

    return $out;
}

#4 make new rcs files in rcs master
sub make_new_rcs_files{
    my ($clan,$comment) = @_;
    my $out = 1;
    
    my $access_list   = "$Rfam::accession_dir/access_list";
    my $allowed_user_string;

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

    if( system("rcs -a$allowed_user_string -q -i -t-\"$comment\" $Rfam::clan_rcs_dir/$clan/CLANDESC") != 0 ) {
	print STDERR ("RCS: Was unable make new file CLANDESC... hmm...\n");
	$out=0;
    }
    return $out;
}


#5 check files into the rcs system
sub check_in_new_rcs_files {
    my($clan,$log) = @_;
    my $new_rcs;
    my $out = 1;

    
    if( system("ci -f -q -m\"$log\" $Rfam::clan_rcs_dir/$clan/CLANDESC") != 0 ) {
	print STDERR ("RCS: Was unable to check in CLANDESC... hmm...\n");
	$out=0;
    }
    return $out;
}


#6 update clan current dir with the new clan/updated clan
sub update_current_directory{
    my($clan) = @_;
    
    my $out = 1;

   if( system("co -q $Rfam::clan_rcs_dir/$clan/CLANDESC") != 0 ) {
	print STDERR ("RCS: For CLANDESC of clan $clan,\ncould not check (unlocked) out for update\n");
	$out=0;
    }
    if( system("mv -f $Rfam::clan_rcs_dir/$clan/CLANDESC $Rfam::clan_dir/$clan/CLANDESC") != 0 ) {
	print STDERR ("RCS: For CLANDESC of clan $clan,\ncould not move file from RCS to current for update\n");
	$out=0;
    }
    if( system("chmod a-x $Rfam::clan_dir/$clan/CLANDESC") != 0 ) {
	print STDERR "failure to chmod the co file CLANDESC";
	$out=0;
    }

    return $out;
}

 
#called by ci/new code to lock given clan while rcs calls made-simple file in clan dir in RCS_clan 
sub lock_clan{
    my ($acc, $tag)=@_;
    open(LOCK,">$Rfam::clan_rcs_dir/$acc/locked") or die "unable to open lock file $Rfam::clan_rcs_dir/$acc/locked  for $acc\n";
    print LOCK "$tag";
    close(LOCK);
}

#as it says on the tin - removes the lock file from the clan dir
sub unlock_clan{
    my ($acc)=@_;

     if( ! -e "$Rfam::clan_rcs_dir/$acc/locked" ) {
	warn("Bad news... you have checked in a family and I have lost my locked file. Ooops!");
    } else {
	unlink("$Rfam::clan_rcs_dir/$acc/locked");
    }
    
}

#checks for a lock file in the clan directory and in the rcs ,v file for a user lock tag
sub check_clan_is_not_locked {
    my ($clan) = @_;
    my ($locked, $locker);
    my $islocked = 0;

    if ( open(_LOCK, "<$Rfam::clan_rcs_dir/$clan/locked")) {
	while(<_LOCK>) {
	    /^Lock file to indicate lock by user (\S+)/ && do {
		$locker = $1;
	    };
	}
    }

    if (defined($locker) && length($locker)>1) {
	$islocked = 1;
	$locked = "$locker";
    }

    #also check the ,v file for lock-as this is the real rcs lock.
    open(RLOG,"rlog -L -h $Rfam::clan_rcs_dir/$clan/CLANDESC|") || die "Could not open rlog pipe for $clan!";

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


sub user_has_locked_clan {
    my ($clan,$user) = @_;
    my($islocked,$locked) = &check_clan_is_not_locked($clan);

    if( !$islocked) {
	print("RCS: The clan [$clan] has no lock on it -- user $user cannot unlock!\n");
    }

    if( defined($user) && $locked =~ /^\s*$user\s*/ ) {
	return (1,$locked);
    }
    else {
	return (0,$locked);
    }
}

#latest version of file written to the rcs_clan dir
sub check_out_clandesc {
    my ($clan,$handle) = @_;
    my $out=1;

   if( !open(CHECK,"co -l -q $Rfam::clan_rcs_dir/$clan/CLANDESC|") ) {
	print("RCS: Could not open co -l for CLANDESC of family $clan, $!\n");
	return 0;
   }

   while(<CHECK>) {
	print $handle;
   }

   close(CHECK);

    my $user=`whoami`; chop $user;
    my $tag="Lock file to indicate lock by user $user\n";
    &lock_clan($clan,$tag);

    return $out;
}

#makes a local copy of the file after a rcs check out call made
sub move_clandesc_to_local {
    my ($clan) = @_;
    my $out = 1;

    if( system("mv $Rfam::clan_rcs_dir/$clan/CLANDESC ./CLANDESC") != 0 ) {
	$out = 0;
    }
    if( system("chmod a-x ./CLANDESC") != 0 ) {
	$out = 0;
    }
    return $out;
}

#for rfclinfo
sub get_locked_families {
    my (@names);
    if( !opendir(_CLAN_RCS,$Rfam::clan_rcs_dir) ) {
	print("RCS: A real bad problem, can't see root at $Rfam::clan_rcs_dir\n");
	return @names;
    }

    my @files = readdir _CLAN_RCS;

    foreach my $file (@files) {
	if( $file =~ /^\.$/ || $file =~ /^\.\.$/ ) { next; }
	if( !(-d "$Rfam::clan_rcs_dir/$file") ) {
	    print("RCS: I don't like this, a non directory file [$file] in the CLAN_RCS. Yuk!\n");
	}
	else {
	    if( -e "$Rfam::clan_rcs_dir/$file/locked" ) {
		push(@names,$file);
	    }
	}

    }

    return @names;
}

#for rfclinfo.pl
sub get_info_on_clan {
    my($clan,$fileout) = @_;
    my($line, $locker);
    my $locked = "";

    if ( open(_LOCK, "<$Rfam::clan_rcs_dir/$clan/locked")) {
	while(<_LOCK>) {
	    /^Lock file to indicate lock by user (\S+)/ && do {
		$locker = $1;
	    };
	}
    }

    if( !open(INFO,"rlog -b $Rfam::clan_rcs_dir/$clan/CLANDESC |") ) {
	print("RCS: can't get any info on clan [$clan] in $Rfam::rcs_clan_dir/$clan\n");
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

#for rfclinfo.pl
sub get_short_info_on_clan {
    my($clan,$fileout) = @_;
    my($line, $lasttouched, $lastauthor);

    my ($islocked, $locker) = &check_clan_is_not_locked( $clan );
    my $db = Rfam::clan_db();
    my $id=&acc2id($db, $clan);

    if( !open(INFO,"rlog -b $Rfam::clan_rcs_dir/$clan/CLANDESC |") ) {
	print("RCS: can't get any short info on family [$clan]\n");
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
	print $fileout sprintf("[%-7s] [%-15s] Revision $lasttouched [%-10s] Locked [$locker]\n",$clan,$id,$lastauthor);
    } else {
	print $fileout sprintf("[%-7s] [%-15s] Revision $lasttouched [%-10s]\n",$clan,$id,$lastauthor);
    }

}

####################################
#internal methods
####################################

sub _add_accession {
   my ($self,$acc,$id) = @_;
   my ($obj,%accmap);

   if( $self->{'is_locked'} != 1 ) {
       die("Have not locked the database - cannot add an accession!");
   }

   # now get and tie the hash, saving the object   
   if( ($obj = tie( %accmap, 'DB_File' ,$self->{'clan_index'}, O_RDWR,0666)) == 0){
       die("Could not open $self->{'clan_index'} as a DB_file $!");
       
   }

   while ((my $k, my $v) = each %accmap)
      { print "$k -> $v\n" ;}

   if( exists $accmap{$acc} ) {
       die("$acc already exists in the database");
   }

   $accmap{$acc} = $id;
   $obj->sync();
   undef $obj;
   untie(%accmap);
}


##############################################

sub _get_lock{
   my ($self) = @_;
   my ($line, $name, $obj, %accmap, $index_file);

   my ($me) = `whoami`;
   chomp($me);

   if( !-e $self->{'rcsdb_lock'} ) {
       # get the lock now!
       open( _LOCK_, ">".$self->{'rcsdb_lock'} ) || die "Could not open lock file $!";
       print _LOCK_ "$me has locked the database\n";
       close(_LOCK_);
       $self->{'is_locked'} = 1;

       return undef;
   } else {
       open( _LOCK_, $self->{'rcsdb_lock'} ) || die "Could not open lock file $!";
       $line = <_LOCK_>;
       $line =~ /(\S+)/;
       $name = $1;
       close(_LOCK_);
       return $name;
   }
}


sub _unlock{
   my ($self,@args) = @_;
   my ($key);

   if( $self->{'is_locked'} != 1 ) {
       die("Trying to release the lock when you haven't got it!");
   }

   if( ! -e $self->{'rcsdb_lock'} ) {
       die("Bad error - you have the lock but there is no lock file!");
   }

   unlink($self->{'rcsdb_lock'});
   $self->{'is_locked'} = 0;

   return 0;
}


sub _get_tied_accmap{
   my ($self) = @_;
   my ($obj,%accmap,$out,$temp);

   if( defined $self->_accmap_cache() ) {
       return $self->_accmap_cache();
   }
   # now get and tie the hash, saving the object  
   if( tie(%accmap,'DB_File',$self->{'clan_index'},O_RDONLY,0666) == 0){
       die("Could not open $self->{'clan_index'} as a DB_file $!");
   }
   $out = {};
   %{$out} = %accmap;
   untie(%accmap);
   $self->_accmap_cache($out);
   
   return $out;
}


sub _accmap_cache{
   my ($self,$value) = @_;
   if( defined $value) {
      $self->{'_accmap_cache'} = $value;
    }
    return $self->{'_accmap_cache'};
}

=head1 AUTHOR

Jennifer Daub <jd7@sanger.ac.uk> (mainly taken from old Rfam code)

=head1 COPYRIGHT

Copyright (c) 2009: Genome Research Ltd.

Authors: Jennifer Daub <jd7@sanger.ac.uk> (mainly taken from old Rfam code)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


1;
