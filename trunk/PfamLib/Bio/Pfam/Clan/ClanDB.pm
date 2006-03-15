#
# Deals with updating the RDB and RCS parts during
# clan building
#

=head1 NAME

CfamDB

=head1 SYNOPSIS

Deals with updating the RDB and RCS parts of pfam during
clan entry into the database

=cut

package Bio::Pfam::Clan::ClanDB;

use strict;

use Text::Wrap;
use Bio::Pfam;
use Bio::Pfam::PfamRCS;
use Bio::Pfam::UpdateRDB;
use Bio::AnnotationCollectionI;

=head2  update_clan_members_in_RDB 

 Title    : update_clan_members_in_RDB
 Usage    : &ClanDB::update_clan_members_in_RDB ($rdb, @family)
 Function : Updates the RDB with the latest RCS version of specified families 
 Returns  : Nothing
 Args     : rdb handle, array contain family names

=cut

sub update_clan_members_in_RDB {
    my ($rdb, @clan_members) = @_;
    foreach my $member (@clan_members){
	chomp($member);
	&Bio::Pfam::PfamRCS::lock_family($member, "RDB");
	my ($db, $en);
	
	eval {
	    $db = Bio::Pfam->checkin_db( "." );
	    $en = $db->get_EntryA_by_id( $member );
	    $rdb->check_in_EntryA( $en );
	};
	$@ and die "RDB update; Could not update relational database for family $member [$@]\n";
	
	&Bio::Pfam::PfamRCS::abort_lock_on_family( $member );
	
	##### the following will obtain a lock on the family and release it 
	##### when done
	
	&Bio::Pfam::PfamRCS::make_view_files($member); 
	
	print STDERR "\n\nChecked in Family [$member]\n";
	
    }
}

=head2 check_in_pfam_into_RCS

 Title    : check_in_pfam_into_RCS
 Usage    : &ClanDB::check_in_pfam_into_RCS(@family)
 Function : checks family into the RCS system
			** THIS DOES NOT UPDATE RDB **
 Returns  : Nothing
 Args     : family id

=cut

sub check_in_pfams_into_RCS {
    my $message = shift;
	my @oldpfams = @_; 
	foreach my $family (@oldpfams){
		if( &Bio::Pfam::PfamRCS::move_files_to_rcs_directory($family, $family) == 0 ) {
			die "Could not move RCS files to directory for family [$family]. Problem!\n";
		}
	
		my ($comment);
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

		if( &Bio::Pfam::PfamRCS::check_in_rcs_files($family, $comment) == 0 ) {
    		die "pfci: Could not check in files for $family. This is a bad error\n";
		}

		if( &Bio::Pfam::PfamRCS::update_current_directory($family) == 0 ) {
    		die "Could not update directory for $family\n";
		}
	}
}


=head2 check_new_pfam_into_RCS

 Title    : check_new_pfam_into_RCS
 Usage    : &ClanDB::check_new_pfam_into_RCS($newfamily)
 Function : Runs all RCS checks before checking into the RCS system
			** THIS DOES NOT UPDATE RDB **
 Returns  : Nothing
 Args     : family id

=cut


sub check_new_pfam_into_RCS {
    my $message = shift;
    my $family = shift;
	
	if( &Bio::Pfam::PfamRCS::check_family_directory_exists($family) ) {
    	die "Family [$family] already has a directory.\nIf this is an existing family, check in the revision using pfci\n";
	}

	my $name_clash = &Bio::Pfam::PfamQC::name_clashes($family);
	if( $name_clash ) {
    	die "Family name [$family] clashes with existing name $name_clash\n";
	}

	if(! &Bio::Pfam::PfamQC::check_current_family_files($family) ) {
    	die "Your current family files do not pass the file existance checks.\nYou must remake HMM and ALIGN after modifying SEED\n";
	}

	if(-s "$family/overlap" ) {
		die "Your family seems to have an overlap to current, see $family/overlap\n";
	}

	if(! -e "$family/overlap" ) {
    	print STDERR "pfnew: your family seems to have no overlap check\n";
	}

	#if(! &PfamQC::family_overlaps_with_db($family) ) {
    #	die "$family appears to overlap with the database - check $family/overlaps for details.\n";
	#}

	if( ! &Bio::Pfam::PfamQC::all_format_checks_ok($family,1) ) {
    	print "$family does not pass format checks. Really check in? [yes/no default - no]\n";
    	my $line = <STDIN>;
    	chop $line;
    	if( $line =~ /^y/ ) {
			print "Ignoring format irregularities\n";
    	} 
		else {
			die "Aborting check-in for $family\n";
    	}
	}


	if( ! open(DESC,"./$family/DESC") ) {
    	die "A bad error - cannot open the desc file [./$family/DESC]. Yikes! [$!]";
	}

	while(<DESC>) {
    	!/^AC/ && next;
    	chop;
	
    	die "Your DESC file has a AC line [$_]. Currently pfnew insists on having no\n accession lines to prevent ambiguous accession allocation\n";
	}
	close(DESC);

	$message .= "This is a new family in the clan";

	#
	# Ok. Attempt to get the lock. If we fail a rather ungraceful exit.
	# 


	my $ret = &Bio::Pfam::PfamRCS::get_pfam_accession_lock();

	if( !($ret =~ /^success/ ) ) {
    	die "The accession lock has been grabbed by [$ret].\nAccession locking should be short - try again in a couple of minutes\n";
	}

	if( ! &Bio::Pfam::PfamRCS::make_new_rcs_directory($family) ) {
    	&Bio::Pfam::PfamRCS::release_accession_lock();
    	die "pfnew: Cannot make a new directory for $family.\nCheck you have write permissions to RCS_MASTER\n";
	}


	my $accession = &Bio::Pfam::PfamRCS::allocate_new_pfam_accession($family);
		if( !defined $accession ) {
    		&Bio::Pfam::PfamRCS::release_accession_lock();
			die "Unable to allocate new accession number. Check write permission to ACCESSION dir\n";
		}

	&Bio::Pfam::PfamRCS::release_accession_lock();
	if( ! open(DESC,"./$family/DESC") ) {
    	die "A bad error - cannot open the desc file [./$family/DESC]. Yikes! [$!]";
	}


	# Why oh why
	if( !open(TEMP,">./$family/pfam_temp_DESC_ewan") ) {
    	die "A bad error - cannot open a desc file to write to. Yikes!";
	}
	
	print TEMP "AC   $accession\n";
	while(<DESC>) {
    	print TEMP;
	}
	close(DESC);
	close(TEMP);


	if( !rename("$family/pfam_temp_DESC_ewan","$family/DESC") ) { 
    	die "Could not rename Temp desc file (??? file permissions!) ($!)";
	}

    
	if( &Bio::Pfam::PfamRCS::move_files_to_rcs_directory($family,$family) == 0 ) {
    	die "Could not move RCS files to directory for family [$family]. Problem!\n";
	}


	if( &Bio::Pfam::PfamRCS::make_new_rcs_files($family,$message) == 0 ) {
    	die "Could not make new files for family [$family], this is a bad error!\n";
	}


	open(LOCK,">$Bio::Pfam::PfamRCS::Pfam_rcs_root/$family/locked");
	print LOCK "First lock due to pfnew\n";
	close(LOCK);

  
	my $date = gmtime();
	$message .= "family [$family] deposited on $date";

	if( &Bio::Pfam::PfamRCS::check_in_rcs_files($family,$message) == 0 ) {
    	die "pfnew: could not check in files for family $family. this is a bad error\n";
	}


	if( &Bio::Pfam::PfamRCS::update_current_directory($family) == 0 ) {
    	die "pfnew: Could not update directory for $family\n";
	}
	#Success if we get here......hopefully
}

=head2 assign_clan

 Title    : assign_clan
 Usage    : &ClanDB::assign_clan($rdb)
 Function : Works out the next clan number from the RDB 
 Returns  : Next Clan number
 Args     : rdb file handle.  AS we  have opened a transaction to lock all
            all tables, we need to use the same handle.

=cut

sub assign_clan {
	my $rdb = shift;
	# Need to set up a transcation on the clans table, read it and then
	# work out the next available clan number.
	my @clan_numbers;
	eval {
		my $dbh = $rdb->open_transaction('clans', 'dead_clans');
		my $st = $dbh->prepare("select distinct clan_acc from clans");
		$st->execute();
		while(my $clan_acc = $st->fetchrow){
			if ($clan_acc =~ /CL(\d+)/){
				push(@clan_numbers, $1);
			}
		}
		$st = $dbh->prepare("select distinct clan_acc from dead_clans");
		$st->execute();
		while(my $clan_acc = $st->fetchrow){
			if ($clan_acc =~ /CL(\d+)/){
				push(@clan_numbers, $1);
			}
		}

		$st->finish();
	};
	$rdb->close_transaction($@);
	if ($@){
		die "Something has gone very wrong during the assignment of the clan accession number:$@\n";
	}
	my @clan_number_sort = sort{$a <=> $b}@clan_numbers;
	my $last_clan_number = $clan_numbers[$#clan_numbers];
	my $new_number = $last_clan_number + 1;
	my $newclanno = "CL"."0" x (4 - length($new_number))."$new_number";
	return ($newclanno);
}


=head2 load_clandesc_file_into_RDB

 Title    : load_clandesc_file_into_RDB
 Usage    : &ClanDB::load_clandesc_file_into_RDB($rdb, $clan_acc, \*CLANDESC)
 Function : Loads the information in the CLANDESC file into the RDB
 Returns  : auto_clan_number
 Args     : 

=cut

sub load_clandesc_file_into_RDB {
	my ($rdb, $clan_acc, $clandesc_fh) = @_;
	my ($acc, $author, $desc, $id);
	my $ann = Bio::Annotation::Collection->new();
	my @lines = (<$clandesc_fh>);
	my $i; # line counter
	for($i=0;$i <= $#lines;$i++) {
       $_ = $lines[$i];
       #print STDERR "$i Looking at main line $_";
       /^AC\s+(CL\d+)(\.\d+)?$/ && do {
           $acc = $1;
           next;
       };
       /^ID\s+(\S{1,16})$/ && do {
           $id = $1;
           next;
       };
       /^DE\s+(.*?)\s+$/ && do {
           $desc = $1;
           next;
       };
       /^AU\s+(.+)$/ && do {
           $author = $1;
           next;
       };
	   last;	
	}
my ($refcomment, $ref);
OPTIONAL_LINE :
   for(;$i <= $#lines;) { 
       #print STDERR "Looking at $lines[$i]";
		if( $lines[$i] =~ /^CC/ ) {
			last;
		}
		if( $lines[$i] =~ /^R/ ) {
           # read references
		   my $refcomment;
           #print STDERR "Reference $lines[$i]";
           for(;$i <= $#lines;$i++) { 
               #print STDERR "Looking at $lines[$i] in refcomment\n";
               if( $lines[$i] =~ /^RC\s+(.*)/ ) {
                   $refcomment = $1;
                   for($i++;$i <= $#lines;$i++) {
                       $lines[$i] =~ /RC\s+(.*)/ || do { last; };
                       $refcomment .= $1;
                   }
               };
               
               #print STDERR "Left with $lines[$i] after comments\n";
               
               # eaten all the RC lines at the top of this 
               # reference. Now to eat the rest
               
               # if not an /R line, end of references
               
               $lines[$i] =~ /^R/ || do {
                   if( defined $refcomment ) {
                       warn "Got reference comment with no reference! [$clan_acc]";
                   }
                   last;
               };
               # make a new reference line, add in comment if there
               $lines[$i] =~ /^RN\s+\[(\d+)\]/ || die ("[$lines[$i]] is not a reference number (expected) [$clan_acc]");
               my $ref = new Bio::Annotation::Reference; 
               if (defined $refcomment){
                   $ref->comment($refcomment);
                   undef $refcomment;
               }
			   # first a medline line
               $i++;
               $ann->add_Annotation('reference', $ref);
               $lines[$i] =~ /^RM   (\d+)/ || die ("[$lines[$i]] is not a reference medline line (expected) [$clan_acc]");
               my $temp = $1; 
               $ref->medline($temp);
               # RT
               
               $i++;
               
               $lines[$i] =~ /^RT   (.*)/ || die ("[$lines[$i]] is not a reference title line (expected) [$clan_acc]");
               $temp = $1 . " ";
               for($i++;$i <= $#lines;$i++) {
                   if( $lines[$i] =~ /^RT   (.*)/ ) {
                       $temp .= $1 . " ";
                   } else {
                       last;
                   }
               }
               $temp =~ s/  / /g;
               $ref->title($temp);
               
               # don't need to add one, as we ended on a non RT line
               
               $lines[$i] =~ /^RA   (.*)/ || die ("[$lines[$i]] is not a reference author line (expected) [$clan_acc]");
               $temp = $1 . " ";
               for($i++;$i <= $#lines;$i++) {
                   if( $lines[$i] =~ /^RA   (.*)/ ) {
                       $temp .= $1 . " ";
                   } else {
                       last;
                   }
               }
               $temp =~ s/  / /g;
               $ref->authors($temp);
               
               $lines[$i] =~ /^RL   (.*)/ || die ("[$lines[$i]] is not a reference author line (expected) [$clan_acc]");
               $temp = $1;
               $ref->location($temp);
           }
		next OPTIONAL_LINE;
		}
		if( $lines[$i] =~ /^D/ ) {
			my $com;
			#print STDERR "Link $lines[$i]";
			for(;$i <= $#lines;$i++) { 
				#print STDERR "Looking at $lines[$i] as a link\n";
				my $link = Bio::Annotation::DBLink->new();
				if( $lines[$i] =~ /^DC\s+(.*)/ ) {
					$com = $1;
					for($i++;$i <= $#lines;$i++) {
						$lines[$i] =~ /DC\s+(.*)/ || do { last; };
						$com .= $1;
					}
					$link->comment($com);
					undef $com;  
				}
        	       # eaten all the DC lines at the top of this 
        	       # link. Now to eat the rest
        	       # if not an /DR line, end of references
				my ($db_name, $db_ref, $rest);
				if ( $lines[$i] =~ /^DR\s+/) {
					if ($lines[$i] =~ /^DR\s+(\S+);\s+(\S+\s\S?);\s*(.*)/) {
						($db_name, $db_ref, $rest) = ($1, $2, $3);
					}
					elsif ($lines[$i] =~ /^DR\s+(\S+);\s+(\S+);\s*(.*)/) {
						($db_name, $db_ref, $rest) = ($1, $2, $3);
					}
					else {
						warn "Bad DR line - $lines[$i]";
					}
				}
				else {
					if( defined $com ) {
						warn "Got link comment with no links - at $lines[$i]! [$clan_acc]";
					}
					last;
				} 
				$ann->add_Annotation('dblink', $link);
				$link->database($db_name);
				$link->primary_id($db_ref);
				$link->optional_id($rest);
			}
			next OPTIONAL_LINE;
	
		} # end of if ^D

	}
	#Comment lines
	for(;$i <= $#lines;$i++) { 
		my $com = new Bio::Annotation::Comment;
		if( $lines[$i] =~ /^CC\s{3}(.*)/ ){
			$com->text($1);
			$ann->add_Annotation('comment', $com);
		} 
		elsif ( $lines[$i] =~ /^CC\s*$/ ) {
			$com->text("");
			$ann->add_Annotation('comment', $com);
		} 
		elsif ( $lines[$i] =~ /^MB\s*(PF\d+)/ ) {
			;
		}
		else {
			warn "Cannot read [$lines[$i]] as comment";
		}
	}

	for(;$i <= $#lines;$i++) {
       warn "Unexpected line at end of the CLANDESC [$lines[$i]]";
	}

	# All annotationation should be loaded up into the annotation object or 
	# stored in scalar variables.
	my $auto_clan = &Bio::Pfam::UpdateRDB::check_in_clan($rdb, $clan_acc, $author, $ann, $desc, $id);
	return ($auto_clan);
}

=head2 write_clandesc

 Title    : write_clandesc
 Usage    : &ClanDB::write_clandesc($rdb, $clan_acc, $filehandle, $recored_terminator)
 Function : Write the clandesc file contained in the RDB to the filehandle
 Returns  : Nothing
 Args     : database handle, clan number, filehandle, record_terminator (1/0)

=cut

sub write_clandesc{
	my ($rdb, $clan_acc, $fh, $record_term) = @_;
	my ($ann, $authors, $desc, $id) = &get_clandesc($rdb, $clan_acc);
	my $order = 1;
	my $version = get_current_version($rdb, $clan_acc);
	$Text::Wrap::columns = 75;
	print $fh "AC   $clan_acc.$version\n";
	print $fh "ID   $id\n";
	print $fh "DE   ".$desc."\n";
	print $fh "AU   $authors\n";
	foreach my $ref ( $ann->get_Annotations('reference') ) {
		if (($ref->comment)&& ($ref->comment ne "NULL")){
			print $fh wrap("RC   ","RC   ",$ref->comment());
			print $fh "\n";
		}
		print $fh "RN   [$order]\n";
		print $fh "RM   ".$ref->medline()."\n";
		print $fh wrap("RT   ","RT   ", $ref->title());
		print $fh "\n";
		print $fh wrap("RA   ","RA   ", $ref->authors());
		print $fh "\n";
		print $fh "RL   ".$ref->location."\n";
		$order++;
	}
	foreach my $link ( $ann->get_Annotations('dblink') ) {
	 	if($link->comment() && $link->comment() !~/NULL/){
			print $fh wrap("DC   ", "DC   ", $link->comment."\n");
		}
		print $fh "DR   ".$link->database()."; ".$link->primary_id().";";
		if ($link->optional_id()){
			print $fh " ".$link->optional_id().";";
		}
		print $fh "\n"; 
	}

	foreach my $com ($ann->get_Annotations('comment')){
	print $fh wrap("CC   ","CC   ",$com->text."\n");
	}
	my @acc = get_clan_membership_acc($rdb, $clan_acc);
	foreach (@acc){
		print $fh "MB   $_;\n";
	}
	if($record_term){
		#Clandesc file does not need the //
		print $fh "//\n";
	}
}

=head2 get_all_clan_accs

 Title    : get_all_clan_accs
 Usage    : &ClanDB::get_all_clan_accs($rdb)
 Function : Retrieves all of the clans from the release
 Returns  : array of accessions
 Args     : database handle

=cut

sub get_all_clan_accs {
    my ($rdb) = @_;
    my $dbh;
    eval{
	$dbh = $rdb->open_transaction('clans');
    };
    if ($@){
	$dbh=$rdb;
    }
    my @all_clan_accs;
    eval{
	my $st = $dbh->prepare("select clan_acc from clans");
	$st->execute;
	while(my $clan_acc = $st->fetchrow){
	    push(@all_clan_accs, $clan_acc);
	}
    };
    if ($@){
	die "There was a problem getting all the clan accession numbers. [$@]\n";
    }

    eval{
	$rdb->close_transaction;
    };
    return @all_clan_accs;
}

=head2 get_all_locked_clan_accs

 Title    : get_all_locked_clan_accs
 Usage    : &ClanDB::get_all_locked_clan_accs($rdb)
 Function : Retrieves all of the clans from the release
 Returns  : array of accessions
 Args     : database handle

=cut

sub get_all_locked_clan_accs {
	my ($rdb) = @_;
	my @all_clan_accs;
	eval{
		my $st = $rdb->prepare("select clan_acc from clans join clan_locks where clans.auto_clan=clan_locks.auto_clan and locked=1");
		$st->execute;
		while(my $clan_acc = $st->fetchrow){
			push(@all_clan_accs, $clan_acc);
		}
	};
	if ($@){
		die "There was a problem getting all the clan accession numbers. [$@]\n";
	}
	return @all_clan_accs;
}

=head2 get_clandesc

 Title    : get_clandesc
 Usage    : &ClanDB::get_clandesc($rdb, $clan_acc)
 Function : Retrieves the clandesc file from the RDB
 Returns  : annotation object, author, update date, created date
 Args     : database handle, clan accession

=cut

sub get_clandesc {
	my ($rdb, $clan_acc) = @_;
	my $ann = Bio::Annotation::Collection->new();
	my ($author, $desc, $auto_clan_number, $comment, $updated, $created, $version, $id);
	## first get the author, description and comment 
	my $dbh;
	eval{
		$dbh = $rdb->open_transaction('clans', 'literature_references', 'clan_lit_refs', 'clan_database_links');
	};
	if ($@){
		$dbh = $rdb;
	}
	eval{
		my $st = $dbh->prepare("select * from clans where clan_acc=\"$clan_acc\"");
		$st->execute;
		($auto_clan_number,$clan_acc,$id, $desc,$author,$comment,$updated,$created) = $st->fetchrow_array;
	};
	if ($@){
		die "Could not retrieve the clan description for $clan_acc: $@\n";
	}
	#$ann->description($desc);
	my $com = Bio::Annotation::Comment->new();
	$com->text($comment);
	$ann->add_Annotation('comment', $com);
	
	## now get the references
	my @refs_as_hash_refs;
	eval{
		my $st = $dbh->prepare("select medline, title, author, journal, order_added, comment from clan_lit_refs join literature_references where clan_lit_refs.auto_lit=literature_references.auto_lit and auto_clan=\"$auto_clan_number\"");
		$st->execute;
		while(my $hash_ref = $st->fetchrow_hashref){
			push(@refs_as_hash_refs, $hash_ref);
		}
	};
	if ($@){
		die "Could not retrieve the clan references for $clan_acc: $@\n";
	}
	# Get version
	
	
	## Sort the array basd on  the order added;
	foreach my $ref_hash_ref (sort{$$a{'order_added'} <=> $$b{'order_added'}}@refs_as_hash_refs){
		my $ref  = new Bio::Annotation::Reference;
		$ann->add_Annotation('reference', $ref);
		$ref->comment($$ref_hash_ref{'comment'});
		$ref->medline($$ref_hash_ref{'medline'});
		$ref->title($$ref_hash_ref{'title'});
		$ref->location($$ref_hash_ref{'journal'});
		$ref->authors($$ref_hash_ref{'author'});
	}
	
	my @links_as_hash_refs;
	eval{
		my $st = $dbh->prepare("select db_id, comment, db_link, other_params from clan_database_links where auto_clan=\"$auto_clan_number\"");
		$st->execute;
		while(my $hash_ref = $st->fetchrow_hashref){
			push(@links_as_hash_refs, $hash_ref);
		}
	};
	if ($@){
	 warn "Error, $@\n";
	}
	foreach my $link_hash_ref (@links_as_hash_refs){
		my $link = new Bio::Annotation::DBLink;
		$ann->add_Annotation('dblink', $link);
		$link->comment($$link_hash_ref{'comment'});
		$link->database($$link_hash_ref{'db_id'});
		$link->primary_id($$link_hash_ref{'db_link'});
		$link->optional_id($$link_hash_ref{'other_params'});
	}

	eval{
		$rdb->close_transaction;
	};
	return($ann, $author, $desc, $id);			

}

=head2 load_clan_membership

 Title    : load_clanmembership
 Usage    : &ClanDB::load_clan_membership($rdb, $auto_clan_number, @clan_membership)
 Function : Loads the members of the clan into the membership table 
 Returns  : 
 Args     : database handle, auto clan number, list of pfamA ids

=cut

sub load_clan_membership {
	my ($rdb, $auto_clan_number, @clan_membership) = @_;
		
	#Get all of the pfamA auto numbers
	my $pfamA_list = join("\" or pfamA_id=\"", @clan_membership);
	my @auto_pfams;
	eval{
		my $dbh = $rdb->open_transaction("pfamA");
		my $st= $dbh->prepare("select auto_pfamA from pfamA where pfamA_id=\"$pfamA_list\"");
		$st->execute();
		while(my $auto_pfam = $st->fetchrow){
			push(@auto_pfams, $auto_pfam);
		}
		$st->finish();
	};
	$rdb->close_transaction($@);
	warn "difference in array size between auto_pfams and pfamA ids" if ($#clan_membership != $#auto_pfams);
	# reload new membership
	&Bio::Pfam::UpdateRDB::update_clan_membership($rdb, $auto_clan_number,@auto_pfams); 
}

=head2 check_clan_lock

 Title    : check_clan_lock
 Usage    : &ClanDB::check_clan_lock($rdb, $clan_acc)
 Function : Checks if a clan has a lock
 Returns  : lock status (1 means that it is locked), the user and the timestamp of the lock
 Args     : database handle, clan accession

=cut

sub check_clan_lock {
	my ($rdb, $clan_acc) = @_;
	my ($clan_lock_status, $user, $date, $type);
	my $dbh;
	eval{
		$dbh = $rdb->open_transaction('clans','clan_locks');
	};
	if ($@){
		$dbh=$rdb;
	}
	eval {
		my $st = $dbh->prepare("select locked, user, clan_locks.updated, type from clan_locks join clans where clans.auto_clan=clan_locks.auto_clan and clan_acc=\"$clan_acc\"");
		$st->execute();
		($clan_lock_status, $user, $date, $type) = $st->fetchrow_array;
	};
	if ($@) {
		die "Something went wrong with getting the clan lock status: $@\n";

	}
	eval{
		$rdb->close_transaction();
	};	
	#$date = &dateStringFormat($date);
	return ($clan_lock_status, $user, $date, $type);
}

=head2 check_clan_exists

 Title    : check_clan_exists
 Usage    : &ClanDB::check_clan_exists($rdb, $clan_acc)
 Function : Checks that a clan accession corresponds to an exisiting clan
 Returns  : 1 if all is OK, 0 if not
 Args     : database handle, clan accession

=cut


sub check_clan_exists {
	my ($rdb, $clan_acc) = @_;
	my $auto_clan;
	my ($dbh);
	eval{
		$dbh = $rdb->open_transaction("clans", "clan_locks");
	};
	if($@){
		$dbh = $rdb;
	}
	eval {
		my $st = $dbh->prepare("select auto_clan from clans where clan_acc=\"$clan_acc\"");
		$st->execute();
		$auto_clan = $st->fetchrow;
	};
	if ($@) {
		die "Something went wrong with getting the clan auto number: $@\n";

	}
	eval{
		$rdb->close_transaction;
	};
	if ($auto_clan) {
		return 1; # Success
	}
	else {
		return 0; # Failure
	}
}

=head2 get_clan_membership_id

 Title    : get_clan_membership_id
 Usage    : &ClanDB::get_clan_membership_id($rdb, $clan_acc)
 Function : Gets a list of the pfam entries that make up the clan by pfam id
 Returns  : array of pfam ids
 Args     : database handle, clan accession

=cut


sub get_clan_membership_id {
    my ($rdb, $clan_acc) = @_;
    my @pfamAs;
    
    my $dbh;
    eval {
	$dbh = $rdb->open_transaction('pfamA', 'clan_membership', 'clans');
    };
    if ($@){
	$dbh = $rdb;
    }
    eval {
	my $st = $dbh->prepare("select pfamA_id from pfamA join clan_membership, clans where pfamA.auto_pfamA=clan_membership.auto_pfamA and clans.auto_clan=clan_membership.auto_clan and clan_acc=\"$clan_acc\"");
	$st->execute();
	while(my @ar = $st->fetchrow_array) {
	    push(@pfamAs, $ar[0]);
	}
    };
    if ($@){
	die "Something went wrong with getting the clan membership: $@\n";
    }
    eval{
	$rdb->close_transaction;
    };
    $@ = undef;
    return (@pfamAs);
}

=head2 get_clan_membership_acc

 Title    : get_clan_membership_acc
 Usage    : &ClanDB::get_clan_membership_acc($rdb, $clan_acc)
 Function : Gets a list of the pfam entries that make up the clan by pfam accession number
 Returns  : array of pfam accessions
 Args     : database handle, clan accession

=cut

sub get_clan_membership_acc {
	my ($rdb, $clan_acc) = @_;
	my @pfamAs;
	my $dbh;
	eval{
		$dbh=$rdb->open_transaction('pfamA', 'clans', 'clan_membership');
	};
	if ($@) {
		$dbh = $rdb;
	}

	eval {
		my $st = $dbh->prepare("select pfamA_acc from pfamA join clan_membership, clans where pfamA.auto_pfamA=clan_membership.auto_pfamA and clans.auto_clan=clan_membership.auto_clan and clan_acc=\"$clan_acc\"");
		$st->execute();
		while(my @ar = $st->fetchrow_array) {
			push( @pfamAs, $ar[0]);
		}
	};
	if ($@) {
		die "Something went wrong with getting the clan membership: $@\n";
	}
	eval{
		$rdb->close_transaction();
	};
	return (@pfamAs);
}

=head2 get_revdat_credat

 Title    : get_revdat_credat
 Usage    : &ClanDB::get_revdat_credat($rdb, $clan_acc))
 Function : Gets the creation and last updated time stamps 
 Returns  : hashref of dates
 Args     : database handle, clan accession

=cut

sub get_revdat_credat{
	my ($dbh, $clan_acc) = @_;
	my $dates;
	eval {
		my $st = $dbh->prepare("select updated, created from clans where clan_acc =\"$clan_acc\"");
		$st->execute;
		$dates = $st->fetchrow_hashref;
	};
	if ($@) {
		die "Something went wrong with getting the clan revision dates: $@\n";
	}
	#$$dates{'updated'} = &dateStringFormat($$dates{'updated'});
	return $dates;
}


=head2 kill_clan

 Title    : kill_clan
 Usage    : &ClanDB::kill_clan($rdb, $clan_acc, $message, $forward)
 Function : Kills off a clan and puts the relevant information into the RDB 
 Returns  : 1 if successed.  Dies otherwise
 Args     : database handle, clan accession, reason for being killed, the clan_acc (if merged)

=cut


sub kill_clan {
	my ($dbh, $clan_acc, $message, $forward);
	#my @membership = get_clan_membership_id($dbh, $clan_acc);
	my @membership_with_rcsversion = &clan_membership_with_rcsversion($dbh, $clan_acc);
	my $membership_string = join(", ", @membership_with_rcsversion);
	my $clan_desc = &get_kill_info($dbh,$clan_acc);		  
	my $user = `whoami`;
	chomp $user;
	
	my $error = 0;
	&add_dead_clan($dbh, $clan_acc, $clan_desc, $membership_string, $message, $forward, $user);
	#If we get here then everything has worked in theory!
	return 1;
}

sub get_kill_info {
	my ($dbh, $clan_acc) = @_;
	my $desc;
	eval {
		my $st = $dbh->prepare("select clan_description from clans where clan_acc = \"$clan_acc\"");
		$st->execute;
		$desc =  $st->fetchrow;
	};
	if ($@){
		warn "Something has gone a bit pair shaped whilst getting the kill info: $@\n";
	}
	return $desc;
}

=head2 membership_with_rcsversion

 Title    : membership_with_rcsversion
 Usage    : &ClanDB::membership_with_rcs($rdb, $clan_acc)
 Function : Gets a list of the pfam entries that make up the clan by pfam id
			and adds the rcs version.
 Returns  : array of pfam ids/acc with - rcs version
 Args     : database handle, clan accession

=cut

sub membership_with_rcsversion {
	my ($rdb, $clan_acc) = @_;
	my @membership = get_clan_membership_id($rdb, $clan_acc);
	my @membership_with_rcs;
	foreach my $member (@membership){
		open(RCSLOG, "rlog -r $Bio::Pfam::rcs_master_dir/$member/SEED |");
		while(<RCSLOG>){
			if ($_ =~ /revision (\d+\.\d+)/){
				push(@membership_with_rcs, "$member-$1");
			}
		}
		close(RCSLOG);
	}
	return @membership_with_rcs;
}

=head2 add_clan_to_clan_lock_table

 Title    : add_clan_to_clan_lock_table
 Usage    : add_clan_to_clan_lock_table($rdb, $name, $clan_acc)
 Function : Adds the clan to the clan lock table with no lock
 Returns  : nothing
 Args     : database handle, user id, clan accession

=cut

sub  add_clan_to_clan_lock_table{
	my ($rdb, $name, $clan_acc) = @_;
	&Bio::Pfam::UpdateRDB::update_lock_on_clan($rdb, $name, $clan_acc, 0);
}

=head2 get_lock_on_clan

 Title    : get_lock_on_clan
 Usage    : get_lock_on_clan($rdb, $name, $clan_acc, $type)
 Function : Gets a lock in the rdb for the specified clan accession number
 Returns  : nothing
 Args     : database handle, user id, clan accession

=cut

sub  get_lock_on_clan{
	my ($rdb, $name, $clan_acc, $type) = @_;
	&Bio::Pfam::UpdateRDB::update_lock_on_clan($rdb, $name, $clan_acc, 1, $type);
}

=head2 release_lock_on_clan

 Title    : release_lock_on_clan
 Usage    : release_lock_on_clan($rdb, $name, $clan_acc)
 Function : Releases a lock in the rdb for the specified clan accession number
 Returns  : nothing
 Args     : database handle, user id, clan accession

=cut

sub release_lock_on_clan{
	my ($rdb, $name, $clan_acc) = @_;
	&Bio::Pfam::UpdateRDB::update_lock_on_clan($rdb, $name, $clan_acc, 0);
}

=head2 update_version

 Title    : update_version
 Usage    : update_version($rdb, $clan_acc, $message, $name)
 Function : Updates the clan version table with all of the necessary revision info
 Returns  : nothing
 Args     : database handle, clan accession, description of changes, user id

=cut

sub update_version {
	my ($rdb, $clan_acc, $message, $name) = @_;
	my $version = &get_current_version($rdb, $clan_acc);
	$version = 0 if (!$version);
	$version++;
	&Bio::Pfam::UpdateRDB::zero_current($rdb, $clan_acc);
	my @membership_with_rcs = &membership_with_rcsversion($rdb, $clan_acc);
	my $membership_sting = join(": ", @membership_with_rcs);
	&Bio::Pfam::UpdateRDB::update_version_table($rdb, $clan_acc, 1, $version, $membership_sting, $message, $name);	
}

=head2 get_current_version

 Title    : get_current_version
 Usage    : get_current_version($rdb, $clan_acc)
 Function : Gets the current version number for the clan
 Returns  : version number
 Args     : database handle, clan accession

=cut

sub get_current_version{
	my ($rdb, $clan_acc) = @_;
	my $dbh;
	eval{
		$dbh = $rdb->open_transaction('clans', 'clan_versions');
	};
	if ($@){
		$dbh = $rdb;
	}
	my $version = 0;
	eval{
		my $st = $dbh->prepare("select version from clan_versions join clans where clans.auto_clan=clan_versions.auto_clan and  current=1 and clan_acc=\"$clan_acc\"");
		$st->execute();
		$version = $st->fetchrow()
	};
	if($@){
		warn "Could not get current version information:[$@}\n";
	}
	eval{
		$rdb->close_transaction();
	};
	return $version;
}

=head2 get_revision_history

 Title    : get_revision_history
 Usage    : get_revision_history($rdb, $clan_acc)
 Function : Gets all of the revision history for a specified clan 
 Returns  : nothing, but prints the revision history to STDOUT
 Args     : database handle, clan accession

=cut

sub get_revision_history {
	my ($rdb, $clan_acc) = @_;
	my @version_data;
	eval{
	my $st = $rdb->prepare("select version, current, rcs_trace, message, user, clan_versions.updated from clan_versions join clans where clans.auto_clan=clan_versions.auto_clan and clan_acc=\"$clan_acc\"");
	$st->execute;
	
		while(my $hash_ref = $st->fetchrow_hashref){
			push(@version_data, $hash_ref);
		}
	};
	if ($@){
		warn "error getting version data: [$@]\n";
	}
	my $hr = "-" x 20;
	print STDOUT "$hr\n";
	foreach(sort{$$a{'version'}<=>$$b{'version'}}@version_data){
		#my $date = &dateStringFormat($$_{'updated'});
		print STDOUT "version:".$$_{'version'}."\n";
		print STDOUT $$_{'message'}."\n";
		print STDOUT "rcs versions of members:".$$_{'rcs_trace'}."\n";
		print STDOUT "date:".$$_{'updated'}." author:".$$_{'user'}."\n";
		if($$_{'current'}){
			print STDOUT "LATEST VERSION\n";
		}
		print STDOUT "$hr\n";
	}
}

=head2 dateStringFormat

 Title    : dateStringFormat
 Usage    : dateStringFormat($datestring)
 Function : Converts the rdb timestamp from a string of numbers to 
            a more human readable format. 
 Returns  : The date in the format YYYY-MM-DD HH:MM:SS
 Args     : rdb timestamp

=cut

#This seems obsolete due to upgrade of MySQL on 16/03/05

sub dateStringFormat {
	my $datestring=shift;
	my $formated_date_string;
	if($datestring =~/(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})/){
		$formated_date_string = "$1-$2-$3 $4:$5:$6"; 
	}
	return $formated_date_string;
}

=head2 id2acc

 Title    : id2acc
 Usage    : id2acc($rdb, $clan_id)
 Function : Works out the clans accession for a given id 
 Returns  : clan accession, 0 if it fails
 Args     : rdb_handle, clan_id

=cut

sub id2acc {
    my ($rdb, $clan_id) = @_;
    my $clan_acc;
    my ($dbh);
    eval{
	$dbh = $rdb->open_transaction("clans");
    };
    if($@){
	$dbh = $rdb;
    }
    eval {
	my $st = $dbh->prepare("select clan_acc from clans where clan_id=\"$clan_id\"");
	$st->execute();
	$clan_acc = $st->fetchrow;
    };
    
    eval{
	$rdb->close_transaction;
    };
    if ($clan_acc) {
	return $clan_acc; # Success
    }
    else {
	return 0; # Failure
    }
}

=head2 acc2id
    
    Title    : acc2id
    Usage    : acc2id($rdb, $clan_acc)
    Function : Works out the clans identifier for a given accession 
    Returns  : clan identifier, 0 if it fails
    Args     : rdb_handle, clan_acc
    
=cut
    
sub acc2id {
    my ($rdb, $clan_acc) = @_;
    my $clan_id;
    my ($dbh);
    eval{
	$dbh = $rdb->open_transaction("clans");
    };
    if($@){
	$dbh = $rdb;
    }
    eval {
	my $st = $dbh->prepare("select clan_id from clans where clan_acc=\"$clan_acc\"");
	$st->execute();
	$clan_acc = $st->fetchrow;
    };
    
    eval{
	$rdb->close_transaction;
    };
    if ($clan_id) {
	return $clan_id; # Success
    }
    else {
	return 0; # Failure
    }
}

=head2 get_clan_acc
    
    Title    : get_clan_acc
    Usage    : get_clan_acc($pfam_id)
    Function : Tells you which clan a family belongs to 
    Returns  : clan identifier, 0 if it is not in a clan
    Args     : pfam_id
    
=cut

sub get_clan_acc {
   my $family = shift;

   my $rdb = Bio::Pfam->live_rdb;
   my $clan_acc=0;
   my @results = $rdb->query("select clan_id, clan_acc from clans, clan_membership, pfamA where clans.auto_clan=clan_membership.auto_clan and pfamA.auto_pfamA=clan_membership.auto_pfamA and pfamA_id=\"$family\"");
   if(@results){
      $clan_acc = ${$results[0]}[1];
   }
   return $clan_acc;
}


1;
