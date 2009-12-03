
#
# Perl module for UpdateRDB
#
# Cared for by Mhairi Marshall <mm1@sanger.ac.uk>
#
# Copyright Mhairi Marshall & Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Rfam::UpdateRDB

=head1 DESCRIPTION

This module contains the basic functionality for updating the Pfam 
relational database. It relies upon the Bio::Pfam::PfamRDB module
to provide basic connection facilities


=cut


# Let the code begin...
    

package Rfam::UpdateRDB;

use vars qw($AUTOLOAD 
	    @ISA 
	    @EXPORT_OK ); 

#use strict;
use Rfam::DB::RfamRDB;

@ISA = qw(Rfam::DB::RfamRDB);

#my $Attributes = ('-db_name' => "-db_name");

sub new {

  my $caller = shift;
  my $class  = ref( $caller ) || $caller;
  my %arguments = @_;
  
  my $self = $class->SUPER::new(%arguments);

  return $self;

}


############## The following mewthods can be used to update the database
############## and should beused with extreme caution


=head2 check_in_Entry

 Title   : check_in_entry
 Usage   : $updatedb->check_in_Entry( $entry );
 Function:
    This function updates the underlying relational database
    by loading the given family through the middleware layer
    and inserting all relevant records into the correct tables,
    deleting records if necesary
    -the order of table updates is important 
 Returns :
 Args    : Rfam::Entry::Entry object

=cut

sub check_in_Entry {
  my ($self, @en ) = @_;
  
  my ($dbh,
      @regions );
  
 
  $dbh = $self->open_transaction('wikitext', 'rfam', 'rfam_reg_seed', 'rfam_reg_full', 'rfamseq', 'rfam_literature_references', 'literature_references' , 'rfam_database_links');
  
    eval {
      foreach my $ent (@en) {
	$self->update_wikitext( [$ent] );  
	$self->update_rfam( [$ent] );	
	$self->update_rfam_reg_seed( [$ent] );
	$self->update_rfam_reg_full( [$ent] );
	$self->update_literature_references( [$ent] );
	$self->update_rfam_database_links( [$ent] );
      }
    };

  $self->close_transaction( $@);
  $@ and $self->throw($@);
  
}



=head2 empty_tables

 Title   : empty_tables
 Usage   :
 Function:
    This function empties the given tables as a single recoverable transaction.
    If a transaction handle is given, it is assumed that the operation is part
    of a larger internal transaction
 Returns :
    The number of rows deleted from each table (a list);
    Throws an exception if something went wrong
 Args    :
    1. A ref to a list of table names
 Notes   :
    1. If the transaction handle is undefined, a transaction is created and committed
    for just this update.

=cut

sub empty_tables {
   my ($self, $table_list) = @_;
   my (@rows, $dbh, $tab, $error, $rows);

   $dbh = $self->open_transaction( @{$table_list} );

   eval {
       foreach $tab (@{$table_list}) {
	   $self->report_mode and 
	       printf STDERR "Removing all records from %s.%s\n", $self->_database_name, $tab;
	   $dbh->do( $self->__empty_table_sql( $tab ) );
	   push @rows, $dbh->rows;
       }
   };

   $self->close_transaction( $@ );
   $@ and die $@;

   return $rows;
}

#originall added by jen-not currently used but might get resurrected
sub genomic_species_data {
     my($self, @array) = @_;
    warn "Rfam::Entry_RCS::genome_species_data() is deprecated - you're code is now probably broken!";
   return 0;
}

#originally added by jen-not currently used-might get resurrected.
sub chromosome_data {
     my($self, @array) = @_;
    warn "Rfam::Entry_RCS::chromosome_data() is deprecated - you're code is now probably broken!";
   return 0;
}

# This parses the final genome embl file after sam has tweaked with it!!
sub final_genomic_species_data {
     my($self, @array) = @_;
    warn "Rfam::Entry_RCS::final_genomic_species() is deprecated - you're code is now probably broken!";
   return 0;
}




sub load_generic_file_to_rdb {

  my ($self, $file, $table_name, $delete_from_table) = @_;
   my ($rows,
       $dbh);

 
   $dbh = $self->open_transaction( $table_name);
   
   if (! -e $file) {
       $self->throw("load_ $table_name _from_file error: Could not find file $file");
   }
  if (!$table_name) {
       $self->throw("load_ $table_name _from_file error: Did not pass the table_name");
   }
  
  if ($delete_from_table) {
    $dbh->do("delete from $table_name");
  }
   

   $self->report_mode and 
       printf STDERR "Inserting into %s.$table_name from file %s\n", $self->_database_name, $file;
   eval {
       $rows = $dbh->do( $self->__insert_from_file_sql( $table_name, $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;

}



sub check_in_dead_Entry {
 my ($self, @en) = @_;
 
 my ($stat,
     $dbh,
     $rows, 
     $error, 
     $rdb_acc, 
     $rdb_id, 
     $rdb_comment,
     $rdb_auto_num,
     $rdb_forward
     );

 $dbh = $self->open_transaction( 'rfam', 'dead_families' );

 $self->report_mode();
 
 eval {
   foreach my $en (@en) {
 
     $rdb_acc = $en->acc;
     $rdb_comment = $en->comment();
     foreach my $forward ($en->each_forward_acc()) {
	 $rdb_forward .= "$forward;";
     }
     if( $rdb_forward =~ /\;$/ ) {
	 chop $rdb_forward;
     }

     eval {
       if (not defined $stat) {
	 $stat = $dbh->prepare($self->__replace_sql('dead_families', 3));
       }
       
       $stat->execute( $rdb_acc, 
		       $rdb_comment,
		       $rdb_forward
		     );
       $rows += $stat->rows;
     };

  
     if ($@) {
       $error = "Could not do the insertion/update on the dead_families table [$@]";
      last;
    }
    
  }

  };
 

 
 $self->close_transaction( $error );
 $error and $self->throw( $error );

 ### No errors so delete the rfam entry from RDB !
 if(!$error) {
   $self->delete_Entry(@en);
 }


}

=head2 delete_Entry

 Title   : delete_Entry
 Usage   : $updatedb->delete_Entry( @entry_list );
 Function:
    This function deletes all of the region information
    associated with an rfam entry. Note that even the entry
    itself is deleted from rfam. Even though dead entries
    are not deleted from the rfam table, this occurs so
    that if we wish to replace the entry by using the file
    entry mechanism, everything works.
 Returns :
 Args    : Rfam::Entry pbjects (list of)

=cut

sub delete_Entry {
   my ($self, @ents) = @_;

   my ($dbh,
       @regions );

   $dbh = $self->open_transaction( 'rfam', 'rfam_reg_seed', 'rfam_reg_full' , 'rfam_database_links', 'rfam_literature_references');

   eval {
       foreach my $en (@ents) {
           my $acc = $en->acc;

           ####### AUTO NUM
           my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$acc'");
           $st->execute();
           my($temp_auto) = $st->fetchrow;
           $st->finish();

           my $rdb_auto_num = $temp_auto if(defined($temp_auto));

           $dbh->do("delete from rfam                       where auto_rfam = '$rdb_auto_num'");
           $dbh->do("delete from rfam_reg_seed              where auto_rfam = '$rdb_auto_num'");
           $dbh->do("delete from rfam_reg_full              where auto_rfam = '$rdb_auto_num'");
           $dbh->do("delete from rfam_database_links        where auto_rfam = '$rdb_auto_num'");
           $dbh->do("delete from rfam_literature_references where auto_rfam = '$rdb_auto_num'");
       }
   };

   $self->close_transaction( $@);
   $@ and $self->throw($@);

}

#soley to stop multiple checkins to RDB
sub add_lock {
    my ($self, $usr, $family)=@_;
    my ($lock,
	$locker,
	$error
	);

    $dbh = $self->connect('_lock'); #error raising is on

    eval{
	my $asth=$dbh->prepare("SELECT * from  _lock;");
	$asth->execute();
	$locker=$asth->fetchall_arrayref();
	$asth->finish();
    };if ($@){
	$self->disconnect;
	die "Failed to get the current RDB info: $@";
    }
    if (scalar(@$locker) > 1) {
	my $userlist;
	foreach my $r (@$locker){
	    $userlist.=join("\t", @$r);
	    $userlist.="\n";
	}
	$self->disconnect();
	$error= "More than one lock on the lock table-this is BAD\n
             $userlist If you know these locks can be removed use the -remove code? remove option at check in\n";
    }
        
    #should only be one entry;
    $user=$locker->[0];
    
    #if already locked-disconnect      
    if ($user->[0]){
	$self->disconnect();
	$lock->{'status'}=undef; #already locked
	$lock->{'locker'}=$user->[0]; 
	$lock->{'family'}=$user->[1];
	return $lock;
    }else{
	eval{
	    my $bsth=$dbh->prepare("Insert into _lock values (?,?)");
	    $bsth->execute($usr, $family);
	}; if ($@){
	    $error= "Failed to add lock to rdb for $usr $@";
	}else{
	    $self->disconnect();
	    $lock->{'status'}=1; #locked and ready to go;
	    $lock->{'locker'}=$usr;
	    $lock->{'family'}=$family;;
	    return $lock;
	}
	
    }
    $self->disconnect();
    print STDERR $error if $error;
}

sub remove_my_lock {
    my ($self, $usr, $lock)=@_;
    my $error;
    my $status=undef;
    $dbh = $self->connect(); #error raising is on
    
    eval{
	my $asth=$dbh->prepare("Delete from _lock  where locker=?");
	$asth->execute($usr);
	$asth->finish();
    }; if($@){
	$self->disconnect();
	return $lock;
    }
    $lock=undef;
    $self->disconnect();
    return $lock; #removed lock
}

#########
#UPWIKITEXT
#########

sub update_wikitext{
    my ($self, $entries) = @_;
    my ($wk_title,
	$status,
	$rdb_auto_wiki,
	$error
	);

   
    $dbh = $self->open_transaction( 'wikitext' );
       
    #prepare the queries we will need
    my $asth;
    unless( $asth = $dbh->prepare("SELECT auto_wiki FROM wikitext where title=?") ){
	$self->close_transaction($dbh->errstr);
    }

    my $bsth;
    unless( $bsth = $dbh->prepare("INSERT into wikitext (title) values (?)")){
	$self->close_transaction($dbh->errstr);
    }
    
   foreach my $en (@{$entries}) {
	
	eval {
	    if ($en->wiki_title()){
		$wk_title=$en->wiki_title();
	    }else{
		$status= "No wiki title available in DESC file. ";
		$wk_title='Not specified';
	    }
	    $asth->execute($wk_title);
	    my ($temp_auto) = $asth->fetchrow();
	    $asth->finish();
 
            if (defined $temp_auto){
		$status .= "Used an existing wiki title entry";
		$rdb_auto_wiki = $temp_auto;
	    }
	    else{
		#insert and get the new auto_wiki number;
		$bsth->execute($wk_title);
		$bsth->finish();
		$status="Added new title to wikitext table";
	    }
 
	}; #end of eval
	    if ( $@ ) {
		$error = "Could not do the insertion/update on the wikitext table [$@]";
		last;
	    }
    }#end of entries

    
    $self->close_transaction( $error );
    if (!$error){
	print STDERR "Completed wikitext: $status\n";
    }
    
}

#######
#RFAM
######

sub update_rfam{
   my ($self, $entries) = @_;
   my ($stat,
       $dbh,
       $rows, 
       $error, 
       $rdb_acc, 
       $rdb_id,  
       $rdb_desc, 
       $rdb_prevId,
       $rdb_modlen,
       $rdb_auto_num,
       $rdb_author,
       $rdb_comment,
       $rdb_align_method,
       $gathering_cutoff,
       $noise_cutoff,
       $rdb_previous_id,
       $rdb_source,
       $trusted_cutoff,
       $rdb_GA,
       $rdb_TC,
       $rdb_NC,
       $rdb_cmcalibrate,
       $rdb_cmbuild,
       $rdb_num_full,
       $rdb_num_seed,
       $rdb_entry_type,
       $rdb_structure_source,
       $wiki_title,
       $rdb_auto_wiki,
       $rdb_states,
       $rdb_nodes,
       $rdb_species,
       $rdb_tax_domain,
       $rdb_tax_root,
       $rdb_full_sscons,
       $rdb_reference_structure,
       $rdb_reference_sequence,
       $rdb_structure_annotations
      );


   $dbh = $self->open_transaction( 'rfam', 'wikitext' );
   $dbh->{PrintError}=1;
   #prepare the queries we will need
   my $asth;
   unless( $asth = $dbh->prepare("Select auto_rfam from rfam where rfam_acc = ?") ){
       $self->close_transaction($dbh->errstr);
   }
   my $csth;
   unless( $csth = $dbh->prepare("select auto_wiki from wikitext where title = ?") ){
       $self->close_transaction($dbh->errstr);
   }

   my $bsth;
   unless( $bsth = $dbh->prepare($self->__replace_sql('rfam', 28))){
       $self->close_transaction($dbh->errstr);
   }

   
   foreach my $en (@{$entries}) {
                  
       eval {
	   
	   #get values from the entry obj
	   $rdb_acc = $en->acc;
	   $rdb_id = $en->id();
	   $rdb_auto_num = $en->auto_rfam;
           
	   if ($en->wiki_title()){
	       $wiki_title=$en->wiki_title();
	   }else{
	       $wiki_title='Not specified';
	   }

           ####### AUTO NUM 
           #get the existing auto_rfam from rdb if it exists if new family
           my $st = $dbh->prepare("SELECT auto_rfam from rfam where rfam_acc = '$rdb_acc'");
	   $st->execute();
	   my($temp_auto) = $st->fetchrow;
	   $st->finish();

	   $rdb_auto_num = $temp_auto if(defined($temp_auto)); #note in this case it can be undefined.
	   
	   #get the relevant auto_wiki if it exists
       	   $csth->execute($wiki_title);
	   $rdb_auto_wiki= $csth->fetchrow;
	   $csth->finish();
    
	   $rdb_desc = $en->description;
	   $rdb_author = $en->author;
	   $rdb_comment = $en->comment;
           $rdb_previous_id=$en->prevId(),     

	   $rdb_align_method = $en->alignmethod();
	   $rdb_GA = $en->gathering_cutoff();
	   $rdb_TC = $en->trusted_cutoff();
	   $rdb_NC = $en->noise_cutoff();
	      
	   $rdb_previous_ids = $en->previous_ids();
	   $rdb_source = $en->source();
	   $rdb_structure_source = $en->structure_source();
	   foreach my $line ($en->each_build_line) {
	       $rdb_cmcalibrate .= $line if (($rdb_cmbuild) &&  (!$rdb_cmcalibrate) );
	       $rdb_cmbuild .= $line if   (!$rdb_cmbuild)  ;
	   }
	   $rdb_num_seed = $en->num_seqs_in_seed();
	   $rdb_num_full = $en->num_seqs_in_full();
	   $rdb_entry_type = $en->entry_type;
	   
       };
       if ($@) {
	   $error = "Could not fetch all the needed data from the rfam entry [$@]";
	   last;
       }

       eval{
       $self->report_mode and 
	   printf STDERR "Inserting into %s.rfam from entry %s\n", $self->_database_name, $rdb_acc;
       
       
           $bsth->execute( $rdb_auto_num, 
			   $rdb_auto_wiki,
			   $rdb_acc, 
			   $rdb_id, 
			   $rdb_desc, 
	 		   $rdb_author,			  
			   $rdb_source,
			   $rdb_align_method,			 			   
			   $rdb_GA,			   
			   $rdb_TC,			   
			   $rdb_NC,
			   $rdb_comment,
			   $rdb_previous_id,
			   $rdb_cmbuild,
			   $rdb_cmcalibrate,
			   $rdb_num_seed, 
			   $rdb_num_full,
	                   $rdb_entry_type,
			   $rdb_structure_source,
			   $rdb_states,
			   $rdb_nodes,
			   $rdb_species,
			   $rdb_tax_domain,
			   $rdb_tax_root,
			   $rdb_full_sscons,
			   $rdb_reference_structure,
			   $rdb_reference_sequence,
			   $rdb_structure_annotations
			    );
       $rows += $bsth->rows;
   }; if ($@){
           $error = "Could not do the insertion/update on the rfam table $@";
	   last;
       }
     
   }#each entry obj
   
   $self->close_transaction( $error );
   if (!$error){
       print STDERR "Completed rfam: $rows rows added\n";
   }
   return $rows;

}

sub fix_tax_and_species {
    my($self, @array) = @_;
    warn "Rfam::Entry_RCS::fix_tax_and_species() is deprecated - you're code is now probably broken!";
    return 0;
}


###########################################
#
# CHECK IN FULL
#
###########################################

#UPSMALL_FAM: this update method only to be used for smaller families as it loads directly into RDB
sub update_rfam_reg_full {
   my ($self, $entries) = @_;
   my ($error, 
       $rows,
       $dbh,
       $stat,
       $full_ss,
       $full_structure,
       $counter,
       $rdb_acc, 
       $rdb_id, 
       $rdb_auto_num,
       $count,
       $rdb_mode,
       $rdb_significant,
       $rdb_type, 
       $rdb_full_string,
       $rdb_evalue,
       @regions,
       %store_seed, @seed_region, $seed_acc,
       %store_rfamseq,  
       $no_seq_count,
       $s_evalues
       );
   
   $dbh = $self->open_transaction( 'rfam_reg_full', 'rfam', 'rfamseq' );
   
   foreach my $en (@{$entries}) {
     
     eval {
       
       $rdb_acc = $en->acc;
       $rdb_id = $en->id();
       $rdb_auto_num = $en->auto_rfam;
       
       #this method loads in the full align!!
       $full_ss=$en->full_strings();
       $full_structure=$full_ss->{'sscons'};
       #get the evalues
       $s_evalues=$en->scores_evalue();
       if (! $s_evalues) {
	   $error="Problem obtaining the data from scores_evalue;";
	   last;
       }

       #get auto-rfam for this family 
       my $asth = $dbh->prepare("SELECT auto_rfam from rfam where rfam_acc = '$rdb_acc'");
       $asth->execute();
       my($temp_auto) = $asth->fetchrow;
       $asth->finish();
	   
       $rdb_auto_num = $temp_auto if(defined($temp_auto));  
              
       #update the ss cons line in the rfam table
       my $bsth=$dbh->prepare("Update rfam set full_structure=? where auto_rfam=?");
       $bsth->execute($full_structure, $rdb_auto_num);
       $counter += $bsth->rows;
       $bsth->finish();
       
       if ( $counter != 1 ){
	   $error= "ERROR: Updating the rfam table with full_structure for $rdb_acc failed $!";
	   last;
       }

       #delete exsiting data before filling the table with data
       $dbh->do("delete from rfam_reg_full where auto_rfam = '$rdb_auto_num' ");

       #load up the seed annotations in order to asign data for type field
       @seed_region=$en->annotated_regions('SEED');
       foreach my $s (@seed_region){
	  if( $s->seq_name =~ /^(\S+)\.\d+/ ) {
	       $seed_acc = $1;
	   }
	   else {
	       $seed_acc = $reg->seq_name;
	   } 
	  $store_seed{$seed_acc}{$s->from}{$s->to}=1;
       }

       #collate the data for each region and fill rfam_reg_full
       @regions = $en->annotated_regions('FULL');
       
       foreach my $reg (@regions) {
	   my $rfamseq_acc;     # should be embl acc not acc.ver
	   if( $reg->seq_name =~ /^(\S+)\.\d+/ ) {
	       $rfamseq_acc = $1;
	   }
	   else {
	       $rfamseq_acc = $reg->seq_name;
	   }
	   #padded seq string from full align
	   $rdb_full_string=$full_ss->{$rfamseq_acc}->{$reg->from}->{$reg->to};
	   #type
	   if (defined $store_seed{$rfamseq_acc}{$reg->from}{$reg->to} ){
	       $rdb_type='seed';
	   }else{
	       $rdb_type='full';
	   }
	   #get the evalue from the scores_evalue file
	   $rdb_evalue=$s_evalues->{$rfamseq_acc}->{$reg->from}->{$reg->to};

	   if (defined($store_rfamseq{$rfamseq_acc})) {
	       $rfamseq_auto = $store_rfamseq{$rfamseq_acc};
	   } else {
	       my $st = $dbh->prepare("SELECT auto_rfamseq from rfamseq where rfamseq_acc = '$rfamseq_acc'");
	       $st->execute();
	       $rfamseq_auto = $st->fetchrow;
	       print  STDERR"\tERROR: full $rfamseq_acc is NOT in the rfamseq  table. This record has not been added \n" if (!$rfamseq_auto);
	       $no_seq_count++  if (!$rfamseq_auto);
	       next if (!$rfamseq_auto);
	       $st->finish();
	       $store_rfamseq{$rfamseq_acc} = $rfamseq_auto;
	   }
  
	   if (not defined $stat) {
	       $stat = $dbh->prepare( $self->__insert_sql( 'rfam_reg_full', 11));
	   }
	   eval {
	       $stat->execute($rdb_auto_num,
			      $rfamseq_auto,
			      '',
			      $reg->from, 
			      $reg->to,
			      $reg->bits_score,
			      $rdb_evalue,
			      $rdb_type,
			      '',
			      '',
			      $rdb_full_string
			      );
	           
	       $rows += $stat->rows;
	       $stat->finish();
	   };
	 if ($@) {
	    $error = "Could not insert data into rfam_reg_full table [$@]";
	    last;
	 }
      
       }#end of regions
 
     };#main eval
     if ($@){
	 $error="ERROR:Problems obtaining and loading data for rfam_reg_full $@";
	 last;
     }     
   } #end of @entries
       
   if (!$no_seq_count){$no_seq_count=0};
   $self->close_transaction( $error );
   if (!$error){
       print STDERR "Completed rfam_reg_full: $rows rows added $no_seq_count missing \n";
   }
 }

##UPBIG_FAM:this method only to be used for large families: no lock placed on RDB

sub collate_large_fam_reg_full {
   my ($self, $entries) = @_;
   my ($error,
       $data,
       $dbh,
       $stat,
       $counter,
       $full_ss,
       $full_structure,
       $rdb_acc, 
       $rdb_id, 
       $rdb_auto_num,
       $count,
       $rdb_mode,
       $rdb_significant,
       $rdb_type,
       $rdb_full_string,
       $rdb_evalue,
       @regions,
       %store_seed, @seed_region, $seed_acc,
       %store_rfamseq, 
       %store_rfam, 
       $no_seq_count,
       $s_evalue
       );
        
   if (scalar(@$entries) >1){
       print STDERR "This method collate_large_fam_reg_full cannot currently deal with parsing more than one entry at a time\n";
       return;
   }

   $dbh = $self->connect();

   foreach my $en (@{$entries}) {
     
     eval {
      
       $rdb_acc = $en->acc;
       $rdb_id = $en->id();
       $rdb_auto_num = $en->auto_rfam;
       $full_ss=$en->full_strings();
       $full_structure=$full_ss->{'sscons'};
        #get the evalues
       $s_evalues=$en->scores_evalue();
       if (! $s_evalues) {
	   $error="Problem obtaining the data from scores_evalue;";
	   last;
       }
       my $asth = $dbh->prepare("SELECT auto_rfam from rfam where rfam_acc = ?");
       $asth->execute($rdb_acc);
       my($temp_auto) = $asth->fetchrow;
       $asth->finish();
	   
       $rdb_auto_num = $temp_auto if(defined($temp_auto));  
              
       #this is stored so we can add the ss_cons line to the rfam table;
       push(@data, $rdb_auto_num);

       #load up the seed annotations in order to delete data before the load
       
       @seed_region=$en->annotated_regions('SEED');
       
       foreach my $s (@seed_region){
	  if( $s->seq_name =~ /^(\S+)\.\d+/ ) {
	       $seed_acc = $1;
	   }
	   else {
	       $seed_acc = $reg->seq_name;
	   } 
	  $store_seed{$seed_acc}{$s->from}{$s->to}=1;
       }
       eval{
	   #LOCK WHILE  INSERTING 
	   $dbh->do("Lock table rfam write");
	   
	   #update the ss cons line in the rfam table
	   my $bsth=$dbh->prepare("Update rfam set full_structure=? where auto_rfam=?");
	   $bsth->execute($full_structure, $rdb_auto_num);
	   $counter += $bsth->rows;
	   $bsth->finish();
	   
	   $dbh->do("UNLOCK tables");
       }; if ($@){ 
	   print STDERR "ERROR here\n"; $error="Problem trying to lock and unlock tables\n";
	   last;
       }
       
       @regions = $en->annotated_regions('FULL');
       foreach my $reg (@regions) {
	   my $rfamseq_acc;     # should be embl acc not acc.ver
	   if( $reg->seq_name =~ /^(\S+)\.\d+/ ) {
	       $rfamseq_acc = $1;
	   }
	   else {
	       $rfamseq_acc = $reg->seq_name;
	   }
	   #define type 
	   if (defined $store_seed{$rfamseq_acc}{$reg->from}{$reg->to} ){
	       $rdb_type='seed';
	   }else{
	       $rdb_type='full';
	   }
	   #full seq string
	   $rdb_full_string=$full_ss->{$rfamseq_acc}->{$reg->from}->{$reg->to};

	   #get the evalue from the scores_evalue
	   $rdb_evalue=$s_evalues->{$rfamseq_acc}->{$reg->from}->{$reg->to};
	   
	   if (defined($store_rfamseq{$rfamseq_acc})) {
	       $rfamseq_auto = $store_rfamseq{$rfamseq_acc};
	   } else {
	       my $st = $dbh->prepare("SELECT auto_rfamseq from rfamseq where rfamseq_acc = '$rfamseq_acc'");
	       $st->execute();
	       $rfamseq_auto = $st->fetchrow;
	       print  STDERR "\tERROR: full $rfamseq_acc is NOT in the rfamseq  table. This record has not been added \n" if (!$rfamseq_auto);
	       $no_seq_count++  if (!$rfamseq_auto);
	       next if (!$rfamseq_auto);
	       $st->finish();
	       $store_rfamseq{$rfamseq_acc} = $rfamseq_auto;
	   }
       
	   #generate the data structure to return;
	   #empty values=auto_genome, genome_start, genome_end
           
	   my $string=join("\t", $rdb_auto_num, $rfamseq_auto, '', $reg->from, $reg->to, $reg->bits_score, $rdb_evalue, $rdb_type, '',  '', $rdb_full_string);
	   push (@$data, $string);
       }#end of regions for $entry;
    
     };#end of eval for entries
     if ($@){
	 $error="Error with generating data for the rfam_reg_full table $@";
	 last;
	 }#main eval

   } #end of all entries


   $self->disconnect();
   if (!$error){
       my $c=(scalar(@data))-1; #has a header we dont want to count
       print STDERR "Completed collation of rfam_ref_full data: $c collated and $no_seq_count missing\n";
   }
   return $data;
}

##load rfam_reg_full file
sub load_rfam_reg_full_file_to_rdb {

  my ($self, $file, $table_name, $auto_rfam) = @_;
   my ($rows,
       $error,
       $dbh);
  
  $dbh = $self->open_transaction( 'rfam_reg_full');
   
  if (! -e $file) {
       die "Load_ $table_name _from_file error: Could not find file $file";
  }
  if (!$table_name) {
       die "load_ $table_name _from_file error: Did not pass the table_name";
  }
  eval{
  my $asth=$dbh->prepare("Delete from rfam_reg_full where auto_rfam=?");
  $asth->execute($auto_rfam);   
  $asth->finish();

  $self->report_mode and 
       printf STDERR "Loading data to rfam_reg_full";
   
       my $load=$dbh->prepare("LOAD data INFILE '$file' into table rfam_reg_full");
       $rows = $load->execute( );
       $load->finish();
   };if ($@){
       $error="Problems loading and re-loading rhe rfam_reg_full data $@";  
   }

  $self->close_transaction( $error );
  if (!$error){
      print STDERR "Loaded data into reg_full for big fam\n";
  }
  return $rows;
   
}



###########################################
#
# CHECK IN SEED
#
###########################################

#jd7- tried to move all the prepare statments outside of the regions loop
#get a seg fault error which couldnt be fixed.
sub update_rfam_reg_seed {
   my ($self, $entries) = @_;
   
   my ($error, 
       $dbh,
       $rows,
       $rdb_acc, 
       $rdb_id, 
       $rdb_auto_num,
       $count,
       $rdb_mode,
       $rdb_significant,
       $rdb_in_full,       
       @regions, $from, $to,
       %store_rfamseq, %store_rfam, $no_seq_count);
   
   
   
   $dbh = $self->open_transaction( 'rfam_reg_seed', 'rfam', 'rfamseq' );

   #prepare sql queries
   my $asth;
   unless( $asth = $dbh->prepare("SELECT auto_rfam from rfam where rfam_acc = ?") ){
	print STDERR "fail prepare\n";
	$self->close_transaction($dbh->errstr);
   }

   my $csth;
   unless( $csth = $dbh->prepare( $self->__insert_sql( 'rfam_reg_seed', 4)) ){
       print STDERR "fail prepare\n";
       $self->close_transaction($dbh->errstr);
   }
       
   foreach my $en (@{$entries}) {
  
     eval {
       
       $rdb_acc = $en->acc;
       $rdb_id = $en->id();
       $rdb_auto_num = $en->auto_rfam;
  
       #get auto_rfam 
       $asth->execute($rdb_acc);
       my ($temp_auto)= $asth->fetchrow;
       $asth->finish();
       $rdb_auto_num = $temp_auto if(defined($temp_auto));   
       
       #delete existing data
       $dbh->do("DELETE from rfam_reg_seed where auto_rfam='$rdb_auto_num'");
           
       #load new regions       
       @regions = $en->annotated_regions('SEED');
   
       foreach my $reg (@regions) {
	   my $rfamseq_acc;     # should be embl acc not acc.ver
	   if( $reg->seq_name =~ /^(\S+)\.\d+/ ) {
	       $rfamseq_acc = $1;
	   }
	   else {
	       $rfamseq_acc = $reg->seq_name;
	   }
	   
	   if (defined($store_rfamseq{$rfamseq_acc})) {
	       $rfamseq_auto = $store_rfamseq{$rfamseq_acc};
	       
	   }else {
               #moving this prepare outide the regions loop generates a segmentation fault..
     
	       my $bsth=$dbh->prepare("SELECT auto_rfamseq from rfamseq where rfamseq_acc = '$rfamseq_acc' ");
	       $bsth->execute();
	       $rfamseq_auto = $bsth->fetchrow();
	       $bsth->finish();
	       if (!$rfamseq_auto){
		   print STDERR "\tERROR: seed $rfamseq_acc is NOT in the rfamseq table. This region  cannot been added \n";
		   ++$no_seq_count;
		   next;
	       }
	       $store_rfamseq{$rfamseq_acc} = $rfamseq_auto;
	   } 
 
	    eval {
	       $csth->execute($rdb_auto_num,  $rfamseq_auto,  $reg->from, $reg->to );
	       $rows += $csth->rows;
	       $csth->finish();
	   };
	   if ($@) {
	       $error = "Could not insert data into rfam_reg_seed table [$@]";
	       last; 
	   }
   
       } #end each region
   }; #eval each family entry
     if ($@) {
	 $error = "Problem family eval[$@]";
	 last;
     } 
     
 }# end of each entry
   if (!$no_seq_count){$no_seq_count=0};
   $self->close_transaction( $error ); #wont commit data until reaches this point.
   if (!$error){
       print STDERR "Completed rfam_reg_seed: $rows rows added, $no_seq_count missing\n";
   }
}


########### NEW SUBS ############

sub update_literature_references {
  
  my ($self, $entries) = @_;
  my ($stat,
      $dbh,
      $rows, 
      $error, 
      $rdb_acc, 
      $rdb_id, 
      $rdb_auto_num
     );
  
  $dbh = $self->open_transaction( 'rfam', 'rfam_literature_references', 'literature_references' );
  
  foreach my $en (@{$entries}) {
     
   
      $rdb_acc = $en->acc;
      $rdb_id = $en->id();
      
      ####### AUTO NUM 
      my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
      $st->execute();
      $rdb_auto_num = $st->fetchrow;
      $st->finish();
      
      if (! $rdb_auto_num ){
	  $error= "ERROR: No auto_rdb_number obtained for this family $rdb_acc$!";
	  last;
      }
    
    eval {
      $self->report_mode and 
	printf STDERR "Deleting from %s.rfam_literature_references for entry %s\n", $self->_database_name, $rdb_auto_num;
      
      $dbh->do("delete from rfam_literature_references where auto_rfam = '$rdb_auto_num'") if ($rdb_auto_num);
    };
  
    ## If there is a literature reference
    if ($en->each_reference()) {
      my $count = 1;
      # For each reference
      foreach my $ref ( $en->each_reference() ) {
	my($comment, $medline, $authors, $journal, $title);
	

	$medline = $ref->medline();
	$title = $ref->title();
	
	$authors = $ref->authors;
	$journal = $ref->location();
	
	$title = $dbh->quote($title);
	
	$authors = $dbh->quote($authors);
	$journal = $dbh->quote($journal);
	
	### SEE IF MEDLINE ALREADY EXISTS in literature_reference table
	my $st = $dbh->prepare("select auto_lit from literature_references where medline = '$medline'");
	$st->execute();
	my($auto_lit) = $st->fetchrow;
	$st->finish();
	## Not already in medline so add it
	if (!$auto_lit) {
	  ## need to add the lit reference
	  eval {
	    $stat = undef;
	    if (not defined $stat) {
	      my $sql = "INSERT INTO literature_references VALUES ( NULL , $medline, $title, $authors, $journal)";
	      $stat = $dbh->prepare($sql);
	    }
	    
	    $stat->execute();
	    $auto_lit = $stat->{mysql_insertid}; ## get the auto number
	    $rows += $stat->rows;
	  };
	  
	}
	### ok so added lit refs if we had to now add to pfamA_lit_ref whatsit table :-)
	eval {
	  $stat = undef;
	  if (not defined $stat) {
	    $stat = $dbh->prepare($self->__replace_sql('rfam_literature_references', 4));
	  }
	  
	  $stat->execute( $rdb_auto_num, 
			  $auto_lit,
			  $comment,
			  $count);
	  
	};
	if ($@) {
	  $error = "Could not do the insertion/update on the rfam_lit_refs table [$@]";
	  last;
	}
	
	$count++;	
      }				#/ for EACH REFERENCE     
      
    }				#/ if REFERENCES   
  }				#/ end for each entries
  
  $self->close_transaction( $error );
  if (! $error){
      print STDERR "Completed literature_references:\n";
  }
  return $rows;
}

sub update_rfam_database_links {

 
  my ($self, $entries) = @_;
  my ($stat,
      $dbh,
      $rows, 
      $error, 
      $rdb_acc, 
      $rdb_id, 
      $rdb_auto_num
     );
  
  $dbh = $self->open_transaction( 'rfam', 'rfam_database_links' );
  
  foreach my $en (@{$entries}) {
    
    eval {
      $rdb_acc = $en->acc;
      $rdb_id = $en->id();
      
      ####### AUTO NUM 
      my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
      $st->execute();
      my($temp_auto) = $st->fetchrow;
      $st->finish();
      
      $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
           
    };
       
    eval {
      $self->report_mode and 
	printf STDERR "Deleting from %s.rfam_database_references for entry %s\n", $self->_database_name, $rdb_auto_num;
      
      $dbh->do("delete from rfam_database_links where auto_rfam = '$rdb_auto_num'");
    };
    ## If there is a database reference
    if ($en->each_dblink ()) {
      foreach my $link ($en->each_dblink ) {
	my($database, $title, $db_link, @other_info, $all_other);
	$database = $link->database;
	$db_link = $link->primary_id();
        $title=$link->comment();
	eval {
	  $stat = undef;
	  if (not defined $stat) {
	    $stat = $dbh->prepare($self->__replace_sql('rfam_database_links', 5));
	  }

	  $stat->execute( $rdb_auto_num, 
			  $database,
			  $title,
			  $db_link,
			  $all_other);
	  
	};
	if ($@) {
	  $error = "Could not do the insertion/update on the rfam_lit_refs table [$@]";
	  last;
	}
	
      }				#/ for EACH REFERENCE     
      
    }				#/ if REFERENCES   
  }				#/ end for each entries
  
   $self->close_transaction( $error );
   if (!$error){
       print STDERR "Finished the database links\n";
   }
   
   return $rows;
}





############# / end of new subs ##########




#=head2 update_pfamseq

# Title   : update_pfamseq
# Usage   :
# Function:
#    This function takes a list of hash references each with 4 fields:
#    1. acc - accession number
#    2. id - identifier
#    3. desc - description for entry
#    4. len - length of associated sequence
#    5. org - hist organism os sequence
    
#    and inserts the information into the pfamseq RDB table
# Returns :
#    The number of rows inserted/_updated (should be the size of pfamseq);
#    Throws an exception if something went wrong
# Args    :
#    1. A ref. to a list of hash refs

# Note   :
#    1. If the transaction handle is undefined, a transaction is created and committed
#    for just this _update. If a handle is given, then it is assumed that the update
#    is part of a larger transaction.

#=cut


# sub add_rfamseq {
#     my $self = shift;
#     my @seqs = @_;

#     $dbh = $self->open_transaction( 'rfamseq' );

#     my $error = "";
#     my $rows = 0;
#     foreach my $seq ( @seqs ) {
# 	my $auto_id = '';
# 	my $st = $dbh->prepare( 'select auto_rfamseq from rfamseq where rfamseq_acc = ?' );
# 	$st->execute( $seq->accession_number );
# 	($auto_id) = $st->fetchrow;
# 	$st->finish();

# 	if( $auto_id ) {
# 	    $st = $dbh->prepare($self->__replace_sql('rfamseq', 9));
# 	}
# 	else {
# 	    $st = $dbh->prepare($self->__insert_sql('rfamseq', 9));
# 	}

# 	my $OS = "unknown";
# 	my $OC = "unknown";

# 	# stolen from Bio::SeqIO::embl->write_seq
# 	my $spec = $seq->species;
# 	if( $spec ) {
# 	    my($species, @class) = $spec->classification();
# 	    my $genus = $class[0];
# 	    $OS = "$genus $species";
# 	    if (my $ssp = $spec->sub_species) {
# 		$OS .= " $ssp";
# 	    }
# 	    if (my $common = $spec->common_name) {
# 		$OS .= " ($common)";
# 	    }
# 	    $OC = join('; ', reverse(@class)) .'.';
# 	}
# 	####

# 	$st->execute( $auto_id,
# 		      $seq->id,
# 		      $seq->accession_number,
# 		      $seq->description,
# 		      $OS,
# 		      $OC,
# 		      $seq->seq_version,
# 		      join( ';', $seq->get_secondary_accessions ),
# 		      ''
# 		      );

# 	$rows += $st->rows;
#     };
#     if ($@) {
# 	$error = "Could not do the insertion on the rfamseq table [$@]";
# 	last;
#     }
	 
#     $self->close_transaction( $error );
#     $error and $self->throw( $error );

#     $self->report_mode and 
# 	printf STDERR "Just added %d records to %s.pfamseq\n", $rows, $self->_database_name();

#     return $rows;
# }


1;
