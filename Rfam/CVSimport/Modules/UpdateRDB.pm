
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

UpdateRDB

=head1 DESCRIPTION

This module contains the basic functionality for updating the Pfam 
relational database. It relies upon the Bio::Pfam::PfamRDB module
to provide basic connection facilities


=cut


# Let the code begin...
    

package UpdateRDB;

use vars qw($AUTOLOAD 
	    @ISA 
	    @EXPORT_OK ); 

#use strict;
use Rfam::RfamRDB;

@ISA = qw(Rfam::RfamRDB);

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


=head2 check_in_EntryA

 Title   : check_in_entry
 Usage   : $updatedb->check_in_EntryA( $entry );
 Function:
    This function updates the underlying relational database
    by loading the given family through the middleware layer
    and inserting all relevant records into the correct tables,
    deleting records if necesary
 Returns :
 Args    : Bio::Pfam::EntryA

=cut

sub check_in_Entry {
  my ($self, @en ) = @_;
  
  my ($dbh,
      @regions );
  
 
  $dbh = $self->open_transaction('rfam', 'rfam_reg_seed', 'rfam_reg_full', 'rfamseq', 'rfam_literature_references', 'literature_references' , 'rfam_database_links');
  
    eval {
      foreach my $ent (@en) {
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
   $@ and $self->throw($@);

   return $rows;
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



sub check_in_dead_EntryA {
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

 $dbh = $self->open_transaction( 'pfamA', 'dead_families' );

$self->report_mode();
 
 eval {
   foreach my $en (@en) {
 
     $rdb_acc = $en->acc;
    
     $rdb_id = $en->id();
     $rdb_comment = $en->comment();
     $rdb_forward = $en->each_forward_acc();
     foreach my $forward ($en->each_forward_acc()) {
        $rdb_forward = $rdb_forward . ";$forward";
    }

          
     ### GET COMMENT
     foreach my $comment ( $en->ann->flatcomment->each_flat() ) {
       $rdb_comment .= " " . $comment;
     }
     
     
     eval {
       if (not defined $stat) {
	 $stat = $dbh->prepare($self->__replace_sql('dead_families', 4));
       }
       
       $stat->execute( $rdb_acc, 
		       $rdb_id, 
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

 ### No errors so delete the pfamA entry from RDB !
 if(!$error) {
   $self->delete_EntryA(@en);
 }


}


sub query {
  my ($self, $var) = @_;
my $dbh = $self->open_transaction( 'rfam' , 'rfam_database_links');
my ($stat);
	   my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = 'RF00001'");
	   $st->execute();
	   my($temp_auto) = $st->fetchrow;
	   $st->finish();
#	   print "st: $st \n";
	  my $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
#print "AUTO: $rdb_auto_num \n";

       eval {
	   if (not defined $stat) {
	       $stat = $dbh->prepare($self->__replace_sql('rfam_database_links', 5));
	   }
	   print "ADDING \n";

	   $stat->execute( "10000", 
			  "TEST", 
			   "this is a super test", 
			   "", 
			   ""
			 );
	   $rows += $stat->rows;
       };
       
       if ($@) {
	 
	   $error = "Could not do the insertion/update on the pfamA table [$@]";
	   
	   last;
       }



}

sub update_rfam{
   my ($self, $entries) = @_;
   my ($stat,
       $dbh,
       $rows, 
       $error, 
       $rdb_acc, 
       $rdb_id,  
       $rdb_desc, 
       $rdb_modlen,
       
       $rdb_auto_num,
       $rdb_author,
       $rdb_comment,
       $rdb_align_method,
       $gathering_cutoff,
       $noise_cutoff,
       $rdb_previous_ids,
       $rdb_source,
       $trusted_cutoff,

       $rdb_GA,

       $rdb_TC,

       $rdb_NC,

    
       $rdb_cmcalibrate,
       $rdb_cmbuild,
       $rdb_num_full,
       $rdb_num_seed,

      );

   $dbh = $self->open_transaction( 'rfam' );
 
   foreach my $en (@{$entries}) {
   
       eval {

	   $rdb_acc = $en->acc;
	
	   $rdb_id = $en->id();
	   $rdb_auto_num = $en->auto_pfamA;

	   ### NEW AUTO NUMBER

 
	   ####### AUTO NUM 
	   my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
	   $st->execute();
	   my($temp_auto) = $st->fetchrow;
	   $st->finish();
	  $rdb_auto_num = $temp_auto if(defined($temp_auto)); 



	   if (defined $en->ann) {
	    
	   
	       $rdb_desc = $en->ann->description;

	     ### NEW RDB PARAMS

	       $rdb_author = $en->author;  # author

	      ### GET COMMENT 
	       foreach my $comment (  $en->ann->each_Comment ) {
		$rdb_comment .= " " . $comment->text();
	       }
	
	     

	       $rdb_align_method = $en->alignmethod();
	       $rdb_GA = $en->gathering_cutoff();
	       $rdb_TC = $en->trusted_cutoff();
	       $rdb_NC = $en->noise_cutoff();
	      


	       $rdb_previous_ids = $en->previous_ids();
	       $rdb_source = $en->source();
	      
	       foreach my $line ($en->each_build_line) {
		 $rdb_cmcalibrate .= $line if (($rdb_cmbuild) &&  (!$rdb_cmcalibrate) );
		 $rdb_cmbuild .= $line if   (!$rdb_cmbuild)  ;
		
		
	       }
	     

	       $rdb_num_seed = $en->num_seqs_in_seed();
	       $rdb_num_full = $en->num_seqs_in_full();
	       

	   }

	       $rdb_modlen = $en->model_length;
	 
	 
       };
       if ($@) {
	   $error = "Could not fetch all the needed data from the rfam entry [$@]";
	   last;
       }
    
       $self->report_mode and 
	   printf STDERR "Inserting into %s.rfam from entry %s\n", $self->_database_name, $rdb_acc;
       
    
       eval {
	   if (not defined $stat) {
	       $stat = $dbh->prepare($self->__replace_sql('rfam', 18));
	   }
	  
	 #  print "ADDING DATA \n";
	   $stat->execute( $rdb_auto_num, 
			  $rdb_acc, 
			   $rdb_id, 
			   $rdb_desc, 
			   $rdb_modlen,
			   $rdb_author,			  
			   $rdb_source,
			   $rdb_align_method,			 			   
			   $rdb_GA,			   
			   $rdb_TC,			   
			   $rdb_NC,
			   $rdb_comment,
			   $rdb_previous_ids,
			   $rdb_cmbuild,
			   $rdb_cmcalibrate,
			   $rdb_num_seed, 
			   $rdb_num_full,
	                   ""
			 );
	   $rows += $stat->rows;
       };
       
       if ($@) {
	 
	   $error = "Could not do the insertion/update on the pfamA table [$@]";
	   
	   last;
       }
     
   }
   


   $self->close_transaction( $error );
   $error and $self->throw( $error );

   return $rows;
}



###########################################
#
# CHECK IN FULL
#
###########################################


sub update_rfam_reg_full {
   my ($self, $entries) = @_;
 
   
   my ($error, 
       $dbh,
       $stat,
       $rdb_acc, 
       $rdb_id, 
       $rdb_auto_num,
       $count,
       $rdb_mode,
       $rdb_significant,
       $rdb_in_full,       
       @regions, $from, $to,
       %store_rfamseq, %store_rfam, $no_seq_count);
   
   
   
   $dbh = $self->open_transaction( 'rfam_reg_full', 'rfam', 'rfamseq' );
   
   my (%hash);

   foreach my $en (@{$entries}) {
     
     eval {
       
       $rdb_acc = $en->acc;
       $rdb_id = $en->id();
       $rdb_auto_num = $en->auto_pfamA;
       
       
       ####### AUTO NUM 
       my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
       $st->execute();
       my($temp_auto) = $st->fetchrow;
       $st->finish();
       
       $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
       $dbh->do("delete from rfam_reg_full where auto_rfam = '$rdb_auto_num' ");
       





       my @regions = $en->annotated_regions('FULL');
       foreach my $reg (@regions) {
	 my $rfamseq_acc = $reg->seq_name;
	
	 
	 if (defined($store_rfamseq{$rfamseq_acc})) {
	   $rfamseq_auto = $store_rfamseq{$rfamseq_acc};
	 } else {
	   my $st = $dbh->prepare("select auto_rfamseq from rfamseq where rfamseq_acc = '$rfamseq_acc'");
	   $st->execute();
	   $rfamseq_auto = $st->fetchrow;
	   print  "ERROR: $rfamseq_acc is NOT in the rfamseq  table. This record has not been added \n" if (!$rfamseq_auto);
	   $no_seq_count++  if (!$rfamseq_auto);
	   next if (!$rfamseq_auto);
	   $st->finish();
	   $store_rfamseq{$rfamseq_acc} = $rfamseq_auto;
	 }

	 if (not defined $stat) {
	   $stat = $dbh->prepare( $self->__insert_sql( 'rfam_reg_full', 5));
	 }
	 eval {
	   $stat->execute($rdb_auto_num,
			  $rfamseq_auto, 
			  $reg->from, 
			  $reg->to,
			  $reg->bits_score
			 );
	   
	   
	   
	 };
	 if ($@) {
	   my  $error = "Could not insert data into rfam_reg_seed table [$@]";
	   print "ERROR: $error \n";
	   
	 }


       }


      



     };

   }


  
   $self->close_transaction( $error );



#print "Invalid seqs: $no_seq_count \n";

}


###########################################
#
# CHECK IN SEED
#
###########################################


sub update_rfam_reg_seed {
   my ($self, $entries) = @_;
 #  print "here \n";
   
   my ($error, 
       $dbh,
       $stat,
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
   
   my (%hash);

   foreach my $en (@{$entries}) {
     
     eval {
       
       $rdb_acc = $en->acc;
       $rdb_id = $en->id();
       $rdb_auto_num = $en->auto_pfamA;
       
       
       ####### AUTO NUM 
       my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
       $st->execute();
       my($temp_auto) = $st->fetchrow;
       $st->finish();
        
     
       $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
       $dbh->do("delete from rfam_reg_seed where auto_rfam = '$rdb_auto_num' ");
       
       my @regions = $en->annotated_regions('SEED');
       foreach my $reg (@regions) {
	 my $rfamseq_acc = $reg->seq_name;
	
	 
	 if (defined($store_rfamseq{$rfamseq_acc})) {
	   $rfamseq_auto = $store_rfamseq{$rfamseq_acc};
	 } else {
	   my $st = $dbh->prepare("select auto_rfamseq from rfamseq where rfamseq_acc = '$rfamseq_acc'");
	   $st->execute();
	   $rfamseq_auto = $st->fetchrow;
	   print  "ERROR: $rfamseq_acc is NOT in the rfamseq table. This record has not been added \n" if (!$rfamseq_auto);
	   $no_seq_count++  if (!$rfamseq_auto);
	   next if (!$rfamseq_auto);
	   $st->finish();
	   $store_rfamseq{$rfamseq_acc} = $rfamseq_auto;
	 }

	 if (not defined $stat) {
	   $stat = $dbh->prepare( $self->__insert_sql( 'rfam_reg_seed', 4));
	 }
	 eval {
	   $stat->execute($rdb_auto_num,
			  $rfamseq_auto, 
			  $reg->from, 
			  $reg->to
			 );
	   
	   
	   
	 };
	 if ($@) {
	   my  $error = "Could not insert data into rfam_reg_seed table [$@]";
	   print "ERROR: $error \n";
	   
	 }


       }

      



     };

   }



 
  
   $self->close_transaction( $error );



#print "Invalid seqs: $no_seq_count \n";

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
  
  $dbh = $self->open_transaction( 'rfam_literature_references', 'literature_references' );
  
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
	printf STDERR "Deleting from %s.rfam_literature_references for entry %s\n", $self->_database_name, $rdb_auto_num;
      
      $dbh->do("delete from rfam_literature_references where auto_rfam = '$rdb_auto_num'") if ($rdb_auto_num);
    };
  
    
    ## If there is a literature reference
    if ($en->ann->each_Reference()) {

      my $count = 1;
      # For each reference
      foreach my $ref ( $en->ann->each_Reference() ) {
	
	my($comment, $medline, $authors, $journal, $title);
	
	## Get out comment if there is one
	if (defined ($ref->comment() ) ) {
	#  print "COMMENT: " .$ref->comment() . " \n";
	  #foreach my $refcomm ( $ref->comment->each_flat() ) {
	   # $comment .=  $refcomm . " ";
	  $comment = $ref->comment();
	  #  chop($refcomm);
	#  }
	}
	


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
  $error and $self->throw( $error );
  
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
  
  $dbh = $self->open_transaction( 'rfam_database_links' );
  
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

    if ($en->ann->each_DBLink ()) {
      foreach my $link ($en->ann->each_DBLink ) {
#	print "MEAOW BOO HISS \n";
	my($database, $title, $db_link, @other_info, $all_other);
	$database = $link->database;
	$db_link = $link->primary_id();
#	@other_info = $link->each_additional();
#	foreach (@other_info) {#
#
#	  $all_other .= $_;
#	}
	
	

#	if (defined( $link->comment() )) {

#	  foreach my $linkcomm ($link->comment->each_flat() ) {
#	    $title = $linkcomm ;

#	  }
#	}

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
  $error and $self->throw( $error );
  
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

# Notes   :
#    1. If the transaction handle is undefined, a transaction is created and committed
#    for just this _update. If a handle is given, then it is assumed that the update
#    is part of a larger transaction.

#=cut


sub add_rfamseq {
   my ($self, $entries) = @_;
   my ($stat, $stat_add,
       $dbh,
       $rows, 
       $error, 
       $rdb_acc, 
       $rdb_id, 
       $rdb_desc, 
       $rdb_len, 
       $rdb_org,
      $rdb_auto,
      $rdb_crc64,
      $rdb_version,
      $rdb_is_fragment,
      $rdb_taxonomy,
      %store_rfamseq, $st);

   $dbh = $self->open_transaction( 'rfamseq' );
#print "EN : $entries \n";
#   open(_EN, $entries);
#   while(<_EN>) {
#     print "BLEE: $_ \n"; sleep 1;
#   }
#   close(_EN);
#   exit(0);
  open(_EN, $entries);
   while(<_EN>) {
  #   print "EN : $_ \n";
    # print "HERE $entries \n"; exit(0);

     my($temp, $rdb_id, $rdb_acc, $rdb_version, $rdb_desc,$rdb_os, $rdb_oc,  $rdb_prev);

  #   if (!$seed) {
  #     ($rdb_acc, $rdb_id) = split(/\s+/, $_);
     
   #  } else {
     chop($_);
     ($temp, $rdb_id, $rdb_acc, $rdb_version, $rdb_desc,$rdb_os, $rdb_oc,  $rdb_prev) = split(/\t/, $_);
     #print "ID: $rdb_id, ACC: $rdb_acc, VER: $rdb_version, DESC: $rdb_desc, OS: $rdb_os, OC: $rdb_oc,  PREV: $rdb_prev \n";
     $rdb_acc =~ s/\s+//g;
     $rdb_id =~ s/\s+//g;
  

     my $st = $dbh->prepare("select auto_rfamseq from rfamseq where rfamseq_acc = '$rdb_acc'");
     $st->execute();
     my($temp_auto) = $st->fetchrow;
     $st->finish();

     if ($temp_auto) {
       $rdb_auto = $temp_auto;
     } else {
       $rdb_auto = undef;
     }
   #  print "ACC: $rdb_acc:: auto: $rdb_auto \n";
#      if (!$temp_auto) {

##	print "$rdb_acc not in database \n";
#	next;

#       } else {
#	# print "$rdb_acc in RDB: $rdb_id, $rdb_acc, $rdb_version, $rdb_desc,$rdb_os, $rdb_oc,  $rdb_prev\n"; sleep 1;
#	 next;
#       }

    
       
     if (defined($store_rfamseq{$rdb_acc})) {
       
     } else {
       $store_rfamseq{$rdb_acc} = $rdb_acc;
       #print "HERE \n";
       if (!$rdb_auto) {
	# print "NO AUTO \n";
	 eval {
	   if (not defined $stat_add) {
	     $stat_add = $dbh->prepare($self->__insert_sql('rfamseq', 8));
	     #print "stat: $stat \n";
	   }
          print "ADDING: $rdb_acc \n";
	
	   $stat_add->execute($rdb_auto,
			  $rdb_id, 
			  $rdb_acc, 
			  $rdb_desc, 
			  $rdb_os,
			  $rdb_oc,
			  $rdb_version,
			  $rdb_prev
			 );
	   $rows += $stat_add->rows;
	 };
	 if ($@) {
	   $error = "Could not do the insertion on the rfamseq table [$@]";
	   last;
	 }
	 
	 
       } else {
	 
	 
	 # my $st = $dbh->prepare("select auto_rfamseq from rfamseq where rfamseq_acc = '$rdb_acc'");
	 # $st->execute();
	 #my($temp_auto) = $st->fetchrow;
	 #$st->finish();
	 #   if (!$temp_auto) {
#	 print "updating: $rdb_acc \n";
	 eval {
	   if (not defined $stat) {
	     $stat = $dbh->prepare($self->__replace_sql('rfamseq', 8));
	    # print "stat: $stat \n";
	   }
	   #print "UPDATING $rdb_auto , $rdb_acc \n";
	   $stat->execute($rdb_auto,
			  $rdb_id, 
			  $rdb_acc, 
			  $rdb_desc, 
			  $rdb_os,
			  $rdb_oc,
			  $rdb_version,
			  $rdb_prev
			 );
	   $rows += $stat->rows;

	   
	 };
	 if ($@) {
	   $error = "Could not do the update on the rfamseq table [$@]";
	   last;
	 }
	 
	 
	 
	 
	 
	 
	 
       }
       
     }

   } #/ end while

   $self->close_transaction( $error );
   $error and $self->throw( $error );

   $self->report_mode and 
       printf STDERR "Just added %d records to %s.pfamseq\n", $rows, $self->_database_name();

   return $rows;
}


######################################################################################
#
#  miRNA stuff
#
######################################################################################

sub  delete_mirna_tables{
  my($self) = @_;

  my ($dbh, $stat_add, $rows, $error, $auto_lit, $auto_mirna, $stat_lit, $stat_mirna);

 # print "ID: $id, ACC: $acc, $desc , $start, $end, $mature_name, $sequence \n";
   $dbh = $self->open_transaction( 'mirna' , 'mirna_literature_references', 'mirna_mature', 'mirna_species', 'mirna_database_links' );
  $dbh->do("delete from mirna");
  $dbh->do("delete from mirna_literature_references");
  $dbh->do("delete from mirna_mature");
  $dbh->do("delete from mirna_species");
  $dbh->do("delete from mirna_database_links");
  $self->close_transaction();
}


sub add_mirna {
  my($self, $id, $acc, $desc , $mature, $sequence, $refs, $database) = @_;

  my ($dbh, $stat_add, $stat_mat,$stat_data, $rows, $error, $auto_lit, $auto_mirna, $stat_lit, $stat_mirna);

 # print "ID: $id, ACC: $acc, $desc , $start, $end, $mature_name, $sequence \n";
   $dbh = $self->open_transaction( 'mirna' ,'literature_references', 'mirna_literature_references', 'mirna_mature', 'mirna_species', 'mirna_database_links'  );

  
  ######### UPDATE mirna 
  eval {
    if (not defined $stat_add) {
      $stat_add = $dbh->prepare($self->__insert_sql('mirna', 5));
      #print "stat: $stat \n";
    }
    #   print "ADDING: $rdb_acc \n";
    #  sleep 1;
    $stat_add->execute($auto_mirna,
		       $acc, 
		       $id, 
		       $desc, 
		       $sequence
		      );
    $rows += $stat_add->rows;
    $auto_mirna = $stat_add->{mysql_insertid}; ## get the auto number
  };
  if ($@) {
    $error = "Could not do the insertion on the mirna table [$@]";
    last;
  }
  

  my @mature_temp = @${mature};

foreach my $query_return (@mature_temp) {
  my %output = %{$query_return};


  ########## UPDATE mirna_mature  
  eval {
    if (not defined $stat_mat) {
      $stat_mat = $dbh->prepare($self->__insert_sql('mirna_mature', 4));
    }

    $stat_mat->execute($auto_mirna,
		       $output{'NAME'},
		       $output{'START'},
		       $output{'END'}
		      );
      $rows += $stat_add->rows;
  };
  if ($@) {
    $error = "Could not do the insertion on the mirna_mature table [$@]";
    last;
  }

}

######## UPDATE literature tables

my @refs_temp = @${refs};

foreach my $query_return (@refs_temp) {
  my %output = %{$query_return};
  
  my $medline = $output{MEDLINE};
  my $st = $dbh->prepare("select auto_lit from literature_references where medline = '$medline'");
  $st->execute();
  my($auto_lit) = $st->fetchrow;
  $st->finish();
  
  
  if (!$auto_lit) {
    eval {
      if (not defined $stat_lit) {
	$stat_lit = $dbh->prepare($self->__insert_sql('literature_references', 5));
	#print "stat: $stat \n";
      }
      #   print "ADDING: $rdb_acc \n";
      #  sleep 1;
    #  die "No auto_
      $stat_lit->execute($auto_lit,
			 $output{MEDLINE},
			 $output{TITLE},
			 $output{AUTHORS},
			 $output{JOURNAL}
			);
      $rows += $stat_lit->rows;
      
      $auto_lit = $stat_lit->{mysql_insertid}; ## get the auto number
      
    };
    if ($@) {
      $error = "Could not do the insertion on the literature_references table [$@]";
      last;
    }
  }
  
  
  
  
  eval {
    if (not defined $stat_mirna) {
      $stat_mirna = $dbh->prepare($self->__insert_sql('mirna_literature_references', 4));
      #print "stat: $stat \n";
    }
    #   print "ADDING: $rdb_acc \n";
    #  sleep 1;
    $stat_mirna->execute($auto_mirna,
			 $auto_lit,
			 $output{COMMENT},
			 $output{NUMBER}
			);
    $rows += $stat_mirna->rows;
  
}

  
};
if ($@) {
  $error = "Could not do the insertion on the mirna_literature_references table [$@]";
  last;
}







######## UPDATE DATABASE_LINKS tables

my @database_tmp = @${database};

foreach my $query_return (@database_tmp) {
  my %output = %{$query_return};

  my $db_id = $output{ID};
  my $db_link = $output{LINK};
  my $db_comment = $output{COMMENT};
  


  eval {
    if (not defined $stat_data) {
      $stat_lit = $dbh->prepare($self->__insert_sql('mirna_database_links', 5));
      #print "stat: $stat \n";
    }
    #   print "ADDING: $rdb_acc \n";
    #  sleep 1;
    $stat_lit->execute($auto_mirna,
		       $output{ID},
		       $output{COMMENT},
		       $output{LINK},
		       ""
		      );
    $rows += $stat_data->rows;
    
  
    
  };
  if ($@) {
    $error = "Could not do the insertion on the mirna_database_links table [$@]";
    last;
    }  





  
}


}

1;
