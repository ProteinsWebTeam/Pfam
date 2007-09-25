
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
 Returns :
 Args    : Rfam::Entry::Entry object

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


##############################################
#
# Jen#edited method to fill the genome_entry table with the genome_assemblies.rgp data
# and updating the rfamseq with auto_genome mapping
#
###############################################

##each genome_acc is entered into the genome_entry table
##and the auto_genome entry generated is used to update the rfamseq table with this 
##auto_genome value;
##nv each rfamseq_acc and auto_rfamseq are unique so dont need to use the starts and stops.

sub genomic_species_data {
  my($self, $ac, $de, $ci, $joined_tax, @rf) = @_;

  my ($dbh, $stat);

  $dbh = $self->open_transaction('genome_entry','rfam_reg_full' , 'rfamseq');
  my ($genome_auto);
  eval {
    if (not defined $stat) {
      $stat = $dbh->prepare($self->__replace_sql('genome_entry', 5 ));
    }

    $stat->execute( $genome_auto,
		    $ac,
		    $de,
		    $joined_tax,
		    $ci
		  );
    $rows += $stat->rows;
    $genome_auto = $stat->{mysql_insertid}; ## get the auto number
  };
  
  if ($@) {
    
    $error = "Could not do the insertion/update on the pfamA table [$@]";
    
    last;
  }

  foreach my $rfamseq_acc (@rf) {

      my $auto_sql = "select auto_rfamseq from rfamseq where rfamseq_acc = '$rfamseq_acc'";
      $stat = $dbh->prepare($auto_sql);
      $stat->execute();
      my $auto_rfamseq = $stat->fetchrow;
      $stat->finish();

      my $sql = "UPDATE rfam_reg_full set auto_genome = $genome_auto where auto_rfamseq = '$auto_rfamseq'";
      $stat = $dbh->prepare($sql);
      $stat->execute();
      $stat->finish();
      
  }

}


#################################################################
#
# jen ##New method to load chromosome build info  
# for all the genomes entered into the genome_entry table
# hte relevant rfamseq acc are mapped to this auto_genome number,
# not all auto_genomes have rfmaseqs mapped to it.
################################################################

sub chromosome_data {
  my($self, $ac, @chr) = @_;

  my ($dbh, $insert);

  $dbh = $self->open_transaction('genome_entry','rfamseq', 'chromosome_build');
 

  ##get the auto_genome number for this genome
    my $auto_sql = "select  auto_genome from genome_entry where genome_acc= '$ac' ";
    $stat = $dbh->prepare($auto_sql);
    $stat->execute();
    my $auto_genome= $stat->fetchrow;
    $stat->finish();

    print STDERR "autogenome=$auto_genome\n";
    print STDERR "number of entries should be", scalar(@chr), "\n";

  ##process eah GP line in @chr
  ##get the accesion number for the seqeunce to be mapped;
  foreach my $chrline (@chr){
      my ($xsome_start, $xsome_end, $gbacc, $clone_start, $clone_end, $strand) = @{$chrline}; 
    #  print STDERR join("|", $xsome_start, $xsome_end, $gbacc, $clone_start, $clone_end, $strand), "\n";
      
      ##get the relevant auto_rfamseq number for this sequence ##this should be unique i think...
      my $sql = "select auto_rfamseq from rfamseq where rfamseq_acc= '$gbacc' ";
      $stat = $dbh->prepare($sql);
      $stat->execute();
      my $auto_rfamseq= $stat->fetchrow();
      $stat->finish();

      if (!$auto_rfamseq) { 
	  print STDERR "no entry for this $gbacc in rfamseq\n"; 
	  next;
      }
      print STDERR "got $auto_rfamseq from rfamseq table\n";
      
      ##insert the GP line data using this auto_genome and auto_rfamseq number
       eval {
       if (not defined $insert) {
       $insert = $dbh->prepare($self->__insert_sql('chromosome_build', 7 ));
       }
       ##print "ADDING DATA to chromosome build\n";
       $insert->execute( $auto_genome,
			 $auto_rfamseq,
			 $xsome_start, 
			 $xsome_end,
			 $clone_start, 
			 $clone_end, 
			 $strand 
			 );
       $rows += $insert->rows;
   };
      if ($@) {
	  $error = "Could not do the insertion/update on the chromosome_build table [$@]";
	  last;
      }

  } #end of each GP line in @chr
      
} # end of chromosome_data


################################
#
# This parses the final genome embl file after sam has tweaked with it!!
#
################################

sub final_genomic_species_data {
  my($self, $ac, $de, $joined_tax, @rf) = @_;

  my ($dbh, $stat);

  $dbh = $self->open_transaction('genome_entry','rfam_reg_full' , 'rfamseq', 'rfam');
  my ($genome_auto);
  eval {
    if (not defined $stat) {
      $stat = $dbh->prepare($self->__replace_sql('genome_entry', 4));
    }
    
    # print "ADDING DATA $rdb_auto_num, $rdb_acc, \n";
    $stat->execute( $genome_auto, 
		    $ac,
		    $de,
		    $joined_tax
		  );
    $rows += $stat->rows;
    $genome_auto = $stat->{mysql_insertid}; ## get the auto number
  };
  
  if ($@) {
    
    $error = "Could not do the insertion/update on the pfamA table [$@]";
    
    last;
  }

  foreach my $update (@rf) {
    my $rfamseq_acc = $update->{'seq_acc'};
    my $rfam_acc = $update->{'rfam_acc'};
    my $seq_start = $update->{'start'};
    my $seq_end = $update->{'end'};
#    print "rfamseq: $rfamseq_acc, $rfam_acc, $seq_start, $seq_end \n";
    my $auto_sql = "select auto_rfamseq from rfamseq where rfamseq_acc = '$rfamseq_acc'";
 #   print "$auto_sql \n";
   $stat = $dbh->prepare($auto_sql);
    $stat->execute();
    my $auto_rfamseq = $stat->fetchrow;
    $stat->finish();

    $auto_sql = "select auto_rfam from rfam where rfam_acc = '$rfam_acc'";
    $stat = $dbh->prepare($auto_sql);
    $stat->execute();
    my $auto_rfam = $stat->fetchrow;
    $stat->finish();

    
    my $sql = "select auto_rfamseq, auto_rfam from rfam_reg_full where auto_rfamseq = '$auto_rfamseq' and auto_rfam = '$auto_rfam' and seq_start = '$seq_start' and seq_end = '$seq_end' ";
    $stat = $dbh->prepare($sql);
    $stat->execute();
    my ($tmp_auto_rfamseq, $tmp_auto_rfam) = $stat->fetchrow;
    $stat->finish();
    
    #  print "FAILED: $rfamseq_acc $rfam_acc, $seq_start, $seq_end \n" if (!$tmp_auto_rfamseq);
    
    my $sql_update = "UPDATE rfam_reg_full set auto_genome = $genome_auto where auto_rfamseq = '$auto_rfamseq' and auto_rfam = '$auto_rfam' and seq_start = '$seq_start' and seq_end = '$seq_end' ";
    $stat = $dbh->prepare($sql_update);
    $stat->execute();
    
    
  }




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


# this method looks like some test code - it won't do anything useful
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
#	   $rows += $stat->rows;
       };
       
       if ($@) {
	 
#	   $error = "Could not do the insertion/update on the pfamA table [$@]";
	   
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
       $rdb_entry_type,
       $rdb_structure_source
      );

   $dbh = $self->open_transaction( 'rfam' );
 
   foreach my $en (@{$entries}) {
    
       eval {
	   $rdb_acc = $en->acc;
	   $rdb_id = $en->id();
	 
	 $rdb_auto_num = $en->auto_rfam;
	 
	 ### NEW AUTO NUMBER
 
	 ####### AUTO NUM 
	   my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
	   $st->execute();
	   my($temp_auto) = $st->fetchrow;
	   $st->finish();
	   $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
	   $rdb_desc = $en->description;
	   $rdb_author = $en->author;  # author
	   $rdb_comment = $en->comment;

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
	       
	   $rdb_modlen = $en->model_length;
	   $rdb_entry_type = $en->entry_type;
       };
       if ($@) {
	   $error = "Could not fetch all the needed data from the rfam entry [$@]";
	   last;
       }
    
       $self->report_mode and 
	   printf STDERR "Inserting into %s.rfam from entry %s\n", $self->_database_name, $rdb_acc;
       
    
       eval {
	   if (not defined $stat) {
	       $stat = $dbh->prepare($self->__replace_sql('rfam', 19));
	   }
	  
	  # print "ADDING DATA $rdb_auto_num, $rdb_acc, \n";
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
	                   $rdb_entry_type,
			   $rdb_structure_source
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

sub fix_tax_and_species {
  my($self, @array) = @_;
   $dbh = $self->open_transaction( 'rfamseq');
  foreach (@array) {
    chop($_);
    my($rfamseq_acc, $species, $taxonomy) = split(/~/, $_);
    $species = $dbh->quote($species);
    $taxonomy = $dbh->quote($taxonomy);
    my $sql = "update rfamseq set species = $species, taxonomy = $taxonomy where rfamseq_acc = '$rfamseq_acc'";
  #  print "sql: $sql \n";
    my $st = $dbh->prepare($sql);
    $st->execute();
    
    $st->finish();
  #  exit(0);
  }

   $self->close_transaction();
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
       $rdb_auto_num = $en->auto_rfam;
       
       
       ####### AUTO NUM 
       my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
       $st->execute();
       my($temp_auto) = $st->fetchrow;
       $st->finish();
       
       $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
       $dbh->do("delete from rfam_reg_full where auto_rfam = '$rdb_auto_num' ");
       





       my @regions = $en->annotated_regions('FULL');
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
	   $stat = $dbh->prepare( $self->__insert_sql( 'rfam_reg_full', 6));
	 }
	 eval {
	   $stat->execute($rdb_auto_num,
			  $rfamseq_auto, 
			  $reg->from, 
			  $reg->to,
			  $reg->bits_score,
			 ""
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
       $rdb_auto_num = $en->auto_rfam;
       
       
       ####### AUTO NUM 
       my $st = $dbh->prepare("select auto_rfam from rfam where rfam_acc = '$rdb_acc'");
       $st->execute();
       my($temp_auto) = $st->fetchrow;
       $st->finish();
        
     
       $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
       $dbh->do("delete from rfam_reg_seed where auto_rfam = '$rdb_auto_num' ");
       my @regions = $en->annotated_regions('SEED');
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
    if ($en->each_reference()) {
      my $count = 1;
      # For each reference
      foreach my $ref ( $en->each_reference() ) {
	my($comment, $medline, $authors, $journal, $title);
	
	## Get out comment if there is one - ignored for the moment
#	if (defined ($ref->comment() ) ) {
#	  print "DEF\n";
#	#  print "COMMENT: " .$ref->comment() . " \n";
#	  #foreach my $refcomm ( $ref->comment->each_flat() ) {
#	   # $comment .=  $refcomm . " ";
#	  $comment = $ref->comment();
#	  print "COMMENT: " .$ref->comment() . " \n";
#	  #  chop($refcomm);
#	#  }
#	}


	
#	print "COMM $comment \n";

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
    if ($en->each_dblink ()) {
      foreach my $link ($en->each_dblink ) {
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
    my $self = shift;
    my @seqs = @_;

    $dbh = $self->open_transaction( 'rfamseq' );

    my $error = "";
    my $rows = 0;
    foreach my $seq ( @seqs ) {
	my $auto_id = '';
	my $st = $dbh->prepare( 'select auto_rfamseq from rfamseq where rfamseq_acc = ?' );
	$st->execute( $seq->accession_number );
	($auto_id) = $st->fetchrow;
	$st->finish();

	if( $auto_id ) {
	    $st = $dbh->prepare($self->__replace_sql('rfamseq', 9));
	}
	else {
	    $st = $dbh->prepare($self->__insert_sql('rfamseq', 9));
	}

	my $OS = "unknown";
	my $OC = "unknown";

	# stolen from Bio::SeqIO::embl->write_seq
	my $spec = $seq->species;
	if( $spec ) {
	    my($species, @class) = $spec->classification();
	    my $genus = $class[0];
	    $OS = "$genus $species";
	    if (my $ssp = $spec->sub_species) {
		$OS .= " $ssp";
	    }
	    if (my $common = $spec->common_name) {
		$OS .= " ($common)";
	    }
	    $OC = join('; ', reverse(@class)) .'.';
	}
	####

	$st->execute( $auto_id,
		      $seq->id,
		      $seq->accession_number,
		      $seq->description,
		      $OS,
		      $OC,
		      $seq->seq_version,
		      join( ';', $seq->get_secondary_accessions ),
		      ''
		      );

	$rows += $st->rows;
    };
    if ($@) {
	$error = "Could not do the insertion on the rfamseq table [$@]";
	last;
    }
	 
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
  ##$dbh->do("delete from mirna_species"); # not needed at the moment as the species have been static
  $dbh->do("delete from mirna_database_links");
  $self->close_transaction();
}


sub add_mirna {
  my($self, $id, $acc, $desc , $mature, $sequence, $comment_line, $refs, $database) = @_;
  
  my ($dbh, $stat_add, $stat_mat,$stat_data, $rows, $error, $auto_lit, $auto_mirna, $stat_lit, $stat_mirna);
  
  
  $dbh = $self->open_transaction( 'mirna' ,'literature_references', 'mirna_literature_references', 'mirna_mature', 'mirna_species', 'mirna_database_links'  );
  
  
  ######### UPDATE mirna 
  eval {
    if (not defined $stat_add) {
      $stat_add = $dbh->prepare($self->__insert_sql('mirna', 6));
      #print "stat: $stat \n";
    }
    #   print "ADDING: $rdb_acc \n";
    #  sleep 1;
    # print "$auto_mirna, $acc, $id, $desc, $sequence, $comment_line \n";
    $stat_add->execute($auto_mirna,
		       $acc, 
		       $id, 
		       $desc, 
		       $sequence,
		       $comment_line
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
		       $output{NAME},
		       $output{START},
		       $output{END}
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
  my $st = $dbh->prepare("select auto_lit from literature_references where medline = '$medline' or journal like '%" .$output{JOURNAL} . "%' ");
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
      #    print "AUTO LIT: $auto_lit , " .$output{MEDLINE} . " , " .$output{TITLE} . " , " .$output{AUTHORS} . " , " .$output{JOURNAL} . "\n";
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
  print "EROR: $error \n";
  last;
}







######## UPDATE DATABASE_LINKS tables

my @database_tmp = @${database};


foreach (@database_tmp) {
  # print "QUERY: $_ \n";
  my %output = %{$_};
  
  my $db_id = $output{ID};
  #  print "DB ADD: $db_id \n";
  my $db_link = $output{LINK};
  my $db_comment = $output{COMMENT};
  # print "LINK: $db_link \n";
  
  
  eval {
    # if (not defined $stat_data) {
    #     $stat_lit = $dbh->prepare($self->__insert_sql('mirna_database_links', 5));
    # print "stat: $stat_lit \n";
    #  }
    #   print "ADDING: $rdb_acc \n";
    #  sleep 1;
    # print "auto: $auto_mirna, id: $db_id, link: $db_link, comment: $db_comment \n";
    # $db_link = "RF";
    my $params;
    
    #print "SQL: ";
    
    my $stat_lit = $dbh->prepare("insert into mirna_database_links values ('$auto_mirna', '$db_id','$db_comment', '$db_link',   '$params')");
    $stat_lit->execute();
    #   $stat_lit->execute($auto_mirna,
    #		       $db_id,
    #		       $db_link,
    #		       $db_comment,
    #		       ""
    #		      );
    #  $rows += $stat_data->rows;
    
    
    
  };
  if ($@) {
    $error = "Could not do the insertion on the mirna_database_links table [$@]";
    print "error: $error \n";
    last;
    }  





  
}
#exit(0);

}

1;
