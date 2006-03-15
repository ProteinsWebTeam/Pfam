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


package Bio::Pfam::UpdateRDB;

use vars qw($AUTOLOAD
	    @ISA
	    @EXPORT_OK );

use strict;
use Time::HiRes qw(gettimeofday);
use Bio::Pfam::PfamRDB;
use vars qw(%all_pfamseq_db_store %all_pfamA_db_store);

@ISA = qw(Bio::Pfam::PfamRDB Exporter);
@EXPORT_OK = qw();

sub _initialize {
    my ($self, %params) = @_;

    my $make = $self->SUPER::_initialize( %params );
    # insert fields here

    return $make;
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

sub check_in_EntryA {
  my ($self, @en ) = @_;
  my ($dbh,
      @regions );
 

  $dbh = $self->open_transaction('pfamA', 'pfamA_reg_seed', 'pfamA_reg_full', 'pfamseq',  'pfamA_literature_references', 'literature_references' , 'pfamA_database_links', 'nested_domains');


    eval {
      foreach my $ent (@en) {
#	print "Adding to pfamA \n";
	$self->update_pfamA( [$ent] );
#	print "pfamA_reg_seed\n";
	$self->update_pfamA_reg_seed( [$ent] );
#	print "pfamA_reg_full \n";
	$self->update_pfamA_reg_full( [$ent] );
#	print "lit refs \n";
	$self->update_literature_references( [$ent] );
#	print "database_links \n";
	$self->update_pfamA_database_links( [$ent] );
#	print "nested domains \n";
	$self->update_nested_domains( [$ent] );
      }
    };

  $self->close_transaction( $@);
  $@ and $self->throw($@);

}

=head2 check_in_EntryA_desc

 Title   : check_in_entry_desc
 Usage   : $updatedb->check_in_EntryA_desc( $entry );
 Function:
    This function updates the underlying relational database
    by loading the given family through the middleware layer
    and inserting all relevant records into the correct tables,
    deleting records if necesary.  It only updates information
    contained within the DESC file
 Returns :
 Args    : Bio::Pfam::EntryA

=cut

sub check_in_EntryA_desc {
  my ($self, @en ) = @_;
  my ($dbh,
      @regions );


  $dbh = $self->open_transaction('pfamA', 'pfamA_reg_seed', 'pfamA_reg_full', 'pfamseq',  'pfamA_literature_references', 'literature_references' , 'pfamA_database_links', 'nested_domains');


    eval {
      foreach my $ent (@en) {
#	print "Adding to pfamA \n";
	$self->update_pfamA( [$ent] );
#	print "lit refs \n";
	$self->update_literature_references( [$ent] );
#	print "database_links \n";
	$self->update_pfamA_database_links( [$ent] );

      }
    };

  $self->close_transaction( $@);
  $@ and $self->throw($@);

}







=head2 delete_EntryA

 Title   : delete_entryA
 Usage   : $updatedb->delete_EntryA( @entry_list );
 Function:
    This function deletes all of the region information
    associated with a pfamA enttry. Note that even the entry
    itself is deleted from pfamA. Even though dead entries
    are not deleted from the pfamA table, this occurs so
    that if we wish to replace the entry by using the file
    entry mechanism, everything works.
 Returns :
 Args    : Bio::Pfam::EntryA (list of)

=cut

sub delete_EntryA {
   my ($self, @ents) = @_;

   my ($dbh,
       @regions );

   $dbh = $self->open_transaction( 'pfamA', 'pfamA_reg_seed', 'pfamA_reg_full' , 'pfamA_database_links', 'pfamA_literature_references', 'pdbmap', 'pfam_to_interpro', 'nested_domains');

   eval {
       foreach my $en (@ents) {
	   my $acc = $en->acc;

	   ####### AUTO NUM
	   my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$acc'");
	   $st->execute();
	   my($temp_auto) = $st->fetchrow;
	   $st->finish();

	   my $rdb_auto_num = $temp_auto if(defined($temp_auto));

	   $dbh->do("delete from pfamA where auto_pfamA = '$rdb_auto_num'");
	   $dbh->do("delete from pfamA_reg_seed where auto_pfamA = '$rdb_auto_num'");
	   $dbh->do("delete from pfamA_reg_full where auto_pfamA = '$rdb_auto_num'");
	   $dbh->do("delete from pfamA_database_links where auto_pfamA = '$rdb_auto_num'");
	   $dbh->do("delete from pfamA_literature_references where auto_pfamA = '$rdb_auto_num'");
	   $dbh->do("delete from pdbmap where auto_pfam = '$rdb_auto_num'");
	   $dbh->do("delete from pfam_to_interpro where auto_pfamA = '$rdb_auto_num'");
	   $dbh->do("delete from nested_domains where auto_pfamA = '$rdb_auto_num'");
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






sub write_table_to_file{
	my ($self, $table_name, $file) = @_;
	my ($rows, $dbh);
	$dbh = $self->open_transaction($table_name);
	if (-e $file) {
       $self->throw("write_ $table_name _to_file error: File $file already exists");
	}
  	if (!$table_name) {
       $self->throw("write_ $table_name _to_file error: Did not pass the table_name");
	}
	eval{
		my $st = $dbh->prepare("select * from $table_name into outfile '$file'");
		$st->execute;
	};
	
	$self->close_transaction( $@ );
	$@ and $self->throw( $@ );

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


####################################################################################
#
# PFAMB UPLOAD
#
####################################################################################


=head2 load_pfamB_from_file

 Title   : load_pfamB_from_file
 Usage   :
 Function:
    This function takes the name of a file, each line of which is a tab-delimited
    list of:
    1. Pfam-B accession
    2. Pfam-B identifier
    3. Number of sequences in the alignment/family
 Returns :
    The number of rows inserted/updated;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :
    1. If the transaction handle is undefined, a transaction is created and committed
    for just this update. If a handle is given, then it is assumed that the update
    is part of a larger transaction.

=cut

sub load_pfamB_from_file{
   my ($self, $file) = @_;
   my ($rows,
       $dbh,
	$stat,
	$error);

   $dbh = $self->open_transaction( 'pfamB' );

   if (! -e $file) {
       $self->throw("load_pfamB_from_file error: Could not find file $file");
   }

   $self->report_mode and
       printf STDERR "Inserting into %s.pfamB from file %s\n", $self->_database_name, $file;

   eval {

	open(_FILE, $file) or die "canna open $file as $! \n";
	while(<_FILE>) {
		chop($_);
	my($rdb_acc, $rdb_id) = split(/\s+/,$_);
	my $rdb_auto_pfamB;
           if (not defined $stat) {
               $stat = $dbh->prepare($self->__replace_sql('pfamB', 3));
           }

           $stat->execute( $rdb_auto_pfamB,
                          $rdb_acc,
                           $rdb_id
		);
	$rows += $stat->rows;


	}
	close(_FILE);

       #$rows = $dbh->do( $self->__insert_from_file_sql( 'pfamB', $file ));


   };



  if ($@) {
           $error = "Could not do the insertion/update on the pfamB table [$@]";
           last;
       }

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}









######## UPDATE ############
=head2 load_pfamB_reg_from_file

 Title   : load_pfamB_reg_from_file
 Usage   :
 Function:
    This function populates the pfamB_reg table from a file,
    each line of which is tab-delimited as:
 Returns :
    The number of rows inserted/updated;
    Throws an exception if something went wrong
 Args    :
    1. A file name (the name of a file that has already been filled).

 Notes   :

=cut

sub load_pfamB_reg_from_file{
   my ($self, $file) = @_;
   my ($dbh,
       $count,
       @regions,
	$stat,
	$error);

   $dbh = $self->open_transaction( 'pfamB_reg' , 'pfamseq', 'pfamB');

   if (! -e $file) {
       $self->throw("update_pfamB_reg_from_file error. Could not find $file");
   }

   $self->report_mode and
       printf STDERR "Inserting into %s.pfamB_reg from file %s\n", $self->_database_name, $file;

   eval {
	open(_FILE, $file) or die "cant open $file as $! \n";
	while(<_FILE>) {
	chop($_);
	my($pfamseq, $start, $end, $pfamB_acc) = split(/\s+/, $_);
#	print "PFAMSEQ: $pfamseq \n";
	my $st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_id = '$pfamseq'");
	$st->execute();
	my $rdb_seqid = $st->fetchrow;
	$st->finish();

        my $b_st = $dbh->prepare("select auto_pfamB from pfamB where pfamB_acc = '$pfamB_acc'");
        $b_st->execute();
	my $rdb_auto_pfamB = $b_st->fetchrow;
	$b_st->finish();


	if (not defined $stat) {
	  $stat = $dbh->prepare($self->__replace_sql('pfamB_reg', 4));
	}

	$stat->execute( $rdb_auto_pfamB,
			$rdb_seqid,
                        $start,
			$end
		      );

	$count += $stat->rows;




      };

	if ($@) {
	  $error = "Could not do the insertion/update on the pfamB_reg table [$@]";
	  last;
	}


	close(_FILE);


	#$count = $dbh->do( $self->__insert_from_file_sql( 'pfamB_reg', $file ));
      };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $count;
}





##### SHOULD BE OK #########
=head2 load_other_reg_from_file

 Title   : load_other_reg_from_file
 Usage   :
 Function:
    This function takes the name of a file, each line of which is a tab-delimited
    list of:
    1. pfamseq id
    2. start res.
    3. end res.
    4. region type
    5. region source
    6. score
 Returns :
    The number of rows inserted/updated;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :
    1. If the transaction handle is undefined, a transaction is created and committed
    for just this update. If a handle is given, then it is assumed that the update
    is part of a larger transaction.

=cut

sub load_other_reg_from_file {
   my ($self, $file, $temp_dir) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->open_transaction( 'other_reg', 'pfamseq' );

   if (! -e $file) {
       $self->throw("load_other_regions_from_file error: Could not find file $file");
   }

   $self->report_mode and
       printf STDERR "Inserting into %s.other_reg from file %s\n", $self->_database_name, $file;

   my %store_pfamseq;

   ### If pfamseq not in correct format then do pfamseq lookup.
   my ($fname1 , $write_file);
   if ($temp_dir) {
     my $read_file = $file;
     open(_FILE, $read_file);
     $file = $temp_dir . "/output_$$";
     $write_file = FileHandle->new();
     $write_file->open(">$file") or die "Could not open file $fname1 for writing";

     while(<_FILE>) {
       my($id, $pfamseq, $start, $end, $region, $source) = split(/\s+/, $_);
       my $rdb_seqid;
       if (defined($store_pfamseq{$pfamseq})) {
	 $rdb_seqid = $store_pfamseq{$pfamseq};
       } else {
	 my $st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_acc = '$pfamseq'");
	 $st->execute();
	 $rdb_seqid = $st->fetchrow;
	 $st->finish();
	 $store_pfamseq{$pfamseq} = $pfamseq;
       }


       
       print $write_file "\\N\t$rdb_seqid\t$start\t$end\t$region\t$source\n";
       if ($@) {
	 my $error = "Problem with getting the auto_pfamseq from pfamseq table, maybe a problem with the format";
	 last;
       }

     }


     close(_FILE);

   }

   eval {
     $write_file->close;

       $rows = $dbh->do( $self->__insert_from_file_sql( 'other_reg', $file ));
   };

   if ($temp_dir) {

    unlink $file;
   }

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}







# FIXED
sub load_pdbmap_from_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

  # print "HERE \n";
  # exit(0);
   $dbh = $self->open_transaction( 'pdbmap', 'pfamseq', 'pfamA', 'pfamB', 'pdb' );

   if (! -e $file) {
       $self->throw("load_pdbmap_from_file: Could not find file $file");
   }

	$dbh->do("delete from pdbmap");

   $self->report_mode and
       printf STDERR "Inserting into %s.complexes from file %s\n", $self->_database_name, $file;


   my (%store_pfamseq, %store_pdb, %store_pfam);
     open(_FILE, $file) or die "Canna open $file as $! \n";
     while(<_FILE>) {
       eval {

       chop($_);
       $_ =~ s/ //g;
       my($pdb_id, $chain, $pdb_start, $pdb_end, $pfam_region, $pfamseq, $start, $end, $complex) = split(/\t/, $_);
	if ($pdb_end !~ /\d+/) {
		print "no digit $pdb_id, $chain , end: $pdb_end \n";
	}
	if ($pdb_start !~ /\d+/) {
		print "no digit $pdb_id, $chain , start: $pdb_start \n";
	}
	my $diff = $pdb_end - $pdb_start;
	if ($diff < 10) {
	#print "end: $pdb_end, start: $pdb_start \n";
	#print "ERROR in pdb diff: $_ :: the diff is tooo small \n";
	next;

	}

       my ($rdb_seqid,  $stat, $auto_pdb);

       if (defined($store_pfamseq{$pfamseq})) {
	 $rdb_seqid = $store_pfamseq{$pfamseq};

       } else {
	 #### NEEDS TO BE CHANGED TO _acc 
	 my $st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_id = '$pfamseq'");
	 $st->execute();
	 $rdb_seqid = $st->fetchrow;
	 $st->finish();

	 $store_pfamseq{$pfamseq} = $rdb_seqid;
       }

       my ($domain_type, $auto_pfam);

       if (defined($store_pfam{$pfam_region})) {
	 $auto_pfam = $store_pfam{$pfam_region};
	 if ($pfam_region =~ /Pfam-B_/i) {
	   $domain_type = "0";
	 } else {
	   $domain_type = "1";
	 }

       } else {

	 if ($pfam_region =~ /Pfam-B_/i) {
	   $domain_type = "0";


	   my $st = $dbh->prepare("select auto_pfamB from pfamB where pfamB_id = '$pfam_region'");
	   $st->execute();
	   $auto_pfam = $st->fetchrow;
	   $st->finish();
	   $store_pfam{$pfam_region} = $auto_pfam;

	 } else {
	   $domain_type = "1";

	   my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_id = '$pfam_region' ");

	   $st->execute();
	   $auto_pfam = $st->fetchrow;
	   $st->finish();
	   if (!$auto_pfam) {
	     print "NONE: select auto_pfamA from pfamA where pfamA_id = '$pfam_region'  \n"; 
	   }
	   
	   $store_pfam{$pfam_region} = $auto_pfam;
	 }

       }



       if (defined($store_pdb{$pdb_id})) {
	 $auto_pdb = $store_pdb{$pdb_id};

       } else {
       	 my $st = $dbh->prepare("select auto_pdb from pdb where pdb_id = '$pdb_id' ");

	 $st->execute();
	 $auto_pdb = $st->fetchrow;
	 $st->finish();

	 $store_pdb{$pdb_id} =  $auto_pdb;
       }


           if (not defined $stat) {
               $stat = $dbh->prepare($self->__replace_sql('pdbmap', 11));
           }

       if ( (!$auto_pdb) || (!$auto_pfam) ||(!$rdb_seqid) ) {
	 
	 print  qq (CANT ADD: pdbid: $pdb_id :: AUTO: $auto_pdb, CHAIN: $chain, pdb_start: $pdb_start, pdb_end: $pdb_end, auto_pfam: $auto_pfam,  domain_type: $domain_type, rdb_seqid: $rdb_seqid, start: $start, end: $end, $complex \n);
       } else {
           $stat->execute( $auto_pdb,
			   $chain,
			   $pdb_start,
			   $pdb_end,
			   $auto_pfam,
			   $domain_type,
			   $rdb_seqid,
			   $start,
			   $end,
			   $complex,
				"");

	   $rows += $stat->rows;

	 }



     };

       if ($@) {
	 print "Update of pdbmap failed as:  $@ \n";
       }

     }

   close(_FILE);

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
 }


sub add_key_to_pdb {
  my($self, $temp_cols) = @_;
  my(@temp_cols_arr) = @{$temp_cols};

  my(%temp_store_pdb, %temp_store_pfamA,$dbh, $st, $st_update);
  
  $dbh = $self->open_transaction( 'pdb', 'pdbmap' , 'pfamA');
  
  ## GET THE PDB AUTO NUM'S 
  my $pfamA_sql = "select auto_pfamA, pfamA_acc from pfamA";
  
  my $pfamA_st = $dbh->prepare($pfamA_sql);
  $pfamA_st->execute();
  while ( my($auto_pfamA, $pfamA_acc) = $pfamA_st->fetchrow) {
  
    $temp_store_pfamA{$pfamA_acc} = $auto_pfamA;
  }
  $pfamA_st->finish();

  my $pdb_sql = "select auto_pdb, pdb_id from pdb";

  my $pdb_st = $dbh->prepare($pdb_sql);
  $pdb_st->execute();
  while ( my($auto_pdb, $pdb_id) = $pdb_st->fetchrow) {

    $temp_store_pdb{$pdb_id} = $auto_pdb;
  }
  $pdb_st->finish();

  my $count =0;
  foreach (@temp_cols_arr) {
    my($pdb, $pfamA_acc, $chain, $pdb_start, $pdb_end,$hex) = split(/~/, $_);
    
    my $auto_pdb = $temp_store_pdb{$pdb};
    my $auto_pfamA = $temp_store_pfamA{$pfamA_acc};
    my $chain_sql;
    if ($chain =~ /\_/) {
      $chain_sql = "";
    } else {
      $chain_sql = "and chain = '$chain'";
    }

    my $update_sql = "select * from pdbmap where auto_pfam = '$auto_pfamA' and pfam_region = '1' and auto_pdb = '$auto_pdb' $chain_sql and  $pdb_start <= pdb_end_res and pdb_start_res <= $pdb_end  ";
    eval {
      my $update_st = $dbh->prepare($update_sql);
      eval {
	$update_st->execute();
      };
      if ($@) {
	print "BOMBED AS : $update_sql \n";
      }
      my($auto_pdb , $chain , $pdb_start_res , $pdb_end_res , $auto_pfam , $pfam_region , $auto_pfamseq , $pfam_start_res , $pfam_end_res , $complex , $hex_colour) = $update_st->fetchrow;
      

      if (!$auto_pdb) {
	$count++;
      } else {
	if ( ($hex_colour) && ($hex_colour ne $hex) ) {
#	  print "ALREDY GOT HEX COLOUR: ($hex_colour :: $hex) $auto_pdb , $chain , $pdb_start_res , $pdb_end_res , $auto_pfam , $pfam_region , $auto_pfamseq , $pfam_start_res , $pfam_end_res , $complex , $hex_colour -> $update_sql ($pdb_start -> $pdb_end)\n";
	} else {
	  my $update = "update pdbmap set hex_colour = '$hex' where auto_pfam = '$auto_pfamA' and pfam_region = '1' and auto_pdb = '$auto_pdb' $chain_sql and  $pdb_start <= pdb_end_res and pdb_start_res <= $pdb_end  ";
	  my $update_sta = $dbh->prepare($update);
	  $update_sta->execute();
	  $update_sta->finish();
	}

      }


      $update_st->finish();
    };
    if ($@) {
      my $error = "Could not do the update on the pdbmap table";
      print "ERROR $error $@ \n"; exit(0);
      last;
    }
    
    
  }

  print "COUNT: $count \n";
  



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
#     $rdb_comment = $en->comment();
     my $rdb_comment;
     $rdb_forward = $en->each_forward_acc();
     foreach my $forward ($en->each_forward_acc()) {
        $rdb_forward = $rdb_forward . ";$forward";
    }


     ### GET COMMENT
     foreach my $comment ( $en->ann->get_Annotations('comment') ) {
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


=head2 update_pfamA

 Title   : update_pfamA
 Usage   :
 Function:
    This function takes a pfam EntryA and progresses the given
    tranaction by updating the corresponding pfamA table in the RDB
 Returns :
    The number of rows inserted/updated (should be 1);
    Throws an exception if something went wrong
 Args    :
    1. A reference to a list of entries


 Notes   :
    1. If the transaction handle is undefined, a transaction is created and committed
    for just this update. If a handle is given, then it is assumed that the update
    is part of a larger transaction.

    2. If a dead entry is given, it is assumed that we need to update the database
    for the KILLING of this family.

=cut

sub update_pfamA{
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
       $rdb_entry_type,
       $rdb_align_method,
       $gathering_cutoff,
       $noise_cutoff,
       $rdb_previous_ids,
       $rdb_source,
       $trusted_cutoff,

       $rdb_ls_sequence_GA,
       $rdb_ls_domain_GA,
       $rdb_fs_sequence_GA,
       $rdb_fs_domain_GA,

       $rdb_ls_sequence_TC,
       $rdb_ls_domain_TC,
       $rdb_fs_sequence_TC,
       $rdb_fs_domain_TC,

       $rdb_ls_sequence_NC,
       $rdb_ls_domain_NC,
       $rdb_fs_sequence_NC,
       $rdb_fs_domain_NC,

       $rdb_ls_mu,
       $rdb_ls_kappa,
       $rdb_fs_mu,
       $rdb_fs_kappa,
       $rdb_hmmcalibrate_ls,
       $rdb_hmmbuild_ls,
       $rdb_hmmcalibrate_fs,
       $rdb_hmmbuild_fs,
       $rdb_num_full,
       $rdb_num_seed,
       $rdb_created

      );

   $dbh = $self->open_transaction( 'pfamA' );

   foreach my $en (@{$entries}) {

       eval {
	   $rdb_acc = $en->acc;

	   $rdb_id = $en->id();
	   $rdb_auto_num = $en->auto_pfamA;

	   ### NEW AUTO NUMBER


	   ####### AUTO NUM
	   my $st = $dbh->prepare("select auto_pfamA, created from pfamA where pfamA_acc = '$rdb_acc'");
	   $st->execute();
	   my($temp_auto, $tmp_rdb_created) = $st->fetchrow;
	   $st->finish();

	   $rdb_auto_num = $temp_auto if(defined($temp_auto));
	   $rdb_created = $tmp_rdb_created if(defined($temp_auto));
	
	  	

	   if (defined $en->ann) {


	       $rdb_desc = $en->description;

	     ### NEW RDB PARAMS

	       $rdb_author = $en->author;  # author

	      ### GET COMMENT
	       foreach my $comment ( $en->ann()->get_Annotations('comment') ) {
		$rdb_comment .= " " . $comment->text();
	       }

	       $rdb_entry_type = $en->entry_type();


	       $rdb_align_method = $en->alignmethod();

	       $rdb_ls_sequence_GA = $en->ls_sequence_gathering_cutoff();
	       $rdb_ls_domain_GA = $en->ls_domain_gathering_cutoff();
	       $rdb_fs_sequence_GA = $en->fs_sequence_gathering_cutoff();
	       $rdb_fs_domain_GA  = $en->fs_domain_gathering_cutoff();


	       $rdb_ls_sequence_NC = $en->ls_sequence_noise_cutoff();
	       $rdb_ls_domain_NC = $en->ls_domain_noise_cutoff();
	       $rdb_fs_sequence_NC = $en->fs_sequence_noise_cutoff();
	       $rdb_fs_domain_NC  = $en->fs_domain_noise_cutoff();

	       $rdb_previous_ids = $en->previous_ids();
	       $rdb_source = $en->source();

	       $rdb_ls_sequence_TC = $en->ls_sequence_trusted_cutoff();
	       $rdb_ls_domain_TC = $en->ls_domain_trusted_cutoff();
	       $rdb_fs_sequence_TC = $en->fs_sequence_trusted_cutoff();
	       $rdb_fs_domain_TC  = $en->fs_domain_trusted_cutoff();


	       $rdb_ls_mu = $en->ls_mu();
	       $rdb_ls_kappa = $en->ls_kappa();
	       $rdb_fs_mu = $en->fs_mu();
	       $rdb_fs_kappa = $en->fs_kappa();

	     #  $en->load_desc_file_info();
	       $rdb_hmmcalibrate_ls = $en->hmmcalibrate_ls;
	       $rdb_hmmbuild_ls = $en->hmmbuild_ls;
	       $rdb_hmmcalibrate_fs = $en->hmmcalibrate_fs;
	       $rdb_hmmbuild_fs = $en->hmmbuild_fs;

	       $rdb_num_full = "";
	       $rdb_num_seed = "";


	   }
	   if (not $en->is_dead) {
	       $rdb_modlen = $en->model_length;
	   }
       };
       if ($@) {
	   $error = "Could not fetch all the needed data from the pfamA entry [$@]";
	   last;
       }

       $self->report_mode and
	   printf STDERR "Inserting into %s.pfamA from entry %s\n", $self->_database_name, $rdb_acc;


       eval {
	   if (not defined $stat) {
	       $stat = $dbh->prepare($self->__replace_sql('pfamA', 35));
	   }


	   $stat->execute( $rdb_auto_num,
			  $rdb_acc,
			   $rdb_id,
			   $rdb_desc,
			   $rdb_modlen,
			   $rdb_author,
			   $rdb_source,
			   $rdb_align_method,
			   $rdb_entry_type,
			   $rdb_ls_sequence_GA,
			   $rdb_ls_domain_GA,
			   $rdb_fs_sequence_GA,
			   $rdb_fs_domain_GA,
			   $rdb_ls_sequence_TC,
			   $rdb_ls_domain_TC,
			   $rdb_fs_sequence_TC,
			   $rdb_fs_domain_TC,
			   $rdb_ls_sequence_NC,
			   $rdb_ls_domain_NC,
			   $rdb_fs_sequence_NC,
			   $rdb_fs_domain_NC,
			   $rdb_ls_mu,
			   $rdb_ls_kappa,
			   $rdb_fs_mu,
			   $rdb_fs_kappa,
			   $rdb_comment,
			   $rdb_previous_ids,
			   $rdb_hmmbuild_ls,
			   $rdb_hmmcalibrate_ls,
			   $rdb_hmmbuild_fs,
			   $rdb_hmmcalibrate_fs,
			   $rdb_num_seed,
			   $rdb_num_full,
			   undef,
			   $rdb_created
			 );
	   $rows += $stat->rows;

	   if(!$rdb_auto_num) {
	     my $time_sql = "update pfamA set created = now() where pfamA_acc = '$rdb_acc'";
	     $stat = $dbh->prepare($time_sql);
	     $stat->execute;
	   }
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







=head2 update_pfamA_reg_full

 Title   : update_pfamA_reg_full
 Usage   :
 Function:
    This function takes a pfam EntryA and progresses the given
    tranaction by updating the corresponding pfamA_table table in the RDB
 Returns :
    The number of rows inserted/updated;
    Throws an exception if something went wrong
 Args    :
    1. A list of entries

 Notes   :
    If the given entry is dead, then we need to kill the family. As far
    as the pfamA_reg_full table is concerned, this just means deleting
    all records concerning that family from the table

=cut

sub update_pfamA_reg_full {
    my ($self, $entries) = @_;
    my $t0 = gettimeofday();
    
    my ($error,
	$dbh,
	$stat,
	$rdb_acc,
	$rdb_id,
	$rdb_auto_num,
	$count,
	$rdb_mode,
	@regions,
	%store_pfamseq);
    
    $dbh = $self->open_transaction( 'pfamA_reg_full' );
    $self->report_mode(1);
    
    foreach my $en (@{$entries}) {
	eval {
	    $rdb_acc = $en->acc;
	    $rdb_id = $en->id();
	    #### Get what the auto_pfamA is from pfamA table
	    my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$rdb_acc'");
	    $st->execute();
	    my($temp_auto) = $st->fetchrow;
	    $st->finish();
	    
	    $rdb_auto_num = $temp_auto if(defined($temp_auto));
	   
	    
	    if (not $en->is_dead()) {
		@regions = $en->all_full_annotated_regions('FULL');
	    }
	    
	};
	if ($@) {
	    $error = "Could not fetch all the needed data from the pfamA entry [$@]";
	    last;
	}
	my $t1 = gettimeofday();
	print STDERR "Time elapsed prior to delete ".($t1 - $t0)."\n";
	
	#First get the sequences ids for sequences region for the previous checkin.  This will give some speed-up later on.
	my $ent_st = $dbh->prepare("select pfamseq.auto_pfamseq, pfamseq_acc from pfamA_reg_full, pfamseq where auto_pfamA=$rdb_auto_num and pfamseq.auto_pfamseq=pfamA_reg_full.auto_pfamseq");
	$ent_st->execute;
	my $rows = $ent_st->fetchall_arrayref;
	%all_pfamseq_db_store = map{$$_[1] => $$_[0]}@$rows;
	
	
	eval {
	    $self->report_mode and
		printf STDERR "Deleting from %s.pfamA_reg_full for entry %s\n", $self->_database_name, $rdb_auto_num;
	    $dbh->do("delete from pfamA_reg_full where auto_pfamA = '$rdb_auto_num'");
	    
	};
	$t1 = gettimeofday();
	print STDERR "Time elapsed after delete ".($t1 - $t0)."\n";
	
	if ($@) {
	    $error = "Could not delete from pfamA_reg_full rec. $rdb_auto_num [$@]";
	    last;
	}
	
	
	if (not $en->is_dead) {
	    $self->report_mode and
		printf STDERR "Inserting into %s.pfamA_reg_full from entry %s\n", $self->_database_name, $rdb_auto_num;
	    
	    eval {
		

		
		
		my $st = $dbh->prepare_cached("select auto_pfamseq from pfamseq where pfamseq_acc = ? ");
		
		$t1 = gettimeofday();
		print STDERR "Time elapsed prior to insert ".($t1 - $t0)."\n";
	       
		
		#Okay, this bit is not pretty, but the fastest way to upload the data that scales is to load via an external file.
		#Yes, this goes outside the code, but single inserts and mutliple inserts using VALUES (bla), (bla) are much slower
		
		my $pwd = `pwd`;
		chomp($pwd);
		if(!-d "$rdb_id/pfamA_reg_full"){
		    mkdir("$rdb_id/pfamA_reg_full", 0777) || warn "failed to make directory $rdb_id/pfamA_reg_full:[$!]\n";
		}
		open(OUT, ">$rdb_id/pfamA_reg_full/regions") || warn "failed to open file $rdb_id/pfamA_reg_full/regions:[$!]\n";
		
		
		#Now this is the slowest part
		foreach my $reg (@regions) {
		    #### Get what the auto_pfamseq is from pfamseq table - especially this part.  Even
		    if (!defined($all_pfamseq_db_store{$reg->seq_name})) {
			$st->execute($reg->seq_name);
			my $rdb_auto_pfamseq = $st->fetchrow;
			if (!$rdb_auto_pfamseq){
			    $error =   "ERROR: ".$reg->seq_name." is NOT in the pfamseq table \n";
			}else{
			    $all_pfamseq_db_store{$reg->seq_name} = $rdb_auto_pfamseq;
			}
		    }
		    
		    
		    #Inserting region by region is very slow. Therefore, need to use insert delayed and batch it or write a file and 
		    #use load data infile.  For the release, it is probably work removing the indexes from the table and reindexing at the end.
		    #Also, we could skip the delete by deleting the table at the start.
		    
		    #Print the data to file
		    print OUT "$rdb_auto_num\t".$all_pfamseq_db_store{$reg->seq_name}."\t".$reg->from."\t".$reg->to."\t".$reg->model_from."\t".$reg->model_to."\t".$reg->domain_bits_score."\t".$reg->domain_evalue_score."\t".$reg->sequence_bits_score."\t".$reg->sequence_evalue_score."\t".$reg->mode."\t".($reg->is_significant?1:0)."\t".($reg->in_full?1:0)."\t\\N\n";
		    
		}#end of foreach region
		    close(OUT);
		$t1 = gettimeofday();
		print STDERR "Time elapsed after writing to file insert ".($t1 - $t0)."\n";
		my $up_st = $dbh->prepare("LOAD DATA INFILE \"$pwd/$rdb_id/pfamA_reg_full/regions\" INTO TABLE pfamA_reg_full");
		$up_st->execute;
		$t1 = gettimeofday();
		print STDERR "Time elapsed after insert ".($t1 - $t0)."\n";
	    };
	    
	    if ($@) {
		$error = "Could not insert data into pfamA_reg_full table [$@]";
		print "ERROR: $error \n";
		last;
	    }   
	}
    }
    
    $self->close_transaction( $error );
    $error and $self->throw( $error );
    return $count;
}




####### OK ?
=head2 update_pfamA_reg_seed

 Title   : update_pfamA_reg_seed
 Usage   :
 Function:
    This function takes a pfam EntryA and progresses the given
    tranaction by updating the corresponding pfamA_table table in the RDB
 Returns :
    The number of rows inserted/updated;
    Throws an exception if something went wrong
 Args    :
    1. A list of entries

 Notes   :
    If the given entry is dead, then we need to kill the family. As far
    as the pfamA_reg_seed table is concerned, this just means deleting
    all records concerning that family from the table

=cut
sub update_pfamA_reg_seed {
   my ($self, $entries) = @_;
   my ($error,
       $dbh,
       $stat,
       $rdb_acc,
       $rdb_id,
       $rdb_auto_num,
       $count,
       @regions);

   $dbh = $self->open_transaction( 'pfamA_reg_seed' , 'pfamseq');

   foreach my $en (@{$entries}) {
       eval {
	   $rdb_acc = $en->acc;
	   $rdb_id = $en->id();

	   #### Get what the auto_pfamA is from pfamA table
	   my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$rdb_acc'");
	   $st->execute();
	   my($temp_auto) = $st->fetchrow;
	   $st->finish();

	   $rdb_auto_num = $temp_auto if(defined($temp_auto));

	   if (not $en->is_dead()) {
	       @regions = $en->annotated_regions('SEED');
	   }
       };

       if ($@) {
	   $error = "Could not fetch all the needed data from the pfamA entry [$@]";
	   last;
       }


       eval {
	   $self->report_mode and
	       printf STDERR "Deleting from %s.pfamA_reg_seed for entry %s\n", $self->_database_name, $rdb_auto_num;


	   $dbh->do("delete from pfamA_reg_seed where auto_pfamA = '$rdb_auto_num'");
       };
       if ($@) {
	   $error = "Could not delete from pfamA_reg_seed rec. $rdb_auto_num [$@]";
	   last;
       }

       if (not $en->is_dead) {
	   $self->report_mode and
	       printf STDERR "Inserting into %s.pfamA_reg_seed from entry %s\n", $self->_database_name, $rdb_auto_num;

	   eval {
	       if (not defined $stat) {
		   $stat = $dbh->prepare( $self->__insert_sql( 'pfamA_reg_seed', 4));
	       }
	       foreach my $reg (@regions) {



		 #### Get what the auto_pfamseq is from pfamseq table
		 my $pfamseq_acc = $reg->seq_name;
		 my($auto_pfamseq);
		 if(defined($all_pfamseq_db_store{$pfamseq_acc})) {
		   $auto_pfamseq = $all_pfamseq_db_store{$pfamseq_acc};

		 } else {

		   
		   my $st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_acc = '$pfamseq_acc'");
		   $st->execute();
		   ($auto_pfamseq) = $st->fetchrow;
		   $st->finish();
		   $all_pfamseq_db_store{$pfamseq_acc} = $auto_pfamseq;
		   
		 }
		 
		  print "$pfamseq_acc is not present in pfamseq, where did you get it from ?!?! \n" if (!$auto_pfamseq);
		   $stat->execute($rdb_auto_num,
				  $auto_pfamseq,
				  $reg->from,
				  $reg->to,
				  );
		   $count += $stat->rows;
	       }
	   };
	   if ($@) {
	       $error = "Could not insert data into pfamA_reg_seed table [$@]";
	       last;
	   }
       }
   }

   $self->close_transaction( $error );
   $error and $self->throw( $error );

   return $count;
}



sub update_nested_domains {

 my ($self, $entries) = @_;
   my ($error,
       $dbh,
       $stat,
       $rdb_acc,
       $rdb_id,
       $rdb_auto_num,
       $count);

   $dbh = $self->open_transaction( 'nested_domains');

   foreach my $en (@{$entries}) {

       eval {
	   $rdb_acc = $en->acc;
	   $rdb_id = $en->id();

	   #### Get what the auto_pfamA is from pfamA table
	   my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$rdb_acc'");
	   $st->execute();
	   my($temp_auto) = $st->fetchrow;
	   $st->finish();

	   $rdb_auto_num = $temp_auto if(defined($temp_auto));


	 };

       if ($@) {
	 $error = "Could not fetch all the needed data from the pfamA entry [$@]";
	 last;
       }


       eval {
	   $self->report_mode and
	     printf STDERR "Deleting from %s.nested_domains for entry %s\n", $self->_database_name, $rdb_auto_num;


	   $dbh->do("delete from nested_domains where auto_pfamA = '$rdb_auto_num'");
       };
       if ($@) {
	   $error = "Could not delete from nested_domains rec. $rdb_auto_num [$@]";
	   last;
       }

       my @nested_domains = ();

       @nested_domains = $en->nested_domains();


       if (defined($nested_domains[0])) {
	 my $nested_auto;

	 foreach (@nested_domains) {
	   my $nest_acc = $$_{'domain'};
	   eval {
	     my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$nest_acc'");
	     $st->execute();
	     my($temp_auto) = $st->fetchrow;
	     $st->finish();
	     $nested_auto = $temp_auto;
	   };

	   if ( ($@) || (!$nested_auto) ) {

	     $error = "Couldnt get auto_pfamA for accession number $nest_acc \n";
	     last;
	   }
	   eval {
	     if (not defined $stat) {
	       $stat = $dbh->prepare( $self->__insert_sql( 'nested_domains', 2));
	     }





	     $stat->execute($rdb_auto_num,
			    $nested_auto
			   );
	     $count += $stat->rows;

	   };
	   if ($@) {
	     $error = "Could not insert data into nested_domains table [$@]";
	     last;
	   }

	 }




       }



     }



 $self->close_transaction( $error );
 $error and $self->throw( $error );

}




########## NEW SUBS ############



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

  $dbh = $self->open_transaction( 'pfamA_literature_references', 'literature_references' );

  foreach my $en (@{$entries}) {



    eval {
      $rdb_acc = $en->acc;
      $rdb_id = $en->id();

      ####### AUTO NUM
      my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$rdb_acc'");
      $st->execute();
      my($temp_auto) = $st->fetchrow;
      $st->finish();

      $rdb_auto_num = $temp_auto if(defined($temp_auto));


    };

    eval {
      $self->report_mode and
	printf STDERR "Deleting from %s.pfamA_literature_references for entry %s\n", $self->_database_name, $rdb_auto_num;

      $dbh->do("delete from pfamA_literature_references where auto_pfamA = '$rdb_auto_num'");
    };


    ## If there is a literature reference
    if ($en->ann->get_Annotations('reference')) {

      my $count = 1;
      # For each reference
      foreach my $ref ( $en->ann->get_Annotations('reference') ) {

	my($comment, $medline, $authors, $journal, $title);

	## Get out comment if there is one
	if (defined ($ref->comment() ) ) {

	  foreach my $refcomm ( $ref->comment() ) {
	    $comment .=  $refcomm . " ";
	    chop($refcomm);
	  }
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
	    $stat = $dbh->prepare($self->__replace_sql('pfamA_literature_references', 4));
	  }

	  $stat->execute( $rdb_auto_num,
			  $auto_lit,
			  $comment,
			  $count);

	};
	if ($@) {
	  $error = "Could not do the insertion/update on the pfamA_lit_refs table [$@]";
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

sub update_pfamA_database_links {



  my ($self, $entries) = @_;
  my ($stat,
      $dbh,
      $rows,
      $error,
      $rdb_acc,
      $rdb_id,
      $rdb_auto_num
     );

  $dbh = $self->open_transaction( 'pfamA_database_links' );

  foreach my $en (@{$entries}) {

    eval {
      $rdb_acc = $en->acc;
      $rdb_id = $en->id();

      ####### AUTO NUM
      my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$rdb_acc'");
      $st->execute();
      my($temp_auto) = $st->fetchrow;
      $st->finish();

      $rdb_auto_num = $temp_auto if(defined($temp_auto));

    };

    eval {
      $self->report_mode and
	printf STDERR "Deleting from %s.pfamA_database_references for entry %s\n", $self->_database_name, $rdb_auto_num;

      $dbh->do("delete from pfamA_database_links where auto_pfamA = '$rdb_auto_num'");
    };


    ## If there is a database reference

    if ($en->ann->get_Annotations('dblink')) {
      foreach my $link ($en->ann->get_Annotations('dblink')) {

	my($database, $title, $db_link, @other_info, $all_other);
	$database = $link->database;
	$db_link = $link->primary_id();
	@other_info = $link->optional_id();
	foreach (@other_info) {

	  $all_other .= $_;
	}



	if (defined( $link->comment() )) {

	  foreach my $linkcomm ($link->comment ) {
	    $title = $linkcomm ;

	  }
	}

	eval {
	  $stat = undef;
	  if (not defined $stat) {
	    $stat = $dbh->prepare($self->__replace_sql('pfamA_database_links', 5));
	  }

	  $stat->execute( $rdb_auto_num,
			  $database,
			  $title,
			  $db_link,
			  $all_other);

	};
	if ($@) {
	  $error = "Could not do the insertion/update on the pfamA_lit_refs table [$@]";
	  last;
	}


      }				#/ for EACH REFERENCE

    }				#/ if REFERENCES
  }				#/ end for each entries


  $self->close_transaction( $error );
  $error and $self->throw( $error );

  return $rows;



}



sub update_pdb {

  my($self, $file) = @_;

  my($dbh, $st, %store_pdb);
  $dbh = $self->open_transaction( 'pdb');

  $st = $dbh->prepare("select pdb_id, auto_pdb from pdb");
  $st->execute();
  while ( my($pdb_id, $auto_pdb) = $st->fetchrow) {
    $store_pdb{$pdb_id} = $auto_pdb;
  }
  $st->finish();
  

  open(_HEADER, "$file");
  while(<_HEADER>) {
    chop($_);
    my($pdb,$header, $title) = split(/\t/, $_); 
    
    if (defined($store_pdb{$pdb})) {
      ## UPDATE
      $title = $dbh->quote($title);
      $header = $dbh->quote($header);
      my $update_sql = "update pdb set header = $header, title = $title where auto_pdb = '" .$store_pdb{$pdb}. "' "; 
	 
      eval {
	my $update_st = $dbh->prepare($update_sql);
	$update_st->execute();
	$update_st->finish();
      };
      if ($@) {
	my $error = "Could not do the update on the pdb table for auto:" .$store_pdb{$pdb} . "  $ [$@]";
	print "ERROR \n"; exit(0);
	last;
      }
    } else {
      ## ADD
      my @row;
      if(!$title){
		$title = " ";
	}
      push @row, "null" ."~~" . $pdb ."~~". $header ."~~". $title;
      $self->_add_row("pdb", \@row);
    }

  }

  close(_HEADER);


}



############ / end of new subs ##########




=head2 update_pfamseq

 Title   : update_pfamseq
 Usage   :
 Function:
    This function takes a list of hash references each with 4 fields:
    1. acc - accession number
    2. id - identifier
    3. desc - description for entry
    4. len - length of associated sequence
    5. org - hist organism os sequence

    and inserts the information into the pfamseq RDB table
 Returns :
    The number of rows inserted/_updated (should be the size of pfamseq);
    Throws an exception if something went wrong
 Args    :
    1. A ref. to a list of hash refs

 Notes   :
    1. If the transaction handle is undefined, a transaction is created and committed
    for just this _update. If a handle is given, then it is assumed that the update
    is part of a larger transaction.

=cut


sub update_pfamseq {
   my ($self, $entries) = @_;
   my ($stat, $acc_stat,
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
      $rdb_taxonomy, $rdb_is_current, $rdb_non_cons, $rdb_sequence, $rdb_sec_acc, $rdb_created);

   my $auto_pfamseq;

   my($add_count, $new_count);

  # print "UPDATE \n";
   $dbh = $self->open_transaction( 'pfamseq' , 'secondary_pfamseq_acc');
#$dbh->do("delete from secondary_pfamseq_acc") if ($first_file);
#   $dbh->do("delete from pfamseq") if ($first_file);

   foreach my $en (@{$entries}) {
     $rdb_acc = $en->{'acc'};
     $rdb_id = $en->{'id'};
     $rdb_desc = $en->{'desc'};
     $rdb_len = $en->{'len'};
     $rdb_org = $en->{'org'};
     $rdb_crc64= $en->{'crc64'};
     $rdb_is_fragment= $en->{'is_fragment'};
     $rdb_version = $en->{'version'};
     $rdb_taxonomy = $en->{'taxonomy'};

     $rdb_sequence = $en->{'sequence'};
     $rdb_is_current = $en->{'is_current'};
     $rdb_non_cons = $en->{'non_cons'};
     $rdb_sec_acc = $en->{'sec_acc'};


     if (!$rdb_crc64) {
       next ;
     }
    # my $skip = 0;

    # if ($skip) { 
       my $temp_rdb_auto;
       my $st = $dbh->prepare("select auto_pfamseq, created from pfamseq where pfamseq_acc = '$rdb_acc'");
       $st->execute();
       ($temp_rdb_auto, $rdb_created) = $st->fetchrow;
       $st->finish();
       
       if ($temp_rdb_auto) {
	 ### EXISTING SEQ SO UPDATE IT
#	 print "UPDATE \n";
	 $rdb_desc = $dbh->quote($rdb_desc);
	 $rdb_org = $dbh->quote($rdb_org);
	 $rdb_taxonomy = $dbh->quote($rdb_taxonomy);
	 
	 my $update_sql = "update pfamseq set pfamseq_id = '$rdb_id',pfamseq_acc = '$rdb_acc' , crc64 = '$rdb_crc64' , description = $rdb_desc, length = '$rdb_len', species = $rdb_org,  taxonomy = $rdb_taxonomy , is_fragment = '$rdb_is_fragment', version = '$rdb_version',  current = '$rdb_is_current',  non_cons = '$rdb_non_cons', sequence = '$rdb_sequence' where auto_pfamseq = '$temp_rdb_auto' "; 
	 
	 eval {
	   my $update_st = $dbh->prepare($update_sql);
	   $update_st->execute();
	   $update_st->finish();
	 };
	 if ($@) {
	   $error = "Could not do the update on the pfamseq table for auto:$temp_rdb_auto $ [$@]";
	   print "ERROR \n"; exit(0);
	   last;
	 }
	 $auto_pfamseq = $temp_rdb_auto;
	 $add_count++;
       } else {
	 ### NEW SEQ SO INSERT IT 
	 $new_count++;
#	 print "NEW SEQ : $rdb_acc\n";
	 eval {
	   if (not defined $stat) {
	     $stat = $dbh->prepare($self->__insert_sql('pfamseq', 15));
	   }
	   $stat->execute($rdb_auto,
			  $rdb_id,
			  $rdb_acc,
			  $rdb_crc64,
			  $rdb_desc,
			  $rdb_len,
			  $rdb_org,
			  $rdb_taxonomy,
			  $rdb_is_fragment,
			  $rdb_version,
			  $rdb_is_current,
			  $rdb_non_cons,
			  $rdb_sequence,
			  undef,
			  $rdb_created
			 );
	   $auto_pfamseq = $stat->{mysql_insertid};
	   $rows += $stat->rows;
	 };
	 if ($@) {
	   $error = "Could not do the insertion/update on the pfamseq table [$@]";
	   last;
	 }
	 
	
	 my $time_sql = "update pfamseq set created = now() where pfamseq_acc = '$rdb_acc'";
	 my $time_stat = $dbh->prepare($time_sql);
	 $time_stat->execute();
	 $time_stat->finish();

	 
       }
  #   }				#/ SKIP 
     
     if  ($rdb_sec_acc) {
       my (@all_sec_acc) = split(/~/, $rdb_sec_acc);
       ## ADD SECONDARY ACC
       $dbh->do("delete from secondary_pfamseq_acc where auto_pfamseq = '$auto_pfamseq'");
       if (not defined $acc_stat) {
	 $acc_stat = $dbh->prepare($self->__insert_sql('secondary_pfamseq_acc', 2));
       }
       foreach my $sec_acc (@all_sec_acc) {
	 eval {
	   $acc_stat->execute($auto_pfamseq,
			      $sec_acc
			     );
	   
	 };
	 if ($@) {
	   $error = "Could not do the insertion/update on the pfamseq table [$@]";
	   last;
	 }
	 
       }			# end for



     }
     
     
     
   }

   
   $self->close_transaction( $error );
   $error and $self->throw( $error );
   #  printf STDERR "Just added %d records to %s.pfamseq\n", $rows, $self->_database_name();
   
   return $rows;
 }


sub delete_pfamseq {
  my ($self, @deleted_pfamseq) = @_;
  my ($dbh, $auto_pfamseq);
  $dbh = $self->open_transaction( 'pfamseq' , 'old_pfamseq');

  foreach my $pfamseq_acc (@deleted_pfamseq) {
    my ($temp_rdb_auto, $temp_rdb_created, $rdb_acc);
    my $st = $dbh->prepare("select auto_pfamseq, pfamseq_acc, created from pfamseq where pfamseq_acc = '$pfamseq_acc'");
    $st->execute();
    ($temp_rdb_auto, $rdb_acc, $temp_rdb_created) = $st->fetchrow;
    $st->finish();
    next if (!$temp_rdb_auto);
    $dbh->do("delete from pfamseq where auto_pfamseq = '$temp_rdb_auto'");
   # print "EEP: insert into old_pfamseq values ('$temp_rdb_auto', '$pfamseq_acc', null, '$temp_rdb_created')  \n";
    my $null = undef;
    my $st_old = $dbh->prepare($self->__insert_sql('old_pfamseq', 4));
       
  #    print "created: $temp_rdb_created \n";
    $st_old->execute($temp_rdb_auto,
		       $rdb_acc,
		       undef,
		       $temp_rdb_created
		      );
   # print "insert into old_pfamseq values ('$temp_rdb_auto', '$rdb_acc', $null, '$temp_rdb_created') \n";
   # my $st_old = $dbh->prepare("insert into old_pfamseq values ('$temp_rdb_auto', '$rdb_acc', undef, '$temp_rdb_created') ");
    
   # $st_old->execute();
    $st_old->finish();
   # exit(0);
  }
  $self->close_transaction( );
} #/ end sub delete_pfamseq

sub remove_deleted_pfamseq {
  my ($self, @deleted_pfamseq) = @_;
  
  my ($dbh, $auto_pfamseq);
 # $self->{'_connection_count'} = 0;
 my $select_handle = $self->connect();

#  my $pfamseq_type = "pfamseq_" . $type ;

  my $st = $select_handle->prepare("select auto_pfamseq from old_pfamseq ");

  $st->execute();
  my(%deleted_pfamseq);

  while ( my($auto_pfamseq) = $st->fetchrow) {
    $deleted_pfamseq{$auto_pfamseq} = $auto_pfamseq;


  }
  $st->finish();
  $select_handle->disconnect(); 
$self->{'_connection_count'} = 0;


 $dbh = $self->open_transaction( 'pfamA_reg_full', 'pfamA_reg_seed', 'pdbmap', 'other_reg', 'smart_regions', 'pfamB_reg', 'context_pfam_regions', 'msd_data');
  #$dbh = $self->open_transaction( 'pfamA_reg_seed');

#  my @delete_tables = qw(pfamseq);
 my @delete_tables = qw(pfamA_reg_full pfamA_reg_seed pdbmap other_reg smart_regions pfamB_reg context_pfam_regions msd_data );
#my @delete_tables = qw(pfamA_reg_full);
  
  foreach my $auto_pfamseq (sort keys %deleted_pfamseq) {


     foreach my $table (@delete_tables) {
   #    print "delete from $table where auto_pfamseq = $auto_pfamseq \n";
       $dbh->do("delete from $table where auto_pfamseq = '$auto_pfamseq'");
     }

     

   } # end for each acc number

}



sub remove_insig_matches {
  my($self, $family, %sequences) = @_;
  my($dbh);
  $dbh = $self->open_transaction( 'pfamA_reg_full');
  my(%pfamA, %pfamseq);
  
  my $auto_pfamA =  $self->store_pfamA_by_id($family);
  if (!$auto_pfamA) {
    print "ERROR: no auto_pfamA for $family so bombed out \n";
    return ;
  }
  
  foreach (sort keys %sequences) {
   
 
    my $auto_pfamseq = $self->store_all_pfamseq($_);

    if (!$auto_pfamseq) {
      print "ERROR: no auto_pfamseq for pfamseq acc $_ so skipped !! \n";
      next;
    }

    eval {
      
      $dbh->do("delete from pfamA_reg_full  where auto_pfamseq = '$auto_pfamseq' and auto_pfamA = '$auto_pfamA'  and significant = '0'");
    
    };

    if ($@) {
      print "remove insig matches failed as $@ \n";
      exit(0);
    }


  }
  #$dbh->disconnect();

}



sub add_new_insig_matches {
  my($self, $family, $complete_file, $mode, $file_out) = @_;
  #my($dbh);
  #$dbh = $self->open_transaction( 'pfamA_reg_full');
  
  my($auto_pfamA) = $self->store_pfamA_by_id($family);
  
  if (!$auto_pfamA) {
    print "ERROR: no auto_pfamA for family: $family ($complete_file). Tell Mhairi!\n";
    #exit(1);
  }

  my $res = new HMMResults;
  
  open(_OUTPUT, "$complete_file");
  $res->parse_pfam(\*_OUTPUT); 
  
  open(_WRITE_OUT, ">>$file_out");

  foreach my $seq ( $res->eachHMMSequence() ) {
    $seq->evalue("0.0") if !$seq->evalue;
    $seq->bits("0.0")   if !$seq->bits;
    
    foreach my $unit ( $seq->eachHMMUnit() ) {
      
      $unit->evalue("0.0") if !$unit->evalue;
      $unit->bits("0.0")   if !$unit->bits;
      

      
      my $auto_pfamseq = $self->store_all_pfamseq($unit->seqname);
      if (!$auto_pfamseq) {
	print "ERROR: no auto_pfamseq for sequence . "  . $unit->seqname . " , ($complete_file). This is bad so tell Mhairi\n";
	#exit(1);
      }
      

      my $seq_start = $unit->start_seq;
      my $seq_end = $unit->end_seq;
      my $model_start = $unit->start_hmm;
      my $model_end = $unit->end_hmm;
      my $domain_bits_score = $unit->bits;
      my $domain_evalue_score = $unit->evalue;
      my $sequence_bits_score = $seq->bits;
      my $sequence_evalue_score = $seq->evalue;
      my $in_full = 0;      
      my $is_significant = 0;
      my $tree_order = 0;

      print _WRITE_OUT "$auto_pfamA\t$auto_pfamseq\t$seq_start\t$seq_end\t$model_start\t$model_end\t$domain_bits_score\t$domain_evalue_score\t$sequence_bits_score\t$sequence_evalue_score\t$mode\t$is_significant\t$in_full\t$tree_order\n";

      
      
    }
  }
  
  close(_OUTPUT) || print ("Could not close file [$complete_file]");
  close(_WRITE_OUT);
 
  
  
  
}

sub fix_the_evaules {
  my($self, $family, $pfamseq_count, $evalue_file_out) = @_;
  my($dbh);
  $dbh = $self->open_transaction( 'pfamA_reg_full', 'pfamA');
  
  my $auto_pfamA = $self->store_pfamA_by_id($family);
  if (!$auto_pfamA) {
    print "no auto_pfamA for $family. This is bad so let Mhairi know\n";
   # exit(0);
  }

  open(_OUT, ">>$evalue_file_out");
  ## pfamA mu, lambda values
  my $st_pfamA = $dbh->prepare("select ls_mu, ls_kappa, fs_mu, fs_kappa from pfamA where auto_pfamA = '$auto_pfamA'");
  $st_pfamA->execute();
  my ($ls_mu, $ls_kappa, $fs_mu, $fs_kappa) = $st_pfamA->fetchrow();
  $st_pfamA->finish();

  ## ALL THE PFAMA REGIONS
   my $st_full = $dbh->prepare("select * from pfamA_reg_full where auto_pfamA = '$auto_pfamA'");
  $st_full->execute();
  my ($auto, $acc);
  while ( my($auto_pfamA, $auto_pfamseq, $seq_start, $seq_end, $model_start, $model_end, $domain_bits_score, $domain_evalue_score, $sequence_bits_score, $sequence_evalue_score, $mode, $significant, $in_full, $tree_order) = $st_full->fetchrow) {
  #  print "$auto_pfamA, $auto_pfamseq, $seq_start, $seq_end, $model_start, $model_end, $domain_bits_score, $domain_evalue_score, $sequence_bits_score, $sequence_evalue_score, $mode, $significant, $in_full, $tree_order \n";
    my($mu, $kappa);
    if ($mode =~ /ls/) {
      $mu = $ls_mu; $kappa = $ls_kappa;
    } else {
      $mu = $fs_mu; $kappa = $fs_kappa
    }

    ## 984936 is the select count(*) from pfamseq
    my $new_domain_evalue_score = &HMMResults::bits2evalue( $domain_bits_score, $mu, $kappa, $pfamseq_count );
    my $new_sequence_evalue_score = &HMMResults::bits2evalue( $sequence_bits_score, $mu, $kappa,  $pfamseq_count);
  #  print "old: $domain_evalue_score , new: $new_domain_evalue_score \n" if ($new_domain_evalue_score != $domain_evalue_score);
    print _OUT "$auto_pfamA\t$auto_pfamseq\t$seq_start\t$seq_end\t$model_start\t$model_end\t$domain_bits_score\t$new_domain_evalue_score\t$sequence_bits_score\t$new_sequence_evalue_score\t$mode\t$significant\t$in_full\t$tree_order\n";

  }
  $st_full->finish();


  $dbh->do("delete from pfamA_reg_full where auto_pfamA = '$auto_pfamA'");
  
  close(_OUT);
}



=head2 update_other_reg

Title   : update_other_reg
  Usage   :
 Function:
This function takes a list of hash references each with 5 fields:
1. seqid - id of pfamseq seq
  2. from - start residue
  3. to - end residue
  4. type - annotation type for this region
  5. source - source of the annotation for this region
  
  and inserts the information into the other_reg RDB table
  Returns :
  The number of rows inserted/_updated;
Throws an exception if something went wrong
  Args    :
  1. A ref. to a list of hash refs
  
  Notes   :
  1. If the transaction handle is undefined, a transaction is created and committed
  for just this _update. If a handle is given, then it is assumed that the update
  is part of a larger transaction.

=cut
  
  
  sub update_other_reg {
    my ($self, $entries, $read_in_pfamseq) = @_;
    my ($stat,
	$dbh,
	$rows,
	$error,
	$rdb_seqid,
	$rdb_from,
	$rdb_to,
	$rdb_type,
	$rdb_source,
	$rdb_score,
	$rdb_orie,
	$rdb_other_auto);
    
    $dbh = $self->open_transaction( 'other_reg' , 'pfamseq');
    
    foreach my $en (@{$entries}) {
      $rdb_seqid = $en->{'seqid'};
      
      ## This is a flag use when the pfamseq_acc has not been converted to the auto_pfamseq as used in new schema.
       if ($read_in_pfamseq) {
	 my $st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_acc = '$rdb_seqid'");
	 $st->execute();
	 $rdb_seqid = $st->fetchrow;
	 $st->finish();
       }
       if ($@) {
	 $error = "Problem with getting the auto_pfamseq from pfamseq table, maybe a problem with the format";
	 last;
       }

       $rdb_from = $en->{'from'};
       $rdb_to = $en->{'to'};
       $rdb_type = $en->{'type'};
       $rdb_source = $en->{'source'};
       $rdb_score = $en->{'score'};
       $rdb_orie = $en->{'orientation'};

       eval {
	   if (not defined $stat) {
	       $stat = $dbh->prepare($self->__replace_sql('other_reg', 8));
	   }
	   $stat->execute( $rdb_other_auto,
			  $rdb_seqid,
			   $rdb_from,
			   $rdb_to,
			   $rdb_type,
			   $rdb_source,
			   $rdb_score,
			   $rdb_orie
			 );
	   $rows += $stat->rows;
       };
       if ($@) {
	   $error = "Could not do the insertion/update on the other_reg table [$@]";
	   last;
       }
   }

   $self->close_transaction( $error );
   $error and $self->throw( $error );

   $self->report_mode and
       printf STDERR "Just added %d records to %s.other_reg\n", $rows, $self->_database_name;

   return $rows;
}


###################
######## THIS SUB READ ALL THE INTERPRO FILES FROM A DIR AND PUTS INFO + QiuckGO into RDB :-)
###################


sub upload_interpro_from_dir {

  my($self, $upload_dir) = @_;

  my($stat, $rows, $error);

  my  $dbh = $self->open_transaction( 'pfamA' ,  'pfam_to_interpro',  'interpro_and_go');


  eval {
    $self->report_mode and
      printf STDERR "Deleting from pfamA_interpro\n";

    $dbh->do("delete from pfam_to_interpro");
    $dbh->do("delete from interpro_and_go");
  };
  if ($@) {
    $error = "Could not delete from pfam_to_interpro";
    last;
  }

  ## get all the acc & auto in one go
  my %store_acc;
  my $st = $dbh->prepare("select auto_pfamA, pfamA_acc from pfamA");
  $st->execute();
  my ($auto, $acc);
  while ( ($auto, $acc) = $st->fetchrow) {
    $store_acc{$acc} = $auto;
  }
  $st->finish();

  close(_READ);

  opendir(_EX_DIR,$upload_dir) || die("Could not open $upload_dir $!");


  

  foreach my $file ( readdir(_EX_DIR) ) {
    $file =~ /^\.+$/ && next;

   
    my ($acc_type, $junk) = split(/\./, $file);


    my $pfam_acc;
    if ($acc_type =~ /^PF/) {
      $pfam_acc = $acc_type;
    }


    ## temp taken out
    my $rdb_auto_pfamA = $store_acc{$pfam_acc};

      my $open_file = $upload_dir . "/" . $file;
      my($interpro_id, $interpro_abstract, $function, $process, $component, $auto_interpro);

      open(_FILE, $open_file) or die "CANNA open $file $! \n";

      while(<_FILE>) {


	if ($_ =~ /^ABSTRACT\s+(.*)/) {
	  $interpro_abstract .= $1;
	} elsif ($_ =~  /^INTERPRO\s+(\S+)/){
	  $interpro_id = $1;
	} elsif ($_ =~  /^PROCESS\s+(.*)/) {
	  $process = $1;
	} elsif ($_ =~  /^COMPONENT\s+(.*)/){
	  $component = $1;
	} elsif ($_ =~  /^FUNCTION\s+(.*)/) {
	  $function = $1;
	} else {
	  $interpro_abstract .= $_;
	}


      }

  #  $interpro_count++;
      eval {
	if (not defined $stat) {
	  $stat = $dbh->prepare($self->__replace_sql('interpro_and_go', 6));
	}
	$stat->execute( $auto_interpro,
			$interpro_id,
			$interpro_abstract,
			$function,
			$component,
			$process
		      );
	$rows += $stat->rows;
	$auto_interpro = $stat->{mysql_insertid};
      };
      if ($@) {
	my $error = "Could not do the insertion/update on the pfamA_interpro table [$@]";
	last;
      }

      if ($rdb_auto_pfamA) {
	my $st;
	eval {
	  if (not defined $st) {
	    $st = $dbh->prepare($self->__replace_sql('pfam_to_interpro', 2));
	  }
	  $st->execute( $rdb_auto_pfamA,
			$auto_interpro
		      );
	  $rows += $st->rows;
	};



	if ($@) {
	  my $error = "Could not do the insertion/update on the pfamA_interpro table [$@]";
	  last;
	}

      }

    close(_FILE);



  }

  return $rows;
  $self->close_transaction( $error );
}







sub upload_pfamB_database_links {
  my ($self, $auto_pfamA, $family_desc_file) = @_;

  my ($dbh);


  $dbh = $self->open_transaction( 'pfamA_database_links'); # 'pfamA'

 
  open(_DESC, "$family_desc_file") or print "CANNNA OPEN $family_desc_file as $! \n";;
  while(<_DESC>) {




    if ($_ =~ /^DR\s+PFAMB;\s+(\w+);/) {
      my $pfamB_acc = $1;
      my $empty = "";

    
      my $st = $dbh->prepare("insert into pfamA_database_links values('$auto_pfamA', 'PFAMB', '$empty', '$pfamB_acc' , '$empty' )");
      eval {
	$st->execute();
	$st->finish();
      };
      if ($@) {
	print "BOMBED AS $@ \n";
      }

    }


  }
  close(_DESC) or print "CANNNA CLOSE $family_desc_file as $! \n";
}


sub upload_tree_information {

  my($self, $family) = @_;
  my $error;
  my ($dbh,
      @regions );

  $dbh = $self->open_transaction('pfamA', 'pfamA_reg_full', 'pfamseq');

  chop($family)   if ($family =~ /\/$/);
  open(_FAMILY, "$family/ALIGN.ann") or print "canna open the family ALIGN.ann file for : $family $! \n";

  my $got_gs = 0;
  my $count = 1;
  my $auto_pfamA;
  my $no_auto = 1;
  my $acc;
  my %store_pfamseq;


  ###### UPDATE TO DELETE TREE BEFORE ADDS NEXT LOT !
  while(<_FAMILY>) {
    $got_gs  = 1 if ($_ =~ /^\#\=GS/);
    $acc = $1 if ($_ =~ /^\#\=GF\s+AC\s+(\S+)/);

    if ( ($acc) && ($no_auto) ) {
      eval {
	####### AUTO NUM
	my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$acc'");
	$st->execute();
	($auto_pfamA) = $st->fetchrow;
	$st->finish();
      };
      if ($@) {
	 print STDERR "Couldnt get auto_pfamA for this family, acc: $acc as !@  \n";
      }

	### RESET ALL TREE ORDER To NULL
	eval {
	  my $reset_tree;
	  my $st = $dbh->prepare("update pfamA_reg_full set tree_order = '$reset_tree' where auto_pfamA = '$auto_pfamA'");
	  $st->execute();
	  $st->finish();


	};


      $no_auto = 0;

      ### RESET THE COUNT !

    }


    if ( ($got_gs) && ($_ =~ /^\#\=/) ) {
      my $temp_id = $1 if ($_ =~ /^\#\=GS\s+(\S+)\s+\S+/);
      my($id, $residues) = split(/\//, $temp_id);
      my($start, $end) = split(/-/, $residues);

      my $auto_pfamseq;
      if (!defined($store_pfamseq{$id})) {
	eval {

	  ####### AUTO NUM
	  my $st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_id = '$id'");
	  $st->execute();
	  ($auto_pfamseq) = $st->fetchrow;

	  $st->finish();
	};

	$store_pfamseq{$id} = $auto_pfamseq;

      } else {
	$auto_pfamseq = $store_pfamseq{$id};
      }

      my $st = $dbh->prepare("update pfamA_reg_full set tree_order = '$count' where auto_pfamA = '$auto_pfamA' and auto_pfamseq = '$auto_pfamseq' and seq_start = '$start' and seq_end = '$end' and in_full = 1");

      $st->execute();
      $st->finish();



      $count++;
    }

  }

  close(_FAMILY);





#  #####  num full & num seed to pfamA table

  if (!$auto_pfamA) {
    exit_with_mail("The tree order wasnt uploaded as #=GF AC couldnt be read from ALIGN.ann file", $family);
    return 0;

  }

  my $seed = $family . "/SEED.ann";


 open(_SEED, $seed) || $self->throw("For E entry object, got no valid directory for SEED.ann $family ($seed) $!");


  my ($num_seed, $num_full);
  while(<_SEED>) {
    $_ =~ /^\#\=GF\s+SQ\s+(.*)/ && do {
      $num_seed = $1;
    };
    last if ( $num_seed);



  }

  close(_SEED);

  open(_FULL,"$family/ALIGN.ann") || $self->throw("For entry object, got no valid directory for ALIGN.ann $family $!");

   while(<_FULL>) {

     $_ =~ /^\#\=GF\s+SQ\s+(.*)/ && do {
       $num_full = $1;
     };
     last if ( $num_full);



  }

   close(_SEED);


  my $st = $dbh->prepare("update pfamA set num_seed = '$num_seed', num_full = '$num_full'  where auto_pfamA = '$auto_pfamA' ");

  $st->execute();
  $st->finish();


  $self->close_transaction( $error );
}

sub exit_with_mail {
    my $message = shift;
    my $id = shift;
    my $me = `whoami`;
    chop $me;
    my $mail = "$me\@sanger.ac.uk";
    open(MAIL, "|/usr/lib/sendmail -oi -t") or die "Can't fork for sendmail: $!\n";
    print MAIL "From: $mail\n";
    print MAIL "To: $mail\n";
    print MAIL "Subject: Error in view process for $id\n";
    print MAIL "\n$message\n";
    close(MAIL);
    print STDERR "$message\n";
    exit(1);
}


sub delete_pfamA_seed_full {


  my($self, $acc) = @_;
  my $error;
  my ($dbh,
      @regions, $auto_pfamA );

  $dbh = $self->open_transaction('pfamA', 'pfamA_reg_full', 'pfamA_reg_seed');
  ####### AUTO NUM

  eval {
    my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$acc' ;");
    $st->execute();
    ($auto_pfamA) = $st->fetchrow;
    $st->finish();
  };


  if ($auto_pfamA) {
    $dbh->do("delete from pfamA_reg_full where auto_pfamA = '$auto_pfamA'");
    $dbh->do("delete from pfamA_reg_seed where auto_pfamA = '$auto_pfamA'");

  } else {
    $self->throw("Couldnt delete pfamA_reg_full/pfamA_reg_seed as no auto_pfamA for $acc ! \n");
  }


  $self->close_transaction( $error );


}


sub upload_context_pfam_regions {

  my ($self, $file,  $delete_table, $pfam_thres, $prob_thres) = @_;

  my  $dbh = $self->open_transaction( 'pfamA' , 'pfamseq', 'context_pfam_regions');

  if ($delete_table) {

    eval {
      $dbh->do("delete from context_pfam_regions");

    };
    if ($@) {
      print "There was an error deleting from context_pfam_regions $@\n";

    }
    return 1;

  }

  open(_FILE, "$file");


  while(<_FILE>) {
    if ($_ =~ /^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
      my $pfamseq_acc = $1;
      my $pfamA_id = $2;
      my $seq_start = $3;
      my $seq_end = $4;
      my $sentence_score = $5;
      my $contribution_to_sentence_score = $6;


      my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_id = '$pfamA_id'");
      $st->execute();
      my $auto_pfamA = $st->fetchrow;
      $st->finish();
      if (($@) || (!$auto_pfamA) ) {
	print "Couldnt get auto_pfamA for pfamA id: $pfamA_id as $@ \n";
      }

      my $seq_st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_acc = '$pfamseq_acc'");
      $seq_st->execute();
      my $auto_pfamseq = $seq_st->fetchrow;
      $seq_st->finish();
      if ($@) {
	print "Couldnt get auto_pfamseq for pfamseq id: $pfamseq_acc as $@ \n";
      }

      eval {
	my $stat;
	  $stat = $dbh->prepare($self->__replace_sql('context_pfam_regions', 8));



	$stat->execute(	$auto_pfamseq,
			$auto_pfamA,
			$seq_start,
			$seq_end,
			$sentence_score,
			$contribution_to_sentence_score,
			$pfam_thres,
			$prob_thres
		      );
	$stat->rows;
      };

      if ($@) {
	print "Problem with inserting row into context_pfam_regions  as: $@ \n";
	exit(1);

      }


    }





  }




}


sub upload_smart_regions {

  my ($self, $file, $delete_table) = @_;
 # print "HERE HERE \n\n"; exit (0);


  my  $dbh = $self->open_transaction( 'smart' , 'pfamseq', 'smart_regions');

  if ($delete_table) {

    eval {
      $dbh->do("delete from smart_regions");

    };
    if ($@) {
      print "There was an error deleting from smart_regions $@\n";

    }


  }
  
  my %store_pfamseq;

  open(_FILE, "$file");

  while(<_FILE>) {
    if ($_ =~ /^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
      my $smart_id = $1;
      my $pfamseq_acc = $2;
      my $seq_start = $3;
      my $seq_end = $4;
      my $bits = $5;
      my $evalue = $6;


      my $st = $dbh->prepare("select auto_smart from smart where smart_id = '$smart_id'");
      $st->execute();
      my $auto_smart = $st->fetchrow;
      $st->finish();
      if ($@) {
	print "Couldnt get auto_smart for smart id: $smart_id as $@ \n";
      }


      my $auto_pfamseq;
      if(defined($store_pfamseq{$pfamseq_acc})) {

	$auto_pfamseq = $store_pfamseq{$pfamseq_acc};
	
      } else {

	my $seq_st = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_acc = '$pfamseq_acc'");
	$seq_st->execute();
	$auto_pfamseq = $seq_st->fetchrow;
	$seq_st->finish();
	if ($@) {
	  print "Couldnt get auto_pfamseq for pfamseq id: $pfamseq_acc as $@ \n";
	}
	
      }


      eval {
	my $stat;
	  $stat = $dbh->prepare($self->__replace_sql('smart_regions', 6));



	   $stat->execute( $auto_smart,
			   $auto_pfamseq,
			   $seq_start,
			   $seq_end,
			   $bits,
			   $evalue
			 );
	    $stat->rows;
       };

      if ($@) {
	print "Problem with inserting row into smart_regions as: $@ \n";
	exit(1);

      }


    }

  }


close(_FILE);

}



sub upload_cogs_links {
  my($self, $file, $db) = @_;

  ### FILE IN FORMANT $db_link $pfamA_acc
 # print "FILE: $file && db: $db \n";
  if ($file && $db) {


    my  $dbh = $self->open_transaction('pfamA', 'pfamA_database_links');




    my $st = $dbh->prepare("delete from pfamA_database_links where db_id = '$db'");
   # print "ST: delete from pfamA_database_links where db_id = '$db' \n";
    #exit(0);
    $st->execute();
    $st->finish();

    open(_FILE, $file) or die "CANNA OPEN COGS FILE: $file \n";;

    while(<_FILE>) {
      chop($_);
      if ($_ =~ /^(\S+)\s+(\S+)/) {
	my($db_link, $pfamA_acc);
	$db_link = $1;
	next if ($db_link =~ /nolink/i);

	$pfamA_acc = $2;
	my $auto_pfamA;
	eval {

	  ####### AUTO NUM
	  my $stat = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$pfamA_acc'");
	  $stat->execute();
	  ($auto_pfamA) = $stat->fetchrow;
	  $stat->finish();

	};
	if ($@) {
	  print "Couldnt get auto_pfamA for $pfamA_acc \n";
	  exit(0);
	}

	eval {

	  my $st = $dbh->prepare("insert into pfamA_database_links values('$auto_pfamA', '$db', '', '$db_link', '') ");
	#  print "SQL UPDATE : insert into pfamA_database_links values('$auto_pfamA', '$db', '', '$db_link', '')   \n";
	  $st->execute();
	  $st->finish();
	};

	if ($@) {
	  print "Couldnt upload to pfamA_database_links:: insert into pfamA_database_links values('$auto_pfamA', '$db', '', $db_link, '') as $@  \n";
	  exit(0);
	}


      } #/ end IF VALID FORMAT IN FILE

    } #/ end WHILE



    close(_FILE);

  } else {

    print "Cant updata pfamA_database_links as need file & new database to link to \n";

  }


}

sub load_pfamA_web {
  my($self, $file) = @_;
 
  open(_FILE, $file);
  my $select_handle = $self->connect();
  
  while(<_FILE>) {
    chop($_);
    my ($pfamA_acc, $avl, $avp, $status) = split(/\t/, $_);

  

  #  print "ACC: $pfamA_acc \n";

    my $st = $select_handle->prepare("select auto_pfamA from pfamA where pfamA_acc = '$pfamA_acc' ");
    $st->execute();
    my $auto_pfamA = $st->fetchrow;
    $st->finish();
  #  print "auto: $auto_pfamA \n";
    print " select distinct  pfamseq_id from pfamA_reg_full, pfamseq where auto_pfamA = '987' and in_full = '1' and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq \n";
   my $st_1 = $select_handle->prepare(" select distinct  pfamseq_id from pfamA_reg_full, pfamseq where auto_pfamA = '987' and in_full = '1' and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq");
    my $full_count = 0;
    while ( my($pfamseq) = $st_1->fetchrow) {
      print "$pfamseq \n";
      $full_count++;
    }
    $st_1->finish();
    print "FULL: $full_count \n";
    exit(0);
  }


  $select_handle->disconnect(); 
  
  $self->{'_connection_count'} = 0;
  
 # $dbh = $self->open_transaction( 'pfamA_web');


## end

 #$self->close_transaction( );
 
 #  $dbh->close_transaction();
#  $select_handle = $self->connect();

 # $self->close_transaction( );
#  $select_handle->disconnect;
#  print "NUMBER of invalid lines: $invalid_lines \n";
}


sub add_rosetta_data {

  my($self, $group_model_confidences_file , $model_links_file, $delete_table) = @_;



 my  $dbh = $self->open_transaction( 'pfamA' , 'pdb', 'rosetta_pfam_structures' ,'rosetta_pfam_values');
  if ($delete_table) {

    eval {
      $dbh->do("delete from rosetta_pfam_structures");
      $dbh->do("delete from rosetta_pfam_values");

    };
    if ($@) {
      print "There was an error deleting from rosetta_pfam_structures & rosetta_pfam_values $@\n";

    }


  }

  ########################################################
  ### ADD THE GROUP VALUES ###########
  ########################################################

  open(_GROUP, "$group_model_confidences_file") or die "Cant open file: $group_model_confidences_file as !@ \n";;

  while(<_GROUP>) {
    chop($_);


    if ($_ =~ /^\S+\s+\S+\s+\S+\s+(\S+)\s+\S+\s+(\S+)\s+\S+\s+(\S+)/) {
      my ($pfamA_acc, $group_confidence, $scop_confidence);
      $pfamA_acc = $1;
      $group_confidence = $2;
      $scop_confidence = $3;

      my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$pfamA_acc'");
      $st->execute();
      my $auto_pfamA = $st->fetchrow;
      $st->finish();
      if (($@) || (!$auto_pfamA) ) {
	print "Couldnt get auto_pfamA for pfamA acc: $pfamA_acc as $@ \n";
      }

      eval {
	my $stat;
	$stat = $dbh->prepare($self->__replace_sql('rosetta_pfam_values', 3));



	$stat->execute(	$auto_pfamA,
			$group_confidence,
			$scop_confidence
		      );
	$stat->rows;
      };

      if ($@) {
	print "Problem with inserting row into rosetta_pfam_values  as: $@ \n";
	exit(1);

      }
    }

  }
  close(_GROUP);

  ########################################################
  ### ADD THE GROUP VALUES ###########
  ########################################################


  open(_LINKS, "$model_links_file");

  while(<_LINKS>) {
    if ($_ =~ /^\S+\s+(\S+)\s+\S+\s+(\d+)\s+\S+\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
      my $pfamA_acc = $1;
      my $model_num = $2;
      my $temp = $3;
      my $pdb = $4;
      my $mammoth = $5;
      my $length = $6;

      my($junk, $structure) = split(/\//, $temp);


      ## Get auto_pfamA
      my $st = $dbh->prepare("select auto_pfamA from pfamA where pfamA_acc = '$pfamA_acc'");
      $st->execute();
      my $auto_pfamA = $st->fetchrow;
      $st->finish();
      if (($@) || (!$auto_pfamA) ) {
	print "Couldnt get auto_pfamA for pfamA acc: $pfamA_acc as $@ \n";
      }

      my $chain;
      if ( (length($pdb) eq 5) && ($pdb !~ /1xxx/i) ){
	$chain = substr($pdb, 4, 1);

      }
      my $old = $pdb;
      $pdb = undef;
      $pdb = substr($old, 0, 4);

      my $auto_pdb;
      if ($pdb !~ /1xxx/i) {
	## Get auto_pdb
	my $st = $dbh->prepare("select auto_pdb from pdb where pdb_id = '$pdb'");
	$st->execute();
	$auto_pdb = $st->fetchrow;
	$st->finish();
	if (($@) || (!$auto_pdb) ) {
	  print "Couldnt get auto_pdb for pdb id: $pdb as $@ \n";
	}
      } else {
	$auto_pdb = 0;
      }


      ## Add the row to the RDB

      eval {
	my $stat;
	$stat = $dbh->prepare($self->__replace_sql('rosetta_pfam_structures', 7));



	$stat->execute(	$auto_pfamA,
			$structure,
			$model_num,
			$auto_pdb,
			$chain,
			$mammoth,
			$length
		      );
	$stat->rows;
      };

      if ($@) {
	print "Problem with inserting row into rosetta_pfam_structures  as: $@ \n";
	exit(1);

      }



    }


  }

  close(_LINKS);








}


sub genomic_upload {
  my($self, $first, $ncbi_code, $ncbi_species,  $genomic_sequence_length, $existing_pfamA,$existing_pfamseq, @store_acc) = @_;

  
  my(@genomic_sequences);


  if ($first) {
      my  $dbh = $self->open_transaction('genome_species', 'genome_seqs');
      my $st = $dbh->prepare("delete from genome_species");
      $st->execute();
      $st->finish();
    
      $st = $dbh->prepare("delete from genome_seqs");
      $st->execute();
      $st->finish();
      $self->close_transaction;
    
  }
  #$self->close_transaction;
  my $dbh = $self->connect;
  print STDERR "HERE\n";
  ###### FIRST ADD THE NCBI SPECIES CODE

  
  

    my $group;
    foreach (@store_acc) {
      my($acc, $id) = split(/~/, $_);
      my $seq_st2 = $dbh->prepare("select taxonomy from pfamseq where pfamseq_acc = '$acc' and pfamseq_id = '$id'");
      $seq_st2->execute();
      my $taxonomy = $seq_st2->fetchrow;
      $seq_st2->finish();
      
      if ($taxonomy) {
	$group = $1 if ($taxonomy =~ /^(\S+)\;\s/);
	last;
      }
    }



  ## NOW ADD THE GENOMIC REGIONS!
# open(_WRITE, ">>$write_file"); 
  my(%all_pfamseq_store);
  if (!$existing_pfamA) {
    my $all_pfamseq_stat =  $dbh->prepare("select auto_pfamseq, length, pfamseq_acc from pfamseq");
    $all_pfamseq_stat->execute();
    while (my ($auto_pfamseq, $length, $pfamseq_acc) = $all_pfamseq_stat->fetchrow) {
      $all_pfamseq_store{$pfamseq_acc} = $auto_pfamseq . "~" .  $length;
    }
  } else {
     %all_pfamseq_store = %{$existing_pfamseq};
  }

  my(%genome_seqs, $no_seq_count);
  my $total_genome_seqs;
  foreach (@store_acc) {
    my($acc, $id) = split(/~/, $_);
    $total_genome_seqs++;
   # my $seq_st = $dbh->prepare("select auto_pfamseq, length from pfamseq where pfamseq_acc = '$acc'");
   # $seq_st->execute();
    my ($auto_pfamseq, $length) = split(/~/, $all_pfamseq_store{$acc});
   # $seq_st->finish();
    
    if ($auto_pfamseq) {
      push @genomic_sequences, "$ncbi_code~$auto_pfamseq~$length";
      #print _WRITE "$ncbi_code\t$auto_pfamseq\n";
      $genome_seqs{$auto_pfamseq} = $auto_pfamseq;
      
    } else {
      $no_seq_count++;
  #    print "NO SEQ: $acc -> $id \n";
    }

  }
  
#  #print "NO SEQ COUNT: $no_seq_count\n";
   open(_ALL_SEQ, ">>/pfam/db/web_temp/gen_seq.dat");
  foreach (sort keys %genome_seqs) {
    print _ALL_SEQ "$ncbi_code\t$_\n";
  }
  close(_ALL_SEQ);
 
  my(%store_pfamseq_pfamA, @new_genomic_sequences);

  if (!$existing_pfamA) {
    print "NEW SELECT PFAMSEQ \n";
    my $pfamseq_stat =  $dbh->prepare("select auto_pfamA, auto_pfamseq , seq_start, seq_end from pfamA_reg_full where in_full = '1'");
    $pfamseq_stat->execute();
    while (my ($auto_pfamA, $auto_pfamseq, $seq_start, $seq_end) = $pfamseq_stat->fetchrow) {
     # $store_pfamseq_pfamA{$auto_pfamseq} = $auto_pfamA;
     # print "PFAMSEQ: $auto_pfamseq, PFAMA: $auto_pfamA: " .$store_pfamseq_pfamA{$auto_pfamseq} . " \n";
      if (defined($store_pfamseq_pfamA{$auto_pfamseq})) {
	#print "DEFINED\n"; sleep 1;
	$store_pfamseq_pfamA{$auto_pfamseq} .= "~$auto_pfamA-$seq_start-$seq_end";
      } else {
	#print "NOT DEFINED\n";
	$store_pfamseq_pfamA{$auto_pfamseq} = "$auto_pfamA-$seq_start-$seq_end";
      }
      
      
    }

  } else {
    print "EXISTING PFAMSEQ\n";
    %store_pfamseq_pfamA = %{$existing_pfamA};
  }


  open(_FILE, ">>/pfam/db/web_temp/proteome_update.txt");
  my $sum_domains;
  ### PROTEINS FOR AN INDIVIDUAL GENOME
  my (%all_doms_count, $total_seqs, $seqs_with_pfamA, $total_residues, $domain_residues, %distinct_pfamA);
  foreach (@genomic_sequences) {
    $total_seqs++;
    my ($ncbi, $auto_pfamseq, $length) = split(/~/, $_);    
    $total_residues += $length;
    my $all_seq_pfam_doms = $store_pfamseq_pfamA{$auto_pfamseq};
    my (@all) = split(/~/, $all_seq_pfam_doms);
    
    $seqs_with_pfamA++ if (@all); ## INCREMENT DISTINCT PFAMSEQ COUNT
    
    my (%auto_pfamA);
    foreach (@all) {
      my($auto_pfamA, $seq_start, $seq_end) = split(/-/, $_);
      my $tmp_dom = $seq_end - $seq_start + 1;
      $domain_residues += $tmp_dom;
      $auto_pfamA{$auto_pfamA}++;
    }
    
    foreach (sort keys %auto_pfamA) {
      $distinct_pfamA{$_} += $auto_pfamA{$_};
      print _FILE "$ncbi\t$auto_pfamseq\t$_\t". $auto_pfamA{$_}.  "\n";
      $sum_domains++;
    }    
  }


  my ($seq_cov, $res_cov);
  $seq_cov = ($seqs_with_pfamA / $total_genome_seqs) * 100 if ($total_genome_seqs);
  $res_cov =  ($domain_residues / $total_residues) * 100 if ($total_residues);
  my($distinct);
  foreach (sort keys %distinct_pfamA) {
    $distinct++;
  }


    my $species_stat;
    $species_stat = $dbh->prepare($self->__replace_sql('genome_species', 10));

  eval {  
    
    $species_stat->execute(	$ncbi_code,
				$ncbi_species,
				$group,
				$distinct,
				$sum_domains,
				$seqs_with_pfamA,
				$seq_cov,
				$res_cov,
				$total_genome_seqs,
				$total_residues
			  );
    $species_stat->rows;
  };
  
  if ($@) {
    print "Problem with inserting row into genome_species as: $@ \n";
    exit(1);
    
  }



  close(_FILE);
  return \%store_pfamseq_pfamA, \%all_pfamseq_store;
}


sub load_architecture {
  my($self, $tmp_arch) = @_;
  my(%arch) = %{$tmp_arch};
  my(%frag);
 

  my (@frag) =  $self->query("select auto_pfamseq from pfamseq where is_fragment = '1'");
  foreach my $frag (@frag) {
      my($frag_seq) = @{$frag};
      $frag{$frag_seq} = $frag_seq;
  }
  # close(_FRAG);
  my  $dbh = $self->open_transaction('pfamA_architecture', 'architecture', 'pfamA', 'pfamseq_architecture');
  $dbh->do("delete  from pfamA_architecture");
  $dbh->do("delete  from pfamseq_architecture");
  
  my $st = $dbh->prepare("select auto_pfamA, pfamA_id from pfamA ");
  
  $st->execute();
  my(%pfamA);
  
  while ( my($auto_pfamA, $pfamA_id) = $st->fetchrow) {
      $pfamA{$pfamA_id} = $auto_pfamA;
      
  }
  $st->finish();
  

  my(%distinct_pfamA);
  
  open(_SEQ, ">/pfam/db/web_temp/arch_seq.dat") || die "Could not open /pfam/db/web_temp/arch_seq.dat :[$!]\n";
  open(_PFAM, ">/pfam/db/web_temp/arch_pfamA.dat") || die "Could not open /pfam/db/web_temp/arch_pfamA.dat :[$!]\n";
  foreach my $architecture (sort keys %arch) {
      my($auto_arch);
      #print "ARCH: $architecture \n" if ($architecture =~ /\~~/);
      my $st_arch = $dbh->prepare("select auto_architecture from architecture where architecture = '$architecture'");
      $st_arch->execute();
      ($auto_arch) = $st_arch->fetchrow();
    
   
      # print "STR: " .$arch{$architecture} . " \n";
      my(@auto_pfamseq) = split(/~/, $arch{$architecture});

      my $arch_stat;
      $arch_stat = $dbh->prepare($self->__replace_sql('architecture', 2));

      eval {  
	  
	  $arch_stat->execute($auto_arch,
			      $architecture
			      );
      
	  $auto_arch = $arch_stat->{mysql_insertid};
      };


      foreach my $auto_pfamseq (@auto_pfamseq) {
	  my $is_frag;
	  $is_frag = "1" if (defined($frag{$auto_pfamseq}));
	  print _SEQ "$auto_pfamseq\t$auto_arch\t$is_frag\n";
	  
      }

      my(@arch_domains) = split(/~/, $architecture);
      foreach my $arch (@arch_domains) {
	  my $auto_pfamA = $pfamA{$arch};
	  my $join = $auto_arch . "~" . $auto_pfamA;
	  $distinct_pfamA{$join} = $join;
	  #print _PFAM "$auto_arch\t$auto_pfamA\n";
      }
  }
  
  foreach (sort keys %distinct_pfamA) {
      my($auto_arch, $auto_pfam)  = split(/~/, $distinct_pfamA{$_});
      print _PFAM "$auto_pfam\t$auto_arch\n";
  }
  close(_SEQ);
  close(_PFAM);

  $self->load_generic_file_to_rdb("/pfam/db/web_temp/arch_seq.dat", "pfamseq_architecture", 1);
  $self->load_generic_file_to_rdb("/pfam/db/web_temp/arch_pfamA.dat", "pfamA_architecture", 1);

}



sub pfamA_web_data {
  my($self, $file) = @_;
  
  
  my ($dbh, $auto_pfamseq);

  my $select_handle = $self->connect();
  
 
  
  my $st = $select_handle->prepare("select auto_pfamA, pfamA_acc from pfamA ");
  
  $st->execute();
  my(%pfamA);
  
  while ( my($auto_pfamA, $pfamA_acc) = $st->fetchrow) {
    $pfamA{$pfamA_acc} = $auto_pfamA;
    
    
  }
  $st->finish();
  $select_handle->disconnect(); 
  $self->{'_connection_count'} = 0;
  
  
#  $dbh = $self->open_transaction( 'pfamA_web');
  
#  $dbh->do("delete from pfamA_web");
    
    my (@the_array);
  open(_FILE, "$file");

  while(<_FILE>) {

    my($pfamA_acc, $av_len, $per_id, $status) = split(/\t/, $_);
 #   print "$_ \n";
    $pfamA_acc =~ s/\s+//g;
    $av_len =~ s/\s+//g;
    $per_id =~ s/\s+//g;
    $status =~ s/\s+//g;
    push @the_array, $pfamA{$pfamA_acc} . "~" . $av_len . "~" .  $per_id . "~" .  $status;
    

  }

  close(_FILE);
     
#  print "ARRAY: @the_array <P>";
  $self->_add_row("pfamA_web", \@the_array, "","delete");
 


}



####################################################################
#
# MSD DATA UPLOAD
#
####################################################################




sub msd_data_upload {
  my($self, $rdb_ro, $file) = @_;
  my(%auto_pdb, %auto_pfamseq);

  my @temp;
  
  # So, this should really get all of the infromation from that database!
  # Therefore, passs in the appropriate read-only datbase handle
  my (%store_pfamseq);
  my @results = $rdb_ro->query("select pfamseq_acc, auto_pfamseq from pfamseq");
  foreach my $res (@results){
 	$store_pfamseq{$$res[0]}= $$res[1];
  }
  my @pdb_autos = $rdb_ro->query("select pdb_id, auto_pdb from pdb");
  foreach my $res (@pdb_autos){
	$auto_pdb{$$res[0]}= $$res[1];
  }	

  open(_FILE, ">/pfam/db/web_temp/msd_upload.dat");
  open(_MSD, "$file") or die "Canna open $file as $! \n";

  my $invalid_lines;
  my $line_count;
  while(<_MSD>) {
    $line_count++;
    my($auto_pdb,  $auto_pfamseq);
 
   
    my ($pdb_id, $msd_chain,$chain, $serial, $pdb_res,$pdb_seq_number, $pdb_insert,  $pfamseq_acc, $pfamseq_res, $pfamseq_seq_number );
    if ($_ =~ /^(\S{4})\s+(\S+)\s+(\S+)\s+(\d+)\s+(\w\w\w)\s+(\-*?\d*?\w*?)\s+(\S+)\s+(\w+)\s+(\d+)/) {
      $pdb_id = $1; 
	  $msd_chain = $2; 
	  $chain = $3;
	  $serial = $4;
	  $pdb_res = $5; 
	  $pdb_seq_number = $6; 
	  $pfamseq_acc = $7; 
	  $pfamseq_res = $8; 
	  $pfamseq_seq_number = $9; 
      if ($pdb_seq_number =~ /^(\d+)(\D+)$/) {
	$pdb_seq_number = $1; $pdb_insert = $2;
      }
    } elsif  ($_ =~ /^(\S+)\s+(\S+)\s+(\d+)\s+(\-*?\d*?\w*?)\s+(\w\w\w)/) {
      $line_count++;
      next;
    } else {
      $invalid_lines++;
      next;
    } 

   
  

	$chain = "" if ($chain =~ /\@/o);
	$msd_chain = "" if ($msd_chain =~ /\@/o);

	$pdb_id = lc($pdb_id);
    ###### AUTO_PDB

    if (defined($auto_pdb{$pdb_id})) {
      $auto_pdb = $auto_pdb{$pdb_id};
    } 


    if ($pfamseq_acc) {

      ####### AUTO_PFAMSEQ
      if (defined($store_pfamseq{$pfamseq_acc})) {
	$auto_pfamseq = $store_pfamseq{$pfamseq_acc};

      } else {
	$auto_pfamseq= $self->get_pfamseq_auto_number($pfamseq_acc, "acc");

	$store_pfamseq{$pfamseq_acc} = $auto_pfamseq;
      }  ## / end get_auto_pfamseq


    } # end need to get pfamseq


   if($msd_chain && $chain){ 
       print _FILE "$auto_pdb\t$msd_chain\t$chain\t$serial\t$pdb_res\t$pdb_seq_number\t$auto_pfamseq\t$pfamseq_res\t$pfamseq_seq_number\n"; 
   }elsif($msd_chain && !$chain ){
       print _FILE "$auto_pdb\t$msd_chain\t\t$serial\t$pdb_res\t$pdb_seq_number\t$auto_pfamseq\t$pfamseq_res\t$pfamseq_seq_number\n"; 
   }elsif(!$msd_chain && $chain ){
       print _FILE "$auto_pdb\t\t$chain\t$serial\t$pdb_res\t$pdb_seq_number\t$auto_pfamseq\t$pfamseq_res\t$pfamseq_seq_number\n"; 
   }elsif(!$msd_chain && !$chain ){
       print _FILE "$auto_pdb\t\t\t$serial\t$pdb_res\t$pdb_seq_number\t$auto_pfamseq\t$pfamseq_res\t$pfamseq_seq_number\n"; 
   }
	
  }

  close(_MSD);
  close(_FILE);
  ## DO THE LAST ARRAY!
  #print "NUMBER of invalid lines: $invalid_lines \n";
  $self->load_generic_file_to_rdb("/pfam/db/web_temp/msd_upload.dat" , "msd_data", 1);
} #/ end sub interaction


sub _add_row {

  my($self, $table, $tmp_data, $dbh, $delete) = @_;
  my @data = @{$tmp_data};
 
  #$self->{'_connection_count'} = 0;
  my $handle_int = 0;
  if(!$dbh){
	$dbh = $self->open_transaction( $table);
	$handle_int++;
	}
	
  if ($delete) {
    $dbh->do("delete from $table");
  }

  my $auto;
  eval {

    foreach (@data) {
      my(@temp) = split(/~~/, $_);

      my $values;

      foreach (@temp) {
		 $_ = $dbh->quote($_);
	if($values) {
	  $values = $values . " , $_ ";
	} else {
	  $values = $_ ;
	}

      }
      my $stat = $dbh->prepare("INSERT into $table values ($values) ");
      $stat->execute();
      $auto = $stat->{mysql_insertid};
    }

  };
  if ($@) {
    die "error adding row to $table as $@ \n";

  }

  if ($handle_int){
  	$self->close_transaction( );
	}
  return $auto;

}

sub store_all_pfamseq {
   my ($self,$value) = @_;
	
   if(! $value) {


     my $select_handle = $self->connect();

     my $st = $select_handle->prepare("select pfamseq.auto_pfamseq, pfamseq_acc from pfamseq");#
     $st->execute();
     my ($auto_pfamseq, $pfamseq_acc, %pfamseq_store);
     while ( ($auto_pfamseq, $pfamseq_acc) = $st->fetchrow) {
       $all_pfamseq_db_store{$pfamseq_acc} = $auto_pfamseq;
     }
     
     $st->finish();
     
     
     $select_handle->disconnect();
     $self->{'_connection_count'} = 0;
   } else {
      return 	$all_pfamseq_db_store{$value};
   }
}

sub store_pfamA_by_id {
   my ($self,$value) = @_;
	
   if(! $value) {


     my $select_handle = $self->connect();

     my $st = $select_handle->prepare("select auto_pfamA, pfamA_id from pfamA");#
     $st->execute();
     my ($auto_pfamA, $pfamA_id);
     while ( ($auto_pfamA, $pfamA_id) = $st->fetchrow) {
       $all_pfamA_db_store{$pfamA_id} = $auto_pfamA;
     }
     
     $st->finish();
     
     
     $select_handle->disconnect();
     $self->{'_connection_count'} = 0;
   } else {
      return 	$all_pfamA_db_store{$value};
   }
}

sub get_pfamseq_auto_number {

  my($self, $pfamseq_acc, $type) = @_;

  my($auto_pfamseq);

  my $select_handle = $self->connect();

  my $pfamseq_type = "pfamseq_" . $type ;

  my $st = $select_handle->prepare("select auto_pfamseq from pfamseq where $pfamseq_type = '$pfamseq_acc' ");

  $st->execute();
  $auto_pfamseq = $st->fetchrow;
  $st->finish();


 # $select_handle->disconnect();


  return $auto_pfamseq;

}

=head2 get_pfamA_auto_number

	Title   : get_pfamA_auto_number
	Usage   : $auto_pfamA = $rdb->get_pfamA_auto_number($domain);
	Function: Gets the auto incremented key for the domain.
	Returns : Unsigned integer
	Args	: a pfam domain name e.g. 7tm

=cut

sub get_pfamA_auto_number {
	my($self, $domain) = @_;
	my($auto_pfam);
 	my $select_handle = $self->connect();
	my $st = $select_handle->prepare("select distinct auto_pfamA from pfamA where pfamA_id = '$domain' or pfamA_acc = '$domain'");
	$st->execute();
	$auto_pfam = $st->fetchrow;
	$st->finish();
	#$select_handle->disconnect();
	return $auto_pfam;
}

=head2 check_in_clan

 Title    : check_in_clan
 Usage    : check_in_clan($rdb, $acc, $author, $ann, $desc)
 Function : Loads the clan annotation into the the RDB (effectively the check in).  
            Thus, if the clan already exists, then the row lock needs to be released.  
            The clans_lit_ref table uses the same literature table as pfam.
 Returns  : clans auto number
 Args     : database handle, clan accession, authors, annotation collection object, desc.

=cut

sub check_in_clan{
	my ($self, $acc, $author, $ann, $desc, $id) = @_;
	my ($st, $temp_auto, $tmp_rdb_created, $rdb_auto_clan, $rdb_created, $rdb_author, $rdb_acc, $rdb_desc,
		$rdb_comment, $rdb_id, $rows, $error);	
	## Step one -> see if the acc is new.
	my $dbh = $self->open_transaction( 'clans' );
	eval {
		$st = $dbh->prepare("select auto_clan, created from clans where clan_acc = '$acc'");
		$st->execute();
		my($temp_auto, $tmp_rdb_created) = $st->fetchrow;
		$st->finish();
		$rdb_auto_clan = $temp_auto if(defined($temp_auto));
	   	$rdb_created = $tmp_rdb_created if(defined($tmp_rdb_created));
			
		$rdb_author = $author;
		$rdb_acc = $acc;
		$rdb_desc = $desc;
		$rdb_id = $id;

		foreach my $com ($ann->get_Annotations('comment')){
			if ($rdb_comment){	
				$rdb_comment  .= " ".$com->text;
			}
			else{
					$rdb_comment  = $com->text;
			}
		}
	};
	if ($@) {
	   $error = "Could not fetch all the needed data from the clan entry [$@]";
       }
	
	#If not and I have got here, then the table must have a row lock
	#  Release the lock
		## Not done yet.....

	## Step two -> Update clans table
	eval {
		$st = $dbh->prepare($self->__replace_sql('clans', 8));
		$st->execute($rdb_auto_clan, $rdb_acc, $rdb_id, $rdb_desc, $rdb_author, $rdb_comment, undef, $rdb_created);
		$rows += $st->rows;

	   if(!$rdb_auto_clan) {
	     my $time_sql = "update clans set created = now() where clan_acc = '$rdb_acc'";
	     $st = $dbh->prepare($time_sql);
	     $st->execute;
	   }
	};
		
	if ($@) {
		$error = "Could not do the insertion/update on the clan table [$@]";
	}
	## close the transation	
	$self->close_transaction();
	## Step three -> Update clans_lit_ref table
	$dbh = $self->open_transaction( 'clan_database_links', 'clan_lit_refs', 'literature_references', 'clans' );
	my $auto_clan;
	eval{
		$st = $dbh->prepare("select auto_clan from clans where clan_acc = '$rdb_acc'");
        $st->execute();
     	my($temp_auto) = $st->fetchrow;
      	$st->finish();
      	$auto_clan = $temp_auto if(defined($temp_auto));
	};
	if ($@ or !$auto_clan) {
		$error = "Could not do get the auto clan number [$@]";
	}
	eval{
		$dbh->do("delete from clan_database_links where auto_clan='$auto_clan'"); 
	};
	if ($@) {
		$error = "Could not delete old database links [$@]";
	}
	eval{
		$dbh->do("delete from clan_lit_refs where auto_clan='$auto_clan'");
	};
	if ($@) {
		$error = "Could not delete old literature references [$@]";
	}

	if($ann->get_Annotations('dblink')){
		#my ($comment, $db_name, $db_ref, $add_params);
		foreach my $link ( $ann->get_Annotations('dblink') ){
		    my ($comment, $db_name, $db_ref, $add_params);
		    if (defined ($link->comment() ) ) {
	  			foreach my $linkcomm ( $link->comment() ) {
	    			chomp($linkcomm);
	    			$comment .=  $linkcomm ." ";
	  			}
			}

			$db_name = $link->database();
			$db_ref = $link->primary_id();
			$add_params = $link->optional_id();
			
		       
			eval{
			        
				$comment = $dbh->quote($comment);
				$st = $dbh->prepare($self->__replace_sql('clan_database_links', 5));
				$st->execute($auto_clan, $db_name, $comment, $db_ref, $add_params); 
			};
			if ($@){
				$error = "Error loading in the database references: [$@]";
				last;
			}
			
		}
	}
    ## If there is a literature reference
	if ($ann->get_Annotations('reference')) {
		
		my $count = 1;
		## For each reference
		foreach my $ref ( $ann->get_Annotations('reference') ) {
			my($comment, $medline, $authors, $journal, $title);
			## Get out comment if there is one
			if (defined ($ref->comment() ) ) {
			    foreach my $refcomm ( $ref->comment() ) {
				    chomp($refcomm);
				    $comment .=  $refcomm . " ";
	  			}
			}

			$medline = $ref->medline();
		

			### SEE IF MEDLINE ALREADY EXISTS in literature_reference table
			$st = $dbh->prepare("select auto_lit from literature_references where medline = '$medline'");
			$st->execute();
			my($auto_lit) = $st->fetchrow;
			$st->finish();

			if (!$auto_lit) {
	  			## need to add the lit reference
				$title = $ref->title();
				$authors = $ref->authors;
				$journal = $ref->location();
				$title = $dbh->quote($title);
				$authors = $dbh->quote($authors);
				$journal = $dbh->quote($journal);
				eval {
	    			$st = undef;
	    			if (not defined $st) {
	      				my $sql = "INSERT INTO literature_references VALUES ( NULL , $medline, $title, $authors, $journal)";
	      				$st = $dbh->prepare($sql);
	    			}
					$st->execute();
	    			$auto_lit = $st->{mysql_insertid}; ## get the auto number
	   	 			$rows += $st->rows;
				};
				if ($@) {
				  $error = "Could not do the insertion/update on the pfamA_lit_refs table [$@]";
				  last;
				}
			}
			
			#$comment = $dbh->quote($comment);
			## ok so added lit refs if we had to now add to clans_lit_ref table
	
			eval {
		    	$st = $dbh->prepare($self->__replace_sql('clan_lit_refs', 4));
		  		$st->execute( $auto_clan, $auto_lit, $count, $comment);
	
			};
		
			if ($@) {
			  $error = "Could not do the insertion/update on the clan_lit_refs table [$@]";
			}
	
			$count++;
			}
		}
	$self->close_transaction();
	$error and $self->throw( $error );

	return($auto_clan)
}

=head2 update_clan_membership

 Title    : update_clan_membership
 Usage    : update_clan_membershio($rdb, $auto_clan_number, @clan_membership)
 Function : Loads the members of the clan into the membership table 
 Returns  : Nothing
 Args     : database handle, clan auto number, array of Pfam auto numbers.

=cut

sub update_clan_membership {
	my  ($self, $clan_acc, @clan_membership) = @_;
	my $auto_clan_number =&_get_auto_clan_no($self, $clan_acc);  
	my $dbh = $self->open_transaction('clan_membership');
	
	eval{
		#delete all of the old membership
		$dbh->do("delete from clan_membership where auto_clan = '$auto_clan_number'");
		#replace for all of the new membership
		foreach my $auto_pfam (@clan_membership){ 
			my $st = $dbh->prepare("INSERT into clan_membership values ($auto_clan_number, $auto_pfam)");
			$st->execute();
		}
	};
	if($@){
		$self->throw("Could not do the update of the membership table [$@]\n");
	}
		
}

=head2 update_clan_lock_table

 Title	  : update_clan_lock_table
 Usage    : update_clan_lock_table($rdb, $user, $auto_clan_number, $lock)
 Function : Loads the clan into the lock table.  As this is done during a new_clan,
            the clan lock will be false. $lock 0 or 1.
 Returns  : Nothing
 Args     : database handle, user_id, clan auto number.

=cut

sub update_lock_on_clan {
	my  ($self, $user, $auto_clan_number, $lock, $type) = @_;
	if(!$lock){
	    $lock =0;
	    $type =0;
	}else{
	    $lock = 1;
	    $type = "FULL" if(!$type);
	}
	
	if ($auto_clan_number =~ /CL\d+/) {
		$auto_clan_number = _get_auto_clan_no($self, $auto_clan_number);
	}
	my $dbh = $self->open_transaction('clan_locks');
	my $st;
	eval{
		$st = $dbh->prepare($self->__replace_sql('clan_locks', 5));
		$st->execute($auto_clan_number, $lock, $user, $type, undef);
	};
	if ($@){
		die "Could not do the insertion/update on the clan_locks table [$@]";
	}
	$self->close_transaction;
}

=head2 release_lock_on_clan

 Title    : release_lock_on_clan
 Usage    : &ClanDB::release_lock_on_clan($rdb, $user, $clan_acc)
 Function : Releases a lock on the the clan in the clan lock table
 Returns  : nothing
 Args     : database handle, user, clan accession number

=cut

sub release_lock_on_clan{
	my ($self, $user, $clan_acc) = @_;
	#get clan auto number
	my $auto_clan_number = &get_auto_clan_no($self, $clan_acc);	
	#die if not defined
	if (!$auto_clan_number) {
		die "No auto number for the clan $clan_acc!! This is very bad\n";
	}
	&update_lock_on_clan($self, $user, $auto_clan_number, 0);
}

=head2 get_lock_on_clan

 Title    : get_lock_on_clan
 Usage    : &ClanDB::get_lock_on_clan($rdb, $user, $clan_acc)
 Function : Gets a lock on the the clan in the clan lock table
 Returns  : nothing
 Args     : database handle, user, clan accession number

=cut


sub get_lock_on_clan {
	my ($self, $user, $clan_acc) = @_;
	#get clan auto number
	my $auto_clan_number = &_get_auto_clan_no($self, $clan_acc);
	if (!$auto_clan_number) {
		die "No auto number for the clan $clan_acc!! This is very bad\n";
	}
	&update_lock_on_clan($self, $user, $auto_clan_number, 1);

}

=head2 _get_auto_clan_no

 Title    : _get_auto_clan_no
 Usage    : &ClanDB::_get_auto_clan_no($rdb, $clan_acc)
 Function : Gets the auto clan number from the RDB for the a given clan accession number
 Returns  : auto_clan_number
 Args     : database handle, clan accession number

=cut


sub _get_auto_clan_no {
	my ($self, $clan_acc) = @_;
	die "$clan_acc is an inappropriate format: Should be CLXXXX\n" if ($clan_acc !~ /CL\d{4}/);
	my $dbh = $self->open_transaction("clans");
	my $auto_clan;
	eval {
		my $st = $dbh->prepare("select auto_clan from clans where clan_acc ='$clan_acc'");
		$st->execute();
		$auto_clan = $st->fetchrow;
		$st->finish;
	};
	if ($@){
		die "Could not get clan auto number form clans table with $clan_acc\n";
	}
	$self->close_transaction();
	return ($auto_clan);
}

=head2 update_dead_clan

 Title    : update_dead_clan
 Usage    : &ClanDB::update_dead_clan($rdb, $clan_acc, $clan_desc, 
            $membership_string, $message, $forward, $user)
 Function : This is takes most of the information about the clan and enters it
            into the dead clan table, prior to the removal of the clan 
            information in all of the alive clan tables.  
 Returns  : nothing
 Args     : database handle, clan accession, clan description, a sting 
            containing the membership of the clan, the reason for the clan 
            being killed, any forwarding clan accession (e.g. clan merger) and
            the user whopwd
is killing the clan. 

=cut

sub update_dead_clan {
	my ($self, $clan_acc, $clan_desc, $membership_string, $message, $forward, $user) = @_;
	my $error;
	my $dbh->open_transaction('dead_clans');
	$clan_desc = $dbh->quote($clan_desc);
	$membership_string = $dbh->quote($membership_string);
	$message = $dbh->quote($message);
	$self->report_mode();
	eval{
		my $st = $dbh->prepare($self->replace_sql('dead_clans', 7));
		$st->execute( $clan_acc, $clan_desc,  $membership_string, $message, $forward, $user, undef);
		## Need to add the time stamp
		my $time_sql = "update dead_clans set killed = now() where clan_acc = '$clan_acc'";
	    $st = $dbh->prepare($time_sql);
	    $st->execute;

	};
	
	if ($@) {
      $error = "Could not do the insertion/update on the dead_clans table [$@]";
    }
	$self->close_transaction( $error );
	$error and $self->throw( $error );
	if(!$error) {
   		$self->_purge_clan($self, $clan_acc);
 	}

}

=head2 _purge_clan

 Title    : load_clandesc_file_into_RDB
 Usage    : load_clandesc_file_into_RDB($rdb, $clan_acc, \*CLANDESC)
 Function : Loads the information in the CLANDESC file into the RDB
 Returns  : nothing
 Args     : database handle, clan accession number

=cut

sub _purge_clan {
	my ($self, $clan_acc) = @_;
	my $clan_auto = _get_auto_clan_no($self, $clan_acc);
	my $dbh = $self->open_transaction('clans', 'clan_membership', 'clan_locks', 'clan_lit_refs');
	my $error;
	eval{
		$dbh->do("delete from clans where auto_clan = '$clan_auto'");
		$dbh->do("delete from clan_membership where auto_clan = '$clan_auto'");
		$dbh->do("delete from clan_locks where auto_clan = '$clan_auto'");
		$dbh->do("delete from clan_lit_refs where auto_clan = '$clan_auto'");
	};
	if($@){
		$error = "Could not delete clan details [$@]";
	}
	$self->close_transaction( $error );
}

=head2 zero_current

 Title    : zero_current
 Usage    : zero_current($rdb, $clan_acc)
 Function : Sets the status to zero for all versions
 Returns  : nothing
 Args     : database handle, clan accession number

=cut


sub zero_current{
	my ($self, $clan_acc) = @_;
	my $clan_auto = _get_auto_clan_no($self, $clan_acc);
	my $dbh = $self->open_transaction('clan_revisions');
	eval {
		my $sql = "update clan_versions set current = 0 where auto_clan = '$clan_auto'";
		my $st = $dbh->prepare($sql);
		$st->execute;
	};
	if ($@){
		die "Error zeroing current status:[$@]\n";
	}
	$self->close_transaction();
}

=head2 update_version_table

 Title    : update_version_table
 Usage    : &ClanDB::update_version_table($rdb, $clan_acc, $current, $version, $membership)
 Function : Updates the version table for clans
 Returns  : nothing
 Args     : database handle, clan accession, current status, version number, membership 
			(Strings of PFAM ids, tagged with RCS version).

=cut

sub update_version_table {
	my ($self, $clan_acc, $current, $version, $membership, $message, $name) = @_;
	my $clan_auto = _get_auto_clan_no($self, $clan_acc);
	my $dbh = $self->open_transaction('clan_versions');
	eval {
		$membership = $dbh->quote($membership);
		$message = $dbh->quote($message);
		$name = $dbh->quote($name);
		my $sql = "INSERT into clan_versions VALUES( $clan_auto, $current, $version, $membership, $message, $name, NULL)";
		my $st = $dbh->prepare($sql);
		$st->execute;
		my $time_sql = "update clan_versions set updated = now() where auto_clan = '$clan_auto' and current=1";
		$st = $dbh->prepare($time_sql);
		$st->execute;

	};
	if ($@){
		die "Error updating  clan version:[$@]\n";
	}
	$self->close_transaction;
}

sub VERSION_table_update {
  my($self, $pfam_release, $swiss_prot_version, $trembl_version , $hmmer_versiion) = @_;
  my $dbh = $self->open_transaction('VERSION');
  eval {
    
    $dbh->do("delete from VERSION");
    my $sql = "INSERT into VERSION VALUES('$pfam_release', '$swiss_prot_version', '$trembl_version' , '$hmmer_versiion' )";
    my $st = $dbh->prepare($sql);
    $st->execute;
  };
  
  if ($@) {
    print "Updating VERSION table failed as $@ \n";
  }
}

sub query{
    my ($self, $query) = @_;
    my ($dbh, $st, @retlist);

#my $select_handle = $self->connect();
  
 

  

 if ( ($query !~ /^select/i) && ($query !~ /^show/i) &&  ($query !~ /^describe/i) ) {

        $self->throw("DB_RDB::query - only 'select' queries are allowed");
        }

    eval {
        $dbh = $self->connect();
        $st = $dbh->prepare($query);
        $st->execute();
        while( my @list = $st->fetchrow() ) {
            # print STDERR "$query: $list[0]\n";
            push @retlist, \@list;
        }
        $st->finish;
      #  $self->_the_RDB->disconnect;
        
    };   
    $@ and $self->throw("DB_RDB::query - error with query '$query' [$@]"); 
    #$dbh->disconnect(); 
    $self->{'_connection_count'} = 0;
    return @retlist;
}


sub upload_pfamB_stockholm {
  my ($self, %pfamB) = @_;

  my ($dbh);


  $dbh = $self->open_transaction( 'pfamB_stockholm'); # 'pfamA'
  $dbh->do("delete from pfamB_stockholm");
  foreach (sort keys %pfamB) {
     
      my $st = $dbh->prepare("insert into pfamB_stockholm values('$_',  '" . $pfamB{$_}. "' )");
      eval {
	  $st->execute();
	  $st->finish();
      };
      if ($@) {
	  print "BOMBED AS $@ \n";
      }
  }
}


1;
