#
# BioPerl module for Bio::Pfam::DB_RDB
#
# Written by Kevin Howe
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Rfam::DB_RDB - An interface to a pfam database implemented as a relational
database.

=head1 SYNOPSIS

This is a concrete implementation of the abstract database object
found in Bio::Pfam::DB

=head1 DESCRIPTION

An interface to a pfam database implemented as a relational database.
This is a project in develpment. 

At time of writing, the Pfam relational database onyl contains skeleton
information concerning Pfam entries. Therefore, the key methods 
get_EntryA_by_acc, get_EntryA_by_id, (and corresponding EntryB methods)
are not supported. Therefore, the main use of this database module is for
queries about the Pfam structures of sequences in pfamseq


=head1 CONTACT

pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. 
This module has a slightly unusual method-naming convention:

    Methods whivh change the underlying Pfam database in any
       way are preceded with a '_'.
    Internal methods, which are usually preceded with a '_',
        are preceded with a '__.'
    

=cut


# Let the code begin...


package Rfam::DB::DB_RDB;

use vars qw(@ISA);
use strict;
use DBI;
use FileHandle;

use Rfam::DB::RfamRDB;
use Rfam::AnnotatedSequence;

@ISA = qw(Rfam::DB::DB);

# _initialize is where the heavy stuff will happen when new is called

sub new {
   my $caller = shift;
   my $self = bless {}, ref($caller) || $caller;

 #  print "BOO <P>";
   my %params = @_;
   my $make = $self->SUPER::new(%params);

   $make->_the_RDB( Rfam::DB::RfamRDB->new( %params ));
return $make; # success - we hope!
}

#sub _initialize {
#  my($self, %params) = @_;

#  my $make = $self->SUPER::_initialize(%params);
 
#  # ok - ready to rock

#  $self->_the_RDB( Rfam::DB::RfamRDB->new( %params ));

#  # set stuff in self from @args
#  return $make; # success - we hope!
#}









=head2 query

 Title   : query
 Usage   :
    $rdb->query("select * from * where * = *");
 Function:
    This function allows the caller to query the rdb directly
    with sql. It returns the query results as a
    a list of array references. This mechanism is used for
    generality. Many queries return just a single value. In
    this case, the method returns a single list reference,
    referring to a singleton list.
 Returns : 
    An array of list references
 Args    : A query string
 Notes   :
    This method is not intended to change the database
    in any way. Therefore, only select statementsd are 
    allowed.

=cut

sub query{
    my ($self, $query) = @_;
    my ($dbh, $st, @retlist);

 if ( ($query !~ /^select/i) && ($query !~ /^show/i) &&  ($query !~ /^describe/i) ) {

	print "DB_RDB::query - only 'select' queries are allowed";
	exit();
	}

    eval {
	$dbh = $self->_the_RDB->connect();
	$st = $dbh->prepare($query);
	$st->execute();
	while( my @list = $st->fetchrow() ) {
	    # print STDERR "$query: $list[0]\n";
	    push @retlist, \@list;
	}
	$st->finish;
	
	$self->_the_RDB->disconnect;
    };   
    $@ and print "DB_RDB::query - error with query '$query' [$@]\n"; 

    return @retlist;
}




sub get_AnnotSeqs {
  my ($self, $id_list, $type_list) = @_;
  
  my ($dbh, $st_pfamA_reg_seed, $st_pfamA_reg_full,  $st_pfamB_reg, $st_other_reg,$st_hmm_other_region,$st_context_region, $st_length, $length,       $seq_id, $st, $en, $score, $mo_st, $mo_en, $bits, $ev, $acc, $id, $desc, $orien,       $annot, $sou, $all_regs, $mod_len, $tree_order,      @annseqlist,$domain_score,	  %temp);
  
  #  my $fac = Rfam::AnnotatedSequence->new();
  eval {
    $dbh = $self->_the_RDB->connect();
  };
  
  if (not $type_list) {
    $all_regs = "true";	 
  }   else {
    foreach my $info_type (@{$type_list}) {
      if ($info_type =~ /seed/i) {
	$temp{ "seed" } = 1;
      }       elsif ($info_type =~ /full/i) {
	$temp{ "full" } = 1;
      }
    }
    
  }
  
  if ($temp{"full"} or $all_regs) {
    
    #	   my $stat = "select seq_start, seq_end, model_start, model_end, ";
    #	   $stat .= "bits_score, evalue_score, pfamA_reg_full.pfamA_acc, pfamA_id, description ";
    #	   $stat .= "from pfamA_reg_full natural left join pfamA  where pfamseq_id = ?";
    
#    my $stat = "select seq_start, seq_end, model_start, model_end, ";
#    $stat .= "domain_bits_score, domain_evalue_score, pfamA.pfamA_acc, pfamA_id, pfamA.description , pfamA_reg_full.tree_order ";
#    $stat .= "from pfamseq, pfamA, pfamA_reg_full ";
#    $stat .= "where pfamseq.pfamseq_id =  ? and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq and pfamA_reg_full.auto_pfamA = pfamA.auto_pfamA  and in_full = '1'";

    my $stat = "select rfamseq_acc, rfam_acc, rfam_id, rfam.auto_rfam, seq_start, seq_end, rfamseq.description  bits_score from rfam_reg_full , rfamseq, rfam";
    $stat .= " where rfam_acc = ? and rfam.auto_rfam = rfam_reg_full.auto_rfam ";
    $stat .= "and rfam_reg_full.auto_rfamseq = rfamseq.auto_rfamseq order by rfamseq_id";
    
    ##and significant = '1'  #### BUG FIX !! 
    
    
    $st_pfamA_reg_full = $dbh->prepare($stat);
    
  }
  
  if ($temp{"seed"}) {
    #	   my $stat = "select seq_start, seq_end, ";
    #	   $stat .= "pfamA_reg_seed.pfamA_acc, pfamA_id, description ";
    #	   $stat .= "from pfamA_reg_seed natural left join pfamA where pfamseq_id = ?";
    
#    my $stat = "select seq_start, seq_end, ";
#    $stat .= "pfamA.pfamA_acc, pfamA_id, pfamA.description ";
#    $stat .= "from pfamA_reg_seed, pfamA, pfamseq  where pfamseq.pfamseq_id  = ? and pfamseq.auto_pfamseq = pfamA_reg_seed.auto_pfamseq and pfamA_reg_seed.auto_pfamA = pfamA.auto_pfamA";
    
    my $stat = "select rfamseq_acc, rfam_acc, rfam_id, rfam.auto_rfam, seq_start, seq_end, rfamseq.description  from rfam_reg_seed , rfamseq, rfam";
    $stat .= " where rfamseq_acc = ? and rfam.auto_rfam = rfam_reg_seed.auto_rfam ";
    $stat .= "and rfam_reg_seed.auto_rfamseq = rfamseq.auto_rfamseq order by rfamseq_id";
#    print "STAT: $stat \n";
    $st_pfamA_reg_seed = $dbh->prepare($stat);
  }
  
  
  foreach my $in_id (@{$id_list}) {
    my $fac = Rfam::AnnotatedSequence->new();
    #my $annSeq = $fac->createAnnotatedSequence();
    $fac->id( $in_id );
#    print "ID: $in_id \n";
    if (defined $st_pfamA_reg_full) {
      $st_pfamA_reg_full->execute($in_id);
#      print "FULL: $st_pfamA_reg_full \n";
      while ( my($rfamseq_id, $rfam_acc, $rfam_id,$auto_rfam, $seq_start, $seq_end,$desc, $bits_score)
	      = $st_pfamA_reg_full->fetchrow) {
	
	
	$fac->addAnnotatedRegion( Rfam::RfamRegion->new('-RFAM_ACCESSION' => $rfam_acc,
							'-RFAM_ID' => $rfam_id,
							'-SEQ_ID' => $rfamseq_id,
							'-MODEL_FROM' => $seq_start,
							'-MODEL_TO' => $seq_end,
							'-AUTO_RFAM' => $auto_rfam,
							'-BITS' => $bits_score,
							
							'-ANNOTATION' => $desc
						       ));
      }
      
      
      $st_pfamA_reg_full->finish;
    }
    
    if (defined $st_pfamA_reg_seed) {
      eval {
	$st_pfamA_reg_seed->execute($in_id);
   #   print "SEED: $st_pfamA_reg_seed \n";
      while ( my($rfamseq_id, $rfam_acc, $rfam_id,$auto_rfam, $seq_start, $seq_end,$desc)
	      = $st_pfamA_reg_seed->fetchrow) {
#	print "$rfamseq_id, $rfam_acc, $rfam_id,$auto_rfam, $seq_start, $seq_end,$desc\n";
	$fac->addAnnotatedRegion( Rfam::RfamRegion->new('-RFAM_ACCESSION' => $rfam_acc,
							'-RFAM_ID' => $rfam_id,
							'-SEQ_ID' => $rfamseq_id,
							'-MODEL_FROM' => $seq_start,
							'-MODEL_TO' => $seq_end,
							'-AUTO_RFAM' => $auto_rfam,
							
							'-ANNOTATION' => $desc
						       ));



      }
      $st_pfamA_reg_seed->finish;
    };
      if ($@) {
#	print "BOMB \n";
      }
    }
 #   print "FAC: $fac \n";
    push @annseqlist, $fac;
  }
  
  $self->_the_RDB->disconnect;
#}

  return @annseqlist;
#}#
  
}





=head2 _the_RDB

Title   : _the_RDB
  Usage   : $rdb = $self->_the_RDB();
 Function:
    Gets/sets the underlying database object for this query layer
 Returns : A object that provides basic connection facilities to the RDB
 Args    : A object that provides basic connection facilities to the RDB (optional)

=cut

sub _the_RDB {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_the_rdb'} = $value;
   }
   return $self->{'_the_rdb'};
}



1;
