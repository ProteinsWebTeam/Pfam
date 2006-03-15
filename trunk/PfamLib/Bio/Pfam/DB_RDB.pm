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

Bio::Pfam::DB_RDB - An interface to a pfam database implemented as a relational
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


package Bio::Pfam::DB_RDB;

use vars qw($AUTOLOAD @ISA);
use strict;
use DBI;
use FileHandle;

use Bio::Pfam::SeqPfam;
use Bio::Pfam::DB;
use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRDB;
use Bio::Pfam::EntryA_RDB;


@ISA = qw(Bio::Pfam::DB);

sub new {
    my($class, %params) = @_;
    my $self = $class->SUPER::new(%params);
    $self->_the_RDB( Bio::Pfam::PfamRDB->new( %params ));
    return $self;
}



=head2 id2acc

 Title   : id2acc
 Usage   : $acc = $db->id2acc("pkinase"); 
 Function: converts an id to an accession. Only works for
	   current ids - does not work for previous ids
 Returns : string of the accession, or undef if no accession
 Throws  : Exceptions thrown if this $id does not exist or due to RCS misconfiguration
 Args    :

=cut

sub id2acc{
   my ($self,$id) = @_;
   my ($dbh, $st, $acc, $len);
   eval {
       $dbh = $self->_the_RDB->connect();
       $st = $dbh->prepare("select pfamA_acc, model_length from pfamA where pfamA_id = '$id'");
       $st->execute();
       ($acc, $len) = $st->fetchrow;
       $st->finish();
       $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::id2acc - error with the database connection [$@]");
    
   if (not defined $acc) {
       $self->throw("DB_RDB::id2acc - $id is not a valid id for this pfam database");
   }
   elsif (not defined $len) {
       $self->throw("DB_RDB::id2acc - $id is the id of a dead family");
   }
   
   return $acc;
}



=head2 acc2id

 Title   : acc2id
 Usage   : $id = $db->acc2id("PF00001");
 Function: converts an accession to an id
 Returns : the id, throws an exception if no accession, or dead accession
 Args    :

=cut

sub acc2id{
   my ($self,$acc) = @_;
   my ($dbh, $st, $id, $len);
   
   eval {
       $dbh = $self->_the_RDB->connect();
       $st = $dbh->prepare("select pfamA_id, model_length from pfamA where pfamA_acc = '$acc'");
       $st->execute;
       ($id, $len) = $st->fetchrow;
       $st->finish;
       $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::acc2id - error with the database connection [$@]");

   if (not defined $id) {
       $self->throw("DB_RDB::acc2id - $acc is not a valid id for this pfam database");
   }
   elsif (not defined $len) {
       $self->throw("DB_RDB::acc2id - $acc is the id of a dead family");
   }

   return $id;
}

=head2 get_family_pfamseqidacc

 Title   : get_family_pfamseqidacc
 Usage   : $db -> get_family_pfamseqidacc("PF00001", 'FULL');
 Function: get a list of pfamseqs in a family
 Returns : a hash reference of id -> acc mappings
 Args    : family acc, FULL or SEED

=cut

sub get_family_pfamseqidacc {
    my( $self, $famacc, $which ) = @_;
    my( $dbh, $st );
    my %mapping;
    eval {
	$dbh = $self->_the_RDB->connect();
	if( $which eq "FULL" ) {
	    $st = $dbh->prepare("select distinct pfamseq.pfamseq_acc, pfamseq.pfamseq_id from pfamA_reg_full, pfamseq, pfamA where pfamA_acc = '$famacc' and pfamA.auto_pfamA = pfamA_reg_full.auto_pfamA and pfamA_reg_full.auto_pfamseq = pfamseq.auto_pfamseq and in_full = '1';");
	}
	elsif( $which eq "SEED" ) {
	    $st = $dbh->prepare("select distinct pfamseq.pfamseq_acc, pfamseq.pfamseq_id from pfamA_reg_seed, pfamseq, pfamA where pfamA_acc = '$famacc' and pfamA.auto_pfamA = pfamA_reg_seed.auto_pfamA and pfamA_reg_seed.auto_pfamseq = pfamseq.auto_pfamseq;");
	}
	else {
	    $self->throw("DB_RDB::get_pfamseqidacc_in_family - you must specify FULL or SEED");
	}   
	$st->execute;
    };
    $@ and $self->throw("DB_RDB::get_pfamseqidacc_in_family - error with the database connection [$@]");

    while( my( $acc, $id ) = $st->fetchrow ) {
	$mapping{ $id } = $acc;
    }
    $st->finish;
    $self->_the_RDB->disconnect;
    
    if ( not %mapping ) {
	$self->throw("DB_RDB::get_pfamseqidacc_in_family - could not get acc to id mapping");
    }
    return \%mapping;
}


=head2 pfamseqacc2id

 Title   : pfamseqacc2id
 Usage   : $id = $db->pfamseqacc2id("Q02453");
 Function: converts a pfamseq accession to an id
 Returns : the pfamseq id, throws an exception if no accession
 Args    :

=cut

sub pfamseqacc2id{
    my $self   = shift;
    my $accref = shift;
    my @accs;
    my $useref;
    if( ref( $accref ) eq "ARRAY" ) {
	$useref = 1;
	@accs = @{$accref};
    }
    else {
	push( @accs, $accref );
    }

    my %ids;
   eval {
       my $dbh = $self->_the_RDB->connect();
       foreach my $acc ( @accs ) {
	   my $st = $dbh->prepare("select pfamseq_id from pfamseq where pfamseq_acc = '$acc'");
	   $st->execute;
	   my ($id) = $st->fetchrow;
	   if (not defined $id) {
	       $self->throw("DB_RDB::pfamseqacc2id - $acc is not a valid acc for this pfam database");
	   }
	   $ids{ $acc } = $id; 
	   $st->finish;
       }
       $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::pfamseqacc2id - error with the database connection [$@]");

    if( $useref ) {
	return \%ids;
    }
    else {
	my( $id ) = values( %ids );
	return $id;
    }
}


=head2 pfamseqid2acc

 Title   : pfamseqid2acc
 Usage   : $acc = $db->pfamseqid2acc("VAV_HUMAN");
 Function: converts a pfamseq id to an acc
 Returns : the pfamseq acc, throws an exception if no id
 Args    :

=cut

sub pfamseqid2acc{
    my $self  = shift;
    my $idref = shift;
    my @ids;
    my $useref;
    if( ref( $idref ) eq "ARRAY" ) {
	@ids = @{$idref};
	$useref = 1;
    }
    else {
	push( @ids, $idref );
    }

   my %accs;
   
   eval {
       my $dbh = $self->_the_RDB->connect();
       foreach my $id ( @ids ) {
	   my $st = $dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_id = '$id'");
	   $st->execute;
	   my ($acc) = $st->fetchrow;
	   if (not defined $acc) {
	       $self->throw("DB_RDB::pfamseqid2acc - $id is not a valid id for this pfam database");
	   }
	   $accs{ $id } = $acc; 
	   $st->finish;
       }
       $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::pfamseqid2acc - error with the database connection [$@]");

    if( $useref ) {
	return \%accs;
    }
    else {
	my( $acc ) = values( %accs );
	return $acc;
    }
}


=head2 pfamseqcrc2acc

 Title   : pfamseqcrc2acc
 Usage   : @accs = $db->pfamseqcrc2acc("VAV_HUMAN");
 Function: gets a list of pfamseq accs with given crc64 checksum
 Returns : a list of accs
 Args    : checksum string

=cut

sub pfamseqcrc2acc{
   my ($self,$crc) = @_;
   my @accs;

   eval {
       my $dbh = $self->_the_RDB->connect();
       my $st = $dbh->prepare("select pfamseq_acc from pfamseq where crc64 = '$crc'");
       $st->execute;
       while( my ($acc) = $st->fetchrow ) {
	   push( @accs, $acc );
       }
       $st->finish;
       $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::pfamseqcrc2acc - error with the database connection [$@]");

   if (not @accs) {
       $self->throw("DB_RDB::pfamseqcrc2acc - $crc is not a valid id for this pfam database");
   }

   return @accs;
}




=head2 is_dead_acc

 Title   : is_dead_acc
 Usage   : if( $db->is_dead_acc('PF00034') ) { print "Is a dead accession" }
 Example :
 Returns : 1 if dead, undef if not
 Args    : accesion number


=cut

sub is_dead_acc{
  my ($self,$acc) = @_;
  my ($dbh, $st, $len);
  
  eval {
       $dbh = $self->_the_RDB->connect();
       $st = $dbh->prepare("select model_length from pfamA where pfamA_acc = '$acc'");
       $st->execute;
       ($len) = $st->fetchrow;
       $st->finish;
       $self->_the_RDB->disconnect;
  };
  $@ and $self->throw("DB_RDB::is_dead_acc - error with the database connection [$@]");
  
  return (defined $len)?1:undef;

}


=head2 get_AnnotSeqs

 Title   : get_AnnotSeqs
 Usage   : ($annot) = $db->get_annotSeqs( $id );
 Function: gets an Annotated sequence from underlying swisspfam db
 Returns : Bio::Pfam::AnnotatedSequence 
 Args    : A ref to a list of swisspfam (pfamseq) identifier
 Notes   : 
    1. The AnnotatedSequence returned has no enclosed sequence,
    just annotation. If the sequence is required, use 
    get_Seq_pfamseq to get a Bio::Pfam::SeqPfam and insert it into
    the AnnotatedSequence method using 
    AnnotatedSequence->sequence().

    2. Region-type items can be:
        'seed'
        'full'
        'pfamb'
        'other'
    When the list is empty, all regions are returned. If not, then 
    if the specified region types are availalble they will be 
    returned, but the types specified will not be returned

=cut


sub get_AnnotSeqs {
   my ($self, $id_list, $type_list) = @_; # $id_list should contain accs now
   my ($dbh, $st_pfamA_reg_seed, $st_pfamA_reg_full,  $st_pfamB_reg, $st_other_reg, 
       $seq_id, $st, $en, $score, $mo_st, $mo_en, $bits, $ev, $acc, $id, $desc, $orien,$mode, $sig,
       $annot, $sou, $all_regs,
       @annseqlist,
       %temp);

   my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();

   eval {
       $dbh = $self->_the_RDB->connect();


       if (not $type_list) {
	   $all_regs = "true";
       }
       else {
	   foreach my $info_type (@{$type_list}) {
	       if ($info_type =~ /seed/i) {
		   $temp{ "seed" } = 1;
	       }
	       elsif ($info_type =~ /full/i) {
		   $temp{ "full" } = 1;
	       }
	       elsif ($info_type =~ /pfamb/i) {
		   $temp{ "pfamb" } = 1;
	       }
	       elsif ($info_type =~ /other/i) {
		  $temp{ "other" } = 1;
		
	       }
	       else {
		   $self->throw("Bio::Pfam::DB_RDB->get_AnnotSeqs does not supprto region type $info_type");
	       }
	   }
       }

       
      if ($temp{"full"} or $all_regs) {

#	   my $stat = "select seq_start, seq_end, model_start, model_end, ";
#	   $stat .= "bits_score, evalue_score, pfamA_reg_full.pfamA_acc, pfamA_id, description ";
#	   $stat .= "from pfamA_reg_full natural left join pfamA where pfamseq_id = ?";


	  my $stat = "select seq_start, seq_end, model_start, model_end,";
	  $stat .= "domain_bits_score, domain_evalue_score, pfamA.pfamA_acc, pfamA_id, pfamA.description , mode, significant ";
	  $stat .= "from pfamseq, pfamA, pfamA_reg_full ";
	  $stat .= "where pfamseq.pfamseq_acc = ? and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq and pfamA_reg_full.auto_pfamA = pfamA.auto_pfamA and in_full = '1' ";

	  
	   $st_pfamA_reg_full = $dbh->prepare($stat);
       }
       if ($temp{"seed"} ) {
	   my $stat = "select seq_start, seq_end, ";
	   $stat .= "pfamA.pfamA_acc, pfamA_id, pfamA.description ";
	   $stat .= "from pfamA_reg_seed, pfamA, pfamseq  where pfamseq.pfamseq_acc = ? and pfamseq.auto_pfamseq = pfamA_reg_seed.auto_pfamseq and pfamA_reg_seed.auto_pfamA = pfamA.auto_pfamA";
	   
	  
	   $st_pfamA_reg_seed = $dbh->prepare($stat);
       }
       if ($temp{"pfamb"} or $all_regs) {

#	   my $stat = "select seq_start, seq_end, pfamB_reg.pfamB_acc, pfamB_id ";
#	   $stat .= "from pfamB_reg natural left join pfamB where pfamseq_id = ?";
	   
	 my $stat = "select seq_start, seq_end, pfamB.pfamB_acc, pfamB_id ";
	  $stat .= "from pfamB_reg, pfamB, pfamseq ";
	  $stat .= "where pfamseq_acc = ? and pfamB_reg.auto_pfamseq = pfamseq.auto_pfamseq and pfamB_reg.auto_pfamB = pfamB.auto_pfamB;";

	   $st_pfamB_reg = $dbh->prepare($stat);
       }
       if ($temp{"other"} or $all_regs) {
 
#	   my $stat = "select seq_start, seq_end, type_id, source_id, score, orientation ";
#	   $stat .= "from other_reg ";
#	   $stat .= "where pfamseq_id = ?";
	 
	   my $stat = "select seq_start, seq_end, type_id, source_id, score, orientation ";
	   $stat .= "from other_reg, pfamseq ";
	   $stat .= "where pfamseq.pfamseq_acc = ? and other_reg.auto_pfamseq = pfamseq.auto_pfamseq";

	   $st_other_reg = $dbh->prepare($stat);
 
       }
       
       foreach my $in_id (@{$id_list}) {
	
	   my $annSeq = $fac->createAnnotatedSequence();
	   $annSeq->id( $in_id );
	   if (defined $st_pfamA_reg_full) {
	       $st_pfamA_reg_full->execute($in_id);
	       while ( ($st, $en, $mo_st, $mo_en, $bits, $ev, $acc, $id, $desc, $mode, $sig)
		       = $st_pfamA_reg_full->fetchrow) {
		   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
									   '-PFAM_ID' => $id,
									   '-SEQ_ID' => $in_id,
									   '-FROM' => $st,
									   '-TO' => $en,
									   '-MODEL_FROM' => $mo_st,
									   '-MODEL_TO' => $mo_en,
									   '-BITS' => $bits,
									   '-EVALUE' => $ev,
									   '-ANNOTATION' => $desc,
									  '-MODE' => $mode,
									  '-IS_SIGNIFICANT' => $sig
									  ));
	       }
	       $st_pfamA_reg_full->finish;
	   }
	   if (defined $st_pfamA_reg_seed) {
	     
	       $st_pfamA_reg_seed->execute($in_id);
	       while ( ($st, $en, $acc, $id, $desc)
		       = $st_pfamA_reg_seed->fetchrow) {
		
		   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
									   '-PFAM_ID' => $id,
									   '-SEQ_ID' => $in_id,
									   '-FROM' => $st,
									   '-TO' => $en,
									   '-ANNOTATION' => $desc));
	       }
	       $st_pfamA_reg_seed->finish;
	   }
	   if (defined $st_pfamB_reg) {
	       $st_pfamB_reg->execute($in_id);
	       
	       while ( ($st, $en, $acc, $id) = $st_pfamB_reg->fetchrow) {
		   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
									   '-PFAM_ID' => $id,
									   '-SEQ_ID' => $in_id,
									   '-FROM' => $st,
									   '-TO' => $en));
	       }
	       $st_pfamB_reg->finish;
	   }
	   if (defined $st_other_reg) {
	       $st_other_reg->execute($in_id);
	       while ( ($st, $en, $id, $sou,$score, $orien ) = $st_other_reg->fetchrow) {
		   $annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
									    '-FROM' => $st,
									    '-TO' => $en,
									    '-TYPE' => $id,
									    '-SOURCE' => $sou,
									    '-SCORE' => $score,
									    '-ORIENTATION' => $orien
									    )); 

	       }
	       $st_other_reg->finish;
	   }
	   
	   push @annseqlist, $annSeq;
       }

    #   $self->_the_RDB->disconnect if(defined($dbh));
   };
   $@ and $self->throw("DB_RDB::get_AnnotSeqs - error with the database connection [$@]");
 
   return @annseqlist;
}



=head2 get_EntryA_by_id

 Title   : get_EntryA_by_id
 Usage   : $en = $db->get_EntryA_by_id('pkinase');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family
 Notes   : RDB only has limited information regarding
    Pfam entries. However, we can give them what they do know

=cut

sub get_EntryA_by_id{
   my ($self, $id) = @_;
   my ($myacc, $myid, $mydesc, $mylen, $auto_pfamA);
   my ($dbh, $st);
  
   eval {
       $dbh = $self->_the_RDB->connect();
       $st = $dbh->prepare("select pfamA_acc, pfamA_id, description, model_length, auto_pfamA from pfamA where pfamA_id = '$id'");
       $st->execute();
       ($myacc, $myid, $mydesc, $mylen, $auto_pfamA) = $st->fetchrow;
       $st->finish();
       $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::get_EntryA_by_id - error with the database connection [$@]");
   
   if (not $myacc or not $myid) {
       $self->throw("DB_RDB::get_EntryA_by_id - $id is not a valid accession");
   }
   elsif (not $mydesc and $mylen) {
       $self->is_dead(1);
   }

   
   my $en = Bio::Pfam::EntryA_RDB->new('-rdb' => $self->_the_RDB());
   $en->acc( $myacc );
   $en->id( $myid );
   $en->auto_pfamA( $auto_pfamA );
   $en->description($mydesc);
   $en->model_length( $mylen );

   return $en;
}



=head2 get_EntryB_by_id

 Title   : get_EntryB_by_id
 Usage   : $en = $db->get_EntryB_by_id('Pfam-B_101');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family


=cut

sub get_EntryB_by_id{
    my ($self, @args) = @_;

   $self->throw("Bio::Pfam::DB_RDB does not currently support get_EntryB_by_id");
}




=head2 get_EntryA_by_acc

 Title   : get_EntryA_by_acc
 Usage   : $en = $db->get_EntryA_by_acc('PF00076');
 Function: get a Bio::Pfam::EntryA object by acc
 Returns : Bio::Pfam::EntryA object
 Args    : string of the accession number of the family


=cut

sub get_EntryA_by_acc{
    my ($self, $acc) = @_;
    my ($myacc, $myid, $mydesc, $mylen, $auto_pfamA, $dbh, $st);
    
    eval {
	$dbh = $self->_the_RDB->connect();
	$st = $dbh->prepare("select pfamA_acc, pfamA_id, description, model_length, auto_pfamA from pfamA where pfamA_acc = '$acc'");
	$st->execute();
	($myacc, $myid, $mydesc, $mylen, $auto_pfamA) = $st->fetchrow;
	$st->finish();
	$self->_the_RDB->disconnect;
    };
    $@ and $self->throw("DB_RDB::get_EntryA_by_acc - error with the database connection [$@]");
    
    if (not $myacc or not $myid) {
	$self->throw("DB_RDB::get_EntryA_by_acc - $acc is not a valid accession");
    }
    elsif (not $mydesc and $mylen) {
	$self->is_dead(1);
    }
    
    my $en = Bio::Pfam::EntryA_RDB->new('-rdb' => $self->_the_RDB());
    $en->acc( $myacc );
    $en->id( $myid );
    $en->auto_pfamA( $auto_pfamA );
    $en->description($mydesc );
    $en->model_length( $mylen );

    return $en;
}




=head2 get_EntryB_by_acc

 Title   : get_EntryB_by_id
 Usage   : $en = $db->get_EntryB_by_id('Pfam-B_101');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family


=cut

sub get_EntryB_by_acc{
    my ($self, @args) = @_;

   $self->throw("Bio::Pfam::DB_RDB does not currently support get_EntryB_by_acc");

}



=head2 get_dead_accs

 Title   : get_dead_accs
 Usage   : @list = $db->get_dead_accs('ALPHA');
 Function: Gets a list of accession numbers of dead families in current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_dead_accs{
   my ($self,$type) = @_;
   my ($dbh, $st, $acc, $id, @acclist, %map );

   eval {
       $dbh = $self->_the_RDB->connect();
       $st = $dbh->prepare("select pfamA_acc, pfamA_id from pfamA where model_length is null");
       $st->execute();
       while ( ($acc, $id) = $st->fetchrow) {
	   $map{ $acc } = $id;
       }
       $st->finish;
       $self->_the_RDB->disconnect;

   };
   $@ and $self->throw("DB_RDB::get_dead_accs - error with the database connection [$@]"); 
	   
   if ($type =~ /ALPHA/) {
       @acclist = sort { lc($map{$a}) cmp lc($map{$b}) } keys %map;
   }
   elsif ($type =~ /DATE/) {
       @acclist = sort {my ($aa,$bb); $a =~ /^PF+0+(\d+)$/; $aa = $1; $b =~ /^PF+0+(\d+)$/; $bb = $1; return $aa <=> $bb; } keys %map;
   }
   else {
       $self->throw("Bio::Pfam::DB_RDB - get_dead_accs - cannot sort into $type order");
   }

   return @acclist;
}



=head2 get_allacc

 Title   : get_allacc
 Usage   : @list = $db->getallacc('ALPHA');
 Function: Gets a list of all accession numbers in the current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_allacc{
   my ($self,$type) = @_;
   my ($dbh, $st, $acc, $id, @acclist, %map);

   eval {
       $dbh = $self->_the_RDB->connect();
       $st = $dbh->prepare("select pfamA_acc, pfamA_id from pfamA where model_length is not null");
       $st->execute();
       while ( ($acc, $id) = $st->fetchrow) {
	   $map{ $acc } = $id;
       }
       $st->finish;
       $self->_the_RDB->disconnect;

   };
   $@ and $self->throw("DB_RDB::get_allacc - error with the database connection [$@]"); 
	   
   if ($type =~ /ALPHA/) {
       @acclist = sort { lc($map{$a}) cmp lc($map{$b}) } keys %map;
   }
   elsif ($type =~ /DATE/) {
       @acclist = sort {my ($aa,$bb); $a =~ /^PF+0+(\d+)$/; $aa = $1; $b =~ /^PF+0+(\d+)$/; $bb = $1; return $aa <=> $bb; } keys %map;
   }
   else {
       $self->throw("Bio::Pfam::DB_RDB - get_allaccs - cannot sort into $type order");
   }

   return @acclist;

}




=head2 get_allids

 Title   : get_allids
 Usage   : @list = $db->get_allids('ALPHA');
 Function: Gets a list of all accession numbers in the current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_allids{
   my ($self,$type) = @_;
   my ($dbh, $st, $acc, $id, @idlist, %map);

   eval {
       $dbh = $self->_the_RDB->connect();
       $dbh->prepare("select pfamA_acc, pfamA_id from pfamA where model_length is not null");
       $st = $dbh->execute();
       while ( ($acc, $id) = $st->fetchrow) {
	   $map{ $id } = $acc;
       }
       $st->finish;
       $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::get_allids - error with the database connection [$@]"); 
	   
   if ($type =~ /ALPHA/) {
       @idlist = sort { $a cmp $b } keys %map;
   }
   elsif ($type =~ /DATE/) {
       @idlist = sort {my ($aa,$bb); $map{$a} =~ /^PF+0+(\d+)$/; $aa = $1; $map{$b} =~ /^PF+0+(\d+)$/; $bb = $1; return $aa <=> $bb; } keys %map;
   }
   else {
       $self->throw("Bio::Pfam::DB_RDB - get_allids - cannot sort into $type order");
   }

   return @idlist;

}



=head2 get_Seq_pfamseq

 Title   : get_Seq_pfamseq($id)
 Usage   : $seq = $db->get_Seq_pfamseq($id)
 Function: gets a Bio::Pfam::SeqPfam object from the underlying database
 Returns : a list of Bio::Pfam::SeqPfam objects
 Args    :
    1. name of sequence
    2. Whether the name is an accession number or id ('id' or 'acc')
    3. Whether the actual sequence itself is required
 Notes   :
    Arg 3 is ignored in this database implementation, since the sequences themselves
    are not strored in the pfam relational database

=cut

sub get_Seq_pfamseq{
   my ($self, $input_id, $lookup, $seqrequired) = @_;
   my ($dbh, $st);
   my @results;

   if( $seqrequired ) {
       warn "DB_RDB: deprecated option in get_Seq_pfamseq - you always get the sequence now!\n";
   }

   eval {
       $dbh = $self->_the_RDB->connect();
       
       if ($lookup =~ /acc/i) {
	   $st = "select * from pfamseq where pfamseq_acc = '$input_id'";
       }
       else {
	   $st = "select * from pfamseq where pfamseq_id = '$input_id'";
       }
       my($stat) = $dbh->prepare($st);
       $stat->execute();
       while( my($auto, $id, $acc, $crc, $desc, $len, $org, $tax, $is_frag, $seq_version, $current, $non_cons, $seqstr) = $stat->fetchrow ) {
#	   print "$_\n";
#	   ($auto, $id, $acc, $crc, $desc, $len, $org, $tax, $is_frag, $seq_version, $current);
	   $org  =~ s/\.$//; $org  = lc $org;
	   $desc =~ s/\.$//; $desc = lc $desc;

	   ($acc and $id and $desc and $org and $len) or
	       $self->throw("DB_RDB::get_Seq_pfamseq - Information missing");

	   my $out = Bio::Pfam::SeqPfam->new('-start'    => '1',
					     '-end'      => $len,
					     '-id'       => $id,
					     '-acc'      => $acc,
					     '-organism' => $org,
					     '-desc'     => $desc,
					     '-current'  => $current,
					     '-seq_version' => $seq_version,
					     '-seq'      => $seqstr,    # seq objects need a sequence!
					     );      
	   push( @results, $out );
       }
   
       $stat->finish;
      # $self->_the_RDB->disconnect;
   };
   $@ and $self->throw("DB_RDB::get_Seq_pfamseq - error with the database connection [$@]"); 

   return @results;
}


=head2 get_latest_Seq_pfamseq

 Title   : get_latest_Seq_pfamseq($id)
 Usage   : $seq = $db->get_latest_Seq_pfamseq($id)
 Function: gets the latest seq version from the underlying database
 Returns : a new Bio::Pfam::SeqPfam objects
 Args    :
    1. name of sequence
    2. Whether the name is an accession number or id ('id' or 'acc')
    3. Whether the actual sequence itself is required
 Notes   :
    Arg 3 is ignored in this database implementation, since the sequences themselves
    are not strored in the pfam relational database

=cut

sub get_latest_Seq_pfamseq {
    my ($self, $input_id, $lookup, $seqrequired) = @_;
    if( $seqrequired ) {
	warn "DB_RDB: deprecated option in get_latest_Seq_pfamseq - you always get the sequence now!\n";
    }

    my( @seqs ) = $self -> get_Seq_pfamseq( $input_id, "$lookup", $seqrequired );
    my $latest_seq;
    foreach my $seq ( @seqs ) {
	if( not $latest_seq or $seq->seq_version() > $latest_seq->seq_version() ) {
	    $latest_seq = $seq;
	}
    }
    return $latest_seq;
}



=head2 get_current_Seq_pfamseq

 Title   : get_current_Seq_pfamseq($id)
 Usage   : $seq = $db->get_current_Seq_pfamseq($id)
 Function: gets the currently used seq version from the underlying database
 Returns : a new Bio::Pfam::SeqPfam object
 Args    :
    1. name of sequence
    2. Whether the name is an accession number or id ('id' or 'acc')
    3. Whether the actual sequence itself is required
 Notes   :
    Arg 3 is ignored in this database implementation, since the sequences themselves
    are not strored in the pfam relational database

=cut

sub get_current_Seq_pfamseq {
    my ($self, $input_id, $lookup, $seqrequired) = @_;
    if( $seqrequired ) {
	warn "DB_RDB: deprecated option in get_current_Seq_pfamseq - you always get the sequence now!\n";
    }
    my( @seqs ) = $self -> get_Seq_pfamseq( $input_id, "$lookup", $seqrequired );
    foreach my $seq ( @seqs ) {
	if( $seq->is_current() == 1 ) {
	    return $seq;
	}
    }
    return undef;
}



=head2 pfamseq_stats

 Title   : pfamseq_stats
 Usage   : ($residues,$sequences) = $db->pfamseq_stats();
 Function: Calculate statistics for underlying pfamseq database
 Returns : An array of number of residues and sequences
         : in pfamseq
 Args    :


=cut

sub pfamseq_stats {
  my ($self) = @_;
  my ($residues,$sequences, $dbh, $st, $len);

  eval {
      $dbh = $self->_the_RDB->connect();
      $st = $dbh->prepare("select length from pfamseq");
      $st->execute();
      while( ($len) = $st->fetchrow() ) {
	  $sequences++;
	  $residues += $len;
      }
      $st->finish;
      
      $self->_the_RDB->disconnect;
  };
  $@ and $self->throw("DB_RDB::pfamseq_stats - error with the database connection [$@]"); 

  return ($residues,$sequences);
}




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
  my ($dbh, $st, $retlist);

  if ( ($query !~ /^select/i) && ($query !~ /^show/i) &&  ($query !~ /^describe/i) ) {
	$self->throw("DB_RDB::query - only 'select' queries are allowed");
  }

  eval {
	$dbh = $self->_the_RDB->connect();
	$st = $dbh->prepare($query);
	$st->execute();
	$retlist = $st->fetchall_arrayref;
	# this is hideously inefficient... changed to allow DBI to do its
	# thing
	# JT 20060111 WTSI
	# 	while( my @list = $st->fetchrow() ) {
	# 	    # print STDERR "$query: $list[0]\n";
	# 	    push @retlist, \@list;
	# 	}
	$st->finish;
	$self->_the_RDB->disconnect;
	
  };
  $@ and $self->throw("DB_RDB::query - error with query '$query' [$@]");

  return wantarray ? @$retlist : $retlist;
}



=head2 get_nested_domain

Title	: get_nested
Useage	: $rdb->get_nested_domain("$family");
Function: This function returns the list of allowed overlaps for the given family.
	  If the argument is not supplied, then all nested domain combinations will 
	  be returned.
Returns : hash of nested.  Query domain is the key;
Args	: Family name (id). Optional.

=cut

sub get_nested_domain
	{
	my ($self, $family) = @_;
	my ($out, $in, $dbh, $st,%nest);
	my $query = "select T1.pfamA_id, T2.pfamA_id from pfamA as T1 right join nested_domains on T1.auto_pfamA = nested_domains.auto_pfamA left join pfamA as T2 on T2.auto_pfamA = nested_domains.nests_auto_pfamA";
	if ($family)
		{
		#check family id is vaild and not miss typed etc.
		eval
			{
			my $acc = $self->id2acc($family);
			};	
			if ($@ =~ /$family is not a valid id/)
			{
			open (DESC, "$family/DESC")|| die "could not open DESC file to check to see if this is a new family\n";
			while (<DESC>)
				{
				if ($_ =~ /^AC(\s+)(\S+)$/)
					{
					die "This family appears to have an AC number in the DESC file, but I can not find it in the RDB!!!! (This should never happen !)\n";
					}
				}
			}
		




		eval {
       		$dbh = $self->_the_RDB->connect();
       		$st = $dbh->prepare($query." where T1.pfamA_id = \"$family\"");
       		$st->execute;
       		while(($out, $in) = $st->fetchrow)
			{
			push(@{$nest{$out}}, $in);
			}
		$st = $dbh->prepare($query." where T2.pfamA_id = \"$family\"");
      		$st->execute;
       		while(($out, $in) = $st->fetchrow)
			{
			push(@{$nest{$out}},$in);
			}
		$st->finish;
       		$self->_the_RDB->disconnect;
   		};
		}
	else
		{
		eval {
       		$dbh = $self->_the_RDB->connect();
       		$st = $dbh->prepare($query);
       		$st->execute;
       		while(($out, $in) = $st->fetchrow)
			{
			push(@{$nest{$out}},$in);
			}
      		$st->finish;
       		$self->_the_RDB->disconnect;
   			};
		}
	return %nest;
	}	
	
	
############################ Internal methods


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
