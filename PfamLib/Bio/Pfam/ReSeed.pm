
#
# BioPerl module for Bio::ReSeed
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
# Copyright Ewan Birney
#
# You may distribute this module under the same terms as perl itself
#
# sgj 11/11/02
# Trying to separate this from bioperl itself -- reliance seems unecessary 
# and complex.  Can use what we want without inheriting from Bio::Root::Object.
#
# blast methods deleted!

# POD documentation - main docs before the code

=head1 NAME

Bio::ReSeed - Converts one multiple alignment to another database

=head1 SYNOPSIS

  $re = Bio::ReSeed->new( -olddb => 'pfamseq', -newdb => 'pfamseq4' );

  # or

  $re = Bio::ReSeed->new ( -blast => 'pfamseq', -blastindex => $pfamseq_bioperl_index);
  # $old is a Bio::SimpleAlign object

  $new = $re->convert($old);


=head1 DESCRIPTION

This object factory converts Bio::SimpleAlign objects into new SimpleAlign objects
which have a new underlying database

It is quite specific to Pfam. In particular, it expects the old and new databases
to be pfamseq style databases, where you want to follow the sequences by id, but
have to use the accession number to follow the database. This requires the databases
to be SRS indexed (it makes calls to getz).

However, if you want to use it on a different database mapping system, you can always
subclass it and replace the functions _get_old_seq and _convert_seq which provide
this logic.

Finally a note about the conversion process. If the old and new sequences are identical,
then it maps them without changing the sequence. If the old subsequence can be found 
identically inside the new sequence, that subsequence is used. Otherwise an HMM alignment
step is used to make the new sequence.

=head1 CONTACT

pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::ReSeed;
use vars qw($AUTOLOAD @ISA);
use strict;

use Bio::Pfam;
use Bio::LocatableSeq;
use Bio::Tools::pSW;
use Bio::Index::Swissprot;
use Bio::Pfam::AlignPfam;

sub new {
    my $caller = shift;
    my %args   = @_;
    my $class  = ref( $caller ) || $caller;
    my $self   = {};
    $self->{'newseqcache'} = {};
    $self->{'oldseqcache'} = {};
    bless( $self, $class );
    $self->_oldinx( Bio::Index::Swissprot->new( '-filename' => $args{'-oldinx'} ) )
        if( $args{'-oldinx'} );
    $self->_newinx( Bio::Index::Swissprot->new( '-filename' => $args{'-newinx'} ) )
        if( $args{'-newinx'} );
    return $self;
}


=head2 convert

 Title   : convert
 Usage   : $new = $reseed->convert($old,\*ERROR_LOG,\*LOG);
 Function: Actually does the conversion of the old to the new
           database
 Returns : New Bio::SimpleAlign object
 Args    : old Bio::SimpleAlign object, and then two optional filehandles
           The first is only written to for subs/indel errors: the second
           records everything which is done

=cut

sub convert{
   my ($self,$old,$error,$log) = @_;
   my @toalign;

   # flush sequence caches
   $self->_flush_newseqcache();
   $self->_flush_oldseqcache();

   my $new = Bio::Pfam::AlignPfam->new();

   # in case we need some alignments, we need the pSW alignment factory
 
   my $psw = Bio::Tools::pSW->new('-matrix' => $Bio::Pfam::pfam_bin_dir."/blosum62.bla"); 
  
   # get all the accessions
   foreach my $seq ( $old->each_seq ) {
       $seq->accession_number( $seq->id ); # pfam SEED keyed off seq acc
   }

   my %nse;

   SEQ: foreach my $seq ( $old->each_seq ) {

     # so we can report easily
     my $name = $seq->accession_number."/".$seq->start."-".$seq->end;

     # get old and new underlying sequences
#     my $oldseq = $self->_get_old_seq($seq);
	 
#     if (! defined $oldseq or length($oldseq->seq()) <=0 ) {
#	 if ( defined $error ) {
#	     print $error "$name could not be retrieved from old database\n";
#	     print $log "$name could not be retrieved from old database\n";  
#	     next SEQ;
#	 } 
#	 else {
#	     $self->throw("Could not convert $name");
#	 }	 
#     }

     my $newseq = $self->_convert_seq($seq, $error, $log);

     if( not $newseq or length($newseq->seq()) < 1 ) {
	 if ( defined $error ) {
	     print $error "$name could not be retrieved from new database\n";
	     print $log "$name could not be retrieved from new database\n";
	     next SEQ;
	 } 
	 else {
	     die("Could not convert $name");
	 }
     }
     
     if( $newseq->accession_number ne $seq->accession_number ) {
	 if ( defined $error ) {
	     print $error "$name is a secondary accession of ", $newseq->accession_number, "\n";
	     print $log "$name is a secondary accession of ", $newseq->accession_number, "\n";  
	 } 
	 else {
	     die("Could not convert $name");
	 }	 
     }
	 
     my $oldstart = $seq->start();
     my $oldend   = $seq->end();
     my $oldacc   = $seq->id();

     my $strippedseq = $seq->seq();
	     
     $strippedseq =~ s/[^A-Za-z]//g;
     $strippedseq =~ tr/[a-z]/[A-Z]/;

     # get fields out for convience
     # so we can report stuff to log files etc
	     
     my $newacc = $newseq->accession_number();

     # set seq id to acc
     $seq->id( $newacc );
     $newseq->id( $newacc );
     
     # if they are the same - yippee!
     if( ($oldend <= $newseq->length) and ($strippedseq eq $newseq->subseq( $oldstart, $oldend )) ) {
	 if( exists $nse{ $newacc."/".$oldstart."-".$oldend } ) {
	     $log && print $log "$name new name $newacc already in seed so skipping\n";
	     next SEQ;
	 }
	 # add the old sequence in
	 $new->add_seq($seq);
	 $nse{ $newacc."/".$oldstart."-".$oldend } = 1;
	 $log && print $log "$name maps without any changes $oldacc:$newacc\n";
	 next SEQ;
     }
	     
     # check to see if the subsequence is unique somewhere
     # we need to take the old subsequence, remove the gaps and
     # see if it matches to anything in the new subsequence
     
     if( ($oldend <= $newseq->length) and ($newseq->subseq( $oldstart, $oldend ) =~ /^([A-Z]*)$strippedseq([A-Z]*)$/) ) {
	 # we have an identical subsequence
	 my $pre   = $1;
	 my $post  = $2;
	 my $start = length($pre)+1;
	 my $end   = length($strippedseq)-1+$start;
	 
	 my $newsubseq = Bio::LocatableSeq->new( -seq              => $seq->seq(), 
						 -id               => $newseq->accession_number(), # key off acc
						 -accession_number => $newseq->accession_number(), 
						 -start            => $start, 
						 -end              => $end, 
						 -type             => 'aligned');
	 
	 
	 if( exists $nse{ "$newacc/$start-$end" } ) {
	     $log && print $log "$name maps to $newacc/$start-$end but already in new seed\n";
	     next SEQ;
	 }

	 # add it to the alignment	 
	 $new->add_seq($newsubseq);
	 $nse{ "$newacc/$start-$end" } = 1;
	 $log && print $log "$name sequence has changed but subsequence ok $oldacc:$newacc, old[$oldstart:$oldend] vs new[$start:$end]\n";
	 
	 next SEQ; # back to main loop
     }
	 
     # yuk! We have a subs/indel problem.
     # we need to chop out the sequence as best we can using bp_sw
     # to get start/end points. Then we place it on the @toalign
     # array. At the end of processing all the sequences, we realign
     # these by making a temporary hmm...

     my $tempseq = Bio::LocatableSeq->new ( -seq => $strippedseq, 
					    -id  => 'tempseq');

     # At this point, we have decided that the sequence has changed, so we 
     # are going to do a smith-waterman alignment of the subsequence in the 
     # old seed to the new sequence.

     my ($tempaln, $score) = $psw->pairwise_alignment($tempseq,$newseq);
     my @tarray = $tempaln->each_seq();
     $tempseq = $tarray[1];
	     
     my $newsubseq = Bio::LocatableSeq->new ( -seq   => $newseq->subseq($tempseq->start,$tempseq->end),
					      -id    => $newseq->accession_number(), # key off acc 
					      -accession_number => $newseq->accession_number(),
					      -start => $tempseq->start, 
					      -end   => $tempseq->end,
					      );
     
     if( exists $nse{ $newsubseq->id."/".$newsubseq->start."-".$newsubseq->end } ) {
	 $log && print $log "$name has changes but already in new seed\n";
	 next SEQ;
     }

    # push it on to the array
     push(@toalign,$newsubseq);
     $nse{ $newsubseq->id."/".$newsubseq->start."-".$newsubseq->end } = 1;
     $log && print $log "$name has changes\n";
     $error && print $error "$name has changes\n";
 } # end of loop over all sequences.


   # now we need to align the remaining sequences
   if( @toalign >= 1 ) { 
       # we have some sequences to align to or HMMer will die
       # build a temporary alignment file

       open(_TEMP_,">reseed.temp.$$") || die("Could not open temp file $!");
       $new->write_Pfam(\*_TEMP_);
       close(_TEMP_);

	 
       open(_TEMP_,">reseed.unaln.$$") || die("Could not open temporary unaligned file $!");
       foreach my $tseq ( @toalign ) {
	   my $name = $tseq->get_nse();
	   print _TEMP_ ">$name\n";
	   print _TEMP_ $tseq->seq() , "\n";
       }
       
       close(_TEMP_);
     
# build an HMM, but only when reseed.temp.$$ has size.
       if(-s "reseed.temp.$$"){
	   open(_T_,"hmmbuild reseed.HMM.$$ reseed.temp.$$ |");
	   while(<_T_>) {
	       ;
	   }
	   close(_T_);
	   # write out all the other sequences to be aligned
	   ;

	   # make the alignment
       

          system("hmmalign --mapali reseed.temp.$$ -o temp.$$ reseed.HMM.$$ reseed.unaln.$$") and die "hmmalign failied";
	   open(_TEMP2_, "temp.$$") or die "Couln;t open filehandle";   
	    

       }else{
	   warn "\n*** All sequences changed in SEED, please check carefully ***\n\n";
	   open(_TEMP2_, "create_alignment.pl -fasta reseed.unaln.$$ -m |")
       }

       my $newaln = Bio::Pfam::AlignPfam->new();
       $newaln->read_selex(\*_TEMP2_);
   
       close(_TEMP2_) || die("Could not close hmmalign ");

       # remove temporary files
   
       unlink("reseed.temp.$$");
       unlink("reseed.HMM.$$");
       unlink("reseed.unaln.$$");
       unlink("temp.$$");

       # reset new to newaln
   
       $new = $newaln;
       $new->map_chars('\-','.');
   }
   return $new;
  
}

=head2 _get_old_seq

 Title   : _get_old_seq
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _get_old_seq{
  my ($self,$subseq) = @_;
  my $acc = $subseq->accession_number();
  my $new = $self->_is_oldseqcache($acc);

  if( $new ) {
      return $new;
  }

  if( my $oldinx = $self->_oldinx() ) {
      $new = $oldinx->fetch( $acc );
  }

  $self->_add_to_oldseqcache($acc,$new);
  return $new;  
}


=head2 _convert_seq

 Title   : _convert_seq
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _convert_seq{
  my ($self,$subseq) = @_;
  my $newinx = $self->_newinx();

  my $acc = $subseq->accession_number();
  my $new = $self->_is_newseqcache($acc);

  if( ! defined $new ) {
      $new = $newinx->fetch( $acc );;
  } 
  else {
      # print STDERR "Using cache...\n";
      return $new;
  }

  $self->_add_to_newseqcache($acc,$new);
  return $new;
}

=head2 _oldinx

 Title   : _oldinx
 Usage   : $obj->_oldinx($newval)
 Function: 
 Example : 
 Returns : value of _oldinx
 Args    : newvalue (optional)


=cut

sub _oldinx {
   my ($self,$value) = @_;
   if( defined $value) {
      $self->{'_oldinx'} = $value;
    }
    return $self->{'_oldinx'};

}

=head2 _newinx

 Title   : _newinx
 Usage   : $self->_newinx($newval)
 Function: 
 Example : 
 Returns : value of _newinx
 Args    : newvalue (optional)


=cut

sub _newinx {
   my ($self,$value) = @_;
   if( defined $value) {
      $self->{'_newinx'} = $value;
    }
    return $self->{'_newinx'};

}

=head2 _add_to_newseqcache

 Title   : _add_to_newseqcache
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :

=cut

sub _add_to_newseqcache{
   my ($self,$hashval,$seq) = @_;
   
   $self->{'newseqcache'}->{$hashval} = $seq;

}

=head2 _flush_newseqcache

 Title   : _flush_newseqcache
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _flush_newseqcache{
   my ($self) = @_;

   $self->{'newseqcache'} = {};
}

=head2 _is_newseqcache

 Title   : _is_newseqcache
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _is_newseqcache{
   my ($self,$hashval) = @_;
   return $self->{'newseqcache'}->{$hashval};
}


=head2 _add_to_oldseqcache

 Title   : _add_to_oldseqcache
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _add_to_oldseqcache{
   my ($self,$hashval,$seq) = @_;
   
   $self->{'oldseqcache'}->{$hashval} = $seq;

}

=head2 _flush_oldseqcache

 Title   : _flush_oldseqcache
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _flush_oldseqcache{
   my ($self) = @_;

   $self->{'oldseqcache'} = {};
}

=head2 _is_oldseqcache

 Title   : _is_oldseqcache
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _is_oldseqcache{
   my ($self,$hashval) = @_;

   return $self->{'oldseqcache'}->{$hashval};

}

1;
# 
