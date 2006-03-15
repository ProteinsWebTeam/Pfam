
#
# BioPerl module for Bio::Pfam::SeqPfam
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::SeqPfam - A protein sequence with mark-up as per Stockholm format 

=head1 SYNOPSIS

    use Bio::Pfam::SeqPfam;

    $pfamAreg = new Bio::Pfam::SeqPfam( '-seq' => $sequence,
					'-id' => $id,
					'-acc' => $acc,
					'-desc' => $description,
					'-org' => $organism,
					'-tax' => $taxononmy,
					'-start' => $start,
					'-end' => 'end',
					'-annotation' => $annot,
					'-seq_version' => $seq_version,
					'-current' => $current,
					'-ss' => $sec_struct_reg,
					'-sa' => $surf_aacc_reg,
					'-tm' => $trans_mem_reg,
					'-pp' => $post_prob_reg,
					'-li' => $lig_bind_reg,
					'-db' => $dom_bind_reg);
					


=head1 DESCRIPTION

This object is represents a pfamseq sequence, as used by the Pfam database. It 
is a sub-class of Bio::Seq, but provides some additional fields/methods not 
provided by that class, for example organism, accession, secondary structure,
surface_accessibility. The annotattion field is capable of storing any 
literature or database references associated with the sequence 

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::SeqPfam;
use vars qw($AUTOLOAD @ISA);
use strict;

use Bio::Pfam::Root;
use Bio::LocatableSeq;
use Bio::Seq::RichSeq;

@ISA = qw(Bio::Pfam::Root Bio::LocatableSeq Bio::Seq::RichSeq);


sub new {
  my($class, %params ) = @_;
  my( $id, $start, $end, $seq, $acc, $desc, $org, $tax, $annot, $seq_version, $current, $ss, $sa, $tm, $pp, $lb, $db) = 
      (
       ($params{'-ID'}          || $params{'-id'}),
       ($params{'-START'}       || $params{'-start'}),
       ($params{'-END'}         || $params{'-end'}),
       ($params{'-SEQ'}         || $params{'-seq'}),
       ($params{'-ACC'}         || $params{'-acc'}),
       ($params{'-DESC'}        || $params{'-desc'}), 
       ($params{'-ORGANISM'}    || $params{'-organism'}),
       ($params{'-TAXONOMY'}    || $params{'-taxonomy'}),
       ($params{'-ANNOTATION'}  || $params{'-annotation'}),
       ($params{'-SEQ_VERSION'} || $params{'-seq_version'}),
       ($params{'-CURRENT'}     || $params{'-current'}),
       ($params{'-SS'}          || $params{'-ss'}),
       ($params{'-SA'}          || $params{'-sa'}),
       ($params{'-TM'}          || $params{'-tm'}),
       ($params{'-PP'}          || $params{'-pp'}),
       ($params{'-LI'}          || $params{'-li'}),
	   ($params{'-DB'}          || $params{'-db'})
       );
       
  my $self = $class->SUPER::new( %params );  # this is Bio::Pfam::Root
                      # so we have to set Bio::LocatableSeq fields ourself

  $self->id( $id );
  $self->start( $start );
  $self->end( $end );
  $self->seq( $seq );
  
  $self->acc( $acc );
  $self->desc( $desc );
  $self->organism( $org );
  $self->taxonomy( $tax );
  $self->annotation( $annot );
  $self->seq_version( $seq_version );
  $self->sec_struct( $ss );
  $self->surf_access( $sa );
  $self->trans_membrane( $tm );
  $self->posterior_prob( $pp );
  $self->ligand_binding( $lb );
  $self->domain_binding( $db );

  return $self; # success - we hope!
}



=head2 acc

 Title   : acc
 Usage   : 
    $dom->accession(); # or ...
    $dom->accession( 123 );
 Function: For setting and getting the accession number field in the object

=cut

sub acc {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'seqpfam_acc'} = $value;
   }
   return $self->{'seqpfam_acc'};
}

=head2 desc

 Title   : desc
 Usage   : 
    $dom->desc(); # or ...
    $dom->desc( 123 );
 Function: For setting and getting the accession number field in the object

=cut

sub desc {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'description'} = $value;
   }
   return $self->{'description'};
}



=head2 organism

 Title   : organism
 Usage   : 
    $seq->organism(); # or ...
    $seq->organism( "duck" );
 Function: For setting and getting the organism field in the object

=cut

sub organism {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'seqfam_org'} = $value;
   }
   return $self->{'seqfam_org'};
}



=head2 taxonomy

 Title   : taxonomy
 Usage   : 
    $seq->taxonomy(); # or ...
    $seq->taxonomy( "fgdrgdsrg" );
 Function: For setting and getting the taxononmy field in the object
    
=cut
    
sub taxonomy {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'seqfam_tax'} = $value;
   }
   return $self->{'seqfam_tax'};
}





=head2 annotation

 Title   : annotation
 Usage   : 
    $annot = $seq_>annotation(); # or ...
    $seq->annotation( $annot );
 Function: For setting and getting the annotation associated with the sequence
    
=cut
    
sub annotation {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'seqpfam_annot'} = $value;
   }
   return $self->{'seqpfam_annot'};
}



=head2 active_site

 Title   : active_site
 Usage   : 
    $seq->active_site( $reg ); # or ...
 Function: For setting and getting an OtherRegion object to 
    store the active site for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 
    
=cut
    
sub active_site {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'seqpfam_active_site'} = $newreg;
   }
   return $self->{'seqpfam_active_site'};
   
}


=head2 crc64

 Title   : crc64
 Usage   : 
    $checksum = $seq->crc64(); 
 Function: Returns a CRC checksum for the sequence
 Notes:
    The algorithm to compute the CRC is described in the ISO 3309
    standard.  The generator polynomial is x64 + x4 + x3 + x + 1.
  Reference: W. H. Press, S. A. Teukolsky, W. T. Vetterling, and B. P.
    Flannery, "Numerical recipes in C", 2nd ed., Cambridge University 
    Press. Pages 896ff.

=cut

sub crc64 {
    my $self = shift;

    my $POLY64REVh = 0xd8000000; 
    my @CRCTableh = 256;
    my @CRCTablel = 256;
    
    my $crcl = 0;
    my $crch = 0;

    my $sequence = $self->seq();

    # ** Initialisation
    #32 first bits of generator polynomial for CRC64
    #the 32 lower bits are assumed to be zero

    for (my $i=0; $i<256; $i++) {
	my $partl = $i;
	my $parth = 0;
	for (my $j=0; $j<8; $j++) {
	    my $rflag = $partl & 1;
	    $partl >>= 1;
	    $partl |= (1 << 31) if $parth & 1;
	    $parth >>= 1;
	    $parth ^= $POLY64REVh if $rflag;
	}
	$CRCTableh[$i] = $parth;
	$CRCTablel[$i] = $partl;
    }

    foreach (split '', $sequence) {
	my $shr = ($crch & 0xFF) << 24;
	my $temp1h = $crch >> 8;
	my $temp1l = ($crcl >> 8) | $shr;
	my $tableindex = ($crcl ^ (unpack "C", $_)) & 0xFF;
	$crch = $temp1h ^ $CRCTableh[$tableindex];
	$crcl = $temp1l ^ $CRCTablel[$tableindex];
    }
    return sprintf("%08X%08X", $crch, $crcl);
}



=head2 sec_struct

 Title   : sec_struct
 Usage   : 
    $seq->sec_struct( $reg); # or ...
 Function: For setting and getting an OtherRegion object to 
    store the secondary structure for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub sec_struct {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'seqpfam_sec_struct'} = $newreg;
   }
   return $self->{'seqpfam_sec_struct'};
   
}




=head2 surf_access

 Title   : surf_access
 Usage   : 
    $seq->surf_access( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the secondary structure for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub surf_access {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'seqpfam_surf_access'} = $newreg;
   }
   return $self->{'seqpfam_surf_access'};

}





=head2 trans_membrane

 Title   : trans_membrane
 Usage   : 
    $seq->trans_membrane( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the transmembrane mark-up for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub trans_membrane {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'seqpfam_trans_mem'} = $newreg;
   }
   return $self->{'seqpfam_trans_mem'};   
}



=head2 posterior_prob

 Title   : posterior_prob
 Usage   : 
    $seq->posterior_prob( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the per-residue posterior prob.   for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub posterior_prob {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'seqpfam_post_prob'} = $newreg;
   }
   return $self->{'seqpfam_post_prob'};
}




=head2 ligand_binding

 Title   : ligand_binding
 Usage   : 
    $seq->ligand_binding( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the per-residue ligand binding. for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub ligand_binding {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'seqpfam_lig_bind'} = $newreg;
   }
   return $self->{'seqpfam_lig_bind'};
}

=head2 domain_binding
Title   : domain_binding
 Usage   : 
    $seq->domain_binding( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the per-residue domain binding. for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub domain_binding {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       push(@{$self->{'seqpfam_dom_bind'}}, $newreg);
   }
   return \@{$self->{'seqpfam_dom_bind'}};
}

=head2 is_current

 Title   : is_current
 Usage   : $seq->is_current();
 Function: Check if $seq is currently used version
 Notes:

=cut

sub is_current {
   my ($self) = @_;
   return $self->{'current'};
}


=head2 seq_version

 Title   : seq_version
 Usage   : $seq->seq_version( $num );
 Function: For setting and getting sequence version
 Notes:

=cut

sub seq_version {
   my ($self, $newver) = @_;
   if (defined $newver) {
       $self->{'seq_version'} = $newver;
   }
   return $self->{'seq_version'};
}


1;
