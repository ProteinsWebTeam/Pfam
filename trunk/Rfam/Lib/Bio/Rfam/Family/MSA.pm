=head1 NAME

Bio::Rfam::Family::MSA - a module that reads in a Rfam SEED or ALIGN file.

=cut

package Bio::Rfam::Family::MSA;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: MSA.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 25, 2013 8:29:29 AM

=cut

use strict;
use warnings;
use Data::Printer;
use Carp;
use Moose;
use MooseX::NonMoose;
use Moose::Util::TypeConstraints;

extends 'Bio::Easel::MSA';

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'aliType' => (
  is       => 'ro',
  isa      => enum([qw[ full seed ]]),
  required => 1,
);

#------------------------------------------------------------------------------
=head2 seqToSpeciesNames

  Title    : seqToSpeciesNames
  Incept   : finnr, Feb 6, 2013 4:36:29 PM
  Usage    : $msa->seqToSpeciesNames($rfamdb);
  Function : Converts the NSE in the alignment to species names, based on
           : precalculated alignment display names found in the taxonomy table.
  Args     : RfamLive schema object
  Returns  : Nothing
  
=cut

sub seqToSpeciesNames {
  my ($self, $rfamdb) = @_;
  
  my %seenSpecies;
  my $sth = $rfamdb->prepare_seqaccToTaxon;
  for( my $i = 0; $i < $self->nseq; $i++){
    my $nse = $self->get_sqname($i);
    
    #Add the nse as a GS line
    #=GS Shigella_flexneri_20.1 AC    CP001383.1/2080784-2080698
    $self->addGS('AC', $nse, $i);
    
    
    my ($name, $start, $end) = $nse =~ /^(\S+)\/(\d+)\-(\d+)$/;
    
    $sth->execute($name);
    my $row = $sth->fetchrow_hashref;
    #Row will look like this....
    #{
    #align_display_name   "Enterobacter_aerogenes[548]",
    #ncbi_id              548
    #}
    
    #Idaelly we would use the taxid as the hash key and not
    #do this every time, but we can not due to subsp etc.

    if(!exists($seenSpecies{$row->{align_display_name}})){
      $seenSpecies{$row->{align_display_name}} = 1;
    }
    my $speciesName = $row->{align_display_name}.'.'.$seenSpecies{$row->{align_display_name}};
    $self->set_sqname($i, $speciesName);
    #Increment counter
    $seenSpecies{$row->{align_display_name}}++;
  }
}

#------------------------------------------------------------------------------
=head2 seqToBitNSEAndSpecies

  Title    : seqToBitNSEAndSpecies
  Incept   : finnr, Feb 6, 2013 4:38:52 PM
  Usage    : $msa->seqToBitNSEAndSpecies($rfamdb, $rfam_acc, $isSeed);
  Function : Looks up regions in the database full_region table to add the bit 
           : score and species string to the alignment sequence name.  It determines
           : the region using maximal overlap criteia as the regions may not be
           : the same. This was incepted to work for SEED alignments and back
           : estimate the bit score. Although, written slightly genericly, I
           : have reservations of how else it could be used. 
  Args     : RfamLive schema object, rfam accession and a boolean to restict
           : to terms labeled as 'seed' in the full regions table.
  Returns  : Nothing, results applied to the object.
  
=cut

sub seqToBitNSEAndSpecies {
  my ($self, $rfamdb, $rfam_acc, $isSeed) = @_;
  
  #check the rfamlive object
  croak("Expected an RfamLive object\n") if(!$rfamdb->isa('RfamLive'));
  #check the rfam_acc looks real!
  croak("$rfam_acc does not look like a Rfam accession\n") 
    if($rfam_acc !~ /^RF\d{5}$/);
  #Deal with isSeed being not defined
  $isSeed = defined($isSeed) ? $isSeed : 0;
  
  #Get the statement handle - when Rfam expands to genomes etc, we are going
  #to need to put a switch around the statement, so it uses the appropriate
  #table.
  my $sth = $rfamdb->prepare_fullRegionAndTaxonBySeqAcc( $isSeed );
  
  #Iterate over the alignment
  for( my $i = 0; $i < $self->nseq; $i++){
    my $nse = $self->get_sqname($i);
    my ($name, $start, $end) = $nse =~ /^(\S+)\/(\d+)\-(\d+)$/; 
    #Now get the list of potential hits
    $sth->execute($name, $rfam_acc);
    my $rows = $sth->fetchall_arrayref;
    #$rows is a 2d array that looks like this:
    #[
    #[0] 155,
    #[1] 239,
    #[2] 95.72,
    #[3] 548,
    #[4] "Enterobacter_aerogenes"
    #]
    #
    # seq_start, seq_end, bit_score, tax_id
    #
    if($rows){
      my $row;
      my $max_overlap = 0.;
      for($j = 0; $j < scalar(@rows); $j++) { 
	  # TODO: put overlap_fraction in Utils.pm? But then Bio-Easel would need Utils.pm...
	  $overlap = $self->overlap_fraction($start, $end, $rows->[$j]->[0], $rows->[$j]->[1]);
	  if($overlap > $max_overlap) { 
	      $row = $rows->[$j]; 
	      $max_overlap = $overlap;
	  }
      }
      if(! defined $row) { croak "Unable to find overlapping hit for $nse"; }
      
      #Build up the name....
      my $newName = $row->[2].'_'.$nse.'_'.$row->[4];
       $self->set_sqname($i, $newName);
      
    }else{
      croak("Failed to find region from database for $nse.");
    }
  }
}

1;
