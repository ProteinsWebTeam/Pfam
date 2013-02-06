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
    
    #Idaelly we would use the taxid as the hash key and not
    #do this every time, but we can not due to subsp etc.
    #
    #This data should als go into the database taxonomy table.
    
    #Copy this sideway as we can not modify it.
    
    my $longSpecies = $row->{species};
    if($longSpecies =~ /(.*)\s+(sp|subsp)\./){
      $longSpecies = $1;
    }
      
    #Looks like this may have been going on....
    #if(my ($genus, $species) = $longSpecies =~ /^(\S)\S+\s+(\.*)/){
    #  $seenSpecies{$row->{ncbi_id}}->{name} = $genus.'.'.$species;
    #}
    #=GS Enterobacter.1         AC    CP000653.1/2739273-2739189
    #=GS E.aerogenes.1          AC    M15749.1/155-239
    #=GS Escherichia_coli_APE.1 AC    CP000468.1/2032638-2032552
    #=GS Salmonella_enterica_.1 AC    CP000857.1/1802194-1802277
    #=GS Shigella_flexneri_20.1 AC    CP001383.1/2080784-2080698
    
    
    #TODO - need to check with JD
    #I can not work out what Rfam has done in the past to this name!  
    $longSpecies =~ s/\s+/\_/g;
    
    if(!exists($seenSpecies{$longSpecies})){
      $seenSpecies{$longSpecies} = 1;
    }
    my $speciesName = $longSpecies.'.'.$seenSpecies{$longSpecies};
    $self->set_sqname($i, $speciesName);
    #Increment counter
    $seenSpecies{$longSpecies}++;
  }
}

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
  
  #Iternate over the alignment
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
      if(scalar(@$rows) > 1){
        #TODO - Eric put your region matching thing here if there are more than
        #one rows returned..
      }else{
        $row= $rows->[0];
      }
      
      #Build up the name....
      my $newName = $row->[2].'_'.$nse.'_'.$row->[4];
       $self->set_sqname($i, $newName);
      
    }else{
      croak("Failed to find region from database for $nse.");
    }
  }
}

1;