
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# $Id: SpeciesTree.pm,v 1.23 2009-10-05 09:51:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::SpeciesTree - controller to build a representation
of the species tree

=cut

package PfamWeb::Controller::SpeciesTree;

=head1 DESCRIPTION

This controller subclasses its namesake in PfamBase, to generate either an 
interactive or a text representation of the species tree for a Pfam-A, a Pfam-B 
or a clan.

Generates a B<page fragment>.

$Id: SpeciesTree.pm,v 1.23 2009-10-05 09:51:58 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::SpeciesTree';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Decides what kind of accession we've got and retrieves the row from the primary
table for that entry.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  # do we have an accession ?
  return unless $c->req->param('acc');
  
  # yes; what type of accession is it ?
  if ( $c->req->param('acc') =~ m/^(PF\d{5})(\.\d+)?$/i ) {

    # pfam A
    $c->stash->{acc}  = $1;
    $c->stash->{entryType} = 'A';
    $c->log->debug( 'SpeciesTree::begin: found Pfam A accession |' 
                    . $c->stash->{acc} . '|' )
      if $c->debug;

    # make sure we can retrieve data for that entry
    $c->stash->{entry} = $c->model('PfamDB::Pfama')
                           ->find( { pfama_acc => $c->stash->{acc} } );

  }
  elsif ( $c->req->param('acc') =~ m/^(PB\d{6})$/i ) {

    # pfam B
    $c->stash->{acc}  = $1;
    $c->stash->{entryType} = 'B';
    $c->log->debug( 'SpeciesTree::begin: found Pfam B accession |'
                    . $c->stash->{acc} . '|' )
      if $c->debug;      

    $c->stash->{entry} = $c->model('PfamDB::Pfamb')
                           ->find( { pfamb_acc => $c->stash->{acc} } );

  }
  elsif ( $c->req->param('acc') =~ m/^(CL\d{4})$/i ) {

    # looks like a clan
    $c->stash->{acc}  = $1;
    $c->stash->{entryType} = 'C';
    $c->log->debug( 'SpeciesTree::begin: found Clan accession |'
                    . $c->stash->{acc} . '|' )
      if $c->debug;

    $c->stash->{entry} = $c->model('PfamDB::Clans')
                           ->find( { clan_acc => $c->stash->{acc} } );
  }

  # make sure we actually got a VALID accession  
  unless ( $c->stash->{acc} and $c->stash->{entry} ) {
    $c->stash->{errorMsg} = 'No valid accession specified';
    return;    
  }

  # see if we should override the "too many species" check
  $c->stash->{loadTree} = defined $c->req->param('loadTree');

  # see if we're serving to Internet Exploder...
  $c->stash->{isIE} = ( defined $c->req->param('ie') and
                        $c->req->param('ie') eq 'true' ) ? 1 : 0;
  
  $c->log->debug( 'SpeciesTree::begin: isIE: |' . $c->stash->{isIE} . '|' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 auto : Private

Actions that build text representation of the tree might want to use the
release date, so we retrieve it here. We need to use an "auto" method
because C<relData> is only dropped into the stash in the C<Root:auto>.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  # see if we can get the release version, but make sure it's empty otherwise
  $c->stash->{release_data} = ''; 

  if ( $c->stash->{relData}->pfam_release ) {
    $c->stash->{release_data} = '# Generated from Pfam version ' .
                                $c->stash->{relData}->pfam_release . "\n";
  }
}

#-------------------------------------------------------------------------------
#- public actions --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 graphics : Local

Each node of the species tree represents a species which has one or more
sequences containing the Pfam domain in question. Users can select a subset of
nodes, thereby selecting a set of sequences which contain the Pfam domain. This
action renders those sequences as domain graphics. The process by which that
happens is a little convoluted:

=over 4

=item 

When the "generate graphics" link is clicked in the main page, which contains
the species tree with some nodes selected, we run a javascript snippet that 
collects the selected sequence accessions.

=item

The javascript makes an AJAX request to store the IDs in the DB and is handed
a "job ID" that identifies that list of accessions.

=item

The javascript builds a URL that points to this action, including the job ID 
parameter.

=item

This action retrieves the list of accessions from the DB, using the job ID, and
stuffs them into the stash, before handing off to the template which renders
the contents of the pop-up window.

=item

The template contains javascript that submits a further AJAX request, this time
to the DomainGraphics controller, which again retrieves the list of sequence 
accessions and builds a domain graphic for each one.

=back

Similar (tortuous) logic is used to generate the sequence alignment for selected
nodes in the species tree.

=cut

sub graphics : Local {
  my( $this, $c ) = @_;

  # validate the UUID
  my $jobId = $c->req->param('jobId');
  unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( 'SpeciesTree::graphics: bad job id' ) if $c->debug;
    $c->stash->{errorMsg} = 'Invalid job ID';
    return;
  }

  # retrieve the accessions for that job ID
  my $accession_list = $c->forward( '/utils/retrieve_ids', [ $jobId ] );
  unless( $accession_list ) {
    $c->stash->{errorMsg} ||= 'Could not retrieve sequences for that job ID';
    return;
  }

  $c->stash->{jobId}           = $jobId;
  $c->stash->{selectedSeqAccs} = $accession_list;
  
  $c->log->debug( 'SpeciesTree::graphics: rendering selected seqs as graphics' )
    if $c->debug;
  $c->stash->{template} = 'components/tools/seqViewGraphic.tt';
}

#-------------------------------------------------------------------------------

=head2 sequences : Local

Returns the sequences from selected nodes in the species tree as a FASTA
formatted text file.

=cut

sub sequences : Local {
  my( $this, $c ) = @_;
  
  # validate the UUID
  my $jobId = $c->req->param('jobId');
  unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( 'SpeciesTree::sequences: bad job id' ) if $c->debug;
    $c->stash->{errorMsg} = 'Invalid job ID';
    return;
  }

  # retrieve the sequences
  my $fasta = $c->forward( '/utils/get_sequences', 
                           [ $jobId, $c->stash->{entry} ] );
  
  # make sure we got something...
  unless( length $fasta ) {
    $c->log->debug( 'SpeciesTree::sequences: failed to get a FASTA sequence' )
      if $c->debug;
    $c->stash->{errorMsg} = 'We failed to get a FASTA format sequence file for your selected sequences.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # we got a FASTA file; make it a plain text download
  $c->res->content_type( 'text/plain' );
  $c->res->headers->header( 'Content-disposition' => 'attachment; filename='
                            . 'selected_species.fasta' );

  # format it nicely and drop it straight into the response
  my $output = '# Sequences for selected nodes from the species tree for Pfam entry '
               . $c->stash->{acc} . "\n";
  $output .= $c->stash->{release_data};

  # the accessions themselves
  $output .= $fasta;
  
  $c->res->body( $output );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 buildTree : Private

Builds an in-memory representation of the species tree, by walking recursively
down the branches found for each region in turn. The "raw" tree is dropped into 
the stash.

=cut

sub buildTree : Private {
  my ( $this, $c ) = @_;
  
  # get the species data for whatever entry we're dealing with
  $c->forward('getData');

  # check that we got data. The getData method will bomb out if the entry hits
  # the limits that are set in the config, provided the "loadTree" flag isn't 
  # set in the stash
  return unless $c->stash->{regions};
  
  $c->log->debug( 'SpeciesTree::buildTree: got '
                  . scalar @{$c->stash->{regions}} .' regions from sub-class' )
    if $c->debug;

  # we've got data; let's build the tree  
  my $tree     = {};
  my $maxDepth = 0;
  foreach my $region ( @{ $c->stash->{regions} } ) {

    # first, get the species information
    my $species = $region->auto_pfamseq->species;
    $species =~ s/^(\s+)//g; # trim leading whitespace

    # next, the taxonomy above the species
    my $tax = $region->auto_pfamseq->taxonomy;
    $tax =~ s/\s+//g;
    my @tax = split m/\;/, $tax;

    # add the species onto the end of the taxonomy, so we have it all in
    # one place
    $tax[$#tax] = $species;

    # find the maximum depth for the tree
    $maxDepth = scalar @tax if scalar @tax > $maxDepth;

    # build a hash to describe this branch
    my $speciesData = { acc     => $region->auto_pfamseq->pfamseq_acc,
                        species => $species,
                        tax     => \@tax };

    # flag the node if it's in the seed alignment
    $speciesData->{inSeed}++ if $c->stash->{inSeed}->{ $speciesData->{acc} };
    
    # add this branch to the tree
    $this->addBranch( $tree, $speciesData );
  }
  
  # store the final depth of the tree
  $tree->{maxTreeDepth} = $maxDepth;

  $c->stash->{rawTree} = $tree;
}

#-------------------------------------------------------------------------------

=head2 getData : Private

Forwards straight to the appropriate method for the type of entry that we're
working with, be it Pfam-A, Pfam-B or clan. This is the point at which we 
decide whether the entry has too many species to attempt building the tree. The
actual data are retrieved only if we don't hit up against the limits that are
specified in the config.

=cut

sub getDataByType : Private {
  my( $this, $c ) = @_;

  # retrieve the data for the appropriate family type
  if( $c->stash->{entryType} eq 'A' ) {
    $c->forward( 'getFamilyData' );
  } elsif( $c->stash->{entryType} eq 'C' ) {
    $c->forward( 'getClanData' );
  }
  # (we don't need to forward to a method to retrieve Pfam-B data, since we have
  # already retrieved that when we had to count the number of species)
  
}

#-------------------------------------------------------------------------------

=head2 countSpecies : Private

Retrieves or calculates the number of species in the entry. For Pfam-As and 
clans we can look this up directly in the DB but for Pfam-B we actually have
to count it up.

Again, for Pfam-Bs, since we need to get all regions here, we'll stash them
and they can be used if we go ahead and build the tree, rather than repeating
the query in the C<getData> methods.

=cut

sub countSpecies : Private {
  my ( $this, $c ) = @_;

  # for Pfam-As or clans we can just look up the number of species in the 
  # main table, via the "entry" that was put in the stash by C<begin>, but
  # for Pfam-Bs we'll actually have to count the number of species

  if ( $c->stash->{entryType} eq 'B' ) {
    
    my @regions = $c->model('PfamDB::PfambReg')
                    ->search( { auto_pfamB => $c->stash->{entry}->auto_pfamb },
                              { prefetch   => [ qw( auto_pfamseq ) ] } );

    # as we're retrieving them here anyway, stash the regions, so we don't need
    # to get them again later
    $c->stash->{regions} = \@regions;

    my %species_unique = map {$_->auto_pfamseq->species => 1} @regions;
    $c->stash->{numSpecies} = scalar( keys %species_unique );

  }
  else {
    $c->stash->{numSpecies} = $c->stash->{entry}->number_species;
  }

  $c->log->debug( 'SpeciesTree::countSpecies: numSpecies: |'
                  . $c->stash->{numSpecies} . '|' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 getFamilyData : Private

Retrieves species data for the specified family. For a Pfam-A this requires
two queries, to retrieve the species that are represented in the seed, as well
as the Pfam-A sequences themselves.

=cut

sub getFamilyData : Private {
  my( $this, $c ) = @_;
  
  # get the species information for the full alignment
  my @regions = $c->model('PfamDB::PfamaRegFullSignificant')
                  ->search( { 'auto_pfama.pfama_acc' => $c->stash->{acc},
                              'in_full'         => 1 },
                            { join              => [ qw( auto_pfamseq auto_pfama ) ],
                              prefetch          => [ qw( auto_pfamseq ) ] } );

  $c->stash->{regions} = \@regions;

  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @regions . '| full regions' ) if $c->debug;

  # get the species information for the seed alignment
  my @resultsSeed = $c->model('PfamDB::PfamaRegSeed')
                      ->search( { 'auto_pfama.pfama_acc' => $c->stash->{acc} },
                                { join              => [ qw( auto_pfamseq auto_pfama ) ],
                                  prefetch          => [ qw( auto_pfamseq ) ] } );
  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @resultsSeed . '| seed regions' ) if $c->debug;
                
  # hash the seed info so we can easily look up whether a sequence is 
  # found in the seed alignment
  my %inSeed;
  foreach my $region ( @resultsSeed ) {
    $inSeed{ $region->pfamseq_acc }++;
  }

  $c->stash->{inSeed}  = \%inSeed;  
}

#-------------------------------------------------------------------------------

=head2 getClanData : Private

Retrieves species data for the specified clan. This requires us to look up 
the auto_pfamA numbers for each of the Pfam-As in the clan and then, for each
of those families, to get all species in that family.

=cut

sub getClanData : Private {
  my ( $this, $c ) = @_;
  
  # get the species information for the full alignment for each clan member. 
  # This probably could be done in one query, but this is going to be quicker
  # (I think...)
  my @clan_members = $c->model('PfamDB::ClanMembership')
                        ->search( { 'auto_clan.clan_acc' => $c->stash->{acc} },
                                  { join                 => [ qw( auto_clan ) ] } );

  $c->log->debug( 'SpeciesTree::getClanData: found ' . scalar( @clan_members )
                  . ' clan members' ) if $c->debug;

  my ( @allRegions, @regions );
  foreach my $clan_member ( @clan_members ) {
    @regions = $c->model('PfamDB::PfamaRegFullSignificant')
                 ->search( { 'auto_pfama' => $clan_member->auto_pfama->auto_pfama,
                             'in_full'    => 1 },
                           { join              => [ qw( auto_pfamseq ) ],
                             prefetch          => [ qw( auto_pfamseq ) ] } );

    push @allRegions, @regions;
  }

  $c->log->debug( 'SpeciesTree::getClanData: found ' . scalar( @regions )
                  . ' regions' ) if $c->debug;
  
  $c->stash->{regions} = \@allRegions;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
