
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# $Id: SpeciesTree.pm,v 1.4 2008-11-04 15:07:56 jt6 Exp $

=head1 NAME

RfamWeb::Controller::SpeciesTree - controller to build a representation
of the species tree

=cut

package RfamWeb::Controller::SpeciesTree;

=head1 DESCRIPTION

This controller subclasses its namesake in PfamBase, to generate either an 
interactive or a text representation of the species tree for an Rfam family.

Generates a B<page fragment>.

$Id: SpeciesTree.pm,v 1.4 2008-11-04 15:07:56 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

use File::Temp qw( tempfile );
use Data::Dump qw( dump );

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::SpeciesTree';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Retrieves the row from the primary table for the specified entry.

=cut

sub begin : Private {
  my( $this, $c, $entry_arg ) = @_;

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;
    $c->stash->{errorMsg} = 'Invalid Rfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Rfam family accession or ID specified';
  }
  
  #  find out what type of alignment we need, seed, full, etc
  $c->stash->{alnType} = 'seed';
  if ( defined $c->req->param('alnType') ) {
    $c->stash->{alnType} = $c->req->param( 'alnType' ) eq 'full' 
                           ? 'full'
                           : 'seed';
  }
  
  $c->log->debug( 'Family::begin: setting alnType to ' . $c->stash->{alnType} )
    if $c->debug;
  
  # hard-wire the entry type to Rfam
  $c->stash->{entryType} = 'R';
  
  # retrieve the family data
  my $rs = $c->model('RfamDB::Rfam')
             ->search( [ { rfam_acc => $entry },
                         { rfam_id  => $entry } ] );
  my $rfam = $rs->first if defined $rs;
  
  unless ( defined $rfam ) {
    $c->stash->{errorMsg} = 'No valid Rfam family accession or ID found';
    return;
  }

  $c->stash->{rfam}       = $rfam;
  $c->stash->{acc}        = $rfam->rfam_acc;

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

  # see if we can get the release version
  $c->stash->{release_data} = ''; 
  if ( $c->stash->{relData}->rfam_release ) {
    $c->stash->{release_data} = '# Generated from Rfam version ' .
                                $c->stash->{relData}->rfam_release . "\n";
  }
}

#-------------------------------------------------------------------------------
#- public actions --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 alignment  : Local

Returns a Stockholm-format alignment of the sequence snippets for the selected
species.

=cut

sub alignment : Local {
  my( $this, $c ) = @_;
  
  # validate the UUID
  my $jobId = $c->req->param('jobId');
  unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( 'SpeciesTree::alignment: bad job id' ) if $c->debug;

    $c->stash->{errorMsg} = 'Invalid job ID';

    return;
  }

  # retrieve the accessions for that job ID
  my $accession_list = $c->forward( '/utils/retrieve_ids', [ $jobId ] );
  unless( $accession_list ) {
    $c->log->debug( 'SpeciesTree::alignment: could not retrieve accessions list' ) 
      if $c->debug;

    $c->stash->{errorMsg} ||= 'Could not retrieve sequences for that job ID';

    return;
  }

  # retrieve the region sequences
  my @sequences;
  my $input = "# STOCKHOLM 1.0\n";
  ACCESSION: foreach my $accession ( @$accession_list ) {
    my @regions = $c->model('RfamDB::RfamRegFull')
                             ->search( { auto_rfam   => $c->stash->{rfam}->auto_rfam,
                                         rfamseq_acc => $accession },
                                       { prefetch => 'auto_rfamseq' } );

    unless ( scalar @regions ) {
      $c->log->warn( "SpeciesTree::alignment: no sequences for |$accession|" )
        if $c->debug;
      next ACCESSION;
    }

    $c->log->debug( 'SpeciesTree::alignment: got ' . scalar @regions 
                    . " regions for |$accession|" )
      if $c->debug;

    REGION: foreach my $region ( @regions ) {
      $input .=   $region->auto_rfamseq->rfamseq_acc . '/'
                . $region->seq_start . '-'
                . $region->seq_end . ' '
                . $region->sequence . "\n";
    }

  }
  $input .= '#=GC SS_cons ' . $c->stash->{rfam}->reference_structure . "\n"; 
  $input .= '#=GC RF ' . $c->stash->{rfam}->reference_sequence . "\n";
  $input .= "//\n";

  # get a temporary file and filehandle
  my ( $fh, $fn ) = tempfile();
  unless ( $fn and $fh ) {
    $c->log->error( "SpeciesTree::alignment: couldn't open temp file for alignment: $!" );
    return;
  }

  # dump the alignment to that file
  print $fh $input;
  close $fh;

  # build the command for running esl-reformat    
  my $cmd = $this->{eslreformat_binary} . " -u -r --mingap --informat stockholm stockholm $fn";
  $c->log->debug( "SpeciesTree::alignment: running system command: |$cmd|" )
    if $c->debug;

  unless ( open OUTPUT, "$cmd|" ) {    
    $c->log->error( "SpeciesTree::alignment: couldn't run esl-reformat: $!" );
    return;
  }

  # stick the output of esl-reformat back together and remove the temp file
  my $output = join '', <OUTPUT>;
  close OUTPUT;

  unlink $fn;

  # we got a nicely formatted Stockholm alignment; make it a plain text download
  $c->res->content_type( 'text/plain' );
  $c->res->headers->header( 'Content-disposition' => 'attachment; ' .
                            'filename=selected_sequence_accessions.txt' );
  
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
  my( $this, $c ) = @_;
  
  # get the species data for whatever entry we're dealing with
  $c->forward('getData');

  # check that we got data. The getData method will bomb out if the entry hits
  # the limits that are set in the config, provided the "loadTree" flag isn't 
  # set in the stash
  return unless $c->stash->{regions};
  
  $c->log->debug( 'SpeciesTree::buildTree: got '
                  . scalar @{$c->stash->{regions}} .' Rfam regions from sub-class' )
    if $c->debug;

  # we've got data; let's build the tree  
  my $tree     = {};
  my $maxDepth = 0;
  foreach my $region ( @{ $c->stash->{regions} } ) {

    # first, get the species information
    my $species = $region->auto_rfamseq->ncbi_id->species;
    $species =~ s/^(\s+)//g; # trim leading whitespace

    # next, the taxonomy above the species
    my $tax = $region->auto_rfamseq->ncbi_id->tax_string;
    $tax =~ s/\s+//g;
    my @tax = split m/\;/, $tax;

    # add the species onto the end of the taxonomy, so we have it all in
    # one place
    $tax[$#tax] = $species;

    # find the maximum depth for the tree
    $maxDepth = scalar @tax if scalar @tax > $maxDepth;

    # build a hash to describe this branch
    my $speciesData = { acc     => $region->auto_rfamseq->rfamseq_acc,
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

=head2 getDataByType : Private

Retrieves species data for the specified family. 

=cut

sub getDataByType : Private {
  my( $this, $c ) = @_;
  
  # get the species information for the full alignment
  my @regions = $c->model('RfamDB::RfamRegFull')
                  ->search( { 'me.auto_rfam' => $c->stash->{rfam}->auto_rfam },
                            { join     => [ { 'auto_rfamseq' => 'ncbi_id' },
                                            'auto_rfam' ],
                              prefetch => [ 'auto_rfamseq' ] } );

  $c->stash->{regions} = \@regions;

  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @regions . '| full regions' ) if $c->debug;

  # get the species information for the seed alignment
  my @resultsSeed = $c->model('RfamDB::RfamRegSeed')
                      ->search( { 'me.auto_rfam' => $c->stash->{rfam}->auto_rfam } );
  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @resultsSeed . '| seed regions' ) if $c->debug;
                
  # hash the seed info so we can easily look up whether a sequence is 
  # found in the seed alignment
  my %inSeed;
  foreach my $region ( @resultsSeed ) {
    $inSeed{ $region->auto_rfamseq->rfamseq_acc }++;
  }

  $c->stash->{inSeed}  = \%inSeed;  
}

#-------------------------------------------------------------------------------

=head2 countSpecies : Private

Stashes the number of species in the entry.

=cut

sub countSpecies : Private {
  my( $this, $c ) = @_;

  $c->stash->{numSpecies} = $c->stash->{rfam}->number_of_species;

  $c->log->debug( 'SpeciesTree::countSpecies: numSpecies: |'
                  . $c->stash->{numSpecies} . '|' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>
Rob Finn, C<rdf@sanger.ac.uk>
Paul Gardner, C<pg5@sanger.ac.uk>
Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk),
         Paul Gardner, C<pg5@sanger.ac.uk>, Jennifer Daub, C<jd7@sanger.ac.uk>

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
