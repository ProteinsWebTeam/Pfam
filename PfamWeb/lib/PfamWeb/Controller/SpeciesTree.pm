
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# $Id: SpeciesTree.pm,v 1.16 2008-03-03 16:48:25 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::SpeciesTree - controller to build a representation
of the species tree

=cut

package PfamWeb::Controller::SpeciesTree;

=head1 DESCRIPTION

This controller generates either an interactive or a text representation of the
species tree for a Pfam-A, a Pfam-B or a clan. There are limits, set in the
configuration, which affect whether the tree is actually generated.

=over 4

=item 

small trees (numSpecies < allowInteractiveLimit) are generated as 
interactive trees without complaint

=item 

medium trees (numSpecies < denyInteractiveLimit) can be time consuming 
to generate as interactive trees, so we refuse to generate them unless we have 
a flag set in the request

=item 

large trees (numSpecies < denyAllLimit) are too big for the interactive 
view, so we refuse to generate the interactive tree but will generate a text 
representation

=item 

very large trees (numSpecies > denyAllLimit) can't be touched, so we 
refuse to generate either interactive or text trees

=back

Generates a B<page fragment>.

$Id: SpeciesTree.pm,v 1.16 2008-03-03 16:48:25 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
use Data::UUID;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Decides what kind of accession we've got and retrieves the row from the primary
table for that entry.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # do we have an accession ?
  return unless $c->req->param('acc');
  
  # yes; what type of accession is it ?
  if( $c->req->param('acc') =~ m/^(PF\d{5})(\.\d+)?$/i ) {

    # pfam A
    $c->stash->{acc}  = $1;
    $c->stash->{entryType} = 'A';
    $c->log->debug( 'SpeciesTree::begin: found Pfam A accession |' 
                    . $c->stash->{acc} . '|' )
      if $c->debug;

    # make sure we can retrieve data for that entry
    $c->stash->{entry} = $c->model('PfamDB::Pfam')
                           ->find( { pfamA_acc => $c->stash->{acc} } );

  } elsif( $c->req->param('acc') =~ m/^(PB\d{6})$/i ) {

    # pfam B
    $c->stash->{acc}  = $1;
    $c->stash->{entryType} = 'B';
    $c->log->debug( 'SpeciesTree::begin: found Pfam B accession |'
                    . $c->stash->{acc} . '|' )
      if $c->debug;      

    $c->stash->{entry} = $c->model('PfamDB::PfamB')
                           ->find( { pfamB_acc => $c->stash->{acc} } );

  } elsif( $c->req->param('acc') =~ m/^(CL\d{4})$/i ) {

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
  unless( $c->stash->{acc} and $c->stash->{entry} ) {
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

=head2 end : ActionClass

Renders the tree. If there's a text representation of the tree in the stash,
it's returned directly in the response. If not, we hand off to the view to 
render whatever template was previously specified in the stash.

=cut

sub end : ActionClass( 'RenderView' ) {
  my( $this, $c ) = @_;

  if( not $c->stash->{isIE} and 
      defined $c->stash->{textTree} ) {

    $c->log->debug( 'SpeciesTree::end: NOT in IE and we got a text tree' )
      if $c->debug;

    # we got a text tree; make it a plain text download
    $c->res->content_type( 'text/plain' );
    $c->res->headers->header( 'Content-disposition' => 'attachment; filename='
                              . $c->stash->{acc} . '_tree.txt' );
  
    $c->res->body( $c->stash->{textTree} );
  }
  
  # hand off to the template
  $c->stash->{template} ||= 'components/speciesTree.tt';

}

#-------------------------------------------------------------------------------
#- public actions --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 store_ids : Local

Stores a set of sequence accessions in a DB table and returns a unique ID for
the set. Expects the sequence accessions to be stored as an escaped, semi-colon-
separated list. The list is unescaped before being stored.

This is intended to be called from an AJAX requests, so we hand back
raw information:

=over 4

=item 

If everything goes well, we hand back the ID as a simple, plain text string. 

=item 

If the sequence list has illegal characters we set the response status to 
400 and set the body to "Bad request", although we don't really expect it to 
be used.

=item 

If there's a problem adding the row to the DB, we set the response status
to 500 and put "Failed" into the response body.

=back

The DB table is intended to be used as a staging post for features of the
species tree that pop up a new window to show things like sequence alignments or
domain graphics.

=cut

sub store_ids : Local {
  my( $this, $c ) = @_;
  
  my $id_list = uri_unescape( $c->req->param('ids') ); 
  
  unless( $id_list =~ m/^([\w]{6}\s+)+$/ ) {
    $c->log->debug( 'SpeciesTree::store_ids: not a valid ID string' )
      if $c->debug;

    $c->res->body( 'Bad request' );
    $c->res->status( 400 );
    return;
  }
  
  $c->log->debug( 'SpeciesTree::store_ids: got some valid ids' ) if $c->debug;
  
  # build an ID for this set of IDs
  my $job_id = Data::UUID->new()->create_str();
  
  # add it to the DB
  my $row;
  eval {
    # we use "update_or_create" because we don't really care if this ID has 
    # been used before; it's not important enough to spend time making sure 
    # it's unique
    $row = $c->model('WebUser::Species_collection')
             ->update_or_create( { job_id => $job_id,
                                   id_list => $id_list } );
  };
  if( $@ ) {
    # oops...
    $c->log->error( "SpeciesTree::store_ids: error from query: |$@|" )
      if $c->debug;

    $c->res->body( 'Failed' );
    $c->res->status( 500 );
    return;
  }

  # the row was successfully added to the table. Hand back the job ID as the 
  # response
  $c->res->body( $job_id );
  $c->res->content_type( 'text/plain' );    

}

#-------------------------------------------------------------------------------

=head2 interactive : Local

Generates an interactive tree for the specified entry.

=cut

sub interactive : Local {
  my( $this, $c ) = @_;

  $c->log->debug( 'SpeciesTree::interactive: rendering the species tree for acc: |'
                  . $c->stash->{acc} . '|' ) if $c->debug;

  $c->forward('buildTree');

  # point to the template that will generate the javascript that
  # builds the tree in the client
  $c->stash->{template} = 'components/speciesTree.tt';
  
  # cache the output of the template for one week
  #$c->cache_page( 604800 );
}

#-------------------------------------------------------------------------------

=head2 text : Local

Generates a text representation of the species tree for the specified entry.

=cut

sub text : Local {
  my( $this, $c ) = @_;

  # see if we can simply retrieve the tree from cache first
  my $cacheKey = 'speciesTree'
                 . $c->stash->{acc}
                 . $c->stash->{entryType};

  my $textTree;
  if( $textTree = $c->cache->get( $cacheKey ) ) {
    $c->log->debug( 'SpeciesTree::text: retrieved the text tree from cache' )
      if $c->debug;
  } else {
    $c->log->debug( 'SpeciesTree::text: no cached text tree; generating' )
      if $c->debug;

    # actually retrieve the data and build the in-memory representation
    $c->forward('buildTree');

    # did we build a tree ? If the entry has too many species, the getData
    # method will refuse to retrieve the raw data, even after we've set the 
    # "loadTree" flag, so we don't find anything in the stash 
    if( $c->stash->{rawTree} ) {

      # yes; convert the tree to plain text
      my $treeBody;
      if ( $c->req->param( 'pnh' ) ) {
        $treeBody  = "(\n";
        convertToPnh( $c->stash->{rawTree}, \$treeBody );
        $treeBody .= ");\n";
      }
      else {
        convertToText( $c->stash->{rawTree}, \$treeBody );
      }
      
      # add a couple of header lines
      $textTree = '# Species tree for ' . $c->stash->{acc} . "\n";
    
      # see if we can get the release version
      if( my $release = $c->stash->{relData}->pfam_release ) {
        $textTree .= "# Generated from Pfam version $release\n";
      }

      # tack on the actual tree and we're done
      $textTree .= $treeBody;
    }

    # cache the text tree
    $c->cache->set( $cacheKey, $textTree );
  }
  
  $c->stash->{textTree} = $textTree;
}

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
  if( length( $jobId ) != 36 or $jobId !~ /^[A-F0-9\-]+$/ ) {
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
  
  $c->log->debug( 'SpeciesTree::graphics: rendering selected seqs as Pfam graphics' )
    if $c->debug;
  $c->stash->{template} = 'components/tools/seqViewGraphic.tt';
}

#-------------------------------------------------------------------------------

=head2 accessions : Local

Returns the sequence accessions from selected nodes in the species tree as a 
plain text file.

=cut

sub accessions : Local {
  my( $this, $c ) = @_;
  
  # validate the UUID
  my $jobId = $c->req->param('jobId');
  if( length( $jobId ) != 36 or $jobId !~ /^[A-F0-9\-]+$/ ) {
    $c->log->debug( 'SpeciesTree::accessions: bad job id' ) if $c->debug;
    $c->stash->{errorMsg} = 'Invalid job ID';
    return;
  }

  # retrieve the accessions for that job ID
  my $accession_list = $c->forward( '/utils/retrieve_ids', [ $jobId ] );
  unless( $accession_list ) {
    $c->stash->{errorMsg} ||= 'Could not retrieve sequences for that job ID';
    return;
  }

  # we got a list of accessions; make it a plain text download
  $c->res->content_type( 'text/plain' );
  $c->res->headers->header( 'Content-disposition' => 'attachment; ' .
                            'filename=selected_sequence_accessions.txt' );

  # format the list nicely and drop it straight into the response
  my $output = "# Sequence accessions for selected nodes from the species tree\n";
  if( $c->stash->{entryType} eq 'A' ) {
    $output .= '# for Pfam-A entry ';
  } elsif( $c->stash->{entryType} eq 'B' ) {
    $output .= '# for Pfam-B entry ';
  } elsif( $c->stash->{entryType} eq 'C' ) {
    $output .= '# for Pfam clan ';
  }
  $output .= $c->stash->{acc} . "\n";

  # see if we can get the release version
  if( my $release = $c->stash->{relData}->pfam_release ) {
    $output .= "# Generated from Pfam version $release\n";
  }

  # the accessions themselves
  $output .= join "\n", @$accession_list;
  
  $c->res->body( $output );
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
  if( length( $jobId ) != 36 or $jobId !~ /^[A-F0-9\-]+$/ ) {
    $c->log->debug( 'SpeciesTree::sequences: bad job id' ) if $c->debug;
    $c->stash->{errorMsg} = 'Invalid job ID';
    return;
  }

  # retrieve the sequences
  my $fasta = $c->forward( '/utils/get_sequences', 
                           [ $jobId, $c->stash->{entry} ] );
  
  # make sure we got something...
  unless( length $fasta ) {
    $c->log->debug( 'SpeciesTree::sequences: failed to get a FASTA sequence' );
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

  # see if we can get the release version
  if( my $release = $c->stash->{relData}->pfam_release ) {
    $output .= "# Generated from Pfam version $release\n";
  }

  # the accessions themselves
  $output .= $fasta;
  
  $c->res->body( $output );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 buildTree : Private

Tries to retrieve the pre-built tree from cache and, if it can't be simply
retrieved, forwards to C<_buildTree> which will actually construct it from
the raw data.

=cut

sub buildTree : Private {
  my( $this, $c ) = @_;
  
  # get the species data for whatever entry we're dealing with
  $c->forward('getData');

  # check that we got data. The getData method will bomb out if the entry hits
  # the limits that are set in the config, provided the "loadTree" flag isn't 
  # set in the stash
  return unless $c->stash->{regions};

  # we've got data; let's build the tree  
  my $tree     = {};
  my $maxDepth = 0;
  foreach my $region ( @{ $c->stash->{regions} } ) {

    # first, get the species information
    my $species = $region->species;
    chop($species);          # remove trailing full stop
    $species =~ s/^(\s+)//g; # trim leading whitespace

    # next, the taxonomy above the species
    my $tax = $region->taxonomy;
    $tax =~ s/\s+//g;
    my @tax = split m/\;/, $tax;

    # add the species onto the end of the taxonomy, so we have it all in
    # one place
    $tax[$#tax] = $species;

    # find the maximum depth for the tree
    $maxDepth = scalar @tax if scalar @tax > $maxDepth;

    # build a hash to describe this branch
    my $speciesData = { acc     => $region->pfamseq_acc,
                        species => $species,
                        tax     => \@tax };

    # flag the node if it's in the seed alignment
    $speciesData->{inSeed}++
      if $c->stash->{inSeed}->{$region->pfamseq_acc};
    
    # add this branch to the tree
    addBranch( $tree, $speciesData );
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

sub getData : Private {
  my( $this, $c ) = @_;

  $c->forward('countSpecies');

  my $limits = {                                             # set to...
    allowInteractiveLimit => $this->{allowInteractiveLimit}, # 1000
    denyInteractiveLimit  => $this->{denyInteractiveLimit},  # 2000
    denyAllLimit          => $this->{denyAllLimit}           # 3000
  };

  # this is a hard limit
  if( $c->stash->{numSpecies} > $this->{denyAllLimit} ) {
    $c->log->debug( 'SpeciesTree::getData: too many families ('
                    . $c->stash->{numSpecies} . '); hit "denyAll" limit' )
      if $c->debug;
    $c->stash->{limits} = $limits;
    return;
  }

  # this is a soft limit, overridden by the "loadTree" flag
  if( $c->stash->{numSpecies} > $this->{denyInteractiveLimit} and
      not $c->stash->{loadTree} ) {
    $c->log->debug( 'SpeciesTree::getData: too many families ('
                    . $c->stash->{numSpecies} . ', loadTree = '
                    . ($c->stash->{loadTree} || 0) . '); hit "denyInteractive" limit' )
      if $c->debug;
    $c->stash->{limits} = $limits;
    return;
  }

  # this is another soft limit, overridden by the "loadTree" flag
  if( $c->stash->{numSpecies} > $this->{allowInteractiveLimit} and
      not $c->stash->{loadTree} ) {
    $c->log->debug( 'SpeciesTree::getData: too many families ('
                    . $c->stash->{numSpecies} . ', loadTree = '
                    . ($c->stash->{loadTree} || 0) . '); hit "allowInteractive" limit' )
      if $c->debug;
    $c->stash->{limits} = $limits;
    return;
  }

  # having made sure there aren't too many families, we'll go ahead and 
  # retrieve the data
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
  my( $this, $c ) = @_;

  # for Pfam-As or clans we can just look up the number of species in the 
  # main table, via the "entry" that was put in the stash by C<begin>, but
  # for Pfam-Bs we'll actually have to count the number of species

  if( $c->stash->{entryType} eq 'B' ) {
    
    my @regions = $c->model('PfamDB::PfamB_reg')
                    ->search( { auto_pfamB => $c->stash->{entry}->auto_pfamB },
                              { join       => [ qw( pfamseq ) ],
                                prefetch   => [ qw( pfamseq ) ] } );

    # as we're retrieving them here anyway, stash the regions, so we don't need
    # to get them again later
    $c->stash->{regions} = \@regions;

    my %species_unique = map {$_->species => 1} @regions;
    $c->stash->{numSpecies} = scalar( keys %species_unique );

  } else {
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
  my @regions = $c->model('PfamDB::PfamA_reg_full')
                  ->search( { 'pfamA.pfamA_acc' => $c->stash->{acc},
                              'in_full'         => 1 },
                            { join              => [ qw( pfamseq pfamA ) ],
                              prefetch          => [ qw( pfamseq ) ] } );

  $c->stash->{regions} = \@regions;

  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @regions . '| full regions' ) if $c->debug;

  # get the species information for the seed alignment
  my @resultsSeed = $c->model('PfamDB::PfamA_reg_seed')
                      ->search( { 'pfamA.pfamA_acc' => $c->stash->{acc} },
                                { join              => [ qw( pfamseq pfamA ) ],
                                  prefetch          => [ qw( pfamseq ) ] } );
  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @resultsSeed . '| seed regions' ) if $c->debug;
                
  # hash the seed info so we can easily look up whether a sequence is 
  # found in the seed alignment
  my %inSeed;
  foreach my $region ( @resultsSeed ) {
    $inSeed{ $region->pfamseq_acc}++;
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
  my( $this, $c ) = @_;
  
  # get the species information for the full alignment for each clan member. 
  # This probably could be done in one query, but this is going to be quicker
  # (I think...)
  my @auto_pfamAs = $c->model('PfamDB::Clan_membership')
                      ->search( { 'clans.clan_acc' => $c->stash->{acc} },
                                { join             => [ qw( clans ) ] } );

  my(@allRegions, @regions );
  foreach my $auto_pfamA ( @auto_pfamAs ) {
    
    @regions = $c->model('PfamDB::PfamA_reg_full')
                 ->search( { 'auto_pfamA' => $auto_pfamA->auto_pfamA,
                             'in_full'     => 1 },
                             { join              => [ qw( pfamseq ) ],
                               prefetch          => [ qw( pfamseq ) ] } );

    push @allRegions, @regions;
  }
  
  $c->stash->{regions} = \@allRegions;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 addBranch

Add a new branch to the tree.

Not a Catalyst controller. Called as a regular method because it's called 
recursively when building the tree.

=cut

sub addBranch {
  my( $tree, $branch ) = @_;

  # shift one level off the taxonomy array
  if( my $node = shift @{ $branch->{tax} } ) {

    # count the number of unique sequences
    # count the number of unique species
    # count the number of regions
    # flag this node if it's in the seed   

    $tree->{branches}->{$node}->{sequences}->{ $branch->{acc} }++;
    $tree->{branches}->{$node}->{species  }->{ $branch->{species} }++;
    $tree->{branches}->{$node}->{frequency}++; 
    $tree->{branches}->{$node}->{inSeed   }++
      if( $branch->{inSeed} and $node eq $branch->{species} );
 
    # carry on down the tree
    addBranch( $tree->{branches}->{$node}, $branch );
  }
}

#-------------------------------------------------------------------------------

=head2 convertToText

Walks the tree and generates a plain-text representation.

Not a Catalyst controller. Called as a regular method because it's called 
recursively when walking the tree.

=cut

sub convertToText {
  my ($tree, $ptrOutput, $indent, $flag1, $flag2 ) = @_;

  # add an increment, either a bar or whitespace
  $indent .= ( not $flag1 and $flag2 ) ? '|  ' : '   ';

  # we're done unless there are more branches to walk down 
  my @keys = keys %{ $tree->{branches} };
  if( my $numNodes = scalar @keys ) {

    my $nodeCount = 1;

    foreach my $node ( @keys ){
      $flag1 = ( $numNodes != $nodeCount ) ?  0 : 1;
      $$ptrOutput .= $indent . "|\n";
      $$ptrOutput .= $indent . '+--';

      # are there branches under this one ?
      if( $tree->{branches}->{$node}->{branches} ) {
        # yes; add this node and then keep going down
        $flag2 = $nodeCount eq $numNodes ? 0 : 1;
        $$ptrOutput .= $node . ' (' . $tree->{branches}->{$node}->{frequency} . ")\n";
                       
        convertToText( $tree->{branches}->{$node}, 
                       $ptrOutput, 
                       $indent, 
                       $flag1, 
                       $flag2 );

      } else {
        # no; just add this node
        $$ptrOutput .= $node . ' (' . $tree->{branches}->{$node}->{frequency} . ")\n";
      }
      $nodeCount++;
    }
  }
}

#-------------------------------------------------------------------------------

=head2 convertToPnh

Walks the tree and generates a plain-text representation.

Not a Catalyst controller. Called as a regular method because it's called 
recursively when walking the tree.

=cut

sub convertToPnh {
  my ($tree, $ptrOutput, $indent, $flag1, $flag2 ) = @_;

  # add an increment, either a bar or whitespace
  #$indent .= ( not $flag1 and $flag2 ) ? '|  ' : '   ';
  $indent .= '  ';

  # we're done unless there are more branches to walk down 
  my @keys = keys %{ $tree->{branches} };
  if( my $numNodes = scalar @keys ) {

    my $nodeCount = 1;

    for ( my $i = 0; $i < scalar @keys; $i++ ) {
      my $node = $keys[$i];

      my $node_text = $node;
      $node_text =~ tr/()/[]/;

      $flag1 = ( $numNodes != $nodeCount ) ?  0 : 1;
      $$ptrOutput .= $indent;

      # are there branches under this one ?
      if( $tree->{branches}->{$node}->{branches} ) {
        # yes; add this node and then keep going down
        $flag2 = $nodeCount eq $numNodes ? 0 : 1;
        
        $$ptrOutput .= "(\n";

        convertToPnh( $tree->{branches}->{$node}, 
                      $ptrOutput, 
                      $indent, 
                      $flag1, 
                      $flag2 );

        $$ptrOutput .= $indent . ") ";
        $$ptrOutput .= $node_text . ':' . $tree->{branches}->{$node}->{frequency} . "\n";
#        if ( $i <= scalar @keys - 1 ) {
#          $$ptrOutput .= ",\n";
#        }
#        else {
#          $$ptrOutput .= "\n";
#        }
        
      } 

      # no; just add this node
      else {
        $$ptrOutput .= $node_text . ':' . $tree->{branches}->{$node}->{frequency};
        if ( $i <= scalar @keys - 2 ) {
          $$ptrOutput .= ",\n";
        }
        else {
          $$ptrOutput .= "\n";
        }
      }

      $nodeCount++;
    }
  }

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
