
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# $Id: SpeciesTree.pm,v 1.3 2008-10-23 15:29:47 jt6 Exp $

=head1 NAME

PfamBase::Controller::SpeciesTree - controller to build a representation
of the species tree

=cut

package PfamBase::Controller::SpeciesTree;

=head1 DESCRIPTION

This is the base controller for generating interactive or text represenations
of a species tree. It's intended to be sub-classed to add methods that are
specific to the site that uses it (currently designed to be Pfam or Rfam).

There are limits, set in the configuration, which affect whether the tree is 
actually generated.

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

$Id: SpeciesTree.pm,v 1.3 2008-10-23 15:29:47 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
use Data::UUID;
use JSON;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

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
  
  unless( $id_list =~ m/^([\w]{6,13}\s+)+$/ ) {
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

=head2 json : Local

Generates an JSON rendering of the species tree for the specified entry.

=cut

sub json : Local {
  my( $this, $c ) = @_;

  $c->log->debug( 'SpeciesTree::interactive: generating JSON tree for acc: |'
                  . $c->stash->{acc} . '|' ) if $c->debug;

  $c->forward('buildTree');

  $c->res->body( to_json( $c->stash->{rawTree} ) );
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
        $this->convertToPnh( $c->stash->{rawTree}, \$treeBody );
        $treeBody .= ");\n";
      }
      else {
        $this->convertToText( $c->stash->{rawTree}, \$treeBody );
      }
      
      # add a couple of header lines
      $textTree = '# Species tree for ' . $c->stash->{acc} . "\n";
      $textTree .= $c->stash->{release_data};

      # tack on the actual tree and we're done
      $textTree .= $treeBody;
    }

    # cache the text tree
    $c->cache->set( $cacheKey, $textTree ) unless $ENV{NO_CACHE};
  }
  
  $c->stash->{textTree} = $textTree;
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
  unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
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
  }
  elsif( $c->stash->{entryType} eq 'B' ) {
    $output .= '# for Pfam-B entry ';
  }
  elsif( $c->stash->{entryType} eq 'C' ) {
    $output .= '# for Pfam clan ';
  }
  elsif( $c->stash->{entryType} eq 'R' ) {
    $output .= '# for Rfam family ';
  }
  $output .= $c->stash->{acc} . "\n";
  $output .= $c->stash->{release_data};

  # the accessions themselves
  $output .= join "\n", @$accession_list;
  
  $c->res->body( $output );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getData : Private

Retrieves a count of the number of species and, based on that number, decides
whether we can generate the interactive tree, the text tree, or no tree at all.
If we do want to generate a tree, we forward to a method (which must be supplied
by the sub-classing controller) that will retrieve data according to the type
of entry that we're dealing with, be it some sort of Pfam entry or an Rfam 
entry.

=cut

sub getData : Private {
  my( $this, $c ) = @_;

  $c->forward('countSpecies');

  my $limits = {                                             # set to...
    allowInteractiveLimit => $this->{allowInteractiveLimit}, # 1000
    denyInteractiveLimit  => $this->{denyInteractiveLimit},  # 2000
    denyAllLimit          => $this->{denyAllLimit}           # 3000
  };
  $c->stash->{limits} = $limits;

  if( $c->stash->{numSpecies} > $this->{denyAllLimit} ) {
    # this is a hard limit
    $c->log->debug( 'SpeciesTree::getData: too many families ('
                    . $c->stash->{numSpecies} . '); hit "denyAll" limit' )
      if $c->debug;
  }
  elsif( $c->stash->{numSpecies} > $this->{denyInteractiveLimit} and

    # this is a soft limit, overridden by the "loadTree" flag
      not $c->stash->{loadTree} ) {
    $c->log->debug( 'SpeciesTree::getData: too many families ('
                    . $c->stash->{numSpecies} . ', loadTree = '
                    . ($c->stash->{loadTree} || 0) . '); hit "denyInteractive" limit' )
      if $c->debug;
  }
  elsif( $c->stash->{numSpecies} > $this->{allowInteractiveLimit} and

    # this is another soft limit, overridden by the "loadTree" flag
      not $c->stash->{loadTree} ) {
    $c->log->debug( 'SpeciesTree::getData: too many families ('
                    . $c->stash->{numSpecies} . ', loadTree = '
                    . ($c->stash->{loadTree} || 0) . '); hit "allowInteractive" limit' )
      if $c->debug;
  }
  else {
  
    # having made sure there aren't too many families, we'll go ahead and 
    # retrieve the data
    $c->forward( 'getDataByType' );
  }  

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
  my( $this, $tree, $branch ) = @_;

  # shift one level off the taxonomy array
  if( my $node = shift @{ $branch->{tax} } ) {

    # count the number of unique sequences
    # count the number of unique species
    # count the number of regions
    # flag this node if it's in the seed   

    $tree->{branches}->{$node}->{sequences}->{ $branch->{acc}     }++;
    $tree->{branches}->{$node}->{species  }->{ $branch->{species} }++;
    $tree->{branches}->{$node}->{frequency}++; 
    $tree->{branches}->{$node}->{inSeed   }++
      if ( $branch->{inSeed} and $node eq $branch->{species} );
 
    # carry on down the tree
    $this->addBranch( $tree->{branches}->{$node}, $branch );
  }
}

#-------------------------------------------------------------------------------

=head2 convertToText

Walks the tree and generates a plain-text representation.

Not a Catalyst controller. Called as a regular method because it's called 
recursively when walking the tree.

=cut

sub convertToText {
  my ( $this, $tree, $ptrOutput, $indent, $flag1, $flag2 ) = @_;

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
                       
        $this->convertToText( $tree->{branches}->{$node}, 
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
  my ( $this, $tree, $ptrOutput, $indent, $flag1, $flag2 ) = @_;

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

        $this->convertToPnh( $tree->{branches}->{$node}, 
                             $ptrOutput, 
                             $indent, 
                             $flag1, 
                             $flag2 );

        $$ptrOutput .= $indent . ") ";
        $$ptrOutput .= $node_text . ':' . $tree->{branches}->{$node}->{frequency} . "\n";
      } 

      # no; just add this node
      else {
        $$ptrOutput .= $node_text . ':' . $tree->{branches}->{$node}->{frequency};
        $$ptrOutput .= ',' if $i <= scalar @keys - 2;
        $$ptrOutput .= "\n";
      }

      $nodeCount++;
    }
  }

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
