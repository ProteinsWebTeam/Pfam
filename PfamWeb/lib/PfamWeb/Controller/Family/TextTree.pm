
# TextTree.pm
# jt6 20061011 WTSI
#
# $Id: TextTree.pm,v 1.2 2007-03-15 14:06:10 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::TextTree - output a plain text
representation of the species tree

=cut

package PfamWeb::Controller::Family::TextTree;

=head1 DESCRIPTION

Controller to output a species tree as a simple text file.

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base "PfamWeb::Controller::Family::SpeciesTree";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getTextTree : Path

Retrieves the data structure for the tree from the stash and passes it
off to a private method that formats it as plain text. Adds comments
to give some details of the source of the data.

=cut

sub getTextTree : Path {
  my( $this, $c ) = @_;

  # convert the tree to plain text
  my $textTree;
  convert_to_text( $c->stash->{rawTree}, \$textTree );

  # append a couple of header lines
  $c->stash->{textTree} =
	  "# Species tree for " . $c->stash->{pfam}->pfamA_id
	. " (" . $c->stash->{pfam}->pfamA_acc . ")\n"
	. "# Generated from Pfam version "
	. $c->model( "PfamDB::Version" )->first()->pfam_release . "\n";
  $c->stash->{textTree} .= $textTree;

}

#-------------------------------------------------------------------------------

=head2 end : Private

Just dumps the text tree to the response stream.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  # plain text
  $c->res->content_type( "text/plain" );

  # make it a download
  $c->res->headers->header( "Content-disposition" => "attachment; filename="
 							. $c->stash->{pfam}->pfamA_id . "_tree.txt" );

  # dump out the tree
  $c->res->write( $c->stash->{textTree} );

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# walk the tree and convert it to plain text

sub convert_to_text {
 my ($tree, $ptrOutput, $indent, $flag1, $flag2) = @_;
 my $isNotRoot = ($$ptrOutput eq '')? undef:1;
 $indent .= (!$flag1 && $flag2)? "|   ":"    ";
 my $lastNode = 1 if (! keys(%{$$tree{branches}}));
 if (!$lastNode) {
   my $numNodes = scalar(keys(%{$$tree{branches}}));
   my $nodeCount = 1;
   foreach my $nodeId (keys(%{$$tree{branches}})){
     $flag1 = ($numNodes != $nodeCount) ? 0:1;
     $$ptrOutput .= $indent."|\n";
     $$ptrOutput .= $indent."+---";
     if ($$tree{branches}{$nodeId}{branches}) {
       $flag2 = 1 if($nodeCount ne $numNodes);
       $$ptrOutput .= $nodeId."(".$$tree{branches}{$nodeId}{frequency}.")"."\n";
       convert_to_text($$tree{branches}{$nodeId}, $ptrOutput, $indent, $flag1, $flag2);
     }else{
       $$ptrOutput .= $nodeId."(".$$tree{branches}{$nodeId}{frequency}.")"."\n";
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
