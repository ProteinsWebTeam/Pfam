
# TextTree.pm
# jt6 20061011 WTSI
#
# Controller to output a species tree as a simple text file.
#
# $Id: TextTree.pm,v 1.1 2006-10-18 12:16:57 jt6 Exp $

package PfamWeb::Controller::Family::TextTree;

use strict;
use warnings;

use Data::Dump qw( dump );

use base "PfamWeb::Controller::Family::SpeciesTree";

#-------------------------------------------------------------------------------

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

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
