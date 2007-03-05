# Ligand.pm
# rdf 20060818 WTSI
#
# $Id: Ligand.pm,v 1.4 2007-03-05 13:23:39 jt6 Exp $
# Controller to build the main Ligand page.
#

=head1 NAME

PfamWeb::Controller::Int::Ligand -  controller to build the main iPfam ligand
page.

=cut

package PfamWeb::Controller::Int::Ligand;

=head1 DESCRIPTION

Controller to build the main iPfam ligand page.

$Id: Ligand.pm,v 1.4 2007-03-05 13:23:39 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Section";

# define the name of the section...
__PACKAGE__->config( SECTION => "ligand" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 generateLigandIntSum : Path

Retrieves a Ligand based on the supplied three-letter-code.

=cut

sub generateLigandIntSum : Path {
  my( $this, $c ) = @_;

  if( defined $c->req->param("code") ) {
    $c->req->param("code") =~ m/^(\w{3})$/i;
    $c->stash->{ligand} = $c->model("PfamDB::Ligands")->find( { three_letter_code => $1 } );
    $c->log->debug("Int::Ligand::generateLigandIntSum: Got ligand data for:".$c->stash->{ligand}->name.":");
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
