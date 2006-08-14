
# GetPdbFile.pm
# jt6 20060731 WTSI
#
# Controller to return a PDB file
#
# $Id: GetPdbFile.pm,v 1.1 2006-08-14 10:47:20 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::GetPdbFile - serve a PDB file

=cut

package PfamWeb::Controller::Structure::GetPdbFile;

=head1 DESCRIPTION

Retrieves a specified PDB file from the model and serves it up with
the appropriate MIME type (chemical/x-pdb). Relies on
L<Structure::begin|PfamWeb::Controller::Structure/"begin : Private"> to
extract the PDB ID from the URL.

Generates a B<flat file>, MIME type C<chemical/x-pdb>.

$Id: GetPdbFile.pm,v 1.1 2006-08-14 10:47:20 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

Pick up http://localhost:3000/structure/getpdbfile?id=1abc and similar
URLs. Retrieves the file from the model. Currently the model uses the
WTSI pfetch server only.

=cut

sub default : Path {
  my( $this, $c, $pdbId ) = @_;

  return unless defined $c->stash->{pdbId};

  $c->stash->{pdbFile} =
	PfamWeb::Model::Pfetch->retrieve( { "--pdb" => $c->stash->{pdbId} } );

}

#-------------------------------------------------------------------------------

=head2 end : Private

Push the file to the response

=cut

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pdb object in the stash
  return 0 unless defined $c->stash->{pdbFile};

  $c->res->content_type( "chemical/x-pdb" );
  foreach ( @{$c->stash->{pdbFile}} ) {
	$c->res->write( $_ );
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
