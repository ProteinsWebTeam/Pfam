
# Browse.pm
# jt6 20060717 WTSI
#
# $Id: Browse.pm,v 1.2 2007-03-05 13:23:39 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Proteome::Browse - controller for the proteome "browse"
pages

=cut

package PfamWeb::Controller::Proteome::Browse;

=head1 DESCRIPTION

Generates the "browse" page for proteomes.

$Id: Browse.pm,v 1.2 2007-03-05 13:23:39 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 browse : Path

Retrieves the list of proteomes from the DB and stashes them for the template.

=cut

sub browse : Path {
  my( $this, $c ) = @_;

  my @res = $c->model("PfamDB::Genome_species")
    ->search( { },
				      { order_by => "species ASC"}
				    );

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

}

#-------------------------------------------------------------------------------

=head2 end : Private

Renders the "browse proteomes" page.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if ( scalar @{ $c->error } ) {
  	$c->stash->{template} = "pages/error.tt";
  } else {
  	$c->stash->{pageType} = "proteome";
  	$c->stash->{template} ||= "pages/browseProteomes.tt";
  }

  # and use it
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->clear_errors;

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
