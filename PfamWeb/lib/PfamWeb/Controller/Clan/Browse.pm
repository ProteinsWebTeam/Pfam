
# Browse.pm
# jt6 20060821 WTSI
#
# $Id: Browse.pm,v 1.2 2006-09-22 10:47:02 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure - controller for the clan "browse"

=cut

package PfamWeb::Controller::Clan::Browse;

use strict;
use warnings;

use base "Catalyst::Controller";


=head1 DESCRIPTION

Just a simple wrapper around a template that displays a "browse" page
for clans. Since there are relatively few clans we can put them all on
one page, rather than splitting them up like the families.

Generates a B<full page>.

$Id: Browse.pm,v 1.2 2006-09-22 10:47:02 jt6 Exp $

=cut

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 browse : Path

Picks up a URL like C<http://localhost:3000/clan/browse/>. Retrieves a
list of all clan IDs and hands off to the template.

=cut

sub browse : Path {
  my( $this, $c ) = @_;

  my @res = $c->model("PfamDB::Clans")->search( {},
												{ order_by => "clan_id ASC" }
											  );

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

}

#-------------------------------------------------------------------------------

=head2 end : Private

Renders the clan browse pages.

=cut

# render the page

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{template} = "pages/error.tt";
  } else {
	$c->stash->{pageType} = "clan";
	$c->stash->{template} = "pages/browseClans.tt";
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
