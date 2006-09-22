
# Section.pm
# jt6 20060922 WTSI
#
# $Id: Section.pm,v 1.1 2006-09-22 10:43:53 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Section - base class for section pages, e.g. Family

=cut

package PfamWeb::Controller::Section;

=head1 DESCRIPTION

This is the base class for the various "section" controllers, such as
Family, Clan, etc. It contains an empty C<default> method that just
captures the URL, and an C<end> that catches errors from earlier in
the process and reports them. If there are no errors it renders the
view that's for the section, e.g. "family.tt", etc.

$Id: Section.pm,v 1.1 2006-09-22 10:43:53 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head2 default : Path

An empty action to capture URLs like

=over

=item http://localhost:3000/SECTION?id=ID

=back

=cut

sub default : Path {

  # empty; just here to capture the URL

}

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 end : Private

Hands off to the tab layout template or catches any errors that were
generated earlier

=cut

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if( scalar @{ $c->error } ) {

	# there was a system error...
	$c->stash->{template} = "components/systemError.tt";

	# report the error as a broken internal link
	$c->forward( "/reportError" );

	# clear the errors before we finish up
	$c->clear_errors;

  } elsif ( $c->stash->{errorMsg} ) {

	# there was an error with user input, e.g. bad ID or accession
	$c->stash->{template} = "components/blocks/" . $this->{SECTION} . "/error.tt";

  } else {

	# no problems; set up the template and let it rip
	$c->stash->{pageType} = $this->{SECTION};
	$c->stash->{template} ||= "pages/layout.tt";

  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

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
