=head1 NAME

PfamWeb::Controller::ProteinFeatures - controller to build an independent protein features page

=cut

package PfamWeb::Controller::ProteinFeatures;

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Protein";

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

	# there was an error with user input, e.g. bad ID or
	# accession. Check the config for the location of the error
	# template file
	$c->stash->{template} =
	  $c->config
		->{"View::TT"}
		  ->{VARIABLES}
			->{layouts}
			  ->{ $this->{SECTION} }
				->{errorTemplate};

  } else {

	# no problems; set up the template and let it rip
	$c->stash->{pageType} = $this->{SECTION};
	$c->stash->{template} ||= "pages/graphics.tt";

  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

Andy Jenkinson, C<aj5@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

  1;
