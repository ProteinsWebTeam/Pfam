
# ProteinFeatures.pm
# aj5 20060208 WTSI
#
# $Id: ProteinFeatures.pm,v 1.2 2007-03-15 14:06:13 jt6 Exp $

=head1 NAME

PfamWeb::Controller::ProteinFeatures - controller to build an
independent protein features page

=cut

package PfamWeb::Controller::ProteinFeatures;

=head1 DESCRIPTION

Builds an independent protein features page.

$Id: ProteinFeatures.pm,v 1.2 2007-03-15 14:06:13 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Protein";

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

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk),
         Andy Jenkinson (aj5@sanger.ac.uk)

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
