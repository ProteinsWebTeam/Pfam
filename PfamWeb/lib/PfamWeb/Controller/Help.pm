
# Help.pm
# jt6 20060925 WTSI
#
# $Id: Help.pm,v 1.2 2007-01-31 14:04:00 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Help - controller to build the help pages

=cut

package PfamWeb::Controller::Help;

=head1 DESCRIPTION



Generates a B<tabbed page>.

$Id: Help.pm,v 1.2 2007-01-31 14:04:00 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "help" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 index : Private

The default help page

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # set the page to be cached for two weeks
  $c->cache_page( 1209600 );

  # otherwise empty... just capture the index URL

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# none

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
