
# pg6 
# 20090930 WTSI
# 
# Help.pm
# $Id: Help.pm,v 1.2 2009-11-13 10:37:39 pg6 Exp $

package iPfamWeb::Controller::Help;

=head1 Name

iPfamWeb::Controller::Help - Controller to build hekp pages for iPfam site.

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::Help';

=head1 Methods

=head2 about : Global

displays an about page containing information about the iPfam project

=cut

sub about: Global {
  my( $self, $c ) = @_;
  
  $c->stash->{ nav } = 'about';
  
  $c->stash->{ template } = 'pages/about.tt';  
  
}




1;
 