
# DasProxy.pm
# jt6 20070307 WTSI
#
# $Id: DasProxy.pm,v 1.2 2007-03-15 14:06:14 jt6 Exp $

=head1 NAME

PfamWeb::Controller::DasProxy - a simple proxy server for DAS calls

=cut

package PfamWeb::Controller::DasProxy;

=head1 DESCRIPTION

A simple implementation of a proxy server for DAS calls. The aim of this is to
allow javascript methods from our pages to access DAS servers outside of our
domain.

$Id: DasProxy.pm,v 1.2 2007-03-15 14:06:14 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Private

Default method stub.

=cut

sub default : Private {
  my ( $this, $c ) = @_;

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

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
