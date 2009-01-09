
# ImageSet.pm
# jt6 20081215 WTSI
#
# $Id: ImageSet.pm,v 1.2 2009-01-09 12:57:54 jt6 Exp $

=head1 NAME

PfamWeb::ImageSet- a custom L<ImageSet|Bio::Pfam::Drawing::Image::ImageSet> object

=cut

package PfamWeb::ImageSet;

=head1 DESCRIPTION

A custom wrapper around the basic L<ImageSet|Bio::Pfam::Drawing::Image::ImageSet>.
We need to modify the original class to use a custom version of the 
L<Image|Bio::Pfam::Drawing::Image::Image>, which will use a CouchDB store for 
images, rather than a temporary directory.

$Id: ImageSet.pm,v 1.2 2009-01-09 12:57:54 jt6 Exp $

=cut

use strict;
use warnings;

use PfamWeb::Image;

use base 'Bio::Pfam::Drawing::Image::ImageSet';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 image_class

Overrides the same method in the original 
L<ImageSet|Bio::Pfam::Drawing::Image::ImageSet> and returns the name of the 
class that should be used instead of the original 
L<Image|Bio::Pfam::Drawing::Image::Image> when generating Image objects.

=cut

sub image_class { 'PfamWeb::Image' }

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
