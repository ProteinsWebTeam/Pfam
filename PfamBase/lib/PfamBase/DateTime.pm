
# DateTime.pm
# jt6 20120516 WTSI
# 
# A subclass to add the TO_JSON method to DateTime. This is used when we serialise
# a data structure to JSON when submitting a sequence search.

package PfamBase::DateTime;

use Moose;
use namespace::autoclean;

extends 'DateTime';

sub TO_JSON { 
  my $dt = shift;
  return $dt->ymd . ' ' . $dt->hms;
}

=head1 COPYRIGHT

Copyright (c) 2012: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk)

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
