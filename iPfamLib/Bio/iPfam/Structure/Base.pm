
# Base.pm
# rdf/jt6 20080129 WTSI
#
# $Id: Base.pm,v 1.1 2008-01-31 17:10:02 jt6 Exp $

=head1 NAME

Base - store details of a nucleotide

=cut

package Bio::iPfam::Structure::Base;

=head1 SYNOPSIS


=head1 DESCRIPTION


$Id: Base.pm,v 1.1 2008-01-31 17:10:02 jt6 Exp $

=cut

use strict;
use warnings;

use Carp;

use Bio::iPfam::Structure::Atom;

use base qw( Bio::iPfam::Structure::Monomer );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( 
                             ) );

# logging ! Just gotta do it.
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 COPYRIGHT

Copyright (c) 2008: Genome Research Ltd.

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
