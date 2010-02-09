
# Region.pm
# rdf/jt6 20080125 WTSI
#
# $Id: Region.pm,v 1.1 2008-03-03 13:16:14 rdf Exp $

=head1 NAME

Chain - store chain details

=cut

package Bio::iPfam::Structure::Region;

=head1 SYNOPSIS

=head1 DESCRIPTION

The region object contains the information on sub part of a chain found within the structure.  

$Id: Region.pm,v 1.1 2008-03-03 13:16:14 rdf Exp $

=cut

use strict;
use warnings;

use base qw( Class::Accessor::Fast );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( id acc start end uniqueID ) );
__PACKAGE__->mk_ro_accessors( qw( log ) );
# logging ! Log early, log often
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

