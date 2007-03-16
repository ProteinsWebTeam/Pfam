
# $Id: Dead_clans.pm,v 1.4 2007-03-16 11:25:16 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Dead_clans;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "dead_clans" );

#Add the columns
__PACKAGE__->add_columns(qw/clan_acc clan_description clan_membership comment forward_to user killed/);

#Set up the primary key
__PACKAGE__->set_primary_key("clan_acc");

#In theory the forward_to could join onto self or clans...... -todo

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

