
# Table definition for functional_similarity table. 
#
# $Id: Funshift.pm,v 1.1 2007-05-17 08:39:36 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::Funshift;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "functional_similarity" );

__PACKAGE__->add_columns( qw/ auto_pfamA_A
                              auto_pfamA_B
                              rfunSim
                              mfscore
                              bpscore / );

__PACKAGE__->set_primary_key( "auto_pfamA_A" );

__PACKAGE__->has_one( pfam => "PfamDB::Pfam",
                      { "foreign.auto_pfamA" => "self.auto_pfamA_B" },
                      { proxy                => [ qw/ pfamA_acc pfamA_id / ] } );

__PACKAGE__->might_have( clan => "PfamDB::Clan_membership",
                      { "foreign.auto_pfamA" => "self.auto_pfamA_B" },
                      { proxy                => [ qw/ clan_id / ] } );

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

