
# $Id: Ncbi_taxonomy.pm,v 1.5 2007-08-20 08:58:47 rdf Exp $
#
# $Author: rdf $
package PfamDB::Ncbi_taxonomy;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "ncbi_taxonomy" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/ncbi_code species taxonomy/);

__PACKAGE__->set_primary_key("ncbi_code");

__PACKAGE__->has_many("auto_pfamseq" => "PfamDB::Pfamseq",
		      {"foreign.ncbi_code" => "self.ncbi_code"});

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

