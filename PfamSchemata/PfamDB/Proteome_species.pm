
# $Id: Proteome_species.pm,v 1.1 2007-08-20 08:57:17 rdf Exp $
#
# $Author: rdf $
package PfamDB::Proteome_species;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "genome_species" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/ncbi_code species grouping num_distinct_regions num_total_regions num_proteins sequence_coverage residue_coverage total_genome_proteins total_aa_length/);

#Set the the keys
__PACKAGE__->set_primary_key( "ncbi_code" );


#Now Set up the relationships

__PACKAGE__->has_one("ncbi_tax"  => "PfamDB::Ncbi_taxonomy",
		     {"foreign.ncbi_code" => "self.ncbi_code"},
		     { proxy => [ qw( taxonomy ) ] } );
#Do all of the annotated regions

##genome pfamseq
__PACKAGE__->has_many("genomePfamSeq"  => "PfamDB::Pfamseq",
		     {"foreign.ncbi_code" => "self.ncbi_code"} );
		     
		     

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

