
# $Id: Genome_species.pm,v 1.2 2007-03-08 14:16:25 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Genome_species;

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


#Do all of the annotated regions

##genome pfamseq
__PACKAGE__->has_many("genomePfamSeq",  => "PfamDB::Genome_pfamseq",
		     {"foreign.ncbi_code" => "self.ncbi_code"} );

1;
