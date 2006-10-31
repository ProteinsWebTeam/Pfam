package PfamDB::Genome_pfamseq;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "genome_pfamseq" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq ncbi_code/);

#Set the the keys
__PACKAGE__->set_primary_key( "auto_pfamseq", "ncbi_code" );


#Now Set up the relationships


#Do all of the annotated regions

##pfamseq
__PACKAGE__->has_one("pfamseq",  => "PfamDB::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"} );

##genome_species
__PACKAGE__->has_one( "genome_species",  => "PfamDB::Genome_species",
		      {"foreign.ncbi_code" => "self.ncbi_code"},
		      { proxy => [qw/ species grouping num_distinct_regions num_total_regions
							  num_proteins sequence_coverage residue_coverage 
							  total_genome_proteins total_aa_length /]} );


1;
