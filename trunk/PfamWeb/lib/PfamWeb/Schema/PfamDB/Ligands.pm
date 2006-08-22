package PfamWeb::Schema::PfamDB::Ligands;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "ligands" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/ligand_id code three_letter_code one_letter_code name systematic_name num_atoms_all num_atoms_no_h stereo_smiles non_stereo_smiles charge category formula molecular_weight/);

__PACKAGE__->set_primary_key( "ligand_id", "code" );
#__PACKAGE__->might_have ( "synonym" 

1;
