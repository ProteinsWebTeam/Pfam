
package PfamWeb::Schema::PfamDB::Pdb_residue;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "pdb_residue_data" );
__PACKAGE__->add_columns( qw/ auto_pdb
							  msd_chain
							  chain
							  serial
							  pdb_res
							  pdb_seq_number
							  dssp_code
							  auto_pfamseq
							  pfamseq_res
							  pfamseq_seq_number / );

__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one( "pdb" => "PfamWeb::Schema::PfamDB::Pdb",
					  { "foreign.auto_pdb" => "self.auto_pdb" },
					  { proxy => [ qw/ pdb / ] }
					);

__PACKAGE__->has_one( "pfamseq" => "PfamWeb::Schema::PfamDB::Pfamseq",
					  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
					  { proxy => [ qw/ pfamseq_id pfamseq_acc / ] }
					);

__PACKAGE__->has_one( "pfamseq_arch" => "PfamWeb::Schema::PfamDB::Pfamseq_architecture",
					  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
					  { proxy => [ qw/auto_architecture/ ] }
					);
__PACKAGE__->has_one( "pfam_anseq" => "PfamWeb::Schema::PfamDB::Pfam_annseq",
					  { "foreign.auto_pfam" => "self.auto_pfam" },
					  { proxy => [ qw/ pfamseq_storable / ] }
					);

1;

