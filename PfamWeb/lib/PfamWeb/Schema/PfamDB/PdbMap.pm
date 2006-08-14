
package PfamWeb::Schema::PfamDB::PdbMap;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdbmap" );
__PACKAGE__->add_columns( qw/auto_pdb auto_pfam auto_pfamseq chain pdb_start_res pdb_end_res pfam_start_res pfam_end_res hex_colour pfam_region/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one( "pfamA" => "PfamWeb::Schema::PfamDB::Pfam",
		      { "foreign.auto_pfamA" => "self.auto_pfam" },
		       { proxy => [ qw/pfamA_id pfamA_acc/ ] } );

__PACKAGE__->has_one( "pfamB" => "PfamWeb::Schema::PfamDB::PfamB",
		      { "foreign.auto_pfamB" => "self.auto_pfam" } );

__PACKAGE__->has_one( "pdb"  => "PfamWeb::Schema::PfamDB::Pdb",
		      { "foreign.auto_pdb"   => "self.auto_pdb" },
		      { proxy => [ qw/pdb_id header title pdb_image/ ] } );

__PACKAGE__->has_one( "pfamseq" => "PfamWeb::Schema::PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq" => "self.auto_pfamseq"},
		      { proxy => [ qw/pfamseq_id pfamseq_acc/]});

1;

