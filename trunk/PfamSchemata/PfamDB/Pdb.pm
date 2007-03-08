
# $Id: Pdb.pm,v 1.4 2007-03-08 14:16:27 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::Pdb;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_new" );
__PACKAGE__->add_columns( qw/auto_pdb pdb_id header title date resolution
							 experiment_short experiment_long pubmed_id/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_many( "pdbAuthor" => "PfamDB::PdbAuthor",
		       { "foreign.auto_pdb" => "self.auto_pdb" });

__PACKAGE__->has_many( "pdbResidueData" => "PfamDB::PdbResidueData",
		       { "foreign.auto_pdb" => "self.auto_pdb" });

__PACKAGE__->has_many( "pdbMap" => "PfamDB::PdbMap",
			  { "foreign.auto_pdb" => "self.auto_pdb" } );

__PACKAGE__->might_have( "image"  => "PfamDB::PdbImage",
			 { "foreign.auto_pdb" => "self.auto_pdb" },
			 { proxy => [ qw/pdb_image pdb_image_sml/ ] } );

1;

