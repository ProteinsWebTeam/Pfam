
# $Id: PfamB.pm,v 1.4 2007-03-08 14:16:24 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamB;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamB" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamB pfamB_acc pfamB_id/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamB", "pfamB_acc");

#Now setup the relationship 

#PfamB joins are to pfamB_reg, pdbmap & pfamB_stockholm 

__PACKAGE__->has_many( pfamb_reg => "PfamDB::PfamB_reg",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" } );

__PACKAGE__->has_many( pdbMap => "PfamDB::PdbMap",
			  { "foreign.auto_pfam"  => "self.auto_pfamB" },
			  { proxy => [ qw/pdb_id/ ] } );

__PACKAGE__->has_one( pfamB_stockholm => "PfamDB::PfamB_stockholm",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" },
		      { proxy => qw[/stockholm_data/] } );

__PACKAGE__->has_many( pfamB_database_links => "PfamDB::PfamB_database_links",
					   { "foreign.auto_pfamB" => "self.auto_pfamB" } );


1;
