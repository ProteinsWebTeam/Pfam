package PfamDB::Version;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table - The version table is a stand alone table, so this is a simple module.
__PACKAGE__->table( "VERSION" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/pfam_release
														 pfam_release_date
														 swiss_prot_version
														 trembl_version
														 hmmer_version
														 number_families
														 pfamA_coverage
														 pfamB_additional_coverage
														 pfamA_residue_coverage
														 pfamB_additional_residue_coverage/);

__PACKAGE__->set_primary_key( qw/pfam_release swiss_prot_version trembl_version hmmer_version/);

1;
