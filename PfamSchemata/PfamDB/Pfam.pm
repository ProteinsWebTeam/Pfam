
# $Id: Pfam.pm,v 1.4 2007-03-08 14:16:24 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::Pfam;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamA" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamA pfamA_acc pfamA_id description model_length author seed_source alignment_method type ls_sequence_GA ls_domain_GA fs_sequence_GA fs_domain_GA ls_sequence_TC ls_domain_TC fs_sequence_TC fs_domain_TC ls_sequence_NC ls_domain_NC fs_sequence_NC fs_domain_NC ls_mu ls_kappa fs_mu fs_kappa comment previous_id hmmbuild_ls hmmcalibrate_ls hmmbuild_fs hmmcalibrate_fs num_full num_seed version number_archs number_structures number_species/ );


#Set the the keys
__PACKAGE__->set_primary_key( "auto_pfamA", "pfamA_id", "pfamA_acc" );


#Now on to the relationships

__PACKAGE__->might_have ( "interpro" => "PfamDB::Interpro",
			  {"foreign.auto_pfamA"  => "self.auto_pfamA" },
			  { proxy => [ qw/interpro_id abstract/ ] } );

__PACKAGE__->has_many   ( "pdbMap"   => "PfamDB::PdbMap",
			  { "foreign.auto_pfam"  => "self.auto_pfamA" });

__PACKAGE__->has_many   ( "go"       => "PfamDB::GO",
			  { "foreign.auto_pfamA" => "self.auto_pfamA" } );

__PACKAGE__->has_many   ( "pfamA_lit_refs" => "PfamDB::PfamA_literature_references",
			  {"foreign.auto_pfamA"  => "self.auto_pfamA"} );

__PACKAGE__->might_have ( "clan_membership" => "PfamDB::Clan_membership",
			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
			  {proxy => [ qw/clan_acc clan_id clan_description/]});

__PACKAGE__->has_one    ( "pfamA_web" => "PfamDB::PfamA_web",
			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
			  {proxy => [ qw/average_length percentage_id average_coverage status/]});

__PACKAGE__->has_many    ( "pfamA_arch" => "PfamDB::PfamA_architecture",
			  {"foreign.auto_pfamA" => "self.auto_pfamA"});

__PACKAGE__->has_many( "pfamA_database_links" => "PfamDB::PfamA_database_links",
		       {"foreign.auto_pfamA" => "self.auto_pfamA"});

#PRC tables - todo


#All of the region tables that join on to pfamA

__PACKAGE__->has_many ("pfamA_reg_full" => "PfamDB::PfamA_reg_full",
		       {"foreign.auto_pfamA" => "self.auto_pfamA"});

__PACKAGE__->has_many ("pfamA_reg_seed" => "PfamDB::PfamA_reg_seed",
		       {"foreign.auto_pfamA" => "self.auto_pfamA"});

__PACKAGE__->has_many ("context" => "PfamDB::Context_pfam_regions",
		       {"foreign.auto_pfamA" => "self.auto_pfamA"});

#Interaction tables - todo

#Genome tables - todo



1;
