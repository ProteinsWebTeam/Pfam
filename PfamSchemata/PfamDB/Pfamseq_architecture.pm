package PfamDB::Pfamseq_architecture;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamseq_architecture" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq auto_architecture frag/);

__PACKAGE__->set_primary_key("auto_pfamseq", "auto_architecture" );

__PACKAGE__->has_one("auto_pfamseq" => "PfamDB::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one("annseq" => "PfamDB::Pfam_annseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/annseq_storable/]});

__PACKAGE__->has_one("arch" => "PfamDB::Architecture",
		     {"foreign.auto_architecture" => "self.auto_architecture"},
		     {proxy => [qw/architecture no_seqs/]});


1;
