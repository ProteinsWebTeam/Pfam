package PfamWeb::Model::Pfamseq_markup;

use strict;
use warnings;
use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamseq_markup" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq auto_markup residue annotation/);

__PACKAGE__->set_primary_key("auto_pfamseq");

__PACKAGE__->has_one("auto_pfamseq" => "PfamWeb::Model::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one("auto_markup" => "PfamWeb::Model::Markup_key",
		     {"foreign.auto_markup" => "self.auto_markup"},
		     {proxy => [qw/label/]});


1;
