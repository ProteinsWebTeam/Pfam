
# $Id: PfamB_reg.pm,v 1.5 2007-03-08 14:16:27 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamB_reg;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamB_reg" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq auto_pfamB seq_start seq_end /);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamB", "auto_pfamseq");

#Now setup the relationship

__PACKAGE__->has_one( "pfamB" =>  "PfamDB::PfamB",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" },
		      { proxy => [ qw/pfamB_id pfamB_acc/ ] } );


__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_acc pfamseq_id species taxonomy/ ] } );

__PACKAGE__->might_have( "pfamseq_architecture" =>  "PfamDB::Pfamseq_architecture",
			 { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
			 { proxy => [ qw/auto_architecture/ ] } );

__PACKAGE__->has_one("annseq" => "PfamDB::Pfam_annseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/annseq_storable/]});

1;
