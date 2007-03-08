
# $Id: PfamA_reg_seed.pm,v 1.3 2007-03-08 14:16:30 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamA_reg_seed;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamA_reg_seed" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq auto_pfamA seq_start seq_end /);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamA", "auto_pfamseq");

#Now setup the relationship
__PACKAGE__->has_one( "pfamA" =>  "PfamDB::Pfam",
		      { "foreign.auto_pfamA"  => "self.auto_pfamA" },
		      { proxy => [ qw/pfamA_id/ ] } );

__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_acc pfamseq_id/ ] } );

1;
