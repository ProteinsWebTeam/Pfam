
# $Id: Context_pfam_regions.pm,v 1.3 2007-03-08 14:16:28 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Context_pfam_regions;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "context_pfam_regions" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamA auto_pfamseq seq_start seq_end domain_score/);

#Set the the keys
__PACKAGE__->set_primary_key( "auto_pfamA", "auto_pfamseq");


#Now on to the relationships

__PACKAGE__->has_one    ( "pfamA" => "PfamDB::Pfam",
			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
			  {proxy => [qw/pfamA_id pfamA_acc description/]});

__PACKAGE__->has_one    ( "pfamseq" => "PfamDB::Pfamseq",
			  {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
			  {proxy => [ qw/pfamseq_id pfamseq_acc/ ]});

1;
