
# $Id: Ncbi_taxonomy.pm,v 1.3 2007-03-08 14:16:26 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Ncbi_taxonomy;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "ncbi_taxonomy" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/ncbi_code species taxonomy/);

__PACKAGE__->set_primary_key("ncbi_code");

__PACKAGE__->has_many("auto_pfamseq" => "PfamDB::Pfamseq_ncbi",
		      {"foreign.ncbi_code" => "self.ncbi_code"});

1;
