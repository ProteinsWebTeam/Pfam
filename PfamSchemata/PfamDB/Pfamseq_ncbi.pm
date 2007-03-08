
# $Id: Pfamseq_ncbi.pm,v 1.3 2007-03-08 14:16:27 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Pfamseq_ncbi;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamseq_ncbi" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq ncbi_code/);

__PACKAGE__->set_primary_key("auto_pfamseq", "ncbi_code");

__PACKAGE__->has_one("auto_pfamseq" => "PfamDB::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one("ncbi" => "PfamDB::Ncbi_taxonomy",
		     {"foreign.ncbi_code" => "self.ncbi_code"},
		     {proxy => [qw/species taxonomy/]});

1;
