
# $Id: Pfam_annseq.pm,v 1.3 2007-03-08 14:16:27 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Pfam_annseq;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfam_annseq" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq annseq_storable/);

__PACKAGE__->set_primary_key("auto_pfamseq");

#This doesnot need any proxies as all of the data should be in the storable object
__PACKAGE__->has_one("auto_pfamseq" => "PfamDB::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
					{proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one("auto_architecture" => "PfamDB::Architecture",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"});
1;
