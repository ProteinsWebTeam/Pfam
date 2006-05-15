
# Secondary_pfamseq_acc.pm
# jt6 20060427 WTSI
#
# Model for the secondary_pfamseq_acc table.
#
# $Id: Secondary_pfamseq_acc.pm,v 1.1 2006-05-15 12:15:45 jt6 Exp $

package PfamWeb::Model::Secondary_pfamseq_acc;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "secondary_pfamseq_acc" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ auto_pfamseq secondary_acc /);

# now set up the primary keys/contraints
__PACKAGE__->set_primary_key( "auto_pfamseq", "secondary_acc" );

# now setup the relationship
__PACKAGE__->has_one( "pfamseq" => "PfamWeb::Model::Pfamseq",
					  { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
					  { proxy => [ qw/ pfamseq_id pfamseq_acc sequence / ] }  );

1;
