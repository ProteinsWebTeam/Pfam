
# $Id: PfamA2pfamA_scoop_results.pm,v 1.2 2007-03-08 14:16:24 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamA2pfamA_scoop_results;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("pfamA2pfamA_scoop_results"); # This is how we define the table
__PACKAGE__->add_columns( qw/ auto_pfamA1 auto_pfamA2 score /); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamA1", "auto_pfamA2");

#Set up relationships
#1 to many relationship
__PACKAGE__->has_one( "pfamA1" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA1"},);
		      #{proxy => [qw/pfamA_id pfamA_acc/]});

__PACKAGE__->has_one( "pfamA2" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA2"},);
		      #{proxy => [qw/pfamA_id pfamA_acc/]});



1;
