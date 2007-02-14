
package PfamLive::Clan_versions;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clan_versions"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_clan current version rcs_trace message user updated/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_clan" );

#Set up relationships

#1 to 1 relationship
                      		       
__PACKAGE__->has_one( "clans" => "Pfamlive::Clans",
                      {"foreign.auto_clan"  => "self.auto_clan"},
                      {proxy => [qw/clan_id clan_acc/],
                       cascade_delete => 0 });
                      
                     
                      	       
1;
