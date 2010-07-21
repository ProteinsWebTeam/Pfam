
package WikiApprove::Wikiuser;

use strict;
use warnings;

# doesn't need to be a Moose class

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wikiuser");

#-------------------------------------------------------------------------------
#- columns ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

__PACKAGE__->add_columns(
  "user_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 100 },
  "approved",
  { data_type => "TINYINT", default_value => 0, is_nullable => 0, size => 1 },
  "number_edits",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);

#-------------------------------------------------------------------------------
#- keys ------------------------------------------------------------------------
#-------------------------------------------------------------------------------

__PACKAGE__->set_primary_key("user_name");

#-------------------------------------------------------------------------------
#- custom methods --------------------------------------------------------------
#-------------------------------------------------------------------------------

sub add_edits {
  my ( $this, $new_edits ) = @_;
  
  my $edit_count = ( $this->number_edits || 0 ) + $new_edits;
  $this->update( { number_edits => $edit_count } );
}

#-------------------------------------------------------------------------------

1;

__END__
CREATE TABLE `wikiuser` (
  `user_name` varchar(100) NOT NULL,
  `approved` tinyint(1) default '0',
  `number_edits` int(11) default '0',
  PRIMARY KEY  (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1

