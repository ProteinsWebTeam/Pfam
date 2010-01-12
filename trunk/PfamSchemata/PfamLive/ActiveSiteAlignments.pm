package PfamLive::ActiveSiteAlignments;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("_active_site_alignments");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 5 },
  "alignment",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "as_residues",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "Pfamlive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-06 16:49:53
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:vGDRQUAb6OvcCBwN3IbDJw

__PACKAGE__->set_primary_key( 'auto_pfama' );


__PACKAGE__->has_one( pfama =>  'PfamLive::Pfama',
                      { 'foreign.auto_pfama'  => 'self.auto_pfama' },
                                            { proxy => [ qw( auto_pfama 
                                                             pfama_acc
                                                             pfama_id ) ] } );


# You can replace this text with custom content, and it will be preserved on regeneration
1;
