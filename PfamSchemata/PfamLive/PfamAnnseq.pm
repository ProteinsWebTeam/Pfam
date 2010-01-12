package PfamLive::PfamAnnseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfam_annseq");
__PACKAGE__->add_columns(
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "annseq_storable",
  {
    data_type => "MEDIUMBLOB",
    default_value => undef,
    is_nullable => 0,
    size => 16777215,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:cKJ85pYcAglwUa+TdXZ7iQ

__PACKAGE__->set_primary_key("auto_pfamseq");
# You can replace this text with custom content, and it will be preserved on regeneration
1;
