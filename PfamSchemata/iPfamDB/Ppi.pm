package iPfamDB::Ppi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ppi");
__PACKAGE__->add_columns(
  "ppi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "protein_acc_a",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "protein_acc_b",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
);
__PACKAGE__->set_primary_key("ppi");
__PACKAGE__->belongs_to(
  "protein_acc_a",
  "iPfamDB::Protein",
  { accession => "protein_acc_a" },
);
__PACKAGE__->belongs_to(
  "protein_acc_b",
  "iPfamDB::Protein",
  { accession => "protein_acc_b" },
);
__PACKAGE__->has_many("ppi_res", "iPfamDB::PpiRes", { "foreign.ppi" => "self.ppi" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Jk4IetWsAmu8msSfzRrciA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
