package iPfamDB::Pfama;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfama");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "pfama_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "numberinalign",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "description",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "domcount",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 5 },
  "ligcount",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "nacount",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "interpro_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "interpro_abstract",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("pfama_acc");
__PACKAGE__->has_many(
  "domains",
  "iPfamDB::Domain",
  { "foreign.pfam_acc" => "self.pfama_acc" },
);
__PACKAGE__->has_many(
  "gene_ontologies",
  "iPfamDB::GeneOntology",
  { "foreign.pfama_acc" => "self.pfama_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bXtowMSLYK1r+gB9b/mu3g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
