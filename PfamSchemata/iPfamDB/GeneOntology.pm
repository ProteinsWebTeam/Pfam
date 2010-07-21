package iPfamDB::GeneOntology;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("gene_ontology");
__PACKAGE__->add_columns(
  "go_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "category",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "term",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("go_id", "pfama_acc");
__PACKAGE__->belongs_to("pfama_acc", "iPfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:VOszGsR+2Ev28bpIAPxLGg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
