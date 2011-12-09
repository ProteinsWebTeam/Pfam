package PfamLive::GeneOntology;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("gene_ontology");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "go_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "term",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 4294967295,
  },
  "category",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:9Jm6+n4C9o84GVj7Le/o5w

__PACKAGE__->add_unique_constraint(
  auto_go_unq => [ qw( auto_pfama go_id ) ] 
);

__PACKAGE__->has_one( pfama =>  'PfamLive::Pfama',
                      { 'foreign.auto_pfama'  => 'self.auto_pfama' },
                                            { proxy => [ qw( auto_pfama 
                                                             pfama_acc
                                                             pfama_id ) ] } );

# You can replace this text with custom content, and it will be preserved on regeneration
1;
