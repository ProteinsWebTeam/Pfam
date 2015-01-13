use utf8;
package PfamLive::Result::GeneOntology;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::GeneOntology

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<gene_ontology>

=cut

__PACKAGE__->table("gene_ontology");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 go_id

  data_type: 'tinytext'
  is_nullable: 0

=head2 term

  data_type: 'longtext'
  is_nullable: 0

=head2 category

  data_type: 'tinytext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "go_id",
  { data_type => "tinytext", is_nullable => 0 },
  "term",
  { data_type => "longtext", is_nullable => 0 },
  "category",
  { data_type => "tinytext", is_nullable => 0 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i1NMTGASEtxzbyEbUY2avg
__PACKAGE__->add_unique_constraint(
  pfama_go_unq => [ qw( pfama_acc go_id ) ] 
);

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
