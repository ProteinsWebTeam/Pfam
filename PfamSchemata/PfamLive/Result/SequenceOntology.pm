use utf8;
package PfamLive::Result::SequenceOntology;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::SequenceOntology

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<sequence_ontology>

=cut

__PACKAGE__->table("sequence_ontology");

=head1 ACCESSORS

=head2 type

  data_type: 'varchar'
  is_nullable: 0
  size: 30

=head2 so_id

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 so_name

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=cut

__PACKAGE__->add_columns(
  "type",
  { data_type => "varchar", is_nullable => 0, size => 30 },
  "so_id",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "so_name",
  { data_type => "varchar", is_nullable => 0, size => 100 },
);

=head1 PRIMARY KEY

=over 4

=item * L</type>

=back

=cut

__PACKAGE__->set_primary_key("type");

=head1 RELATIONS

=head2 pfams_a

Type: has_many

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->has_many(
  "pfams_a",
  "PfamLive::Result::PfamA",
  { "foreign.type" => "self.type" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2018-06-21 16:04:29
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:S7IxUHkEPft9iQTgdtUOhg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
