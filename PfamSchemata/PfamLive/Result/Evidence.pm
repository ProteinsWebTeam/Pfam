use utf8;
package PfamLive::Result::Evidence;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Evidence

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<evidence>

=cut

__PACKAGE__->table("evidence");

=head1 ACCESSORS

=head2 evidence

  data_type: 'tinyint'
  is_nullable: 0

=head2 description

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=cut

__PACKAGE__->add_columns(
  "evidence",
  { data_type => "tinyint", is_nullable => 0 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
);

=head1 PRIMARY KEY

=over 4

=item * L</evidence>

=back

=cut

__PACKAGE__->set_primary_key("evidence");

=head1 RELATIONS

=head2 pfamseqs

Type: has_many

Related object: L<PfamLive::Result::Pfamseq>

=cut

__PACKAGE__->has_many(
  "pfamseqs",
  "PfamLive::Result::Pfamseq",
  { "foreign.evidence" => "self.evidence" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6heDz3cNO2yT93UBiHxEMg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
