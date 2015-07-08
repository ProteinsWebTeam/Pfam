use utf8;
package PfamDB::Evidence;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Evidence

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

Related object: L<PfamDB::Pfamseq>

=cut

__PACKAGE__->has_many(
  "pfamseqs",
  "PfamDB::Pfamseq",
  { "foreign.evidence" => "self.evidence" },
  undef,
);

=head2 uniprots

Type: has_many

Related object: L<PfamDB::Uniprot>

=cut

__PACKAGE__->has_many(
  "uniprots",
  "PfamDB::Uniprot",
  { "foreign.evidence" => "self.evidence" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-06-25 11:22:06
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:YFdxWsqh8FohxJeuwaLv2g


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
