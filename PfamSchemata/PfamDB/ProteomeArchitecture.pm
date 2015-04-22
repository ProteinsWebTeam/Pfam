use utf8;
package PfamDB::ProteomeArchitecture;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::ProteomeArchitecture

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<proteome_architecture>

=cut

__PACKAGE__->table("proteome_architecture");

=head1 ACCESSORS

=head2 auto_proteome

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 auto_architecture

  data_type: 'bigint'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 type_example

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 10

=head2 no_seqs

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "auto_proteome",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "auto_architecture",
  {
    data_type => "bigint",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "type_example",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 10 },
  "no_seqs",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
);

=head1 RELATIONS

=head2 auto_architecture

Type: belongs_to

Related object: L<PfamDB::Architecture>

=cut

__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamDB::Architecture",
  { auto_architecture => "auto_architecture" },
);

=head2 auto_proteome

Type: belongs_to

Related object: L<PfamDB::CompleteProteomes>

=cut

__PACKAGE__->belongs_to(
  "auto_proteome",
  "PfamDB::CompleteProteomes",
  { auto_proteome => "auto_proteome" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:w7CLzloAMGss6pfW7hNFKw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
