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

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
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
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
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


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-12-02 12:30:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:s0QTDL+RIjJ6KJGyncX51g


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
