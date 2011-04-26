package DfamDB::Result::Dfamseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::Dfamseq

=cut

__PACKAGE__->table("dfamseq");

=head1 ACCESSORS

=head2 auto_dfamseq

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 dfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 dfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 6

=head2 crc64

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 md5

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 length

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 sequence

  accessor: undef
  data_type: 'blob'
  is_nullable: 0

=head2 updated

  data_type: 'timestamp'
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  is_nullable: 1

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_dfamseq",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "dfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "dfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 6 },
  "crc64",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "sequence",
  { accessor => undef, data_type => "blob", is_nullable => 0 },
  "updated",
  {
    data_type     => "timestamp",
    default_value => \"current_timestamp",
    is_nullable   => 0,
  },
  "created",
  { data_type => "datetime", is_nullable => 1 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
);
__PACKAGE__->set_primary_key("auto_dfamseq");
__PACKAGE__->add_unique_constraint("dfamseq_acc", ["dfamseq_acc"]);

=head1 RELATIONS

=head2 dfam_reg_full_significants

Type: has_many

Related object: L<DfamDB::Result::DfamRegFullSignificant>

=cut

__PACKAGE__->has_many(
  "dfam_reg_full_significants",
  "DfamDB::Result::DfamRegFullSignificant",
  { "foreign.auto_dfamseq" => "self.auto_dfamseq" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dfam_reg_seeds

Type: has_many

Related object: L<DfamDB::Result::DfamRegSeed>

=cut

__PACKAGE__->has_many(
  "dfam_reg_seeds",
  "DfamDB::Result::DfamRegSeed",
  { "foreign.auto_dfamseq" => "self.auto_dfamseq" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 ncbi_taxid

Type: belongs_to

Related object: L<DfamDB::Result::NcbiTaxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "DfamDB::Result::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LffE4FmQ9s6wkAW6ho0xaA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
