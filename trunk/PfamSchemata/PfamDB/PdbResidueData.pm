use utf8;
package PfamDB::PdbResidueData;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PdbResidueData

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_residue_data>

=cut

__PACKAGE__->table("pdb_residue_data");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 chain

  data_type: 'varchar'
  is_nullable: 1
  size: 4

=head2 serial

  data_type: 'integer'
  is_nullable: 1

=head2 pdb_res

  data_type: 'char'
  is_nullable: 1
  size: 3

=head2 pdb_seq_number

  data_type: 'integer'
  is_nullable: 1

=head2 pdb_insert_code

  data_type: 'varchar'
  is_nullable: 1
  size: 1

=head2 observed

  data_type: 'integer'
  is_nullable: 1

=head2 dssp_code

  data_type: 'varchar'
  is_nullable: 1
  size: 4

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 pfamseq_res

  data_type: 'char'
  is_nullable: 1
  size: 3

=head2 pfamseq_seq_number

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "chain",
  { data_type => "varchar", is_nullable => 1, size => 4 },
  "serial",
  { data_type => "integer", is_nullable => 1 },
  "pdb_res",
  { data_type => "char", is_nullable => 1, size => 3 },
  "pdb_seq_number",
  { data_type => "integer", is_nullable => 1 },
  "pdb_insert_code",
  { data_type => "varchar", is_nullable => 1, size => 1 },
  "observed",
  { data_type => "integer", is_nullable => 1 },
  "dssp_code",
  { data_type => "varchar", is_nullable => 1, size => 4 },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "pfamseq_res",
  { data_type => "char", is_nullable => 1, size => 3 },
  "pfamseq_seq_number",
  { data_type => "integer", is_nullable => 1 },
);

=head1 RELATIONS

=head2 pdb_id

Type: belongs_to

Related object: L<PfamDB::Pdb>

=cut

__PACKAGE__->belongs_to("pdb_id", "PfamDB::Pdb", { pdb_id => "pdb_id" });

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamDB::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamDB::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hO9yrzICkPfu/SwII99suA


__PACKAGE__->might_have(
  "pfamseqMarkup",
  "PfamDB::PfamseqMarkup",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc"},
  undef
    
);


__PACKAGE__->might_have(
  "pfamseqMarkup2",
  "PfamDB::PfamseqMarkup",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc", "foreign.residue" => "self.pfamseq_seq_number"},
  undef
);
# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
