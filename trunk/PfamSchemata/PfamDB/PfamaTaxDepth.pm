use utf8;
package PfamDB::PfamaTaxDepth;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamaTaxDepth

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_tax_depth>

=cut

__PACKAGE__->table("pfamA_tax_depth");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 root

  data_type: 'char'
  is_nullable: 0
  size: 24

=head2 count

  data_type: 'integer'
  is_nullable: 0

=head2 common

  data_type: 'text'
  is_nullable: 0

=head2 ncbi_taxid

  data_type: 'integer'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "root",
  { data_type => "char", is_nullable => 0, size => 24 },
  "count",
  { data_type => "integer", is_nullable => 0 },
  "common",
  { data_type => "text", is_nullable => 0 },
  "ncbi_taxid",
  { data_type => "integer", is_nullable => 0 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:pfzdiO0Dbo0Sh2uYsZJJRw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
