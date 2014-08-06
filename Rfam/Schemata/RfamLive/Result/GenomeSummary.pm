use utf8;
package RfamDB::Result::GenomeSummary;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::GenomeSummary

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<genome_summary>

=cut

__PACKAGE__->table("genome_summary");

=head1 ACCESSORS

=head2 ncbi_id

  data_type: 'char'
  is_nullable: 0
  size: 10

=head2 species

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 kingdom

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=head2 regions

  data_type: 'integer'
  is_nullable: 1

=head2 families

  data_type: 'integer'
  is_nullable: 1

=head2 genome_size

  data_type: 'bigint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "ncbi_id",
  { data_type => "char", is_nullable => 0, size => 10 },
  "species",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "kingdom",
  { data_type => "varchar", is_nullable => 1, size => 50 },
  "regions",
  { data_type => "integer", is_nullable => 1 },
  "families",
  { data_type => "integer", is_nullable => 1 },
  "genome_size",
  { data_type => "bigint", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</ncbi_id>

=back

=cut

__PACKAGE__->set_primary_key("ncbi_id");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:NI6ySA+Vv2V9KV7N+78WiA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
