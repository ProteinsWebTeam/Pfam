use utf8;
package RfamDB::Result::GenomeEntry;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::GenomeEntry

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<genome_entry>

=cut

__PACKAGE__->table("genome_entry");

=head1 ACCESSORS

=head2 auto_genome

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 genome_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 ensembl_id

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 description

  data_type: 'mediumtext'
  is_nullable: 1

=head2 ncbi_id

  data_type: 'integer'
  is_nullable: 1

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 circular

  data_type: 'tinyint'
  is_nullable: 1

=head2 length

  data_type: 'bigint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_genome",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "genome_acc",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "ensembl_id",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "description",
  { data_type => "mediumtext", is_nullable => 1 },
  "ncbi_id",
  { data_type => "integer", is_nullable => 1 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "circular",
  { data_type => "tinyint", is_nullable => 1 },
  "length",
  { data_type => "bigint", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_genome>

=back

=cut

__PACKAGE__->set_primary_key("auto_genome");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:/cpatkuulYZqaoskFu+5yQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
