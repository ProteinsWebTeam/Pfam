use utf8;
package RfamDB::Result::ChromosomeBuild;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::ChromosomeBuild

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<chromosome_build>

=cut

__PACKAGE__->table("chromosome_build");

=head1 ACCESSORS

=head2 auto_genome

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=head2 xsome_start

  data_type: 'bigint'
  is_nullable: 0

=head2 xsome_end

  data_type: 'bigint'
  is_nullable: 1

=head2 clone_start

  data_type: 'bigint'
  is_nullable: 1

=head2 clone_end

  data_type: 'bigint'
  is_nullable: 1

=head2 strand

  data_type: 'char'
  is_nullable: 1
  size: 2

=cut

__PACKAGE__->add_columns(
  "auto_genome",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
  "xsome_start",
  { data_type => "bigint", is_nullable => 0 },
  "xsome_end",
  { data_type => "bigint", is_nullable => 1 },
  "clone_start",
  { data_type => "bigint", is_nullable => 1 },
  "clone_end",
  { data_type => "bigint", is_nullable => 1 },
  "strand",
  { data_type => "char", is_nullable => 1, size => 2 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_genome>

=back

=cut

__PACKAGE__->set_primary_key("auto_genome");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:h0hq/sUg7sDtZW3TnYUByA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
