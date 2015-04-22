use utf8;
package PfamDB::Adda;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Adda

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<adda>

=cut

__PACKAGE__->table("adda");

=head1 ACCESSORS

=head2 adda_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 seqacc

  data_type: 'varchar'
  is_nullable: 0
  size: 6

=head2 version

  data_type: 'tinyint'
  is_nullable: 0

=head2 start

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 end

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "adda_acc",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "seqacc",
  { data_type => "varchar", is_nullable => 0, size => 6 },
  "version",
  { data_type => "tinyint", is_nullable => 0 },
  "start",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "end",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:QkxbS8bCkrdsfuOfV7yg3Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
