package DfamDB::Result::Lock;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::Lock

=cut

__PACKAGE__->table("_lock");

=head1 ACCESSORS

=head2 locked

  data_type: 'tinyint'
  is_nullable: 0

=head2 locker

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 allowcommits

  data_type: 'tinyint'
  is_nullable: 0

=head2 alsoallow

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "locked",
  { data_type => "tinyint", is_nullable => 0 },
  "locker",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "allowcommits",
  { data_type => "tinyint", is_nullable => 0 },
  "alsoallow",
  { data_type => "text", is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tdoYlKWZoIFvjN5HDi9C7g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
