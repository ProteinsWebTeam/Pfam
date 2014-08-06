use utf8;
package RfamLive::Result::Overlap;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Overlap

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_overlap>

=cut

__PACKAGE__->table("_overlap");

=head1 ACCESSORS

=head2 auto_overlap

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 id

  data_type: 'varchar'
  is_nullable: 1
  size: 40

=head2 description

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 author

  data_type: 'tinytext'
  is_nullable: 1

=head2 comment

  data_type: 'longtext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_overlap",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "id",
  { data_type => "varchar", is_nullable => 1, size => 40 },
  "description",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "author",
  { data_type => "tinytext", is_nullable => 1 },
  "comment",
  { data_type => "longtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_overlap>

=back

=cut

__PACKAGE__->set_primary_key("auto_overlap");

=head1 RELATIONS

=head2 overlap_memberships

Type: has_many

Related object: L<RfamLive::Result::OverlapMembership>

=cut

__PACKAGE__->has_many(
  "overlap_memberships",
  "RfamLive::Result::OverlapMembership",
  { "foreign.auto_overlap" => "self.auto_overlap" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jPYIGrojWcOlQBdKsi7+kg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
