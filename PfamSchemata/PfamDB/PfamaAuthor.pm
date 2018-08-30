use utf8;
package PfamDB::PfamaAuthor;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamaAuthor

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_author>

=cut

__PACKAGE__->table("pfamA_author");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 author_rank

  data_type: 'integer'
  is_nullable: 0

=head2 author_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "author_rank",
  { data_type => "integer", is_nullable => 0 },
  "author_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<acc_author>

=over 4

=item * L</pfama_acc>

=item * L</author_id>

=back

=cut

__PACKAGE__->add_unique_constraint("acc_author", ["pfama_acc", "author_id"]);

=head2 C<acc_rank>

=over 4

=item * L</pfama_acc>

=item * L</author_rank>

=back

=cut

__PACKAGE__->add_unique_constraint("acc_rank", ["pfama_acc", "author_rank"]);

=head1 RELATIONS

=head2 author_id

Type: belongs_to

Related object: L<PfamDB::Author>

=cut

__PACKAGE__->belongs_to("author_id", "PfamDB::Author", { author_id => "author_id" });

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2018-08-30 08:56:11
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:0oPx3hYiT8NzELTxPs7mxA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
