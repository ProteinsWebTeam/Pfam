package DfamDB::Result::DfamWiki;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::DfamWiki

=cut

__PACKAGE__->table("dfam_wiki");

=head1 ACCESSORS

=head2 dfam_acc

  data_type: 'varchar'
  default_value: 0
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 auto_wiki

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "dfam_acc",
  {
    data_type => "varchar",
    default_value => 0,
    is_foreign_key => 1,
    is_nullable => 0,
    size => 7,
  },
  "auto_wiki",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 dfam_acc

Type: belongs_to

Related object: L<DfamDB::Result::Dfam>

=cut

__PACKAGE__->belongs_to(
  "dfam_acc",
  "DfamDB::Result::Dfam",
  { dfam_acc => "dfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 auto_wiki

Type: belongs_to

Related object: L<DfamDB::Result::Wikipedia>

=cut

__PACKAGE__->belongs_to(
  "auto_wiki",
  "DfamDB::Result::Wikipedia",
  { auto_wiki => "auto_wiki" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:c3rO6NI/QCQAWld3HnNOUg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
