package DfamDB::Result::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::LiteratureReference

=cut

__PACKAGE__->table("literature_references");

=head1 ACCESSORS

=head2 pmid

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=head2 journal

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pmid",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("pmid");

=head1 RELATIONS

=head2 dfam_literature_references

Type: has_many

Related object: L<DfamDB::Result::DfamLiteratureReference>

=cut

__PACKAGE__->has_many(
  "dfam_literature_references",
  "DfamDB::Result::DfamLiteratureReference",
  { "foreign.pmid" => "self.pmid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:BEK9jce1r0Wdt/FdFY6cRQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
