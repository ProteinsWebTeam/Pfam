use utf8;
package RfamLive::Result::GenomeGff;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::GenomeGff

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<genome_gff>

=cut

__PACKAGE__->table("genome_gff");

=head1 ACCESSORS

=head2 auto_genome

  data_type: 'integer'
  is_nullable: 1

=head2 gff3

  data_type: 'longblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_genome",
  { data_type => "integer", is_nullable => 1 },
  "gff3",
  { data_type => "longblob", is_nullable => 1 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:NROubIhOGxQ6zl+vibSYyw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
