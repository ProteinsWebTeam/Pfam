use utf8;
package PfamDB::ProteomeRegions;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::ProteomeRegions

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<proteome_regions>

=cut

__PACKAGE__->table("proteome_regions");

=head1 ACCESSORS

=head2 auto_proteome

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 count

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "auto_proteome",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "count",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 auto_proteome

Type: belongs_to

Related object: L<PfamDB::CompleteProteomes>

=cut

__PACKAGE__->belongs_to(
  "auto_proteome",
  "PfamDB::CompleteProteomes",
  { auto_proteome => "auto_proteome" },
);

=head2 auto_proteome_2

Type: belongs_to

Related object: L<PfamDB::CompleteProteomes>

=cut

__PACKAGE__->belongs_to(
  "auto_proteome_2",
  "PfamDB::CompleteProteomes",
  { auto_proteome => "auto_proteome" },
);

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamDB::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamDB::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tf+KT7vv8VjEnNJROGZicQ


__PACKAGE__->has_many(
  "regions",
  "PfamDB::PfamaRegFullSignificant",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc",
    "foreign.pfama_acc"   => "self.pfama_acc" },
);


=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


1;
