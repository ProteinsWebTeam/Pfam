use utf8;
package PfamDB::PfamaInternal;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamaInternal

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_pfamA_internal>

=cut

__PACKAGE__->table("_pfamA_internal");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 created_by

  data_type: 'tinytext'
  is_nullable: 1

=head2 iterated

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 iteration_gain

  data_type: 'integer'
  default_value: 0
  is_nullable: 1

=head2 iterated_by

  data_type: 'tinytext'
  is_nullable: 1

=head2 iteration_date

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 seed

  data_type: 'longblob'
  is_nullable: 1

=head2 full

  data_type: 'longblob'
  is_nullable: 1

=head2 seed_is_ref_proteome

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "created_by",
  { data_type => "tinytext", is_nullable => 1 },
  "iterated",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "iteration_gain",
  { data_type => "integer", default_value => 0, is_nullable => 1 },
  "iterated_by",
  { data_type => "tinytext", is_nullable => 1 },
  "iteration_date",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "seed",
  { data_type => "longblob", is_nullable => 1 },
  "full",
  { data_type => "longblob", is_nullable => 1 },
  "seed_is_ref_proteome",
  { data_type => "tinyint", default_value => 0, is_nullable => 1 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:45dpGdN6c/3O5RukZmi87Q


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
