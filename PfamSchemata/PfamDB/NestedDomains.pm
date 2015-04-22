use utf8;
package PfamDB::NestedDomains;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::NestedDomains

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<nested_domains>

=cut

__PACKAGE__->table("nested_domains");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 nests_pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "nests_pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
);

=head1 RELATIONS

=head2 nests_pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to(
  "nests_pfama_acc",
  "PfamDB::Pfama",
  { pfama_acc => "nests_pfama_acc" },
);

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Li1m6VAGp4vnrSpgl9XnFg


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
