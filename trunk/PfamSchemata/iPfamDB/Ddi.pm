package iPfamDB::Ddi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ddi");
__PACKAGE__->add_columns(
  "ddi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "region_id_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->set_primary_key("ddi");
__PACKAGE__->belongs_to(
  "region_id_a",
  "iPfamDB::Domain",
  { region_id => "region_id_a" },
);
__PACKAGE__->belongs_to(
  "region_id_b",
  "iPfamDB::Domain",
  { region_id => "region_id_b" },
);
__PACKAGE__->has_many("ddi_res", "iPfamDB::DdiRes", { "foreign.ddi" => "self.ddi" });


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:I89xRKawtYQJEE+q67+V6w


# You can replace this text with custom content, and it will be preserved on regeneration

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

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
