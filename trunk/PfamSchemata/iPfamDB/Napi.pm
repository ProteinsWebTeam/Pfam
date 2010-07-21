package iPfamDB::Napi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("napi");
__PACKAGE__->add_columns(
  "napi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "protein_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
);
__PACKAGE__->set_primary_key("napi");
__PACKAGE__->belongs_to(
  "nucleic_acid_acc",
  "iPfamDB::NucleicAcid",
  { accession => "nucleic_acid_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 15:16:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:uVxfpFphZ89fc8Nd3HqyBA
# These lines were loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Napi.pm' found in @INC.# They are now part of the custom portion of this file# for you to hand-edit.  If you do not either delete# this section or remove that file from @INC, this section# will be repeated redundantly when you re-create this# file again via Loader!
package iPfamDB::Napi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("napi");
__PACKAGE__->add_columns(
  "napi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "protein_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
);
__PACKAGE__->set_primary_key("napi");
__PACKAGE__->belongs_to(
  "nucleic_acid_acc",
  "iPfamDB::NucleicAcid",
  { accession => "nucleic_acid_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XCGYXa9fC1fxjumc70DeLA


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
# End of lines loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Napi.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
