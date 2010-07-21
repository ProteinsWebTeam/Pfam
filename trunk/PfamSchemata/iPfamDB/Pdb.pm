package iPfamDB::Pdb;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb");
__PACKAGE__->add_columns(
  "pdb_id",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 0,
    size => 5,
  },
  "keywords",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "title",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "date",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "resolution",
  { data_type => "DECIMAL", default_value => "", is_nullable => 0, size => 5 },
  "method",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "pubmed_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("pdb_id");
__PACKAGE__->add_unique_constraint("UQ_pdb_1", ["pdb_id"]);
__PACKAGE__->has_many(
  "pdb_chain_datas",
  "iPfamDB::PdbChainData",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_images",
  "iPfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_residue_datas",
  "iPfamDB::PdbResidueData",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "trackings",
  "iPfamDB::Tracking",
  { "foreign.pdb_id" => "self.pdb_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 15:16:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:4omxpjlFiGVikhTl62Ri9A
# These lines were loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Pdb.pm' found in @INC.# They are now part of the custom portion of this file# for you to hand-edit.  If you do not either delete# this section or remove that file from @INC, this section# will be repeated redundantly when you re-create this# file again via Loader!
package iPfamDB::Pdb;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "pdb_id",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 0,
    size => 5,
  },
  "header",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "title",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "date",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "resolution",
  { data_type => "DECIMAL", default_value => "", is_nullable => 0, size => 5 },
  "experiment_short",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "experiment_long",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "pubmed_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "biological_unit",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->add_unique_constraint("UQ_pdb_1", ["pdb_id"]);
__PACKAGE__->has_many(
  "pdb_authors",
  "iPfamDB::PdbAuthor",
  { "foreign.accession" => "self.accession" },
);
__PACKAGE__->has_many(
  "pdb_chain_datas",
  "iPfamDB::PdbChainData",
  { "foreign.accession" => "self.accession" },
);
__PACKAGE__->has_many(
  "pdb_images",
  "iPfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_residue_datas",
  "iPfamDB::PdbResidueData",
  { "foreign.accession" => "self.accession" },
);
__PACKAGE__->has_many(
  "trackings",
  "iPfamDB::Tracking",
  { "foreign.pdb_id" => "self.pdb_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:24Dtjphpn/jaVcUdc1sTWA

__PACKAGE__->might_have(
  "pdb_image",
  "iPfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);

__PACKAGE__->has_many(
  "pdb_chains",
  "iPfamDB::PdbChainData",
  { "foreign.pdb_id" => "self.pdb_id" },
);
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
# End of lines loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Pdb.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
