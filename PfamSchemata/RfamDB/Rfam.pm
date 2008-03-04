package RfamDB::Rfam;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "rfam_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "description",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "model_length",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "author",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "seed_source",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "alignment_method",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "gathering_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "trusted_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "noise_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "previous_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "cmbuild",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "cmcalibrate",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "num_seed",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "num_full",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "type",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 50,
  },
  "structure_source",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->set_primary_key("auto_rfam");
__PACKAGE__->add_unique_constraint("rfam_acc", ["rfam_acc"]);
__PACKAGE__->has_many(
  "rfam_database_links",
  "RfamDB::RfamDatabaseLinks",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_literature_references",
  "RfamDB::RfamLiteratureReferences",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_reg_fulls",
  "RfamDB::RfamRegFull",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_reg_seeds",
  "RfamDB::RfamRegSeed",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "wikis",
  "RfamDB::Wiki",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-02-29 10:23:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:o3jk/8YwI1dKr8BkKHzwLg


#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>
Rob Finn, C<rdf@sanger.ac.uk>
Paul Gardner, C<pg5@sanger.ac.uk>
Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk),
         Paul Gardner, C<pg5@sanger.ac.uk>, Jennifer Daub, C<jd7@sanger.ac.uk>

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
