package RfamDB::GenomeEntry;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("genome_entry");
__PACKAGE__->add_columns(
  "auto_genome",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "genome_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "ensembl_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "description",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "ncbi_id",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "circular",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 3 },
  "length",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
);
__PACKAGE__->set_primary_key("auto_genome");
__PACKAGE__->has_many(
  "chromosome_builds",
  "RfamDB::ChromosomeBuild",
  { "foreign.auto_genome" => "self.auto_genome" },
);
__PACKAGE__->has_many(
  "genome_gffs",
  "RfamDB::GenomeGff",
  { "foreign.auto_genome" => "self.auto_genome" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:1mjRIQBxbIo8Csh8Pf/9pw

__PACKAGE__->has_one(
  "bigbed",
  "RfamDB::GenomeBigbed",
  { "foreign.ncbi_id" => "self.ncbi_id" },
);

__PACKAGE__->has_many(
  "regions",
  "RfamDB::RfamRegFull",
  { "foreign.auto_genome" => "self.auto_genome" }, 
);

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
