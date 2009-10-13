package PfamDB::CompleteProteomes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("complete_proteomes");
__PACKAGE__->add_columns(
  "auto_proteome",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "ncbi_taxid",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "species",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "grouping",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "num_distinct_regions",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "num_total_regions",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "num_proteins",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "sequence_coverage",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 3 },
  "residue_coverage",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 3 },
  "total_genome_proteins",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "total_aa_length",
  { data_type => "BIGINT", default_value => 0, is_nullable => 0, size => 20 },
  "total_aa_covered",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "total_seqs_covered",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
);
__PACKAGE__->set_primary_key("auto_proteome");
__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamDB::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
);
__PACKAGE__->has_many(
  "genome_pfamseqs",
  "PfamDB::GenomePfamseq",
  { "foreign.auto_proteome" => "self.auto_proteome" },
);
__PACKAGE__->has_many(
  "proteome_regions",
  "PfamDB::ProteomeRegions",
  { "foreign.auto_proteome" => "self.auto_proteome" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:QkdbdVnaLqAnja5gvMiQZA


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
