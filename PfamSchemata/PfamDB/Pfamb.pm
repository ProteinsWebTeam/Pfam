package PfamDB::Pfamb;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB");
__PACKAGE__->add_columns(
  "auto_pfamb",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 6 },
  "pfamb_acc",
  { data_type => "CHAR", default_value => undef, is_nullable => 0, size => 8 },
  "pfamb_id",
  { data_type => "CHAR", default_value => undef, is_nullable => 0, size => 15 },
  "number_archs",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_regions",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "number_species",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_structures",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
);
__PACKAGE__->set_primary_key("auto_pfamb");
__PACKAGE__->has_many(
  "pdb_pfamb_regs",
  "PfamDB::PdbPfambReg",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb2pfama_prc_results",
  "PfamDB::Pfamb2pfamaPrcResults",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_database_links",
  "PfamDB::PfambDatabaseLinks",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_fastas",
  "PfamDB::PfambFasta",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_regs",
  "PfamDB::PfambReg",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_stockholms",
  "PfamDB::PfambStockholm",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:66AT/7dRzxlUUAGTsmogwg

__PACKAGE__->has_many(
  "pfamb_species_trees",
  "PfamDB::PfambSpeciesTree",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
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
