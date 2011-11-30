package PfamDB::ClanMembership;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_membership");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 4 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("auto_clan", "PfamDB::Clans", { auto_clan => "auto_clan" });
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamDB::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:u4W7m2GV9KpECd/4L0JpGg


__PACKAGE__->add_unique_constraint("clanMembConst", [qw(auto_clan auto_pfama)]);

__PACKAGE__->set_primary_key( 'auto_clan' );

__PACKAGE__->has_many(
  "pdb_pfama_reg",
  "PfamDB::PdbPfamaReg",
  { "foreign.auto_pfama" => "self.auto_pfama" }
);

__PACKAGE__->belongs_to("clan", "PfamDB::Clans", 'auto_clan' );

          
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
