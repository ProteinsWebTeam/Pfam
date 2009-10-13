package PfamDB::FunctionalSimilarity;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("functional_similarity");
__PACKAGE__->add_columns(
  "auto_pfama_a",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfama_b",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "rfunsim",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "mfscore",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "bpscore",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i4JVvRESIFggxDSrB6PAgQ


__PACKAGE__->has_one( 
  "auto_pfama",
  "PfamDB::Pfama",
  { 'foreign.auto_pfama' => 'self.auto_pfama_b' },
);

# __PACKAGE__->might_have( 
#   "auto_clan",
#   "PfamDB::ClanMembership",
#   { 'foreign.auto_pfama' => 'self.auto_pfama_b' },
# );


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
