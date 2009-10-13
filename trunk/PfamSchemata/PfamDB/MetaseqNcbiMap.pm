package PfamDB::MetaseqNcbiMap;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("metaseq_ncbi_map");
__PACKAGE__->add_columns(
  "gi",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_metaseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("gi", "PfamDB::NcbiSeq", { gi => "gi" });
__PACKAGE__->belongs_to(
  "auto_metaseq",
  "PfamDB::Metaseq",
  { auto_metaseq => "auto_metaseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:y/NuFUezv0JeCCiKzZXoOA


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
