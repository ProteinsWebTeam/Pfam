package RfamDB::Wikitext;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wikitext");
__PACKAGE__->add_columns(
  "auto_wiki",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "text",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 4294967295,
  },
  "title",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 150 },
);
__PACKAGE__->set_primary_key("auto_wiki");
__PACKAGE__->has_many(
  "rfams",
  "RfamDB::Rfam",
  { "foreign.auto_wiki" => "self.auto_wiki" },
);
__PACKAGE__->belongs_to("auto_wiki", "RfamDB::Wikitext", { auto_wiki => "auto_wiki" });
__PACKAGE__->has_many(
  "wikitexts",
  "RfamDB::Wikitext",
  { "foreign.auto_wiki" => "self.auto_wiki" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:31
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:cxBOTXCpcb8mm/R6IrnB7w

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
