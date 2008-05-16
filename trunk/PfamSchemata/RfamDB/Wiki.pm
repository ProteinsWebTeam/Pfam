package RfamDB::Wiki;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wiki");
__PACKAGE__->add_columns(
  "auto_wiki",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "title",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 150 },
);
__PACKAGE__->belongs_to("auto_wiki", "RfamDB::Wikitext", { auto_wiki => "auto_wiki" });
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-02-29 10:23:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LZa+tNhIZ26Dojg44qdEDQ

__PACKAGE__->belongs_to(
  "wikitext",
  "RfamDB::Wikitext",
  { auto_wiki => "auto_wiki" },
  { proxy    => [ qw( text ) ] },
);

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
