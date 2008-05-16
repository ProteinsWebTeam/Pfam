package RfamDB::Version; use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("VERSION");
__PACKAGE__->add_columns(
  "rfam_release",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "rfam_release_date",
  { data_type => "DATE", default_value => "", is_nullable => 0, size => 10 },
  "number_families",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "embl_release",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-02-29 10:23:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hqUi9Pe4eMvno+ecdRi0iw

__PACKAGE__->set_primary_key( qw( rfam_release 
                                  embl_release ) );

# You can replace this text with custom content, and it will be preserved on regeneration

=head1 AUTHOR

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
