package iPfamDB::Pfama;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfama");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "pfama_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "numberinalign",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "description",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "domCount",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 10 },
  "ligCount",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 10 },
  "naCount",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 10 },
);
__PACKAGE__->set_primary_key("pfama_acc");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:0J5+9nwjyJrY8lIDl1JIhw



=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

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

#SELECT me.pfama_acc, me.pfama_id, me.numberinalign, me.description, me.domCount, me.ligCount, me.naCount 
#FROM   pfama me 
#WHERE  ( 
#    (
#      ( pfama_id LIKE ? )
#      AND
#      ( 
#        (
#          ( domCount > ? ) OR 
#          ( ligCount > ? ) OR 
#          ( naCount > ? ) 
#        )
#      )
#    )
#  )
#ORDER BY pfama_id"
