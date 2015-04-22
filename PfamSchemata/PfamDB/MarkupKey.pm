use utf8;
package PfamDB::MarkupKey;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::MarkupKey

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<markup_key>

=cut

__PACKAGE__->table("markup_key");

=head1 ACCESSORS

=head2 auto_markup

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 label

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=cut

__PACKAGE__->add_columns(
  "auto_markup",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "label",
  { data_type => "varchar", is_nullable => 1, size => 50 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_markup>

=back

=cut

__PACKAGE__->set_primary_key("auto_markup");

=head1 RELATIONS

=head2 pfamseq_markups

Type: has_many

Related object: L<PfamDB::PfamseqMarkup>

=cut

__PACKAGE__->has_many(
  "pfamseq_markups",
  "PfamDB::PfamseqMarkup",
  { "foreign.auto_markup" => "self.auto_markup" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:J/2i8wfLNfzQ6KOnOiMptw


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
