use utf8;
package PfamDB::SeqInfo;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::SeqInfo

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<seq_info>

=cut

__PACKAGE__->table("seq_info");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 description

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_description

  data_type: 'text'
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_description",
  { data_type => "text", is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:4fwbKoTHuvJDJs3gXSob/A


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
