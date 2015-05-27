use utf8;
package PfamDB::PfamseqMarkup;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamseqMarkup

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamseq_markup>

=cut

__PACKAGE__->table("pfamseq_markup");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 auto_markup

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 residue

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 annotation

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "auto_markup",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "residue",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "annotation",
  { data_type => "text", is_nullable => 1 },
);

=head1 RELATIONS

=head2 auto_markup

Type: belongs_to

Related object: L<PfamDB::MarkupKey>

=cut

__PACKAGE__->belongs_to(
  "auto_markup",
  "PfamDB::MarkupKey",
  { auto_markup => "auto_markup" },
);

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamDB::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamDB::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZiJG80QOLrqibLUSfA0ywg


__PACKAGE__->set_primary_key( qw/pfamseq_acc auto_markup residue/ );

#__PACKAGE__->has_one(
#  'pfamseqs',
#  'PfamDB::Pfamseq',
#  { 'foreign.auto_pfamseq' => 'self.auto_pfamseq' },
#  { cascade_delete => 0 }
#);

#__PACKAGE__->might_have(
#  'pfama_reg_full_significants',
#  'PfamDB::PfamaRegFullSignificant',
#  { 'foreign.auto_pfamseq' => 'self.auto_pfamseq' },
#  { cascade_delete => 0 }
#);

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
