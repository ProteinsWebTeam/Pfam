use utf8;
package PfamDB::PfamaRegFullInsignificant;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamaRegFullInsignificant

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_reg_full_insignificant>

=cut

__PACKAGE__->table("pfamA_reg_full_insignificant");

=head1 ACCESSORS

=head2 auto_pfama_reg_full

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 auto_pfamseq

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 ali_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 ali_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 model_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 model_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 domain_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 domain_evalue_score

  data_type: 'double precision'
  is_nullable: 0

=head2 sequence_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 sequence_evalue_score

  data_type: 'double precision'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "auto_pfama_reg_full",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "auto_pfamseq",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "ali_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "ali_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "model_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "model_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "domain_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "domain_evalue_score",
  { data_type => "double precision", is_nullable => 0 },
  "sequence_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "sequence_evalue_score",
  { data_type => "double precision", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_pfama_reg_full>

=back

=cut

__PACKAGE__->set_primary_key("auto_pfama_reg_full");

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamDB::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamDB::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2021-03-09 10:30:27
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3O0JZgeXBsEQbWlc5R7iQA


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
