use utf8;
package PfamDB::PfamAnnseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamAnnseq

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfam_annseq>

=cut

__PACKAGE__->table("pfam_annseq");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 annseq_storable

  data_type: 'mediumblob'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "annseq_storable",
  { data_type => "mediumblob", is_nullable => 0 },
);

=head1 RELATIONS

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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:y1reQiuOlTiDNDPUvpLApA


#__PACKAGE__->add_unique_constraint( 'UQ_pfam_annseq_1', [ 'auto_pfamseq' ] );
__PACKAGE__->set_primary_key( 'pfamseq_acc' );

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
