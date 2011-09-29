package RfamDB::GenomeBigbed;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components('Core');
__PACKAGE__->table('genome_bigbed');
__PACKAGE__->add_columns(
  'ncbi_id',
  { data_type => 'INT', default_value => undef, is_nullable => 0, size => 10 },
  "code", 
  { 
    data_type => "MEDIUMTEXT", 
    default_value => undef, 
    is_nullable => 0, 
    size => 16777215, 
  },  
  'bigbed',
  {
    data_type => 'MEDIUMBLOB',
    default_value => undef,
    is_nullable => 0,
    size => 16777215,
  },
);

__PACKAGE__->set_primary_key( 'ncbi_id' );

__PACKAGE__->belongs_to(
  'genome',
  'RfamDB::GenomeEntry',
  { ncbi_id => 'ncbi_id' },
);

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Sarah Burge, C<sb30@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2011: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Sarah Burge (sb30@sanger.ac.uk), 
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
