package RfamDB::RfamRefseq;

use strict;
use warnings;

use base 'DBIx::Class';

# CREATE TABLE `rfam_refseq` (
#   `rfam_acc` varchar(7) NOT NULL,
#   `refseq_accession` char(50) NOT NULL,
#   `bits_score` double(7,2) NOT NULL default '0.00',
#   `evalue_score` varchar(15) NOT NULL default '0',
#   `refseq_start` bigint(20) default NULL,
#   `refseq_end` bigint(20) default NULL,
#   `topology` enum('linear','circular','-') NOT NULL,
#   `ncbi_id` int(10) unsigned NOT NULL default '0',
#   `description` text,
#   `species` varchar(100) default NULL,
#   KEY `rfam_acc` (`rfam_acc`),
#   KEY `refseq_accession` (`refseq_accession`),
#   KEY `ncbi_id` (`ncbi_id`)
# ) ENGINE=MyISAM DEFAULT CHARSET=latin1
# 
# +------------------+-------------------------------+------+-----+---------+-------+
# | Field            | Type                          | Null | Key | Default | Extra |
# +------------------+-------------------------------+------+-----+---------+-------+
# | rfam_acc         | varchar(7)                    | NO   | MUL | NULL    |       | 
# | refseq_accession | char(50)                      | NO   | MUL | NULL    |       | 
# | bits_score       | double(7,2)                   | NO   |     | 0.00    |       | 
# | evalue_score     | varchar(15)                   | NO   |     | 0       |       | 
# | refseq_start     | bigint(20)                    | YES  |     | NULL    |       | 
# | refseq_end       | bigint(20)                    | YES  |     | NULL    |       | 
# | topology         | enum('linear','circular','-') | NO   |     | NULL    |       | 
# | ncbi_id          | int(10) unsigned              | NO   | MUL | 0       |       | 
# | description      | text                          | YES  |     | NULL    |       | 
# | species          | varchar(100)                  | YES  |     | NULL    |       | 
# +------------------+-------------------------------+------+-----+---------+-------+

__PACKAGE__->load_components('Core');
__PACKAGE__->table('rfam_refseq');

__PACKAGE__->add_columns(
  'rfam_acc',
  { data_type => 'VARCHAR', default_value => '', is_nullable => 0, size => 7 },
  'refseq_accession',
  { data_type => 'CHAR', default_value => '', is_nullable => 0, size => 50 },
  'bits_score',
  { data_type => 'DOUBLE', default_value => '0.00', is_nullable => 0, size => 64 },
  'evalue_score',
  { data_type => 'VARCHAR', default_value => 0, is_nullable => 0, size => 15 },
  'refseq_start',
  { data_type => 'BIGINT', default_value => undef, is_nullable => 1, size => 20 },
  'refseq_end',
  { data_type => 'BIGINT', default_value => undef, is_nullable => 1, size => 20 },
  'topology',
  { data_type => 'ENUM', default_value => undef, is_nullable => 0, size => 8 },
  'ncbi_id',
  { data_type => 'INT', default_value => '', is_nullable => 0, size => 10 },
  'description',
  { data_type => 'TEXT', default_value => undef, is_nullable => 1, size => 65535 },
  'species',
  { data_type => 'VARCHAR', default_value => undef, is_nullable => 1, size => 100 },
);

__PACKAGE__->add_unique_constraint('rfam_acc', ['rfam_acc']);

__PACKAGE__->belongs_to('rfam', 'RfamDB::Rfam', { rfam_acc => 'rfam_acc' });

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Sarah Burge, C<sb30@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012: Genome Research Ltd.

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
