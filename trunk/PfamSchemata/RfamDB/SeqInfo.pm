package RfamDB::SeqInfo;

use strict;
use warnings;

use base 'DBIx::Class';

# CREATE TABLE `seq_info` (
#   `auto_rfam` int(10) unsigned default NULL,
#   `auto_rfamseq` int(10) unsigned NOT NULL default '0',
#   `rfam_acc` varchar(7) NOT NULL,
#   `rfam_id` varchar(40) NOT NULL,
#   `rfamseq_acc` varchar(13) NOT NULL default '',
#   `version` varchar(12) default NULL,
#   `rfamseq_acc_v` varchar(26) default NULL,
#   `description` varchar(250) NOT NULL default '',
#   `sequence` longtext,
#   `seq_start` bigint(19) NOT NULL default '0',
#   `seq_end` bigint(19) default NULL
# ) ENGINE=InnoDB DEFAULT CHARSET=latin1

# +---------------+------------------+------+-----+---------+-------+
# | Field         | Type             | Null | Key | Default | Extra |
# +---------------+------------------+------+-----+---------+-------+
# | auto_rfam     | int(10) unsigned | YES  |     | NULL    |       | 
# | auto_rfamseq  | int(10) unsigned | NO   |     | 0       |       | 
# | rfam_acc      | varchar(7)       | NO   |     |         |       | 
# | rfam_id       | varchar(40)      | NO   |     |         |       | 
# | rfamseq_acc   | varchar(13)      | NO   |     |         |       | 
# | version       | varchar(12)      | YES  |     | NULL    |       | 
# | rfamseq_acc_v | varchar(26)      | YES  |     | NULL    |       | 
# | description   | varchar(250)     | NO   |     |         |       | 
# | sequence      | longtext         | YES  |     | NULL    |       | 
# | seq_start     | bigint(19)       | NO   |     | 0       |       | 
# | seq_end       | bigint(19)       | YES  |     | NULL    |       | 
# +---------------+------------------+------+-----+---------+-------+

__PACKAGE__->load_components('Core');
__PACKAGE__->table('seq_info');

__PACKAGE__->add_columns(
  'auto_rfam',
  { data_type => 'INT', default_value => undef, is_nullable => 1, size => 10 },
  'auto_rfamseq',
  { data_type => 'INT', default_value => 0, is_nullable => 0, size => 10 },
  'rfam_acc',
  { data_type => 'VARCHAR', default_value => '', is_nullable => 0, size => 7 },
  'rfam_id',
  { data_type => 'VARCHAR', default_value => '', is_nullable => 0, size => 40 },
  'rfamseq_acc',
  { data_type => 'VARCHAR', default_value => '', is_nullable => 0, size => 13 },
  'version',
  { data_type => 'VARCHAR', default_value => undef, is_nullable => 1, size => 12 },
  'rfamseq_acc_v',
  { data_type => 'VARCHAR', default_value => undef, is_nullable => 1, size => 26 },
  'description',
  { data_type => 'VARCHAR', default_value => '', is_nullable => 0, size => 250 },
  'sequence',
  {
    data_type => 'LONGTEXT',
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  'seq_start',
  { data_type => 'BIGINT', default_value => 0, is_nullable => 0, size => 19 },
  'seq_end',
  { data_type => 'BIGINT', default_value => undef, is_nullable => 1, size => 19 },
);

__PACKAGE__->add_unique_constraint('rfam_acc', ['rfam_acc']);

__PACKAGE__->belongs_to('rfam', 'RfamDB::Rfam', { auto_rfam => 'auto_rfam' });
__PACKAGE__->belongs_to('rfamseq', 'RfamDB::Rfamseq', { auto_rfamseq => 'auto_rfamseq' });

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
