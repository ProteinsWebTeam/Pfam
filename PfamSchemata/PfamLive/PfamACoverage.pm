
# $Id: PfamACoverage.pm,v 1.1 2009-01-06 11:28:27 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::PfamACoverage;

use strict;
use warnings;

use base 'DBIx::Class';

=head2 Table definition

+-------------------+------------------+------+-----+-------------------+-------+
| Field             | Type             | Null | Key | Default           | Extra |
+-------------------+------------------+------+-----+-------------------+-------+
| sequences_covered | int(10) unsigned | YES  | MUL | NULL              |       |
| all_sequences     | int(10) unsigned | YES  |     | NULL              |       |
| residues_covered  | int(10) unsigned | YES  | MUL | NULL              |       |
| all_residues      | int(10) unsigned | YES  |     | NULL              |       |
| entry_date        | timestamp        | NO   | PRI | CURRENT_TIMESTAMP |       |
+-------------------+------------------+------+-----+-------------------+-------+

=cut

__PACKAGE__->load_components( qw( InflateColumn::DateTime Core ) );

__PACKAGE__->table( '_pfamA_coverage' );

__PACKAGE__->add_columns(
  "sequences_covered",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 10 },
  "all_sequences",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 10 },
  "residues_covered",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 10 },
  "all_residues",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 10 },
  "entry_date",
  { data_type => "datetime", default_value => "", is_nullable => 0 },
);

__PACKAGE__->set_primary_key( 'entry_date' );

1;
