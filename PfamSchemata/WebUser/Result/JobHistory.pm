use utf8;
package WebUser::Result::JobHistory;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

WebUser::Result::JobHistory

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<job_history>

=cut

__PACKAGE__->table("job_history");

=head1 ACCESSORS

=head2 id

  data_type: 'bigint'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 job_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 ebi_id

  data_type: 'varchar'
  is_nullable: 1
  size: 60

=head2 status

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 5

=head2 options

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 1
  size: 255

=head2 estimated_time

  data_type: 'integer'
  is_nullable: 1

=head2 opened

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  default_value: '1970-01-01 00:00:00'
  is_nullable: 0

=head2 closed

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  default_value: '1970-01-01 00:00:00'
  is_nullable: 0

=head2 started

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  default_value: '1970-01-01 00:00:00'
  is_nullable: 0

=head2 job_type

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 50

=head2 email

  data_type: 'varchar'
  is_nullable: 1
  size: 255

=cut

__PACKAGE__->add_columns(
  "id",
  {
    data_type => "bigint",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "job_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "ebi_id",
  { data_type => "varchar", is_nullable => 1, size => 60 },
  "status",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 5 },
  "options",
  { data_type => "varchar", default_value => "", is_nullable => 1, size => 255 },
  "estimated_time",
  { data_type => "integer", is_nullable => 1 },
  "opened",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    default_value => "1970-01-01 00:00:00",
    is_nullable => 0,
  },
  "closed",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    default_value => "1970-01-01 00:00:00",
    is_nullable => 0,
  },
  "started",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    default_value => "1970-01-01 00:00:00",
    is_nullable => 0,
  },
  "job_type",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 50 },
  "email",
  { data_type => "varchar", is_nullable => 1, size => 255 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2021-07-14 21:37:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Bow3I/Heu7tI/hM9jKuLRw
# These lines were loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/JobHistory.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

#
# JobHistory
# rdf 20070405 WTSI
#
# Model for the job_history table.
#
# $Id: JobHistory.pm,v 1.5 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package WebUser::Result::JobHistory;

use strict;
use warnings;

use base 'DBIx::Class::Core';

use WebUser::DateTime;

__PACKAGE__->load_components( qw( InflateColumn::DateTime ) );

# use our customised DateTime class when converting DateTime values. The
# subclass adds a TO_JSON method, so that the DateTime can be serialised
# to JSON
sub _inflate_to_datetime {
   my $self = shift;
   my $val = $self->next::method(@_);

   return bless $val, 'WebUser::DateTime';
}

# set up the table
__PACKAGE__->table( 'job_history' );

# get the columns that we want to keep
__PACKAGE__->add_columns( 
  "id",
  { 
    data_type => "BIGINT", 
    default_value => undef,
    is_auto_increment => 1,
    is_nullable => 0,
    size => 20,
  },
  "job_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 40,
  },
  "ebi_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 60,
  },
  "status",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "options",
  { data_type => "VARCHAR", default_value => "", is_nullable => 1, size => 255 },
  "estimated_time",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 3 },
  "opened",
  {
    data_type => "DATETIME",
    default_value => "1970-01-01 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "closed",
  {
    data_type => "DATETIME",
    default_value => "1970-01-01 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "started",
  {
    data_type => "DATETIME",
    default_value => "1970-01-01 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "job_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
  "email",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( "id" );

# relationships

__PACKAGE__->has_one( job_stream => 'WebUser::Result::JobStream',
                      { 'foreign.id' => 'self.id' },
                      { proxy            => [ qw( stdin
                                                  stdout
                                                  stderr ) ] }
                    );

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
# End of lines loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/JobHistory.pm'


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
