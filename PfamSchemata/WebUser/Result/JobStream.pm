use utf8;
package WebUser::Result::JobStream;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

WebUser::Result::JobStream

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<job_stream>

=cut

__PACKAGE__->table("job_stream");

=head1 ACCESSORS

=head2 id

  data_type: 'bigint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 stdin

  data_type: 'longtext'
  is_nullable: 0

=head2 stdout

  data_type: 'mediumblob'
  is_nullable: 1

=head2 stderr

  data_type: 'longtext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "id",
  {
    data_type => "bigint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "stdin",
  { data_type => "longtext", is_nullable => 0 },
  "stdout",
  { data_type => "mediumblob", is_nullable => 1 },
  "stderr",
  { data_type => "longtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2021-07-14 21:37:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:NZKVPggYiMD955t+9iv8oQ
# These lines were loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/JobStream.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

#
# JobStream
# rdf 20070405 WTSI
#
# Model for the job_stream table.
#
# $Id: JobStream.pm,v 1.4 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package WebUser::Result::JobStream;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# set up the table
__PACKAGE__->table( 'job_stream' );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw( id stdin stdout stderr ) );

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( 'id' );

# relationships

__PACKAGE__->has_one( job_history => 'WebUser::Result::JobHistory',
            { 'foreign.id' => "self.id"},
            { proxy            => [ qw( job_id
                                        job_type
                                        status
                                        options
                                        email
                                        estimated_time
                                        opened
                                        closed
                                        started ) ] } );

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
# End of lines loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/JobStream.pm'


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
