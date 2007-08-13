#
# JobHistory
# rdf 20070405 WTSI
#
# Model for the job_history table.
#
# $Id: JobHistory.pm,v 1.4 2007-08-13 14:46:02 rdf Exp $
#
# $Author: rdf $

package WebUser::JobHistory;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# set up the table
__PACKAGE__->table( 'job_history' );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw( id
                              job_id
                              status
                              options
                              estimated_time
                              opened
                              closed
                              started
                              job_type
                              email )
                        );

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( "id" );

# relationships

__PACKAGE__->has_one( job_stream => 'WebUser::JobStream',
                      { 'foreign.id' => 'self.id' },
                      { proxy            => [ qw( stdin
                                                  stdout
                                                  stderr ) ] }
                    );

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
