
package RfamJobs::ResultSet::JobHistory;

use strict;
use warnings;

use base 'DBIx::Class::ResultSet';

use Carp;

sub get_pending_jobs {
  my ( $self, $job_type ) = @_;

  carp 'must supply a job type'
    unless defined $job_type;

  $self->search( { status   => 'PEND',
                   job_type => $job_type },
                 { join     => [ 'job_streams' ],
                   order_by => 'me.id ASC' } );
}

1;

