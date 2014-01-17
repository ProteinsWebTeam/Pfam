
package WebUser::ResultSet::JobHistory;
 
use strict;
use warnings;
 
use base 'DBIx::Class::ResultSet';

sub get_pending_or_running_jobs {
  my ( $self, $job_type ) = @_;

  $self->search( { status   => [ 'PEND', 'RUN' ],
                   job_type => $job_type },
                 { join     => [ 'job_stream' ],
                   order_by => 'me.id ASC' } );
}
 
sub get_pending_jobs {
  my ( $self, $job_type ) = @_;

  $self->search( { status   => 'PEND',
                   job_type => $job_type },
                 { join     => [ 'job_stream' ],
                   order_by => 'me.id ASC' } );
}
 
sub get_running_jobs {
  my ( $self, $job_type ) = @_;

  $self->search( { status   => 'RUN',
                   job_type => $job_type },
                 { join     => [ 'job_stream' ],
                   order_by => 'me.id ASC' } );
}
 
sub get_chunks_for_job {
  my ( $self, $job_id ) = @_;

  $self->search( { job_type => 'chunk',
                   job_id   => $job_id },
                 { join     => [ 'job_stream' ],
                   order_by => 'me.id ASC' } );
}
 
sub get_running_chunks_for_job {
  my ( $self, $job_id ) = @_;

  $self->search( { status   => 'RUN',
                   job_type => 'chunk',
                   job_id   => $job_id },
                 { join     => [ 'job_stream' ],
                   order_by => 'me.id ASC' } );
}
 
1;
