
# EbiRestClient.pm
# jgt 20130612 EBI
#
# $Id$

=head1 NAME

Bio::Pfam::EbiRestClient - an interface to the services in the EBI web
services framework

=cut

package Bio::Pfam::EbiRestClient;

=head1 VERSION

Version 0.01

=cut

use Moose;
use Moose::Util::TypeConstraints;
use Log::Log4perl;
use LWP::UserAgent;
use URI;
use Carp;
use Params::Validate qw(:all);
use Scalar::Util qw( looks_like_number );
use Email::Valid;
use Time::HiRes;

our $VERSION = '0.01';

=head1 SYNOPSIS

This module provides a wrapper around the RESTful web services in the EBI 
External Services framework. It can POST searches and GET the status and 
results for submitted jobs.

    use Bio::Pfam::EbiRestClient;


    # either pass in the base URL for the service
    my $erc = Bio::Pfam::EbiRestClient->new( { base_url => '...' } );

    #or set it explicitly
    $erc->base_url( 'http://www.ebi.ac.uk/Tools/services/rest/pfamscan' );

    my $search_options = {
      sequence => 'MAGAASPCANGCGPSAPSDAEVVHLCRSLE...',
    };

    my $job_id = $erc->search( $search_options );

    # wait...

    # check job status
    my $status = $erc->status( $job_id );

    # if done...
    my $results = $erc->result( $job_id );

=cut

#-------------------------------------------------------------------------------
#- logging ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

BEGIN {
  my $logger_conf = q(
    log4perl.logger                                   = WARN, Screen
    log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.layout                   = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.Screen.layout.ConversionPattern = %d %M:%L %p: %m%n
  );

  Log::Log4perl->init( \$logger_conf );
}

has '_log' => (
  is      => 'ro',
  isa     => 'Log::Log4perl::Logger',
  lazy    => 1,
  default => sub {
    my $self = shift;
    return Log::Log4perl->get_logger( ref $self );
  }
);

#-------------------------------------------------------------------------------
#- accessors -------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 ACCESSORS

=head2 _ua

Read-only. Returns a L<LWP::UserAgent>.

=cut

has '_ua' => (
  is      => 'ro',
  isa     => 'LWP::UserAgent',
  lazy    => 1,
  default => sub {
    my $ua = LWP::UserAgent->new;
    $ua->env_proxy;
    $ua->agent( 'Mozilla/5.001 (windows; U; NT4.0; en-us) Gecko/25250101' );
    return $ua;
  }
);

#---------------------------------------
# allows us to accept URIs as strings

subtype 'Bio::Pfam::Types::URI' => as class_type('URI::http');

coerce 'Bio::Pfam::Types::URI'
  => from 'Str'
  => via { URI->new($_) };

has 'base_url' => (
  is        => 'rw',
  isa       => 'Bio::Pfam::Types::URI',
  coerce    => 1
);

#---------------------------------------
# the encodings that are accepted by the installed HTTP::Message

has '_can_accept' => (
  is => 'ro',
  isa => 'Str',
  default => sub {
    my $can_accept = '';
    eval {
      $can_accept = HTTP::Message::decodable();
    };
    return $can_accept;
  }
);

#---------------------------------------
# convenience attribute to store accepted HTTP methods

has '_accepted_methods' => (
  is => 'ro',
  isa => 'HashRef',
  default => sub {
    my %methods = map { $_ => 1 } qw( PUT POST GET );
    return \%methods;
  }
);

#---------------------------------------
# regular expressions for validating parameters

# job ID
has '_valid_id_re' => (
  is => 'ro',
  default => sub { qr/^[a-z_]+-[A-Z]\d{8}-(\d+-)+(hx|oy|pg|es)$/ }
);

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 status

Retrieves the status of the specified job.

=cut

sub status {
  my $self = shift;
  my $params = validate_pos(
    @_, {
      jobid => { type  => SCALAR,
                 regex => $self->_valid_id_re }
    }
  );

  my $response = $self->_request( {
    path => [ 'status', $params->[0] ]
  } );

  return $response;
}

#-------------------------------------------------------------------------------

=head2 result

Retrieves the result of the specified job.

=cut

sub result {
  my $self = shift;
  my $params = validate_pos(
    @_,
    { # job ID
      type     => SCALAR,
      regex    => $self->_valid_id_re },
    { # optional renderer name
      type     => SCALAR,
      optional => 1 }
  );

  my $response = $self->_request( {
    path => [ 'result', $params->[0], $params->[1] || 'out' ]
  } );

  return $response;
}

#-------------------------------------------------------------------------------

# sub batch {
#
# }

#-------------------------------------------------------------------------------

=head2 search

Submits a new sequence search.

=cut

sub search {
  my $self = shift;

  my $request_params = $self->_validate_search_params( @_ );

  my $content;
  eval {
    $content= $self->_request( {
      method         => 'PUT',
      path           => 'run',
      request_params => $request_params
    } );
  };
  if ( $@ ) {
    croak "ERROR: job submission failed: $@";
  }

  # make sure the result of the query is a job ID that looks something like
  # pfamscan-R20130823-140719-0253-70482578-oy
  croak "ERROR: did not receive a job ID (got '$content')"
    unless $content =~ m/^[a-z_]+-[\w+\-]+(oy|pg|hx|es)/;

  return $content;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

sub _request {
  my $self = shift;
  my $method_params = validate(
      @_, {
        path           => {
          callbacks => {
            'scalar or arrayref' => sub {
              my $val = shift;
              return ( not ref $val or ref $val eq 'ARRAY' );
            }
          }
        },
        request_params => { type => HASHREF, optional => 1 },
        method         => {
          optional  => 1,
          default   => 'GET',
          type      => SCALAR,
          callbacks => {
            'valid method' => sub {
              return exists $self->_accepted_methods->{$_[0]};
            }
          }
        },
      }
  );

  # tidy up the request parameters to remove any undefined values
  # (EBI WS don't like undefined params)
  my @params_to_delete;
  while ( my ( $key, $value ) = each %{ $method_params->{request_params} } ) {
    push @params_to_delete, $key if not defined $value;
  }
  delete $method_params->{request_params}->{$_} for @params_to_delete;

  # append the specified path to the base URL
  my @segments = $self->base_url->path_segments;

  my $url = $self->base_url->clone;
  $url->path_segments(
    @segments,
    ref $method_params->{path} eq 'ARRAY'
      ? @{ $method_params->{path} }
      : $method_params->{path}
  );

  my ( $start, $end, $response );
  if ( uc $method_params->{method} eq 'GET' ) {
    $self->_log->debug( "sending 'get' request to |$url|" );
    $start = Time::HiRes::gettimeofday();
    $response = $self->_ua->get(
      $url,
      %{ $method_params->{request_params} },
      'Accept-Encoding' => $self->_can_accept
    );
    $end = Time::HiRes::gettimeofday();
  }
  elsif ( uc $method_params->{method} eq 'POST' or
          uc $method_params->{method} eq 'PUT' ) {
    $self->_log->debug( "sending 'POST' request to |$url|" );
    $start = Time::HiRes::gettimeofday();
    $response = $self->_ua->post(
      $url,
      $method_params->{request_params},
      'Accept-Encoding' => $self->_can_accept
    );
    $end = Time::HiRes::gettimeofday();
    $self->_log->debug( 'done' );
  }
  # elsif ( uc $method_params->{method} eq 'DELETE' ) {
  #  $self->_log->debug( "sending 'delete' request to |$url|" );

  # }
  else {
    croak 'ERROR: Not a valid request method';
  }

  $self->_log->debug( 'took ' . sprintf( "%.2f", $end - $start ) . 's to submit request' );

  croak 'ERROR: failed to send request'
    unless $response;

  # check for an error from the server
  croak 'ERROR: REST request failed: ' . $response->status_line
    if $response->is_error;

  my $content = $self->_can_accept
              ? $response->decoded_content
              : $response->content;

  if ( $response->is_error ) {
    $content =~ m{<(h1|description)>([^<]+)</};
    croak 'ERROR: submission failed (' . $response->code . ' ' . $response->message . "): $1";
  }

  return $content;
}

#-------------------------------------------------------------------------------

sub _validate_search_params {
  my $self = shift;
  my $params = validate(
    @_, {
      sequence => {
        type  => SCALAR,
        regex => qr/^(>.*?\n)?[A-Za-z\s\n\*]+$/s,
      },
      database => {
        optional  => 1,
        type      => SCALAR,
        regex     => qr/^pfam-(a|b|ab)$/,
      },
      format => {
        optional  => 1,
        type      => SCALAR,
        regex     => qr/^[a-z]+$/,
      },
      email => {
        optional  => 1,
        type      => SCALAR,
        callbacks => {
          'looks like a valid email address' => sub { return Email::Valid->address($_[0]); },
        },
        default => 'xfam@ebi.ac.uk', # TODO we shouldn't really have to give an email address...
      },
      evalue => {
        optional  => 1,
        type      => SCALAR,
        callbacks => {
          'looks like a number' => sub { return looks_like_number( $_[0] ); },
          'is positive'         => sub { return $_[0] > 0.0; }
        }
      }
    }
  );

  my @sequence = split /\n/, $params->{sequence};
  if ( $sequence[0] =~ m/^>/ ) {
    shift @sequence;
  }
  my $sequence = join ' ', @sequence;
  $sequence =~ s/\s//g;
  $sequence =~ s/[\s\n\r]//g;

  $self->_log->debug( 'tidied sequence: |' . $params->{sequence} . '|' );
  $self->_log->debug( 'evalue: |' . ( $params->{evalue}  || 'na' ). '|' );
  $self->_log->debug( 'email: |' . ( $params->{email}  || 'na' ). '|' );

  my $request_params = {
    sequence => $params->{sequence},
    email    => $params->{email},
    database => $params->{database},
    format   => $params->{format},
  };

  if ( $params->{evalue} ) {
    $self->_log->debug( 'E-value specified (' . $params->{evalue} . ')' );
    $request_params->{evalue} = $params->{evalue};
  }
  else {
    $self->_log->debug( 'No E-value specified; using GA cut off' );
    $request_params->{cutOffOption} = 'ga-score';
  }

  return $request_params;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<< <jgt at ebi.ac.uk> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-bio-pfam-ebirestclient at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Bio-Pfam-EbiRestClient>.  I
will be notified, and then you'll automatically be notified of progress on your
bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Bio::Pfam::EbiRestClient


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Bio-Pfam-EbiRestClient>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Bio-Pfam-EbiRestClient>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Bio-Pfam-EbiRestClient>

=item * Search CPAN

L<http://search.cpan.org/dist/Bio-Pfam-EbiRestClient/>

=back


=head1 ACKNOWLEDGEMENTS

=cut

#-------------------------------------------------------------------------------

no Moose;

__PACKAGE__->meta->make_immutable;

#-------------------------------------------------------------------------------

=head1 LICENSE AND COPYRIGHT

Copyright 2013 John Tate.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1;
