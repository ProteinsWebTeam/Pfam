package Catalyst::Plugin::GzipCompressor;
use strict;
use warnings;
use MRO::Compat;

use Compress::Zlib ();

sub finalize {
    my $c = shift;

    if ( $c->response->content_encoding ) {
        return $c->next::method(@_);
    }

    unless ( $c->response->body ) {
        return $c->next::method(@_);
    }

    unless ( $c->response->status == 200 ) {
        return $c->next::method(@_);
    }

    unless ( $c->response->content_type =~ /^text|xml$|javascript$/ ) {
        return $c->next::method(@_);
    }

    my $accept = $c->request->header('Accept-Encoding') || '';

    unless ( index( $accept, "gzip" ) >= 0 ) {
        return $c->next::method(@_);
    }

    my $body = $c->response->body;

    # handle, as a special case, a body containing a URI object
    $body = $body->as_string if ref $body eq 'URI::http';

    eval { local $/; $body = <$body> } if ref $body;
    die "Response body is an unsupported kind of reference" if ref $body;

    $c->response->body( Compress::Zlib::memGzip( $body ) );
    $c->response->content_length( length( $c->response->body ) );
    $c->response->content_encoding('gzip');
    $c->response->headers->push_header( 'Vary', 'Accept-Encoding' );

    $c->next::method(@_);
}

1;

__END__

=head1 NAME

Catalyst::Plugin::GzipCompressor - Gzip response

=head1 SYNOPSIS

    use Catalyst qw[GzipCompressor];


=head1 DESCRIPTION

Gzip compress response if client supports it. This plugin is a straight
copy of L<Catalyst::Plugin::Compress::Gzip>, with the addition of one 
line. Now, if the response is a L<URI::http> object, we stringify it
before trying to do anything else. This is required for the "Jump" 
mechanism. Raw URIs in the response seem to be automatically converted
into objects, but the original plugin gags on them.

=head1 METHODS

=head2 finalize

=head1 SEE ALSO

L<Catalyst>.

=head1 AUTHOR

Christian Hansen, C<ch@ngmedia.com>
John Tate

=head1 LICENSE

This library is free software . You can redistribute it and/or modify it under
the same terms as perl itself.

=cut
