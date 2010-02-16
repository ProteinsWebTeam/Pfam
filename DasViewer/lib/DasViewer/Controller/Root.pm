package DasViewer::Controller::Root;

use strict;
use warnings;
use parent 'Catalyst::Controller';

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config->{namespace} = '';

=head1 NAME

DasViewer::Controller::Root - Root Controller for DasViewer

=head1 DESCRIPTION

[enter your description here]

=head1 METHODS

=cut

=head2 index

=cut

sub index :Path  {
  my ( $self, $c ) = @_;
  
  $c->stash->{template} = 'components/index.tt';
}

sub default :Path {
  my ( $self, $c ) = @_;
  $c->response->body( 'Page not found' );
  $c->response->status(404);
  
}

=head2 end

Attempt to render a view, if needed.

=cut 

sub end : ActionClass('RenderView') {}

=head1 AUTHOR

Prasad Gunasekaran,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
