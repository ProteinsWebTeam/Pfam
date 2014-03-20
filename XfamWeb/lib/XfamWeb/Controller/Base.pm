package XfamWeb::Controller::Base;
use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller'; }

=head1 NAME

XfamWeb::Controller::Base - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut

sub begin : Private {
  my ($self, $c) = @_;
  #check for messages and load them into the stash based on the config
  my $message = $c->req->param('message');
  if ($message) {
    if (exists $c->config->{messages}->{$message}) {
      $c->stash->{message} = $c->config->{messages}->{$message};
    }
  }
  my $success = $c->req->param('success');
  if ($success) {
    if (exists $c->config->{messages}->{$success}) {
      $c->stash->{success} = $c->config->{messages}->{$success};
    }
  }
  return;
}


=head1 AUTHOR

Clements, Jody

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
