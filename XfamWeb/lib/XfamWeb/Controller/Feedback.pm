package XfamWeb::Controller::Feedback;
use Moose;
use namespace::autoclean;
use Data::Printer;

BEGIN { extends 'XfamWeb::Controller::Base'; }

=head1 NAME

XfamWeb::Controller::Feedback - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index

=cut

sub index :Path :Args(0) {
  my ( $self, $c ) = @_;

  if ($c->req->method eq 'POST') {
    $c->forward('validate_params');
  }

}

sub thanks :Path('thanks') : Args(0) {
  my ( $self, $c ) = @_;
}

sub validate_params :Private {
  my ($self, $c) = @_;
  my $args = $c->req->params;

  eval {
    $c->stash->{feedback} = $c->model('Feedback', $args);
  };

  if ($@) {
    # fish out the error message
    my ($field, $message) = $@ =~ /^Attribute \((.*)\) .*: (.*) at \//;
    $c->stash->{error} = {$field => $message}
  }

  if (!exists $c->stash->{error}) {
    $c->detach('email_feedback');
  }

}

sub email_feedback :Private {
  my ($self, $c) = @_;
  $c->stash->{feedback}->save();
  $c->res->redirect($c->uri_for('/feedback',{success => 'thanks'}));
}




=head1 AUTHOR

Clements, Jody

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
