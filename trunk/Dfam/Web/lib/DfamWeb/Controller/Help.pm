package DfamWeb::Controller::Help;

use strict;
use warnings;

use base 'PfamBase::Controller::Help';


=head1 NAME

DfamWeb::Controller::Help - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    $c->response->body('Matched DfamWeb::Controller::Help in Help.');
}



#-------------------------------------------------------------------------------

=head1 METHODS

=head2 about : Global

Displays an "about" page.

=cut 

sub about : Global {
  my ( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'about';

  $c->stash->{template} = 'pages/about.tt';
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

Finn, Rob

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
