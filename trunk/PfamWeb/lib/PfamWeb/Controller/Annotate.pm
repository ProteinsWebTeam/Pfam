
# Annotate.pm
# jt 20061020 WTSI
#
# $Id: Annotate.pm,v 1.1 2006-10-23 12:23:30 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Annotate - accept user annotations

=cut

package PfamWeb::Controller::Annotate;

=head1 DESCRIPTION

Accepts user annotations.

$Id: Annotate.pm,v 1.1 2006-10-23 12:23:30 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 make_book_widget

Build an HTML::Widget form for book creation and updates

=cut

sub buildForm : Private {
  my( $this, $c ) = @_;

  # get a widget
  my $w = $c->widget( "annotationForm" )->method( "post" );

  # add the form fields

  # user's name
  my $f = $w->element( "Textfield", "name" );
  $f->label( "Name" );
  $f->size( 30 );
  $f->maxlength( 200 );

  # email address
  $f = $w->element( "Textfield", "email" );
  $f->label( "Email address" );
  $f->size( 30 );
  $f->maxlength( 100 );

  # the annotation itself
  $f = $w->element( "Textarea", "annotation" );
  $f->label( "Annotation details" );
  $f->cols( 50 );
  $f->rows( 15 );

  # supporting references
  $f = $w->element( "Textarea", "refs" );
  $f->label( "References" );
  $f->cols( 50 );
  $f->rows( 5 );

  # an alignment upload field
  $f = $w->element( "Upload", "alignment" );
  $f->label( "Upload an alignment file" );
  $f->accept( "text/plain" );
  $f->size( 30 );

  # a submit button
  $f = $w->element( "Submit", "submit" );

  # and a reset button
  $f = $w->element( "Reset", "reset" );

  # return the widget
  return $w;
}


sub default : Path {
  my ($self, $c) = @_;

  # Create the widget and set the action for the form
  my $w = $c->forward( "buildForm" );
  $w->action($c->uri_for('check_input'));

  # Write form to stash variable for use in template
  $c->stash->{widget_result} = $w->result;

  # Set the template
  $c->stash->{template} = "pages/annotation.tt";
}


sub checkInput : local {
  my( $this, $c ) = @_;

  $c->log->debug( "checking input..." );

  $c->stash->{status_msg} = "worked fine";
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
