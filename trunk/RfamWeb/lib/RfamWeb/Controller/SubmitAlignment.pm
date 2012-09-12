
# SubmitAlignment.pm
# jt6 20120906 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Controller::SubmitAlignment - controller to accept alignment submissions

=cut

package RfamWeb::Controller::SubmitAlignment;

=head1 DESCRIPTION

This controller handles the uploading of sequence alignments by users.

$Id$

=cut

use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller'; }

use RfamWeb::Form::SubmitAlignment;
use Email::MIME;

has 'form' => ( 
  isa => 'RfamWeb::Form::SubmitAlignment',
  is => 'rw',
  lazy => 1,
  default => sub { RfamWeb::Form::SubmitAlignment->new } 
);

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 submit_alignment : Chained('/') PathPart('submit_alignment') Args(0)

Action to both generate and validate the form. Accepts a C<prefill> parameter
to flag when an accession has been supplied and should be used to pre-fill the
accession field in the form.

=cut

sub submit_alignment : Chained( '/' )
                       PathPart( 'submit_alignment' ) 
                       Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'forms/alignment_submission_form.tt';
  $c->stash->{form} = $this->form;

  if ( $c->req->param('prefill') ) {
    $this->form->process( 
      update_field_list => { 
        accession  => { default => $c->req->param('accession') || '' },
        new_family => { default => 0 }
      }
    );
    $this->form->process();
  }
  else {
    my $params = $c->req->params;
    $params->{alignment} = $c->req->upload('alignment')
      if $c->req->method eq 'POST';
    $this->form->process( params => $params );
  }

  if ( $this->form->validated ) {
    $c->log->debug( 'SubmitAlignment::show_form: form validated' )
      if $c->debug;

    $c->forward('send_mail');
    $c->stash->{template} = 'forms/alignment_submitted_page.tt';
  }
  else {
    $c->log->debug( 'SubmitAlignment::show_form: form NOT validated' )
      if $c->debug;
    return;
  }
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 send_mail : Private

Sends a mail detailing the upload to an address given in the config, under
"alignmentSubmissionEmail".

=cut

sub send_mail : Private {
  my ( $this, $c ) = @_;

  my $subject = $this->form->field('new_family')->value
              ? 'New Rfam family suggestion'
              : 'New alignment for family ' . $this->form->field('accession')->value;

  # get the alignment itself
  my $upload_field = $this->form->field('alignment')->value;
  my $fh = $upload_field->fh;
  my $alignment = join '', <$fh>;

  # render a template to get the email body
  my $body = $c->view('TT')->render($c, 'forms/alignment_submitted_email.tt' );

  # build the email
  $c->stash->{email} = {
    to => $this->{alignmentSubmissionEmail},
    from => $this->form->field('email')->value,
    subject => $subject,
    parts => [
        $body,
        Email::MIME->create(
            attributes => {
                content_type => 'text/plain',
                disposition  => 'attachment',
                filename     => $upload_field->filename
            },
            body => $alignment
        )
    ],
  };

  $c->forward( $c->view('Email') );

  # errors are handled in the template that builds the "thanks for submitting"
  # page. Hopefully.
}

#-------------------------------------------------------------------------------  

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Sarah Burge, C<sb30@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Sarah Burge (sb30@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

__PACKAGE__->meta->make_immutable;

1;

