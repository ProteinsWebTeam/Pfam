
# Annotate.pm
# jt 20061020 WTSI
#
# $Id: Annotate.pm,v 1.6 2007-01-15 14:17:17 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Annotate - accept user annotations

=cut

package PfamWeb::Controller::Annotate;

=head1 DESCRIPTION

Accepts user annotations.

$Id: Annotate.pm,v 1.6 2007-01-15 14:17:17 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

use PfamWeb::CustomContainer;
use HTML::Widget::Element;

use IO::All;

BEGIN {
  HTML::Widget::Element->container_class( "PfamWeb::CustomContainer" );
}

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Checks input parameters and populates the stash accordingly.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  if ( $c->req->param("acc") ) {
	$c->log->debug( "Annotate::begin: accession: |" . $c->req->param("acc") . "|" );
  }

  # build the email subject line based on the accession (if given)

  if ( $c->req->param("acc") and $c->req->param("acc") =~ m/^(P([FB])\d{5,6})$/i ) {
	$c->log->debug( "Annotate::begin: found a Pfam entry ($2)" );

	if ( $2 eq "F" ) {
	  $c->log->debug( "Annotate::begin: it's a pfam A entry" );
	
	  my $pfam = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );
	
	  $c->stash->{type} = "A";
	  $c->stash->{acc}  = $pfam->pfamA_acc;
	  $c->stash->{id}   = $pfam->pfamA_id;
	
	  $c->stash->{subject} = "Annotation submission for Pfam A entry "
		. $pfam->pfamA_id . " (" . $pfam->pfamA_acc . ")";
	
	} elsif ( $2 eq "B" ) {
	  $c->log->debug( "Annotate::begin: it's a pfam B entry" );
	
	  $c->stash->{type} = "B";
	  $c->stash->{acc}  = $1;
	
	  $c->stash->{subject} = "Annotation submission for Pfam B entry $1";
	
	}

  } elsif ( $c->req->param("acc") and $c->req->param("acc") =~ m/^(CL\d{4})$/i ) {
	$c->log->debug( "Annotate::begin: found a clan entry" );
	
	my $clan = $c->model("PfamDB::Clans")->find( { clan_acc => $1 } )
	  if defined $1;
	
	$c->stash->{type} = "C";
	$c->stash->{acc}  = $clan->clan_acc;
	$c->stash->{id}   = $clan->clan_id;
	
	$c->stash->{subject} = "Annotation submission for Pfam clan " .
	  $c->stash->{id} . " (" . $c->stash->{acc} . ")";
	
  } else {
	$c->log->debug( "Annotate::begin: didn't find a recognised accession" );
	
	$c->stash->{subject} = "Annotation submission";
	
  }

  $c->log->debug( "Annotate::begin: generated subject line: " );
  $c->log->debug( "Annotate::begin:   |" . $c->stash->{subject} . "|" );
}

  #-------------------------------------------------------------------------------

=head2 index : Private

The main entry point for the annotation form. Builds the form widget
and hands straight off to the template.

=cut

  sub index : Private {
	my( $this, $c ) = @_;

	# create the widget
	my $w = $c->forward( "buildForm" );

	# stash the widget, set the template and we're done
	$c->stash->{widget}   = $w->result;
	$c->stash->{template} = "pages/annotation.tt";
  }

  #-------------------------------------------------------------------------------

=head2 checkInput : Local

Validates the input from the annotation form. Returns to the
annotation form if there were problems or forwards to the method that
sends an email.

=cut

  sub checkInput : Local {
	my( $this, $c ) = @_;

	# create the widget again
	my $w = $c->forward( "buildForm" );

	# validate the input parameters
	my $r = $w->process( $c->req );
	if ( $r->has_errors ) {
	  $c->log->debug( "Annotate::checkInput: there were validation errors" );
	
	  # drop the widget into the stash, along with the validation error
	  # messages
	  $c->stash->{widget} = $r;
	  $c->stash->{error}  = "There were problems with the items that you entered";

	} else {
	  $c->log->debug( "Annotate::checkInput: no errors in the user input" );
	
	  # the input parameters validated, so send an email
	  $c->forward( "sendMail" );
	
	}

	# hand off to the same template, which can either render the form
	# again, including the error messages, or just show a "success"
	# message
	$c->stash->{template}  = "pages/annotation.tt";
  }

  #-------------------------------------------------------------------------------

=head2 sendMail : Private

Builds an annotation submission email and sends it to the address
specified in the config.

=cut

  sub sendMail : Private {
	my( $this, $c ) = @_;

	$c->log->debug( "Annotate::sendMail: sending an annotation mail" );
	$c->log->debug( "Annotate::sendMail:   acc:   |" . $c->stash->{acc} . "|" );
	$c->log->debug( "Annotate::sendMail:   id:    |" . $c->stash->{id} . "|" );
	$c->log->debug( "Annotate::sendMail:   user:  |" . $c->req->param("user") . "|" );
	$c->log->debug( "Annotate::sendMail:   email: |" . $c->req->param("email") . "|" );
	$c->log->debug( "Annotate::sendMail:   ann:   |" . $c->req->param("annotation") . "|" );
	$c->log->debug( "Annotate::sendMail:   refs:  |" . $c->req->param("refs") . "|" );

	# see if there was an uploaded alignment
	my @parts;
	if ( $c->req->upload("alignment") ) {
	  my $u = $c->req->upload("alignment");
	  $c->log->debug( "Annotate::sendMail: attaching upload to mail (" . $u->filename . ")" );

	  # build an email "part" for it
	  my $attachment = Email::MIME
		->create( attributes => { content_type => $u->type,
								  disposition  => "attachment",
								  filename     => $u->filename },
				  body => io( $u->tempname )->all,
				);
	  push @parts, $attachment;
	}

	# stuff the stash
	$c->stash->{user}       = $c->req->param("user");
	$c->stash->{email}      = $c->req->param("email");
	$c->stash->{annotation} = $c->req->param("annotation");
	$c->stash->{refs}       = $c->req->param("refs");

	# render the email
	my $mailTxt = $c->view( "TT" )->render($c, "components/annotationEmail.tt" );

	# and send it
	eval {
	  $c->email( header     => [ To      => $this->{annotationEmail},
								 From    => $c->req->param("email"),
								 Subject => $c->stash->{subject} ],
				 parts      => [ $mailTxt,
								 @parts ] );
	};
	if ( $@ ) {
	  # something went wrong...
	  $c->stash->{success} = 0;
	  $c->stash->{error} = "There was a problem submitting your annotation.";
	  $c->log->error( "Annotate::sendMail: problem when submitting an annotation: $@" );
	} else {
	  $c->stash->{success} = 1;
	}
	
  }

  #-------------------------------------------------------------------------------

=head2 buildForm : Private

Builds an HTML::Widget form for the annotation page.

=cut

  sub buildForm : Private {
	my( $this, $c ) = @_;

	# get a widget
	my $w = $c->widget( "annotationForm" )->method( "post" );
	$w->subcontainer( "div" );

	# set the action - always the same for the annotation form
	$w->action( $c->uri_for( "checkInput" ) );

	#----------------------------------------
	# add the form fields

	if ( $c->req->param("acc") ) {
	  $w->element( "Hidden", "acc" )
		->value( $c->req->param("acc") );
	}

	# user's name
	$w->element( "Textfield", "user" )
	  ->label( "Name *" )
		->size( 30 )
		  ->maxlength( 200 );

	# email address
	$w->element( "Textfield", "email" )
	  ->label( "Email address *" )
		->size( 30 )
		  ->maxlength( 100 );

	# the annotation itself
	$w->element( "Textarea", "annotation" )
	  ->label( "Annotation details *" )
		->cols( 50 )
		  ->rows( 15 );

	# supporting references
	$w->element( "Textarea", "refs" )
	  ->label( "References" )
		->cols( 50 )
		  ->rows( 5 );

	# an alignment upload field
	$w->element( "Upload", "alignment" )
	  ->label( "Upload an alignment file" )
		->accept( "text/plain" )
		  ->size( 30 );

	# a submit button
	$w->element( "Submit", "submit" )
	  ->value("Send your comments");

	# and a reset button
	$w->element( "Reset", "reset" );

	#----------------------------------------
	# set some constraints on form values

	# required fields
	$w->constraint( All => qw/ user email annotation /)
	  ->message( "This is a required item" );

	# need a valid email address (or, at least, a correctly formatted one)
	$w->constraint( Email => qw/ email /)
	  ->message( "You did not supply a valid email address" );

	# tidy up the input a little
	foreach my $column ( qw/ user annotation refs / ) {
	  $w->filter( HTMLEscape => $column );
	  $w->filter( TrimEdges  => $column );
	}

	#----------------------------------------
	# finally, return the widget

	return $w;

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
