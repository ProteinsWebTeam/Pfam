
# SubmitAlignment.pm
# jt6 20120912 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Form::SubmitAlignment - a form for accepting alignment submissions

=cut

package RfamWeb::Form::SubmitAlignment;

=head1 DESCRIPTION

This is a <LHTML::FormHandler> Form class that builds a form for accepting
uploads of new alignments.

$Id$

=cut
 
use HTML::FormHandler::Moose;
extends 'HTML::FormHandler';

#-------------------------------------------------------------------------------

# form attributes

# encoding
sub build_form_element_attr {
  return { enctype => 'multipart/form-data' };
}

# name; converts to id attribute when the form is rendered
has '+name' => ( default => 'annotationSubmission' );

# fields
has_field 'name' => ( 
  type => 'Text', 
  required => 1,
  messages => { required => 'You must give your name.' },
  element_attr => { placeholder => 'Your name' }
);
has_field 'email' => ( 
  type => 'Email', 
  required => 1,
  messages => { required => 'You must give your email address.' },
  element_attr => { placeholder => 'Your email address' } 
);
has_field 'comments' => ( 
  type => 'TextArea',
  element_attr => { placeholder => 'Comments on the alignment.' }
);
has_field 'alignment' => ( 
  type => 'Upload', 
  required => 1,
  messages => { required => 'You must upload a Stockholm-format alignment.' },
  min_size => 400,
  max_size => 10000,
  element_attr => { placeholder => 'Stockholm-format alignment' }
);
has_field 'new_family' => ( type => 'Checkbox', default => 1 );
has_field 'accession' => (
  type => 'Text',
  messages => { required => 'You must supply an accession for an existing family.' },
  element_attr => { placeholder => 'Rfam accession' }
);
has_field 'pmid' => ( 
  type => 'Text',
  required => 1,
  label => 'PubMed ID',
  messages => { required => 'You must supply a PMID if this is a new family.' },
  element_attr => { placeholder => 'PubMed ID' },
  apply => [ { 
    type => 'Int',
    message => 'Not a valid PubMed ID.'
  } ]
);
 
has_field 'submit' => (
  type => 'Submit', 
  value => 'Submit',
);
 
#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------
  
# set field attributes

sub html_attributes {
  my ( $this, $field, $type, $attr ) = @_;
  # if we want to apply a class to the fields in order to give them a background
  # and a shadow, like the keyword search field in the header
  # if ( $type eq 'element' ) {
  #   $attr->{class} = 'entryField';
  # }
  if ( $type eq 'wrapper' ) {
    $attr->{class} = 'field';
    $attr->{class} .= ' required' if $field->required;
  }
  return $attr;
}

#-------------------------------------------------------------------------------

# validation

# if the "new family" checkbox is checked then a PMID must be supplied. If it's
# unchecked, i.e. this is for an existing family, then an accession must be
# supplied
before 'validate_form' => sub {
  my $this = shift;
  $this->field('accession')->required( not $this->params->{new_family} );
  $this->field('pmid')->required( $this->params->{new_family} );
};

#---------------------------------------

sub validate_accession {
  my ( $this, $field ) = @_;
  return if $this->field('new_family') eq 1;

  $field->add_error( 'Must supply a family accession unless this is a new family' )
    unless $field->value;
  $field->add_error( 'Not a valid Rfam family accession' )
    unless $field->value =~ m/^R[FM]\d{5}$/i;
}

#---------------------------------------

sub validate_pmid {
  my ( $this, $field ) = @_;
  
  $field->add_error( 'Must supply a PubMed ID if this is a new family' )
    if ( $this->field('new_family') and not $field->value );
}

#---------------------------------------

sub validate_alignment {
  my ( $this, $field ) = @_;

  my $fh = $field->value->fh;
  $field->add_error( 'No valid Stockholm-format file' )
    unless defined $fh;

  my @stockholm_file = <$fh>;
  $field->add_error( 'Could not read the Stockholm-format file' )
    unless scalar @stockholm_file;

  # rewind the file, because we're going to need to read it again, this time 
  # in earnest, in the controller that uses this form
  seek( $fh, 0, 0 );

  $field->add_error( 'Not a valid Stockholm-format file' )
    unless ( $stockholm_file[0]  =~ m|^# STOCKHOLM 1.0$| and
             $stockholm_file[-1] =~ m|^//$| );
}

#-------------------------------------------------------------------------------

no HTML::FormHandler::Moose;

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

1;

