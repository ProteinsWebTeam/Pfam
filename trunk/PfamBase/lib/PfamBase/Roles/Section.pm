
# Section.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

PfamBase::Roles::Section - role to apply a sensible "end" method to a controller

=cut

package PfamBase::Roles::Section;

=head1 DESCRIPTION

This role adds an "end" action that applies the "RenderView" action class and
takes care of tracking and displaying errors in some cases.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 section : Path

Default action. This should be over-ridden or excluded by any controllers that
apply this role, otherwise the base section name will go straight to the 404
page:

  with 'PfamBase::Roles::Section' => { -excludes => 'section' };

=cut

sub section : Path {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamBase::Roles::Section::section: defaulting to 404 page' )
    if $c->debug;

  $c->stash->{template} = 'pages/404.tt';
}

#-------------------------------------------------------------------------------

=head2 end : ActionClass

Hands off to the tab layout template or catches any errors that were
generated earlier

=cut

sub end : ActionClass( 'RenderView' ) {
  my ( $this, $c ) = @_;

  # the "$c->detach" method is actually just "$c->forward" with an exception
  # thrown immediately afterwards. We need to catch that exception and disregard
  # it, otherwise we'll get a big stack trace on every "$c->detach"... Pretty
  # ugly, I know.

  my $d = $Catalyst::DETACH; # just to avoid warnings about single use...
  foreach my $e ( @{ $c->error } ) {
    $c->clear_errors if $e =~ /$Catalyst::DETACH/;
  }

  # and having made sure that we're not just being "detached", check for 
  # real errors now
  if ( scalar @{ $c->error } ) {
    $c->log->debug( 'PfamBase::Roles::Section::end: found errors' )
      if $c->debug;

    $c->log->error( $_ ) for @{ $c->error };

  	# there was a system error...
  	$c->stash->{template} = 'components/systemError.tt';

  	# report the error as a broken internal link
  	$c->forward( '/reportError' );

  	# clear the errors before we finish up
  	$c->clear_errors;
  }
  elsif ( $c->stash->{errorMsg} ) {
    $c->log->debug( 'PfamBase::Roles::Section::end: found error message' )
      if $c->debug;

  	# there was an error with user input, e.g. bad ID or accession. Check the 
  	# config for the location of the error template file
  	$c->stash->{template} ||= 'components/blocks/' . $this->{SECTION} . '/error.tt';
  }
  else {
    $c->log->debug( 'PfamBase::Roles::Section::end: no errors; setting template' )
      if $c->debug;

  	# no problems; set up the template and let it rip
  	$c->stash->{pageType} ||= $this->{SECTION};
  	$c->stash->{template} ||= 'pages/layout.tt';
  }
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

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
