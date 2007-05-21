
# Section.pm
# jt6 20060922 WTSI
#
# $Id: Section.pm,v 1.8 2007-05-21 12:51:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Section - base class for section pages, e.g. Family

=cut

package PfamWeb::Controller::Section;

=head1 DESCRIPTION

This is the base class for the various "section" controllers, such as
Family, Clan, etc. It contains an empty C<default> method that just
captures the URL, and an C<end> that catches errors from earlier in
the process and reports them. If there are no errors it renders the
view that's for the section, e.g. "family.tt", etc.

$Id: Section.pm,v 1.8 2007-05-21 12:51:58 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

An empty action to capture URLs like

=over

=item http://localhost:3000/SECTION?id=ID

=back

=cut

sub default : Path {
  my( $this, $c ) = @_;

  # empty; just here to capture the URL

}

#-------------------------------------------------------------------------------

=head2 end : Private

Hands off to the tab layout template or catches any errors that were
generated earlier

=cut

sub end : ActionClass( "RenderView" ) {
  my( $this, $c ) = @_;

  # the "$c->detach" method is actually just "$c->forward" with an exception
  # thrown immediately afterwards. We need to catch that exception and disregard
  # it, otherwise we'll get a big stack trace on every "$c->detach"... Pretty
  # ugly, I know.
  foreach my $e ( @{ $c->error } ) {
    $c->clear_errors if $e =~ /$Catalyst::DETACH/;
  }

  # and having made sure that we're not just being "detached", check for 
  # real errors now
  if( scalar @{ $c->error } ) {

  	# there was a system error...
  	$c->stash->{template} = "components/systemError.tt";

  	# report the error as a broken internal link
  	$c->forward( "/reportError" );

  	# clear the errors before we finish up
  	$c->clear_errors;

  } elsif ( $c->stash->{errorMsg} ) {

  	# there was an error with user input, e.g. bad ID or accession. Check the 
  	# config for the location of the error template file
  	$c->stash->{template} =
  	  $c->config
  		->{"View::TT"}
  		  ->{VARIABLES}
  			->{layouts}
  			  ->{ $this->{SECTION} }
  				->{errorTemplate};

  } else {

  	# no problems; set up the template and let it rip
  	$c->stash->{pageType} ||= $this->{SECTION};
  	$c->stash->{template} ||= "pages/layout.tt";

  }

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
