package DfamWeb::Controller::Root;
#use Moose;
#use namespace::autoclean;


use base 'PfamBase::Controller::Root';

#BEGIN { extends 'Catalyst::Controller' }

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
#__PACKAGE__->config(namespace => '');

=head1 NAME

DfamWeb::Controller::Root - Root Controller for DfamWeb

=head1 DESCRIPTION

[enter your description here]

=head1 METHODS

=head2 index

The root page (/)

=cut

#sub index :Path :Args(0) {
#    my ( $self, $c ) = @_;
#
#    # Hello World
#    $c->response->body( $c->welcome_message );
#}

=head2 default

Standard 404 error page

=cut

#sub default :Path {
#    my ( $self, $c ) = @_;
#    $c->response->body( 'Page not found' );
#    $c->response->status(404);
#}

=head2 end

Attempt to render a view, if needed.

=cut

#sub end : ActionClass('RenderView') {}

=head1 AUTHOR

Finn, Rob

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut




#use strict;
#use warnings;

#use base 'PfamBase::Controller::Root';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Adds the Pfam version data to the stash, so that it's accessible 
throughout the site.

=cut

sub auto : Private {
  my $this = shift;
  my ( $c ) = @_;

  # see if there's a maintenance message in the configuration file
  if ( $c->config->{maintenance} ) {
    $c->log->debug( 'found a maintenance message' )
      if $c->debug;

    $c->stash->{title}   = $c->config->{maintenance}->{title};
    $c->stash->{message} = $c->config->{maintenance}->{message};

    $c->stash->{template} = 'pages/maintenance.tt';

    # break out of the processing chain now and go straight to the "end" action
    return 0;
  }

    # see if we can get a DB ResultSet from the VERSION table, which is 
  # effectively a test of whether we can connect to the DB. If we can't, 
  # set the template to point to a page that will apologise and let the "end"
  # actin do its stuff
  my $releaseData;
  eval {
    # stash some details of the Pfam release
    #$releaseData = $c->model( 'PfamDB::Version' )
    #                 ->search( {} )
    #                 ->first;
    $releaseData = { pfam_release_date => '2011-03-10 17:18:54', 
                     dfam_release      => 0.1, 
                     number_families   => 2 };
  };
  if ( $@ ) {
    $c->stash->{template} = 'pages/db_down.tt';

    # break out of the processing chain now and go straight to the "end" action
    return 0;
  }
  
  $c->stash->{relData} = $releaseData if $releaseData;

  # call the "auto" method on the super-class, which will do the work of
  # picking the correct tab for us
  return $c->SUPER::auto( @_ );
}

#-------------------------------------------------------------------------------

=head2 index : Private

Generates the main site index page. This overrides the version from
PfamBase::Root, so that we can use cache_page for PfamWeb.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # # set the page to be cached for one week
  #$c->cache_page( 604800 );

  # tell the navbar where we are
  $c->stash->{nav} = 'home';

  $c->log->debug('PfamWeb::index: generating site index') if $c->debug;
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

__PACKAGE__->meta->make_immutable;

1;
