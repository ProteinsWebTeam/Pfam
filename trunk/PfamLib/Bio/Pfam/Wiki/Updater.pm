
# Updater.pm
# jt6 20100720 WTSI
#
# $Id$

=head1 NAME

Bio::Pfam::Wiki::Updater - updates the wiki_approve DB with the latest wikipedia edits

=cut

package Bio::Pfam::Wiki::Updater;

=head1 SYNOPSIS

  # get a schema for connecting to the "wiki_approve" DB
	use Bio::Pfam::Wiki::Schema

	my $schema = WikiApp::Schema->connect( $dsn, $username, $password );

	my $updater = Bio::Pfam::Wiki::Updater->new( schema => $schema );
	$updater->update_all

=head1 DESCRIPTION

This is a simple class to update the wiki_approve database table with the 
most recent wikipedia revision number for the specified wikipedia articles.

=cut

use Moose;
use Log::Log4perl;
use MediaWiki::Bot;

our $VERSION = '0.1';

#-------------------------------------------------------------------------------
#- configure logging -----------------------------------------------------------
#-------------------------------------------------------------------------------

BEGIN {
  my $logger_conf = q(
    log4perl.logger                   = INFO, Screen
    log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
  );

  Log::Log4perl->init( \$logger_conf );
}

has 'logger' => (
  is      => 'ro',
  isa     => 'Log::Log4perl::Logger',
  lazy    => 1,
  default => sub { 
    my $this = shift;
    return Log::Log4perl->get_logger( ref $this );
  }
);

#-------------------------------------------------------------------------------
#- public accessors ------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 num_checked

Returns the number of wikipedia entries that were checked in the most recent
C<update> or C<update_all>.

=head2 num_updated

Returns the number of wikipedia entries whose revision ID was updated in the 
database during the most recent C<update> or C<update_all>.

=head2 schema

A L<DBIx::Class::Schema> for the C<wiki_approve> database schema. Required;
must be supplied in the constructor.

=head2 updated_articles

Returns a reference to an array containing the titles of updated articles.

=cut

# set "writer" for these, so that they're effectively read-only accessors
has 'num_checked' => (
  is      => 'rw',
  isa     => 'Int',
  default => 0,
  writer  => '_set_num_checked',
);

has 'num_updated' => (
  is      => 'rw',
  isa     => 'Int',
  default => 0,
  writer  => '_set_num_updated',
);

# DBIC connection to wiki_approve DB
has 'schema' => (
  is       => 'ro',
  isa      => 'DBIx::Class::Schema',
  required => 1,
);

has 'updated_articles' => (
	is       => 'ro',
  isa      => 'ArrayRef[Str]',
  default  => sub { [] },
);

#-------------------------------------------------------------------------------
#- private accessors -----------------------------------------------------------
#-------------------------------------------------------------------------------

# wikipedia articles to be checked
has '_articles' => (
  is      => 'rw',
  isa     => 'ArrayRef[Str]',
);

# allows _articles to be set using a scalar, an array or an array ref
around '_articles' => sub {
  my $orig = shift;
  my $this = shift;

  return $this->$orig unless @_;

  if ( ref $_[0] eq 'ARRAY'  ) {
    $this->logger->debug( 'handed ref to array of wiki titles' );
    $this->$orig( $_[0] );
  }
  elsif ( scalar @_ == 1 ) {
    $this->logger->debug( 'handed a single wiki title' );
    $this->$orig( [ $_[0] ] );
  }
  elsif ( scalar @_ > 1 ) {
    $this->logger->debug( 'handed an array of wiki titles' );
    $this->$orig( \@_ );
  }
};

#---------------------------------------

# MediaWiki::Bot
has '_mw_bot' => (
  is      => 'ro',
  isa     => 'MediaWiki::Bot',
  lazy    => 1,
  default => sub { return MediaWiki::Bot->new; }
);

#-------------------------------------------------------------------------------
#- public methods --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 update_all

Updates the revision IDs for all wikipedia entries in the C<wikipedia> table.

=cut

sub update_all {
  my $this = shift;
  $this->logger->debug( 'updating all wikipedia article version(s)' );

  $this->_articles( [] );
  $this->_update;
}

#-------------------------------------------------------------------------------

=head2 update

Updates the revision IDs for the specified wikipedia articles. Accepts a single
title, a list of a titles or a ref to a list of titles.

=cut

sub update {
  my $this = shift;
  $this->logger->debug( 'updating specified wikipedia article version(s)' );

  $this->logger->logdie( 'must specify one or more wikipedia article titles' )
    unless @_;
  
  $this->_articles( @_ );
  $this->_update;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _update

Updates the revision IDs for the list of wikipedia articles found in 
C<_articles>.

=cut

sub _update {
  my $this = shift;

  # query the database for the rows corresponding to the specified articles

  my @articles;
  if ( scalar @{ $this->_articles } ) {
    $this->logger->debug( 'updating revid for some entries' );

    foreach my $title ( @{ $this->_articles } ) {
      my $row = $this->schema->resultset( 'Wikipedia' )
                             ->find( { title => $title } );
      push @articles, $row if $row;
    }
  }
  else {
    $this->logger->debug( 'updating revid for all entries' );

    @articles = $this->schema->resultset( 'Wikipedia' )
                             ->search( {}, {} );
  }

  if ( scalar @articles ) {
    $this->logger->debug( 'retrieved ' . scalar( @articles ) . ' wikipedia article(s) from DB' );
  }
  else {
    $this->logger->logdie( "couldn't retrieve list of wikipedia articles from DB" )
      unless scalar @articles;
  }

  # for each row, get the last revision number and update the database,
  # if necessary

  my $num_checked = 0;
  my $num_updated = 0;
  foreach my $article ( @articles ) {
		sleep 1;
    $num_checked++;
    my $latest_revid = $this->_get_last_revid( $article->title );

    next unless ( defined $latest_revid and $latest_revid =~ m/^\d+$/ );

    if ( $latest_revid == $article->wikipedia_revision ) {
      $this->logger->debug( 'wikipedia table already has most recent revid for |'
                            . $article->title . '|' );
    }
    else { 
      $this->logger->debug( 'updating wikipedia version for |' . $article->title 
                            . "| to |$latest_revid|" );
      $article->update( { wikipedia_revision => $latest_revid } );
			push @{ $this->updated_articles }, $article->title;
      $num_updated++;
    }
  }

  $this->_set_num_checked( $num_checked );
  $this->_set_num_updated( $num_updated );
}

#-------------------------------------------------------------------------------

=head2 _get_last_revid

Returns the revision ID of the last edit to the specified page.

=cut

sub _get_last_revid {
  my ( $this, $title ) = @_;
  $this->logger->debug( "getting wikipedia version for |$title|" );

  my @history = $this->_mw_bot->get_history( $title, 1 );
  unless ( @history ) {
    print STDERR "Failed to retrieve history for wikipedia article '$title'\n";
    return;
  }

  my $last_revision = $history[0]->{revid};
  $this->logger->debug( "last revision of '$title' was |$last_revision|" );

  return $last_revision;
}

#-------------------------------------------------------------------------------

no Moose;
__PACKAGE__->meta->make_immutable;

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), 
         Paul Gardner, (pg5@sanger.ac.uk),
         John Tate (jt6@sanger.ac.uk)

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

