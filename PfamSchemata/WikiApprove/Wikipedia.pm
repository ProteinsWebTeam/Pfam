
package WikiApprove::Wikipedia;

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;
use Carp qw( croak cluck );
use MediaWiki::API;
use Data::Dump qw(dump);

extends 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wikipedia");

#-------------------------------------------------------------------------------
#- columns ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

__PACKAGE__->add_columns(
  "title",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "updated",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
  "approved_revision",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "wikipedia_revision",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "approved_by",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 100 },
  "pfam_status",
  { data_type => "ENUM", default_value => "", is_nullable => 0, size => 8 },
  "rfam_status",
  { data_type => "ENUM", default_value => "", is_nullable => 0, size => 8 },
);

#-------------------------------------------------------------------------------
#- keys ------------------------------------------------------------------------
#-------------------------------------------------------------------------------

__PACKAGE__->set_primary_key("title");

#-------------------------------------------------------------------------------
#- relationships ---------------------------------------------------------------
#-------------------------------------------------------------------------------

__PACKAGE__->has_many(
  article_mappings => 'WikiApprove::ArticleMapping',
  'title'
);

#-------------------------------------------------------------------------------
#- accessors -------------------------------------------------------------------
#-------------------------------------------------------------------------------

# because there can be trailing semi-colons in some titles, we need to wrap
# the DBIC wrapper and strip them before doing anything meaningful

around 'title' => sub {
  my $orig = shift;
  my $this = shift;

  if ( @_ ) {
    return $this->$orig(@_);
  }

  my $title = $this->$orig();
  $title =~ s/;$//;

  return $title;
};

#-------------------------------------------------------------------------------

# users
has '_users' => (
  is      => 'ro',
  isa     => 'HashRef',
  lazy    => 1,
  builder => '_get_user_approvals',
);

sub _get_user_approvals {
  my $this = shift;

  my @users = $this->result_source->schema->resultset('Wikiuser')->search( {}, {} );
  my %users = map { $_->user_name => { approved     => $_->approved,
                                       number_edits => $_->number_edits } } @users;

  return \%users;
}

#-------------------------------------------------------------------------------

# MediaWiki::API instance
has '_mw_api' => (
  is      => 'ro',
  isa     => 'MediaWiki::API',
  lazy    => 1,
  default => sub { 
    return MediaWiki::API->new( { api_url => 'http://en.wikipedia.org/w/api.php' } );
  }
);

#-------------------------------------------------------------------------------

# wikipedia article revision history for the last update
has 'history' => (
  is      => 'ro',
  isa     => 'ArrayRef',
  lazy    => 1,
  builder => '_get_article_history',
);

sub _get_article_history {
  my $this = shift;

  # from reading the source of MW::Bot, we should be able to do this:
  #   my @history_list = $this->_mw_bot->get_history( $this->title, 
  #                                                   50,
  #                                                   $this->approved_revision,
  #                                                   'newer' );
  # but there's a bug that means that the direction ("newer") is ignored,
  # so we end up with all older revisions, which are useless...

  # retrieve the list of edits that were made since the last approved revision
  my $response = $this->_mw_api->api( {
    action    => 'query',
    titles    => $this->title,
    prop      => 'revisions',
    rvprop    => 'ids|timestamp|user|comment',
    rvstartid => $this->wikipedia_revision,
    rvendid   => $this->approved_revision,
    rvdir     => 'older',
    rvlimit   => 50,
  } );

  unless ( $response ) {
    croak "Error retrieving revision history for '" . $this->title . "' using API: "
          . $this->_mw_api->{error}->{details} 
          . ' (error code ' . $this->_mw_api->{error}->{code} . ')';
    return;
  }

  my ( $id ) = keys %{ $response->{query}->{pages} };
  my $revisions = $response->{query}->{pages}->{$id}->{revisions};

  # we don't actually want the *last* revision, since it's actually the most
  # recently approved revision
  # shift @$revisions;

  # copy the approval status from the full list of users+status (in $this->_users)
  # into this new list of revisions
  foreach my $revision ( @$revisions ) {
    $revision->{user_approved} = $this->_users->{ $revision->{user} }->{approved} || 0;
    $revision->{number_edits}  = $this->_users->{ $revision->{user} }->{number_edits} || 0;
  }
  # TODO don't recalculate these two values everytime the history is requested

  return $revisions;
}

#-------------------------------------------------------------------------------

# boolean showing whether all revisions were made by approved users
has 'all_users_approved' => (
  is      => 'ro',
  isa     => 'Bool',
  lazy    => 1,
  builder => '_check_all_users_approved',
);

sub _check_all_users_approved {
  my $this = shift;

  my $revisions = $this->history;

  my $all_approved  = 0;
  my $num_revisions = 0;

  # the list of revisions ends with the most recently approved revision. Here
  # we only care about revisions *since* that the last approved one, so we
  # slice the array to chop off the last element
  foreach my $revision ( @$revisions ) {
    next if $revision->{revid} == $this->approved_revision;
    $all_approved += $revision->{user_approved} || 0;
    $num_revisions++;
  }

  return ( $all_approved == $num_revisions ) ? 1 : 0;
}

#-------------------------------------------------------------------------------

# boolean showing whether the current user is approved
has 'user_approved' => (
  is      => 'ro',
  isa     => 'Bool',
  lazy    => 1,
  builder => '_check_user_approved',
);

sub _check_user_approved {
  my $this = shift;
  return $this->_users->{$this->last_update_user} ? 1 : 0;
}

#-------------------------------------------------------------------------------

# somewhere to stash the values for the previous approval, so that we can roll
# back to them as necessary
has '_unapprove_values' => (
  is      => 'rw',
  isa     => 'HashRef',
  default => sub { {} },
);

#-------------------------------------------------------------------------------
#- custom methods --------------------------------------------------------------
#-------------------------------------------------------------------------------

# returns the name of the user who last updated this article in wikipedia

sub last_update_user {
  my $this = shift;
  return scalar @{ $this->history } ? $this->history->[-1]->{user} : undef;
}

#-------------------------------------------------------------------------------

# returns the comment that was given when the article was last updated in 
# wikipedia

sub update_comment {
  my $this = shift;
  return scalar @{ $this->history } ? $this->history->[-1]->{comment} : undef;
}

#-------------------------------------------------------------------------------

# updates the approval status of this article in the tracking database

sub update_approval {
  my ( $this, $revid, $approved_by, $updated ) = @_;

  die "not a valid revision ID ($revid)"
    if ( defined $revid and $revid !~ m/^\d+$/ );

  $this->update( { 
    approved_revision => $revid,
    approved_by      => $approved_by,
    updated          => $updated || \'NOW()',
  } );
}

#-------------------------------------------------------------------------------

# for an article that is redirected, this method returns an array containing
# a series of hashes, each with the keys "from" and "to", giving the titles of
# the article redirected from and to, and "row" giving a reference to this 
# object. Returns undef for articles that are not redirected.

sub get_redirects {
  my $this = shift;

  my $response = $this->_mw_api->api( {
    action    => 'query',
    titles    => $this->title,
    redirects => 1,
  } );

  unless ( $response ) {
    croak 'Error retrieving redirects for ' . $this->title . ' using API: '
          . $this->_mw_api->{error}->{details} 
          . ' (error code ' . $this->_mw_api->{error}->{code} . ')';
    return;
  }

  my $redirects = [];
  if ( $response->{query}->{redirects} ) {
    foreach my $redirect ( @{ $response->{query}->{redirects} } ) {
      $redirect->{row} = $this;
      push @{ $redirects }, $redirect; 
    }
  }
  else {
    return;
  }

  return $redirects;
}

#-------------------------------------------------------------------------------

no Moose;

# don't make the class immutable: it breaks "find_or_create"...
# __PACKAGE__->meta->make_immutable;
# __PACKAGE__->meta->make_immutable( inline_constructor => 0 );

1;

__END__
CREATE TABLE `wikipedia` (
  `title` tinytext NOT NULL,
  `updated` timestamp NOT NULL default CURRENT_TIMESTAMP,
  `approved_revision` int(10) unsigned default '0',
  `wikipedia_revision` int(10) unsigned default '0',
  `approved_by` varchar(100) NOT NULL,
  `pfam_status` enum('active','inactive','pending') NOT NULL default 'pending',
  `rfam_status` enum('active','inactive','pending') NOT NULL default 'pending',
  PRIMARY KEY  (`title`(256))
) ENGINE=InnoDB DEFAULT CHARSET=latin1

