
=head1 NAME

  Bio::Pfam::DB_Wrapper - builds wrappers around various database queries

=head1 SYNOPSIS

  my $dbWrapper = Bio::Pfam::DB_Wrapper->new or die "error getting wrapper";
  my $query     = $dbWrapper->getQuery( "myQuery" );
  my $querySet  = $dbWrapper->getQuerySet( "mySet" );

=head1 DESCRIPTION

  This class builds a set of objects that encapsulate database
  queries. The queries are specified in an XML file. Individual Query or
  QuerySet objects can be retrieved by ID.

  There's the beginnings of a query caching mechanism here, whereby we
  see if we've already built a particular Query object, returning that
  in preference to building it from scratch. This caching is crude and
  probably won't work right now, because I've not taken into account the
  problems of DBI statements that have already been executed, etc. If
  this framework is ever used under mod_perl, it'll all need looking at
  again in detail.

  Right now the smart thing that's happening is that Query and QuerySet
  objects aren't actually instantiated until they're asked for through
  the "getQuery" and "getQuerySet" calls.

  Note that the "new" call in turn calls DBI and could end up throwing an
  exception if the database connection fails. Wrap calls to DB_Wrapper->new
  in "eval" to catch them:

    my $dbWrapper;
    eval {
	  $dbWrapper = Bio::Pfam::DB_Wrapper->new;
    };
    if( $@ ) {
	  die "failed to get a wrapper: $@";
    }

=cut

package Bio::Pfam::DB_Wrapper;

use strict;
use warnings;
use Carp;

use Apache::Reload;
use Apache::DBI;

use XML::LibXML;

use Bio::Pfam::Query;
use Bio::Pfam::QuerySet;

use Log::Log4perl qw( get_logger :levels );

use Exporter;

our @ISA = qw( Exporter );

# the location of the XML configuration file
my $XML_CONFIG = "$ENV{DOCUMENT_ROOT}/../data/software/pfam/queries.xml";

#-------------------------------------------------------------------------------
#- constructor -----------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 new (constructor)

    Usage   : my $dbWrapper = Bio::Pfam::DB_Wrapper->new;
    Function: The constructor for the object. Takes no arguments.
    Args    : none
    Returns : scalar - reference to this object
    Throws  : exception on failure to connect to DB

=cut

sub new {

  my $class = shift;
  my $this = {};
  bless( $this, $class );

  $this->{log} = get_logger( "Bio::Pfam::DB_Wrapper" );
  $this->{log}->level( $WARN );

  $this->{log}->debug( "got a logger for DB_Wrapper" );

  $this->{queries}   = ();
  $this->{querySets} = ();
  $this->{queryIds}  = ();

  # parse the config
  $this->_openXML;

  # get a database handle
  $this->_connect;

  # prepare the queries defined in the config
  $this->_prepareQueries;

  $this->{log}->info( "created database wrapper object" );

  return $this;

}

#-------------------------------------------------------------------------------
#- private ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _openXML - PRIVATE METHOD

    Usage   : $this->_openXML;
    Function: loads the XML configuration file as an in-memory DOM and parses
            : out the DB connection details
    Args    : none
    Returns : none

=cut

sub _openXML {
  my $this = shift;

  # get an XML parser and hand it the XML configuration file. Stash
  # the resulting Document in the object
  my $parser = XML::LibXML->new;
  #my $xmlConfig = join "", <DATA>;
  #$this->{XMLroot} = $parser->parse_string( $xmlConfig )->documentElement();

  $this->{XMLroot} = $parser->parse_file( $XML_CONFIG )->documentElement();

  $this->{log}->debug( "opened query XML file" );

  # extract the database connection parameters from the config
  $this->{dbType} = $this->{XMLroot}->findvalue( "/queryConfig/database/type" );
  $this->{dbName} = $this->{XMLroot}->findvalue( "/queryConfig/database/name" );
  $this->{dbHost} = $this->{XMLroot}->findvalue( "/queryConfig/database/host" );
  $this->{dbPort} = $this->{XMLroot}->findvalue( "/queryConfig/database/port" );
  $this->{dbUser} = $this->{XMLroot}->findvalue( "/queryConfig/database/user" );
  $this->{dbPass} = $this->{XMLroot}->findvalue( "/queryConfig/database/pass" );

}

#-------------------------------------------------------------------------------

=head2 _connect - PRIVATE METHOD

    Usage   : $this->_connect;
    Function: connects to the database and store the database handle. Raises
            : an exception in case of difficulties.
    Args    : none
    Returns : none
    Throws  : exception on failure to connect to DB

=cut

sub _connect {

  my $this = shift;

  $this->{log}->debug( "connecting to database..." );

  # build the connection string
  my $dsn = "dbi:"
	. $this->{dbType} . ":"
	. $this->{dbName} . ":"
	. $this->{dbHost} . ":"
	. $this->{dbPort};

  $this->{dbh} = DBI->connect( $dsn,
							   $this->{dbUser},
							   $this->{dbPass},
							   { RaiseError => 1,
								 PrintError => 0 } )
	or confess "couldn't connect to database (".$DBI::errstr."); aborted";

  $this->{log}->debug( "opened database connection" );
}

#-------------------------------------------------------------------------------

=head2 _prepareQueries - PRIVATE METHOD

    Usage   : $this->_prepareQueries;
    Function: reads the XML configuration file and stores IDs for each defined
            : query, QuerySet objects for defined query sets. Every query and
            : every query set has a unique identifier, defined in the XML.
            : These are used as keys to store the objects in internal hashes,
            : and for retrieving them for calling objects.
            : Refer to the XML configuration file to get the IDs for the
            : externally defined queries.
    Args    : none
    Returns : none

=cut

sub _prepareQueries {

  my $this = shift;

  $this->{log}->info( "preparing queries..." );

  # build Query objects
  my( $query, $numSuccesses );
  $numSuccesses = 0;

  # get the list of queries
  foreach ( $this->{XMLroot}->findnodes( "/queryConfig/queryList/query" ) ) {

	my $id = $_->getAttribute( "id" );
	$this->{log}->debug( "found query: |$id|" );

	push @{$this->{queryIds}}, $id;

  }
  $this->{log}->info( "found ",scalar @{$this->{queryIds}}," queries in config." );

  # store the query set IDs too
  foreach ( $this->{XMLroot}->findnodes( "/queryConfig/querySetList/querySet" ) ) {

	my $id = $_->getAttribute( "id" );
	$this->{log}->debug( "found query set: |$id|" );

	push @{$this->{querySetIds}}, $id;
  }
  $this->{log}->info( "found ",scalar @{$this->{querySetIds}}," query sets in config." );

}

#-------------------------------------------------------------------------------
#- public ----------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getQuery

    Usage   : my $queryObject = $dbWrapper->getQuery( "myQuery" );
    Function: returns a reference to the Query object that encapsulates the
            : named query.
    Args    : scalar - ID for required query.
    Returns : scalar - reference for Query object

=cut

sub getQuery {
  my $this = shift;
  my $id   = shift;

  # have we already instantiated this Query ?

  # yes, hand it straight back
  return $this->{queries}->{$id}
	if( defined $this->{queries}->{$id} );

  # no, build it now

  $this->{log}->debug( "Query \"$id\" not found; building now" );

  # first, get the XML node that defines this query
  my $node = $this
	->{XMLroot}
	  ->findnodes( "/queryConfig/queryList/query[\@id='$id']" )
		->shift;
  return undef unless defined $node;

  $this->{log}->debug( "building a new Query using |$node|" );

  my $query = Bio::Pfam::Query->new( $this->{dbh}, $node );
  $this->{queries}->{$id} = $query;

  $this->{log}->debug( "built Query object (\"" . $id . "\"): " . $query );

  return $query;
}

#-------------------------------------------------------------------------------

=head2 getQuerySet

    Usage   : my $querySetObject = $dbWrapper->getQuerySet( "myQuerySet" );
    Function: returns a reference to the QuerySet object with the
            : specified ID.
    Args    : scalar - ID for required query set
    Returns : scalar - reference to QuerySet object

=cut

sub getQuerySet {
  my $this = shift;
  my $id   = shift;

  # have we instantiated the query set ?
  return $this->{querySets}->{$id}
	if defined $this->{querySets}->{$id};

  # no, build it

  $this->{log}->debug( "QuerySet \"$id\" not found; building now" );

  # get the XML for the query set
  my $node = $this
	->{XMLroot}
	  ->findnodes( "/queryConfig/querySetList/querySet[\@id='$id']" )
		->shift;
  return undef unless defined $node;

  $this->{log}->debug( "building a new QuerySet using |$node|" );


  my $qs = Bio::Pfam::QuerySet->new( $id, $this );
  $this->{log}->debug( "built QuerySet object (\"" . $id . "\"): " . $qs );

  $this->{querySets}->{$id} = $qs;

  # add the queries to this new set
  foreach my $qn ( $node->getChildrenByTagName( "query" ) ) {
	my $qId = $qn->textContent;
	$qs->addQuery( $qId );
	$this->{log}->debug( "added query \"$qId\" to query set \"$id\"" );
  }

  return $qs;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

  John Tate (jt6@sanger.ac.uk)

=head1 VERSION

  $Id: DB_Wrapper.pm,v 1.1.1.1 2006-03-15 09:56:25 rdf Exp $

=cut

1;
