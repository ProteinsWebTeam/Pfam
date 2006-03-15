
=head1 NAME

  Bio::Pfam::QuerySet - encapsulates several Bio::Pfam::Query objects

=head1 SYNOPSIS

  my $dbWrapper = Bio::Pfam::DB_Wrapper->new or die "error getting wrapper";
  my $querySet  = $dbWrapper->getQuerySet( "mySet" );
  my $resultSet = $querySet->getRawResults( { term => "search terms" } );

=head1 DESCRIPTION

  This class encapsulates a set of Bio::Pfam::Query objects, which in
  turn perform specific queries on a database.

  QuerySet objects should be obtained from a DB_Wrapper, which does
  the actual database connection.

  The SQL queries are executed via Query objects, which take care of
  passing parameters to the DB and retrieving results for their
  individual queries.

  This class does the heavy lifting of checking that appropriate
  arguments are passed to the Query objects and the merging of the
  resulting sets of data that come back.

=cut

package Bio::Pfam::QuerySet;

use strict;
use warnings;
use Carp;
use Data::Dumper;

use Apache::Reload;
use Apache::DBI;

use XML::LibXML;
use Time::HiRes qw( gettimeofday );

use Log::Log4perl qw( get_logger :levels );

use Exporter;

our @ISA = qw( Exporter Bio::Pfam::Query );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 new (constructor)

    Usage   : my $querySet = Bio::Pfam::QuerySet->new( $id, $dbWrapper );
    Function: The constructor for the object. Requires the ID for this
            : query set, as well as a reference back to the DB_Wrapper that
            : generates it. This is so that the QS can call the wrapper to
            : get the actual Query objects when required.
    Args    : scalar - ID for the QS
            : scalar - reference to DB_Wrapper object
    Returns : none

=cut

sub new {

  my $class = shift;
  my $id = shift;
  my $wrapper = shift;

  my $this = {};
  bless( $this, $class );

  $this->{id} = $id;
  $this->{wrapper} = $wrapper;

  $this->{log} = get_logger( "Bio::Pfam::QuerySet" );
  $this->{log}->level( $WARN );

  $this->{queries} = ();

  $this->{log}->info( "created QuerySet object \"" . $this->{id} . "\"" );

  return $this;

}

#-------------------------------------------------------------------------------

=head2 getId

    Usage   : my $querySetId = $querySet->getId;
    Function: Returns the ID for this QS.
    Args    : none
    Returns : scalar - string, ID of this QS

=cut

sub getId {

  my $this = shift;

  return $this->{id};
}

#-------------------------------------------------------------------------------

=head2 addQuery

    Usage   : $querySet->addQuery( $queryId );
    Function: Add the query with the specified ID to this query set
    Args    : scalar - string, ID of the query to add to this set
    Returns : true if successful, undef otherwise

=cut

sub addQuery {

  my $this  = shift;
  my $id = shift;

  $this->{log}->debug( "adding Query \"$id\"" );

  # get the Query object from the wrapper
  my $query = $this->{wrapper}->getQuery( $id );

  $this->{log}->debug( "got Query from wrapper: |$query|" );

  return undef unless defined $query;

  # add it to this qs
  push @{$this->{queryIds}}, $query->getId;
  $this->{queries}->{$id} = $query;

  $this->{log}->info( "added query \"$id\" to set \"".$this->{id}."\"" );

  return 1
}

#-------------------------------------------------------------------------------

=head2 getQuery

    Usage   : my $query = $querySet->getQuery( $queryId );
    Function: Returns the Query object with the specified ID
    Args    : scalar - string, ID of the query to retrieve
    Returns : scalar - ref to Query object, if found, undef otherwise

=cut

sub getQuery {

  my $this = shift;
  my $queryId = shift;

  return $this->{queries}->{$queryId};
}

#-------------------------------------------------------------------------------

=head2 getQueryIds

    Usage   : my @queries = $querySet->getQueryIds;
    Function: Returns a list of the IDs for the queries in this set. Note that
            : this is the list of Query objects that have been added so far,
            : not the list of IDs that are specified in the XML config. If you
            : call this before the object has been properly populated by the
            : DB_Wrapper, you'll get an empty list.
    Args    : none
    Returns : array - list of IDs loaded into this set

=cut

sub getQueryIds {

  my $this = shift;

  return @{$this->{queryIds}};
}

#-------------------------------------------------------------------------------

=head2 getRawResults

    Usage   : my $resultSet = $querySet->getRawResults( { "term" => "wibble" } );
    Function: Executes the set of queries, passing them the search terms from
            : the argument hash.
            :
            : This method first checks that the required arguments for all
            : queries have been satisfied. All required parameters must be
            : found in the input hash, otherwise it will bail.
            :
            : Results are retrieved from each of the Query objects in the set
            : in turn. The order in which queries are executed is the same as
            : the order in which the IDs are given in the XML.
            :
            : The results from all Query objects are dropped into a single
            : result hash, using the first column of each row as the key.
            :
    Args    : scalar - ref to hash containing the search terms
    Returns : scalar - ref to hash with the results
    Throws  : exception on problem with input parameters
            : exception on DBI problem from Query object

=cut

sub getRawResults {
  my $this = shift;
  my $arguments = shift;

  $this->{log}->level( $WARN );

  # first, check that the individual queries have the same number of
  # parameters

  my %requiredParamIDs;
  my $numParams = 0;
  foreach my $query ( values %{$this->{queries}} ) {

	my $params = $query->getParams;

	# store the IDs of the required parameters for a little later and,
	# at the same time, work out how many parameters are actually
	# required for this query (we have to walk the hash anyway, so add
	# it up manually instead of doing another "scalar keys %hash"
	# thing...

	my $queryParams = 0;
	foreach ( @$params ) {
	  $requiredParamIDs{$_->{id}} = $_->{id};
	  $queryParams++;
	}

	$this->{log}->debug( "query \"" . $query->getId
						 . "\" requires |$queryParams| parameters" );

	# get the number of parameters for the first query and move
	# straight on to the next one
	unless( $numParams ) {
	  $numParams = $queryParams;
	  next;
	}

	# do we have enough parameters for this query ?
	confess "constituent queries have differing numbers of parameters; aborted"
	  unless( $numParams eq $queryParams );
  }

  $this->{log}->debug( "queries all require $numParams parameters" );

  # and check that we have all those parameters in our input
  my $paramListSatisfied = scalar keys %requiredParamIDs;

  foreach my $param ( keys %requiredParamIDs ) {
	$this->{log}->debug( "key: |$param|$requiredParamIDs{$param}|" );
	$paramListSatisfied--
	  if defined( $arguments->{$param} );
  }

  # did we satisfy all of the parameters ?
  confess "failed to satisfy all parameters for query set \"" . $this->{id}
	. "\"; aborted"
	  if( $paramListSatisfied > 0 );

  # execute the queries

  my %allResults;

  foreach my $query ( values %{$this->{queries}} ) {

	my $results;

	eval {
	  $results = $query->getRawResults( $arguments );
	}; if( $@ ) {
	  confess "(EE) ERROR: problem when getting results for query \""
		. $query->getId . "\":\n$@";
	}

	foreach my $row ( @$results ) {
	  my $acc = $row->[0];

	  # see if there's already a hit for this Pfam
	  my $oldRow = $allResults{$acc};

	  if( defined $oldRow ) {

		# there's an existing hit; just push the query ID for this
		# query onto the array at the end of that pre-existing hit

		$oldRow->[-1]->{$query->getId} = "";

	  } else {

		# first hit for this Pfam; add the query ID to the row and
		# drop the row into the results hash

		push @$row, { $query->getId => "" };
		$allResults{$row->[0]} = $row;
	  }

	}

#  	$this->{log}->debug( "results for query \"" . $query->getId . "\"" );
#  	foreach my $row ( @$results ) {
#  	  $this->{log}->debug( "row: |$row|" );
#  	  foreach my $col ( @$row ) {
#  		$this->{log}->debug( "col: |$col|" );
#  	  }
#  	}

  }

  return \%allResults;

}

#-------------------------------------------------------------------------------

=head2 getXMLResults

    Usage   : my $XMLResultSet = $querySet->getXMLResults( { "term" => "wibble" } );
    Function: Runs queries just like "getRawResults", but returns them as
            : an XML Document
    Args    : scalar - ref to hash containing the search terms
    Returns : scalar - ref to XML Document containing results
    Throws  : exception on DBI problem from Query object

=cut

sub getXMLResults {
  my $this = shift;
  my $arguments = shift;

  my $before = gettimeofday();
  my $allResults = $this->getRawResults( $arguments );
  my $timeTaken = sprintf "%.3f", gettimeofday() - $before;

  my $resultDOM = XML::LibXML::Document->new;

  # add the root element
  my $root = $resultDOM->createElement( "resultSet" );
  $resultDOM->setDocumentElement( $root);

  # add the time taken for the query
  $root->setAttribute( "time", $timeTaken );

  # add the row list element to the document
  my $rowList = $resultDOM->createElement( "rowList" );
  $root->appendChild( $rowList );

  # retrieve the results and stuff them into the XML DOM
  my $numRows = 0;
  foreach my $row ( sort keys %$allResults ) {

	# add a new row to the rowList
	my $rowNode = $resultDOM->createElement( "row" );
	$rowList->appendChild( $rowNode );

	# walk over all the columns in the result set
	foreach my $column ( @{$allResults->{$row}} ) {
	
	  my $columnElement = $resultDOM->createElement( "column" );
	  $columnElement->appendTextNode( defined $column ? $column : "" );

	  $rowNode->appendChild( $columnElement );
	}
  }

  return $resultDOM;
}

#-------------------------------------------------------------------------------

=head2 getXMLResults

    Usage   : my $XMLResultSet = $querySet->getResults( { "term" => "wibble" } );
    Function: Shortcut to getResults
    Args    : scalar - ref to hash containing the search terms
    Returns : scalar - ref to XML Document containing results
    Throws  : exception on DBI problem from Query object

=cut

sub getResults {
  my $this = shift;
  my $arguments = shift;

  return $this->getXMLResults( $arguments );
}

#-------------------------------------------------------------------------------

1;
