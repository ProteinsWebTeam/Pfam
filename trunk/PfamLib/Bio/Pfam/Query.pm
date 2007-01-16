
=head1 NAME

Bio::Pfam::Query - a wrapper around a database query

=head1 SYNOPSIS

my $dbWrapper = Bio::Pfam::DB_Wrapper->new or die "error getting wrapper";
my $resultXML = $query->getXMLResults;

my $queryId     = $query->getId; # get the ID for the query
my $queryString = $query->getQueryString; # get the SQL query string
my $queryDesc   = $query->getDescription; # get the description from the XML

=head1 DESCRIPTION

  A wrapper around an SQL query. The query is defined in an XML
  configuration file.

  Note that any call that in turn calls DBI could end up throwing an
  exception. Wrap those calls in "eval":

    my $dom;
    eval {
      $dom = $query->getResults( { "search_word" => "blah" } );
    };
    if( $@ ) {
      confess "(EE) ERROR: problem when getting results:\n$@";
    }

=cut

package Bio::Pfam::Query;

use strict;
use warnings;
use Carp;

#use Apache::Reload;
#use Apache::DBI qw( :sql_types );

use XML::LibXML;

use Log::Log4perl qw( get_logger :levels );

use Exporter;

our @ISA = qw( Exporter );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 new (constructor)

  Usage   : my $query = Bio::Pfam::Query->new( $dbh, $xmlNode );
  Function: Constructor. This isn\'t really intended to be used by external
          : code, just by DB_Wrapper.
  Args    : scalar - reference to a DBI database handle
          : scalar - reference to a XML::LibXML node that defines the query
  Returns : scalar - reference to this object

=cut

sub new {

  my $class = shift;
  my $dbh = shift;
  my $node = shift;

  my $this = {};
  bless( $this, $class );

  $this->{dbh} = $dbh;
  $this->{node} = $node;
  $this->{id} = $node->getAttribute( "id" );

  $this->{log} = get_logger( "Bio::Pfam::Query" );
  $this->{log}->level( $DEBUG );

  $this->_prepare;
  $this->{log}->debug( "created Query" );

  return $this;

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _prepare - PRIVATE METHOD

  Usage   : $this->_prepare;
  Function: extracts the SQL query string from the XML and prepares the
          : DBI query. Stores a reference to the DBI statement handle on
          : the object.
  Args    : none
  Returns : none

=cut

sub _prepare {

  my $this = shift;

  $this->{log}->info( "preparing Query \"" . $this->{id} . "\"" );

  $this->{id}     = $this->{node}->getAttribute( "id" );
  $this->{query}  = $this->{node}->findvalue( "queryString/text()" );
  $this->{desc}   = $this->{node}->findvalue( "description" );
  $this->{label}  = $this->{node}->findvalue( "label" );

  # we'll set this for real later
  $this->{numHits} = 0;

  # get input parameter and result column details from the XML
  $this->_extract( "paramList/param", "paramNodes", "params" );
  $this->_extract( "columnList/column", "columnNodes", "columns" );

  $this->{log}->debug( $this->{id} . " => |$this->{query}|" );
  $this->{log}->debug( "query description: |$this->{desc}|" );

  # not checking the error status, because it's assumed that the
  # database will RaiseErrors on failure
  $this->{sth} = $this->{dbh}->prepare_cached( $this->{query} );

  $this->{log}->debug( "statement handle: |$this->{sth}|" );
}

#-------------------------------------------------------------------------------

=head2 _extract - PRIVATE METHOD

  Usage:    $this->_extract( "paramList/param", "paramNodes", "params" );
  Function: extracts simple details from the XML - will only work for a
            few very specific XML fragments, but that's all it's intended
            to be used for...
  Args:     none
  Returns:  none - puts results into the object hash.

=cut

sub _extract {
  my $this = shift;
  my( $xpath, $nodes, $strings ) = @_;

  $this->{$strings} = ();
  $this->{$nodes} = ();

  my( $id, $desc );
  foreach ( $this->{node}->findnodes( $xpath )->get_nodelist() ) {

	# keep an array with the raw XML Nodes
  	push @{$this->{$nodes}}, $_;

	# and build a hash with the data, rather than XML nodes, which we
	# can hand to callers
	$id = $_->getAttribute( "id" );
	$desc = $_->textContent;
	push @{$this->{$strings}}, { "id"   => $id,
								 "desc" => $desc };
  }
}

#-------------------------------------------------------------------------------

=head2 _execute - PRIVATE METHOD

  Usage:    $this->execute( $arguments ) ;
  Function: executes the actual SQL query
  Args:     scalar - ref to hash containing the arguments for the query
  Returns:  boolean - true if the query was successful, false otherwise
  Throws:   exception on DBI problems

=cut

sub _execute {

  my $this = shift;
  my $arguments = shift;

  #TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO
  # make this a bit smarter... we need either to fail more elegantly
  # or to cope better if the caller tried to execute the statement
  # twice. Can that even be done in DBI ?
  #TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO
  return 0 if $this->{executed};

  $this->{log}->debug( "----------------------------------------" );
  $this->{log}->debug( "executing Query \"" . $this->{id} . "\"" );

  # we need to check that we have all the parameters that we need to
  # run the query; count up how many parameters the SQL will need
  my( $index, $count );
  my $paramListSatisfied = 0;
  foreach my $paramNode ( @{$this->{paramNodes}} ) {
	$index = $paramNode->getAttribute( "index" );
	$count = defined $paramNode->getAttribute( "count" )
	  ? $paramNode->getAttribute( "count" ) : 1;
	$paramListSatisfied += $index * $count;
  }

  $this->{log}->debug( "expecting $paramListSatisfied parameters, found "
					   . scalar( keys %{$arguments} ) . " (neglecting \"count\")" );

  # keep track of the user supplied parameters for a little later
  $this->{userParams} = ();

  # walk the required params in the XML, rather than the user supplied
  # parameters list
  foreach my $paramNode ( @{$this->{paramNodes}} ) {

	$this->{log}->debug( "paramNode: |$paramNode|" );

	my $index = $paramNode->getAttribute( "index" );
	my $id    = $paramNode->getAttribute( "id" );
	my $count = defined $paramNode->getAttribute( "count" )
	  ? $paramNode->getAttribute( "count" ) : 1;

	$this->{log}->debug( "index: |$index|" );
	$this->{log}->debug( "id:    |$id|" );
	$this->{log}->debug( "count: |$count|" );

	# if the dictionary requires it, mess with the search string and
	# add "+" in front of each word
	my $localArguments = ();
	my $prefix = "";
	my $suffix = "";
	if( defined $paramNode->getAttribute( "method" ) and
		        $paramNode->getAttribute( "method" ) eq "fulltext" ) {
	  $this->{log}->debug( "adding boolean text operator to argument" );
	  $prefix = "+";
	}

	# again, if required, add a wildcard to the end of each argument
	if( $paramNode->getAttribute( "wildcard" ) ) {
	  $this->{log}->debug( "adding wildcard operator to argument" );
	  $suffix = "*";
	}

	# see if we need to apply a mapping pattern to the argument
	if( $prefix or $suffix ) {
	  $localArguments->{$id} = join " ", map { $_ = "$prefix$_$suffix" } split /\s+|\W/, $arguments->{$id};
#	  $localArguments->{$id} = join " ", map { $_ = "+$_" } split /\s+/, $arguments->{$id};
	  $this->{log}->debug( "modified query arguments: |$localArguments->{$id}|" );
	} else {
	  $localArguments->{$id} = $arguments->{$id};
	}

	for( my $i = $index; $i < $index + $count; $i++ ) {

	  $this->{log}->debug( "index $i, id \"$id\"" );

	  if( defined( $localArguments->{$id} ) ) {

		$this->{sth}->bind_param( $i, $localArguments->{$id} );
		$paramListSatisfied--;

		$this->{userParams}->{$id} = $localArguments->{$id};
	  }
	}
  }

  # did we satisfy all of the parameters ?
  confess "failed to satisfy all parameters for query \"".$this->{id}."\"; aborted"
	if $paramListSatisfied > 0;

  # yes, we bound all the required parameters for the statement; run it
  $this->{sth}->execute
	or confess "DBI execute failed for query \"$this->{id}\" ("
	  . $this->{sth}->errstr ."); aborted";

  # set a flag to show that this handle has now been executed
  $this->{executed} = 1;

  return 1;
}

#-------------------------------------------------------------------------------
#- public methods --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getResults

  Usage:    my $xmlDocument = $query->getResults( $arguments ) ;
  Function: shortcut to getXMLResults
  Args:     scalar - ref to hash containing the arguments for the query
  Returns:  scalar - ref to XML Document, undef if no results
  Throws:   exception on DBI problems (actually thrown in _execute)

=cut

sub getResults {
  my $this = shift;
  $this->getXMLResults( @_ );
}

#-------------------------------------------------------------------------------

=head2 getXMLResults

  Usage:    my $xmlDocument = $query->getXMLResults( $arguments ) ;
  Function: returns an XML Document, containing the result of running this query
            with the specified arguments
  Args:     scalar - ref to hash containing the arguments for the query
  Returns:  scalar - ref to XML Document, undef if no results
  Throws:   exception on DBI problems (actually thrown in _execute)

=cut

sub getXMLResults {

  my $this = shift;
  my $arguments = shift;

  # execute the query
  $this->_execute( $arguments )
	or die "$!\nfailed to execute query \"" . $this->{id} . "\"; aborted";

  # build an XML document for the results

  my $resultDOM = XML::LibXML::Document->new;

  # add the root element
  my $root = $resultDOM->createElement( "resultSet" );
  $resultDOM->setDocumentElement( $root);

  # put the query, the search parameters and the result columns into
  # the XML too

  # query string
  $root->appendTextChild( "queryString", $this->{query} );

  # user supplied parameters
  my $paramListNode = $resultDOM->createElement( "paramList" );
  $root->appendChild( $paramListNode );

  foreach my $param ( keys %{$this->{userParams}} ) {
	my $paramNode = $resultDOM->createElement( "param" );
	$paramListNode->appendChild( $paramNode );
	$paramNode->setAttribute( "id", $param );
	$paramNode->appendTextNode( $this->{userParams}->{$param} );
  }

  # column details
  my $columnList = $resultDOM->createElement( "columnList" );
  $root->appendChild( $columnList );

  foreach my $colName ( @{$this->{sth}->{NAME}} ) {
	$columnList->appendTextChild( "column", $colName );
  }

  # add the row list element to the document
  my $rowList = $resultDOM->createElement( "rowList" );
  $root->appendChild( $rowList );

  # retrieve the results and stuff them into the XML DOM
  my $numRows = 0;
  while( my $rs = $this->{sth}->fetchrow_arrayref ) {

	# add a new row to the rowList
	my $row = $resultDOM->createElement( "row" );
	$rowList->appendChild( $row );

	# walk over all the columns in the result set
	foreach my $column ( @{$rs} ) {

	  my $columnElement = $resultDOM->createElement( "column" );
	  #$columnElement->setAttribute( "name", $columnName );
	  $columnElement->appendTextNode( defined $column ? $column : "" );

	  $row->appendChild( $columnElement );
	}
	$numRows++;
  }

  # if we got ANY results, add the row count to the XML; otherwise, if
  # there were no results at all, mark the result DOM as undefined and
  # hand that back to the caller TODO Do we want to do that ?
  if( $numRows ) {
	$rowList->setAttribute( "numRows", $numRows );
  } else {
	undef $resultDOM;
  }

  return $resultDOM;
}

#-------------------------------------------------------------------------------

=head2 getRawResults

  Usage:    my $arrayRef = $query->getRawResults( { pfamA_acc => "p450" } ) ;
  Function: returns an array reference for the array containing the result of
            running this query with the specified arguments. See "Programming
            the Perl DBI", p. 132, for the data structure
  Args:     scalar - ref to hash containing the arguments for the query
  Returns:  scalar - ref to array with results
  Throws:   exception on DBI problems

=cut

sub getRawResults {
  my $this = shift;
  my $arguments = shift;

  # execute the query
  $this->_execute( $arguments )
	or die "$!\nfailed to execute query \"" . $this->{id} . "\"; aborted";

  # get all the results at once, as an array ref
  $this->{rs} = $this->{sth}->fetchall_arrayref;
  $this->{numHits} = scalar @{$this->{rs}};

  return $this->{rs};
}

#-------------------------------------------------------------------------------

=head2 retrieve

  Usage:    my $rowArrayRef = $query->retrieve( acc => "PF00067" );
  Function: returns the first row of the result set for this query
  Args:     scalar - ref to hash containing the arguments
  Returns:  scalar - ref to array containing the first row of the result array

=cut

sub retrieve {
  my $this = shift;
  my $arguments = shift;

  # execute the query as normal
  $this->_execute( $arguments )
	or die "$!\nfailed to execute query \"" . $this->{id} . "\"; aborted";

  # retrieve just the first row
  $this->{rs} = $this->{sth}->fetchall_arrayref;
  $this->{numHits} = scalar @{$this->{rs}};

  # and return just the first row
  return $this->{rs}->[0];
}

#-------------------------------------------------------------------------------

=head2 getId

  Usage:    my $queryId = $query->getId;
  Function: returns the ID for this query
  Args:     none
  Returns:  scalar - ID string for the Query

=cut

sub getId {
  my $this = shift;
  return $this->{id};
}

#-------------------------------------------------------------------------------

=head2 getQueryString

  Usage:    my $queryString = $query->getQueryString;
  Function: returns the SQL query string
  Args:     none
  Returns:  scalar - SQL query string

=cut

sub getQueryString {
  my $this = shift;
  return $this->{query};
}

#-------------------------------------------------------------------------------

=head2 getParams

  Usage:    my $paramHash = $query->getParams;
  Function: returns a ref to a hash containing the parameter IDs as the key,
            the parameter description as value
  Args:     none
  Returns:  scalar - ref to hash containing parameter details

=cut

sub getParams {
  my $this = shift;
  return $this->{params};
}

#-------------------------------------------------------------------------------

sub getColumns {
  my $this = shift;
  return $this->{columns};
}

#-------------------------------------------------------------------------------

sub getLabel {
  my $this = shift;
  return $this->{label};
}

#-------------------------------------------------------------------------------

sub getNumHits {
  my $this = shift;
  return $this->{numHits};
}

#-------------------------------------------------------------------------------

# =head2 getColumnTypes

#   Usage:    my $types = $query->getColumnTypes;
#   Function: returns an array containing the types of the columns in the
#             resultset. These types are defined in the DBI, e.g. SQL_CHAR == 1
#   Args:     none
#   Returns:  scalar - ref to array containing column types

# =cut

# sub getColumnTypes {
#   my $this = shift;
#   return $this->{columnTypes};
# }

#-------------------------------------------------------------------------------

=head2 getDescription

  Usage:    my $desc = $query->getDescription;
  Function: returns the description of the query from the configuration file
  Args:     none
  Returns:  scalar - description of the query

=cut

sub getDescription {
  my $this = shift;
  return $this->{desc};
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate (jt6@sanger.ac.uk)

=head1 VERSION

$Id: Query.pm,v 1.2 2007-01-16 08:55:55 jt6 Exp $

=cut

1;
