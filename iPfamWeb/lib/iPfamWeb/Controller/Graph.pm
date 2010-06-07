# Graph.pm
# pg6 16062009

=head1 NAME

iPfamWeb::Controller::Graph - draw inter-domain relationships using GraphViz

=cut

package iPfamWeb::Controller::Graph;

=head1 DESCRIPTION

This controller is responsible drawing graphs showing inter-domain
interactions

=cut

use strict;
use warnings;

use GraphViz;
use LWP::UserAgent;
use XML::LibXML;

#use List::Compare;

use Data::Dump qw( dump );

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract the query terms from the URL and de-taint them.

=cut

sub begin : Private {

  my ( $self, $c ) = @_;

  # first we see if we called with an tainted entry;
  my $tainted_entry = $c->req->param('entry')
    || $c->req->param('acc')
    || $c->req->param('id');
  return unless $tainted_entry;
  $c->log->debug("Graph:begin: The input tainted entry is $tainted_entry ")
    if $c->debug;

# now split the tainted_entry using ~, as the entries are explicitly joined using '~'
  my @tainted_entries = split( '~', $tainted_entry );
  my $entry;

  if ( scalar @tainted_entries == 1 ) {
    $entry = $tainted_entries[0];
  }
  else {
    $entry = $tainted_entries[-1];
  }
  $c->log->debug("Graph:begin: The entry is $entry ")
    if $c->debug;
  $c->stash->{ entry } = $entry;
  
  my @entries;
  my $position = 0;
  my $acc      = {};

  for ( my $i = $#tainted_entries ; $i >= 0 ; $i-- ) {

    if ( $tainted_entries[$i] =~ m/^([\w\._-]+)$/ ) {

      $c->log->debug("Graph::begin: the detainted entry now is $1")
        if $c->debug;
      push @entries, $1;

      # now look for the pfam accessions for the entry
      my $rs =
        $c->model('iPfamDB::Pfama')
        ->search( [ { pfama_acc => $1 }, { pfama_id => $1 } ] );
      my $pfam = $rs->first if ( defined $rs );

      if ( $1 eq $entry ) {
        unless ( defined $pfam ) {
          $c->log->debug( "Graph::begin: No pfam accessions were found for entry $1" )
            if $c->debug;
          $c->stash->{errorMsg} = 'No Pfam family found for entry $1 ';
          return;
        }
      }

      # now store the position and the pfam accession for the entry;
      if ( defined $pfam ) {
        $c->log->debug( "Graph::begin:the entry $1 has pfam_acc and is added to stash" )
        if $c->debug;
        
#        # always use teh pfama_id as key, so if the user provides the input as pfam_acc, change them 
#        # so that we use pfama_id as a key;
#        my $key;
#        if( $1 eq $pfam->pfama_acc ){
#          $key = $pfam->pfama_id;
#        }else{
#          $key = $1;  
#        }
#        
        $acc->{$1}->{pfam}     = $pfam->pfama_acc;
        $acc->{$1}->{position} = $position;
        
      }
      $position++;

    }    # end of if
    else {
      $c->log->debug( "Graph:begin: The input entry $1 contains invalid characters" )
        if $c->debug;
    }    # end of else

  }    # end of foreach tainted_entry

  $c->log->debug( "Graph::begin: The dump of the acc is " . dump($acc) )
    if $c->debug;

  # now stash the acc;
  $c->stash->{acc}     = $acc;
  $c->stash->{entries} = [ reverse @entries ];

  # now get the list of databases;
  $c->stash->{db_list} = [];

  if ( $c->req->param('database') ) {
    foreach ( $c->req->param('database') ) {
      $c->log->debug("Graph::begin: checking for database |$_|" )
        if $c->debug;

      # ditch anything too dodgy
      next unless m/^([\w-]+)$/;

      # make sure the server that we're trying to enable is actually one that
      # we've configured
      next unless defined $self->{servers_hash}->{$1};

      # explicitly limit the number of servers. The form should also limit the
      # number of server checkboxes that can be set, but we're not going to
      # trust that limit, since it's enforced by javascript...
      last if scalar @{ $c->stash->{db_list} } >= $self->{max_servers};

      $c->log->debug("Graph::begin: adding |$_|") 
        if $c->debug;
      push @{ $c->stash->{db_list} }, $1;
    }
  }

}    # end of begin

#-------------------------------------------------------------------------------

=head2 graph : Path

Generates the HTML page showing the empty form and a graph, if we can generate
one for this request.

=cut

sub graph : Path {
  my ( $self, $c ) = @_;

  $c->log->debug("Graph::graph: Inside the Graph controller") 
    if $c->debug;

  # stash the list of servers that the user can choose to query
  $c->stash->{servers_hash} = $self->{servers_hash};
  $c->stash->{servers_list} = $self->{servers_list};
  $c->stash->{max_servers}  = $self->{max_servers};

  # handles all of the page rendering
  $c->stash->{template} = 'pages/graph.tt';
  
  # switch ot fire off the request, if we get an acc,
  if( defined $c->stash->{ entry } ){
    $c->log->debug( "Graph::graph:: we got the acc so set the flag to fire up ajax request" ) 
      if $c->debug;
    $c->stash->{ ajaxStart } = 1;
  }else{
    $c->log->debug( "Graph::graph:: we dint get the acc so set the flag to stop ajax request" )
      if $c->debug;
    $c->stash->{ ajaxStart } = 0;
  }
  
}

#-------------------------------------------------------------------------------

=head2 map : Local

Generates the snippet of HTML that contains the image map for the interaction
graph. Intended to be used from an AJAX call. 

=cut

sub map : Local {
  my ( $self, $c ) = @_;

  $c->stash->{template} = 'components/graph_map.tt';

  unless ( $c->stash->{entries} ) {
    $c->stash->{errorMsg} = 'No Input accession/ID found.';
    return;
  }

  $c->forward('get_graph');
  unless ( defined $c->stash->{map} ) {
    $c->log->debug('Graph::draw: no map found') 
      if $c->debug;
    $c->stash->{errorMsg} ||= 'Failed to build a graph for this Pfam family.';
    return;
  }

  # rebuilding the input entries;
  $c->log->debug( "Graph::Map: the dump of the entries is " . dump( $c->stash->{entries} ) ) 
    if $c->debug;
  my $entry = join( '~', @{ $c->stash->{entries} } );

  my $parameters = {
    entry    => $entry,
    database => $c->stash->{db_list}
  };
  $c->log->debug( 'Graph::map: parameters for image_uri: ', dump $parameters )
   if $c->debug;

  $c->stash->{image_uri} = $c->uri_for( '/graph/image/', $parameters );
  $c->log->debug( 'Graph::graph: setting image_uri to |' . $c->stash->{image_uri} . '|' )
    if $c->debug;

  # make sure that the image map isn't cached...
  $c->res->header( 'Pragma'  => 'no-cache' );
  $c->res->header( 'Expires' => 'Thu, 01 Jan 1970 00:00:00 GMT' );
  $c->res->header( 'Cache-Control' => 'no-store, no-cache, must-revalidate,'
      . 'post-check=0, pre-check=0, max-age=0' );
}

#-------------------------------------------------------------------------------

=head2 get_graph : Private

Generates the interaction graph for the specified family. We try to be sensible 
about caching data rather than retrieving it always from source.

=cut

sub get_graph : Private {
  my ( $self, $c ) = @_;

  $c->log->debug('Graph::get_graph: getting graph image and map') 
    if $c->debug;

  my $entries = join '', @{ $c->stash->{entries} };
  $c->log->debug("Graph:get_graph: the list of entries joined are $entries");

  # cache keys for the image (a PNG) and the image map (HTML snippet). Note that
  # we sort the list of enabled databases, so that we are sure to get the same
  # order whenever we build a cache key using them
  my $db_list = join '', @{ $c->stash->{db_list} };

  $c->log->debug( "Graph:get_graph: the list of db_list are ",
    dump $c->stash->{db_list} ) 
      if $c->debug;
  $c->log->debug("Graph:get_graph: the list of db_list are $db_list") 
    if $c->debug;

  my $image_cache_key = 'graphImage' . $db_list . $entries;
  my $map_cache_key   = 'graphMap' . $db_list . $entries;
  my $ligSource_cache_key = 'ligSource'.$db_list. $entries;
  
  # see if the cache gets us anywhere
  $c->stash->{image} = $c->cache->get($image_cache_key);
  $c->stash->{map}   = $c->cache->get($map_cache_key);
  $c->stash->{ ligSource } = $c->cache->get( $ligSource_cache_key );
  
  if ( $c->debug ) {
    $c->log->debug("Graph::get_graph: image cache key: |$image_cache_key|");
    $c->log->debug("Graph::get_graph: map cache key:   |$map_cache_key|"); 
    $c->log->debug("Graph::get_graph: ligSource cache key:   |$ligSource_cache_key|");
  }
    
  if (  defined $c->stash->{image}
    and defined $c->stash->{map} ) {
    $c->log->debug('Graph::get_graph: retrieved map and image from cache') 
      if $c->debug;
  }
  else {
    $c->forward( 'build_graph', [ $image_cache_key, $map_cache_key, $ligSource_cache_key ] );
  }
}

#-------------------------------------------------------------------------------

=head2 build_graph : Private

Does the donkey work of generating the graph.

=cut

sub build_graph : Private {
  my ( $self, $c, $image_cache_key, $map_cache_key, $ligSource_cache_key ) = @_;

  $c->log->debug('Graph::build_graph: generating map and image') 
    if $c->debug;

  # get the raw interaction data and process it a little
  $c->forward('get_interaction_data');

  # before drawing the image validate the interactions,
  $c->forward('validate_interaction');

  # return if any error messages were stored.
  if ( defined $c->stash->{errorMsg} ) {
    return;
  }

  $c->log->debug('Graph::build_graph: retrieved data; instantiating GraphViz object') 
    if $c->debug;

  # build a new GraphViz object and let it rip
  my $g;
  eval {
    $ENV{PATH} = '/bin:/usr/bin:/usr/local/bin';

    $g = GraphViz->new(
      name         => 'interaction_graph',    # ID for the image map
      layout       => 'neato',
      random_start => 0,
      directed     => 0,
      rankdir      => 1,
      overlap      => 'false',
      epsilon      => 0.001
    );
  };
  if ($@) {
    $c->log->debug('Graph::build_graph: problem generating a GraphViz object') 
      if $c->debug;
    $c->stash->{errorMsg} ||= 'There was a problem generating your graph.';
    return;
  }

  $c->log->debug('Graph::build_graph: got a GV object; adding nodes') 
    if $c->debug;

  # first, interactors, i.e. graph nodes
  my %seen;
  foreach my $interactor ( values %{ $c->stash->{interactors} } ) {
    next if $seen{ $interactor->{node_name} };

    # build a node
    $g->add_node(
      $interactor->{node_name},
      label    => $interactor->{label},
      URL      => $interactor->{url},
      fontsize => 10,
      shape    => $self->{servers_hash}->{ $interactor->{source} }->{shape},
      fontname => 'arial'
    );
    $seen{ $interactor->{node_name} }++;

    # if this is the central node, set the style for it separately. Multiple
    # "add_node" calls will append attributes to a single node rather than
    # adding multiple ones

    my $acc = $c->stash->{acc};
    my $input_entry;
    if ( exists $acc->{ $interactor->{label} } ) {
      $input_entry = $interactor->{ label };
    }elsif( exists $acc->{ $interactor->{ node_name } } ){
      $input_entry = $interactor->{ node_name };
    }
    
    if( defined $input_entry ){
      $c->log->debug( "graph::build_graph: the color for $input_entry is ".$self->{ $acc->{ $input_entry }->{position} }) 
        if $c->debug;
      if ( $acc->{ $input_entry }->{position} < 5 ) {
        $g->add_node(
          $interactor->{node_name},
          style => 'filled',
          color => $self->{ $acc->{ $input_entry }->{position} },
        );
      }    # end of node_name < 5

    }

  }    # end of foreach interactor

  $c->log->debug('Graph::build_graph: done adding interactors; adding interactions') 
    if $c->debug;

  # next, add interactions to the graph, i.e. edges
  foreach my $interaction_id ( keys %{ $c->stash->{interactions} } ) {

    my $seen_dbs = {};
    my $colours  = '';

    foreach my $db_name (
      @{ $c->stash->{interactions}->{$interaction_id}->{sources} } )
    {

      unless ( exists $seen_dbs->{ $interaction_id . $db_name } ) {
        $colours .= $self->{servers_hash}->{$db_name}->{colour} . ':';
        $seen_dbs->{ $interaction_id . $db_name }++;
      }
    }
    chop $colours;

    $g->add_edge(
      $c->stash->{interactions}->{$interaction_id}->{from},
      $c->stash->{interactions}->{$interaction_id}->{to},
      dir   => 'none',
      style => 'solid',
      color => $colours
    );
  }

  $c->log->debug('Graph::build_graph: done adding interactions') 
    if $c->debug;

  # generate the image and the image map
  eval {
    $c->stash->{image} = $g->as_png;
  }; 
  if ( $@ ) {
    $c->log->debug( "Graph::build_graph: error while generating image: $@" ) if $c->debug;
  }
  eval {
    $c->stash->{map}   = $g->as_cmapx;
  }; 
  if ( $@ ) {
    $c->log->debug( "Graph::build_graph: error while generating map: $@" ) if $c->debug;
  }

  # try to cache them, so that we don't have to generate everything again when
  # we receive the request for the image
  if (  defined $c->stash->{image}
    and defined $c->stash->{map} and defined $c->stash->{ ligSource } )
  {
    $c->cache->set( $image_cache_key, $c->stash->{image} );
    $c->log->debug(
      "Graph::build_graph: cached image under key |$image_cache_key|") 
        if $c->debug;
    $c->cache->set( $map_cache_key, $c->stash->{map} );
    $c->log->debug(
      "Graph::build_graph: cached map under key   |$map_cache_key|") 
        if $c->debug;
    $c->cache->set( $ligSource_cache_key, $c->stash->{ ligSource } );
    $c->log->debug(
      "Graph::build_graph: cached the ligSource under the key |$ligSource_cache_key|\n") 
        if $c->debug;   
  }
  else {
    $c->log->warn('Graph::build_graph: failed to generate image and/or map');
  }

}

#-------------------------------------------------------------------------------

=head2 get_interaction_data : Private 

Retrieves XML DOM that describes the interactions of the specified family.

=cut

sub get_interaction_data : Private {
  my ( $self, $c ) = @_;

  # forward to get_xml to get raw xml.
  $c->forward('get_xml');

  # generate an xml parser,
  $self->{xml_parser} = XML::LibXML->new unless $self->{xml_parser};

  my ( %label, %ligSource );
  
  foreach my $xml ( keys %{ $c->stash->{xml} } ) {

    # key has got format $entry~$db
    my ( $entry, $db_name ) = split( '~', $xml );
    
    $c->log->debug( "Graph:get_interaction_data: dump of das response from $db_name ".dump( $c->stash->{ $xml } ) ) 
      if $c->debug;
    my $dom;
    eval {
      $dom = $self->{xml_parser}->parse_string( $c->stash->{xml}->{$xml} );
    };

    if ($@) {
      $c->log->warn("Graph::get_interaction_data: error when parsing XML: |$@|") 
        if $c->debug;
      $c->stash->{errorMsg} =
          'We received a bad response from the '
        . $self->{servers_hash}->{$db_name}->{label}
        . ' DAS server.';
      return;
    }

    unless ( defined $dom ) {
      $c->log->warn( q(Graph::get_interaction_data: couldn't parse XML from )
          . "database |$db_name|: $!" ) 
            if $c->debug;
      $c->stash->{errorMsg} =
          'We did not receive a response from the '
        . $self->{servers_hash}->{$db_name}->{label}
        . ' DAS server.';
      next;
    }

    my $root = $dom->documentElement;

    my @interactors  = $root->getChildrenByTagName('INTERACTOR');
    my @interactions = $root->getChildrenByTagName('INTERACTION');

#    $c->log->debug( "Graph:get_interaction_data: dump of the interactors array is ".dump( \@interactors ) );
#    if( scalar @interactors < 2 ){
#      $c->log->debug( "Graph::get_interaction_data: Less than 2 interactors for the entry $entry ");
#      next;
#    }

    my ( %interactors );
    my $acc = $c->stash->{acc};

    # store interactors
    foreach my $interactor (@interactors) {

      # hash the interactors so that we can nicely get their accession/ID later
      my $intId;
      if ( $interactor->getAttribute('intId') ) {
        $intId = $interactor->getAttribute('intId');
      }
      elsif ( $interactor->getAttribute('id') ) {
        $intId = $interactor->getAttribute('id');
      }
      else {
        $intId = 'unknown';
      }

      $interactors{$intId} = $interactor;
      $label{ $interactor->getAttribute('dbAccessionId') } = $interactor->getAttribute('shortLabel');

# creating a variable called source because pfam accessions should not be considered as from lig;
      my $source;
      if ( $db_name eq 'lig' ) {
        $source = ( $interactor->getAttribute('dbAccessionId') =~ /^PF\d{5}/ ) ? 'ipfam': $db_name;
      }
      else {
        $source = $db_name;
      }

      $ligSource{ $interactor->getAttribute('shortLabel') } = $source;

      my $node = {
        node_name => $interactor->getAttribute('dbAccessionId'),
        label     => $interactor->getAttribute('shortLabel'),
        source    => $source,
        url       => $c->uri_for(
          '/graph', { acc => $interactor->getAttribute('dbAccessionId') }
        )
      };

      # store all the interactors which are less than certain depth.
      my $nodekey = $acc->{$entry}->{pfam} . $node->{node_name};

# remove all the nodes when we go to depth more than one, however if the node is already selected
# then retain it.

      if ( $acc->{$entry}->{position} > 1 ) {

        if ( exists $acc->{ $node->{label} } ) {

#          $c->log->debug( "Graph:get_interaction_data: The nodekey which has greater than 1 is $nodekey");
#          $c->log->debug( "Graph:get_interaction_data: The node which is to be added is ".$node->{ label } );
          $c->stash->{interactors}->{$nodekey} = $node;
        }

      }
      elsif ( $acc->{$entry}->{position} <= 1 ) {
        $c->stash->{interactors}->{$nodekey} = $node;
      }
    }    # end of foreach interctors

# stash the label and ligSource which is used later and in javascript respectively;
    $c->stash->{label}   = \%label;
    $c->stash->{ligSource} = \%ligSource;

    $c->log->debug( "the ligSource is " . dump( $c->stash->{ligSource} ) . "\n\n" ) 
      if $c->debug;
    $c->log->debug( "The label is" . dump( \%label ) ) 
      if $c->debug;

    # store the interactions
    foreach my $interaction (@interactions) {
      my @participants = $interaction->getChildrenByTagName('PARTICIPANT');

      my ($from_id);

      if ( $participants[0]->getAttribute('intId') ) {
        $from_id = $participants[0]->getAttribute('intId');
      }
      elsif ( $participants[0]->getAttribute('id') ) {
        $from_id = $participants[0]->getAttribute('id');
      }
      else {
        $from_id = 'unknown';
      }

      my ($to_id);
      if ( $participants[1]->getAttribute('intId') ) {
        $to_id = $participants[1]->getAttribute('intId');
      }
      elsif ( $participants[1]->getAttribute('id') ) {
        $to_id = $participants[1]->getAttribute('id');
      }
      else {
        $to_id = 'unknown';
      }

      $c->log->debug( "Graph::get_interaction_data:for|$db_name|from_id|$from_id|to_id|$to_id|") 
        if $c->debug;

      my ( $key, $key1, $to_dbid, $from_dbid );

      unless ( $from_id eq 'unknown' ) {
        $key       = $interactors{$from_id}->getAttribute('dbAccessionId');
        $from_dbid = $interactors{$from_id}->getAttribute('dbAccessionId');
      }
      unless ( $to_id eq 'unknown' ) {
        $key .= $interactors{$to_id}->getAttribute('dbAccessionId');
        $to_dbid = $interactors{$to_id}->getAttribute('dbAccessionId');
      }

      # to avoid multiple edges between the two interactors, check for
      # presence of that key in the stashed interactions

      $key1 = $to_dbid . $from_dbid;

      if ( exists $c->stash->{interactions}->{$key} ) {
        push @{ $c->stash->{interactions}->{$key}->{sources} }, $db_name;
      }
      else {

        # add the next interaction to the stash as it already exists.
        next if ( exists $c->stash->{interactions}->{$key1} );

        my $edge;
        if ( $from_id eq 'unknown' ) {
          $edge->{from} = $from_id;
        }
        else {
          $edge->{from} = $interactors{$from_id}->getAttribute('dbAccessionId');
        }

        if ( $to_id eq 'unknown' ) {
          $edge->{to} = $to_id;
        }
        else {
          $edge->{to} = $interactors{$to_id}->getAttribute('dbAccessionId');
        }

        $edge->{sources} = [$db_name];

# here if the path is greater than one leave all the nodes which are linked with the entry
# however, add the nodes which already exists.

        if ( $c->stash->{acc}->{$entry}->{position} > 1 ) {
          $c->log->debug( "Graph:get_interaction_data: edge added between $from_dbid to $to_dbid for key $key ") 
            if $c->debug;
          $c->stash->{interactions}->{$key} = $edge
            if (( exists $acc->{ $label{$to_dbid} } )
            and ( exists $acc->{ $label{$from_dbid} } ) );
        }
        else {
          $c->stash->{interactions}->{$key} = $edge;
        }

      }
    }    # end of foreach interactions

  }    # end of foreach keys xml
  #$c->log->debug( "the dump of interactors are ". dump( $c->stash->{interactors} ) . "\n\n". dump( $c->stash->{interactions} ) );  
}

#-------------------------------------------------------------------------------

=head2 get_xml : Private 

Gets the raw xml from the das servers and caches it for repeated use.

=cut

sub get_xml : Private {
  my ( $self, $c ) = @_;

  $c->log->debug("Graph::get_xml: getting raw xml from the das sources") 
    if $c->debug;
  my $xml_hash = {};

  foreach my $entry ( @{ $c->stash->{entries} } ) {

    #$c->log->debug( " The entry now is".$entry );

    # now try to get the xml data for specific das soruce from cache.
    foreach my $db ( @{ $c->stash->{db_list} } ) {

      my $xml_cache_key = "xml_" . $entry . '_' . $db;

      # try to get the xml from cache
      $c->stash->{xml}->{ $entry . '~' . $db } = $c->cache->get($xml_cache_key);

      unless ( defined $c->stash->{xml}->{ $entry . '~' . $db } ) {

        my $url = $self->{servers_hash}->{$db}->{uri};
        unless ( defined $url ) {
          $c->log->debug(
            "Graph::get_xml: couldn't find URL for database |$db|") 
              if $c->debug;
          next;
        }

        # append the Pfam accession
        $url .= '?interactor=' . $c->stash->{acc}->{$entry}->{pfam};

        $c->log->debug("Graph::get_xml: retrieving XML from URL: |$url|") 
          if $c->debug;

        # get a UserAgent
        my $ua = LWP::UserAgent->new;

        # we need to set up the proxy for Sanger, otherwise we'll just sit here
        # spinning our wheels for 60 seconds before timing out
        $ua->proxy( ['http'], $self->{proxy} ) if defined $self->{proxy};

        # set a sensible timeout
        $ua->timeout( $self->{timeout} || 10 );

        # submit the request...
        my $req = HTTP::Request->new( GET => $url );
        my $res = $ua->request($req);

        # see if the request got us anything
        unless ( $res->is_success ) {
          $c->log->warn(
            'Graph::get_xml: error when retrieving XML: ' . $res->status_line );
          next;
        }

        unless ( $c->stash->{xml}->{ $entry . '~' . $db } =
          $res->decoded_content )
        {
          $c->log->warn('Graph::get_xml: failed to retrieve XML');
          next;
        }

        $c->cache->set( $xml_cache_key,
          $c->stash->{xml}->{ $entry . '~' . $db } );
      }

    }    # end of foreach db_list;
  }    # end of foreach entries

}

#-------------------------------------------------------------------------------

=head2 image : Local

Generates the image showing interactions for the specified domain. Stuffs a
raw PNG image into the response.

=cut

sub image : Local {
  my ( $this, $c ) = @_;

  # don't return an error message here, since this action returns an image
  #  return unless $c->stash->{entry};

  $c->forward('get_graph');
  unless ( defined $c->stash->{image} ) {
    $c->log->debug('Graph::draw: no image found') 
      if $c->debug;
    return;
  }

  # dump the image straight into the response and set the appropriate
  # content type
  $c->res->content_type('image/png');
  $c->res->body( $c->stash->{image} );

  # make sure that the image isn't cached...
  # theoritically the browser shouldn't cache the image, however it do so,
  # javascript handles this job ( graph.tt )
  $c->res->header( 'Pragma'  => 'no-cache' );
  $c->res->header( 'Expires' => 'Thu, 01 Jan 1970 00:00:00 GMT' );
  $c->res->header( 'Cache-Control' => 'no-store, no-cache, must-revalidate,'
      . 'post-check=0, pre-check=0, max-age=0' );
}

#-------------------------------------------------------------------------------

=head2 validate_interaction : Private

Checks the stashed interaction data and removes the nodes which are not connected.

=cut

sub validate_interaction : Private {

  my ( $self, $c ) = @_;

  unless ( defined $c->stash->{interactors}
    and defined $c->stash->{interactions} )
  {
    $c->log->debug( "Graph:validate_interaction: Interactors and Interactions are missing in the stash ") 
      if $c->debug;
    $c->stash->{errorMsg} ||= 'We found no interactions for this entry ';
    $c->log->debug( "graph:validate_interaction: returning in validate_interaction ") 
      if $c->debug;
    return;
  }

  #my @entries = @{ $c->stash->{ entries } };
  my $acc = $c->stash->{acc};

  my $remove_position;

OUTER: for ( my $i = $#{ $c->stash->{entries} } ; $i > 0 ; $i-- ) {

    my $j = $i - 1;

    my $key1 =
        $acc->{ $c->stash->{entries}->[$i] }->{pfam}
      . $acc->{ $c->stash->{entries}->[$j] }->{pfam};
    my $key2 =
        $acc->{ $c->stash->{entries}->[$j] }->{pfam}
      . $acc->{ $c->stash->{entries}->[$i] }->{pfam};

    $c->log->debug( "$i = "
        . $c->stash->{entries}->[$i] . " $j= "
        . $c->stash->{entries}->[$j]
        . "|$key1|$key2|\n" ) 
          if $c->debug;

    # if there is no interaction between the entries then mark the position;
    unless ( exists $c->stash->{interactions}->{$key1}
      or exists $c->stash->{interactions}->{$key2} )
    {

      $c->log->debug(
        "graph:validate_interaction: does it comes here ",
        dump( $acc->{ $c->stash->{entries}->[$i] } )
      ) 
        if $c->debug;
      $remove_position = $acc->{ $c->stash->{entries}->[$i] }->{position};
      $c->stash->{remove_position} = $remove_position;
      $c->log->debug( "Graph::validate_interaction: Entries which are lesser than position $remove_position are removed ") 
        if $c->debug;
      last OUTER;
    }

  }

  # return if remove_position is not defined;
  unless ( defined $remove_position ) {

    $c->log->debug( "Graph::validate_interaction: All input entries are connected hence skipping validation") 
      if $c->debug;
    return;

  }

  my $label = $c->stash->{label};

  # now remove those interactions of the accession which is not connected;

#  $c->log->debug( "graph:validate_interaction: dump of label is ".dump( $c->stash->{ interactors } ) );
  foreach ( keys %{ $c->stash->{interactions} } ) {
    my ( $from, $to ) = $_ =~ /^(PF\d{5})(\w+)$/;

    $c->log->debug( "Graph:validate_interaction: $_ The |$from|"
        . $label->{$from} . '|'
        . $to . '|'
        . $label->{$to} ) 
          if $c->debug;

    if ( exists $acc->{ $label->{$from} } and exists $acc->{ $label->{$to} } ) {
      $c->log->debug(
            "Graph:validate_interaction: both from and to exists in acc: "
          . $label->{$from} . '|'
          . $label->{$to} ) 
            if $c->debug;
      if (  ( $acc->{ $label->{$to} }->{position} > $remove_position )
        and ( $acc->{ $label->{$from} }->{position} > $remove_position ) )
      {
        $c->log->debug("graph:deleting the $label->{ $from }|$label->{ $to }") 
          if $c->debug;
        delete $c->stash->{interactions}->{$_};
      }
    }
    elsif ( exists $acc->{ $label->{$from} }
      and not exists $acc->{ $label->{$to} } )
    {

#      $c->log->debug( "Graph::validate_interaction: input entry is $from is equal to ".$label->{ $from } );
      if ( $acc->{ $label->{$from} }->{position} > $remove_position ) {

#        $c->log->debug( "Graph::validate_interaction: position of ".$label->{ $from }." |".$acc->{ $label->{ $from } }->{ position } );
#        $c->log->debug( "Graph::validate_interaction: so deleting $_");
        delete $c->stash->{interactions}->{$_};
      }

    }
    elsif ( exists $acc->{ $label->{$to} }
      and not exists $acc->{ $label->{$from} } )
    {

#      $c->log->debug( "Graph::validate_interaction: input entry is $to is equal to ".$label->{ $to } );
      if ( $acc->{ $label->{$to} }->{position} > $remove_position ) {

#        $c->log->debug( "Graph::validate_interaction: position of ".$label->{ $to }." |".$acc->{ $label->{ $to } }->{ position } );
#        $c->log->debug( "Graph::validate_interaction: so deleting $_");
        delete $c->stash->{interactions}->{$_};
      }

    }

  }    # end of foreach interactions

  # now remove the interactors with the accession which is to be removed.
  foreach ( keys %{ $c->stash->{interactors} } ) {
    my ( $from, $to ) = $_ =~ /^(PF\d{5})(\w+)$/;

    $c->log->debug( "Graph:validate_interaction: $_ The |$from|"
        . $label->{$from} . '|'
        . $to . '|'
        . $label->{$to} ) 
          if $c->debug;
    if ( exists $acc->{ $label->{$from} } and exists $acc->{ $label->{$to} } ) {

      $c->log->debug(
            "Graph:validate_interaction: both from and to exists in acc: "
          . $label->{$from} . '|'
          . $label->{$to} ) 
            if $c->debug;
      if (  ( $acc->{ $label->{$to} }->{position} > $remove_position )
        and ( $acc->{ $label->{$from} }->{position} > $remove_position ) )
      {
        $c->log->debug("graph:deleting the $label->{ $from }|$label->{ $to }") 
          if $c->debug;
        delete $c->stash->{interactors}->{$_};
      }
    }
    elsif ( exists $acc->{ $label->{$from} }
      and not exists $acc->{ $label->{$to} } )
    {

#      $c->log->debug( "Graph::validate_interaction: input entry is $from is equal to ".$label->{ $from } );
      if ( $acc->{ $label->{$from} }->{position} > $remove_position ) {

#        $c->log->debug( "Graph::validate_interaction: position of ".$label->{ $from }." |".$acc->{ $label->{ $from } }->{ position } );
#        $c->log->debug( "Graph::validate_interaction: so deleting $_");
        delete $c->stash->{interactors}->{$_};
      }

    }
    elsif ( exists $acc->{ $label->{$to} }
      and not exists $acc->{ $label->{$from} } )
    {

#      $c->log->debug( "Graph::validate_interaction: input entry is $to is equal to ".$label->{ $to } );
      if ( $acc->{ $label->{$to} }->{position} > $remove_position ) {

#        $c->log->debug( "Graph::validate_interaction: position of ".$label->{ $to }." |".$acc->{ $label->{ $to } }->{ position } );
#        $c->log->debug( "Graph::validate_interaction: so deleting $_");
        delete $c->stash->{interactors}->{$_};
      }

    }

  }    # end of foreach interactors

}

#-------------------------------------------------------------------------------

=head2 end : ActionClass

Use L<RenderView> to render templates automatically.

=cut

sub end : ActionClass('RenderView') {
  my ( $self, $c ) = @_;
  if ( defined $c->stash->{errorMsg} ) {
    $c->log->debug( 'Graph::end: found an error message in the stash: |'
        . $c->stash->{errorMsg} . '|' ) 
      if $c->debug;
    $c->res->body( $c->stash->{errorMsg} );
    $c->res->status(400);
  }

}

#-------------------------------------------------------------------------------

1;

