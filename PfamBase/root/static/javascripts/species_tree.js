
var SpeciesTree = Class.create( {

  // set parameters that are used by the tree controls 
  
  // the URL for storing the list of selected IDs 
  var selectStoreURI     = "[% stUri %]/store_ids";

  // the URL to visit when the user selects nodes and wants to view the
  // selected sequences as domain graphics
  var selectGraphicsURI  = "[% stUri %]/graphics";

  // the URL to visit when the user selects nodes and wants to view the 
  // selected sequences as an alignment
  var selectAlignmentURI = "[% c.uri_for( '/family/alignment/builder' ) %]";

  // the URL to visit when the user selects nodes and wants to download the 
  // selected sequence accessions
  var selectAccessionsURI = "[% stUri %]/accessions";

  // the URL to visit when the user selects nodes and wants to download the 
  // selected sequences in FASTA format
  var selectFastaURI = "[% stUri %]/sequences";

  // a hash that maps between the ID of the summary node and the TaskNode object
  var nodeMapping = {};

  // a hash that maps between the ID of the summary node and string of sequence 
  // accessions
  var nodeSequences = {};

  // a flag showing whether the tree was build and has nodes
  var treeBuiltSuccessfully;

  // the function that loads the tree and renders it
  var tree;
  var stSuccess = function( oResponse ) {

    // build the tree widget and get a handle on the root, which we'll need
    // when eval'ing the javascript from the server
    tree = new YAHOO.widget.TreeView("treeDiv");
    var root = tree.getRoot();

    // eval the JS that the server generates. This is the set of calls that
    // build the TreeView widget object tree
    try {
      eval( oResponse.responseText );
    } catch( e ) {
      // don't care
    }

    tree.subscribe( "clickEvent", tree.onEventToggleHighlight );
    tree.setNodesProperty( "propagateHighlightUp",   true );
    tree.setNodesProperty( "propagateHighlightDown", true );

    // by this point the tree was successfully built, but the response might
    // have contained a message rather than tree components. If there was a
    // a tree, we must have more than just the root node
    if ( treeBuiltSuccessfully ) {

      // we got a tree; render it
      tree.draw();

      // bring back the control panel
      $("treeTools").show();
    
    } else {

      // if this flag isn't even set, then the AJAX response didn't contain
      // the javascript to set it, so we assume instead that there's an 
      // error message in there
      if ( treeBuiltSuccessfully == undefined ) {
        $("treeDiv").update( oResponse.responseText );
      } else {
        $("treeDiv").update( "There was a problem building the species tree." );
      }

      // hide the control panel too
      $("treeTools").hide();
    }
  };

  var stFailure = function() {
    $("treeDiv").update( "Tree loading failed." );
  }

  // this is an extra method to submit a new ajax request, this time with
  // the "loadTree" flag set, which tells the controller to load the tree
  // even it's large
  var forceLoad = function() {
  
    // show the new spinner and disable the button
    $("secondaryLoadingSpinner").show();
    $("generateButton").disable();

    // override the test for IE and the "loadTree" check, to force tree loading
    var r = new Ajax.Request(
      "[% stUri %]/interactive",
      {
        method:     'get',
        parameters: { acc:      "[% acc %]",
                      ie:       false,
                      loadTree: 1 },
        onSuccess:  stSuccess,
        onFailure:  stFailure
      }
    );
  };

  // the parameters for the initial ajax call to build the tree
  var stUri;
  if( Prototype.Browser.IE && ielt8 ) {
    stUri = "[% stUri %]/text";
  } else {
    stUri = "[% stUri %]/interactive";
  }

  // fire off the request to load the tree
  document.observe( "dom:loaded", function() {
    var r = new Ajax.Request(
      stUri,
      {
        method:     'get',
        parameters: { acc: "[% acc %]",
                      ie:  Prototype.Browser.IE },
        onSuccess:  stSuccess,
        onFailure:  stFailure
      }
    );
  } );


  
  //------------------------------------------------------------
  // collect the sequences that are specified by the checked leaf nodes
  // in the species trees. Submits the form in the page which will act on those
  // accessions. The argument should be either "G" or "A", for graphical or
  // sequence alignment view of the collected sequences.
  
  function collectSequences( sStyle, sAcc ) {
  
    // retrieve all checked nodes in the tree
    var checkedNodes = tree.getNodesByProperty( "highlightState", 1 );  
    
    // make sure we have at least one checked node
    if( ! checkedNodes.size() ) {
      $("stError")
        .update( "Please select some nodes" )
        .show();
      return;
    }
  
    var seqAccs = checkedNodes.inject( "", function( accumulator, n ) {
      if ( typeof n.data == "string" &&
           nodeSequences[n.data] &&
           nodeSequences[n.data] !== "undefined" ) {
        return accumulator + nodeSequences[n.data] + " ";
      } else { 
        return accumulator;
      }
    } );
  
    // TODO we could optimise this a bit, by storing the list of selected 
    // accessions at this point and then checking the new list against the old
    // list before making another AJAX request to store a second, identical list
    // in the DB. 
  
    // store the IDs and get back a "job id"
    var jobId;
    var r = new Ajax.Request( selectStoreURI, {
      method: 'post',
      parameters: { ids: escape( seqAccs ) },
      onSuccess: function( oResponse ) {
  
        // the response should contain only the "job ID", which points to the list
        // of accessions
        jobId = oResponse.responseText;
  
        // build the URI for the next request, the one that actually does the 
        // work here
        var url;
        var popup = true;
  
        // view the selected sequences as...
        switch( sStyle ) {
          case 'G':
            url = selectGraphicsURI;   // domain graphics
            break;
          case 'L':
            url = selectAccessionsURI; // sequence accessions
            popup = false;
            break;
          case 'F':
            url = selectFastaURI;      // FASTA format
            popup = false;
            break;
          default:
            url = selectAlignmentURI;  // an alignment
        }
  
        // tack on the parameters that we need
        url +=   "?acc="   + sAcc
               + "&jobId=" + jobId;
          
        // load that URL, either in a popup or in the main window
        if( popup ) {
          popUp( url, 'console', 800, 800, 'selectedSeqsWin' );
        } else {
          window.location = url;
        }
        
      },
      onFailure: function( oResponse ) {
        $("stError")
          .update( "There was a problem collecting sequence accessions" )
          .show();
        return;
      }
    });
  
  }


} );

