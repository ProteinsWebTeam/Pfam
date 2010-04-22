
// AlignmentViewer.js
//
// Javascript library which gets alignments and renders it in the browser.
//
// $Id$

//------------------------------------------------------------------------------
//- OBJECT ---------------------------------------------------------------------
//------------------------------------------------------------------------------

// spoof a console if necessitates for IE.
if( ! window.console ){
  window.console = {};
  window.console.log = function(){};
}

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

var AlignmentViewer = Class.create({
  
  initialize: function( parent, accession, dsn, size, url, options ){
    
    console.log( 'initialize: begins');
    
    // check for an existing DOM element;
    if( parent !== undefined ){
      this.setParent( parent );
    }else{
      this._throw( 'parent cannot be null' );
    }
    
    // check for accession;
    if( accession !== undefined ){
      this.setAccession( accession );  
    }else{
      this._throw( 'Accession cannot be null' );
    }
     
    // check for dsn; 
    if( dsn !== undefined ){
      this.setDsn( dsn );
    }else{
      this._throw( 'dsn cannot be null' );
    }     
    
    // check for size; 
    if( size !== undefined ){
      this.setSize( size );
    }else{
      this._throw( 'size cannot be null' );
    }     
    
    // add the url
    if( /[\£\$\*\^]+/.test( url ) ){
      this._throw( "Input URL contains invalid characters");
    }else{
      this._url = url;
    } 
    
    // now define the options;
    this.options = {};
    Object.extend( this.options, options || {} );
    
    // now create the child elements for generating grid;
    this.createChildren();
    console.log('initialise: children created' );
    
    // now use livegrid to get the alignments;
    this.fetchAlignments();
    
    // add event listeners after the alignment is populated;
    this._addListeners();
    
  },  // end of initialize
  
  //------------------------------------------------------------------------------
  //- Methods --------------------------------------------------------------------
  //------------------------------------------------------------------------------
  
  // function to initialise the livegrid object and gets the alignment;
  fetchAlignments: function( ){
    
    var parent = this._parent;
    console.log('fetchAlignments: begins' );
    
    // create the options required for making the request;
    var opts = {
      requestParameters:{
        accession: this._accession,
        dsn_name:  this._dsn,
        max_rows:  this._size
      },
      accessionsDiv: this._accDiv,
      sequencesDiv: this._seqDiv,
			scrollerDiv:  this._scroll,
      scrollvalue: 0,
      prefetchBuffer: true,
//      onLoading: function(){
//        var loadDiv = new Element( 'div',{ 'id': 'spinner' } );
//        loadDiv.update( 'Loading Alignment...' );
//        parent.appendChild( loadDiv );
//      },
//      onComplete: function(){
//        parent.removeChild( $('spinner' ) );
//      }
    }; // end of options;
    
    console.log('fetchAlignments: making a livegrid request with |%s|%d|%s|%s|',this._gridDiv, this._size, this._url, opts.toString()  );
    // now make an request using livegrid;
    this._livegrid = new LiveGrid( this._gridDiv, 25, this._size, this._url, opts );
    
  },
  
  //----------------------------------------------------------------------------
  
  // function to create the children;
  createChildren: function( ){
    
    // with the current version of livegrid, I need the following structure
    //    <div id="grid">
    //      <div id="accessions"></div>
    //      <div id="sequences"></div>
    //    </div>
    //    <div id="scrollerDiv"></div>
    var grid   = new Element( 'div',{ 'id': 'grid' } );
    var scroll = new Element( 'div',{ 'id': 'scrollerDiv' } );
    var acc    = new Element( 'div',{ 'id': 'accessions' } );
    var seq    = new Element( 'div',{ 'id': 'sequences' } );
    
    // add the grid and the scroller to the DOM as children of parent;
    this._parent.appendChild( grid );
    this._parent.appendChild( scroll );
    
    // add the acc and seq as children of grid;
    grid.appendChild( acc );
    grid.appendChild( seq );
    
    // now add the grid and other elements to the object;
    this._gridDiv = grid;
    this._accDiv  = acc;
    this._seqDiv  = seq;
    this._scroll  = scroll;
  },
  
  //----------------------------------------------------------------------------
  
  // function which adds event listeners
  _addListeners: function(){
    
    this._seqDiv.observe('scroll', function(){
      this._livegrid.options.scrollvalue = $('sequences').scrollLeft;
    }.bind( this ) );
    
  },    
      
  //------------------------------------------------------------------------------
  //- Get and Set Methods --------------------------------------------------------
  //------------------------------------------------------------------------------
  
  // get the livegrid object for later use;
  getLiveGrid: function(){
    return this._livegrid;  
  },
  
  //--------------------------------------
  
  // function to set parent;
  setParent: function( parent ){
    this._parent = $( parent );
    console.log('the parent is '+this._parent.inspect() );
    
    if ( this._parent === undefined || this._parent === null ) {
      this._throw( "couldn't find the node"+parent );
    }
      
  },
  
  //--------------------------------------
  
  // function to get the parent;
  getParent: function(){
    return this._parent;
  },
  
  //----------------------------------------------------------------------------
  
  // function to set the accession;
  setAccession: function( accession ){
    this._accession = accession;
    
    //check whether we get the accession as a string;
    if( this._accession === null ){
      this._throw( 'Accession cannot be null' );
    }
    
    // use regex to check we get any invalid characters in the string;
    if( /\W+/.test( this._accession ) ){
      this._throw( 'Accession contains invalid characters');
    }
    
    
  }, // end of setAccession
  
  //-------------------------------------
  
  // function to return the accession 
  getAccession: function(){
    return this._accession;
  },
  
  //----------------------------------------------------------------------------
  
  // function to set the dsn;
  setDsn: function( dsn ){
    this._dsn = dsn;
    
    if( this._dsn === null ){
      this._throw( 'dsn cannot be null'); 
    }  
  },
  
  //-------------------------------------
  
  // function to get the dsn;
  getDsn: function(){
    return this._dsn;
  },
  
  //----------------------------------------------------------------------------
  
  // function to set the size;
  setSize: function( size ){
    this._size = size;
    
    if( this._size === null ){
      this._throw( 'input size cannot be null');
    }  
    
  },
  
  //-------------------------------------
  
  // function to get the size;
  getSize: function(){
    return this._size;
  },
  
  //------------------------------------------------------------------------------
  //- Private Methods ------------------------------------------------------------
  //------------------------------------------------------------------------------
  _throw: function( msg ){
    
    throw { name : AlignmentViewerException,
            message: msg,
            toString: function(){ return this.message ;}
          };
    
  } // end of _throw
   
} );  // end of Class.create

