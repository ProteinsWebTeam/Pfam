

//------------------------------------------------------------------------------
//- LiveGridMetaData -----------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridMetaData = Class.create( {

  initialize: function( pageSize, totalRows, options ) {
    console.log( "LiveGridMetaData.initialize: start" );

    this.pageSize  = pageSize;
    this.totalRows = totalRows;

    this.options = {
      largeBufferSize: 50.0, // 7 pages
      smallBufferSize: 1.0, // 1 page
      nearLimitFactor: 0.2  // 20% of buffer
    }
    Object.extend( this.options, options || {} );

    console.log( "LiveGridMetaData.initialize: end" );
  },

  //----------------------------------------------------------------------------

  getPageSize: function() {
    return this.pageSize;
  },

  //----------------------------------------------------------------------------

  getTotalRows: function() {
    return this.totalRows;
  },

  //----------------------------------------------------------------------------

  setTotalRows: function(n) {
    this.totalRows = n;
  },

  //----------------------------------------------------------------------------

  getLargeBufferSize: function() {
    return parseInt( this.options.largeBufferSize * this.pageSize );
  },

  //----------------------------------------------------------------------------

  getSmallBufferSize: function() {
    return parseInt( this.options.smallBufferSize * this.pageSize );
  },

  //----------------------------------------------------------------------------

  getLimitTolerance: function() {
    return parseInt( this.getLargeBufferSize() * this.options.nearLimitFactor );
  },

  //----------------------------------------------------------------------------

  getBufferSize: function( isFull ) {
    return isFull ? this.getLargeBufferSize() : this.getSmallBufferSize();
  }

} );


//------------------------------------------------------------------------------
//- LiveGridScroller -----------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridScroller = Class.create( {

  initialize: function( liveGrid ) {
    console.log( "LiveGridScroller.initialize: start" );

    this.liveGrid = liveGrid;
    this.metaData = liveGrid.metaData;
    
    this._createScrollBar();
    
    this.scrollTimeout = null;
    this.lastScrollPos = 0;

    console.log( "LiveGridScroller.initialize: end" );
  },

  //----------------------------------------------------------------------------

  isUnplugged: function() {
    return this.scrollerDiv.onscroll == null;
  },

  //----------------------------------------------------------------------------

  plugin: function() {
    this.scrollerDiv.onscroll = this.handleScroll.bindAsEventListener(this);
  },

  //----------------------------------------------------------------------------

  unplug: function() {
    this.scrollerDiv.onscroll = null;
  },

  //----------------------------------------------------------------------------

  _createScrollBar: function() {
    console.log( "LiveGridScroller._createScrollBar: start" );

    var grid          = this.liveGrid.grid;
    var visibleHeight = grid.offsetHeight;
    this.lineHeight   = visibleHeight / this.metaData.getPageSize();
      
    // create the outer div...
    this.scrollerDiv = document.createElement( "div" );
    
    this.scrollerDiv.setStyle( {
      borderRight: "1px solid #ababab",
      position:    "relative",
      /* left:        Prototype.Browser.IE ? "-6px" : "-3px", */
      width:       "16px",
      height:      visibleHeight + "px",
      overflow:    "auto"
    } );

    if ( Prototype.Browser.IE ) {

      grid.onmousewheel = function( evt ) {
        if ( event.wheelDelta >= 0 ) { //wheel-up
          this.scrollerDiv.scrollTop -= this.lineHeight;
        } else {
          this.scrollerDiv.scrollTop += this.lineHeight;
        }
        this.handleScroll(true);
      }.bind(this);

    } else {

      grid.addEventListener( "DOMMouseScroll", function( evt ) {
        if ( evt.detail < 0 ) { //wheel-up
          this.scrollerDiv.scrollTop -= this.lineHeight;
        } else {
          this.scrollerDiv.scrollTop += this.lineHeight;
        }
        this.handleScroll(true);
      }.bind(this),
      true );

    }

    // create the inner div...
    this.heightDiv = document.createElement( "div" );
    this.updateSize();

    this.scrollerDiv.appendChild( this.heightDiv );
    this.scrollerDiv.onscroll = this.handleScroll.bindAsEventListener( this );

    Element.insert( grid.nextSibling, { before: this.scrollerDiv } );

    console.log( "LiveGridScroller._createScrollBar: end" );
  },
   
  //----------------------------------------------------------------------------

  updateSize: function() {
    console.log( "LiveGridScroller.updateSize: start" );

    var visibleHeight = this.liveGrid.grid.offsetHeight;
    var divHeight = parseInt( visibleHeight * this.metaData.getTotalRows() / this.metaData.getPageSize() );
    this.heightDiv.setStyle( {
      height: divHeight + "px",
      width:  "1px"
    } );

    console.log( "LiveGridScroller.updateSize: end" );
  },

  //----------------------------------------------------------------------------

  adjustScrollTop: function() {
    console.log( "LiveGridScroller.adjustScrollTop: start" );

    this.unplug();
    var rem = this.scrollerDiv.scrollTop % this.lineHeight;
    if ( rem != 0 ) {
      if ( this.lastScrollPos < this.scrollerDiv.scrollTop ) {
        this.scrollerDiv.scrollTop = this.scrollerDiv.scrollTop + this.lineHeight -rem;
      } else {
        this.scrollerDiv.scrollTop = this.scrollerDiv.scrollTop - rem;
      }
    }
    this.lastScrollPos = this.scrollerDiv.scrollTop;
    this.plugin();

    console.log( "LiveGridScroller.adjustScrollTop: end" );
  },

  //----------------------------------------------------------------------------

  moveScroll: function(rowOffset) {
    console.log( "LiveGridScroller.moveScroll: start" );

    var pixelOffset = (rowOffset / this.metaData.getTotalRows()) * this.heightDiv.offsetHeight;
    this.scrollerDiv.scrollTop = pixelOffset;

    console.log( "LiveGridScroller.moveScroll: end" );
  },

  //----------------------------------------------------------------------------

  handleScroll: function(skiptimeout) {
    console.log( "LiveGridScroller.handleScroll: start" );

    if ( this.scrollTimeout ) {
       clearTimeout( this.scrollTimeout );
    }
    
    var contentOffset = parseInt( this.scrollerDiv.scrollTop * this.metaData.getTotalRows() / this.heightDiv.offsetHeight );
    if ( this.metaData.options.onscroll ) {
      this.metaData.options.onscroll( contentOffset, this.metaData );
    }

    if ( skiptimeout == true ) {
      this.scrollIdle();
    } else {
      this.scrollTimeout = setTimeout( this.scrollIdle.bind(this), 100 );
    }
    
    console.log( "LiveGridScroller.handleScroll: end" );
  },

  //----------------------------------------------------------------------------

  scrollIdle: function() {
    console.log( "LiveGridScroller.scrollIdle: start" );

    if ( this.scrollTimeout ) {
      clearTimeout( this.scrollTimeout );
    }

    // this.adjustScrollTop();
    var contentOffset = parseInt( this.scrollerDiv.scrollTop * this.metaData.getTotalRows() / this.heightDiv.offsetHeight );
    this.liveGrid.requestContentRefresh(contentOffset);

    if ( this.metaData.options.onscrollidle ) {
      this.metaData.options.onscrollidle();
    }

    console.log( "LiveGridScroller.scrollIdle: end" );
  }

} );


//------------------------------------------------------------------------------
//- LiveGridBuffer -------------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridBuffer = Class.create( {

  initialize: function( metaData ) {
    console.log( "LiveGridBuffer.initialize: start" );

    this.startPos = 0;
    this.size     = 0;
    this.metaData = metaData;
    this.rows     = new Array();
    this.updateInProgress = false;

    console.log( "LiveGridBuffer.initialize: end" );
  },

  //----------------------------------------------------------------------------

  update: function(ajaxResponse,start) {
    console.log( "LiveGridBuffer.update: start" );

    this.startPos = parseInt(start);
    this.rows = ajaxResponse.responseText;
    try { 
      //first try the most reliable way
      this.size = ajaxResponse.responseXML.documentElement
                 ? parseInt(ajaxResponse.responseXML.documentElement.getAttribute('rowcount'))
                 : parseInt(ajaxResponse.responseXML.childNodes[0].getAttribute('rowcount'));
    }
    catch( err ) { }

    //alternative way
    if ( ! this.size ) {
      if ( ajaxResponse.responseXML.childNodes[0].getElementsByTagName ) {
        this.size = ajaxResponse.responseXML.childNodes[0].getElementsByTagName('*').length;
      } else if ( ajaxResponse.responseXML.documentElement ) {
        this.size = ajaxResponse.responseXML.documentElement.childNodes.length;
      } else {
        this.size = ajaxResponse.responseXML.childNodes[0].childNodes.length;
      }
    }

    console.log( "LiveGridBuffer.update: end" );
  },

  //----------------------------------------------------------------------------

  isFullP: function() {
    return this.metaData.pageSize != this.size;
  },

  //----------------------------------------------------------------------------

  isClose: function(start) {
    return ( start < this.startPos + this.size + (this.size/2) ) &&
            ( start + this.size + (this.size/2) > this.startPos )
  },

  //----------------------------------------------------------------------------

  isInRange: function(start, count) {
    return (start < this.startPos + this.size) && (start + count > this.startPos)
  },

  //----------------------------------------------------------------------------

  isFullyInRange: function( position ) {
    console.log( "LiveGridBuffer.isFullyInRange: position:       ", position );
    console.log( "LiveGridBuffer.isFullyInRange: start position: ", this.startPos );

    var retVal = ( position >= this.startPos ) && 
                 ( position + this.metaData.getPageSize() ) <= ( this.startPos + this.size )
    console.log( 
      "LiveGridBuffer.isFullyInRange: return val: ( %s >= %s ) && ( %s + %s ) <= ( %s + %s ) = %s",
      position, this.startPos, position, this.metaData.getPageSize(), this.startPos, this.size, retVal
    );

    return retVal;
  },

  //----------------------------------------------------------------------------

  isNearingTopLimit: function(position) {
    console.log( "LiveGridBuffer.isNearingTopLimit: position:       ", position );
    console.log( "LiveGridBuffer.isNearingTopLimit: start position: ", this.startPos );

    var retVal = ( position - this.startPos ) < this.metaData.getLimitTolerance();

    console.log(
      "LiveGridBuffer.isNearingTopLimit: return val: ( %s - %s ) < %s = %s",
      position, this.startPos, this.metaData.getLimitTolerance(), retVal
    );

    return retVal;
  },

  //----------------------------------------------------------------------------

  isNearingBottomLimit: function(position) {
    console.log( "LiveGridBuffer.isNearingBottomLimit: position:   " + position );

    var myEnd     = position + this.metaData.getPageSize();
    var bufferEnd = this.startPos + this.size;

    console.log( "LiveGridBuffer.isNearingBottomLimit: my end:     " + myEnd );
    console.log( "LiveGridBuffer.isNearingBottomLimit: buffer end: " + bufferEnd );

    var retVal = ( bufferEnd - myEnd ) < this.metaData.getLimitTolerance();
    console.log( 
      "LiveGridBuffer.isNearingBottomLimit: return val: ( %s  - %s ) < %s = %s", 
      bufferEnd, myEnd, this.metaData.getLimitTolerance(), retVal
    );

    return retVal;
  },

  //----------------------------------------------------------------------------

  isAtTop: function() {
    return this.startPos == 0;
  },

  //----------------------------------------------------------------------------

  isAtBottom: function() {
    return this.startPos + this.size == this.metaData.getTotalRows();
  },

  //----------------------------------------------------------------------------

  isNearingLimit: function(position) {
    return ( !this.isAtTop()    && this.isNearingTopLimit(position)) ||
            ( !this.isAtBottom() && this.isNearingBottomLimit(position) )
  },

  //----------------------------------------------------------------------------

  getRows: function(start, count) {
    console.log( "LiveGridBuffer.getRows: start" );

    var begPos = parseInt(start) - this.startPos
    var endPos = begPos + parseInt(count)
    console.log( "LiveGridBuffer.getRows: start - end: %d - %d", begPos, endPos );
    var srch = '(.*?)(<(.*?)rownum="'+begPos+'"((\n|.)*?)';
    // er? need more data...
    if ( endPos >= this.size ) {
      endPos = this.size;
      srch = srch + ')</ajax-response>'
    } else {
      srch = srch + ')<(.*?)rownum="'+(endPos)+'"';
    }

    try {
      return this.rows.match(srch)[2];
    }
    catch ( err ) {
      return '';
    }

    console.log( "LiveGridBuffer.getRows: end" );
  }

} );


//------------------------------------------------------------------------------
//- LiveGridRequest ------------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridRequest = Class.create( {

  initialize: function( requestOffset, options ) {
    console.log( "LiveGridRequest.initialize: start" );

    this.requestOffset = requestOffset;

    console.log( "LiveGridRequest.initialize: end" );
  }

} );


//------------------------------------------------------------------------------
//- LiveGrid -------------------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGrid = Class.create( {

  initialize: function( grid, visibleRows, totalRows, url, options ) {
    console.log( "LiveGrid.initialize: start" );

    if ( options == null ) {
      options = {};
    }
         
    this.options = options;
         
    this.grid        = $(grid);
    this.gridId      = this.grid.identify();
    this.metaData    = new LiveGridMetaData( visibleRows, totalRows, options );
    this.buffer      = new LiveGridBuffer( this.metaData );

    this.lastDisplayedStartPos = -1;
    this.timeoutHandler        = null;
    this.additionalParams      = options.requestParameters || {};

    this.processingRequest  = null;
    this.unprocessedRequest = null;

    this.url = url;
      
    if ( options.prefetchBuffer || options.prefetchOffset ) {
      var offset = 0;
      if ( options.prefetchOffset ) {
        this.scroller.moveScroll(options.prefetchOffset);
        offset = options.prefetchOffset;
      }
      
      this.fetchBuffer( offset, true );
    } else {
      this.scroller = new LiveGridScroller( this );
    }

    console.log( "LiveGrid.initialize: end" );
  },

  //----------------------------------------------------------------------------

  setTotalRows: function( newTotalRows ) {
    this.metaData.setTotalRows(newTotalRows);
    this.scroller.updateSize();
  },

  //----------------------------------------------------------------------------

  largeBufferWindowStart: function( offset ) {
    var val = offset - ( ( 0.5 * this.metaData.getLargeBufferSize() ) -
                         ( 0.5 * this.metaData.getPageSize() ) );
    return Math.max( 0, parseInt( val ) );
  },

  //----------------------------------------------------------------------------

  handleTimedOut: function() {
    console.log( "LiveGrid.handleTimedOut: start" );

    //server did not respond in n seconds... assume that there could have been
    //an error or something, and allow requests to be processed again...
    this.processingRequest = null;
    this.processQueuedRequest();

    console.log( "LiveGrid.handleTimedOut: start" );
  },

  //----------------------------------------------------------------------------

  fetchBuffer: function( offset, fullBufferp ) {
    console.log( "LiveGrid.fetchBuffer: start" );

    if ( this.processingRequest ) {
      console.log( "LiveGrid.fetchBuffer: processing request..." );
      this.unprocessedRequest = new LiveGridRequest(offset);
      return;
    }
   
    var fetchSize = this.metaData.getBufferSize(fullBufferp);
    var bufferStartPos = Math.max( 0, fullBufferp ? this.largeBufferWindowStart(offset) : offset );

    this.processingRequest = new LiveGridRequest(offset);
    this.processingRequest.bufferOffset = bufferStartPos;

    var callParams = { id:        this.gridId,
                       page_size: fetchSize,
                       offset:    bufferStartPos };
    Object.extend( callParams, this.additionalParams );
    console.log( "LiveGrid.fetchBuffer: call parameters: " );
    console.log( callParams );

    // setup the AjaxRequest options

    // first, set the request parameters
    var options = { parameters: callParams,
                    method: 'get' };

    // and then add user-defined options. These should be the callbacks for the various
    // AjaxRequest response states, e.g. onLoading, onComplete, etc.
    Object.extend( options, this.options );

    // add the update method from this class, so that we have a hook for updating the
    // page with the server response
    options.onComplete = this.ajaxUpdate.bind( this ); 

    // send the request
    this.ajaxRequest = new Ajax.Request( this.url, options );

    this.timeoutHandler = setTimeout( this.handleTimedOut.bind(this), 10000 );

    console.log( "LiveGrid.fetchBuffer: end" );
  },

  //----------------------------------------------------------------------------

  requestContentRefresh: function( contentOffset ) {
    console.log( "LiveGrid.requestContentRefresh: start" );

    console.log( "LiveGrid.requestContentRefresh: contentOffset: " + contentOffset );

    if ( this.buffer && this.buffer.isFullyInRange(contentOffset) ) {
      console.log( "LiveGrid.requestContentRefresh: buffer is fully in range; updating content" );
      this.updateContent(contentOffset);
      if ( this.buffer.isNearingLimit(contentOffset) ) {
        console.log( "LiveGrid.requestContentRefresh: buffer is nearing limit; fetching buffer (true)" );
        this.fetchBuffer(contentOffset, true);
      }

    } else if ( this.buffer && this.buffer.isClose(contentOffset) ) {
      console.log( "LiveGrid.requestContentRefresh: buffer is close; fetching buffer (true)" );
      this.fetchBuffer(contentOffset, true);

    } else {
      console.log( "LiveGrid.requestContentRefresh: fall-back; fetching buffer (false)" );
      this.fetchBuffer(contentOffset, false);
    }

    console.log( "LiveGrid.requestContentRefresh: end" );
  },

  //----------------------------------------------------------------------------

  ajaxUpdate: function(ajaxResponse) {
    console.log( "LiveGrid.ajaxUpdate: start" );

    clearTimeout( this.timeoutHandler );
    try {
      var totalrows =
        ajaxResponse.responseXML.documentElement ? 
        ajaxResponse.responseXML.documentElement.getAttribute("totalrows"):
        ajaxResponse.responseXML.childNodes[0].getAttribute("totalrows"); //huh?? MSIE thinks <? xml ?> decleration is a node also :S
      if ( totalrows ) {
        this.setTotalRows(totalrows);
      }
    }
    catch ( err ) { }
    this.buffer = new LiveGridBuffer( this.metaData );
    this.buffer.update( ajaxResponse, this.processingRequest.bufferOffset );
    if ( this.unprocessedRequest == null ) {
      offset = this.processingRequest.requestOffset;
      this.updateContent (offset);
    }
    this.processingRequest = null;

    if ( ! this.scroller ) {
      this.scroller = new LiveGridScroller(this);
      if ( this.options.onFirstContent ) {
        this.options.onFirstContent(this);
      }
    }      
    if ( this.options.onComplete ) {
      this.options.onComplete(this);
    }
    this.processQueuedRequest();

    console.log( "LiveGrid.ajaxUpdate: end" );
  },

  //----------------------------------------------------------------------------

  processQueuedRequest: function() {
    console.log( "LiveGrid.processQueuedRequest: start" );

    if ( this.unprocessedRequest != null) {
      console.log( "LiveGrid.processQueuedRequest: found an unprocessed request..." );
      this.requestContentRefresh(this.unprocessedRequest.requestOffset);
      this.unprocessedRequest = null
    }

    console.log( "LiveGrid.processQueuedRequest: end" );
  },

  //----------------------------------------------------------------------------

  updateContent: function( startPos ) {
    console.log( "LiveGrid.updateContent: start" );

    if ( startPos == this.lastDisplayedStartPos ) { 
      return;
    }
    this.lastDisplayedStartPos = startPos
    this.grid.innerHTML = this.buffer.getRows( startPos, this.metaData.getPageSize() );

    console.log( "LiveGrid.updateContent: end" );
  }

} );


