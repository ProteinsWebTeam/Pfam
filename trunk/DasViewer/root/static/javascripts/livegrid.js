
// livegrid.js
// jt6 20090302 WTSI
//
// based heavily on the implementation at:
//
//   http://www.chrisvandesteeg.nl/2005/07/25/livegrid/
// 
// $Id: livegrid.js,v 1.1 2010-02-15 12:02:43 pg6 Exp $


//------------------------------------------------------------------------------
//- LiveGridMetaData -----------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridMetaData = Class.create( {

  initialize: function( pageSize, totalRows, options ) {
     // console.log( "(2)LiveGridMetaData.initialize: starting with params |%s|%s|%s|",pageSize, totalRows, options );

    this.pageSize  = pageSize;
    this.totalRows = totalRows;

//    this.options = {
//      largeBufferSize: 10.0, // 7 pages
//      smallBufferSize: 1.0, // 1 page
//      nearLimitFactor: 0.2  // 20% of buffer
//    }
    
    this.options = {
      largeBufferSize: 4.0,
      smallBufferSize: 1.0,
      nearLimitFactor: 0.1
    }
    
    Object.extend( this.options, options || {} );

    // console.log( "(3)LiveGridMetaData.initialize: end" );
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
//    return this.getLargeBufferSize();
    // console.log("(10)getBufferSize of LiveGridMetaData is called with "+isFull );
    return isFull ? this.getLargeBufferSize() : this.getSmallBufferSize();
  }

} );


//------------------------------------------------------------------------------
//- LiveGridScroller -----------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridScroller = Class.create( {

  initialize: function( liveGrid ) {
    // console.log( "(17.5.1)LiveGridScroller.initialize: start" );
    
    //// console.log('inside lvegrid scroller the scrollLeft valye is '+liveGrid.options.scrollvalue );
    this.liveGrid = liveGrid;
    this.metaData = liveGrid.metaData;
    
    // console.log( "(17.5.2 )creating the scrollbar " );
    this._createScrollBar();
    
    this.scrollTimeout = null;
    this.lastScrollPos = 0;

    // console.log( "LiveGridScroller.initialize: end" );
  },

  //----------------------------------------------------------------------------

  isUnplugged: function() {
    return this.scrollerDiv.onscroll == null;
  },

  //----------------------------------------------------------------------------

  plugin: function() {
    this.scrollerDiv.onscroll = this.handleScroll.bindAsEventListener( this );
  },

  //----------------------------------------------------------------------------

  unplug: function() {
    this.scrollerDiv.onscroll = null;
  },

  //----------------------------------------------------------------------------

  _createScrollBar: function() {
    // console.log( "( 17.5.2.1 ) LiveGridScroller._createScrollBar: start" );
    
    var grid          = this.liveGrid.grid;
    var visibleHeight = grid.offsetHeight;
    this.lineHeight   = visibleHeight / this.metaData.getPageSize();
      
    // create the outer div...
    this.scrollerDiv = document.createElement( "div" );
    
    this.scrollerDiv.setStyle( {
      borderRight: "1px solid #ababab",
      position:    "relative",
      /* left:        Prototype.Browser.IE ? "-6px" : "-3px", */
      width:       "19px",
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
      
      // console.log( "( 17.5.2.2 ) adding event listener to the scroll bar for no-Ie browsers" );
      grid.addEventListener( "DOMMouseScroll", function( evt ) {
        if ( evt.detail < 0 ) { //wheel-up
          this.scrollerDiv.scrollTop -= this.lineHeight;
        } else {
          this.scrollerDiv.scrollTop += this.lineHeight;
        }
        // console.log( "( 17.5.2.3 )calling the handlescroll function with true value" );
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

    // console.log( "( 17.5.2.4 )LiveGridScroller._createScrollBar: end" );
  },
   
  //----------------------------------------------------------------------------

  updateSize: function() {
    // // console.log( "LiveGridScroller.updateSize: start" );

    var visibleHeight = this.liveGrid.grid.offsetHeight;
    var divHeight = parseInt( visibleHeight * this.metaData.getTotalRows() / this.metaData.getPageSize() );
    this.heightDiv.setStyle( {
      height: divHeight + "px",
      width:  "1px"
    } );

    // // console.log( "LiveGridScroller.updateSize: end" );
  },

  //----------------------------------------------------------------------------

  adjustScrollTop: function() {
    // // console.log( "LiveGridScroller.adjustScrollTop: start" );
  
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

    // // console.log( "LiveGridScroller.adjustScrollTop: end" );
  },

  //----------------------------------------------------------------------------

  moveScroll: function(rowOffset) {
    // // console.log( "LiveGridScroller.moveScroll: start" );

    var pixelOffset = (rowOffset / this.metaData.getTotalRows()) * this.heightDiv.offsetHeight;
    this.scrollerDiv.scrollTop = pixelOffset;

    // // console.log( "LiveGridScroller.moveScroll: end" );
  },

  //----------------------------------------------------------------------------

  handleScroll: function( skiptimeout ) {
    //// console.log( "( 17.5.2.3.1 ) LiveGridScroller.handleScroll:inside handleScroll function with value "+skiptimeout );

    if ( this.scrollTimeout ) {
      //// console.log( "( 17.5.2.3.2 )LiveGridScroller.handleScroll: hit timeout "+this.scrollTimeout );
      clearTimeout( this.scrollTimeout );
    }
    
    var contentOffset = parseInt( this.scrollerDiv.scrollTop * this.metaData.getTotalRows() / this.heightDiv.offsetHeight );
    // console.log( "( 17.5.2.3.3 )LiveGridScroller.handleScroll: contentOffset "+contentOffset );
    
    if ( this.metaData.options.onscroll ) {
      this.metaData.options.onscroll( contentOffset, this.metaData );
    }

    if ( skiptimeout == true ) {
      // console.log( "( 17.5.2.3.4 ) scrollIdle function called");
      this.scrollIdle();
    } else {
      this.scrollTimeout = setTimeout( this.scrollIdle.bind(this), 100 );
    }
    
    // console.log( "( 17.5.2.3.5 ) LiveGridScroller.handleScroll: end" );
  },

  //----------------------------------------------------------------------------

  scrollIdle: function() {
    // console.log( "LiveGridScroller.scrollIdle: start" );

    if ( this.scrollTimeout ) {
      clearTimeout( this.scrollTimeout );
    }

    var contentOffset = 
      parseInt( this.scrollerDiv.scrollTop * this.metaData.getTotalRows() / this.heightDiv.offsetHeight );
    this.liveGrid.requestContentRefresh( contentOffset );

    if ( this.metaData.options.onscrollidle ) {
      this.metaData.options.onscrollidle();
    }

    // console.log( "LiveGridScroller.scrollIdle: end" );
  }

} );


//------------------------------------------------------------------------------
//- LiveGridBuffer -------------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridBuffer = Class.create( {

  initialize: function( metaData ) {
    // console.log( "(4)LiveGridBuffer.initialize: start with metadata"+metaData );

    this.startPos = 0;
    this.size     = 0;
    this.metaData = metaData;
    this.rows     = new Array();
    this.updateInProgress = false;
    
    this.alignments = new Hash();
    // console.log( "(5)LiveGridBuffer.initialize: end" );
  },

  //----------------------------------------------------------------------------

  update: function( ajaxResponse, start ) {
    // console.log( "(17.2.1) LiveGridBuffer.update: start |%s|%s|",ajaxResponse,start );


    this.startPos = parseInt(start);
    this.rows     = ajaxResponse.responseJSON.json.rows;
    this.size     = this.rows.length;
    
    // now I am adding the alignments to the object for retrieving;
    this.alignments = ajaxResponse.responseJSON.storeAlignments;
    //console.log( "the alignments are "+this.alignments );
    // console.log( "(17.2.2) LiveGridBuffer.update: end the size is "+this.size );
/*
    try { 
      //first try the most reliable way
      this.size = ajaxResponse.responseXML.documentElement
                 ? parseInt(ajaxResponse.responseXML.documentElement.getAttribute('rowcount'))
                 : parseInt(ajaxResponse.responseXML.childNodes[0].getAttribute('rowcount'));
    }
    catch( err ) {
      // console.error( "LiveGridBuffer.update: problem with update response: ", err );

      if ( ajaxResponse.responseXML.childNodes[0].getElementsByTagName ) {
        this.size = ajaxResponse.responseXML.childNodes[0].getElementsByTagName('*').length;
      } else if ( ajaxResponse.responseXML.documentElement ) {
        this.size = ajaxResponse.responseXML.documentElement.childNodes.length;
      } else {
        this.size = ajaxResponse.responseXML.childNodes[0].childNodes.length;
      }

    }
 */

    // // console.log( "LiveGridBuffer.update: end" );
  },

  //----------------------------------------------------------------------------

  isFullP: function() {
    return this.metaData.pageSize != this.size;
  },

  //----------------------------------------------------------------------------

  isClose: function( start ) {
    return   ( start < ( this.startPos + this.size + ( this.size / 2) ) ) &&
            ( ( start + this.size + ( this.size / 2 ) ) > this.startPos );
  },

  //----------------------------------------------------------------------------

  isInRange: function( start, count ) {
    return   ( start < ( this.startPos + this.size) ) &&
            ( ( start + count ) > this.startPos );
  },

  //----------------------------------------------------------------------------

  isFullyInRange: function( position ) {
    // // console.log( "LiveGridBuffer.isFullyInRange: position:       ", position );
    // // console.log( "LiveGridBuffer.isFullyInRange: start position: ", this.startPos );

    var retVal = ( position >= this.startPos ) && 
                 ( position + this.metaData.getPageSize() ) <= ( this.startPos + this.size );
//     // console.log( 
//      "LiveGridBuffer.isFullyInRange: return val: ( %s >= %s ) && ( %s + %s ) <= ( %s + %s ) = %s",
//      position, this.startPos, position, this.metaData.getPageSize(), this.startPos, this.size, retVal
//    );
    // console.log( 'teh isFUllyInRange is '+retVal );
    return retVal;
  },

  //----------------------------------------------------------------------------

  isNearingTopLimit: function( position ) {
    // // console.log( "LiveGridBuffer.isNearingTopLimit: position:       ", position );
    // // console.log( "LiveGridBuffer.isNearingTopLimit: start position: ", this.startPos );

    var retVal = ( position - this.startPos ) < this.metaData.getLimitTolerance();

//     // console.log(
//      "LiveGridBuffer.isNearingTopLimit: return val: ( %s - %s ) < %s = %s",
//      position, this.startPos, this.metaData.getLimitTolerance(), retVal
//    );

    return retVal;
  },

  //----------------------------------------------------------------------------

  isNearingBottomLimit: function( position ) {
    // // console.log( "LiveGridBuffer.isNearingBottomLimit: position:   " + position );

    var pageEnd   = position + this.metaData.getPageSize();
    var bufferEnd = this.startPos + this.size - 1;

    // // console.log( "LiveGridBuffer.isNearingBottomLimit: page end:   " + pageEnd );
    // // console.log( "LiveGridBuffer.isNearingBottomLimit: buffer end: " + bufferEnd );

    var retVal = ( bufferEnd - pageEnd ) < this.metaData.getLimitTolerance();
//     // console.log( 
//      "LiveGridBuffer.isNearingBottomLimit: return val: ( %s  - %s ) < %s = %s", 
//      bufferEnd, pageEnd, this.metaData.getLimitTolerance(), retVal
//    );

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

  isNearingLimit: function( position ) {
    return ( ! this.isAtTop()    && this.isNearingTopLimit( position ) ) ||
            ( ! this.isAtBottom() && this.isNearingBottomLimit( position ) );
  },

  //----------------------------------------------------------------------------

  getRows: function( start, count ) {
    // // console.log( "LiveGridBuffer.getRows: start" );

    var begPos = parseInt(start) - this.startPos;
    var endPos = begPos + parseInt(count);
    // // console.log( "LiveGridBuffer.getRows: start - end: %d - %d", begPos, endPos );

    return this.rows.slice( begPos, endPos );

    // // console.log( "LiveGridBuffer.getRows: end" );
  }

  //----------------------------------------------------------------------------
  
} );


//------------------------------------------------------------------------------
//- LiveGridRequest ------------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGridRequest = Class.create( {

  initialize: function( requestOffset, options ) {
    // // console.log( "LiveGridRequest.initialize: start" );

    this.requestOffset = requestOffset;

    // // console.log( "LiveGridRequest.initialize: end" );
  }

} );


//------------------------------------------------------------------------------
//- LiveGrid -------------------------------------------------------------------
//------------------------------------------------------------------------------


var LiveGrid = Class.create( {

  initialize: function( grid, visibleRows, totalRows, url, options ) {
    // console.log( "(1)LiveGrid.initialize: start with |%s|%s|%s|%s|%s|",grid, visibleRows, totalRows, url, options );

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
      // console.log( "(6)options.prefetchBuffer defined "+options.prefetchBuffer);
      var offset = 0;
      if ( options.prefetchOffset ) {
        this.scroller.moveScroll( options.prefetchOffset );
        offset = options.prefetchOffset;
      }
      
      // console.log( "(7)fetchBuffer called with offset true |%s|%s|",offset,true );
      this.fetchBuffer( offset, true );
    } else {
      // console.log( "(A)liveGrdScroller called" );
      this.scroller = new LiveGridScroller( this );
    }

    // console.log( "LiveGrid.initialize: end" );
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

     // console.log( "(12)LiveGrid.largeBufferWindowStart: return val: %d - ( ( 0.5 * %d ) - ( 0.5 * %d ) ) = %d", offset, this.metaData.getLargeBufferSize(), this.metaData.getPageSize(), val );

    return Math.max( 0, parseInt(val) );
  },

  //----------------------------------------------------------------------------

  handleTimedOut: function() {
    // console.log( "(21)LiveGrid.handleTimedOut: start" );

    //server did not respond in n seconds... assume that there could have been
    //an error or something, and allow requests to be processed again...
    this.processingRequest = null;
    this.processQueuedRequest();

    // console.log( "(22)LiveGrid.handleTimedOut: end" );
  },

  //----------------------------------------------------------------------------

  fetchBuffer: function( offset, fullBufferp ) {
    //// console.log( "(8)LiveGrid.fetchBuffer: start " );

    if ( this.processingRequest ) {
      //// console.log( "(9)LiveGrid.fetchBuffer: processing request..." );
      this.unprocessedRequest = new LiveGridRequest( offset );
      return;
    }
    
    var fetchSize = this.metaData.getBufferSize( fullBufferp );
    // console.log( "(11)the fetchsize returned from above is "+fetchSize );
    var bufferStartPos = Math.max( 0, fullBufferp ? this.largeBufferWindowStart(offset) : offset );
    
    // console.log( "(13)LiveGrid.fetchBuffer: bufferStartPos: |%d|", bufferStartPos );

    this.processingRequest = new LiveGridRequest(offset);
    // console.log( "(14)LiveGrid:processingRequest |%d|", this.processingRequest );
    
    this.processingRequest.bufferOffset = bufferStartPos;
    // console.log( "(15)LiveGrid:processingRequest:bufferOffset |%d|", this.processingRequest.bufferOffset );
    
    var callParams = { id:        this.gridId,
                       page_size: fetchSize,
                       offset:    bufferStartPos };
    Object.extend( callParams, this.additionalParams );
    // // console.log( "LiveGrid.fetchBuffer: call parameters: " );
    // // console.log( callParams );

    // setup the AjaxRequest options

    // first, set the request parameters
    var options = { parameters: callParams };
    
    // and then add user-defined options. These should be the callbacks for the various
    // AjaxRequest response states, e.g. onLoading, onComplete, etc.
    Object.extend( options, this.options );
    
    // console.log( "(16)before teh ajaxUpdate.bind ");
    // add the update method from this class, so that we have a hook for updating the
    // page with the server response
    options.onComplete = this.ajaxUpdate.bind( this ); 
    // console.log( "(18)after the  ajaxUpdate.bind ");
    
    // console.log( "(19)before ajax request");
    // send the request
    this.ajaxRequest = new Ajax.Request( this.url, options );
    //// console.log( "(20)after ajax request the scroller value is "+this.scroller );
    
//    var test = $H( options.parameters );
//    // console.log( 'the options hash is '+test.inspect() );
    
    //this.timeoutHandler = setTimeout( this.handleTimedOut.bind(this), 10000 ); // i am commenting to increase the timeout to 6 seconds.
    this.timeoutHandler = setTimeout( this.handleTimedOut.bind(this), 120000 );
    
    // // console.log( "LiveGrid.fetchBuffer: end" );
  },

  //----------------------------------------------------------------------------

  requestContentRefresh: function( contentOffset ) {
    //// console.log( "LiveGrid.requestContentRefresh: start" );

    // console.log( "LiveGrid.requestContentRefresh: contentOffset:|%s|, scrollleftvalue:|%s| ", contentOffset , this.options.scrollvalue);

    if ( this.buffer && this.buffer.isFullyInRange(contentOffset) ) {
       // console.log( "LiveGrid.requestContentRefresh: buffer is fully in range; updating content" );
      this.updateContent(contentOffset);
      if ( this.buffer.isNearingLimit(contentOffset) ) {
        // console.log( "LiveGrid.requestContentRefresh: buffer is nearing limit; fetching buffer (true)" );
        this.fetchBuffer(contentOffset, true);
      }

    } else if ( this.buffer && this.buffer.isClose(contentOffset) ) {
      // console.log( "LiveGrid.requestContentRefresh: buffer is close; fetching buffer (true)" );
      this.fetchBuffer(contentOffset, true);

    } else {
      // console.log( "LiveGrid.requestContentRefresh: request range is outside of existing buffer; fetching buffer (false)" );
      this.fetchBuffer(contentOffset, false);
    }

    // console.log( "LiveGrid.requestContentRefresh: end" );
  },

  //----------------------------------------------------------------------------

  ajaxUpdate: function( ajaxResponse ) {
    // console.log( "(17)LiveGrid.ajaxUpdate: start"+ajaxResponse );

    clearTimeout( this.timeoutHandler );

    this.buffer = new LiveGridBuffer( this.metaData );
    // console.log( "(17.1)the buffer is "+this.buffer);
    
    // console.log( "(17.2)the buffer update called");
    this.buffer.update( ajaxResponse, this.processingRequest.bufferOffset );
    // console.log( "(17.3)the buffer update finished");
    
    if ( this.unprocessedRequest == null ) {
      offset = this.processingRequest.requestOffset;
      // console.log( "(17.4) unprocessed reques is null and the offset is "+offset );
      this.updateContent (offset);
    }
    this.processingRequest = null;

    if ( ! this.scroller ) {
      // console.log( "(17.5) the scroller is not set so setting the scroller " );
      this.scroller = new LiveGridScroller(this);
      if ( this.options.onFirstContent ) {
        this.options.onFirstContent(this);
      }
    }      
    if ( this.options.onComplete ) {
      this.options.onComplete(this);
    }
    this.processQueuedRequest();

    // // console.log( "LiveGrid.ajaxUpdate: end" );
  },

  //----------------------------------------------------------------------------

  processQueuedRequest: function() {
    // // console.log( "LiveGrid.processQueuedRequest: start" );

    if ( this.unprocessedRequest != null) {
      // // console.log( "LiveGrid.processQueuedRequest: found an unprocessed request..." );
      this.requestContentRefresh(this.unprocessedRequest.requestOffset);
      this.unprocessedRequest = null;
    }

    // // console.log( "LiveGrid.processQueuedRequest: end" );
  },

  //----------------------------------------------------------------------------

  updateContent: function( startPos ) {
    // console.log( "(17.4.1) LiveGrid.updateContent: start "+startPos );

    if ( startPos == this.lastDisplayedStartPos ) { 
      // console.log( '(17.4.2) this is equivalent to lastdispalyedpos so returning '+ this.lastDisplayedStartPos );
      return;
    }
    
    // console.log( "(17.4.3) set the displayed pos from | %d | to | %d |", this.lastDisplayedStartPos, startPos );
    this.lastDisplayedStartPos = startPos;
    
//    var outputDiv = new Element( "div", { id: "output" } );
//
//    var accessionsDiv = new Element( "div", { id: "accessions" } );
//    var sequencesDiv  = new Element( "div", { id: "sequences" } );
//

    var accessionsDiv = this.options.accessionsDiv;
    var sequencesDiv = this.options.sequencesDiv;
    
    accessionsDiv.update();
    sequencesDiv.update();
    
//    outputDiv.appendChild( accessionsDiv );
//    outputDiv.appendChild( sequencesDiv );    
    
    var rows = this.buffer.getRows( startPos, this.metaData.getPageSize() );
    
    for (var i = 0, len = rows.length; i < len; ++i ) {
      var row = rows[i];
      var className = i % 2 ? "odd" : "even";
  
//      var accDiv = new Element( "div", { id: "acc_"+i, "class": "seq_acc " + className } )
//                   .update( i+startPos + ": " + row.acc );
//      var seqDiv = new Element( "div", { id: "seq_"+i, "class": "seq_seq " + className } )
//                   .update( row.seq );
      
      // use the regular expressions in javascript and get the id and start and end coords;
//      var acc_id;
//      var regExp = /(\w+)\/\d+/;
//      
//      // check whether this is pfam source;
//      if( /(\w+)\/\d+/.test( rows[0] ) )
//      {
//        var matches = /(\w+)\/\d+/.exec( row[0] );
//        acc_id = matches[1];
//        
//      }else if( /(.*)\/\d+/.test( row[0] ) )
//      {
//        var matches = /(.*)\/\d+/.exec( row[0] );
//        acc_id = matches[1];
//        
//      }else
//      {
//        acc_id = row[0];  
//      }
      
      //console.log( 'the accession is '+acc_id );
      var accDiv = new Element( "div" ).update( " " + row[0] );
      var seqDiv = new Element( "div" ).update( " "+ row[1] );
      
      accessionsDiv.appendChild( accDiv );
      sequencesDiv.appendChild( seqDiv );
    }
    
    //this.grid.update( outputDiv );
    
    // store the alignmnets here;
    this.alignments = this.buffer.alignments;
    
    // we know the previous position of the scroller, set that so that user wouldnt find any difference in the grid;
    sequencesDiv.scrollLeft = this.options.scrollvalue;
    
    // // console.log( "LiveGrid.updateContent: end" );
  },

  //----------------------------------------------------------------------------
/*
  updateContent: function( startPos ) {
    // console.log( "LiveGrid.updateContent: start" );

    if ( startPos == this.lastDisplayedStartPos ) { 
      return;
    }
    this.lastDisplayedStartPos = startPos
    
    var outputDiv = new Element( "div", { id: "output" } );

    var rows = this.buffer.getRows( startPos, this.metaData.getPageSize() );

    for (var i = 0, len = rows.length; i < len; ++i ) {
      var row = rows[i];
      var rowDiv = new Element( "div", { "class": i % 2 ? "odd" : "even" } );
      outputDiv.appendChild( rowDiv );
      
      var accDiv = new Element( "div", { id: "acc_"+i, "class": "seq_acc" } )
                   .update( i+startPos + ": " + row.acc );
      var seqDiv = new Element( "div", { id: "seq_"+i, "class": "seq_seq" } )
                   .update( row.seq );

      rowDiv.appendChild( accDiv );
      rowDiv.appendChild( seqDiv );
      
    }
    
    this.grid.update( outputDiv );
    
    // console.log( "LiveGrid.updateContent: end" );
  }
*/
  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  
  getAlignment : function(){
    //console.log( "the buffer alignments are "+$H( this.alignments).inspect() );
    return this.alignments;
  }
   //----------------------------------------------------------------------------
} );

