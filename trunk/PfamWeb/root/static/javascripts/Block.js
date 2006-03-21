
// Block Class
// jt 20060303 WTSI


//----------------------------------------------------------------------
//- class variables ----------------------------------------------------
//----------------------------------------------------------------------

Block.blocks = {};

// constants showing that the block is open or closed
Block.OPEN   = 1;
Block.CLOSED = 0;
Block.OPEN_IMAGE   = "/static/images/open.gif";
Block.CLOSED_IMAGE = "/static/images/closed.gif";

// the URL to post layout data to
Block.STORE_URL = "/store";

//----------------------------------------------------------------------
//- instance variables -------------------------------------------------
//----------------------------------------------------------------------

// the various HTML components of the block
Block.prototype.handleEl  = null;
Block.prototype.closerEl  = null;
Block.prototype.contentEl = null;

// whether the block is open or closed
Block.prototype.state = Block.OPEN;

//----------------------------------------------------------------------
//- constructor --------------------------------------------------------
//----------------------------------------------------------------------

function Block( id ) {
  this.initialiseBlocks( id );
}

//----------------------------------------------------------------------
//- initialisation -----------------------------------------------------
//----------------------------------------------------------------------

Block.prototype.initialiseBlocks = function( id ) {

  if( ! id ) { return; }

  // store this Draggable instance in the class
  Block.blocks[id] = this;

  // find the handle, the content and the closer image
  var internalDivs = YAHOO.util.Dom.get(id).getElementsByTagName( "div" );
  for( var i = 0; i < internalDivs.length; i++ ) {
	var div = internalDivs[i];

	// get the handle
	if( ( ' ' + div.className + ' ' ).indexOf("handle") != -1 ) {
	  this.handleEl  = div; 
	}

	// the block content div could have multiple CSS classes...
	if( ( ' ' + div.className + ' ' ).indexOf("blockContent") != -1 ) {
	  this.contentEl = div; 
	}

	// see if this block is intended to start life closed
	if( ( ' ' + div.className + ' ' ).indexOf("closed") != -1 ) {
	  this.state = Block.CLOSED;

	  // remove the "closed" class name from this div
	  var cnIndex = ( ' ' + div.className + ' ' ).indexOf("closed");
	  var start = div.className.substring( 0, cnIndex - 2 );
	  var end   = div.className.substring( cnIndex + 6 );
	  var newClass = start + end;
	  div.className = newClass;
	}
  }

  // add the open/close gif and make sure we can't drag using that image
  this.closerEl = document.createElement( "img" );
  this.closerEl.src = Block.OPEN_IMAGE;
  this.closerEl.className = "open";
  this.handleEl.insertBefore( this.closerEl, this.handleEl.firstChild );

  // close the block, if it's supposed to start life closed
  if( Block.CLOSED == this.state ) {
	this.close();
  }

  // add the callback to open/close the block
  YAHOO.util.Event.addListener( this.closerEl, 
								"click", 
								Block.show, 
								this );
};

//----------------------------------------------------------------------
//- class methods ------------------------------------------------------
//----------------------------------------------------------------------

// show/hide block content

Block.show = function( e, oBlock ) {
  if( Block.OPEN == oBlock.state ) {
  	oBlock.close();
  } else {
	oBlock.open();
  } 

  Block.saveLayout( oBlock );
};

//----------------------------------------------------------------------
// save the page layout to the server

// callback for successful save of layout
Block.storeSuccess = function( oBlock ) { 
  // do nothing
};

//-------------------------------------

// save failed...
Block.storeFailure = function( oBlock ) { 
  // do nothing
};

//-------------------------------------

// handle the whole save process

Block.saveLayout = function() {

  // build a JSON object to record the layout
  var layout = {};

  // get the column divs
  for( var id in Block.blocks ) {
	layout[id] = Block.blocks[id].state;
  }

  // toString the JSON object
  var layoutString = JSON.stringify(layout);

  // set up the callbacks for the XMLHttpRequest
  var callback = {
	success:Block.storeSuccess,
	failure:Block.storeFailure
  };

  // tie the form to the Connect object
  var layoutInput = YAHOO.util.Dom.get( "layoutInput" );
  layoutInput.value = layoutString;

  // and post the new layout to the server
  YAHOO.util.Connect.setForm( "storeLayout" );
  var c = YAHOO.util.Connect.asyncRequest(
			"POST",
			Block.STORE_URL,
			callback );
};

//----------------------------------------------------------------------
//- instance methods ---------------------------------------------------
//----------------------------------------------------------------------

// open/close the block contents by showing/hiding the blockContent div

Block.prototype.open = function() {
  this.contentEl.style.display = "block";
  this.closerEl.src = Block.OPEN_IMAGE;
  this.state = Block.OPEN;
}

//-------------------------------------

Block.prototype.close = function() {
  this.contentEl.style.display = "none";
  this.closerEl.src = Block.CLOSED_IMAGE;
  this.state = Block.CLOSED;
}

//----------------------------------------------------------------------
