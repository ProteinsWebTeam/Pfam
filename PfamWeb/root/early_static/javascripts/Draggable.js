
// Draggable Class
// jt 20060303 WTSI

// extend DD
Draggable.prototype = new YAHOO.util.DD();

//----------------------------------------------------------------------
//- class variables ----------------------------------------------------
//----------------------------------------------------------------------

// another handle on the logger, this one for the whole class
Draggable.log = new ygLogger( "Draggable" );

// list of all of the draggables that get created... ever ever. Need
// to add in the concept of groups sometime though
Draggable.draggables = {};

// constant for the opacity of blocks being dragged
Draggable.OPACITY = 0.6;

// constants for recording the positions of Draggables relative to
// one another
Draggable.LOST  = -1;
Draggable.ABOVE =  0;
Draggable.BELOW =  1;

// constants showing that the block is open or closed
Draggable.OPEN   = 1;
Draggable.CLOSED = 0;
Draggable.OPEN_IMAGE   = "/static/images/open.gif";
Draggable.CLOSED_IMAGE = "/static/images/closed.gif";

Draggable.CONTENT_DIV = null;

// the URL to post layout data to
Draggable.STORE_URL = "/store";
//Draggable.STORE_URL = "http://web-1-14.internal.sanger.ac.uk:3000/store";

//----------------------------------------------------------------------
//- instance variables -------------------------------------------------
//----------------------------------------------------------------------

// the logger - this one is specific to instances of the class
Draggable.prototype.log = null;

Draggable.prototype.blockId = null;

// the various HTML components of the draggable block
Draggable.prototype.handleEl  = null;
Draggable.prototype.closerEl  = null;
Draggable.prototype.contentEl = null;

// the Z-index for the dragged block;
Draggable.prototype.origZIndex = 0;

// the animator that's responsible for moving the block back at the
// end of a non-target drop
Draggable.prototype.animator = null;

// flag denoting a drop on a valid target
Draggable.prototype.dropped = false;

// the original position of the block, prior to a drag start
Draggable.prototype.origX = 0;
Draggable.prototype.origY = 0;

// the targets that we'll add to the block when this Draggable is
// instantiated
Draggable.prototype.topTarget = null;
Draggable.prototype.bottomTarget = null;

// whether the block is open or closed
Draggable.prototype.state = Draggable.OPEN;

//----------------------------------------------------------------------
//- constructor --------------------------------------------------------
//----------------------------------------------------------------------

function Draggable( id, sGroup ) {
  this.draggableInit( id, sGroup );
}

//----------------------------------------------------------------------
//- initialisation -----------------------------------------------------
//----------------------------------------------------------------------

Draggable.prototype.draggableInit = function( id, sGroup ) {

  if( ! id ) { return; }

  this.log = new ygLogger( "Draggable " + id );

  this.init( id, sGroup );            // initialise the superclass
  this.isTarget = false;              // stop the block itself being a target

  // store this Draggable instance in the class
  Draggable.draggables[id] = this;

  // install target divs for the draggable, above and below the block
  // content. Also set the visibility of the two, according to the
  // position of this draggable in the column
  this.installTargets();

  // add an animator to move this block back to the starting position
  // when dropped outside of a target
  this.origX = YAHOO.util.Dom.getX( id );
  this.origY = YAHOO.util.Dom.getY( id );
  this.animator = new YAHOO.util.Motion( 
                    id, 
					{ points: { to: [this.origX,this.origY] } },
					0.5,
					YAHOO.util.Easing.easeOut );

  // find the handle, the content and the closer image
  var internalDivs = this.getEl().getElementsByTagName( "div" );
  for( var i = 0; i < internalDivs.length; i++ ) {
	var div = internalDivs[i];
	if( "handle"       == div.className ) { this.handleEl  = div; }
	if( "block"        == div.className ) { this.blockId   = div.id; }

	// the block content div could have multiple CSS classes...
	if( ( ' ' + div.className + ' ' ).indexOf("blockContent") != -1 ) {
	  this.contentEl = div; 
	}

	// see if this block is intended to start life closed
	if( ( ' ' + div.className + ' ' ).indexOf("closed") != -1 ) {
	  this.state = Draggable.CLOSED;

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
  this.closerEl.src = Draggable.OPEN_IMAGE;
  this.closerEl.className = "open";
  this.handleEl.insertBefore( this.closerEl, this.handleEl.firstChild );
  this.addInvalidHandleType( "img" );

  // if we've not stored it already, stash the div that contains all
  // the draggables
  if( ! Draggable.CONTENT_DIV ) {
	Draggable.CONTENT_DIV = YAHOO.util.Dom.get( "content" );
  }

  // assign IDs for the handle, content and image
  this.handleEl.id  = IdGenerator.getId();
  this.contentEl.id = IdGenerator.getId();
  this.closerEl.id  = IdGenerator.getId();

  // now we've got IDs for everything...

  // close the block, if it's supposed to start life closed
  if( Draggable.CLOSED == this.state ) {
	this.closeBlock();
  }

  // set the handle div as the drag handle for this Draggable
  this.setHandleElId( this.handleEl.id );

  // add the callback to open/close the block
  YAHOO.util.Event.addListener( this.closerEl.id, 
								"click", 
								Draggable.show, 
								this );
};

//----------------------------------------------------------------------
//- class methods ------------------------------------------------------
//----------------------------------------------------------------------

// show/hide block content

Draggable.show = function( e, oDraggable ) {

  var bodyEl  = oDraggable.contentEl;
  var imageEl = oDraggable.closerEl;

  var currStyle = bodyEl.style;
  var displayVal = currStyle.display ? currStyle.display : "";

  if( displayVal == "block" || displayVal === "" ) {
  	oDraggable.closeBlock();
  } else {
	oDraggable.openBlock();
  } 

  Draggable.saveLayout( oDraggable.getEl().parentNode );
};

//-------------------------------------

Draggable.prototype.openBlock = function() {
  this.contentEl.style.display = "block";
  this.closerEl.src = Draggable.OPEN_IMAGE;
  this.state = Draggable.OPEN;
}

//-------------------------------------

Draggable.prototype.closeBlock = function() {
  this.contentEl.style.display = "none";
  this.closerEl.src = Draggable.CLOSED_IMAGE;
  this.state = Draggable.CLOSED;
}

//----------------------------------------------------------------------
// set the end points for the animators on the draggable objects and
// turn off targets that aren't required

Draggable.update = function() {

  for( var i in Draggable.draggables ) {

	var draggable = Draggable.draggables[i];
	//draggable.log.debug( "updating draggable " + draggable.id );

	var newX = YAHOO.util.Dom.getX( draggable.getEl().id );
	var newY = YAHOO.util.Dom.getY( draggable.getEl().id );

	// update the animator
	draggable.animator.attributes.points = { to: [newX,newY] };

	// turn off the targets for this draggable
	draggable.setTargetsOff();
  }
};

//----------------------------------------------------------------------
// turn off the targets for all registered Draggables

Draggable.setAllTargetsOff = function() {

  for( var i in Draggable.draggables ) {
	var draggable = Draggable.draggables[i];
	draggable.setTargetsOff();
  }

};

//----------------------------------------------------------------------
// save the page layout to the server

// callback for successful save of layout
Draggable.storeSuccess = function( o) { 
  Draggable.log.debug( "stored state successfully" );
};

//-------------------------------------

// save failed...
Draggable.storeFailure = function( o) { 
  Draggable.log.debug( "storing state FAILED" );
};

//-------------------------------------

// handle the whole save process

Draggable.saveLayout = function( columnEl ) {

  // build a JSON object to record the layout
  var layout = {};

  // get the column divs
  var divs = Draggable.CONTENT_DIV.getElementsByTagName( "div" );
  for( var i in divs ) {
	var div = divs[i];
	if( "column" != div.className ) { continue; }

	var colId = div.id;
	layout[colId] = {};
	
	// and for each column get the draggable blocks
	var num = 0;
	var innerDivs = div.getElementsByTagName( "div" );
	for( var j in innerDivs ) {
	  var dragDiv = innerDivs[j];

	  if( "draggable" == dragDiv.className ) {
		var d = Draggable.draggables[dragDiv.id];
		layout[colId][num] = {};
		layout[colId][num++][d.blockId] = d.state;
	  }
	}
  }

  // toString the JSON object
  var layoutString = JSON.stringify(layout);

  Draggable.log.debug( "location string: |" + layoutString + "|" );

  // set up the callbacks for the XMLHttpRequest
  var callback = {
	success:Draggable.storeSuccess,
	failure:Draggable.storeFailure
  };

  // tie the form to the Connect object
  var layoutInput = YAHOO.util.Dom.get( "layoutInput" );
  layoutInput.value = layoutString;

  // and post the new layout to the server
  YAHOO.util.Connect.setForm( "storeLayout" );
  var c = YAHOO.util.Connect.asyncRequest(
			"POST",
			Draggable.STORE_URL,
			callback );
};

//----------------------------------------------------------------------
//- instance methods ---------------------------------------------------
//----------------------------------------------------------------------

// open/close the block contents by showing/hiding the blockContent div

Draggable.prototype.openBlock = function() {
  this.contentEl.style.display = "block";
  this.closerEl.src = Draggable.OPEN_IMAGE;
  this.state = Draggable.OPEN;
}

//-------------------------------------

Draggable.prototype.closeBlock = function() {
  this.contentEl.style.display = "none";
  this.closerEl.src = Draggable.CLOSED_IMAGE;
  this.state = Draggable.CLOSED;
}

//----------------------------------------------------------------------
// turn off and hide both targets

Draggable.prototype.setTargetsOff = function() {
  this.topTarget.isTarget = false;
  this.topTarget.getEl().className = "target";
  this.bottomTarget.isTarget = false;
  this.bottomTarget.getEl().className = "target";
};

//----------------------------------------------------------------------
// install two target areas, above and below the block content
  
Draggable.prototype.installTargets = function() {
  //this.log.debug( "installing targets for " + this.getEl().id );

  // create the divs and make DDTargets from them. Turn them off by
  // default though
  var t                   = document.createElement( "div" );
  t.id                    = IdGenerator.getId( "target" );
  t.className             = "target";
  this.topTarget          = new YAHOO.util.DDTarget( t.id );
  this.topTarget.isTarget = false;

  var b                      = document.createElement( "div" );
  b.id                       = IdGenerator.getId( "target" );
  b.className                = "target";
  this.bottomTarget          = new YAHOO.util.DDTarget( b.id );
  this.bottomTarget.isTarget = false;

  // add them to the DOM
  this.getEl().insertBefore( t, this.getEl().firstChild );
  this.getEl().insertBefore( b, this.getEl().lastChild.nextSibling );

};

//----------------------------------------------------------------------
// find the position of the block with the specified ID, either above
// or below the current Draggable block.

Draggable.prototype.getRelativePosition = function( id ) {

  var divs = this.getEl().parentNode.getElementsByTagName( "div" );
  for( var i in divs ) {
	var div = divs[i];
	if( "draggable" != div.className ) { continue; }

	if( div.id == this.getEl().id ) { return Draggable.BELOW; }
	if( div.id == id )              { return Draggable.ABOVE; }
  }

  return Draggable.LOST;
};

//----------------------------------------------------------------------
//- DnD event handling -------------------------------------------------
//----------------------------------------------------------------------

// mouse button went down

Draggable.prototype.onMouseDown = function( e ) { /* do nothing */ };

//----------------------------------------------------------------------
// a drag has been started

Draggable.prototype.startDrag = function( e ) {
  this.log.debug( "startDrag" );

  YAHOO.util.Dom.setStyle( this.getEl(), "opacity", Draggable.OPACITY );
  this.dropped = false;

  // store the blocks starting position
  this.origX = YAHOO.util.Dom.getX( this.getEl() );
  this.origY = YAHOO.util.Dom.getY( this.getEl() );

  // store the initial Z-index for the dragged block and bring it to
  // the top of the stack whilst under drag
  this.origZIndex = this.getEl().style.zIndex;
  this.getEl().style.zIndex = 999;

  // turn on appropriate drop targets
  var divs = this.getEl().parentNode.getElementsByTagName( "div" );
  for( var i in divs ) {
	var div = divs[i];
	if( "draggable" != div.className ) { continue; }
	if( div.id == this.getEl().id ) { continue; }
	var draggable = Draggable.draggables[div.id];

	var relativePosition = this.getRelativePosition( div.id );
	if( Draggable.ABOVE == relativePosition ) {

	  //this.log.debug( "draggable " + div.id 
	  //                + " is ABOVE this (" + this.getEl().id + ")" );
	  draggable.topTarget.getEl().className = "targetActive";
	  draggable.topTarget.isTarget = true;

	} else if( Draggable.BELOW == relativePosition ) {

	  //this.log.debug( "draggable " + div.id 
	  //                + " is BELOW this (" + this.getEl().id + ")" );
	  draggable.bottomTarget.getEl().className = "targetActive";
	  draggable.bottomTarget.isTarget = true;

	} else {
	  // shouldn't happen, but... leave all targets off if we can't
	  // find a place to drop this Draggable

	  //this.log.debug( "draggable " + div.id + " is LOST IN SPACE" );
	}

  }

};

//----------------------------------------------------------------------
// fired for every movement during a drag

Draggable.prototype.onDrag = function( e ) { /* do nothing */ };

//----------------------------------------------------------------------
// entered a drop target

Draggable.prototype.onDragEnter = function( e, targetId ) {
  this.log.debug( "onDragEnter" );

  YAHOO.util.Dom.get( targetId ).className = "targetSelected";
  //this.getEl().className = "droppable";
};

//----------------------------------------------------------------------
// fired for each movement over a drop target
Draggable.prototype.onDragOver = function( e, id ) { /* do nothing */ };

//----------------------------------------------------------------------
// exited a drop target

Draggable.prototype.onDragOut = function( e, targetId ) {
  this.log.debug( "onDragOut" );

  YAHOO.util.Dom.get( targetId ).className = "targetActive";
  //this.getEl().className = "draggable"
};

//----------------------------------------------------------------------
// actually dropped on a target

Draggable.prototype.onDragDrop = function( e, targetId ) {
  this.log.debug( "onDragDrop" );
  this.log.debug( "dropped on a target: " + targetId );

  YAHOO.util.Dom.get( targetId ).className = "target";
  //this.getEl().className = "dropped";
  YAHOO.util.Dom.setStyle( this.getEl(), "opacity", 1 );
  this.dropped = true;

  // find the block that holds the target that the block was actually
  // dropped onto and swap the positions of that block and the dragged
  // one...

  var toBlock = YAHOO.util.Dom.get( targetId ).parentNode;
  //this.log.debug( "parent: " + toBlock.id );

  var from = this.getEl().parentNode;
  var to   = toBlock.parentNode;

  // looks like we need to reset the position of the dragged block
  // before swapping it with the target
  YAHOO.util.Dom.setX( this.getEl(), this.origX );
  YAHOO.util.Dom.setY( this.getEl(), this.origY );

  // swap the blocks
  var placeholder = document.createElement( "div" );
  from.replaceChild( placeholder, this.getEl() );
  to.replaceChild( this.getEl(), toBlock );
  to.replaceChild( toBlock, placeholder );

  // recalculate the starting positions for all draggable blocks and
  // turn off all targets
  Draggable.update();

  Draggable.saveLayout( this.getEl().parentNode );

};

//----------------------------------------------------------------------
// drag ended - fired both for drops on a target and elsewhere

Draggable.prototype.endDrag = function( e ) {
  this.log.debug( "endDrag" );

  var myId = this.getEl().id;

  // if this wasn't a valid drop, move the block back to its start point
  if( ! this.dropped ) { 
	this.animator.animate();
  }

  // reset the look of the block
  YAHOO.util.Dom.setStyle( this.getEl(), "opacity", 1 );
  this.getEl().style.zIndex = this.origZIndex;

  // turn off all targets for all blocks
  Draggable.setAllTargetsOff();
};

//----------------------------------------------------------------------
