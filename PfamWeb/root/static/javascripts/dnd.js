
// set up drag-and-drop
// jt 20060303 WTSI

// initialise everything
function dragDropInit() {

  // set up the logging
  var log;
  if (typeof(ygLogger) != "undefined") {
	ygLogger.init( YAHOO.util.Dom.get( "logDiv" ) );
	log = new ygLogger( "dragDropInit" );
  }

  // get the divs that define blocks
  var divs = new Array();
  var d = 0;
  var left = YAHOO.util.Dom.get( "left" ).getElementsByTagName( "div" );
  for( var i in left ) {
	if( "draggable" == left[i].className ) divs[d++] = left[i];
  }
  var right = YAHOO.util.Dom.get( "right" ).getElementsByTagName( "div" );
  for( var i in right ) {
	if( "draggable" == right[i].className ) divs[d++] = right[i];
  }

  // need to extract them to a separate list before adding
  // targets... at least in FF
  var draggables = new Object();
  var draggableNum = 0;
  for( var i in divs ) {
  	var div = divs[i];
  	if( "draggable"  == div.className ) draggables[draggableNum++] = div;
  }
  
  // make a Draggable object from each block with a className "draggable"
  for( var i in draggables ) {
	var draggableDiv = draggables[i];
	var id = IdGenerator.getId();
	log.debug( "adding Draggable " + id );
	draggableDiv.id = id;
	new Draggable( id );
  }
}

// initialise dnd on window load
YAHOO.util.Event.addListener(window, "load", dragDropInit);

