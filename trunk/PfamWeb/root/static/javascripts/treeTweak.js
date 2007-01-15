
// augment the base TextNode from Yahoo with functions to walk down
// the tree. These methods are here to allow cascades to work for
// TextNodes, although we're primarily interested in the TaskNodes,
// which have a check method from the outset

YAHOO.widget.TextNode.prototype.check = function( state ) { 
  for( var i = 0; i < this.children.length; i++ ) {
	this.children[i].check( state );
  }
};

YAHOO.widget.TextNode.prototype.uncheck = function( state ) { 
  this.check( 0 );
};
