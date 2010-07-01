
var Node = Class.create( {
	
	initialize: function( nodeData ) {
		this.numSequences = 0;
		this.numDomains   = 0;
		this.numSpecies   = 0;
		this.nodeName     = "";
		this.id           = null;
		this.children     = [];
		this.parentNode   = {};
		Object.extend( this, nodeData );
	},

	toString: function() { 
    return "Node object (" + this.nodeName + ") with " + this.children.length + " children";
  }

} );

var TreeFactory = function( treeData, parentNode ) {
	var node = new Node( {
		numSequences: treeData.numSequences,
		numDomains:   treeData.numDomains,
		numSpecies:   treeData.numSpecies,
		nodeName:     treeData.node,
		id:           treeData.id,
		parentNode:   parentNode
	} );

	if ( treeData.children !== undefined ) {
		treeData.children.each( function( child ) {
			node.children.push( TreeFactory( child, node ) );
		}.bind(this) );
	}

	return node;
};

