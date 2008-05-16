
// treeTweak.js
// jt6 20070115 WTSI.
//
// augment the base TextNode from Yahoo with functions to walk down
// the tree. These methods are here to allow cascades to work for
// TextNodes, although we're primarily interested in the TaskNodes,
// which have a check method from the outset
//
// $Id: treeTweak.js,v 1.2 2008-05-16 14:58:22 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
// 
// This is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
// 
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <http://www.gnu.org/licenses/>.

YAHOO.widget.TextNode.prototype.check = function( state ) { 
  for( var i = 0; i < this.children.length; i++ ) {
	this.children[i].check( state );
  }
};

YAHOO.widget.TextNode.prototype.uncheck = function( state ) { 
  this.check( 0 );
};
