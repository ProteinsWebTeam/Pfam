
// stack.js
// jt6 20080207 WTSI
//
// A simple implementation of a limited-height stack. We can't 
// extend builtins like Array, so we end up doing the donkey work 
// ourselves...
// 
// The maximum permitted height of the stack can be specified as 
// an argument to the constructor, or using the maxHeight method.

var Stack = Class.create( Enumerable, {

  // constructor
  initialize: function( maxHeight ) {
    if ( maxHeight === undefined ) {
      this.height = 4; // default
    } else {
      this.height = maxHeight;
    }
    this.array = new Array;
  },

  // sets/gets the maximum permitted height for the stack
  maxHeight: function( newHeight ) {
    if( newHeight !== undefined ) {
      this.height = newHeight;
    }
    return this.height;
  },

  // adds a new item to the top (or bottom, depending on how you think
  // of your stacks) of this stack. If the stack height exceeds
  // the maximum permitted height, the bottom (or top) item is removed
  // and returned
  push: function( item ) {
    this.array.push( item );
    if ( this.array.length > this.height ) {
      return this.array.shift();
    }
  },
  
  // returns the index of the specified item within the stack, or
  // -1 if the item is not found
  indexOf: function( item ) {
    return this.array.indexOf( item );
  },

  // removes the topmost item of the stack and returns it
  pop: function() {
    return this.array.pop();
  },

  // adds the supplied item to the bottom of the stack
  // stack
  unshift: function( item ) {
    this.array.unshift( item );
  },

  // removes the bottommost item of the stack and returns it
  shift: function() {
    return this.array.shift();
  },
  
  // an iterator function to allow this object to behave as an
  // Enumerable
  _each: function( iterator ) {
    for (var i = 0, length = this.array.length; i < length; i++) {
      iterator( this.array[i] );
    }
  }

} );
