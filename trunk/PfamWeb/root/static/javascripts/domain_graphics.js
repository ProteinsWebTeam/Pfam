
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $R, $w, $break, Class, console, Element, Hash, document, window, 
  G_vmlCanvasManager, Template, Tip */

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript library for drawing Pfam-style domain graphics in an HTML5
// canvas.
//
// jt6 20090803 WTSI
//
// $Id: domain_graphics.js,v 1.3 2009-08-11 12:27:38 jt6 Exp $
//
// Copyright (c) 2009: Genome Research Ltd.
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

var PfamGraphic = Class.create( {

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function( parent, sequence ) {
    // console.log( "PfamGraphic.initialize: setting up a new object" );

    // defaults for the image parameters
    this._imageParams = {

      // lollipop head dimensions
      headSizeCircle:  3,
      headSizeSquare:  6,
      headSizeDiamond: 4,
      headSizeArrow:   3,
      headSizeLine:    3,
      
      // parameters for adjusting the edges of domains
      defaultMarkupHeight:         20, // default height for a lollipop
      lollipopToLollipopIncrement: 7,  // step up each lollipop X pixels from the previous one
      bridgeToBridgeIncrement:     2,  // step up each bridge X pixels from the last bridge
      bridgeToLollipopIncrement:   5,  // step up each bridge X pixels from the last lollipop
      largeJaggedSteps:            6,  // number of steps on jagged edge (must be an even integer)

      // TODO make this configurable
      font:     "sans",
      fontSize: 9,

      // URL to link from a region
      regionUrl: "http://pfam.sanger.ac.uk/family/",

      // general image parameters
      regionHeight: 20,   // the height of a region
      motifHeight:   14,   // the height of a motif
      motifOpacity:  0.6,  // the opacity of a motif
      labelPadding:  3,    // padding for the text label on a region
      xscale:        0.5,  // xscale pixels per residue
      yscale:        1,    // not currently used
      envOpacity:    0.6   // opacity of the envelope regions
    };

    // general options, specified as part of the "sequence"
    this._options = {
      imageMap: true,  // add the image map ?
      labels:   true,  // add the text labels to regions ?
      tips:     true   // add tooltips ? Requires prototip2
    };

    // templates for labelling the various components
    this._templates = {
      lollipop:  new Template( "#{label}, residue #{pos}" ),
      bridge:    new Template( "#{label}, from #{start} to #{end}" ),
      region:    new Template( "#{label}, from #{start} to #{end}" ),
      motif:     new Template( "#{label}, from #{start} to #{end}" ),
      alignment: new Template( "#{label}, alignment region from #{start} to #{end}" ),
      envelope:  new Template( "#{label}, envelope region from #{start} to #{end}" )
    };

    // specification of various allowed values in the input
    this._markupSpec = {
      valignValues:       $w( "top bottom" ),
      linesStyleValues:   $w( "mixed bold dashed" ),
      lollipopHeadValues: $w( "diamond circle square arrow line" ),
      regionEndValues:   $w( "curved straight jagged arrow" )
    };

    // somewhere to put <area> definitions for the domains and markups
    this._heights   = [];
    this._areasList = [];
    this._areasHash = new Hash();
    
    // somewhere to cache the calculated steps for jagged edges 
    this._cache = {};

    // check for a sequence object and a parent node
    if ( parent !== undefined ) {
      this.setParent( parent );
    }

    if ( sequence !== undefined ) {
      this.setSequence( sequence );
    }

  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //- setters and getters ------------------------------------------------------
  //----------------------------------------------------------------------------

  // sets the parent node for the <canvas> element that we create

  setParent: function( parent ) {
    this._parent = $(parent);

    if ( this._parent === undefined || this._parent === null ) {
      throw( "PfamGraphic: ERROR: couldn't find the parent node" );
    }

    // console.log( "PfamGraphic.setParent: parent node ID: |%s|", 
    //   this._parent.identify() );
  },

  //----------------------------------

  // returns the parent node for the <canvas> element

  getParent: function() {
    return this._parent;
  },

  //----------------------------------------------------------------------------

  // sets the <canvas> element for this graphic

  setCanvas: function( canvas ) {
    this._canvas = $(canvas);

    if ( this._canvas === undefined || this._canvas === null ) {
      throw( "PfamGraphic: ERROR: couldn't find the canvas node" );
    }

    // exit if canvas is not supported
    if ( this._canvas.getContext === undefined ) {
      throw( "PfamGraphic: ERROR: canvas is not supported" );
    }

    this._context = this._canvas.getContext( "2d" );

    if ( this._context === undefined ) {
      throw( "PfamGraphic: ERROR: couldn't create a 2d context from canvas" );
    }

  },

  // returns the <canvas> element

  getCanvas: function() {
    return this._canvas;
  },

  //----------------------------------------------------------------------------

  // sets the image parameters

  setImageParams: function( userImageParams ) {
    this._imageParams = Object.extend( this._imageParams, userImageParams );
  },

  //----------------------------------

  // returns the image parameters

  getImageParams: function() {
    return this._imageParams;
  },

  //----------------------------------------------------------------------------

  // sets the flag that determines whether each new sequence will be rendered
  // into a new canvas

  setNewCanvas: function( newCanvas ) {
    this._newCanvas = newCanvas;
  },

  //----------------------------------

  // returns the "new canvas" boolean flag

  getNewCanvas: function() {
    return this._newCanvas;
  },

  //----------------------------------------------------------------------------

  // accepts the "sequence" object, containing all of the data needed to draw
  // the graphic

  setSequence: function( sequence ) {

    // validate the sequence object

    // first, it has to be an object...
    if ( typeof sequence !== "object" ) {
      throw( "PfamGraphic: ERROR: must supply a valid sequence object" );
    }

    // check that the sequence value makes sense
    if ( sequence.length === undefined ) {
      throw( "PfamGraphic: ERROR: must specify a sequence length" );
    }

    if ( isNaN( sequence.length ) ) {
      throw( "PfamGraphic: ERROR: sequence length must be a valid number" );
    }

    if ( sequence.length <= 0 ) {
      throw( "PfamGraphic: ERROR: sequence length must be a positive integer" );
    }

    // check the "regions", "markups" and "motifs" sections of the sequence
    if ( sequence.regions !== undefined ) {
      if ( typeof sequence.regions !== "object" ) {
        throw( "PfamGraphic: ERROR: 'regions' must be a valid object" );
      }
    } else {

      // add an empty "regions" object, to keep later code happy
      sequence.regions = [];
    }

    if ( sequence.markups !== undefined ) {
      if ( typeof sequence.markups !== "object" ) {
        throw( "PfamGraphic: ERROR: 'markups' must be a valid object" );
      }
    } else {
      sequence.markups = [];
    }

    if ( sequence.motifs !== undefined ) {
      if ( typeof sequence.motifs !== "object" ) {
        throw( "PfamGraphic: ERROR: 'motifs' must be a valid object" );
      }
    } else {
      sequence.motifs = [];
    }

    // see if any options were defined
    if ( sequence.options !== undefined ) {
      if ( typeof sequence.options !== "object" ) {
        throw( "PfamGraphic: ERROR: 'options' must be a valid object" );
      }
      this._options = Object.extend( this._options, sequence.options );
      // console.log( "PfamGraphic.setSequence: merged options: ", this._options );
    }

    // see if any image parameters were defined
    if ( sequence.imageParams !== undefined ) {
      if ( typeof sequence.imageParams !== "object" ) {
        throw( "PfamGraphic: ERROR: 'imageParams' must be a valid object" );
      }
      this.setImageParams( sequence.imageParams );
      // console.log( "PfamGraphic.setSequence: merged imageParams: ", this._imageParams );
    }

    // everything passes. Stash it
    this._sequence = sequence;

    // get the scale out of the image parameters
    // console.log( "PfamGraphic.setSequence: x_scale: / y_scale: %d / %d", 
    //   this._imageParams.xscale, this._imageParams.yscale );
    
    // scale the length of the sequence and the weight of the domain image
    this._imageWidth = this._sequence.length * this._imageParams.xscale;
    this._regionHeight = this._imageParams.regionHeight;
    // TODO start taking notice of yscale

    // console.log( "PfamGraphic.setSequence: image width, domain height: %d, %d", 
    //   this._imageWidth, this._regionHeight );

    // set the height of the sequence line and the length of one "step" across
    // that line. The line is draw as a light band, a darker band and another
    // lighter one, to give the effect of a reflection

    // set the sequence line to one sixth the height of the domains
    this._seqHeight = Math.round( this._regionHeight / 6 );

    // we want 5 steps across the sequence line
    this._seqStep   = Math.round( this._seqHeight / 5 );    

    // console.log( "PfamGraphic.setSequence: seqHeight / seqStep: %d / %d", 
    //   this._seqHeight, this._seqStep );
  },
  
  //----------------------------------

  // returns the sequence object

  getSequence: function() {
    return this._sequence;
  },

  //----------------------------------------------------------------------------

  // returns a data structure that stores what is effectively <area> data for
  // each of the regions on the sequence. Returns a list containing two
  // different versions of the same data: the first is a simple list of
  // the <area> objects, the second is a hash of the areas, keyed on the
  // domain/motif name
  
  getAreas: function() {
    return [ this._areasList, this._areasHash ];
  },

  //----------------------------------------------------------------------------
  //- public methods -----------------------------------------------------------
  //----------------------------------------------------------------------------

  // coordinates the construction and drawing of the whole graphic

  // TODO we need to re-visit the issue of using the same PfamGraphic object
  // to render multiple times. Right now we're not clearing the canvas, so 
  // new graphics are just layered on top of old ones 

  render: function( sequence, parent ) {

    if ( sequence !== undefined ) {
      this.setSequence( sequence );
    }

    if ( parent !== undefined ) {
      this.setParent( parent );
    }

    // by this point, we need to have both a sequence object and a parent
    // node (to which we'll append the <canvas>)
    if ( this._sequence === undefined ) {
      throw( "PfamGraphic: ERROR: sequence was not supplied" );
    }

    if ( this._parent === undefined ) {
      throw( "PfamGraphic: ERROR: parent node was not supplied" );
    }

    // build the markups (lollipops, etc.). This will calculate the heights
    // of the various markup elements, without actually drawing them
    this._buildMarkups();

    // find the maximum of three values, in order to determine the top
    // extend (and then bottom extent) for the graphical elements:
    //   o maximum top extend for lollipops
    //   o maximum top extend for bridges
    //   o half the domain height
    var canvasHeight = [ this._heights.lollipops.upMax,
                         this._heights.bridges.upMax,
                         ( this._regionHeight / 2 + 1 ) ].max() +
                       [ this._heights.lollipops.downMax,
                         this._heights.bridges.downMax,
                         ( this._regionHeight / 2 + 1 ) ].max() + 1;
                       // that single pixel is just a fudge factor...

    // canvas width is just calculated from the length of the sequence
    // (which is set in the "setSequence" method)
    var canvasWidth = this._imageWidth;

    // set the baseline, relative to which the various elements will
    // actually be drawn
    this._baseline = [ this._heights.lollipops.upMax,
                       this._heights.bridges.upMax,
                       this._imageParams.regionHeight / 2 ].max()  + 1;
                       // that single pixel is just a fudge factor...

    // if we don't yet have a <canvas>, build the one with the calculated
    // dimensions
    if ( ! this._canvas || this._newCanvas ) {
      // console.log( "PfamGraphic.render: building canvas with width x height, baseline: %d x %d, %d",
      //   canvasWidth, canvasHeight, this._baseline );
      this._buildCanvas( canvasWidth, canvasHeight );
    }

    // draw everything
    this._draw();

    // add mouse event listeners
    this._addListeners();

    // clean up...
    this._heights = {};
    //this._areasList   = [];

  }, // end of "render"

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // constructs a new <canvas> element for this graphic

  _buildCanvas: function( width, height ) {

    // for some reason, creating the <canvas> like this doesn't work:
    // 
    //   var canvas = new Element( "canvas", { width: width,
    //                                         height: height } );
    // 
    // instead, we need to use the plain old document.createElement and
    // then let prototype extend it:
    var canvas = document.createElement( "canvas" );
    Element.extend( canvas );

    // have to set the dimensions explicitly too
    canvas.width = width;
    canvas.height = height;

    // add the new <canvas> to the parent and generate an identifier for it
    this._parent.appendChild( canvas );
    canvas.identify();

    // make sure it gets initialised in bloody IE...
    if ( typeof G_vmlCanvasManager !== "undefined" ) {
      canvas = G_vmlCanvasManager.initElement( canvas );
    }

    // and stash it on the object
    this.setCanvas( canvas );

  },

  //----------------------------------------------------------------------------

  // add the mouse event listeners that will take are of things like changing
  // the cursor over linked domains. If the tooltip library is loaded, this
  // method also adds tips

  _addListeners: function() {

//    console.log( "PfamGraphic._addListeners: got %d areas", this._areasList.size() );
//    this._areasList.each( function( a ) {
//      console.log( "PfamGraphic._addListeners: |%s|: %d - %d", a.label, a.start, a.end ); 
//    } );

//    var tipsDiv = new Element( "div", { id: "tipContentsDivs" } ).hide();
//    this._parent.appendChild( tipsDiv );
//
//    var tipTemplate = new Template( '<p>Tip for #{id}</p>' );
//    this._areasList.each( function( area ) {
//      console.log( "PfamGraphic._addListeners: area: ", area );
//      var tipEl = new Element( "div", { "class": "tipContents",
//                                         id: "tip" + area.metadata.accession } )
//                  .update( tipTemplate.evaluate( { id: area.metadata.id } ) );
//      tipsDiv.appendChild( tipEl ); 
//    } );

    // the offset coordinates of the canvas itself
    var offset = this._canvas.cumulativeOffset();
    var cx = offset[0];
    var cy = offset[1]; 

    // should we add tips ?
    var addTips = ( window.Prototip && this._options.tips );

    // are we inside or outside of an area ?
    var inside = null;

    // add a listener for mouse movements over the canvas

    this._canvas.observe( "mousemove", function( e ) {

      var x = e.pointerX() - cx, 
          y = e.pointerY() - cy,
          activeArea = null;

      // see if we're in an area
      this._areasList.each( function( area ) {
        if ( x > area.coords[0] && x < area.coords[2] &&
             y > area.coords[1] && y < area.coords[3] ) {
          activeArea = area;
          throw $break;
        }  
      } );

      if ( activeArea ) {

        // we were already inside an area
        if ( inside && inside !== activeArea.label ) {

          // we're in a new area
          inside = activeArea.label;
          if ( addTips ) {
            var t1 = new Tip( "dg", inside );
          }

        } else { 

          // we weren't previously in an area
          inside = activeArea.label;

          // change the pointer if there's a link on this area
          if ( activeArea.href ) {
            this._canvas.setStyle( { cursor: "pointer" } );
          }

          // add a tooltip if we can and if we should
          if ( addTips ) {
            var t2 = new Tip( "dg", inside );
          }
        }        

      } else {

        // we aren't inside an area        
        if ( inside ) {
          // we were previously inside an area
          inside = null;
          this._canvas.setStyle( { cursor: "default" } );
          if ( addTips ) {
            $("dg").prototip.remove();
          }
        }
        
      }

    }.bind( this ) );

    //----------------------------------

    // add a listener for when the mouse is moved off the canvas. Clean up
    // the tips and reset the cursor

    this._canvas.observe( "mouseout", function( e ) {
      inside = null;
      this._canvas.setStyle( { cursor: "default" } );
      if ( window.Prototip && $("dg").prototip ) {
        $("dg").prototip.remove();
      }
    }.bind( this ) );

    //----------------------------------

    // watch for clicks on areas with URLs

    this._canvas.observe( "click", function( e ) {
      // console.log( "PfamGraphic._addListeners: click event", e );

      var x = e.pointerX() - cx,
          y = e.pointerY() - cy,
          activeArea = null;

      this._areasList.reverse().each( function( area ) {
        if ( x > area.coords[0] && x < area.coords[2] &&
             y > area.coords[1] && y < area.coords[3] ) {
          activeArea = area;
          throw $break;
        }  
      } );
      
      if ( activeArea && activeArea.href ) {
        window.location = activeArea.href;
      }

    }.bind( this ) );

  }, // end of "_addListeners"

  //----------------------------------------------------------------------------

  // builds an in-memory representation of the markups (lollipops and bridges),
  // but doesn't actually draw them. The data structure built here contains
  // enough information to decide on the size of the <canvas> element and, 
  // once that's built, we can render the elements into it.

  _buildMarkups: function() {

    var heights = { lollipops: { up:   [],
                                 down: [],
                                 markups: [],
                                 downMax: 0,
                                 upMax: 0 },
                    bridges:   { up:   [],
                                 down: [],
                                 markups: [],
                                 downMax: 0,
                                 upMax: 0 } },
        bridgeMarkups   = [],
        ip              = this._imageParams,
        ms              = this._markupSpec;

    // console.log( "PfamGraphic._buildMarkups: assessing lollipops" );

    var orderedMarkups = [];
    this._sequence.markups.each( function( markup ) {

      var start = Math.floor( markup.start );
      if ( start === "NaN" ) {
        throw( "PfamGraphic: ERROR: markup start position is not a number: '" + 
               markup.start + "'" );
      }

      if ( orderedMarkups[markup.start] === undefined ) {
        orderedMarkups[markup.start] = [];
      }

      orderedMarkups[markup.start].push( markup );
    } );
    
    // flatten to get rid of nested arrays and then strip out slots with 
    // "undefined" as a value
    orderedMarkups = orderedMarkups.flatten().compact();

    // walk the markups, in order of start position, and build a map showing where
    // the lollipops are found
    orderedMarkups.each( function( markup ) {

      var start = Math.floor( markup.start );
      if ( start === "NaN" ) {
        throw( "PfamGraphic: ERROR: markup start position is not a number: '" + 
               markup.start + "'" );
      }

      if ( markup.end === undefined ) {
        heights.lollipops.markups.push( markup ); // store as a lollipop
      } else {
        bridgeMarkups.push( markup );   // store as a bridge
        return; // equivalent to "next markup"
      }

      if ( markup.v_align !== undefined &&
           ! ms.valignValues.include( markup.v_align ) ) {
        throw( "PfamGraphic: ERROR: markup 'v_align' value is not valid: '" + 
               markup.v_align + "'" );
      }

      if ( markup.headStyle !== undefined &&
           ! ms.lollipopHeadValues.include( markup.headStyle ) ) {
        throw( "PfamGraphic: ERROR: markup 'headStyle' value is not valid: '" + 
               markup.headStyle + "'" );
      }

      // see if we're drawing on the top or the bottom
      var up = ( markup.v_align === undefined || markup.v_align === "top" );
      // console.log( "PfamGraphic._buildMarkups: up: %s", up );

      var h = up ? heights.lollipops.up : heights.lollipops.down;

      // check for an overlap with another lollipop (which was added previously)
      if ( h[ start - 1 ] !== undefined ||
           h[ start     ] !== undefined ||
           h[ start + 1 ] !== undefined ) {

        var firstLollipopHeight = h.slice( start-1, start+1 ).max();

        h[ start ] = firstLollipopHeight + ip.lollipopToLollipopIncrement;

        // console.log( "PfamGraphic._buildMarkups: duplicate markup at position %d, setting height to %d",
        //   start, h[start] );

      } else { 

        // console.log( "PfamGraphic._buildMarkups: no duplicate markup at position %d; using default height",
        //   start );
        h[start] = ip.defaultMarkupHeight;

      }


      var headSize = ip["headSize" + markup.headStyle.capitalize()];
      // console.log( "PfamGraphic._buildMarkups: head size for '%s': %d",
      //   markup.headStyle, headSize );

      if ( up ) {
        // maximum extent above the sequence line
        heights.lollipops.upMax = Math.max( h[start] + headSize,
                                            heights.lollipops.upMax );
      } else {
        // maximum extent below the sequence line
        heights.lollipops.downMax = Math.max( h[start] + headSize,
                                              heights.lollipops.downMax );
      }

      // console.log( "PfamGraphic.render: max heights for lollipops: up/down: %d / %d",
      //   heights.lollipops.upMax, heights.lollipops.downMax );

    } );

    bridgeMarkups.each( function( bridgeMarkup ) {

      // the hash that stores the bridge parameters
      var bridge = { markup: bridgeMarkup };

      // we need to keep track of the markup for a bridge, but also its
      // calculated height and direction
      heights.bridges.markups.push( bridge );

      var start = Math.floor( bridgeMarkup.start );
      if ( start === "NaN" ) {
        throw( "PfamGraphic: ERROR: bridge start position is not a number: '" + bridgeMarkup.start + "'" );
      }

      var end = Math.floor( bridgeMarkup.end );
      if ( end === "NaN" ) {
        throw( "PfamGraphic: ERROR: bridge end position is not a number: '" + bridgeMarkup.end + "'" );
      }

      // console.log( "PfamGraphic._buildMarkups: checking bridge from %d to %d (+/- 1)",
      //   start, end );

      // see if we're drawing the current bridge on the top or the bottom
      bridge.up = ( bridgeMarkup.v_align === undefined || bridgeMarkup.v_align === "top" );
      var hl = bridge.up ? heights.lollipops.up : heights.lollipops.down,
          hb = bridge.up ? heights.bridges.up   : heights.bridges.down;

      /* console.log( "PfamGraphic._buildMarkups: checking for overlapping bridges" ); */

      // find the maximum height of overlapping bridges
      var maxBridgeHeight = hb.slice( start, end ).flatten().max();
      var bridgeHeight = ip.defaultMarkupHeight;

      // set the height of the current bridge either to the default height or 
      // to the height of the previous highest bridge plus an increment
      if ( maxBridgeHeight === undefined ) {

        // console.log( "PfamGraphic._buildMarkups: no overlapping bridge; setting to default height (%d)",
        //   bridgeHeight );

      } else { 

        if ( hb.slice( start, end ).flatten().include( bridgeHeight ) ) {

          bridgeHeight = maxBridgeHeight + ip.bridgeToBridgeIncrement;

          // console.log( "PfamGraphic._buildMarkups: found overlapping bridge; setting bridge height to %d", 
          //   bridgeHeight );

        // } else { 
        //   console.log( "PfamGraphic._buildMarkups: no bridge at this height; leaving height at %d",
        //     bridgeHeight );
        }

      }

      // console.log( "PfamGraphic._buildMarkups: checking for overlapping lollipops" );

      // find the maximum height of overlapping lollipops (add a buffer to take into 
      // account the width of lollipop heads)
      var maxLollipopHeight = hl.slice( start - 4, end + 4 ).max();

      if ( maxLollipopHeight !== undefined ) {

        // console.log( "PfamGraphic._buildMarkups: found an overlapping lollipop, height %d", 
        //   maxLollipopHeight );

        if ( ( maxLollipopHeight + ip.bridgeToLollipopIncrement ) >= bridgeHeight ) {

          // bridgeHeight += ip.bridgeToLollipopIncrement;
          bridgeHeight = maxLollipopHeight + ip.bridgeToLollipopIncrement;

          // console.log( "PfamGraphic._buildMarkups: found an overlapping lollipop; bridge height reset to %d", 
          //   bridgeHeight );

        }

      // } else {

      //   console.log( "PfamGraphic._buildMarkups: no overlapping lollipop; bridge height unchanged, at %d", 
      //     bridgeHeight );
        
      }

      // check again for a bridge at this height. Increment the current bridge 
      // height if so
      // console.log( "PfamGraphic._buildMarkups: checking again for a bridge at this height (%d)",
      //   bridgeHeight );
      while ( hb.slice( start, end ).flatten().include( bridgeHeight ) ) {

        bridgeHeight += ip.bridgeToBridgeIncrement;
        
        // console.log( "PfamGraphic._buildMarkups: found overlapping bridge; setting bridge height to %d", 
        //   bridgeHeight );

      }

      // console.log( "PfamGraphic._buildMarkups: final bridge: range %d - %d, height %d",
      //   start, end, bridgeHeight );

      // store the calculated height on the bridge object
      bridge.height = bridgeHeight;

      // and set the height on the map
      $R( start, end ).each( function( pos ) { 
        if ( hb[pos] === undefined ) {
          hb[pos] = [];
        }
        hb[pos].push( bridgeHeight );
      } );

      if ( bridge.up ) {
        heights.bridges.upMax = Math.max( bridgeHeight, heights.bridges.upMax );
      } else {
        heights.bridges.downMax = Math.max( bridgeHeight, heights.bridges.downMax );
      }

      // console.log( "PfamGraphic.render: max heights for bridges: %d / %d",
      //   heights.bridges.upMax, heights.bridges.downMax );

    } );

    // finally, push the data structure onto the object, to make it globally
    // accessible
    this._heights = heights;

  }, // end of "_buildMarkups"

  //----------------------------------------------------------------------------

  // draws all of the graphical components

  _draw: function() {

    // draw the sequence
    var seqArea = this._drawSequence();

    // draw the briges
    this._heights.bridges.markups.each( function( bridge ) {
      if ( bridge.display !== undefined && ! bridge.display ) {
        return;
      }
      this._drawBridge( bridge );
    }.bind( this ) );

    // draw the lollipops after the bridges, so that the heads appear on top of
    // any overlapping bridges
    this._heights.lollipops.markups.reverse().each( function( lollipop ) {
      if ( lollipop.display !== undefined && ! lollipop.display ) {
        return;
      }
      this._drawLollipop( lollipop );
    }.bind( this ) );

    // draw the regions
    this._sequence.regions.each( function( region ) {
      if ( region.display !== undefined && ! region.display ) {
        return;
      }
      this._drawRegion( region );
    }.bind( this ) );

    // draw the motifs
    this._sequence.motifs.each( function( motif ) {
      if ( motif.display !== undefined && ! motif.display ) {
        return;
      }
      this._drawMotif( motif );
    }.bind( this ) );

    // add the <area> details for the sequence last
    this._areasList.push( seqArea ); 

  },

  //----------------------------------------------------------------------------

  // draws the basic ribbon, representing the sequence. Returns data for an
  // <area> tag, so that we can add it to the list of areas at the appropriate 
  // end of that list

  _drawSequence: function() {

    // calculate "offsets" for the top and bottom of the sequence line and 
    // draw the rectangles (which make up the line) relative to that. Store the
    // two value in the object, so that we can use them when drawing markups too
    this._topOffset = Math.floor( this._baseline - ( this._seqHeight / 2 ) );
    this._botOffset = Math.floor( this._baseline + ( this._seqHeight / 2 * 3 ) );

    // console.log( "PfamGraphic._drawSequence: baseline: %d", this._baseline );
    // console.log( "PfamGraphic._drawSequence: calculated top offset as %d", this._topOffset );
    // console.log( "PfamGraphic._drawSequence: calculated bottom offset as %d", this._botOffset );

    // console.log( "PfamGraphic._drawSequence: (x1, y1), (w, h): (%d, %d), (%d, %d)",
    //                        1,                 this._topOffset,
    //                        this._imageWidth,  this._seqStep );
    this._context.fillStyle = "#dddddd";
    this._context.fillRect( 1,                this._topOffset,
                             this._imageWidth, this._seqStep );

    // console.log( "PfamGraphic._drawSequence: (x1, y1), (w, h): (%d, %d), (%d, %d)",
    //                        1,                 offset + this._seqStep,
    //                        this._imageWidth,  this._seqStep );
    this._context.fillStyle = "#bbbbbb";
    this._context.fillRect( 1,                this._topOffset + this._seqStep,
                             this._imageWidth, this._seqStep );

    // console.log( "PfamGraphic._drawSequence: (x1, y1), (w, h): (%d, %d), (%d, %d)",
    //                        1,                 this._topOffset + ( this._seqStep * 2 ),
    //                        this._imageWidth,  this._seqStep * 3 );
    this._context.fillStyle = "#cccccc";
    this._context.fillRect( 1,                this._topOffset + ( this._seqStep * 2 ),
                             this._imageWidth, this._seqStep * 3 );

    // add an area
    return { label:  "sequence", // TODO make this more informative...
              text:   "sequence",
              coords: [ 0, this._topOffset, 
              this._imageWidth, this._topOffset + this._seqStep * 5 ] };
  },

  //----------------------------------------------------------------------------

  // draws an individual lollipop

  _drawLollipop: function( markup ) {
    // console.log( "PfamGraphic._drawLollipop: start" );

    var start = markup.start,

        up = markup.v_align === undefined || markup.v_align === "top", 

        x1 = Math.floor( start * this._imageParams.xscale ) + 0.5,
        y1,
        y2;
    if ( up ) {
      // console.log( "PfamGraphic._drawLollipop: drawing lollipop on top at %d", start );
      y1 = Math.round( this._topOffset );
      y2 = Math.floor( y1 - this._heights.lollipops.up[start] + ( this._baseline - this._topOffset ) + 1 );
    } else { 
      // console.log( "PfamGraphic._drawLollipop: drawing lollipop on bottom at %d", start );
      y1 = Math.round( this._botOffset - 1 );
      y2 = Math.ceil( y1 + this._heights.lollipops.down[start] - ( this._botOffset - this._baseline ) + 1 );
    }

    // console.log( "PfamGraphic._drawLollipop: (x1, y1), (x1, y2): (%d, %d), (%d, %d)",
    //              x1, y1, x1, y2 );

    this._context.beginPath();
    this._context.moveTo( x1, y1 );
    this._context.lineTo( x1, y2 );

    this._context.strokeStyle = markup.lineColour || "#000000";  
    this._context.stroke();
    this._context.closePath();
    
    // add an <area> for the stick (the head drawing function should add an 
    // <area> for the head)

    // build a label for this lollipop
    var label = this._templates.lollipop.evaluate( { label: markup.label,
                                                      pos:   markup.start } );

    this._areasList.push( { label:    label,
                             metadata: markup.metadata,
                             start:    start,
                             coords:   [ x1-1, y1-1, x1+1, y2+1 ] } );

    // add the head
    if ( markup.headStyle ) {
      this._drawLollipopHead( x1, y2, start, up, markup.headStyle, markup.colour, label, markup.metadata );
    }

    // console.log( "PfamGraphic._drawLollipop: end" );
  },
 
  //----------------------------------------------------------------------------

  // draws the head of a lollipop

  _drawLollipopHead: function( x, y, start, up, style, colour, label, metadata ) {
    // console.log( "PfamGraphic._drawLollipopHead: starting to draw head |%s|", style );

    var r;
    var d;
    switch ( style ) {

      case "circle":
        r = this._imageParams.headSizeCircle;
        // console.log( "PfamGraphic._drawLollipopHead: drawing circle" );
        this._context.beginPath();
        this._context.arc( x, y, r, 0, (Math.PI * 2), "true" );
        this._context.fillStyle = colour || "red";
        this._context.fill();
        this._areasList.push( { label:    label,
                                 metadata: metadata,
                                 shape:    "circle",
                                 start:    start,
                                 coords:   [ x, y, r ] } );
        break;

      case "square":
        d = this._imageParams.headSizeSquare / 2;
        // console.log( "PfamGraphic._drawLollipopHead: drawing square, edge |%d|, centred at %d x %d",
        //   this._imageParams.headSize.square, x, y );
        this._context.beginPath();
        this._context.moveTo( (x - d), (y - d) );
        this._context.lineTo( (x - d), (y + d) );
        this._context.lineTo( x + d, y + d );
        this._context.lineTo( x + d, y - d );
        this._context.lineTo( x - d, y - d );
        this._context.closePath();
        this._context.fillStyle = colour || "rgb(100, 200, 9)";
        this._context.fill();
        this._areasList.push( { label:    label,
                                 metadata: metadata,
                                 start:    start,
                                 coords:   [ x - d, y - d, x + d, y + d ] } );
        break;

      case "diamond":
        d = this._imageParams.headSizeDiamond;
        // console.log( "PfamGraphic._drawLollipopHead: drawing diamond, extent |%d|, centred %d x %d",
        //   d, x, y );
        this._context.beginPath();
        this._context.moveTo( x - d, y );
        this._context.lineTo( x,     y + d );
        this._context.lineTo( x + d, y );
        this._context.lineTo( x,     y - d );
        this._context.lineTo( x - d, y );
        this._context.closePath();
        this._context.fillStyle = colour || "rgb(100, 200, 9)";
        this._context.fill();
        this._areasList.push( { label:    label,
                                 metadata: metadata,
                                 shape:    "poly",
                                 start:    start,
                                 coords:   [ x - d, y,
                                             x,     y + d,
                                             x + d, y,
                                             x,     y - d,
                                             x - d, y ] } );
        break;

      case "line":
        d = this._imageParams.headSizeLine;
        // console.log( "PfamGraphic._drawLollipopHead: drawing line, length |%d|, centred %d x %d", 
        //   d, x, y );
        this._context.beginPath();
        this._context.moveTo( x, y - d );
        this._context.lineTo( x, y + d );
        this._context.closePath();
        this._context.strokeStyle = colour || "rgb(50, 40, 255)";
        this._context.stroke();
        this._areasList.push( { label:    label,
                                 metadata: metadata,
                                 start:    start,
                                 coords:   [ x - 1, y - d - 1,
                                             x + 1, y + d + 1 ] } );
        break;

      case "arrow":
        d = this._imageParams.headSizeArrow;
        // console.log( "PfamGraphic._drawLollipopHead: drawing arrow, extent |%d|, centred %d x %d", 
        //   d, x, y );
        this._context.beginPath();

        var coords;
        if ( up ) {
          this._context.moveTo( x - 3, this._topOffset - d );
          this._context.lineTo( x,     this._topOffset     );
          this._context.lineTo( x + 3, this._topOffset - d );

          coords = [ x - 4, this._topOffset - d,
                     x,     this._topOffset + 1,
                     x + 4, this._topOffset - d,
                     x + 3, this._topOffset - 1 - d,
                     x,     this._topOffset - 1,
                     x - 3, this._topOffset - 1 - d ];

        } else { 
          this._context.moveTo( x - 3, this._botOffset + d );
          this._context.lineTo( x,     this._botOffset     );
          this._context.lineTo( x + 3, this._botOffset + d );

          coords = [ x - 4, this._botOffset + d,
                     x,     this._botOffset - 1,
                     x + 4, this._botOffset + d,
                     x + 3, this._botOffset + 1 + d,
                     x,     this._botOffset + 1,
                     x - 3, this._botOffset + 1 + d ];
        }

        this._areasList.push( { label:    label,
                                 metadata: metadata,
                                 start:    start,
                                 shape:    "poly",
                                 coords:   coords } );
  
        this._context.strokeStyle = colour || "rgb(50, 40, 255)";
        this._context.stroke();

        break;
    }

    // console.log( "PfamGraphic._drawLollipopHead: done" );
  },

  //----------------------------------------------------------------------------

  // draws a bridge

  _drawBridge: function( bridge ) {
    // console.log( "PfamGraphic._drawBridge: start" );
  
    var start  = bridge.markup.start,
        end    = bridge.markup.end,
        height = bridge.height,
        up     = bridge.up,

        colour = "#000000",
        
        x1 = Math.floor( start * this._imageParams.xscale ) + 0.5,
        x2 = Math.floor( end * this._imageParams.xscale ) + 0.5,
        y1 = Math.round( this._baseline ),
        y2,
        label;

    if ( up ) {
      // console.log( "PfamGraphic._drawBridge: drawing bridge on top at position %d", start );
      y2 = Math.ceil( y1 - height ) - 0.5;
    } else {
      // console.log( "PfamGraphic._drawBridge: drawing bridge on bottom at position %d", start );
      y2 = Math.floor( y1 + height ) + 0.5;
    }

    // console.log( "PfamGraphic._drawBridge: (x1, y1), (x2, y2): (%d, %d), (%d, %d)",
    //              x1, y1, x2, y2 );

    this._context.beginPath();
    this._context.moveTo( x1, y1 );
    this._context.lineTo( x1, y2 );
    this._context.lineTo( x2, y2 );
    this._context.lineTo( x2, y1 );

    if ( bridge.markup.colour.match( "^\\#[0-9A-Fa-f]{6}$" ) ) {
      colour = bridge.markup.colour;
      // console.log( "PfamGraphic._drawBridge: using user-defined colour '%s'", colour );
    }
    this._context.strokeStyle = colour;  
    this._context.stroke();
    this._context.closePath();
    
    // build a label for the bridge
    label = this._templates.bridge.evaluate( { label: bridge.markup.label,
                                                start: start,
                                                end:   end } );

    // add <area> tags for each of the legs and the horizontal
    this._areasList.push( { label:  label, 
                             text:   bridge.markup.label,
                             start:  start,
                             end:    end,
                             coords: [ x1-1, y1-1, x1+1, y2+1 ] } );
    this._areasList.push( { label:  label, 
                             text:   bridge.markup.label,
                             start:  start,
                             end:    end,
                             coords: [ x1-1, y2-1, x2+1, y2+1 ] } );
    this._areasList.push( { label:  label, 
                             text:   bridge.markup.label,
                             start:  start,
                             end:    end,
                             coords: [ x2-1, y2-1, x2+1, y1+1 ] } );

    // console.log( "PfamGraphic._drawBridge: end" );
  },
 
  //----------------------------------------------------------------------------

  // draws a region (most commonly a domain)

  _drawRegion: function( region ) {
    // console.log( "PfamGraphic._drawRegion: drawing region..." );

    if ( ! this._markupSpec.regionEndValues.include( region.startStyle ) ) {
      throw( "PfamGraphic: ERROR: region start style is not valid: '" + region.startStyle + "'" );
    }

    if ( ! this._markupSpec.regionEndValues.include( region.endStyle ) ) {
      throw( "PfamGraphic: ERROR: region end style is not valid: '" + region.endStyle + "'" );
    }

    // calculate dimensions for the inner shape
    var height = Math.floor( this._regionHeight ) - 2,
        radius = Math.round( height / 2 ),
        arrow  = radius,
        width = ( region.end - region.start + 1 ) * this._imageParams.xscale - 2,

        x = Math.max( 1, Math.floor( region.start * this._imageParams.xscale ) + 1.5 ),
        y = Math.floor( this._baseline - radius ) + 0.5,

        regionParams = {
          x: x, 
          y: y, 
          w: width, 
          h: height,
          r: radius,
          a: arrow,
          s: region.startStyle,
          e: region.endStyle
        };

    // console.log( "PfamGraphic._drawRegion: inner: (x, y), h, w: (%d, %d), %d, %d",
    //   x, y, height, width );

    //----------------------------------

    // the inner-most is filled, with a colour gradient running from white to 
    // dark to light colour as y increases. First draw the shell, then fill it
    this._buildRegionPath( regionParams );

    // fill the path with a gradient
    var gradient = this._context.createLinearGradient( x, y, x, y + height );

    gradient.addColorStop( 0, "#ffffff" );
    gradient.addColorStop( 0.5, region.colour );
    gradient.addColorStop( 0.7, region.colour );
    gradient.addColorStop( 1, "#ffffff" ); // TODO make this a bit darker

    this._context.fillStyle = gradient;
    this._context.fill();

    //----------------------------------

    // now step out and draw a solid line round the gradient filled shape

    // calculate dimensions for this outer shape
    height += 2;
    radius  = Math.round( height / 2 );
    arrow   = radius;
    width  += 2;
    x      -= 1;
    y       = Math.floor( this._baseline - radius ) + 0.5;

    // console.log( "PfamGraphic._drawRegion: outer: (x, y), h, w: (%d, %d), %d, %d",
    //   x, y, height, width );

    this._buildRegionPath( { x: x, 
                              y: y, 
                              w: width, 
                              h: height,
                              r: radius,
                              a: arrow,
                              s: region.startStyle,
                              e: region.endStyle } );

    this._context.strokeStyle = region.colour;
    this._context.stroke();

    //----------------------------------

    // build a label for the <area>
    // add the transparent overlay to show the limits of the alignment region
    var label, startArea, endArea;
    if ( region.aliStart !== undefined &&
         region.aliEnd   !== undefined ) {
      var areas = this._drawEnvelope( region, radius, height );
      startArea = areas[0];
      endArea   = areas[1];

      // use a template that includes the envelope details
      label = this._templates.region.evaluate( { label: region.text,
                                                  start: region.start,
                                                  end:   region.end } );
    } else {
      // there's no envelope given, so use a template that describes the whole
      // region
      label = this._templates.alignment.evaluate( { label:    region.text,
                                                     start:    region.start,
                                                     end:      region.end,
                                                     aliStart: region.aliStart,
                                                     aliEnd:   region.aliEnd } );
    }

    //----------------------------------

    // add the text label
    if ( this._options.labels ) {
      this._drawText( x, this._baseline, width, region.text );
    }

    //----------------------------------

    // build a URL
    var url;
    if ( region.acc !== undefined ) {
      url = this._imageParams.regionUrl + region.acc;
    } else if ( region.id !== undefined ) {
      url = this._imageParams.regionUrl + region.id;
    } else if ( region.text !== undefined ) {
      url = this._imageParams.regionUrl + region.text;
    }

    // add the area(s)

    // first the starting envelope region
    if ( startArea ) {
      this._areasList.push( startArea );
    }

    // then the main region area
    var area = { label:    label,
                 text:     region.text,
                 start:    region.start,
                 end:      region.end,
                 aliStart: region.aliStart,
                 aliEnd:   region.aliEnd,
                 href:     url || '',
                 coords:   [ x, y, x+width, y+height ] };
                        
    this._areasList.push( area );

    // also store this hashed on the domain name
    this._areasHash.set( region.text + "_" + region.aliStart + "_" + region.aliEnd, area); 

    // and finally the end envelope region
    if ( endArea ) {
      this._areasList.push( endArea );
    }

    // console.log( "PfamGraphic._drawRegion: done" );
  }, // end of "_drawRegion"

  //----------------------------------------------------------------------------

  // draws a motif

  _drawMotif: function( motif ) {

    // console.log( "PfamGraphic._drawMotif: motif: ", motif );

    // work out the dimensions

    var height = this._imageParams.motifHeight,
        width  = Math.floor( ( motif.end - motif.start + 1 ) * this._imageParams.xscale ),
        x = Math.max( 1, Math.floor( motif.start * this._imageParams.xscale ) ),
        y = Math.floor( this._baseline - Math.round( height / 2 ) );

    // console.log( "PfamGraphic._drawMotif: (x, y), h, w: (%d, %d), %d, %d",
    //   x, y, height, width );

    // decide what we're drawing, based on the number of colours we're given
    if ( motif.colour instanceof Array ) {

       // Pfam-B

      // first, make sure we have a sensible number of colours to play with...
      if ( motif.colour.length !== 3 ) {
        throw( "PfamGraphic: ERROR: motifs must have either one or three colours" );
      }

      // convert the colours from hex strings into "rgba()" values
      var colours = [];
  
      var getRGBColour = this._getRGBColour.bind( this ),
          ip           = this._imageParams;
  
      motif.colour.each( function( colour ) {
        var rgbColour = getRGBColour( colour );
        colours.push( { rgb:  "rgb("  + rgbColour.join(",") + ")",
                        rgba: "rgba(" + rgbColour.join(",") + "," + ip.motifOpacity + ")" } );
      } );
  
      // draw the three stripes
      var step   = Math.round( height / 3 );
      for ( var i = 0; i < 3; i = i + 1 ) {
  
        this._context.fillStyle = colours[i].rgba;
        this._context.fillRect( x, y + ( step * i ), width, step );
  
      }

    } else {

      // regular "motif"

      // convert the colour from a hex string into an "rgba()" value
      var colour = this._getRGBColour( motif.colour );
      var rgb  = "rgb(" + colour.join(",") + ")";
      var rgba = "rgba(" + colour.join(",") + "," + this._imageParams.motifOpacity + ")";
  
      // draw the rectangle
      this._context.fillStyle = rgba;
      this._context.fillRect( x, y, width, parseInt( height, 10 ) + 1 );
  
    }

    // build a label for the <area>
    var label = this._templates.motif.evaluate( { label: motif.text,
                                                  start: motif.aliStart,
                                                  end:   motif.aliEnd } );

    // build a URL
    var url;
    if ( motif.acc !== undefined ) {
      url = this._imageParams.regionUrl + motif.acc;
    } else if ( motif.id !== undefined ) {
      url = this._imageParams.regionUrl + motif.id;
    } else if ( motif.text !== undefined ) {
      url = this._imageParams.regionUrl + motif.text;
    }

    // add the area
    var area = { label:  label,
                 text:   motif.text,
                 start:  motif.aliStart,
                 end:    motif.aliEnd,
                 href:   url || '',
                 coords: [ x, y, x + width, y + height ] };
    this._areasList.push( area );

    this._areasHash.set( motif.text + "_" + motif.aliStart + "_" + motif.aliEnd, area );

    // console.log( "PfamGraphic._drawMotif: done" );
  }, // end of "_drawMotif"

  //----------------------------------------------------------------------------

  // builds the path for constructing regions. The path can be used either as
  // an outline for filling or stroking.

  _buildRegionPath: function( params ) {

    this._context.beginPath();

    // console.log( "PfamGraphic._buildRegionPath: drawing left end" );
    switch ( params.s ) {
      case "curved":
        this._context.moveTo( params.x + params.r, params.y );
        this._drawLeftRounded( params.x, params.y, params.r, params.h );
        break;
      case "jagged":
        this._context.moveTo( params.x, params.y );
        this._drawJagged( params.x, params.y, params.h, true );
        break;
      case "straight":
        this._context.moveTo( params.x, params.y );
        this._context.lineTo( params.x, params.y + params.h );
        break;
      case "arrow":
        this._context.moveTo( params.x + params.a, params.y );
        this._drawLeftArrow( params.x, params.y, params.a, params.h );
        break;
    }

    // bottom line and right hand edge 
    // console.log( "PfamGraphic._buildRegionPath: drawing bottom line and right end" );
    switch ( params.e ) {
      case "curved":
        this._context.lineTo( params.x + params.w - params.r, params.y + params.h );
        this._drawRightRounded( params.x, params.y, params.r, params.h, params.w );
        break;
      case "jagged":
        this._context.lineTo( params.x + params.w, params.y + params.h );
        this._drawJagged( params.x + params.w, params.y + params.h, params.h, false );
        break;
      case "straight":
        this._context.lineTo( params.x + params.w, params.y + params.h );
        this._context.lineTo( params.x + params.w, params.y );
        break;
      case "arrow":
        this._context.lineTo( params.x + params.w - params.a, params.y + params.h );
        this._drawRightArrow( params.x + params.w - params.a, params.y + params.h, params.a, params.h );
        break;
    }

    // top horizontal line
    // console.log( "PfamGraphic._buildRegionPath: drawing top line" );
    if ( params.s === "curved" || 
         params.s === "arrow" ) {
      this._context.lineTo( params.x + params.r, params.y );
    } else {
      this._context.lineTo( params.x, params.y );
    }
    
    this._context.closePath();
  }, // end of "_buildRegionPath"

  //----------------------------------------------------------------------------

  // draws semi-transparent overlays to represent the envelope regions around
  // the core alignment
  
  _drawEnvelope: function( region, radius, height ) {
    // console.log( "PfamGraphic._drawEnvelope: adding envelope overlay" );

    // TODO handle the case where there's an aliStart but no aliEnd given

    // make sure the endpoints are sensible
    if ( region.start > region.aliStart ) {
      throw( "PfamGraphic: ERROR: regions must have start <= aliStart (" + region.start + " is > " + region.aliStart + ")" );
    }

    if ( region.end < region.aliEnd ) {
      throw( "PfamGraphic: ERROR: regions must have end >= aliEnd (" + region.end + " is < " + region.aliEnd + ")" );
    }

    //----------------------------------

    var y  = this._baseline - radius,
        xs = this._imageParams.xscale,
        l = { x: Math.floor( region.start * xs ),
              y: Math.floor( y - 1 ) + 1,
              w: ( region.aliStart * xs ) - ( region.start * xs ) + 1,
              h: height + 1 },
        r = { x: Math.floor( region.aliEnd * xs ),
              y: Math.floor( y - 1 ) + 1,
              w: ( region.end * xs ) - ( region.aliEnd * xs ) + 2,
              h: height + 1 };

    // a grey overlay; more visible
    /* var fillStyle = "rgba(255,255,255," + this._imageParams.envOpacity + ")"; */

    // an overlay that's the same colour as the domain
    // var rgb = this._getRGBColour( region.colour );
    // var fillStyle = "rgba(" + rgb.r + "," + rgb.g + "," + rgb.b + "," +
    //   this._imageParams.envOpacity + ")";

    // clip the envelope regions to the existing canvas content, so that we 
    // restrict the shading to, for example, the true edges of the arrow head
    this._context.globalCompositeOperation = "source-atop";

    // the intended fillStyle
    var fillStyle = "rgba(255,255,255," + this._imageParams.envOpacity + ")";
    this._context.fillStyle = fillStyle;

    // console.log( "PfamGraphic._drawEnvelope: left region: (x, y, w, h): (%d, %d, %d, %d)",
    //   l.x, l.y, l.w, l.h );
    this._context.fillRect( l.x, l.y, l.w, l.h );

    // console.log( "PfamGraphic._drawEnvelope: left region: (x, y, w, h): (%d, %d, %d, %d)",
    //   r.x, r.y, r.w, r.h );
    this._context.fillRect( r.x, r.y, r.w, r.h );

    // reset the compositing rule
    this._context.globalCompositeOperation = "source-over";

    //----------------------------------

    // add two <area> tags for the envelope; one at the start, one at the end

    // build a label for the start <area>
    var label = this._templates.envelope.evaluate( { label: region.text,
                                                      start: region.start,
                                                      end:   region.aliStart } );

    // build a URL
    var url = this._imageParams.regionUrl + region.text;

    // add the area
    var startArea = { label:  label,
                      text:   region.text,
                      href:   url,
                      start:  region.start,
                      end:    region.aliStart,
                      coords: [ l.x, l.y, l.x + l.w, l.y + l.h ] };

    // build the end <area>
    label = this._templates.envelope.evaluate( { label: region.text,
                                                  start: region.aliEnd,
                                                  end:   region.end } );

    url = this._imageParams.regionUrl + region.text;

    var endArea = { label:  label,
                     text:   region.text,
                     href:   url,
                     start:  region.aliEnd,
                     end:    region.end,
                     coords: [ r.x, r.y, r.x + r.w, r.y + r.h ] };

    return [ startArea, endArea ];
  }, // end of "_drawEnvelope"
  
  //----------------------------------------------------------------------------

  // adds the text label to the domain, if it will fit nicely inside the shape

  _drawText: function( x, midpoint, regionWidth, text ) {

    this._context.save();

    // set up the font
    this._context.font         = "bold 1em 'optimer'";
    this._context.textAlign    = "center";
    this._context.textBaseline = "middle";

    // calculate the width of the text to be rendered
    var metrics = this._context.measureText( text );

    // console.log( "PfamGraphic._drawText: textBaseline: %d", this._context.textBaseline );

    // pad the text a little and then compare that to the width of the region,
    // so that we can assess whether it's going to fit inside the region when
    // rendered
    /* var paddedTextWidth = metrics.width + 2 * this._imageParams.labelPadding; */
    var paddedTextWidth = metrics.width + Math.round( this._regionHeight / 3 );

    // console.log( "PfamGraphic._drawText: padded text width: %d, region width: %d",
    //   paddedTextWidth, regionWidth );

    if ( paddedTextWidth > regionWidth ) {
      // console.log( "PfamGraphic._drawText: text is wider than region; not adding" );
      return;
    }

    var textX = x + ( regionWidth / 2 );
    // console.log( "PfamGraphic._drawText: region X, midpoint: (%d, %d); textX: %d", 
    //   x, midpoint, textX );

    // stroke the outline in white...
    this._context.lineWidth   = 2;
    this._context.strokeStyle = "#eeeeee";
    this._context.strokeText( text, textX, midpoint );

    // ... and then fill in black
    this._context.fillStyle = "#000000";
    this._context.fillText( text, textX, midpoint );

    this._context.restore();

  },

  //----------------------------------------------------------------------------

  // draws the left-hand end of region with a curved end

  _drawLeftRounded: function( x, y, radius, height ) {
    this._context.quadraticCurveTo( x, y, x, y + radius );
    this._context.quadraticCurveTo( x, y + height, x + radius, y + height );
  },

  //----------------------------------------------------------------------------

  // draws the right-hand end of region with a curved end

  _drawRightRounded: function( x, y, radius, height, width ) {
    this._context.quadraticCurveTo( x + width, y + height, x + width, y + radius );
    this._context.quadraticCurveTo( x + width, y , x + width - radius, y );
  },

  //----------------------------------------------------------------------------

  // generates a jagged end, either left or right, depending on the value of 
  // the "left" argument (either true or false).

  _drawJagged: function( x, y, height, left ) {

    // make sure we have an even number of steps
    var steps = parseInt( this._imageParams.largeJaggedSteps, 10 );
    steps += steps % 2;

    // get the list of Y-coords
    var yShifts = this._getSteps( height, steps );

    // the step size, in pixels. This is used when stepping on X
    var step = height / steps;

    // console.log( "PfamGraphic._drawJagged: (x, y), height, steps, left: (%d, %d), %d, %d, %s",
    //   x, y, height, steps, left );

    for ( var i = 0; i < yShifts.length; i = i + 1 ) {
      // odd; outer vertices
      if ( i % 2 !== 0 ) {
        if ( left ) {
          this._context.lineTo( x, y + yShifts[i] );
        } else {
          this._context.lineTo( x, y - yShifts[i] );
        }
      }
      // even; inner vertices
      else {
        if ( left ) {
          this._context.lineTo( x + step, y + yShifts[i] );
        } else {
          this._context.lineTo( x - step, y - yShifts[i] );
        }
      }
    }

    // close the path
    if ( left ) {
      this._context.lineTo( x, y + height );
    } else {
      this._context.lineTo( x, y - height );
    }
  },

  //----------------------------------------------------------------------------

  // generates a list of Y-axis coordinates for a jagged end. The list will 
  // be cached for this combination of height and number of steps.

  _getSteps: function( height, steps ) {

    var cacheKey = "shifts_" + height + "_" + steps;
    var list = this._cache[cacheKey];

    if ( list === undefined ) {

      // the "period" of the step, in pixels
      var step = height / steps;

      // walk out from the mid-line and add Y-axis coords to an array
      var yShifts = [];
      for ( var i = 0; i < ( steps / 2 ); i = i + 1 ) {
        yShifts.push( ( height / 2 ) - ( i * step ) );
        yShifts.push( ( height / 2 ) + ( i * step ) );
      }

      // uniquify and (numerically) sort the list of Y-coords
      list = yShifts.uniq().sort( function (a, b) { return a - b; } );

      // cache the list for later
      this._cache[cacheKey] = list;
    }

    return list;
  },

  //----------------------------------------------------------------------------

  // draws the left-hand end of a region as an arrow head

  _drawLeftArrow: function( x, y, arrow, height ) {
    this._context.lineTo( x, y + arrow );
    this._context.lineTo( x + arrow, y + height );
  },

  //----------------------------------------------------------------------------

  // draws the right-hand end of a region as an arrow head

  _drawRightArrow: function( x, y, arrow, height ) {
    this._context.lineTo( x + arrow, y - height + arrow );
    this._context.lineTo( x, y - height );
  },

  //----------------------------------------------------------------------------

  // converts a hex string (e.g. "#07874f") into an RGB triplet (e.g. [ 7, 135, 79 ]).
  // RGB values are in the range 0 - 255. Returns an array containing the RGB values,
  // which are also available as { r: RED, g: GREEN, b: BLUE }.
  //
  // (these two complementary methods were taken originally from
  // "http://www.linuxtopia.org/online_books/javascript_guides/javascript_faq/rgbtohex.htm")

  _getRGBColour: function( hexString ) {

    var matches = /^#?([A-F0-9]{6})$/i.exec( hexString );
    if ( matches === null ) {
      throw( "PfamGraphic: ERROR: not a valid hex colour ('" + hexString + "')" );
    }
    var h = matches[1],
        r = parseInt( h.substring( 0, 2 ), 16 ),
        g = parseInt( h.substring( 2, 4 ), 16 ),
        b = parseInt( h.substring( 4, 6 ), 16 ),
        rgb = [ r, g, b ];

    rgb.r = r;
    rgb.g = g;
    rgb.b = b;

    return rgb;
  },

  //----------------------------------

  // converts an RGB triplet (e.g. 7, 135, 79) into a hex colour (e.g. #07874f".
  // The RGB values must be in the range 0 - 255, but they can be given as
  // a hash (e.g. { r: 7, g: 135, b: 79 }), an array (e.g. [7, 135, 79]) or as
  // individual values (e.g. 7, 135, 79). The hex value is returned with a leading
  // hash (#). RGB values are silently clamped, individually to the range 0 255
  // and are rounded to the nearest integer value.

  _getHexColour: function( red, green, blue ) {

    var r, g, b;

    if ( red.shift ) {
      // console.log( "PfamGraphic._getHexColour: got an array" );
      r = red[0];
      g = red[1];
      b = red[2];
    } else if ( red.r !== undefined &&
                red.g !== undefined &&
                red.b !== undefined ) {
      // console.log( "PfamGraphic._getHexColour: got a hash" );
      r = red.r;
      g = red.g;
      b = red.b;
    } else {
      // console.log( "PfamGraphic._getHexColour: looking for individual values" );
      r = red;
      g = green;
      b = blue;
    }

    var rgbColour = [ r, g, b ].collect( function( x ) {
      if ( x === undefined ) {
        throw( "PfamGraphic: ERROR: need all three RGB colour values" );
      }
      if ( isNaN( x ) ) {
        throw( "PfamGraphic: ERROR: failed to get a valid RGB colour triplet" );
      }
      x = parseInt( x, 10 );
      x = Math.max( 0, x );
      x = Math.min( x, 255 );
      return x;
    } );

    var hex = rgbColour.collect( function( x ) {
      return "0123456789abcdef".charAt( ( x - x % 16 ) / 16 ) +
             "0123456789abcdef".charAt( x % 16 );
    } ).join( "" );

    // console.log( "PfamGraphic._getHexColour: converted (%d, %d, %d) to '%s'",
    //   r, g, b, hex );

    return "#" + hex;
  }

  //----------------------------------------------------------------------------

} );

