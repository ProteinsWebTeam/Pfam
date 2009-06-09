
//------------------------------------------------------------------------------
//- Preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $R, $w, Class, console, Element, G_vmlCanvasManager Template */

//------------------------------------------------------------------------------
//- Class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

var PfamGraphic = Class.create( {

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function( parent, sequence ) {
    // console.log( "PfamGraphic.initialize: setting up a new object" );

    // defaults for the image parameters
    this._imageParams = {

      // lollipop head dimensions
      headSize: { circle:  3,
                  square:  6,
                  diamond: 4,
                  arrow:   3,
                  line:    3 },
      
      // parameters for adjusting the edges of domains
      defaultMarkupHeight:         20, // default height for a lollipop
      lollipopToLollipopIncrement: 7,  // step up each lollipop X pixels from the previous one
      bridgeToBridgeIncrement:     2,  // step up each bridge X pixels from the last bridge
      bridgeToLollipopIncrement:   5,  // step up each bridge X pixels from the last lollipop
      largeJaggedSteps:            6,  // number of steps on jagged edge (must be an even integer)

      // TODO make this configurable
      font:     "sans",
      fontSize: 9,

      // templates for labelling the various components
      templates: {
        lollipop:  new Template( "#{label}, residue #{pos}" ),
        bridge:    new Template( "#{label}, from #{start} to #{end}" ),
        feature:   new Template( "#{label}, from #{start} to #{end}" ),
        motif:     new Template( "#{label}, from #{start} to #{end}" ),
        alignment: new Template( "#{label}, alignment region from #{start} to #{end}" ),
        envelope:  new Template( "#{label}, envelope region from #{start} to #{end}" )
      },

      // URL to link from a feature
      featureUrl: "http://pfam.sanger.ac.uk/family/",

      // general image parameters
      featureHeight: 20,   // the height of a feature
      motifHeight:   14,   // the height of a motif
      motifOpacity:  0.6,  // the height of a motif
      labelPadding:  3,    // padding for the text label on a feature
      xscale:        0.5,  // xscale pixels per residue
      yscale:        1,    // not currently used
      envOpacity:    0.6   // opacity of the envelope regions
    };

    // general options, specified as part of the "sequence"
    this._options = {
      imageMap: true,  // add the image map ?
      labels:   true   // add the text labels to features ?
    };

    // specification of various allowed values in the input
    this._markupSpec = {
      valignValues:       $w( "top bottom" ),
      linesStyleValues:   $w( "mixed bold dashed" ),
      lollipopHeadValues: $w( "diamond circle square arrow line" ),
      featureEndValues:   $w( "curved straight jagged arrow" )
    };

    // somewhere to put <area> definitions for the domains and markups
    this._heights = [];
    this._areas   = [];

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

  // returns the image parameters

  getImageParams: function() {
    return this._imageParams;
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

    // check the "feature", "markups" and "motifs" sections of the sequence
    if ( sequence.features !== undefined ) {
      if ( typeof sequence.features !== "object" ) {
        throw( "PfamGraphic: ERROR: 'features' must be a valid object" );
      }
    } else {
      // add an empty "features" object, to keep later code happy
      sequence.features = [];
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

    // everything passes. Stash it
    this._sequence = sequence;

    // get the scale out of the image parameters
    // console.log( "PfamGraphic.setSequence: x_scale: / y_scale: %d / %d", 
    //   this._imageParams.xscale, this._imageParams.yscale );
    
    // scale the length of the sequence and the weight of the domain image
    this._imageWidth = this._sequence.length * this._imageParams.xscale;
    this._featureHeight = this._imageParams.featureHeight;
    // TODO start taking notice of yscale

    // console.log( "PfamGraphic.setSequence: image width, domain height: %d, %d", 
    //   this._imageWidth, this._featureHeight );

    // set the height of the sequence line and the length of one "step" across
    // that line. The line is draw as a light band, a darker band and another
    // lighter one, to give the effect of a reflection

    // set the sequence line to one sixth the height of the domains
    this._seqHeight = Math.round( this._featureHeight / 6 );

    // we want 5 steps across the sequence line
    this._seqStep   = Math.round( this._seqHeight / 5 );    

    // console.log( "PfamGraphic.setSequence: seqHeight / seqStep: %d / %d", 
    //   this._seqHeight, this._seqStep );
  },

  getSequence: function() {
    return this._sequence;
  },

  //----------------------------------------------------------------------------
  //- public methods -----------------------------------------------------------
  //----------------------------------------------------------------------------

  // coordinates the construction and drawing of the whole graphic. This is
  // pretty much the only public method on the class...

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
                         ( this._featureHeight / 2 + 1 ) ].max() +
                       [ this._heights.lollipops.downMax,
                         this._heights.bridges.downMax,
                         ( this._featureHeight / 2 + 1 ) ].max() + 1;
                       // that single pixel is just a fudge factor...

    // canvas width is just calculated from the length of the sequence
    // (which is set in the "setSequence" method)
    var canvasWidth = this._imageWidth;

    // set the baseline, relative to which the various elements will
    // actually be drawn
    this._baseline = [ this._heights.lollipops.upMax,
                       this._heights.bridges.upMax,
                       this._imageParams.featureHeight / 2 ].max()  + 1;
                       // that single pixel is just a fudge factor...

    // if we don't yet have a <canvas>, build the one with the calculated
    // dimensions
    if ( ! this._canvas ) {
      // console.log( "PfamGraphic.render: building canvas with width x height, baseline: %d x %d, %d",
      //   canvasWidth, canvasHeight, this._baseline );
      this._buildCanvas( canvasWidth, canvasHeight );
    }

    // draw everything
    this._draw();

    // clean up...
    this._heights = {};
    this._areas   = [];

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

  // builds an in-memory representation of the markups (lollipops and bridges),
  // but doesn't actually draw them. The data structure built here contains
  // enough information to decide on the size of the <canvas> element, and, 
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
                                 upMax: 0 } };

    var bridgeMarkups   = [];
    var ip              = this._imageParams;
    var ms              = this._markupSpec;

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


      var headSize = ip.headSize[markup.headStyle];
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
      var hl = bridge.up ? heights.lollipops.up : heights.lollipops.down;
      var hb = bridge.up ? heights.bridges.up   : heights.bridges.down;

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
    this._drawSequence();

    // draw the briges
    var db = this._drawBridge.bind( this );
    this._heights.bridges.markups.each( function( bridge ) {
      db( bridge );
    } );

    // draw the lollipops after the bridges, so that the heads appear on top of
    // any overlapping bridges
    var dl = this._drawLollipop.bind( this );
    this._heights.lollipops.markups.reverse().each( function( lollipop ) {
      dl( lollipop );
    } );

    // draw the features
    var _drawFeature = this._drawFeature.bind( this );
    this._sequence.features.each( function( feature ) {
      _drawFeature( feature );
    } );

    // draw the motifs
    var _drawMotif = this._drawMotif.bind( this );
    this._sequence.motifs.each( function( motif ) {
      _drawMotif( motif );
    } );

    // add the image map (unless we're told not to)
    if ( this._options.imageMap ) {
      this._drawImageMap();
    }

  },

  //----------------------------------------------------------------------------

  // builds the image map

  _drawImageMap: function() {

    // build IDs for the <img> and <map> tags
    var canvasId = this._canvas.identify();
    var imgId = canvasId + "_image";
    var mapId = canvasId + "_map";

    // make an <img> tag and add it to the DOM
    var imgEl = new Element( "img", { id: imgId,
                                      "class": "canvasImageMap",
                                      src: "/shared/images/blank.gif",
                                      useMap: "#" + mapId } );
                                      // NB. need "useMap" (capital "M") for 
                                      // the benefit of IE...
    this._parent.appendChild( imgEl );

    // set the image dimensions
    var w = this._canvas.width + "px";
    var h = this._canvas.height + "px";
    var offsets = this._canvas.cumulativeOffset();
    var l = offsets.left + "px";
    var t = offsets.top + "px";

    // console.log( "PfamGraphic._drawImageMap: l x t + w + h: %d x %d + %d + %d",
    //   l, t, w, h );

    // sort the Y-axis coordinates. We'll get a warning from at least firefox if
    // we try to add an <area> with the coordinates such that y1 > y1
    // [ y1, y2 ] = [ y1, y2 ].sort( function( a, b) { return a - b; } );

    imgEl.setStyle( { left: l,
                      top: t,
                      width: w,
                      height: h } );

    // make a <map> tag and add it to the DOM
    var mapEl = new Element( "map", { name: mapId, 
                                      id: mapId } );
    this._parent.appendChild( mapEl );
    
    // add the areas to the map
    var buildArea = this._buildArea.bind( this );
    this._areas.each( function( area ) {
      var areaEl = buildArea( area );
      mapEl.appendChild( areaEl );

      // build a hash to store the details of this area, which we can 
      // retrieve later and use to build a tooltip
      areaEl.store( "data", area );
    } );

  },

  //----------------------------------------------------------------------------

  // builds and returns an individual <area> element

  _buildArea: function( area ) {
    var label  = area.label;
    var coords = area.coords.collect( function(n) { return Math.max( 0, Math.floor(n) ); } );

    // flip y1 and y2 if y1 > y2 (otherwise firefox, at least, issues a 
    // warning about the coordinates when we add the <area>)
    if ( coords[1] > coords[3] ) {
      var safe = coords[1];
      coords[1] = coords[3];
      coords[3] = safe;
    }

    var shape = ( area.shape === undefined ) ? "rect" : area.shape;

    // console.log( "PfamGraphic._buildArea: area |%s|, shape |%s|: %s",
    //   label, shape, coords.join(",") );

    var params = { coords: coords.join(","),
                   title:  label,
                   alt:    label,
                   shape:  shape };

    if ( area.href !== undefined ) {
      params.href = area.href;
      // console.log( "PfamGraphic._buildArea: adding url: |%s|", area.href );
    }

    var areaEl = new Element( "area", params );
    areaEl.identify();

    return areaEl;
  },

  //----------------------------------------------------------------------------

  // draws the basic ribbon, representing the sequence

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
    //                        0,                 this._topOffset,
    //                        this._imageWidth,  this._seqStep );
    this._context.fillStyle = "#dddddd";
    this._context.fillRect( 0,                this._topOffset,
                            this._imageWidth, this._seqStep );

    // console.log( "PfamGraphic._drawSequence: (x1, y1), (w, h): (%d, %d), (%d, %d)",
    //                        0,                 offset + this._seqStep,
    //                        this._imageWidth,  this._seqStep );
    this._context.fillStyle = "#bbbbbb";
    this._context.fillRect( 0,                this._topOffset + this._seqStep,
                            this._imageWidth, this._seqStep );

    // console.log( "PfamGraphic._drawSequence: (x1, y1), (w, h): (%d, %d), (%d, %d)",
    //                        0,                 this._topOffset + ( this._seqStep * 2 ),
    //                        this._imageWidth,  this._seqStep * 3 );
    this._context.fillStyle = "#cccccc";
    this._context.fillRect( 0,                this._topOffset + ( this._seqStep * 2 ),
                            this._imageWidth, this._seqStep * 3 );

    // add an area
    // this._areas.push( { label:  "sequence", // TODO make this more informative...
    //                     text:   "sequence",
    //                     coords: [ 0, offset, 
    //                               this._imageWidth, offset + this._seqStep * 5 ] } );
  },

  //----------------------------------------------------------------------------

  // draws an individual lollipop

  _drawLollipop: function( markup ) {
    // console.log( "PfamGraphic._drawLollipop: start" );
  
    var start = markup.start;

    var up = markup.v_align === undefined || markup.v_align === "top"; 

    var x1 = Math.floor( start * this._imageParams.xscale ) + 0.5;
    var y1;// = Math.round( this._baseline );
    var y2;
    if ( up ) {
      // console.log( "PfamGraphic._drawLollipop: drawing lollipop on top at %d", start );
      y1 = Math.round( this._topOffset );
      y2 = Math.floor( y1 - this._heights.lollipops.up[start] + ( this._baseline - this._topOffset ) );
    } else { 
      // console.log( "PfamGraphic._drawLollipop: drawing lollipop on bottom at %d", start );
      y1 = Math.round( this._botOffset );
      y2 = Math.ceil( y1 + this._heights.lollipops.down[start] - ( this._botOffset - this._baseline ) );
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
    var label = this._imageParams.templates.lollipop.evaluate( { label: markup.label,
                                                                 pos:   markup.start } );

    this._areas.push( { label:  label,
                        text:   markup.text,
                        start:  start,
                        coords: [ x1-1, y1-1, x1+1, y2+1 ] } );

    // add the head
    if ( markup.headStyle ) {
      this._drawLollipopHead( x1, y2, start, up, markup.headStyle, markup.colour, label, markup.text );
    }

    // console.log( "PfamGraphic._drawLollipop: end" );
  },
 
  //----------------------------------------------------------------------------

  // draws the head of a lollipop

  _drawLollipopHead: function( x, y, start, up, style, colour, label, text ) {
    // console.log( "PfamGraphic._drawLollipopHead: starting to draw head |%s|", style );

    var r;
    var d;
    switch ( style ) {

      case "circle":
        r = this._imageParams.headSize.circle;
        // console.log( "PfamGraphic._drawLollipopHead: drawing circle" );
        this._context.beginPath();
        this._context.arc( x, y, r, 0, (Math.PI * 2), "true" );
        this._context.fillStyle = colour || "red";
        this._context.fill();
        this._areas.push( { label:  label,
                            text:   text,
                            shape:  "circle",
                            start:  start,
                            coords: [ x, y, r ] } );
        break;

      case "square":
        d = this._imageParams.headSize.square / 2;
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
        this._areas.push( { label:  label,
                            text:   text,
                            start:  start,
                            coords: [ x - d, y - d, x + d, y + d ] } );
        break;

      case "diamond":
        d = this._imageParams.headSize.diamond;
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
        this._areas.push( { label:  label,
                            text:   text,
                            shape:  "poly",
                            start:  start,
                            coords: [ x - d, y,
                                      x,     y + d,
                                      x + d, y,
                                      x,     y - d,
                                      x - d, y ] } );
        break;

      case "line":
        d = this._imageParams.headSize.line;
        // console.log( "PfamGraphic._drawLollipopHead: drawing line, length |%d|, centred %d x %d", 
        //   d, x, y );
        this._context.beginPath();
        this._context.moveTo( x, y - d );
        this._context.lineTo( x, y + d );
        this._context.closePath();
        this._context.strokeStyle = colour || "rgb(50, 40, 255)";
        this._context.stroke();
        this._areas.push( { label:  label,
                            text:   text,
                            start:  start,
                            coords: [ x - 1, y - d - 1,
                                      x + 1, y + d + 1 ] } );
        break;

      case "arrow":
        d = this._imageParams.headSize.arrow;
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

        this._areas.push( { label:  label,
                            text:   text,
                            start:  start,
                            shape:  "poly",
                            coords: coords } );
  
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
  
    var start  = bridge.markup.start;
    var end    = bridge.markup.end;
    var height = bridge.height;
    var up     = bridge.up;

    var colour = "#000000";
    if ( bridge.markup.colour.match( "^\\#[0-9A-Fa-f]{6}$" ) ) {
      colour = bridge.markup.colour;
      // console.log( "PfamGraphic._drawBridge: using user-defined colour '%s'", colour );
    }

    var x1 = Math.floor( start * this._imageParams.xscale ) + 0.5;
    var x2 = Math.floor( end * this._imageParams.xscale ) + 0.5;
    var y1 = Math.round( this._baseline );
    var y2;
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

    this._context.strokeStyle = colour;  
    this._context.stroke();
    this._context.closePath();
    
    // build a label for the bridge
    var label = this._imageParams.templates.bridge.evaluate( { label: bridge.markup.label,
                                                               start: start,
                                                               end:   end } );

    // add <area> tags for each of the legs and the horizontal
    this._areas.push( { label:  label, 
                        text:   bridge.markup.label,
                        start:  start,
                        end:    end,
                        coords: [ x1-1, y1-1, x1+1, y2+1 ] } );
    this._areas.push( { label:  label, 
                        text:   bridge.markup.label,
                        start:  start,
                        end:    end,
                        coords: [ x1-1, y2-1, x2+1, y2+1 ] } );
    this._areas.push( { label:  label, 
                        text:   bridge.markup.label,
                        start:  start,
                        end:    end,
                        coords: [ x2-1, y2-1, x2+1, y1+1 ] } );

    // console.log( "PfamGraphic._drawBridge: end" );
  },
 
  //----------------------------------------------------------------------------

  // draws a feature (most commonly a domain)

  _drawFeature: function( feature ) {
    // console.log( "PfamGraphic._drawFeature: drawing feature..." );

    if ( ! this._markupSpec.featureEndValues.include( feature.startStyle ) ) {
      throw( "PfamGraphic: ERROR: feature start style is not valid: '" + feature.startStyle + "'" );
    }

    if ( ! this._markupSpec.featureEndValues.include( feature.endStyle ) ) {
      throw( "PfamGraphic: ERROR: feature end style is not valid: '" + feature.endStyle + "'" );
    }

    // calculate dimensions for the inner shape
    var height = Math.floor( this._featureHeight ) - 2;
    var radius = Math.round( height / 2 );
    var arrow  = radius;
    var width = ( feature.end - feature.start + 1 ) * this._imageParams.xscale - 2;

    var x = Math.floor( feature.start * this._imageParams.xscale ) + 1.5;
    var y = Math.floor( this._baseline - radius ) + 0.5;

    var featureParams = {
      x: x, 
      y: y, 
      w: width, 
      h: height,
      r: radius,
      a: arrow,
      s: feature.startStyle,
      e: feature.endStyle
    };

    // console.log( "PfamGraphic._drawFeature: inner: (x, y), h, w: (%d, %d), %d, %d",
    //   x, y, height, width );

    //----------------------------------

    // the inner-most is filled, with a colour gradient running from white to 
    // dark to light colour as y increases. First draw the shell, then fill it
    this._buildFeaturePath( featureParams );

    // fill the path with a gradient
    var gradient = this._context.createLinearGradient( x, y, x, y + height );

    gradient.addColorStop( 0, "#ffffff" );
    gradient.addColorStop( 0.5, feature.colour );
    gradient.addColorStop( 0.7, feature.colour );
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

    // console.log( "PfamGraphic._drawFeature: outer: (x, y), h, w: (%d, %d), %d, %d",
    //   x, y, height, width );

    this._buildFeaturePath( { x: x, 
                              y: y, 
                              w: width, 
                              h: height,
                              r: radius,
                              a: arrow,
                              s: feature.startStyle,
                              e: feature.endStyle } );

    this._context.strokeStyle = feature.colour;
    this._context.stroke();

    //----------------------------------

    // build a label for the <area>
    // add the transparent overlay to show the limits of the alignment region
    var label;
    if ( feature.aliStart !== undefined &&
         feature.aliEnd   !== undefined ) {
      this._drawEnvelope( feature, radius, height );

      // use a template that includes the envelope details
      label = this._imageParams.templates.feature.evaluate( { label: feature.text,
                                                              start: feature.start,
                                                              end:   feature.end } );
    } else {
      // there's no envelope given, so use a template that describes the whole
      // feature
      label = this._imageParams.templates.alignment.evaluate( { label:    feature.text,
                                                                start:    feature.start,
                                                                end:      feature.end,
                                                                aliStart: feature.aliStart,
                                                                aliEnd:   feature.aliEnd } );
    }


    //----------------------------------

    // add the text label
    if ( this._options.labels ) {
      this._drawText( x, this._baseline, width, feature.text );
    }

    //----------------------------------

    // build a URL
    var url = this._imageParams.featureUrl + feature.text;

    // add the area
    this._areas.push( { label:    label,
                        start:    feature.start,
                        end:      feature.end,
                        aliStart: feature.aliStart,
                        aliEnd:   feature.aliEnd,
                        href:     url,
                        coords:   [ x, y, x+width, y+height ] } );

    // console.log( "PfamGraphic._drawFeature: done" );
  }, // end of "_drawFeature"

  //----------------------------------------------------------------------------

  // draws a motif

  _drawMotif: function( motif ) {

    if ( motif.colour instanceof Array ) {
      this._drawPfamBMotif( motif );
    } else {
      this._drawSingleColourMotif( motif );
    }

  },

  //----------------------------------------------------------------------------

  // draws a Pfam-B-style motif

  _drawPfamBMotif: function( motif ) {
    // console.log( "PfamGraphic._drawPfamBMotif: drawing motif..." );

    // first, make sure we have a sensible number of colours to play with...
    if ( motif.colour.length !== 3 ) {
      throw( "PfamGraphic: ERROR: motifs must have either one or three colours" );
    }

    // convert the colours from hex strings into "rgba()" values
    var colours = [];

    var getRGBColour = this._getRGBColour.bind( this );
    var ip           = this._imageParams;

    motif.colour.each( function( colour ) {
      var rgbColour = getRGBColour( colour );
      colours.push( { rgb:  "rgb("  + rgbColour.join(",") + ")",
                      rgba: "rgba(" + rgbColour.join(",") + "," + ip.motifOpacity + ")" } );
    } );

    // work out the dimensions

    var height = this._imageParams.motifHeight;
    var width  = ( motif.end - motif.start + 1 ) * this._imageParams.xscale;
    var step   = Math.round( height / 3 );

    var x = Math.floor( motif.start * this._imageParams.xscale );
    var y = Math.floor( this._baseline - Math.round( height / 2 ) );

    // console.log( "PfamGraphic._drawPfamBMotif: (x, y), h, w: (%d, %d), %d, %d",
    //   x, y, height, width );

    // draw the three stripes
    for ( var i = 0; i < 3; i++ ) {

      this._context.fillStyle = colours[i].rgba;
      this._context.fillRect( x, y + ( step * i ), width, step );

    }

    // build a label for the <area>
    var label = this._imageParams.templates.motif.evaluate( { label: motif.label,
                                                              start: motif.start,
                                                              end:   motif.end } );

    // build a URL
    // var url = this._imageParams.featureUrl + feature.text;

    // add the area
    this._areas.push( { label:  label,
                        text:   motif.text,
                        start:  motif.start,
                        end:    motif.end,
                        coords: [ x, y, x + width, y + height ] } );

    // console.log( "PfamGraphic._drawPfamBMotif: done" );
  }, // end of "_drawPfamBMotif"

  //----------------------------------------------------------------------------

  // draws a single colour motif

  _drawSingleColourMotif: function( motif ) {
    // console.log( "PfamGraphic._drawSingleColourMotifs: drawing motif..." );

    var height = this._imageParams.motifHeight;
    var width = ( motif.end - motif.start + 1 ) * this._imageParams.xscale;

    var x = Math.floor( motif.start * this._imageParams.xscale );
    var y = Math.floor( this._baseline - Math.round( height / 2 ) );

    // console.log( "PfamGraphic._drawSingleColourMotif: (x, y), h, w: (%d, %d), %d, %d",
    //   x, y, height, width );

    // convert the colour from a hex string into an "rgba()" value
    var colour = this._getRGBColour( motif.colour );
    var rgb  = "rgb(" + colour.join(",") + ")";
    var rgba = "rgba(" + colour.join(",") + "," + this._imageParams.motifOpacity + ")";

    // console.log( "PfamGraphic._drawSingleColourMotif: hex colour: %s, rgb: %s, rgba; %s", 
    //   motif.colour, rgb, rgba );

    // draw the rectangle
    this._context.fillStyle = rgba;
    this._context.fillRect( x, y, width, height + 1 );

    // build a label for the <area>
    var label = this._imageParams.templates.motif.evaluate( { label: motif.label,
                                                              start: motif.start,
                                                              end:   motif.end } );

    // // build a URL
    // var url = this._imageParams.featureUrl + feature.text;

    // // add the area
    this._areas.push( { label:  label,
                        text:   motif.text,
                        start:  motif.start,
                        end:    motif.end,
                        coords: [ x, y, x + width, y + height ] } );

    // console.log( "PfamGraphic._drawMotif: done" );
  }, // end of "_drawSingleColourMotif"

  //----------------------------------------------------------------------------

  // builds the path for constructing features. The path can be used either as
  // an outline for filling or stroking.

  _buildFeaturePath: function( params ) {

    this._context.beginPath();

    // console.log( "PfamGraphic._buildFeaturePath: drawing left end" );
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
    // console.log( "PfamGraphic._buildFeaturePath: drawing bottom line and right end" );
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
    // console.log( "PfamGraphic._buildFeaturePath: drawing top line" );
    if ( params.s === "curved" || 
         params.s === "arrow" ) {
      this._context.lineTo( params.x + params.r, params.y );
    } else {
      this._context.lineTo( params.x, params.y );
    }
    
    this._context.closePath();
  }, // end of "_buildFeaturePath"

  //----------------------------------------------------------------------------

  // draws semi-transparent overlays to represent the envelope regions around
  // the core alignment
  
  _drawEnvelope: function( feature, radius, height ) {
    // console.log( "PfamGraphic._drawEnvelope: adding envelope overlay" );

    // TODO handle the case where there's an aliStart but no aliEnd given

    // make sure the endpoints are sensible
    if ( feature.start > feature.aliStart ) {
      throw( "PfamGraphic: ERROR: features must have start <= aliStart (" + feature.start + " is > " + feature.aliStart + ")" );
    }

    if ( feature.end < feature.aliEnd ) {
      throw( "PfamGraphic: ERROR: features must have end >= aliEnd (" + feature.end + " is < " + feature.aliEnd + ")" );
    }

    //----------------------------------

    var y  = this._baseline - radius;
    var xs = this._imageParams.xscale;
    var l = {
      x: Math.floor( feature.start * xs ),
      y: Math.floor( y - 1 ) + 1,
      w: ( feature.aliStart * xs ) - ( feature.start * xs ) + 1,
      h: height + 1
    };

    var r = {
      x: Math.floor( feature.aliEnd * xs ),
      y: Math.floor( y - 1 ) + 1,
      w: ( feature.end * xs ) - ( feature.aliEnd * xs ) + 2,
      h: height + 1
    };

    // a grey overlay; more visible
    /* var fillStyle = "rgba(255,255,255," + this._imageParams.envOpacity + ")"; */

    // an overlay that's the same colour as the domain
    // var rgb = this._getRGBColour( feature.colour );
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
    var label = this._imageParams.templates.envelope.evaluate( { label: feature.text,
                                                                 start: feature.start,
                                                                 end:   feature.aliStart } );

    // build a URL
    var url = this._imageParams.featureUrl + feature.text;

    // add the area
    this._areas.push( { label:  label,
                        text:   feature.text,
                        href:   url,
                        start:  feature.start,
                        end:    feature.aliStart,
                        coords: [ l.x, l.y, l.x + l.w, l.y + l.h ] } );

    // build the end <area>
    label = this._imageParams.templates.envelope.evaluate( { label: feature.text,
                                                             start: feature.aliEnd,
                                                             end:   feature.end } );

    url = this._imageParams.featureUrl + feature.text;

    this._areas.push( { label:  label,
                        text:   feature.text,
                        href:   url,
                        start:  feature.aliEnd,
                        end:    feature.end,
                        coords: [ r.x, r.y, r.x + r.w, r.y + r.h ] } );
  }, // end of "_drawEnvelope"
  
  //----------------------------------------------------------------------------

  // adds the text label to the domain, if it will fit nicely inside the shape

  _drawText: function( x, midpoint, featureWidth, text ) {

    this._context.save();

    // set up the font
    this._context.font         = "bold 1em 'optimer'";
    this._context.textAlign    = "center";
    this._context.textBaseline = "middle";

    // calculate the width of the text to be rendered
    var metrics = this._context.measureText( text );

    // console.log( "PfamGraphic._drawText: textBaseline: %d", this._context.textBaseline );

    // pad the text a little and then compare that to the width of the feature,
    // so that we can assess whether it's going to fit inside the feature when
    // rendered
    /* var paddedTextWidth = metrics.width + 2 * this._imageParams.labelPadding; */
    var paddedTextWidth = metrics.width + Math.round( this._featureHeight / 3 );

    // console.log( "PfamGraphic._drawText: padded text width: %d, feature width: %d",
    //   paddedTextWidth, featureWidth );

    if ( paddedTextWidth > featureWidth ) {
      // console.log( "PfamGraphic._drawText: text is wider than feature; not adding" );
      return;
    }

    var textX = x + ( featureWidth / 2 );
    // console.log( "PfamGraphic._drawText: feature X, midpoint: (%d, %d); textX: %d", 
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

  // draws the left-hand end of feature with a curved end

  _drawLeftRounded: function( x, y, radius, height ) {
    this._context.quadraticCurveTo( x, y, x, y + radius );
    this._context.quadraticCurveTo( x, y + height, x + radius, y + height );
  },

  //----------------------------------------------------------------------------

  // draws the right-hand end of feature with a curved end

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

    for ( var i = 0; i < yShifts.length; i++ ) {
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
    var list = this._imageParams[cacheKey];

    if ( list === undefined ) {

      // the "period" of the step, in pixels
      var step = height / steps;

      // walk out from the mid-line and add Y-axis coords to an array
      var yShifts = [];
      for ( var i = 0; i < ( steps / 2 ); i++ ) {
        yShifts.push( ( height / 2 ) - ( i * step ) );
        yShifts.push( ( height / 2 ) + ( i * step ) );
      }

      // uniquify and (numerically) sort the list of Y-coords
      list = yShifts.uniq().sort( function (a, b) { return a - b; } );

      // cache the list for later
      this._imageParams[cacheKey] = list;
    }

    return list;
  },

  //----------------------------------------------------------------------------

  // draws the left-hand end of a feature as an arrow head

  _drawLeftArrow: function( x, y, arrow, height ) {
    this._context.lineTo( x, y + arrow );
    this._context.lineTo( x + arrow, y + height );
  },

  //----------------------------------------------------------------------------

  // draws the right-hand end of a feature as an arrow head

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
    var h = matches[1];
    
    var r = parseInt( h.substring( 0, 2 ), 16 );
    var g = parseInt( h.substring( 2, 4 ), 16 );
    var b = parseInt( h.substring( 4, 6 ), 16 );

    var rgb = [ r, g, b ];
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

