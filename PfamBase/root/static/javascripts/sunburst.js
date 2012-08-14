
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console, Element, Hash, document, window, TreeFactory,
         setTimeout, clearTimeout, G_vmlCanvasManager, Tip */

// spoof a console, if necessary, so that we can run in IE (<8) without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript library for drawing &quot;sunburst&quot; species trees using 
// the HTML5 canvas.
//
// jt6 20100611 WTSI
//
// $Id$
//
// Copyright (c) 2010: Genome Research Ltd.
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

var Sunburst = Class.create( {
  /**
   * @lends Sunburst#
   * @author John Tate
   * @author Rob Finn
   */
  //----------------------------------------------------------------------------
  //- class variables ----------------------------------------------------------
  //----------------------------------------------------------------------------
  
  /**
   * Set specific colour ranges for specific domains. These are given as hue
   * values, to be used in HSV colour specifications, with the hue angle given
   * in radians.
   *
   * @private
   */
  _colours: {
    "Archaea":                { "minH": 0.017453292519943295,
                                "maxH": 0.6981317007977318 },
    "Bacteria":               { "minH": 0.8726646259971648,
                                "maxH": 2.6179938779914944 },
    "unclassified sequences": { "minH": 3.141592653589793,
                                "maxH": 3.1764992386296798 },
    "unclassified":           { "minH": 3.193952531149623,
                                "maxH": 3.2288591161895095 },
    "Eukaryota":              { "minH": 4.363323129985824,
                                "maxH": 5.235987755982989 },
    "Viruses":                { "minH": 5.410520681182422,
                                "maxH": 5.759586531581287 },
    "Viroids":                { "minH": 5.777039824101231,
                                "maxH": 5.846852994181004 },
    "other sequences":        { "minH": 5.8643062867009474,
                                "maxH": 6.19591884457987 }
  },

  // these are the original angles, converted to radians above
  // _colours: { 
  //   "Archaea":                { "minH": 1,
  //                               "maxH": 40 },
  //   "Bacteria":               { "minH": 50,
  //                               "maxH": 150 },
  //   "unclassified sequences": { "minH": 180,
  //                               "maxH": 182 },
  //   "unclassified":           { "minH": 183,
  //                               "maxH": 185 },
  //   "Eukaryota":              { "minH": 250,
  //                               "maxH": 300 },
  //   "Viruses":                { "minH": 310,
  //                               "maxH": 330 },
  //   "Viroids":                { "minH": 331,
  //                               "maxH": 335 },
  //   "other sequences":        { "minH": 336,
  //                               "maxH": 355 }
  // },

  // the eight major taxonomic levels. Used only when generating the 
  // tooltips for the mouseover.
  _levels: [
    null,
    "superkingdom",
    "kingdom", 
    "phylum", 
    "class",
    "order",
    "family",
    "genus",
    "species"
  ],

  // a regex for testing whether a node name is something like "(No order)"
  _noLevelRE: new RegExp( '^\\\(No ' ),

  // when the user selects or deselects a set of nodes, we colour each successive
  // layer after a delay of "_selectAnimationDelay" milliseconds
  _selectAnimationDelay: 10,

/*
+------------+--------------------+----------+---------+---------+--------+------------------------+---------+--------------+
| ncbi_taxid | species            | taxonomy | lft     | rgt     | parent | level                  | minimal | rank         |
+------------+--------------------+----------+---------+---------+--------+------------------------+---------+--------------+
|          0 | NULL               | NULL     |       1 |   10506 | root   | other sequences        |       1 | superkingdom |
|     131567 | cellular organisms |          |   10507 |   10508 | root   | NULL                   |       1 | species      |
|          0 | NULL               | NULL     |   10509 |   10686 | root   | Viroids                |       1 | superkingdom |
|          0 | NULL               | NULL     |   10687 |  734442 | root   | Eukaryota              |       1 | superkingdom |
|          0 | NULL               | NULL     |  734443 | 1057744 | root   | Bacteria               |       1 | superkingdom |
|          0 | NULL               | NULL     | 1057745 | 1057748 | root   | unclassified           |       1 | superkingdom |
|          0 | NULL               | NULL     | 1057749 | 1175028 | root   | Viruses                |       1 | superkingdom |
|          0 | NULL               | NULL     | 1175029 | 1184982 | root   | Archaea                |       1 | superkingdom |
|          0 | NULL               | NULL     | 1184983 | 1185712 | root   | unclassified sequences |       1 | superkingdom |
+------------+--------------------+----------+---------+---------+--------+------------------------+---------+--------------+
*/

  /**
   * Enable/disable the handlers that deal with mouse clicks. This effectively
   * enables or disables selections.
   *
   * @private
   */
  _enableClicks: true,

  /**
   * The time limit (in milliseconds) for registering a second click as a 
   * double-click.
   *
   * @private
   */
  _doubleClickPeriod: 250, // milliseconds

  /**
   * The hex colour for a node in the tree that has been selected.
   *
   * @private
   */
  _selectedNodeColour: "#922",

  /**
   * The hex colour for a node in the tree that has been selected and then 
   * "moused-over".
   *
   * @private
   */
  _highlightedSelectedNodeColour: "#C55",

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class
   * A javascript library from drawing taxonomic trees in a &quot;sunburst&quot;
   * representation. The rendering of the tree is done using an HTML
   * &lt;canvas&gt; element.
   * </p>
   *
   * <h2>Synopsis</h2>
   *
   * <code><pre>
   * var sunburst = new Sunburst( &quot;tree&quot;, treeData );
   * sunburst.build();
   * sunburst.draw();</pre>
   * </code>
   *
   * <p>
   * Initialising a Sunburst object will add the following HTML to the existing
   * page, appending it as a child of the specified parent element:
   * </p>
   *
   * <code><pre>
   * <div id="subTreeWrapper" style="display: none">
   *   <h1 id="subTreeSpeciesName"></h1>
   *   <div id="subTree"></div>
   * </div>
   * <div id="treeWrapper"></div>
   * <div id="treeTipContent" style="display: none">
   *   <div id="treeTipTitle"></div>
   *   <div id="treeTipBody">
   *     <span id="tipNumSeq">0</span> sequences<br />
   *     <span id="tipNumSpecies">0</span> species
   *   </div>
   * </div>
   * </pre>
   * </code>
   * 
   * <p>
   * Canvas elements will be added to this basic structure, and the tool
   * tip-related elements will be updated as the mouse is moved across the tree.
   * The main species tree will be drawn in the &quot;treeWrapper&quot; element,
   * while the sub-tree (a representation of a particular branch of the main 
   * tree) will be draw in &quot;subTree&quot;
   * </p>
   */
  initialize: function( options ) {

    this._treeParentEl    = $(options.parent);
    this._subTreeParentEl = $(options.subTreeParent);
    this._treeData        = options.tree;
    this._tipStyle        = options.tipStyle || "pfam";

    this._tree             = TreeFactory( treeData );
    this._layers           = [];
    this._arcs             = [];

    this._treesAndArcs = {
      numSeq:     {
        tree:   this._tree,
        layers: this._layers,
        arcs:   this._arcs
      },
      numSpecies: {}
    };
    this._weightByNumSeq   = true;
                           
    this._depthLimit       = 50;
    this._layerSeparation  = 2;
                           
    this._treeWidth        = options.w || 1000;
    this._treeHeight       = options.h || 1000;
    this._subTreeWidth     = 246;
    this._subTreeHeight    = 125;
                           
    // this._centreX          = this._treeWidth  / 2;
    // this._centreY          = this._treeHeight / 2;
    this._centreX          = 0;
    this._centreY          = 0;
    this._layerWidth       = 35;
                           
    this._branches         = [];
    this._arcCount         = 0;

    this._radialLabels     = false; // true:  draw labels centred on arcs and radially
                                    // false: draw labels tangential to arcs
    this._selectedArcs     = new Hash();
    this._highlightedArc   = null;
    this._clickedArc       = null;

    this._showSunburst     = true;
    this._showTree         = false;

    this._selectedSequencesCount = 0;
    this._selectedSpeciesCount   = 0;

    // this._tipStyle         = {
    //   border: 3,
    //   delay: 0.75,
    //   hook: { tip: 'topLeft', mouse: true },
    //   offset: { x: 8, y: 8 },
    //   radius: 3,
    //   stem: "topLeft",
    //   width: "15em"
    // };
    
    // build the markup that we need for the tree and the sub-tree
    this._buildHTML();
  },

  //----------------------------------------------------------------------------
  /**
   * Builds the data structure that represents the arcs that are actually drawn.
   * Must be called before <code>draw()</code>.
   */
  build: function() {
    this._buildGraph( this._tree.children, this._layerWidth, 1, 0, 0, 0, 0 );
    return this;
  },

  //----------------------------------------------------------------------------
  /**
   * Draws the sunburst. Call <code>build()</code>, to calculate the data needed
   * to draw the arcs, before calling <code>draw()</code>.
   */
  draw: function() {

    this._arcCount = 0;

    // move the origin of the coordinate system back to the origin of the canvas
    this._ctx.translate( 0 - this._centreX, 0 - this._centreY );
    
    // blank the whole canvas
    this._ctx.clearRect( 0, 0, this._treeWidth, this._treeHeight );

    // calculate the new centre position
    //
    // need to take into account the number of layers. The "-1" is a hack to 
    // take into account the fact that the layers array is one-based.
    var radius = ( this._layers.size() - 1 ) * this._layerWidth;
    this._centreX = radius + this._layerWidth;
    this._centreY = radius + this._layerWidth;

    // translate the origin of the coordinate system to the new centre of the
    // sunburst
    this._ctx.translate( this._centreX, this._centreY );

    // label the "root node"
    this._ctx.fillText( "Root", 0, 0 );

    this._ctx.lineWidth = this._layerWidth;

    this._arcs.each( function(layer, i) {
      // console.debug( "drawing layer %d", i );
      if ( layer === undefined ) {
        // console.warn( "layer %d undefined", i );
        return;
      }
      layer.each( function(arc) {
        // console.debug( "drawing arc: " + arc );
        if ( arc.isSelected ) {
          this._drawArc( arc, this._selectedNodeColour );
        } else {
          this._drawArc( arc, arc._colour );
        }
        this._arcCount++;
      }.bind(this) );
    }.bind(this) );

    // console.debug( "drew %d arcs (depth limit %d)", this._arcCount, this._depthLimit );

    return this;
  },

  //----------------------------------------------------------------------------
  /**
   * Sets the scale of the tree. Defaults to <strong>50</strong>. Must be in the
   * range 10 &lt;= scale &lt;= 100.
   *
   * @param s scale value
   * @returns {Sunburst} reference to this object
   */
  setScale: function( s ) {
    this._layerWidth = Math.max( 10, Math.min( s, 100 ) );
    return this;
  },

  //----------------------------------------------------------------------------
  /**
   * Returns a list of the currently selected <code>SunburstNode</code> objects.
   *
   * @returns {Array} list of selected <code>SunburstNode</code> objects
   */
  getSelected: function() {
    return this._selectedArcs.values();
  },

  //----------------------------------------------------------------------------
  /**
   * Returns a list of the sequence accessions under the currently selected
   * nodes.
   *
   * @returns {Array} list of selected sequence accessions
   */
  getSelectedSeqAccs: function() {
    var accessions = new Hash();
    this.getSelected().each( function( arc ) { 
      if ( arc.sequences !== undefined ) {
        arc.sequences.each( function( seq ) {
          accessions.set( seq.seqAcc, null );
        } );
      }
    } );
    return accessions.keys();
  },

  //----------------------------------------------------------------------------
  /**
   * Returns a associative list of the sequence accessions under the currently
   * selected nodes, grouped by taxonomy ID, e.g.
   *   {
   *     "213422":null,
   *     "28231":null,
   *     "35554":["AE017180","CP002031"],
   *     "28232":["CP000148"],
   *     "351604":["CP000698"],
   *     "313985":["CP001089"],
   *     "225194":["CP001124"],
   *     "316067":["CP001390"],
   *     "443144":["CP001661"],
   *     "443143":["CP002479"]
   *   }
   *
   * @returns {Object} list of selected sequence accessions grouped by tax ID
   */
  getSelectedSeqAccsByTaxa: function() {
    var selection = new Hash();
    this.getSelected().each( function( arc ) {
      if ( arc.sequences === undefined ) {
        selection.set( arc.taxid, null );
      } else {
        selection.set(
           arc.taxid,
           arc.sequences.collect( function( seq ) {
            return seq.seqAcc;
           } )
        );
      }
    } );
    return selection.toJSON();
  },
  
  //----------------------------------------------------------------------------
  /**
   * Clears the list of currently selected nodes. This also redraws the 
   * currently selected nodes, returning them to their original colour.
   *
   * @returns {Sunburst} reference to this object
   */
  clearSelection: function() {
    this._selectedArcs.values().each( function(arc) {
      arc.isSelected = false;
      this._drawArc( arc, arc._colour );
    }.bind(this) );
    this._selectedArcs = new Hash();
    this._selectedSequencesCount = 0;
    this._selectedSpeciesCount   = 0;

    $("sunburst").fire( "sunburst:selectionChange", {
      numSequences: 0,
      numSpecies:   0
    } );

    return this;
  },
  
  //----------------------------------------------------------------------------
  /**
   * Sets the weighting scheme for the tree building algorithm. Calling this
   * method re-calculates the angle subtended by each arc, weighting it 
   * according to the number of sequences for the corresponding tree node. This
   * is the default representation. This does not re-draw the tree; call 
   * <code>draw()</code> to show the new tree structure.
   *
   * @returns {Sunburst} reference to this object
   */
  weightByNumSeq: function() {
    this._weightByNumSeq = true;

    if ( ! this._treesAndArcs.numSeq.tree ) {

      // convert the raw tree data into an object tree and get empty arrays for
      // storing the layers and arcs
      this._treesAndArcs.numSeq.tree   = TreeFactory( this._treeData );
      this._treesAndArcs.numSeq.layers = [];
      this._treesAndArcs.numSeq.arcs   = [];

      // set the references to these new data structures...
      this._tree   = this._treesAndArcs.numSeq.tree;
      this._layers = this._treesAndArcs.numSeq.layers;
      this._arcs   = this._treesAndArcs.numSeq.arcs;

      // ... and build the arcs
      this.build();
    }

    // set the references to the (now populated) data structures for the
    // tree weighted by number of sequences
    this._tree   = this._treesAndArcs.numSeq.tree;
    this._layers = this._treesAndArcs.numSeq.layers;
    this._arcs   = this._treesAndArcs.numSeq.arcs;

    return this;
  },
  
  //----------------------------------------------------------------------------
  /**
   * Sets the weighting scheme for the tree building algorithm. Calling this
   * method re-calculates the angle subtended by each arc, weighting it 
   * according to the number of species for the corresponding tree node. 
   * This does not re-draw the tree; call <code>draw()</code> to show the new
   * tree structure.
   *
   * @returns {Sunburst} reference to this object
   */
  weightByNumSpecies: function() {
    this._weightByNumSeq = false;

    if ( ! this._treesAndArcs.numSpecies.tree ) {
      
      this._treesAndArcs.numSpecies.tree   = TreeFactory( this._treeData );
      this._treesAndArcs.numSpecies.layers = [];
      this._treesAndArcs.numSpecies.arcs   = [];

      this._tree   = this._treesAndArcs.numSpecies.tree;
      this._layers = this._treesAndArcs.numSpecies.layers;
      this._arcs   = this._treesAndArcs.numSpecies.arcs;

      this.build();
    }
    
    this._tree   = this._treesAndArcs.numSpecies.tree;
    this._layers = this._treesAndArcs.numSpecies.layers;
    this._arcs   = this._treesAndArcs.numSpecies.arcs;

    return this;
  },
  
  //----------------------------------------------------------------------------
  /**
   * Returns &quot;Sunburst object&quot;. Only useful for debugging.
   */
  toString: function() { return "Sunburst object"; },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Builds the HTML that is required for the tree and sub-tree, adding them to
   * the DOM as child nodes of the specified parent element.
   *
   * @private
   * @returns {Sunburst} reference to this object
   */
  _buildHTML: function() {

    // add the sub-tree markup. Use "insert" to avoid stopping on anything that
    // is already in the element
    this._subTreeParentEl.insert( { top: [ 
      '<div id="subTreeWrapper">',
        '<h1 id="subTreeSpeciesName">Lineage</h1>',
        '<div id="subTree">',
          '<div id="subTreeHelp">',
            '<p>',
              'Move your mouse over the main tree to show the lineage of a particular node.',
            '</p>',
            '<p>',
              'You can move this pane by dragging it.',
            '</p>',
          '</span>',
        '</div>',
      '</div>'
    ].join("") } ); // keep JSLint happy by avoiding a multi-line string...
 
    // and the main tree
    this._treeParentEl.insert( { top: [ 
      '<div id="treeWrapper"></div>',
      '<div id="treeTipContent" style="display: none">',
        '<div id="treeTipTitle" class="title"></div>',
        '<div id="treeTipBody" class="content">',
          '<span id="tipNumSeq">0 sequences</span><br />',
          '<span id="tipNumSpecies">0</span> species',
        '</div>',
      '</div>'
    ].join("") } );
 
    this._subTreeWrapperDiv = $("subTreeWrapper");
    this._speciesNameDiv    = $("subTreeSpeciesName");
    this._subTreeDiv        = $("subTree");
    this._subTreeHelp       = $("subTreeHelp");
    this._treeDiv           = $("treeWrapper");
    this._tipContent        = $("treeTipContent");
    this._tipTitle          = $("treeTipTitle");
    this._tipNumSeq         = $("tipNumSeq");
    this._tipNumSpecies     = $("tipNumSpecies");

    // build the canvases
    this._subTreeCanvas = this._buildCanvas( this._subTreeDiv, this._subTreeWidth, this._subTreeHeight );
    this._treeCanvas    = this._buildCanvas( this._treeDiv,    this._treeWidth,    this._treeHeight );

    // stash references to the contexts directly
    this._ctx   = this._treeCanvas.getContext("2d");
    this._stctx = this._subTreeCanvas.getContext("2d");

    // pre-set some attributes on the canvases
    this._ctx.fillStyle     = "#000000";
    this._ctx.textAlign     = "center";
    this._ctx.textBaseline  = "middle";
    this._stctx.strokeStyle = "#000000";
    this._stctx.fillStyle   = "#000000";

    // create the tooltip
    this._tip = new Tip( 
      this._treeCanvas, 
      this._tipContent,
      { style: this._tipStyle,
        delay: 0.75,
        width: "15em" }
    );

    // watch the main canvas for mouse events
    this._treeCanvas.observe( "mousemove", this._highlightNode.bind(this) );
    if ( this._enableClicks ) {
      this._treeCanvas.observe( "click", this._selectNode.bind(this) );
    }

    this._builtMarkup = true;

    return this;
  },

  //----------------------------------------------------------------------------
  /**
   * Builds the two canvases for the trees. Also takes care of correctly 
   * intialising them in IE...
   *
   * @private
   * @returns {canvas} a canvas element
   */
  _buildCanvas: function( parentEl, w, h ) {

    // build the canvas element
    var canvas = document.createElement("canvas");
    Element.extend( canvas );

    canvas.width  = w;
    canvas.height = h;

    parentEl.appendChild( canvas );

    // initialise it in IE... N.B. we need the quotes around this "undefined"
    if ( typeof G_vmlCanvasManager !== "undefined" ) {
      canvas = G_vmlCanvasManager.initElement( canvas );
    }

    return canvas;
  },

  //----------------------------------------------------------------------------
  /**
   * Recursively calculates the radius and angular limits for each arc.
   *
   * @private
   * @returns {Sunburst} reference to this object
   */
  _buildGraph: function( children, r, depth, initialH, initialAlpha, rootShare, range ) {
  
    this._graphBuilt = true;

    children.each( function( child ) {

      // calculate the limits of the arc
      var fromAlpha  = this._layers[depth] && this._layers[depth].alpha ? this._layers[depth].alpha : 0,
          deltaAlpha, toAlpha, a, fractionOfRange, 
          h, hc, s, v, colours, RGBColours, rgb, nextR;

      if ( this._weightByNumSeq ) {
        // weight arc length by number of sequences
        deltaAlpha = 2 * Math.PI / this._tree.numSequences * child.numSequences;
      } else {
        // weight arc length by number of species
        deltaAlpha = 2 * Math.PI / this._tree.numSpecies * child.numSpecies;
      }
      toAlpha = fromAlpha + deltaAlpha; 
      a       = toAlpha;

      // stuff to do with the current layer...

      // this is a new layer
      if ( this._layers[depth] === undefined ) {
        this._layers[depth] = { 
          alpha: 0,
          sat:   0.4
        };
      }

      // keep track of the starting angle for the next arc in this layer
      this._layers[depth].alpha += deltaAlpha;

      // set up the next layer out
      if ( this._layers[depth + 1] === undefined ) {
        this._layers[depth + 1] = {
          alpha: fromAlpha,
          sat:   Math.min( this._layers[depth].sat + 0.07, 1.0 )
        };
      }

      // stuff to do with this particular arc

      // store the details of this arc on the node in the data tree from which it 
      // was generated
      child._depth     = depth;
      child._fromAlpha = fromAlpha;
      child._toAlpha   = toAlpha;

      // set up the colours for this arc
      if ( this._colours[child.nodeName] && child.parentNode.nodeName == "root" ) {
        initialH     = this._colours[child.nodeName].minH;
        initialAlpha = fromAlpha;
        rootShare    = deltaAlpha;
        range        = this._colours[child.nodeName].maxH - this._colours[child.nodeName].minH;
        // TODO could pre-calculate range in the colours hash
      }
      fractionOfRange = ( ( ( fromAlpha + deltaAlpha / 2 ) - initialAlpha ) / rootShare ) * range; 

      // calculate the colours for this arc
      h = ( ( initialH + fractionOfRange ) / ( 2 * Math.PI ) );
      s = this._layers[depth].sat;
      v = this._layers[depth].sat;

      // calculate a complementary colour for the sub-tree title
      hc = h + 0.5;
      while ( hc >= 1.0 ) hc -= 1.0;
      while ( hc <  0.0 ) hc += 1.0;

      colours = [
        [ h,       s,       v              ], // main
        [ h,       s * 0.4, v * 1.2        ], // highlight
        [ h * 0.8, s,       v              ], // selected
        [ hc,      s,       v              ]  // adjust the colour of the sub-tree title 
      ];                                      // according to the segment colour

      RGBColours = colours.collect( function( c ) {
        rgb = this._hsvToRgb( c[0], c[1], c[2] );
        return "rgb("+rgb[0]+","+rgb[1]+","+rgb[2]+")";
      }.bind(this) );

      child._colour          = RGBColours[0];
      child._highlightColour = RGBColours[1];
      child._selectedColour  = RGBColours[2];
      child._titleColour     = RGBColours[3];

      // store the arc in a data structure with nested arrays. The outer array stores
      // the layers, the inner arrays store the arcs in a particular layer
      if ( depth < this._depthLimit ) {


        if ( this._arcs[depth] === undefined ) {
          this._arcs[depth] = [];
        }
        this._arcs[depth].push( child );
      }

      // keep walking the tree...
      nextR = r + this._layerWidth + 2;
   
      if ( child.children !== undefined && depth < this._depthLimit ) {
        this._buildGraph( child.children, nextR, depth + 1, initialH, initialAlpha, rootShare, range );
      } else {
        this._layers[depth + 1].alpha = toAlpha;
      }

    }.bind(this) );

    return this;
  },

  //----------------------------------------------------------------------------
  /**
   * Actually draws the specified arc on a canvas. Takes a colour parameter, so
   * that the method can be used to draw arcs in various colours, but if no
   * colour is specified, the method defaults to drawing in the colour extracted
   * form the arc itself.
   *
   * @private
   * @returns {Sunburst} reference to this object
   */
  _drawArc: function( arc, colour ) {

    // calculate the radius and angle subtended
    var r  = ( this._layerWidth + this._layerSeparation ) * arc._depth,
        da = arc._toAlpha - arc._fromAlpha, // delta alpha
        ma = arc._fromAlpha + ( da / 2 ),   // midpoint alpha

        // shim = 0.1 * ( 2 * Math.PI ) / 360, // leave a gap of 0.1 degree between arcs
        shim = 0.0017453292519943296,       // this we can pre-calculate...

        to = Math.max( arc._fromAlpha, arc._toAlpha - shim ),

        l = r * da, // arc length

        label = arc.nodeName.match( /([\w\s]*)(?:\(.*?\))?/ )[1]
                            .replace( /\n/g, ' ' ),
        lines, labelHeight, labelWidth;

    if ( to - arc._fromAlpha < 0.001 ) {
      return;
    }

    // if no colour is specified, default to the colour of the arc itself
    if ( colour !== undefined ) {
      this._ctx.strokeStyle = colour;
    } else {
      this._ctx.strokeStyle = arc._colour;
    }

    // draw the arc
    this._ctx.beginPath();
    this._ctx.arc( 0, 0, r, arc._fromAlpha, to, false );
    this._ctx.stroke();

    // draw the text label, if it fits within the arc. Check to see if we should
    // orient the label radially or tangential to the arc
    if ( this._radialLabels ) {
      if ( this._ctx.measureText( label ).width < this._layerWidth &&
           l > 5 ) {
        this._ctx.save();
        if ( ma > Math.PI / 2 && ma < 3 * Math.PI / 2 ) {
          this._ctx.rotate( ma + Math.PI );
          this._ctx.translate( 0 - r, 0 );
        } else {
          this._ctx.rotate( ma );
          this._ctx.translate( r, 0 );
        }
        this._ctx.fillText( label, 0, 0 );
        this._ctx.restore();
      }
    } else {
      // tangential labels
      if ( this._ctx.measureText( label ).width + 4 < l ) {
        this._ctx.save();
        if ( ma < Math.PI ) {
          this._ctx.rotate( ma + Math.PI );
          this._ctx.translate( 0 - ( r - this._layerWidth * 0.1 ), 0 );
        } else {
          this._ctx.rotate( ma );
          this._ctx.translate( r - this._layerWidth * 0.1, 0 );
        }
        // this._ctx.rotate( ma );
        // this._ctx.translate( r - this._layerWidth * 0.1, 0 );
        this._ctx.rotate( Math.PI / 2 );
        this._ctx.fillText( label, 0, 0 );
        this._ctx.restore();
      } else if ( label.indexOf(" ") >= 0 ) {
        // label is too wide. Try splitting on space and then see if it fits

        // only show first two terms
        lines = label.split( " " ).slice(0,2);

        // calculate the height of the split text
        // labelHeight = ( ( lines.size() - 2 ) * 5 ) + 2; // lots of fudge there...
        labelHeight = lines.size() * 10;
        // console.debug( "label: |%s|, lines.size(): |%d|, labelHeight: |%d|", label, lines.size(), labelHeight );

        // and calculate the width of the longest line
        labelWidth = lines.map( function(line) { 
          return this._ctx.measureText(line).width;
        }.bind(this) ).max();
        // console.debug( "label: |%s|, longest line: |%d|, arc length: |%d|", label, labelWidth, l );

        if ( labelHeight < this._layerWidth && 
             labelWidth + 4 < l ) {
          this._ctx.save();
          if ( ma < Math.PI ) {
            this._ctx.rotate( ma + Math.PI );
            this._ctx.translate( 0 - ( r - ( labelHeight / 2 ) + 4 ), 0 );
          } else {
            this._ctx.rotate( ma );
            this._ctx.translate( r + ( labelHeight / 2 ) - 4, 0 );
          }
          // this._ctx.rotate( ma );
          // this._ctx.translate( r + labelHeight, 0 );
          this._ctx.rotate( Math.PI / 2 );
          lines.each( function(line,i) {
            this._ctx.fillText( line, 0, i * 10 );
          }.bind(this) );
          this._ctx.restore();
        }
      }
    }

    return this;
  },
 
  //----------------------------------------------------------------------------
  // this method isn't currently used, but it draws a sort of tree graph,
  // rather than the arcs

  // _drawBranch: function( child ) {

  //   var rInner    = ( this._layerWidth + this._layerSeparation ) * ( child._depth - 1 ),
  //       rOuter    = ( this._layerWidth + this._layerSeparation ) * child._depth + this._layerSeparation,
  //       midAlphas = [],
  //       fromAlpha, toAlpha,
  //       ma, x0, y0, x1, y1;

  //   if ( child.children !== undefined && child.children.length > 1 ) {

  //     // fromAlpha = child.children
  //     //                  .pluck("_fromAlpha")
  //     //                  .min();
  //     //   toAlpha = child.children
  //     //                  .pluck("_toAlpha")
  //     //                  .max();

  //     child.children.each( function(grandchild) {
  //       midAlphas.push( grandchild._fromAlpha + ( ( grandchild._toAlpha - grandchild._fromAlpha ) / 2 ) );
  //     } );

  //     fromAlpha = midAlphas.min();
  //       toAlpha = midAlphas.max();

  //     this._ctx.beginPath();
  //     // console.debug( "radius: %d, fromAlpha: %d, toAlpha: %d, child: ", 
  //     //                r + this._layerWidth, fromAlpha, toAlpha, child );
  //     this._ctx.arc( 0, 0, rOuter, fromAlpha, toAlpha, false );
  //     this._ctx.stroke(); 

  //   } else {
  //   
  //     fromAlpha = child._fromAlpha;
  //     toAlpha   = child._toAlpha;

  //   }

  //   ma = fromAlpha + ( ( toAlpha - fromAlpha ) /  2 ); // midpoint alpha

  //   x0 = rInner * Math.cos(ma);
  //   y0 = rInner * Math.sin(ma);
  //   x1 = rOuter * Math.cos(ma);
  //   y1 = rOuter * Math.sin(ma);

  //   this._ctx.beginPath();
  //   this._ctx.moveTo( x0, y0 );
  //   this._ctx.lineTo( x1, y1 );
  //   this._ctx.stroke();
  // },

  //----------------------------------------------------------------------------
  // like the _drawBranch method, but puts the vertices in different places
  // relative to the arcs
 
  // _drawCentredBranch: function( child ) {

  //   var r         = ( child._depth - 1 ) * this._layerWidth,

  //       midAlphas = [], 
  //       
  //       fromAlpha, toAlpha,
  //       ma, x0, y0, x1, y1;

  //   if ( child.children !== undefined && child.children.length > 1 ) {

  //     child.children.each( function(grandchild) {
  //       midAlphas.push( grandchild._fromAlpha + ( ( grandchild._toAlpha - grandchild._fromAlpha ) / 2 ) );
  //     } );

  //     fromAlpha = midAlphas.min();
  //       toAlpha = midAlphas.max();

  //     this._ctx.beginPath();
  //     this._ctx.arc( 0, 0, r + this._layerWidth, fromAlpha, toAlpha, false );
  //     this._ctx.stroke(); 

  //   } else {
  //   
  //     fromAlpha = child._fromAlpha;
  //     toAlpha   = child._toAlpha;

  //   }

  //   ma = fromAlpha + ( ( toAlpha - fromAlpha ) /  2 ); // midpoint alpha

  //   x0 = r * Math.cos(ma);
  //   y0 = r * Math.sin(ma);
  //   x1 = ( r + this._layerWidth ) * Math.cos(ma);
  //   y1 = ( r + this._layerWidth ) * Math.sin(ma);

  //   this._ctx.beginPath();
  //   this._ctx.moveTo( x0, y0 );
  //   this._ctx.lineTo( x1, y1 );
  //   this._ctx.stroke();
  // },

  //----------------------------------------------------------------------------
  /**
   * Given a mouse event originating from the main tree canvas, this method 
   * takes the X,Y position of the event and tries to find the arc over which
   * the mouse is moving. Returns the arc data structure 
   * (<code>SunburstNode</code>) itself, if an arc was found.
   *
   * @private
   * @param e mouse event from tree canvas
   * @returns {SunburstNode} data structure for the active arc
   */
  _findArc: function(e) {

    // given the X, Y position of the cursor, we need to calculate the radius 
    // of that point and then convert that to a layer (depth). We also need to
    // get the angle between the positive X-axis and that radius, so that we 
    // can find the arc that's highlighted. Because there could potentially be
    // an awful lot of arcs in the outermost layer, we use a binary search to
    // cut down the time spent searching

    var pointer = e.pointer(),
        offsets = this._treeCanvas.cumulativeOffset(),
        x       = pointer.x - offsets[0] - this._centreX,
        y       = pointer.y - offsets[1] - this._centreY,
                
        r       = Math.sqrt( Math.pow(x,2) + Math.pow(y,2) ),
        d       = Math.round( r / ( this._layerWidth + this._layerSeparation ) ),
        layer   = this._arcs[d],

        alpha, low, high, probe, limit, slot;

    // let's fail fast: if layer is undefined, the mouse is outside of the 
    // circle, or on the root node. In either case, we're done here; there's
    // no arc to return
    if ( layer === undefined ) {
      return;
    }

    // now we know we're over one of the arcs...
    
    // atan2 will return alpha in the range -PI to +PI, but we need it in the
    // range 0 to 2PI
    alpha   = Math.atan2( y, x );
    if ( alpha < 0 ) {
      alpha += 2 * Math.PI;
    }

    // this is the binary search loop. Because I don't trust my code as far
    // as I can throw it, there's a limiter on the loop...

    limit   = 0;
    high    = layer.length;
    low     = -1;

    while ( high - low > 1 ) {
      probe = Math.floor( ( high + low ) / 2 );
      // console.debug( "probe: %d, slice: %s", probe, $A( $R( low, high ) ).join(" ") );
      // console.debug( "probe: %d, toAlpha: %d, alpha: %d", probe, layer[probe]._toAlpha, alpha );
      if ( layer[probe]._toAlpha > alpha ) {
        high = probe;
      } else {
        low = probe;
      }

      if ( limit++ > 100000 ) {
        return;
      }
    }

    // clamp the upper limit on alpha so that we get the correct arc when the
    // pointer is close to the X-origin
    slot = Math.max( 0, high );

    return layer[slot];
  },

  //----------------------------------------------------------------------------
  /**
   * Handles the &quot;mousemove&quot; events from the main tree canvas.
   * Highlights the hovered arc by changing its colour to a lighter version of
   * the original colour.
   *
   * @private
   * @param e mouse event from tree canvas
   */
  _highlightNode: function(e) {

    var arc = this._findArc( e ),
        tipTitle;

    if ( arc === undefined ) {
      // the mouse is not over any arc; tidy up and we're done

      // hide the tooltip
      this._treeCanvas.prototip.hide();

      // return the highlighted arc to its original colour
      if ( this._highlightedArc ) {
        if ( this._highlightedArc.isSelected ) {
          this._drawArc( this._highlightedArc, this._selectedNodeColour );
        } else {
          this._drawArc( this._highlightedArc, this._highlightedArc._colour );
        }
      }

      // clear the highlighted arc, so that we start afresh with the next 
      // highlighted arc
      this._highlightedArc = null;

      return;
    }

    // at this point we know that the mouse is over an arc

    // highlight the segment
    if ( this._highlightedArc === arc ) {
      return;
    }

    // return the highlighted arc to its original colour
    if ( this._highlightedArc ) {
      if ( this._highlightedArc.isSelected ) {
        this._drawArc( this._highlightedArc, this._selectedNodeColour );
      } else {
        this._drawArc( this._highlightedArc, this._highlightedArc._colour );
      }
    }

    // colour the new arc and store it
    if ( arc.isSelected ) {
      this._drawArc( arc, this._highlightedSelectedNodeColour );
    } else {
      this._drawArc( arc, arc._highlightColour );
    }
    this._highlightedArc = arc;

    // update the sub-tree
    this._drawSubTree( arc );

    // test the node name to see if it's designating a node that doesn't have
    // one of the main eight levels (e.g. "(No order)"). In that case we leave
    // it untouched, both otherwise we'll add the level after the node name.
    if ( this._noLevelRE.test( arc.nodeName ) ) {
      tipTitle = arc.nodeName;
    } else {
      tipTitle = arc.nodeName + " [" + this._levels[arc._depth] + "]"; 
    }
    
    // update the tip
    this._tipTitle.update( tipTitle );
    this._tipNumSeq.update( arc.numSequences + ( arc.numSequences > 1 ? " sequences" : " sequence" ) );
    this._tipNumSpecies.update( arc.numSpecies );

  },

  //----------------------------------------------------------------------------
  /**
   * Handles clicks on arcs. A click selects the clicked node and the subtree
   * below that node.
   *
   * @private
   * @param e mouse event from tree canvas
   */
  _selectNode: function(e) {

    var arc = this._findArc( e );

    if ( arc === undefined ) {
      return;
    }

    if ( arc.isSelected ) {
      this._drawArc( arc, arc._colour );
      this._selectedArcs.unset( arc.id );
    } else {
      this._drawArc( arc, this._highlightedSelectedNodeColour );
      this._selectedArcs.set( arc.id, arc );
    }

    arc.isSelected = ! arc.isSelected
    this._setSubTreeSelectionStatus( arc, arc.isSelected );
    
    // walk back up the tree from the current node, deselecting parent nodes
    // that have no other selected children
    this._deselectParentNode( arc );

    // notify observers of a change in the number of selected sequences and 
    // species. Because the cascade has a delay, we need to wait until it's 
    // done before calculating the counts and firing the event. There are 
    // only 8 levels in the sunburst, but we'll add an extra buffer, just 
    // to make sure we're done before firing
    setTimeout( this._fireSelectionChangeEvent.bind(this), 9 * this._selectAnimationDelay );
  },

  //----------------------------------------------------------------------------
  /**
   * Walks up the tree from the de-selected node and de-selects any parents
   * that have no other selected child nodes.
   *
   * @private
   * @param arc node from which to walk up the tree
   */
  _deselectParentNode: function( arc ) {

    var p = arc.parentNode;
    // console.debug( "Sunburst._deselectParentNode: checking node %s", p.nodeName );

    if ( ! p.isSelected ) {
      // parent isn't selected itself, so we're done
      // console.debug( "Sunburst._deselectParentNode:   node is not selected; done" );
      return;
    }

    // the parent IS selected; see if it has selected children other than
    // the current arc

    if ( p.children.size() > 1 ) {
      // console.debug( "Sunburst._deselectParentNode:   parent node has > 1 children" );
      // parent has other children besides this one; see if any of those are selected
      var selectedSibling = p.children.detect( function( child ) { return child.isSelected; } );

      if ( selectedSibling !== undefined ) {
        // console.debug( "Sunburst._deselectParentNode:   parent node has other selected children; done" );
        // the parent of the current arc has another selected child, so it 
        // should remain selected itself
        return;
      }
      // console.debug( "Sunburst._deselectParentNode:   parent node has NO other selected children" );
    }

    // parent has only a single child node (this one) or has no selected children
    // console.debug( "Sunburst._deselectParentNode:   de-selecting parent node" );
    p.isSelected = false;
    this._drawArc( p, p._colour );
    this._selectedArcs.unset( p.id );

    // because it could be confusing for the user to see a whole slice of the
    // sunburst being immediately unselected, we add a short delay as we walk 
    // back up the tree, so that the parent nodes will be deselected slightly
    // more gradually. Need to work around the inability of IE to pass parameters
    // using setTimeout...
    if ( Prototype.Browser.IE ) {
      this._deselectParentNode( p );
    } else {
      setTimeout( this._deselectParentNode.bind(this), this._selectAnimationDelay,
                  p );
    }
  },

  //----------------------------------------------------------------------------
  /**
   * Walks the list of currently selected nodes and calculates the total number
   * of selected sequences and species, then fires a custom event to notify
   * listeners of the selection change. The event carries the counts as its
   * payload:
   *
   *   { numSequences: N, numSpecies: M }
   *
   * @private
   */
  _fireSelectionChangeEvent: function() {

    var seqCount = 0,
        speciesCount = 0,
        accessions = new Hash();

    this.getSelected().each( function( arc ) {
      if ( arc.children.size() == 0 ) {
        speciesCount += Number(arc.numSpecies);
      }
      if ( arc.sequences !== undefined ) {
        arc.sequences.each( function( seq ) {
          accessions.set( seq.seqAcc, null );
        } );
      }
    } );

    seqCount = accessions.keys().size();

    this._treeParentEl.fire( "sunburst:selectionChange", {
      numSequences: seqCount,
      numSpecies:   speciesCount
    } );

  },
  
  //----------------------------------------------------------------------------
  /**
   * Walks down the species tree from the supplied node and sets the selection
   * status to the supplied value.
   *
   * @private
   * @param arc node from which to walk down the tree
   * @param selectionStatus the status to set on the sub-tree nodes
   */
  _setSubTreeSelectionStatus: function( arc, selectionStatus ) {

    arc.children.each( function( child ) {

      if ( child.isSelected == selectionStatus ) {
        return;
      }
      
      child.isSelected = selectionStatus;
      if ( selectionStatus ) {
        child.isSelected = selectionStatus;
        this._selectedArcs.set( child.id, child );
        this._drawArc( child, this._selectedNodeColour );
      } else {
        this._selectedArcs.unset( child.id );
        this._drawArc( child, child._colour );
      }

      // because it could be confusing for the user to see a whole slice of the
      // sunburst being immediately unselected, we add a short delay as we walk 
      // back up the tree, so that the parent nodes will be deselected slightly
      // more gradually. Need to work around the inability of IE to pass parameters
      // using setTimeout...
      if ( Prototype.Browser.IE ) {
        this._setSubTreeSelectionStatus( child, selectionStatus );
      } else {
        setTimeout( this._setSubTreeSelectionStatus.bind(this), this._selectAnimationDelay, 
                    child, selectionStatus );
      }

    }.bind(this) );

  },

  //----------------------------------------------------------------------------
  /**
   * Draws the sub-tree for a given node in the main tree.
   *
   * @private
   * @param child the reference node in the tree
   */
  _drawSubTree: function( child ) {

    // get rid of the help message that is present initially in the sub-tree
    // wrapper div
    if ( this._subTreeHelp ) {
      this._subTreeHelp.remove();
      this._subTreeHelp = null;
    }

    var xAdd, yAdd, text, colour,
        parentNode, limit = 0, subTree,
        gap, textWidth, charWidth, numVisibleChars;

    if ( ! this._subTreeWrapperDiv.visible() ) {
      this._subTreeWrapperDiv.show();
    }

    this._stctx.clearRect( 0, 0, this._subTreeWidth, this._subTreeHeight );

    // set the name and the colours in the sub-tree
    this._speciesNameDiv.update( child.nodeName )
                        .setStyle( { backgroundColor: child._colour,
                                     color:           child._titleColour } );

    // walk back up the tree and get each parent node in turn
    parentNode = child.parentNode;
    limit = 0;
    subTree = [ child ];
    while ( parentNode !== undefined && limit++ < 1000 ) {
      subTree.push( parentNode );
      parentNode = parentNode.parentNode;
    }
    subTree = subTree.without( subTree.last() ); // get rid of the root node

    this._stctx.fillText( "Root", 7, 16 );

    subTree.reverse().each( function( node, i ) {
      text   = node.nodeName;
      colour = node._colour;

      xAdd = 20.5 + i * 10;
      yAdd = 20.5 + i * 12 + 10;
      
      this._stctx.beginPath();
      this._stctx.moveTo( xAdd, yAdd - 12 );
      this._stctx.lineTo( xAdd, yAdd );
      this._stctx.moveTo( xAdd, yAdd );
      this._stctx.lineTo( xAdd + 15, yAdd );
      this._stctx.closePath();
      this._stctx.stroke();
      
      // make sure there's space to write the species name in the last row of
      // the sub-tree
      gap = this._subTreeWidth - xAdd - 18; // the space left to write the last line
      textWidth = this._stctx.measureText( text ).width + 10; // add a 10px fudge-factor
      if ( textWidth > gap ) {
        // work out how many characters we can actually get into the last line and
        // truncate the string appropriately
        charWidth = textWidth / text.length;
        numVisibleChars = Math.floor( gap / charWidth );
        this._stctx.fillText( text.truncate( numVisibleChars, "..." ), xAdd + 18, yAdd + 3 );
      } else {
        this._stctx.fillText( text, xAdd + 18, yAdd + 3 );
      }

    }.bind(this) );
    
    // last level gets a dotted line if the node has children
    if ( child.children.length > 0 ) {
      xAdd += 10;
      this._stctx.beginPath();
      this._stctx.moveTo( xAdd, yAdd );
      this._stctx.lineTo( xAdd, yAdd +  3 );
      this._stctx.moveTo( xAdd, yAdd +  6 );
      this._stctx.lineTo( xAdd, yAdd +  9 );
      this._stctx.moveTo( xAdd, yAdd + 12 );
      this._stctx.lineTo( xAdd, yAdd + 15 );
      this._stctx.lineTo( xAdd +  3, yAdd + 15 );
      this._stctx.moveTo( xAdd +  6, yAdd + 15 );
      this._stctx.lineTo( xAdd +  9, yAdd + 15 );
      this._stctx.moveTo( xAdd + 12, yAdd + 15 );
      this._stctx.lineTo( xAdd + 15, yAdd + 15 );
      this._stctx.closePath();
      this._stctx.stroke();
    }
  },

  //----------------------------------------------------------------------------
  /**
   * Converts an HSV colour into an RGB colour.
   * 
   * @private
   * @param h hue
   * @param s saturation
   * @param v value
   * @returns {Array} reference to array containing R, G and B values
   */
  _hsvToRgb: function( h, s, v ) {
    var r, g, b,
        i,
        f, p, q, t;
    
    if ( s === 0 ) {
      // Achromatic (grey)
      r = g = b = v;
      return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
    }
    
    h *= 360;
    h /= 60; // sector 0 to 5
    i = Math.floor(h);
    f = h - i; // factorial part of h
    p = v * (1 - s);
    q = v * (1 - s * f);
    t = v * (1 - s * (1 - f));

    switch(i) {
      case 0:  r = v; g = t; b = p; break;
      case 1:  r = q; g = v; b = p; break;
      case 2:  r = p; g = v; b = t; break;
      case 3:  r = p; g = q; b = v; break;
      case 4:  r = t; g = p; b = v; break;
      default: r = v; g = p; b = q; // case 5:
    }
    
    return [ Math.round(r * 255), Math.round(g * 255), Math.round(b * 255) ];
  }

  //----------------------------------------------------------------------------

} );

