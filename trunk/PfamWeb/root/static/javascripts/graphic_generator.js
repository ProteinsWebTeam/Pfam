
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $R, $w, $break, Class, console, Element, Hash, Event, document,
  window, G_vmlCanvasManager, Template, Tip */

// spoof a console, if necessary, so that we can run in IE (<8) without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript object to control the drawing of domain graphics on the 
// domain graphics generator page.
//
// jt6 20110303 WTSI
//
// $Id$
//
// Copyright (c) 2011: Genome Research Ltd.
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

var GraphicGenerator = Class.create( {
  
  //----------------------------------------------------------------------------
  //- class variables ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // these are example sequence strings
  _smallExample: '{ \r\
  "length" : "534",  \r\
  "regions" : [  \r\
    {  \r\
      "type" : "pfama",  \r\
      "text" : "Peptidase_S8",  \r\
      "colour" : "#2dcfff",  \r\
      "display": "true", \r\
      "startStyle" : "curved", \r\
      "endStyle" : "curved", \r\
      "start" : "159", \r\
      "end" : "361",  \r\
      "aliEnd" : "350",  \r\
      "aliStart" : "163"\r\
    }, \r\
    { \r\
      "type" : "pfama", \r\
      "text" : "PA", \r\
      "colour" : "#ff5353", \r\
      "display" : true, \r\
      "startStyle" : "jagged", \r\
      "endStyle" : "curved", \r\
      "start" : "388",\r\
      "end" : "469", \r\
      "aliEnd" : "469", \r\
      "aliStart" : "396"\r\
    } \r\
  ] \r\
}',

  _largeExample: '{ \r\
  "length" : "950", \r\
  "regions" : [ \r\
    { \r\
      "modelStart" : "5", \r\
      "modelEnd" : "292", \r\
      "colour" : "#2dcf00", \r\
      "endStyle" : "jagged", \r\
      "startStyle" : "jagged", \r\
      "display" : true, \r\
      "end" : "361", \r\
      "aliEnd" : "361", \r\
      "href" : "/family/PF00082", \r\
      "text" : "Peptidase_S8", \r\
      "modelLength" : "307", \r\
      "metadata" : { \r\
        "scoreName" : "e-value", \r\
        "score" : "1.3e-38", \r\
        "description" : "Subtilase family", \r\
        "accession" : "PF00082", \r\
        "end" : "587", \r\
        "database" : "pfam", \r\
        "aliEnd" : "573", \r\
        "identifier" : "Peptidase_S8", \r\
        "type" : "Domain", \r\
        "aliStart" : "163", \r\
        "start" : "159" \r\
      }, \r\
      "type" : "pfama", \r\
      "aliStart" : "163", \r\
      "start" : "159" \r\
    }, \r\
    { \r\
      "modelStart" : "5", \r\
      "modelEnd" : "292", \r\
      "colour" : "#2dcf00", \r\
      "endStyle" : "jagged", \r\
      "startStyle" : "jagged", \r\
      "display" : true, \r\
      "end" : "587", \r\
      "aliEnd" : "573", \r\
      "href" : "/family/PF00082", \r\
      "text" : "Peptidase_S8", \r\
      "modelLength" : "307", \r\
      "metadata" : { \r\
        "scoreName" : "e-value", \r\
        "score" : "1.3e-38", \r\
        "description" : "Subtilase family", \r\
        "accession" : "PF00082", \r\
        "end" : "587", \r\
        "database" : "pfam", \r\
        "aliEnd" : "573", \r\
        "identifier" : "Peptidase_S8", \r\
        "type" : "Domain", \r\
        "aliStart" : "163", \r\
        "start" : "159" \r\
      }, \r\
      "type" : "pfama", \r\
      "aliStart" : "470", \r\
      "start" : "470" \r\
    }, \r\
    { \r\
      "modelStart" : "12", \r\
      "modelEnd" : "100", \r\
      "colour" : "#ff5353", \r\
      "endStyle" : "curved", \r\
      "startStyle" : "jagged", \r\
      "display" : true, \r\
      "end" : "469", \r\
      "aliEnd" : "469", \r\
      "href" : "/family/PF02225", \r\
      "text" : "PA", \r\
      "modelLength" : "100", \r\
      "metadata" : { \r\
        "scoreName" : "e-value", \r\
        "score" : "7.1e-09", \r\
        "description" : "PA domain", \r\
        "accession" : "PF02225", \r\
        "end" : "469", \r\
        "database" : "pfam", \r\
        "aliEnd" : "469", \r\
        "identifier" : "PA", \r\
        "type" : "Family", \r\
        "aliStart" : "385", \r\
        "start" : "362" \r\
      }, \r\
      "type" : "pfama", \r\
      "aliStart" : "385", \r\
      "start" : "362" \r\
    }, \r\
    { \r\
      "modelStart" : "1", \r\
      "modelEnd" : "112", \r\
      "colour" : "#5b5bff", \r\
      "endStyle" : "curved", \r\
      "startStyle" : "curved", \r\
      "display" : true, \r\
      "end" : "726", \r\
      "aliEnd" : "726", \r\
      "href" : "/family/PF06280", \r\
      "text" : "DUF1034", \r\
      "modelLength" : "112", \r\
      "metadata" : { \r\
        "scoreName" : "e-value", \r\
        "score" : "1.1e-13", \r\
        "description" : "Fn3-like domain (DUF1034)", \r\
        "accession" : "PF06280", \r\
        "end" : "726", \r\
        "database" : "pfam", \r\
        "aliEnd" : "726", \r\
        "identifier" : "DUF1034", \r\
        "type" : "Domain", \r\
        "aliStart" : "613", \r\
        "start" : "613" \r\
      }, \r\
      "type" : "pfama", \r\
      "aliStart" : "613", \r\
      "start" : "613" \r\
    } \r\
  ], \r\
  "markups" : [ \r\
    { \r\
      "lineColour" : "#ff0000", \r\
      "colour" : "#000000", \r\
      "display" : true, \r\
      "end" : "470", \r\
      "v_align" : "top", \r\
      "metadata" : { \r\
        "database" : "pfam", \r\
        "type" : "Link between discontinous regions", \r\
        "end" : "470", \r\
        "start" : "361" \r\
      }, \r\
      "type" : "Nested", \r\
      "start" : "361" \r\
    }, \r\
    { \r\
      "lineColour" : "#333333", \r\
      "colour" : "#e469fe", \r\
      "display" : true, \r\
      "residue" : "S", \r\
      "headStyle" : "diamond", \r\
      "v_align" : "top", \r\
      "type" : "Pfam predicted active site", \r\
      "metadata" : { \r\
        "database" : "pfam", \r\
        "description" : "S Pfam predicted active site", \r\
        "start" : "538" \r\
      }, \r\
      "start" : "538" \r\
    }, \r\
    { \r\
      "lineColour" : "#333333", \r\
      "colour" : "#e469fe", \r\
      "display" : true, \r\
      "residue" : "D", \r\
      "headStyle" : "diamond", \r\
      "v_align" : "top", \r\
      "type" : "Pfam predicted active site", \r\
      "metadata" : { \r\
        "database" : "pfam", \r\
        "description" : "D Pfam predicted active site", \r\
        "start" : "185" \r\
      }, \r\
      "start" : "185" \r\
    }, \r\
    { \r\
      "lineColour" : "#333333", \r\
      "colour" : "#e469fe", \r\
      "display" : true, \r\
      "residue" : "H", \r\
      "headStyle" : "diamond", \r\
      "v_align" : "top", \r\
      "type" : "Pfam predicted active site", \r\
      "metadata" : { \r\
        "database" : "pfam", \r\
        "description" : "H Pfam predicted active site", \r\
        "start" : "235" \r\
      }, \r\
      "start" : "235" \r\
    } \r\
  ], \r\
  "metadata" : { \r\
    "database" : "uniprot", \r\
    "identifier" : "Q560V8_CRYNE", \r\
    "organism" : "Cryptococcus neoformans (Filobasidiella neoformans)", \r\
    "description" : "Putative uncharacterized protein", \r\
    "taxid" : "5207", \r\
    "accession" : "Q560V8" \r\
  }, \r\
  "motifs" : [ \r\
    { \r\
      "colour" : "#ffa500", \r\
      "metadata" : { \r\
        "database" : "Phobius", \r\
        "type" : "sig_p", \r\
        "end" : "23", \r\
        "start" : "1" \r\
      }, \r\
      "type" : "sig_p", \r\
      "display" : true, \r\
      "end" : 23, \r\
      "start" : 1 \r\
    }, \r\
    { \r\
      "colour" : "#00ffff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "2.5100", \r\
        "end" : "21", \r\
        "start" : "3" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : false, \r\
      "end" : 21, \r\
      "start" : 3 \r\
    }, \r\
    { \r\
      "colour" : "#86bcff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "1.4900", \r\
        "end" : "156", \r\
        "start" : "134" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : true, \r\
      "end" : "156", \r\
      "start" : "134" \r\
    }, \r\
    { \r\
      "colour" : "#00ffff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "2.0200", \r\
        "end" : "187", \r\
        "start" : "173" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : false, \r\
      "end" : "187", \r\
      "start" : "173" \r\
    }, \r\
    { \r\
      "colour" : "#00ffff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "2.0800", \r\
        "end" : "218", \r\
        "start" : "207" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : false, \r\
      "end" : "218", \r\
      "start" : "207" \r\
    }, \r\
    { \r\
      "colour" : "#00ffff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "2.1300", \r\
        "end" : "231", \r\
        "start" : "220" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : false, \r\
      "end" : "231", \r\
      "start" : "220" \r\
    }, \r\
    { \r\
      "colour" : "#00ffff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "2.0000", \r\
        "end" : "554", \r\
        "start" : "538" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : false, \r\
      "end" : "554", \r\
      "start" : "538" \r\
    }, \r\
    { \r\
      "colour" : "#86bcff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "1.9100", \r\
        "end" : "590", \r\
        "start" : "578" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : true, \r\
      "end" : "590", \r\
      "start" : 588 \r\
    }, \r\
    { \r\
      "colour" : "#00ffff", \r\
      "metadata" : { \r\
        "database" : "seg", \r\
        "type" : "low_complexity", \r\
        "score" : "1.7600", \r\
        "end" : "831", \r\
        "start" : "822" \r\
      }, \r\
      "type" : "low_complexity", \r\
      "display" : false, \r\
      "end" : "831", \r\
      "start" : "822" \r\
    }, \r\
    { \r\
      "href" : "/pfamb/PB075017", \r\
      "colour" : [ \r\
        "#ff7ff0", \r\
        "#f2ff7f", \r\
        "#7ff2ff" \r\
      ], \r\
      "metadata" : { \r\
        "database" : "pfam", \r\
        "identifier" : "Pfam-B_75017", \r\
        "type" : "Pfam-B", \r\
        "accession" : "PB075017", \r\
        "end" : "949", \r\
        "start" : "791" \r\
      }, \r\
      "type" : "pfamb", \r\
      "display" : true, \r\
      "end" : "949", \r\
      "start" : "791" \r\
    } \r\
  ] \r\
}',

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function() {

    this._pg = new PfamGraphic();

    // set up sequence field
    $("seq").focus();
    $("seq").select();

    // add listeners to the various buttons
    $("large").observe( "click", function() {
      $("seq").value = this._largeExample;
    }.bind(this) );
    $("small").observe( "click", function() {
      $("seq").value = this._smallExample;
    }.bind(this) );
    $("submit").observe( "click", this.generate.bind(this) );
    $("clear").observe( "click", this.clear.bind(this) );
  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------
  
  generate: function() {

    // get ride of the "no graphic yet" message
    if ( $("none") ) {
      $("none").remove();
    }

    // hide any previous error messages and remove the previous canvas element
    $("errors").hide();
    if ( $("dg").select("canvas").size() > 0 ) {
      $("dg").select("canvas").first().remove();
    }

    // see if we can turn the sequence string into an object
    var sequence;
    try {
      eval( "sequence = " + $("seq").getValue() );
    } catch ( e ) {
      $("error").update( e );
      $("errors").show();
      return;
    }

    // set up the PfamGraphic object
    this._pg.setParent( "dg" );

    this._pg.setImageParams( {
      xscale: $F("xscale"),
      yscale: $F("yscale")
    } );

    // render the sequence
    try {
      this._pg.setSequence( sequence );
      this._pg.render();
    } catch ( e ) {
      $("error").update( e );
      $("errors").show();
      return;
    }

    // document.observe('keydown', function(k) {
    //   if (k.ctrlKey) return;
    //     alert('ctrl');
    // } ); 
  },

  //----------------------------------------------------------------------------

  clear: function() {
    $("errors").hide();
    $("seq").setValue("");
    $("seq").focus()
    $("seq").select();
  }

  //----------------------------------------------------------------------------

} );

