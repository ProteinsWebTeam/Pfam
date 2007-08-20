
// family.js
// jt6 20060721 WTSI
//
// javascript glue for the family section
//
// $Id: family.js,v 1.25 2007-08-20 09:02:41 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
// 
// This is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//  
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//  
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
// or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

// this will make the ajax calls for the family page components

function familyPostLoad() {
  // structure image
  if( typeof( loadOptions.si.uri ) != "undefined" ) {
    new Ajax.Request( loadOptions.si.uri,
                      { method:     'get', 
                        parameters: loadOptions.si.params,
                        onSuccess:  siSuccess
                        // not even bothering with a failure callback...
                      } );
  }
  
  // domain graphics
  if( typeof( loadOptions.dg.uri ) != "undefined" ) {
    new Ajax.Request( loadOptions.dg.uri,
                      { method:     'get', 
                        parameters: loadOptions.dg.params,
                        onSuccess:  dgSuccess,
                        onFailure:  dgFailure
                      } );
  }
	
  // species tree
  if( typeof( loadOptions.st.uri ) != "undefined" ) {
    new Ajax.Request( loadOptions.st.uri,
                    { method:     'get', 
                      parameters: loadOptions.st.params,
                      onSuccess:  stSuccess,
                      onFailure:  stFailure
                    } );
  }

  // alignment tree
  if( typeof( loadOptions.at.uri ) != "undefined" ) {
    new Ajax.Request( loadOptions.at.uri,
                      { method:     'get', 
                        parameters: loadOptions.at.params,
                        onSuccess:  atSuccess,
                        onFailure:  atFailure
                      } );
  }
  // clan structure tab
  if( typeof( loadOptions.fstruc.uri ) != "undefined" ) {
     new Ajax.Request( loadOptions.fstruc.uri,
                       { method:     'get', 
                         parameters: loadOptions.fstruc.params,
                         onSuccess:  fstrucSuccess,
                         onFailure:  fstrucFailure
                       } );
  }
  // coloured alignment
  if( typeof( loadOptions.ca.uri ) != "undefined" ) {
    new Ajax.Request( loadOptions.ca.uri,
                      { method:     'get', 
                        parameters: loadOptions.ca.params,
                        onSuccess:  caSuccess,
                        onFailure:  caFailure
                      } );
  }
}

//------------------------------------------------------------
// callback for the structure image call

function siSuccess( oResponse ) {
  Element.update( $("siph"), oResponse.responseText );
}

//------------------------------------------------------------
// callbacks for the domain graphics generation call

function dgSuccess( oResponse ) {
  Element.update( $("dgph"), oResponse.responseText );
}
function dgFailure() {
  Element.update( $("dgph"), "Domain graphics loading failed." );
}

//------------------------------------------------------------
//- alignment tree methods -----------------------------------
//------------------------------------------------------------

// callbacks for the alignment tree generation call

function atSuccess( oResponse ) {
  $("alignmentTree").update( oResponse.responseText );
}

function atFailure() {
  var p = $("atph");

  // if a previous update succeeded, the "atph" should have
  // disappeared. We need to re-create it before trying to update it
  // with an error message...
  if( ! p ) {
    p = document.createElement( "p" );
    p.id = "atph";
    var parent = $("alignmentTree");
    parent.insertBefore( p, parent.firstChild );
  }
  $("alignmentTree").update( "Alignment tree loading failed." );

  // disable the form now, otherwise we'll end up chasing our tail...
  $('phyloForm').disable();
}

//------------------------------------------------------------
//- DAS sequence alignment viewer methods --------------------
//------------------------------------------------------------

// callbacks for the coloured alignment

function caSuccess( oResponse ) {
  $("caph").update( oResponse.responseText );
}

function caFailure() {
  $("caph").update( "Coloured alignment loading failed." );
}

//------------------------------------------------------------
// function to submit the alignment generation form  

function generateAlignment( page ) {
//  console.debug( "generateAlignment: showing page |" + page + "|" );

  // disable various bits of the page and show the spinner
  $( "pagingForm" ).disable();
  $( "handle" ).removeClassName( "sliderHandle" );
  $( "handle" ).addClassName( "disabledSliderHandle" );
  slider.setDisabled();
 
  $( 'spinner' ).show();
  
  var params = { page:        page,
                 acc:         $F('acc'),
                 alnType:     $F('alnType'),
                 numRows:     $F('numRows'),
                 scrollValue: $F('scrollValue') };

  // submit the form
  new Ajax.Updater( "caph",
                    loadOptions.ca.uri, 
                    { method:      'get', 
                      parameters:  params,
                      evalScripts: true
                    }
                  );

}

//------------------------------------------------------------
// scroll the element horizontally based on its width and the slider 
// maximum value

function scrollHorizontal( value, element, slider ) {
  
  // set the scroll position of the alignment
	element.scrollLeft =
    Math.round( value / slider.maximum * ( element.scrollWidth - element.offsetWidth ) );

  // store the value of the slider in the form
  $('scrollValue').value = value;
}

//------------------------------------------------------------
// tweak the alignment to add links to the sequence IDs

var slider;
function formatAlignment( sURLBase, oSlider ) {
  slider = oSlider;

  // pre-compile a regular expression to filter out the ID, start and
  // end residues
  var re = /^(.*?)\/(\d+)\-(\d+)$/;

  // get all of the spans in the key and walk the list to add link tags
  var spans = $("alignmentKey").getElementsByTagName( "span" );
  $A( spans ).each( function( row ) {
      var s  = row.firstChild.nodeValue;
      var ar = re.exec( s );
  
      // build the link
      var a = document.createElement( "a" );
      var t = document.createTextNode( ar[1] );
      a.appendChild( t );
      a.setAttribute( "href", sURLBase + ar[1] );
      a.setAttribute( "onclick", 
                      "window.open(this.href,'pfamProteinWindow');return false;" );
  
      row.replaceChild( a, row.firstChild );
  
      // tack on the residue range, as plain text for now at least
      var r = document.createTextNode( "/" + ar[2] + "-" + ar[3] );
      row.appendChild( r );
    }
  );
}

//------------------------------------------------------------
var numColsTable;

function fstrucSuccess( oResponse ) {
  Element.update( "familyStructureTabHolder", oResponse.responseText );
        // how many columns are there in the table ?
      var firstRow = $("structuresTable").getElementsByTagName("tr")[1]
      numColsTable  = firstRow.getElementsByTagName("td").length;

      // walk over all of the cells in the table and add listeners for mouseover and 
      // mouseout events
      $A( $("structuresTable").getElementsByTagName( "td" ) ).each( function( cell ) {
          cell.onmouseover = highlight.mouseoverHandler.bindAsEventListener( highlight );
          cell.onmouseout  = highlight.mouseoutHandler.bindAsEventListener( highlight );
        }
   );


}

function fstrucFailure() {
  Element.update( "fstrucph", "Graphics loading failed." );
}
