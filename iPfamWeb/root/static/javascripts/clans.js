
// clans.js
// jt6 20060721 WTSI
//
// javascript glue for the clans section
//
// $Id: clans.js,v 1.1.1.1 2007-10-26 13:00:58 rdf Exp $

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

// load ajax components for the clans page

function clanPostLoad() {

  // domain graphics
  if( typeof( loadOptions.cg.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.cg.uri,
					  { method: "get",
						parameters: loadOptions.cg.params,
						onComplete: cgSuccess,
						onFailure:  cgFailure
					  } );
  }

  // add an example structure image to the summary tab
  if( typeof( loadOptions.si.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.si.uri,
					  { method:     "get", 
 					    parameters: loadOptions.si.params,
 					    onComplete: siSuccess
					    // not even bothering with a failure callback...
 					  } );
  }

  // species tree
  if( typeof( loadOptions.st.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.st.uri,
					  { method:     'get', 
						parameters: loadOptions.st.params,
						onComplete: stSuccess,
						onFailure:  stFailure
					  } );
  }
  // clan structure tab
  if( typeof( loadOptions.cstruc.uri ) != "undefined" ) {
	 new Ajax.Request( loadOptions.cstruc.uri,
		 			 	 { method:     'get', 
			 			parameters: loadOptions.cstruc.params,
						onComplete: cstrucSuccess,
						onFailure:  cstrucFailure
					  } );
  }
}
//new Ajax.Request( loadOptions.st.uri,
//	                { method:     "get", 
// 			  parameters: loadOptions.st.params,
//                          onComplete: stSuccess,
//                          onFailure:  stFailure
// 					} );

//------------------------------------------------------------
// callbacks for the domain graphics generation call

function cgSuccess( oResponse ) {
  Element.update( $("clanGraphicsHolder"), oResponse.responseText );
}

function cgFailure() {
  Element.update( $("cgph"), "Graphics loading failed." );
}



//------------------------------------------------------------
// callback for the structure image call

function siSuccess( oResponse ) {
  Element.update( $("siph"), oResponse.responseText );
}

//------------------------------------------------------------
// callbacks for the domain graphics generation call

var numColsTable;

function cstrucSuccess( oResponse ) {
  Element.update( "clanStructureTabHolder", oResponse.responseText );
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

function cstrucFailure() {
  Element.update( "cstrucph", "Graphics loading failed." );
}
