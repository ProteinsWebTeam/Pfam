
// family.js
// jt6 20060721 WTSI
//
// javascript glue for the family section
//
// $Id: family.js,v 1.1 2008-10-23 12:29:22 jt6 Exp $

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

//------------------------------------------------------------
var numColsTable;

function fstrucSuccess( oResponse ) {
  $("familyStructureTabHolder").update( oResponse.responseText );
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
