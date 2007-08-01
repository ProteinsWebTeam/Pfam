
// structure.js
// jt6 20060721 WTSI
//
// javascript glue for the structure section
//
// $Id: structure.js,v 1.4 2007-08-01 14:47:28 jt6 Exp $

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

// load ajax components for the structure page
function structurePostLoad() {
   new Ajax.Request( loadOptions.sg.uri,
 					{ method: "get",
 					  parameters: loadOptions.sg.params,
 					  onComplete: sgSuccess,
 					  onFailure:  sgFailure
 					} );
//   new Ajax.Request( loadOptions.getDomains.uri,
//					{ method: "get",
// 					  parameters: loadOptions.getDomains.params,
// 					  onComplete: getDomainsSuccess,
// 					  onFailure:  getDomainsFailure
// 					} );
}

// called in response to a successful call
function sgSuccess( oResponse ) {
  Element.update( $("structureGraphicsHolder"), oResponse.responseText );
}




// called in response to a failed call
function sgFailure() {
  Element.update( $("sgph"), "Graphics loading failed." );
}

//Structural domains
// called in response to a successful call
function getDomainsSuccess( oResponse ) {
  Element.update( $("getDomph"), oResponse.responseText );
}

// called in response to a failed call
function getDomainsFailure() {
  Element.update( $("getDomph"), "Graphics loading failed." );
}
