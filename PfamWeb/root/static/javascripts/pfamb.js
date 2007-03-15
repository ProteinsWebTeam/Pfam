
// pfamb.js
// jt6 20061018 WTSI
//
// javascript glue for the PfamB section
//
// $Id: pfamb.js,v 1.4 2007-03-15 15:03:52 jt6 Exp $

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

function pfambPostLoad() {
  if( typeof( loadOptions.st.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.st.uri,
					  { method:     "get", 
						parameters: loadOptions.st.params,
						onComplete: stSuccess,
						onFailure:  stFailure
					  } );
  }
  if( typeof( loadOptions.dg.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.dg.uri,
					  { method:     'get', 
						parameters: loadOptions.dg.params,
						onComplete: dgSuccess,
						onFailure:  dgFailure
					  } );
  }
  // pfamb structure tab
  if( typeof( loadOptions.fstruc.uri ) != "undefined" ) {
	 new Ajax.Request( loadOptions.fstruc.uri,
		 			 	 { method:     'get', 
			 			parameters: loadOptions.fstruc.params,
						onComplete: fstrucSuccess,
						onFailure:  fstrucFailure
					  } );
  }	
}

//------------------------------------------------------------
