
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global Prototip */

//------------------------------------------------------------------------------
//- data -----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript data structure that specifies the style of the Pfam tooltips.
//
// jt6 20090817 WTSI
//
// $Id: tip_definition.js,v 1.1 2009-09-04 13:01:15 jt6 Exp $
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

//------------------------------------------------------------------------------

Prototip.Styles.pfam = {
  border: 3,
  borderColor: '#074987',
  className: 'pfam',
  closeButton: false,
  hideAfter: false,
  hideOn: 'mouseleave',
  hook: false,
  radius: 3,
  showOn: 'mousemove',
  stem: {
    position: 'topLeft',
    height: 12,
    width: 15
  },
  width: 'auto'
};

//------------------------------------------------------------------------------
