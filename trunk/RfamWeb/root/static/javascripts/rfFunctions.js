
// rfFunctions.js
// jt6 20080310 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: rfFunctions.js,v 1.4 2008-07-25 13:34:46 jt6 Exp $

// define an Rfam style for prototip tooltips

Prototip.Styles.rfam = {
  borderColor: "#621700",
  className: "rfam",
  border:    2,
  radius:    2,
  hook:      { target: "topLeft", tip: "bottomLeft" },
  offset:    { x: -8, y: 1 },
  stem:      { position: "bottomLeft", width: 9, height: 14 }
};

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk),
//          Jennifer Daub (jd7@sanger.ac.uk)
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
