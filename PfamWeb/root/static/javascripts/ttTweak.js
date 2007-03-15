
// ttTweak.js
// jt6 20070313 WTSI.
//
// adjust the default behaviour of the Yahoo tooltips 
//
// $Id: ttTweak.js,v 1.2 2007-03-15 15:03:52 jt6 Exp $

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

// make the tip follow the cursor
YAHOO.widget.Tooltip.prototype.onContextMouseMove = function(e, obj) {
  obj.pageX = YAHOO.util.Event.getPageX(e);
  obj.pageY = YAHOO.util.Event.getPageY(e);
  obj.moveTo(obj.pageX, obj.pageY + 20);
};

// alter the initial configuration of the tips, mainly the delays
YAHOO.widget.Tooltip.prototype.initDefaultConfig = function() {
  YAHOO.widget.Tooltip.superclass.initDefaultConfig.call(this);

  this.cfg.addProperty("preventoverlap",   { value:true, validator:this.cfg.checkBoolean, supercedes:["x","y","xy"] } );
  
  this.cfg.addProperty("showdelay",        { value:250, handler:this.configShowDelay, validator:this.cfg.checkNumber } );
  this.cfg.addProperty("autodismissdelay", { value:5000, handler:this.configAutoDismissDelay, validator:this.cfg.checkNumber } );
  this.cfg.addProperty("hidedelay",        { value:20, handler:this.configHideDelay, validator:this.cfg.checkNumber } );
  
  this.cfg.addProperty("text",             { handler:this.configText, suppressEvent:true } );
  this.cfg.addProperty("container",        { value:document.body, handler:this.configContainer } );
};
