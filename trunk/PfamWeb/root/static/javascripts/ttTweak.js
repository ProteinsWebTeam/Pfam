
// ttTweak.js
// jt6 20070313 WTSI.
//
// adjust the default behaviour of the Yahoo tooltips 
//
// $Id: ttTweak.js,v 1.1 2007-03-13 10:35:09 jt6 Exp $

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
