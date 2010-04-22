
// WidgetTagger.js
//
// Javascript Object which tags the Alignment and Feature widget together.
//
// $Id$

//------------------------------------------------------------------------------
//- OBJECT ---------------------------------------------------------------------
//------------------------------------------------------------------------------

// spoof a console if necessitates for IE.
if( ! window.console ){
  window.console = {};
  window.console.log = function(){};
}

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

var WidgetTagger = Class.create({
  
  initialize: function( alignObj, featureObj ){
    
    // now check whether alignObj and featureObj is valid;
    if( alignObj !== undefined ){
      this.setAlignObj( alignObj );
    }else{
      this._throw( 'Please provide a valid alignment object reference' );
    }
    
    if( featureObj !== undefined ){
      this.setFeatureObj( featureObj );
    }else{
      this._throw( 'Please provide a valid feature object reference' );
    }
    
    // if i reach this point, then there exists a valid alignment and features;
    
    
  }, // end of initialize;
  
  //------------------------------------------------------------------------------
  //- Methods --------------------------------------------------------------------
  //------------------------------------------------------------------------------
  
  //------------------------------------------------------------------------------
  //- Getters and Setters --------------------------------------------------------
  //------------------------------------------------------------------------------
  
  // set method for alignObj;
  setAlignObj: function( alignObj ){
    this._alignObj = alignObj;
    
    // now check whether the alignObj is valid by checkign the HTML DOM;
    var accDiv = this._alignObj.getAccDiv();
    var seqDiv = this._alignObj.getSeqDiv();
    
    if( $( accDiv ) !== undefined ){
      this._throw( 'Its not a valid alignment object, as no accessions for alignments are shown in page' );  
    }
    
    if( $( seqDiv ) !== undefined ){
      this._throw( 'Its not a valid alignment object, as no alignments for sequences are shown in page' );
    }
    
  },
  
  //-------------------------------------
  
  // get method for alignObj;
  getAlignObj: function(){
    return this._alignObj;
  },
  
  //-------------------------------------
  
  // set method for featureObj;
  setFeatureObj: function( featureObj ){
    this._featureObj = featureObj;
    
    // now check whether the featureObj has got canvas;
    var img = this._featureObj.getimgCanvas();
    var txt = this._featureObj.gettxtCanvas();
    
    if( $( img ) !== undefined ){
      this._throw( 'Its not a valid feature object, as features are not shown in page' );  
    }
    
    if( $( txt ) !== undefined ){
      this._throw( 'Its not a valid feature object, as das sources are not shown in page' );
    }
    
  },
  
  //-------------------------------------
  
  // get method for featureObj;
  getFeatureObj: function(){
    return this._featureObj;
  },
  
  //------------------------------------------------------------------------------
  //- Private Methods ------------------------------------------------------------
  //------------------------------------------------------------------------------
  
  _throw: function( msg ){
    throw {
      name: 'WidgetTaggerException',
      message:  msg,
      toString: function(){ return this.message }
    };
          
  }
  
} ); // end of class.create
