
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
  
  initialize: function( alignObj, options ){
    
    // now check whether alignObj is valid;
    if( alignObj !== undefined ){
      this.setAlignObj( alignObj );
    }else{
      this._throw( 'Please provide a valid alignment object reference' );
    }
    
    // before making sure, check whether options is defined;
    if( options === undefined ){
      this._throw( 'Atlease one other viewer has to be set and provide parameters for it ' );
    }else{
      this.setOptions( options );
    }
    
    console.log( 'the featureViewer is '+options.FeatureParams );
    
    // the user should have enabled atleast one other viewer for usign this viewer;
    // check the options hash;
    var status = 1;
    var msg;
    
    if( options.FeatureParams === undefined && options.StructureParams === undefined ){
      
      status = 0;
      msg    = 'Atlease one other viewer has to be enabled to tag the alignment viewer';
      
    }else if( options.FeatureParams !== undefined ){
      
      // check for parent;
      status = ( options.FeatureParams.featureParent === undefined ) ? 0 : 1 ;
      msg    = ( options.FeatureParams.featureParent === undefined ) ? 'DOM reference necessary for Feature viewer' : '' ;
      
      // check for sources;
      status = ( options.FeatureParams.featureSources === undefined ) ? 0 : 1 ;
      msg    = ( options.FeatureParams.featureSources === undefined ) ? 'DAS sources necessary for Feature viewer' : '' ;
      
      // check for url
      status = ( options.FeatureParams.featureURL === undefined ) ? 0 : 1 ;
      msg    = ( options.FeatureParams.featureURL === undefined ) ? 'URL necessary for Feature viewer' : '' ;
      
    }else if( options.StructureParams.StructureViewer === true ){
      
      // check for parent;
      status = ( options.StructureParams.structureParent === undefined ) ? 0 : 1 ;
      msg    = ( options.StructureParams.structureParent === undefined ) ? 'DOM reference necessary for structure viewer' : '' ;
      
      // check for sources;
      status = ( options.StructureParams.structureURL === undefined ) ? 0 : 1 ;
      msg    = ( options.StructureParams.structureURL === undefined ) ? 'URL necessary for structure viewer' : '' ;
      
      // check for URL
      status = ( options.StructureParams.structureJmolURL === undefined ) ? 0 : 1 ;
      msg    = ( options.StructureParams.structureJmolURL === undefined ) ? 'Jmol URL necessary for structure viewer' : '' ;
      
    }
    
    // if the status value is 0, then something is wrong;
    if( status == 0 ){
      this._throw( msg );
    }
    
    // set an poller to check whether the alignObj is ready;
    this._alignChecker = setInterval( this.alignStatusChecker.bind( this ), 500 );  
    
    
  }, // end of initialize;
  
  //------------------------------------------------------------------------------
  //- Methods --------------------------------------------------------------------
  //------------------------------------------------------------------------------
  
  // function to check the status of the alignment;
  alignStatusChecker: function(){
    
    if( this._alignObj.getReadyState() === true ){
      
      // alignments are loaded now, so clear the timer;
      clearInterval( this._alignChecker );
      
      //console.log( 'the alignments are '+$H( this._alignObj._livegrid.getAlignment() ).inspect() );
      // now proceed with the other process of adding listeners;
      this.getWidgets();
      
    }else if( this._alignObj.getReadyState() === false ){
      
      console.log('ERROR: alignments could not be retrieved ' );
      clearInterval( this._alignChecker );
      
    }else{
      console.log('****Alignments are not ready *****');
    }
    
  },
  
  //-------------------------------------------------------------------------------
  
  // function to continue the tagging the alignment viewer;
  getWidgets: function( ){
    
    // add event listeners to the accesions of the alignObj;
    this.addListener();
     
  },
  
  //-------------------------------------------------------------------------------
  
  // function which adds event listener to the accDiv;
  addListener: function(){
    
    var accDiv = this._alignObj.getAccDiv();
    console.log( 'add event listener for the accDiv in the alignments' + accDiv.inspect() );
    
    // event listener added for click
    accDiv.observe( 'click', function( e ){
      
      // store the clicked element as wholeAcc, as we need this to 
      // retrieve raw alignment to tag this with the feature viewer;
      this._wholeAcc = e.element().identify();
      console.log('teh wholeAcc is '+this._wholeAcc );
      
      this._parseAccession( e.element().identify() );
      
      // if the clickedAccession is defined then make a request;
      if( this.clickedAccession !== undefined ){
        
        // get the features;
        if( options.FeatureParams !== undefined ){
          console.log( 'making a feature request with |%s|%s|%s|%s|',this.options.FeatureParams.featureParent, this.clickedAccession, this.options.FeatureParams.featureSources, this.options.FeatureParams.featureURL );
          this._featureObj = new FeatureViewer( this.options.FeatureParams.featureParent, this.clickedAccession, this.options.FeatureParams.featureSources, this.options.FeatureParams.featureURL );  
        }
        
        // simultaneously, if structure params are defined;
        if( options.StructureParams !== undefined ){
          console.log('making a structure request ' );
          this._structureObj = new StructureViewer( this.options.StructureParams.structureParent, this.clickedAccession, this.options.StructureParams.structureURL, this.options.StructureParams.structureJmolURL );
        }
        
        // add an poller to check the status of the features and structures;
        this._featureChecker   = setInterval( this.featureStatusChecker.bind( this ), 500 );
        this._structureChecker = setInterval( this.structureStatusChecker.bind( this ), 500 );
         
      } // end  of clickedAccession;
      
    }.bind( this ) );
          
  },
  
  //-------------------------------------------------------------------------------
  
  // function to track down the features to load in the page;
  
  featureStatusChecker: function(){
    
    if( this._featureObj.getReadyState() === true ){
      
      // features are loaded now, so clear the timer;
      clearInterval( this._featureChecker );
      
      // now add the mouse move listener to the features;
      this.mousemoveListener();
      
    }else if( this._featureObj.getReadyState() === false ){
      
      console.log('ERROR: Features could not be retrieved ' );
      clearInterval( this._featureChecker );
      
    }else{
      console.log('****Features are not ready *****');
    }
    
  },
  
  //-------------------------------------------------------------------------------
  
  // function to track down the structures to load in the page;
  
  structureStatusChecker: function(){
    
    if( this._structureObj.getReadyState() === true ){
      
      // structures are loaded now, so clear the timer;
      clearInterval( this._structureChecker );
      
    }else if( this._structureObj.getReadyState() === false ){
      
      console.log('ERROR: Strcucture could not be retrieved here' );
      clearInterval( this._structureChecker );
      
    }else{
      console.log('****Structure are not ready *****');
    }
    
  },
  
  //-------------------------------------------------------------------------------
  
  // function which registers the mousemove and tags the alignment viewer together;
  mousemoveListener : function(){
    
    var featureObj = this._featureObj;
    
    // first get the canvas;
    var imgCanvas = featureObj.getimgCanvas();
    var txtCanvas = featureObj.gettxtCanvas();
    
    //console.log( this._featureObj._graphicXOffset,this._featureObj._extraXspace, this._featureObj._Yincrement,imgCanvas.inspect() );
    
    // as we now have the features, to tag it to the alignment viewer, we need certain parameters
    // so parse the features response and get those details;
    // ******* features is already present in DOM *****
    var graphicXOffset = featureObj._graphicXOffset;
    var extraXspace    = featureObj._extraXspace;
    var Yincrement     = featureObj._Yincrement  
    var canvasHeight   = features.dasTracks * Yincrement;
    var resWidth       = featureObj._resWidth;
    var sequenceLength = parseInt( features.seqLength ) * resWidth;
    var canvasWidth    = sequenceLength + graphicXOffset + extraXspace;
    var baseline       = featureObj._baseline;
    var graphicYOffset = featureObj._graphicYOffset; 
    var sources        = features.json_sources;
    var canvasWrapper  = featureObj._canvasWrapper;
    var scroller       = featureObj._scroller;
    var backgroundDiv  = featureObj._backgroundDiv;
    var height         = graphicYOffset - Yincrement + baseline;  
    
    console.log( 'xOf|%s|, xS|%s|, Yin|%s|, CanHt|%s|, resWi|%s|, seqLe|%s|, canWI|%s|, base|%s|, Yof|%s|, sources|%s|,',
    graphicXOffset,extraXspace, Yincrement, canvasHeight, resWidth, sequenceLength, canvasWidth, baseline, graphicYOffset, sources
    );
      
    console.log('the canvas wrapper is '+canvasWrapper.inspect()+'|'+scroller.inspect()+'|'+ backgroundDiv.inspect() );
    
    
    // now get the parameters from alignObj;
    var alignObj       = this._alignObj;
    var alignStart     = this._alignStart;
    var alignEnd       = this._alignEnd;
    var alignments     = $H( alignObj._livegrid.getAlignment() );
    
    //console.log('the alignments are '+ alignments.inspect() );
    imgCanvas.observe( 'mousemove', function( e ){
      
      // calculate the offset position of the element;
      var offset = imgCanvas.up().cumulativeOffset();
        
      // calulate the x position
      var x = e.pointerX() - offset[ 0 ];
      var y = e.pointerY() - offset[ 1 ];
      //console.log( 'the x and y is |%d|%d|',x,y, graphicXOffset, canvasWidth, extraXspace ); 
      
      // inserted from here
      if( x > graphicXOffset && x < ( canvasWidth - extraXspace ) ){
        
        // as the y are positioned using float:left, we need to consider the width of the text canvas;
        var scrollerLeftPos = x + parseInt( txtCanvas.getAttribute( 'width' ) );
        
        // now calculate the size of the scroller to be displayed
        scroller.setStyle( {
          'height': height+'px',
          'left'  : scrollerLeftPos+'px'
        } );
        
        // now update the resNum div to say which residue is currently shown;
        var res = ( x / resWidth ) - graphicXOffset + parseInt( canvasWrapper.scrollLeft );
        //console.log( 'the start, end of alignment and the res is |%d|%d|',alignStart,alignEnd,res);
        
        // TAGGING THE ALIGNMENT VIEWER TO FEATURE VIEWER HERE;
        //if the resnumber falls between the alignmnet coordinates
        if( ( res >= alignStart ) && ( res <= alignEnd ) ){
          
          // use the accession and residue number get the column number and change the color.
          var sequence = $A( alignments.get( this._wholeAcc ).toArray() );
          console.log( 'the start, end of alignment and the res is |%d|%d|',alignStart,alignEnd,res);
          
          var resNum = 0 ;
          var columnNum;
          sequence.each( function ( value, index ){
            
            // check whether we get residue or a
            if( /[A-Z]+/.test( value) ){
              //// console.log('it is a residue '+ value );
              if( ( parseInt(alignStart) + resNum ) == res ){
                columnNum = index;
                throw $break;
              }
              resNum++;  
            }
            
          });  
          //// console.log('the column number to be highlighted is '+columnNum );
          
          // now calculate the position of the div to be placed;
          var accSize  = alignObj.getAccDiv().getWidth();
          var seqSize  = alignObj.getSeqDiv().getWidth();
          
          // get the width of the sequence % total residues to get pixel value for single residue;
          var highlightLeftPos =  ( Math.round( ( $( this._wholeAcc +'seq').getWidth()/sequence.size() ) * columnNum ) );
          //// console.log( 'accsize|highlightLeftPos|seqsize|totalres|,|%d|%d|%d|%d|',accSize,highlightLeftPos,$(acc+'seq').getWidth(),sequence.size() );
          
          // get the total size we have moved;
          var visibleWinStart = alignObj.getSeqDiv().scrollLeft;
          var visibleWinEnd = seqSize + alignObj.getSeqDiv().scrollLeft ;
          var diffPos = 0;
          
          // if the total visible size is lesser than the residue to be highlighted,
          //// console.log( 'bef the windowstart,end,leftpos are |%d|%d|%d|',visibleWinStart,visibleWinEnd,highlightLeftPos);
          // console.log("the scrollLEft and highlightleftpos is |%d|%d|",$('sequences').scrollLeft, highlightLeftPos );
          if ( ( highlightLeftPos > visibleWinEnd ) ){
            // now set the new scrollLeft as the visibleWinEnd;
            diffPos = highlightLeftPos - visibleWinEnd;
            //$('sequences').scrollLeft = visibleWinEnd;
            alignObj.getSeqDiv().scrollLeft = alignObj.getSeqDiv().scrollLeft + diffPos + 20;
            diffPos = highlightLeftPos - diffPos -visibleWinStart - 20;
          }
           else if(  highlightLeftPos < visibleWinStart ){
            diffPos = visibleWinStart - highlightLeftPos ;
            alignObj.getSeqDiv().scrollLeft = highlightLeftPos;
            // console.log( 'the highlightleftPos is lesser than windowStart|d '+diffPos);
            
          }else{
            diffPos = highlightLeftPos - alignObj.getSeqDiv().scrollLeft;
          }
          // console.log( 'the diffPOs,accSize,div width |%d|%d|%d|',diffPos,accSize,$('highlightColumn').getWidth() );
          // now add the accSize to display the correct residue;
          highlightLeftPos = ( diffPos + accSize + $('highlightColumn').getWidth() );
          // console.log( 'aft the windowstart,end,leftpos are |%d|%d|%d|',visibleWinStart,visibleWinEnd,highlightLeftPos);
          
          alignObj._highlighter.setStyle({
            left: highlightLeftPos+'px',
            height: alignObj.getAccDiv().getHeight()+'px'
          });
          
          alignObj._highlighter.setStyle({
            'visibility' : 'visible'
          });
         
        }else{  // else(1)
          
          alignObj._highlighter.setStyle({
           'visibility' : 'hidden'
          });
         
        } // end of else (1)
        
        // TAGGING THE ALIGNMENT VIEWER TO FEATURE VIEWER ENDS HERE;
        
      } // because we start to draw the sequence from 100px;
      // insert completed; 
      
      // also make sure the strucutreObj is ready;
      if( this._structureObj.getReadyState() === true ){
        
        jmolScript( 'select all; color red;', 'foo' );
          
      } // end of structureObj.getReadyState;
      
    }.bind(this) ); // end of imgCanvas.observe
    
      
  },
  
  //------------------------------------------------------------------------------
  //- Getters and Setters --------------------------------------------------------
  //------------------------------------------------------------------------------
  
  // set method for alignObj;
  setAlignObj: function( alignObj ){
    this._alignObj = alignObj;
  },
  
  //-------------------------------------
  
  // get method for alignObj;
  getAlignObj: function(){
    return this._alignObj;
  },
  
  //-------------------------------------------------------------------------------
  
  // function to set the options;
  setOptions: function( options ){
    this.options = options;
  },
  
  //-------------------------------------
  
  // function to get teh options;
  getOptions: function(){
    return this.options;
  },
  
  //------------------------------------------------------------------------------
  //- Private Methods ------------------------------------------------------------
  //------------------------------------------------------------------------------
  
  // function to parse the accession which is clicked;
  _parseAccession: function( acc ){
    
    console.log( 'parseAccession is called with '+acc );
    
    // now the format of the id is, ( accession ) / ( start ) - ( end );
    // so parse it to get the accession;
    if( /(\w+)\/(\d+)\-(\d+)/.test( acc ) ){
        
      var data = /(\w+)\/(\d+)\-(\d+)/.exec( acc );
      this.clickedAccession = data[1]; 
      this._alignStart       = data[2];
      this._alignEnd         = data[3];
          
    }else{
      this.clickedAccession = acc;
      this._alignStart       = 0;
      this._alignEnd         = 0;
    }
     
  },
  
  //-------------------------------------------------------------------------------
  
  _throw: function( msg ){
    throw {
      name: 'WidgetTaggerException',
      message:  msg,
      toString: function(){ return this.message }
    };
          
  }
  
  
} ); // end of class.create
