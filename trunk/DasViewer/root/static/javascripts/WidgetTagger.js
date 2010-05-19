
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
    
    // console.log( 'the featureViewer is '+options.FeatureParams );
    
    // the user should have enabled atleast one other viewer for usign this viewer;
    // check the options hash;
    
    if( options.FeatureParams === undefined && options.StructureParams === undefined ){
      this._throw( 'Atlease one other viewer has to be enabled to tag the alignment viewer' );
      
    }else{
      if( options.FeatureParams !== undefined ){
      
        // check for parent;
        if( options.FeatureParams.featureParent === undefined ){
          this._throw( 'DOM reference necessary for Feature viewer' );
        }
        
        // check for sources;
        if( options.FeatureParams.featureSources === undefined ){
          this._throw( 'DAS sources necessary for feature viewer' );
        }
        
        // check for url
        if( options.FeatureParams.featureURL === undefined ){
          this._throw( 'URL necessary for feature viewer' );  
        }
        
      }
      
      if( options.StructureParams !== undefined ){
        
        // check for parent;
        if( options.StructureParams.structureParent === undefined ){
          this._throw( 'DOM reference necessary for Structure viewer' );
        }
        
        // check for URL;
        if( options.StructureParams.structureURL === undefined ){
          this._throw( 'URL necessary for Structure viewer' );  
        }
        // check for JmolURL
        if( options.StructureParams.structureJmolURL === undefined ){
          this._throw( 'JmolURL necessary for Structure viewer'+ options.StructureParams.structureJmolURL );  
        }
        
      }
    } // end of both undefined check
    
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
      
      //// console.log( 'the alignments are '+$H( this._alignObj._livegrid.getAlignment() ).inspect() );
      // now proceed with the other process of adding listeners;
      this.getWidgets();
      
    }else if( this._alignObj.getReadyState() === false ){
      
      // console.log('ERROR: alignments could not be retrieved ' );
      clearInterval( this._alignChecker );
      
    }else{
      // console.log('****Alignments are not ready *****');
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
    // console.log( 'add event listener for the accDiv in the alignments' + accDiv.inspect() );
    
    // event listener added for click
    accDiv.observe( 'click', function( e ){
      
      // ******************************************************************************
      // * when a click is made, look for the following;                              *
      // * First stop the observer, as it might initiate another request              *
      // * (1) whether already some features are shown i.e( wholeAcc is defined )     *
      // * (2) remove the class name of the previous accession clicked;               *
      // * (3) scroller is to be hidden when the request is success, if shown;        *
      // ******************************************************************************
      
      if( this.getClickProcessStatus() === true ){
        console.log( 'there is a request in process; so returning;' );
        return;
      }
      
      // if the wholeAcc is already defined, then remove the className;
      if( this._wholeAcc !== undefined ){
        // console.log('removing the backgrund class from the accession' );
        
        // now check its in the range;
        if( $( this._wholeAcc ) !== null ){
          $( this._wholeAcc ).removeClassName( 'background' );  
        }
        
      }
      
      // store the clicked element as wholeAcc, as we need this,
      // also add the background class to highight the clicked accession;
      e.element().addClassName( 'background' );
       
      this._wholeAcc      = e.element().identify();
      this._wholeSeqWidth = $( this._wholeAcc+'seq' ).getWidth();
      this._alignments    = $H( this._alignObj._livegrid.getAlignment() );
      this._sequence      = $A( this._alignments.get( this._wholeAcc ).toArray() );
     
      // console.log('teh wholeAcc and seq width is '+this._wholeAcc +'|'+ this._wholeSeqWidth );
      
      this._parseAccession( e.element().identify() );
      
      // if the clickedAccession is defined then make a request;
      if( this.clickedAccession !== undefined ){
        
        // as the mousemoveListener is called for everytime the canvas is rendered,
        // its adds multiple listener to the same element, so everytime remove the 
        // event listener and create a new one;
        if( this._featureObj !== undefined ){
          // console.log( 'Already a featureviewer is in place ' );
          if( this._featureObj.getimgCanvas() !== undefined ){
            // console.log('stopping the event listener ');
            this._featureObj.getimgCanvas().stopObserving( 'mousemove' );  
          }
          
        } // end of this._featureObj
        
        // get the features;
        if( options.FeatureParams !== undefined ){
          
          // now make a status holder for featureviewer adn say true;
          this.FeatureViewerEnabled = true;
          
          // now try to add another method for making feature request and seting nterval;
          this.getFeatureViewer();
            
        }
        
        // simultaneously, if structure params are defined;
        if( options.StructureParams !== undefined ){
          
          // now make a status holder for structureviewer adn say true;
          this.StructureViewerEnabled = true;
          
          // console.log('making a structure request ' );
          this._structureObj = new StructureViewer( this.options.StructureParams.structureParent, this.clickedAccession, this.options.StructureParams.structureURL, this.options.StructureParams.structureJmolURL );
          
          // add an poller to check the status of the structures;
          this._structureChecker = setInterval( this.structureStatusChecker.bind( this ), 500 );  
        }
        
        // now set the clickProcessStatus;
        this.setClickProcessStatus( true );
         
      } // end  of clickedAccession;
      
    }.bind( this ) );
          
  },
  
  //-------------------------------------------------------------------------------
  
  // function to get the feature viewer;
  
  getFeatureViewer: function(){
     
     // console.log( 'making a feature request with |%s|%s|%s|%s|',this.options.FeatureParams.featureParent, this.clickedAccession, this.options.FeatureParams.featureSources, this.options.FeatureParams.featureURL );
     this._featureObj = new FeatureViewer( this.options.FeatureParams.featureParent, this.clickedAccession, this.options.FeatureParams.featureSources, this.options.FeatureParams.featureURL );
          
     // add an poller to check the status of the features
     this._featureChecker   = setInterval( this.featureStatusChecker.bind( this ), 500 );
      
  },
  
  //-------------------------------------------------------------------------------
  
  // function to track down the features to load in the page;
  
  featureStatusChecker: function(){
    
    if( this._featureObj.getReadyState() === true ){
      
      // now we got the features back so set the clickProcessStatus to false;
      this.setClickProcessStatus( false );
      
      // now hide the scroller if exists already;
      if( this._featureObj._scroller !== undefined ){
        this._featureObj._scroller.hide();
      }
      
      // console.log( 'the featureChecker value is '+ this._featureChecker );
      // features are loaded now, so clear the timer;
      clearInterval( this._featureChecker );
      // console.log( 'after clearing :the featureChecker value is '+ this._featureChecker );
      
      // now add the mouse move listener to the features;
      this.mousemoveListener();
      
    }else if( this._featureObj.getReadyState() === false ){
      
      // request is failure, but still enable the click for other request;
      this.setClickProcessStatus( false );
      
      // console.log( 'ERROR: Features could not be retrieved' );
      clearInterval( this._featureChecker );
      
    }else{
      // console.log('****Features are not ready *****');
    }
    
  },
  
  //-------------------------------------------------------------------------------
  
  // function to track down the structures to load in the page;
  
  structureStatusChecker: function(){
    
    if( this._structureObj.getReadyState() === true ){
      
      // we got strucutres so set the status to false to enable other request;
      this.setClickProcessStatus( false );
      
      this._mapData = this._structureObj._processedStructure.get( this._structureObj.getShownStructure() );
      this._resDiff = this._mapData.pstart - this._mapData.start;
      
      // structures are loaded now, so clear the timer;
      clearInterval( this._structureChecker );
      
    }else if( this._structureObj.getReadyState() === false ){
      
      // request is failure, but still enable the click for other request;
      this.setClickProcessStatus( false );
      
      // console.log('ERROR: Strcucture could not be retrieved here status:'+this._structureObj.getReadyState()  );
      clearInterval( this._structureChecker );
      
    }else{
      // console.log('****Structure are not ready *****');
    }
    
  },
  
  //-------------------------------------------------------------------------------
  
  // function which registers the mousemove and tags the alignment viewer together;
  mousemoveListener : function(){
    
    var featureObj = this._featureObj;
    
    // first get the canvas;
    var imgCanvas = featureObj.getimgCanvas();
    var txtCanvas = featureObj.gettxtCanvas();
    
    //// console.log( this._featureObj._graphicXOffset,this._featureObj._extraXspace, this._featureObj._Yincrement,imgCanvas.inspect() );
    
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
    
//    // console.log( 'xOf|%s|, xS|%s|, Yin|%s|, CanHt|%s|, resWi|%s|, seqLe|%s|, canWI|%s|, base|%s|, Yof|%s|, sources|%s|,',
//    graphicXOffset,extraXspace, Yincrement, canvasHeight, resWidth, sequenceLength, canvasWidth, baseline, graphicYOffset, sources
//    );
//      
//    // console.log('the canvas wrapper is '+canvasWrapper.inspect()+'|'+scroller.inspect()+'|'+ backgroundDiv.inspect() );
    
    // console.log( 'the height is '+height );
    // now get the parameters from alignObj;
    var alignObj       = this._alignObj;
    var alignStart     = this._alignStart;
    var alignEnd       = this._alignEnd;
    var diff_size      = ( alignEnd - alignStart ) + 1 + 1;
    var leftPos        = graphicXOffset + parseInt( alignStart  ) + 1;
    var alignments     = this._alignments;
    var sequence       = this._sequence;
    var wholeSeqWidth  = this._wholeSeqWidth;
    
    // console.log( 'the wholeseqWidth is '+this._wholeSeqWidth );
     
    // background div;
    backgroundDiv.setStyle( {
          'height': height+'px',
          'left'  : leftPos+'px',
          'width' : diff_size+'px'
        });
    
    // event listener for mouse move;
    imgCanvas.observe( 'mousemove', function( e ){
      
      // show the scroller;
      scroller.show();
      
      if (this._mapData !== undefined) {
        // first reset the structure to native stage;
        var script = 'select all; color grey; spacefill false;';
        jmolScript(script, 'foo');
      }
      
      // calculate the offset position of the element;
      var offset = imgCanvas.up().cumulativeOffset();
        
      // calulate the x position
      var x = e.pointerX() - offset[ 0 ];
      var y = e.pointerY() - offset[ 1 ];
      //// console.log( 'the x and y is |%d|%d|',x,y, graphicXOffset, canvasWidth, extraXspace ); 
      
      // as the y are positioned using float:left, we need to consider the width of the text canvas;
      var scrollerLeftPos = x + parseInt( txtCanvas.getAttribute( 'width' ) );
      
      // now update the resNum div to say which residue is currently shown;
      var res = ( x / resWidth ) - graphicXOffset + parseInt( canvasWrapper.scrollLeft );
      //// console.log( 'the start, end of alignment and the res is |%d|%d|',alignStart,alignEnd,res);
          
      // x is in range with the size of the canvas
      if (x > graphicXOffset && x < (canvasWidth - extraXspace)) {
      
        // now calculate the size of the scroller to be displayed
        scroller.setStyle({
          'height': height + 'px',
          'left': scrollerLeftPos + 'px'
        });
        
        // TAGGING THE ALIGNMENT VIEWER TO FEATURE VIEWER HERE;
        //if the resnumber falls between the alignmnet coordinates
        if ((res >= alignStart) && (res <= alignEnd)) {
        
          var resNum = 0;
          var columnNum;
          sequence.each(function(value, index){
          
            // check whether we get residue or a
            if (/[A-Z]+/.test(value)) {
              //// // console.log('it is a residue '+ value );
              if ((parseInt(alignStart) + resNum) == res) {
                columnNum = index;
                throw $break;
              }
              resNum++;
            }
            
          });
          //// // console.log('the column number to be highlighted is '+columnNum );
          
          // now calculate the position of the div to be placed;
          var accSize = alignObj.getAccDiv().getWidth();
          var seqSize = alignObj.getSeqDiv().getWidth();
          
          // get the width of the sequence % total residues to get pixel value for single residue;
          var highlightLeftPos = (Math.round((wholeSeqWidth / sequence.size()) * columnNum));
          //// // console.log( 'accsize|highlightLeftPos|seqsize|totalres|,|%d|%d|%d|%d|',accSize,highlightLeftPos,$(acc+'seq').getWidth(),sequence.size() );
          
          // get the total size we have moved;
          var visibleWinStart = alignObj.getSeqDiv().scrollLeft;
          var visibleWinEnd = seqSize + alignObj.getSeqDiv().scrollLeft;
          var diffPos = 0;
          
          // if the total visible size is lesser than the residue to be highlighted,
          //// // console.log( 'bef the windowstart,end,leftpos are |%d|%d|%d|',visibleWinStart,visibleWinEnd,highlightLeftPos);
          // // console.log("the scrollLEft and highlightleftpos is |%d|%d|",$('sequences').scrollLeft, highlightLeftPos );
          
          if ((highlightLeftPos > visibleWinEnd)) {
            // now set the new scrollLeft as the visibleWinEnd;
            diffPos = highlightLeftPos - visibleWinEnd;
            //$('sequences').scrollLeft = visibleWinEnd;
            alignObj.getSeqDiv().scrollLeft = alignObj.getSeqDiv().scrollLeft + diffPos + 20;
            diffPos = highlightLeftPos - diffPos - visibleWinStart - 20;
          }
          else 
            if (highlightLeftPos < visibleWinStart) {
              diffPos = visibleWinStart - highlightLeftPos;
              alignObj.getSeqDiv().scrollLeft = highlightLeftPos;
            // // console.log( 'the highlightleftPos is lesser than windowStart|d '+diffPos);
            
            }
            else {
              diffPos = highlightLeftPos - alignObj.getSeqDiv().scrollLeft;
            }
          // // console.log( 'the diffPOs,accSize,div width |%d|%d|%d|',diffPos,accSize,$('highlightColumn').getWidth() );
          // now add the accSize to display the correct residue;
          highlightLeftPos = (diffPos + accSize + $('highlightColumn').getWidth());
          // // console.log( 'aft the windowstart,end,leftpos are |%d|%d|%d|',visibleWinStart,visibleWinEnd,highlightLeftPos);
          
          alignObj._highlighter.setStyle({
            left: highlightLeftPos + 'px',
            height: alignObj.getAccDiv().getHeight() + 'px'
          });
          
          alignObj._highlighter.setStyle({
            'visibility': 'visible'
          });
          
        }
        else { // else(1)
          alignObj._highlighter.setStyle({
            'visibility': 'hidden'
          });
          
        } // end of else (1)
      // TAGGING THE ALIGNMENT VIEWER TO FEATURE VIEWER ENDS HERE;
      
      }  //  end of if graphicOffset ;because we start to draw the sequence from 100px;
//      else { 
//        // console.log( 'the scroller is out of range so hide it' );
//        
//        scroller.hide();
//        
//      } 
      
      // ****************************************************
      // * if the structure viewer is enabled, do something *
      // ****************************************************
      
      if( this.isStructureViewerEnabled() === true ){
        
        // now check the status of the structure, if its changed, then new mapdata is to be calculated;
        if( this._structureObj.isStructureChanged() === true ){
          
          // console.log('the strucutre is changed, so gettitng new mapdata for '+ this._structureObj.getShownStructure() )
          this._mapData = this._structureObj._processedStructure.get( this._structureObj.getShownStructure() );
          this._resDiff = this._mapData.pstart - this._mapData.start;
        
          // console.log( 'the mapdata and the resDiff is '+this._mapData+'|'+ this._resDiff );  
          
          // now we got the change so set to false;
          this._structureObj.setStructureChange( false );
          // console.log( 'the structure status changed to false ' );
          
        } // end  of structure changed;
        
        if( this._mapData !== undefined ){
          
          if( res >= this._mapData.pstart && res <= this._mapData.pend ){
            
            // if we subtract the diff in mapping numbers, we get the actual residue to be highlighted;
            var pdbRes = res - this._resDiff;
            
            // console.log( 'the res,pdbRes, pstart, pend, start, end  ',res,pdbRes, this._mapData.pstart, this._mapData.pend, this._mapData.start, this._mapData.end );
            var script = 'select '+pdbRes+'; color red; spacefill true;';
            jmolScript( script, 'foo' );
            
          }
          
        } // end of mapData undefined
        
      } // end of isStructureViewerEnabled
      
    }.bind(this) ); // end of imgCanvas.observe
    
      
  },
  
  //-------------------------------------------------------------------------------
  
  // function to check whether feature viewer enabled.
  isFeatureViewerEnabled: function(){
    if( this.FeatureViewerEnabled !== undefined ){
      return this.FeatureViewerEnabled;
    }else{
      return false;
    }
  },
  
  // function to check whether structure viewer enabled.
  isStructureViewerEnabled: function(){
    if( this.StructureViewerEnabled !== undefined ){
      return this.StructureViewerEnabled;
    }else{
      return false;
    }
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
  
  //-------------------------------------------------------------------------------
  
  // function to set teh click status
  setClickProcessStatus: function( bool ){
    this.clickProcessStatus = bool;
  },
  
  //-------------------------------------
  
  // function to get the clicked status;
  getClickProcessStatus: function( ){
    return this.clickProcessStatus;
  },
  //------------------------------------------------------------------------------
  //- Private Methods ------------------------------------------------------------
  //------------------------------------------------------------------------------
  
  // function to parse the accession which is clicked;
  _parseAccession: function( acc ){
    
    // console.log( 'parseAccession is called with '+acc );
    
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
