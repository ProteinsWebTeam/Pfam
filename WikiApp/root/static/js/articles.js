
//------------------------------------------------------------------------------
//- class ArticleApprover ------------------------------------------------------
//------------------------------------------------------------------------------

var ArticleApprover = Class.create( {

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function( articleUri, userUri, authorised ) {
    // console.debug( "ArticleApprover: initialising" );

    if ( authorised  === undefined ) {
      $("message").update( "You are not authorised to approve entries. Please login and try again." )
                  .show();
      return;
    }

    this._articleUri = articleUri;
    this._userUri    = userUri;
    this._authorised = authorised;

    // console.debug( "userUri: %s", this._userUri );

    this._addListeners();

    // console.debug( "ArticleApprover: done initialising" );
  },

  //----------------------------------------------------------------------------
  //- public methods -----------------------------------------------------------
  //----------------------------------------------------------------------------
  
  // no user-serviceable parts !

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // adds listeners to the various buttons in the "approve article" page

  _addListeners: function() {

    // add click listeners on the "approve" buttons in the table. These
    // will handle both approve and unapproval
    $$("button.approve").each( function( button ) {
      button.observe( "click", this._approveArticle.bind(this) );
    }.bind(this) );

    // add click listeners on the "approve all" buttons. These will only do
    // approvals
    $$("button.approveAll").each( function( button ) {

      button.observe( "click", function( e ) {

        var div = e.findElement("div");
        div.select("tr.article").each( function( tr ) {
          this._approve( tr );
        }.bind(this) );

      }.bind(this) );

    }.bind(this) );

  },

  //----------------------------------------------------------------------------
  
  // toggles the status of an article between approved and unapproved

  _approveArticle: function(e) {

    var button = e.findElement( "button" );
    var tr     = button.up("tr");

    if ( button.hasClassName( "approve" ) ) {
      // approve row
      this._approve( tr );
      this._updateEditCounts( tr );
    } else {
      // UNapprove row
      this._unapprove( tr );
    }

  },

  //----------------------------------------------------------------------------

  // marks an article as approved

  _approve: function( tr ) {

    var title   = tr.down("a[rel='title']")
                    .innerHTML
                    .trim();
    var revid   = tr.down("a[rel='wikipedia_revision']")
                    .innerHTML
                    .trim();
    var button  = tr.down("button");

    // console.debug( "approving revision %d for article %s", revid, title );

    var uri = this._articleUri + '/' + encodeURIComponent(title) + '/approve/';
    var r = new Ajax.Request( uri, {
      parameters: {
        revid:       revid,
        approved_by: this._authorised
        // don't set "updated"; leave it undefined and the DB will set it to "NOW()"
      },
      onSubmit:  function() {
        button.disabled = "disabled";  // disable the button so it can't be clicked while
      },                               // we're in the middle of approving the article...
      onSuccess: function() {
        button.update("Unapprove")
              .removeClassName("approve")
              .addClassName("unapprove");
        tr.addClassName("articleApproved");
      },
      onFailure: function(response) {
        var msgDiv = tr.up("div").down("div.errorMessage");
        msgDiv.update( "Failed to approve article '" + title + "': " + response.responseText )
              .show();
      },
      onComplete: function() {
        button.disabled = "";          // re-enable the button
      }
    } );

  },

  //----------------------------------------------------------------------------

  // revert an article to unapproved

  _unapprove: function( tr ) {
    var approved_by = tr.down("td.approvedBy")
                        .innerHTML
                        .trim();
    var title       = tr.down("a[rel='title']")
                        .innerHTML
                        .trim();
    var revid       = tr.down("a[rel='approved_revision']") // roll back to the last
                        .innerHTML                         // approved revision
                        .trim();
    var updated     = tr.down("td.updated")                // and to the previous
                        .innerHTML                         // update timestamp
                        .trim();
    var button      = tr.down("button");

    // console.debug( "UNapproving article %s", title );

    var uri = this._articleUri + '/' + encodeURIComponent(title) + '/approve/';
    var r = new Ajax.Request( uri, {
      parameters: {
        revid:       revid,
        approved_by: approved_by,
        updated:     updated
      },
      onSubmit:  function() {
        button.disabled = "disabled";
      },
      onSuccess: function() {
        button.update("Approve")
              .removeClassName("unapprove")
              .addClassName("approve");
        tr.removeClassName("articleApproved");
      },
      onFailure: function(response) {
        var msgDiv = tr.up("div").down("div.errorMessage");
        msgDiv.update( "Failed to unapprove article '" + title + "': " + response.responseText )
              .show();
      },
      onComplete: function() {
        button.disabled = "";
      }
    } );
  },

  //----------------------------------------------------------------------------

  // increments the edit count for every user who made an edit to the approved
  // article

  _updateEditCounts: function( tr ) {

    var users = tr.next("tr")
                  .select("a.user")
                  .invoke("readAttribute","rel");

    users.each( function( unescaped_user ) {
      var user = escape( unescaped_user );
      // console.debug( "incrementing edit count for '%s'", user );
      var uri = this._userUri + '/' + user + '/addedit/';
      
      var r = new Ajax.Request( uri, {
        onFailure: function(response) {
          var msgDiv = tr.up("div").down("div.errorMessage");
          msgDiv.update( "Failed to increment edit count for user '"
                         + user + "': " + response.responseText )
                .show();
        }
      } );

    }.bind(this) );

  }

  //----------------------------------------------------------------------------

} );

//------------------------------------------------------------------------------
//- class UserApprover ---------------------------------------------------------
//------------------------------------------------------------------------------

var UserApprover = Class.create( {

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function( userUri, authorised ) {
    // console.debug( "UserApprover: initialising" );

    this._userUri    = userUri;
    this._authorised = authorised;

    // console.info( "user %s authorised to make changes",
    //               ( this._authorised ? "is" : "is not" ) );

    $$("a.user").each( function( link ) {

      var div = new Element( "div", { "class": "userApproval" } );
      if ( link.hasClassName( "approved" ) ) {
        div.addClassName( "approved" );
      }
      link.insert( { after: div } );

      if ( this._authorised ) {
        div.update( "&nbsp;" )
           .observe( "mouseover", function() { div.addClassName( "approveToggle" ); } )
           .observe( "mouseout",  function() { div.removeClassName( "approveToggle" ); } )
           .observe( "click", this._toggleApproval.bind( this ) );
      }

    }.bind( this ) );

    // console.debug( "UserApprover: done initialising" );
  },

  //----------------------------------------------------------------------------
  //- public methods -----------------------------------------------------------
  //----------------------------------------------------------------------------

  // no user-serviceable parts !

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // handler for "click" events on the toggle switches. Finds the clicked 
  // element, finds the user associated with that switch and fires an AJAX 
  // request to the server to set the status

  _toggleApproval: function( e ) {

    // get the element that was clicked. This is the toggle switch
    var div = e.findElement( "div" );

    // walk from there to the link, which is where we can find out which
    // user we are dealing with here
    var user = div.adjacent( "a.user" )
                  .first()
                  .rel;

    // console.debug( "toggling approval status for %s", user );

    // build the URI that we'll use to toggle the status on the server side
    var uri = this._userUri + "/" + user + "/toggleapproval";

    // send the request 
    var r = new Ajax.Request( uri, {
      onSuccess: function( response ) {
        this._setApprovalStatus( user, response.responseText == 1 ? true : false );
      }.bind( this )
    } );

  },

  //----------------------------------------------------------------------------

  // walks the DOM and find all of the toggle switches for the specified user,
  // then sets the approval status of each to the specified value

  _setApprovalStatus: function( user, approved ) {
    // console.debug( "setting approval status for %s to %d", user, approved );
    $$("a.user[rel="+user+"]").each( function( link ) {
      var div = link.adjacent( "div.userApproval" ).first();
      if ( approved ) {
        div.addClassName( "approved" );
      } else {
        div.removeClassName( "approved" );
      }
    } );
  }

  //----------------------------------------------------------------------------

} );

//------------------------------------------------------------------------------

