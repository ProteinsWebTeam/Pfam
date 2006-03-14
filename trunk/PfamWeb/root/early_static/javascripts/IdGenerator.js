
// IdGenerator class
// jt 20060308 WTSI
//
// generate unique HTML IDs on request

// class variables
IdGenerator.ids = new Object();
IdGenerator.stub = "id";
IdGenerator.counter = 0;

// constructor
function IdGenerator() {

}

// class methods
IdGenerator.getId = function( sStub ) {
  var stub;
  if( typeof sStub == "undefined" || sStub == "" ) {
	stub = IdGenerator.stub;
  } else {
	stub = sStub;
  }
  var id = stub + IdGenerator.counter++;
  if( YAHOO.util.Dom.get( id ) ) {
	return IdGenerator.getId( sStub );
  } else {
	return id;
  }
}
