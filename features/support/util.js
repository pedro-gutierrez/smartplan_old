var Client = require( './client.js' ),
    fse = require('fs-extra'),
    path = require( 'path' );
  

String.prototype.toBoolean = function() {
    if ( (/^true$/i).test(this) || (/^false$/i).test(this) ) {
    	return 'true' === this.toLowerCase();
    } else return null;
};

String.prototype.toNumber = function() {
    if( /^-?(\d+)$/.test(this) ) return parseInt( this );
    if( /^-?\d*(\.\d+)?$/.test(this) ) return parseFloat( this );
};

String.prototype.toNull = function() {
  return (/^null$/i).test(this) ? null : this;
}

var convert = function( v ) {
	var str = ''+v;
	var b = str.toBoolean();
	if( b != null ) {
		return b;
	}  else {
		b = str.toNumber();
		if( b != null ){
			return b;
		} else {
          if( !str.toNull() ) {
            return null;
          } else return v;
        }
	}
};

exports.convert = convert;

exports.http = function( w, callback, withClient, debug ) {
  return withClient( new Client(w) ).addListener( 'complete', function(d, r) {
        w.d = d;
        w.r = r;
        if( debug ) console.log( w.d );
        callback();
  });    
}

var UPLOAD_DIR = path.resolve( __dirname, "../../uploads" );
var DEFAULT_PHOTO_SRC = path.resolve( __dirname, "anonymous.png" );
var DEFAULT_PHOTO_DEST = path.resolve( UPLOAD_DIR, "98972d95-684d-4ef1-a758-b5682ef995de" );

exports.initUploads = function() {
  fse.removeSync( UPLOAD_DIR );
  fse.emptyDirSync( UPLOAD_DIR );
  fse.copySync( DEFAULT_PHOTO_SRC, DEFAULT_PHOTO_DEST );
}

exports.id_at = function(w, i) {
  return w.d[ convert(i)].id;
}

exports.withId = function( w, prop, value, cb, next) {
  var found = w.d.filter( function(i) {
    return i[prop] === value;
  });
  if( !found.length ) cb( new Error("No element with " + prop + "=" + value + "found in: " + JSON.stringify(w.d) )); 
  return next(found[0].id);
}


var retryInternal = function( rem, w, callback, getfn, checkfn, errFn, lastD, lastR ) {
  if( rem == 0 ) return callback( new Error( "Timeout reached: " + errFn(lastD, lastR) ));
  getfn( new Client(w) ).addListener( 'complete', function(d, r) {
    r.should.have.status(200);
    if( checkfn( d, r ) ) return callback();
    return retryInternal( rem-1, w, callback, getfn, checkfn, errFn, d, r );
  });
    
}


var retry = function( rem, w, callback, getFn, checkFn, errFn ) {
  retryInternal( rem, w, callback, getFn, checkFn, errFn, null, null );
}



exports.retry = retry;






