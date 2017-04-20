 (function() {
   
    Object.byString = function(o, s) {
      if( !o ) {
        console.log( "[ERROR] [Object.byString] [" + s 
                    + "] invalid argument", o );
        return;
      }
      s = s.replace(/\[(\w+)\]/g, '.$1');
        s = s.replace(/^\./, '');
        var a = s.split('.');
        for (var i = 0, n = a.length; i < n; ++i) {
            var k = a[i];
            if (k in o) {
                o = o[k];
            } else {
                return;
            }
        }
        return o;
    }
    
    Object.defineProperty( Object.prototype, "$isEmpty", {
       value: function() {
         for( var key in this ) return !this.hasOwnProperty(key) ;
         return true;
       } 

    });
    
    Object.defineProperty( Object.prototype, '$map', {
      value: function(f, ctx ){
        ctx = ctx || this;
        var self = this;
        for( var k in self ) {
          if( self.hasOwnProperty(k))
            f.call( ctx, k, self[k], self );
        }
      }
    });

    Object.defineProperty( Object.prototype, "$toArray", {
      value: function() {
        var a = [];
        this.$map( function(i){ a.push(i); } );
        return a;
      }
    });

    Object.defineProperty( Object.prototype, 'forEachEntry', {
      value: function(f, ctx ){
        this.$map( f, ctx )
      }
    });
   
    Object.defineProperty( Object.prototype, 'simpleCopy', {
      value: function(){
        return JSON.parse(JSON.stringify(this));
      }
    });
  
	
	Object.defineProperty( Object.prototype, 'isNumber', {
		value: function(){
			var trimmed = (""+this).trim();
			return trimmed.length > 0 && !isNaN(trimmed);
		}
	});
	
	Object.defineProperty( Object.prototype, 'isInt', {
		value: function(){
			return this.isNumber() && this % 1 ==0;
		}
	});
	
	Object.defineProperty( Object.prototype, "$update", {
		value: function(source) {
			var self = this;
			source.forEachEntry( function(k, v) {
				self.hasOwnProperty(k) ? self.$set( k, v ) : self.$add( k, v );
			});
		}
	});
  
    Object.defineProperty( Object.prototype, "$qs", {
        value: function(){
            var self = this;
            return Object
                  .keys(this)
                  .map(function(key){
                    return key+"="+encodeURIComponent(self[key])
                  })
                  .join("&");
        }  
    });
	
	if( !String.prototype.$format ) {
		String.prototype.$format = function( args ) {
			var formatted = this;
			for (var i = 0; i < args.length; i++) {
				var regexp = new RegExp('\\{'+i+'\\}', 'gi');
				formatted = formatted.replace(regexp, args[i]);
			}
			return formatted;
		}
	};
    
    if( !String.prototype.$isEmpty ) {
		String.prototype.$isEmpty = function() {
			return this.length == 0;
		}
	};
    
    if( !String.prototype.$is ) {
        String.prototype.$is = function(n) {
            return parseInt(this) == n;
        }
    }
    
	
    String.prototype.startsWith = function (str){
      	return this.indexOf(str) === 0;
    };
	
	
    String.prototype.contains = function(str) {
      return this.indexOf(str) != -1;	
    };

    if( !String.prototype.$contains ) {
      String.prototype.$contains = function(v) {
        return this.indexOf(str) != -1;
      };
    }
    
    if( !String.prototype.$match ) {
        String.prototype.$match = function(pattern) {
            var r = this.match(pattern);
            return (r != null && r.length > 0);
        }
    }
    
    if( !String.prototype.$capitalize ) {
        String.prototype.$capitalize = function() {
            return this.charAt(0).toUpperCase() + this.slice(1).toLowerCase();
        }
    }
    
	
	Array.prototype.clone = function() {
	   return this.slice(0);
    };
	
	
	if( !Array.prototype.$findIndex ) {
		Array.prototype.$findIndex = function( comp ) {
			for( var i=0; i<this.length; i++ ) {
				if( comp(this[i]) ) return i;
			}
			return -1;
		}
	} 
	
    if( !Array.prototype.$isEmpty ) {
		Array.prototype.$isEmpty = function() {
			return this.length == 0;
		}
	}
    
	if( !Array.prototype.$findIndexUsing ) {
		Array.prototype.$findIndexUsing = function( prop, value ) {
			return this.$findIndex( function(i) {
				return i[prop] === value;	
			});
		}
	}
	
	if( !Array.prototype.$find ) {
		Array.prototype.$find = function( comp ) {
			for( var i=0; i<this.length; i++ ) {
				if( comp(this[i]) ) return this[i];
			}
			return null;		
		}
	}
	
	if( !Array.prototype.$findUsing ) {
		Array.prototype.$findUsing = function( prop, value ) {
			var comp = function(i) {
				return i[prop] === value;	
			};
			
			for( var i=0; i<this.length; i++ ) {
				if( comp(this[i]) ) return this[i];
			}
			return null;		
		}
	}
	
	
	if( !Array.prototype.$contains ) {
		Array.prototype.$contains = function( comp ) {
			return this.filter( comp ).length != 0;	
		}
	}
	
	if( !Array.prototype.$containsUsing ) {
		Array.prototype.$containsUsing = function( prop, value ) {
			return this.filter( function(i) {
				return i[prop] === value;	
			}).length != 0;	
		}
	}
	
	if( !Array.prototype.$removeUsing ) {
		Array.prototype.$removeUsing = function( prop, value ) {
			var i = this.$findIndexUsing( prop, value );
			if( i > -1 ) this.splice(i, 1);
		}
	}
	
	if( !Array.prototype.$containsUsing ) {
		Array.prototype.$containsUsing = function( prop, value ) {
			return this.filter( function(i) {
				return i[prop] === value;	
			}).length != 0;	
		}
	}
    
    if( !Array.prototype.$last ) {
        Array.prototype.$last = function() {
            return this[this.length-1];
        }
    }
    
    if( !Array.prototype.$containsTranslate ) {
        Array.prototype.$containsTranslate = function( v, onTrue, onFalse ) {
            return (this.indexOf(v) != -1 ? onTrue: onFalse );
        }
    }
    
    if( !Array.prototype.$removeAll ) {
        Array.prototype.$removeAll = function(e) {
            for (var i=this.length-1; i>=0; i--) {
                if (this[i] === e) {
                    this.splice(i, 1);
                }
            }
            return this;
        };
    }
	
	
	Array.prototype.insert = function (index, item) {
  		this.splice(index, 0, item);
	};
	
	Array.prototype.remove = function(cb) {
		for (var i=this.length-1; i>=0; i--) {
    		if (cb( this[i] )) this.splice(i, 1);
       	}
	};
	
    var each = function( obj, callback ) {
        for( var k in obj ) {
            if( obj.hasOwnProperty(k) ) {
                var v = obj[k];
                if( v ) callback( k, v );    
            }
        }
    };
	
	function _p8(s) {
    	var p = (Math.random().toString(16)+"000000000").substr(2,8);
        return s ? "-" + p.substr(0,4) + "-" + p.substr(4,4) : p ;
    }
    
    var stateToProp = function(s){
        return "is"+s.toLowerCase();
    }
		
	//
    // Extends Vue with our Finite State machine DSL
    //
    Vue.use( function( V, options ) { 

        V.prototype.$_init = function( n ) {
            this.states = {};
            this.current = { name: 'NULL', transitions:{} };
			this._in = null;
            this.children = [];
            this.parent = null;
            this._name = n;
            this.initial = null;
		}
        
        
		
		//
		// Returns true if the fsm is in the given state
		//
		V.prototype.$is = function( s ) {
			return s === this.current.name;	
		}
        
		V.prototype.$_moveTo = function(state ) {
            if (this.current && "NULL" != this.current.name ) 
                this.$data["is_"+this.current.name.toLowerCase()]=false;
            if( this.current && this.current.name != "NULL" ) this.$debug( "-> [" + state.name + "]" );
            this.current = state;
            this.$data["is_"+this.current.name.toLowerCase()]=true;
            
        }
        
        V.prototype.$_applyUiRules = function() {
            if( this.$options.ui ) {
                var fsm = this;
                this.$options.ui.forEachEntry( function(prop, rules) {
                    var value=null;
                    switch( typeof rules ) {
                        case "object":
                            value = rules.hasOwnProperty( fsm.current.name ) ? rules[fsm.current.name] 
                                : (rules.$OTHERWISE || rules.$DEFAULT || "");
                            break;
                        case "function":
                            value = rules.apply(fsm);
                            break;
                        case "string":
                            value = (rules === "$state" ) ? fsm.current.name.toLowerCase() : rules;
                            break;
                    }
                    
                    fsm.$data[prop]=value;
                    //fsm.$debug( "[UI] " + prop + " => \"" + value + "\"" );
                });
            }
        }
		
        //
        // Defines a new state in the state machine
        // If this is the first time we call this function on the instance,
        // then the given state becomes the default initial state for the FSM.
        //
        V.prototype.$in = function( s ) {
            var state = { name: s.toUpperCase(), transitions: {} };
            this.states[ state.name ] = state;
            if( !this.current || this.current.name === 'NULL'  ) {
                this.initial = state.name;
                this.$_moveTo( state );
				
				// If defined, apply the init state hook
				if( this.$options.hooks ) {
					var spec = this.$options.hooks.$init;
					if( spec ) this.$sendSpec( spec );
				}
                
                this.$_applyUiRules();
                
            }

            this._in = state;
            return this;
        }

        //
        // Defines a new transition for the current state and the specified
        // event
        //
        V.prototype.$when = function( e ) {
            var eName = e.toLowerCase();
            var t = { event: eName, ops: [], conditions: [] };
            if( !this._in.transitions[eName] ) this._in.transitions[eName]=[];
            this._in.transitions[eName].push(t);
            this._in._in = t;
            return this;
        }

        //
        // Defines a new operation for the current transition
        //
        V.prototype.$then = function( n ) {
            this._in._in.next = n;
            return this;
        }
            
        //
        // Short for '$receive'. To be used in the html.
        // 
        V.prototype.$r = function(e, args ){
          this.$receive(e, args );
        }
        
        //
        // Short for '$send'. To be used in the html.
        // 
        V.prototype.$s = function(t, m){
            Vue.$send( t, m );
        }
        
		//
		// Translates the specified
		// object spec into a Vue.$send command, implementing
		// some conventions
		//
		V.prototype.$sendSpec = function(s){
			var self = this;
			if( "string" === typeof(s) ) {
				this.$receive( s );	
			} else {
				s.forEachEntry( function(t, e) {
					( t === '$parent' ) ? self.$sendParent( e ) : Vue.$send( t, e );		
				});
			}
		}
		
		//
		// Sends a message to the parent fsm
		//
		V.prototype.$sendParent = function(e, args) {
			if( this.$parent ) Vue.$send( this.$parent._name, e, args );
		}
        
        //
        // Ask pattern, for a kind of synchronous request/response
        // interaction between two state machines
        //
        V.prototype.$ask = function( to, e, args ) {
            Vue.$ask( this._name, to, e, args );
        }
        
        V.prototype.$clearTimeout = function() {
            if( this.$_timeout ) {
                clearTimeout( this.$_timeout );  
                this.$debug( "(!) timeout cancelled" );
                this.$_timeout = null;
            }
        }
        
        //
        // Self-enqueue the specified event and arguments.
        // This is done asynchronously by adding the appropiate
        // function to the event-loop
        //
        V.prototype.$receive = function(e) {
            var self = this;
            var args = Array.prototype.slice.call(arguments);
            args.shift();
            setImmediate( function() {
                self.$_receive( e, args );     
            });
            
            //asap(function() {
            //    self.$_receive( e, args );     
            //});
            
            //setTimeout( function() {
            //    self.$_receive( e, args );        
            //}, 0 );
        }
        
        //
        // Handles the specified event and arguments, according to
        // the logic defined by the state machine definition. This function
        // can look complicated, but it allows to define the application logic in terms 
        // of states, events and operations.
        //
        V.prototype.$_receive = function( e, args ) {
            var fsm = this;
            var e = e.toLowerCase();
            this.e = e;
            if( this.current ) {
            var allT = this.current.transitions[e];
            if( !allT ) {
                
                //
                // No transition was found for the specified event
                // on the current state. This is could be a design error or 
                // it could be something intended (the state machine is preventing a race 
                // condition from happening). The event will be ignored
                // but we keep a trace in the logs so that it can be analyzed by
                // the programmer
                //
                this.$debug( "<- " + e + " (?)"  );
                return;
            }
				
            this.$debug( '<- ' + e  );
            for( var i=0; i<allT.length; i++) {
                var t = allT[i];
                if( t && t.next ) {
                    var n = this.states[t.next];
                    if( n ) {
                        
                        //
                        // Look for failed conditions. 
                        //
                        var failedC = t.conditions.filter( function(f) {
                            return !f.apply( fsm, args );
                        });
                        
                        //
                        // If a transition
                        // has no failed conditions, or has no conditions at all,
                        // then it will selected an executed.
                        //
                        if( !failedC.length ) {
                            
                            var oldState = this.current.name;
                            var stateChanged = ( t.next != oldState );
                            
                            //
                            // Log the transition to a new state
                            //
                            if( stateChanged ) {
                                this.$_moveTo( n );

							}
                                
                            //
                            // Schedule any timeouts defined by this transition 
                            //
                            if( t.timeout ) {
                                fsm.$_timeout = setTimeout( function(){
                                    fsm.$receive( "timeout" );
                                }, t.timeout*1000 );
                                fsm.$debug( '(!) timeout=' + t.timeout );
                            }
                            
                            //
                            // Execute all the operations defined by this transition
                            // in order
                            //
                            t.ops.map( function(f){ 
								f.apply(fsm, args); 
                            });
                            
                            if( stateChanged ) {                               
                                
                              this.$_applyUiRules();

                              if( this.$options.hooks ) {
                                var spec = this.$options.hooks.$state;
                                if( spec ) this.$sendSpec( spec );
                              }

                              if( StateObs[this._name] ) {
                                StateObs[this._name].map( function(obs) {
                                  var e = obs.s[t.next] || obs.s.$OTHERWISE;
                                  if( e ) Vue.$send( obs.v, e );
                                });
                              }
                            }
                            
                            break;

                        }
                    } else this.$error( "Undefined state '" + t.next + "'" )
                }
              }
            } else this.$error( "No current state defined?" );
            //return this;
        }
        
        
       
        
        var $_resolveValue = function( source, prop ) {
            if( typeof( prop ) === "string" )    
                return Object.byString( source, prop )
            else {
                if( prop.value ) return prop.value;
            }
        }
        
        var $_resolveArgs = function( spec, source ) {
            
            if( "string" === typeof( spec ) ) {
                //return source[spec];
                return Object.byString( source, spec);

            } else {
                
                var args = {};
                
                if( spec instanceof Array ) {
                    spec.map( function(prop){
                        args[prop.split(".").$last()]= $_resolveValue( source, prop )
                    });
                
                } else {
                    spec.$map(function(k, v) {
                        args[k]= $_resolveValue( source, v )      
                    });
                }
                
                return args
            }
        }
        
        var $_resolveEv = function(ev, params, source) {
            params.map( function(p) {
                return Object.byString( source, p );    
            }).map( function(v) {
                ev = ev +"-" + v;    
            });
            
            return ev;
        }
        
       
        var httpFn = function( f, fsm, action, method ) {
          return function() {
            var opts = {};
            if( f.body ) opts.data=$_resolveArgs( f.body, fsm.$data );
            opts.path = "/api/" + action;
            opts.method = method || "GET";
            opts.binary = f.binary;
            opts.raw = f.raw;
            fsm.$http( opts );
          }
        };
    

        //
        // This function resolves a operation defined by the user (via the DSL) into an executable function.
        //
        // For example:
        // - if the specified argument is a function, then no further work is required
        // - if the argument is a string, and it does not match an own method, then it is considered as a new
        //      event that must be self-enqueued.
        // - if the argument matches the name of an existing method, then the method itself will be returned
        //
        V.prototype.$_resolveFn = function(f) {
            var fsm = this;
            switch( typeof(f) ) {
                case "function": 
                    
                    //
                    // The user defined an inline function in the DSL
                    //
                    return f;
                
                case "string":
                    
                    if( !this.hasOwnProperty(f) || (typeof( this[f] ) != "function" ) ) {
                        
                        //
                        // The user defined an event that needs to be self-enqueued
                        // when the transition is executed
                        //
                        return function( msg ) {
                            fsm.$receive( f, msg );    
                        }
					
                    //
                    // The user defined the name of an existing method, as an op in
                    // a transition. Such a method will be returned (using recursion, in case
                    // we want to do fancier stuff in the future).
                    //
                    } else {
                        return this.$_resolveFn( this[f] );
                    }
                    
                case "object":
                    
                    if( f.send ) {
                        
                        return function(msg) {
                          var params = f.args || f.data;
                          var args = params ? $_resolveArgs( params, fsm.$data ) : msg;
                          args = (args && args.simpleCopy) ? args.simpleCopy() : args;
                          (f.to === '$parent') ? fsm.$sendParent(f.send, args) :  Vue.$send(f.to, f.send, args);   
                        }
                        
                    } else if( f.ask ) {
                      
                        return function(msg) {
                            var args = f.args ? $_resolveArgs( f.args, fsm.$data ) : msg;
                            fsm.$ask( f.ask, f.for, args );    
                        }
                        
                    } else if( f.reply ) {
                        
                        return function(msg) {
                            var $from = msg.$from;
                            Vue.$send( $from, f.reply, fsm.$data[f.body] );
                        }
                        
                    } else if( f.http ) {
                        
                      return httpFn( f, fsm, f.http, "GET");    
                    
                    } else if( f.get ) {
                      
                      return httpFn( f, fsm, f.get, "GET");    
                    
                    } else if( f.post ) {
                        
                      return httpFn( f, fsm, f.post, "POST");    

                    } else if( f.ws ) {
                        
                        return function() {
                            var spec = { action: f.ws };
                            if( f.body ) spec.body=$_resolveArgs( f.body, fsm.$data  );
                            fsm.$debug( JSON.stringify(spec) );
                            fsm.$ws(spec);
                        }
                        
                    } else if( f.pub ) {
                        
                        return function(msg) {
                            var arg = msg;
                            if( f.msg ) {
                                arg = f.msg;
                            } else if( f.data ) {
                                arg = $_resolveArgs( f.data, (f.source === "msg") ? msg : fsm.$data );
                            }
                            
                            var event = f.pub;
                            if( f.params ) event = $_resolveEv( event, f.params, (f.source === "msg") ? msg : fsm.$data );
                            fsm.$pub( event, (arg && arg.simpleCopy) ? arg.simpleCopy() : arg );    
                        }    
                    
                    } else if( f.sub ) {
                        
                        return function(msg) {
                            var src = $_resolveEv( f.when, f.params, (f.source === "msg") ? msg : fsm.$data );
                            fsm.$sub( f.sub, src, f.then );
                        }
                    
                    } else if( f.unshift ) {
                        
                        return function(msg) {
                            this.$data[f.unshift].unshift( msg.body || msg );
                        }
                        
                    } else if( f.inherit ) {


                    
                    } else if( f.set ) {
                        
                        return function(msg){
                          if( "string" === typeof( f.set) ) {
                            fsm.$data[f.set]=msg;    
                          } else {
                            $_resolveArgs( f.set, msg || fsm.$data ).$map( function(k, v) {
                              fsm.$data[k]=v; 
                            });    
                          }
                        }
                        
                    } else if( f.unset ) {
                        
                        return function(msg){
                            f.unset.map( function(field) {
                                fsm.$data[field]="";    
                            });
                        }
                        
                    } else if( f.push ) {
                        return function(msg) {
                            var array =this.$data[f.push];
                            array.push(msg);
                            
                            //var key = f.key || "id";
                            //if( !f.unique || !array.$containsUsing( key, msg.body[key])) {
                            //    array.push(msg.body);
                            //} 
                        }
                    } else if( f.pull ) {
                        
                        return function(msg) {
                            var array =this.$data[f.pull];
                            array.$removeAll(msg);        
                        }
                        
                    } else if( f.empty ) {
                        return function(msg){
                            var v = Object.byString( msg || fsm.$data, f.empty );
                            return (v == null || v.$isEmpty());
                        }
                        
                    } else if ( f.notEmpty ) {
                        return function(msg){
                            var source = msg;
                            if( !msg || f.source === 'data' ) source = fsm.$data;
                            var v = Object.byString( source, f.notEmpty );
                            return (v != null && !v.$isEmpty());
                        }
                    } else if( f.positive ) {
                        
                        return function(msg){
                            var v = Object.byString( msg || fsm.$data, f.positive );
                            return (v != null && v > 0);
                        }
                     
                    } else if ( f.has ) {
                        
                        return function(msg){
                            var src = msg || fsm.$data;
                            return f.has.map( function(prop){
                                return Object.byString(src, prop);
                            }).filter( function(v) {
                                return v == null || v.$isEmpty();
                            }).$isEmpty();
                            
                        }
                    } else if( f.action ) {
                        
                        return function(msg){
                            return msg.action && msg.action === f.action;   
                        }
                        
                    } else if( f.broadcast ) {
                        return function() {
                            Vue.$broadcast( fsm, f.broadcast );
                        }
                    }  else if( f.isNewIn ) {
                        
                        return function(msg) {
                            
                            if( !fsm.$data[f.isNewIn] ) {
                                fsm.$error( "[isNewIn] undef property: " + f.isNewIn ); 
                                return false;
                            }
                            
                            if( f.using ) {
                                return !fsm.$data[f.isNewIn].$containsUsing( f.using, msg[f.using] );
                            } else {
                                var v = ( f.value ) ? fsm.$data[f.value] : msg
                                if( !v || v.$isEmpty() ) return false;

                                return !fsm.$data[f.isNewIn].$contains( f.comp || function(i) {
                                    return i === v;    
                                });
                            }
                        }
                    } else if( f.eq ) {
                        
                        return function(msg) {
                            var src = (f.source === 'msg' ) ? msg : fsm.$data;
                            var v1 = Object.byString(src, f.eq);
                            var v2 = f.value || Object.byString( fsm.$data, f.data );
                            return ""+v1 === ""+v2;
                        }
                    } else if( f.neq ) {
                        
                        return function(msg) {
                            var src = (f.source === 'msg' ) ? msg : fsm.$data;
                            var v1 = Object.byString(src, f.neq);
                            var v2 = f.value;
                            return ""+v1 != ""+v2;
                        }
                    }
                    
				default:
                    fsm.$error( "can't resolve spec of type " + typeof(f) + " to a function", f );
					break;
            }
        }

        //
        // Add a new operation to the current transition. 
        //
        // An operation gets resolved to a function that is 
        // is turn appended to the list of operations 
        // for execution at a later stage
        //
        V.prototype.$and = function( n ) {
            var f = this.$_resolveFn(n);
            this._in._in.ops.push( f );
            return this;
        }
        
        //
        // Add a new condition to the current transition
        //
        // A condition is just like an operation (a function), except that it is 
        // expected that it returns a boolean value, that defines
        // whether or not the condition is satisfied.
        //
        // Once resolved, the function is appended to the list of 
        // functions for execution at a later stage
        //
        V.prototype.$andIf = function( n ) {
            var f = this.$_resolveFn(n);
            this._in._in.conditions.push( f );
            return this;
        }
            
        //
        // Schedule a new timeout as part of the current transition
        //
        V.prototype.$withTimeout = function( secs ) {
            this._in._in.timeout = secs;  
            return this;
        }


        V.prototype.$debug = function( msg, data ) {
            if( Vue.$config.verbose && !this.silent ) {
              var msg = "[D] [" + this._name + "] ["
                + this.current.name + "] " + msg;
              (data != null) ? console.log( msg, data ) : console.log( msg );
            }
        }

        V.prototype.$error = function( msg, data ) {
          var msg = "[E] [" + this._name + "] ["
            + this.current.name + "] " + msg;
          (data != null) ? console.error( msg, data ) : console.error( msg );
        }

        V.prototype.$log = function( msg, data ) {
            if( Vue.$config.verbose && !this.silent ) { 
                (data != null) ? console.log( msg, data ) : console.log( msg );
            }
        }
        
        //
        // Convenience method added to all FSMs since most 
        // of the time we are interacting with http web services. 
        //
        // By using this 
        // pattern, we are centralizing all http requests in a single 
        // FSM (the 'app' fsm, which is by convention something 
        // that should exist on every app that relies on this framework)
        //
        V.prototype.$http = function( opts ) {
            opts.from = this._name;
            Vue.$send( 'app', 'http', opts ); 
        }
		
		//
        // Convenience method added to all FSMs since most 
        // of the time we are interacting with http web services. 
        //
        // By using this 
        // pattern, we are centralizing all websocket requests in a single 
        // FSM (the 'app' fsm, which is by convention something 
        // that should exist on every app that relies on this framework)
        //
        V.prototype.$ws = function( data ) {
           	data.from = this._name;
            Vue.$send( 'app', 'wsreq', data ); 
        }
		
		//
		// Event emitter kind of framework, but avoids chicken-egg
		// conditions 
		//
		V.prototype.$pub = function( ev, args ){
            var self = this;
            setTimeout( function(){
                self.$sync_pub(ev, args);      
            }, 0 );
        }
        
        //
        // Subscribe on the specified vue, for a "src" published
        // event, and receive "target" event on myself
        //
        V.prototype.$sub = function( v, src, target ) {
            if( "$parent" === v ) v = this.$parent._name;
            if( !EventObs[v] ) EventObs[v]=[];
            var myRules = EventObs[v].$findUsing( "v", this._name );
            if( !myRules ) {
                var events = {};
                events[src]=target;
                EventObs[v].push({ v: this._name, e: events });
            } else {
                myRules.e[src]=target;
            }  
            this.$debug( "[sub] [" + v + "] on [" + src + "]" );
            
        }

        V.prototype.$unsub = function( v, src ) {
          if( "$parent" === v ) v = this.$parent._name;
          if( EventObs[v] ) {
            var myRules = EventObs[v].$findUsing( "v", this._name );
            if( myRules ) {
              var events = myRules.e;
              if( events ) delete events[src];
            }
          }
        }
        
        V.prototype.$unsubAll = function() {
            var name = this._name;
            var self = this;
            EventObs.$map( function( k, v) {
            //   EventObs[k].$removeUsing( v, name );
              EventObs[k] = v.filter( function( obs ) {
                return obs.v != name;
              });
            }); 
            
            delete EventObs[name];
        }
        
        
        V.prototype.$sync_pub = function( ev, args ) {
            var self = this;
			if( EventObs[this._name] ) {
                EventObs[this._name].map( function(obs) {
                	var e = obs.e[ev];
                    if( e ) {
                        self.$debug( "[PUB] " + e + " ~> " + obs.v );
					    Vue.$send( obs.v, e, args );
                    }
				});
			} else this.$debug( "[PUB] " + ev + " (no listeners configured)" );    
        }
		
        V.prototype.$uuid = function() {
          uuid(); 
        }
       

    

        //
        // Basic i18n support
        //
        V.prototype.$i18n = function( key, args ) {
            return Vue.$i18n( key, args );
        }
        
        
        V.prototype.$upload = function( opts ) {
            Vue.$upload({
                el: opts.el,
                from: this._name,
                path: opts.path
            });
        }

        V.prototype.$download = function( data, name, type ) {
          Vue.$download( data, name, type );
        }
		
	
    });
    
    //
    // This function parses the DSL and compiles a new transition
    // using the programmatic api ($when, $then, $and, etc...)
    //
    var compileTransition = function( vue, e, t ) {
        
        // define a new transition for the given event
        vue.$when(e); 
        
        // optional: compile the condition(s). multiple
        // conditions can be defined in an array
        if( t.if ) { 
            if( t.if instanceof Array )
                t.if.map( function(c){ vue.$andIf( c ) } );
            else vue.$andIf( t.if );
        }
        
        // define the next state
        vue.$then( t.then ); 
        
        // optional: compile the operations for this transition. multiple
        // operations can be defined in a array
        if( t.and ) {
            if( t.and instanceof Array ) 
                t.and.map( function(a){ vue.$and( a ) } );
            else vue.$and( t.and );
        } 
        
        // optional: schedule a timeout as part of this transition
        if( t.timeout ) vue.$withTimeout( t.timeout );
    }
    
    var uuid = function() {
      return _p8() + _p8(true) + _p8(true) + _p8();
    }


    //
    // Hook function supported by the Vue framework. 
    // This the extension point
    // for every single vue instance we define
    //
    var created = function() {
      
        //
        // Resolves the name of the fsm. In most cases, it is 
        // just a name, but in some other cases, the name can be 
        // dynamically generated (eg. when defining fsms as vue components
        // driven off a dynamic list of data)
        //
        var name = this.$options._name;
        if( "function" ===  typeof(name) ) {
            var prefix = name().toLowerCase();
            name = prefix + "-" + uuid(); 
            //var sibblings = Object.keys( Vues ).filter( function(n){ return n.startsWith( prefix ); });
            //name = prefix + "-" + (sibblings.length  + 1);
        }
		
      	this.$_init( name );

        if( Vues[ this._name ]  ) {
            this.$error( "[NEW INSTANCE] [ERROR] already exists]");
            return;
        } else { 
            Vues[ this._name ] = this;
        }

        this.silent = this.$options.silent;
        var vue = this;
        var fsm = this.$options.states;
        
        //
        // Plugs globally user defined methods into each 
        // fsm instance
        // 
        for( var k in Methods ) {
            if( Methods.hasOwnProperty( k ) ) {
               if( !vue.hasOwnProperty(k) ) {
                   vue[k] = Methods[k];
               }
            }
        }

        //
        // Compiles the FSM from its DSL definition 
        //
        if( fsm ) {
            //
            // Iterate over all states defined
            //
            for( var s in fsm ) {
                if( fsm.hasOwnProperty(s) ) {
                    
                    // defines a new state
                    vue.$in( s );

                    vue.$when( 'http' ).$then( s ).$and( function( opts ) {
                        Vue.$http( vue, opts );    
                    });
				    
                    vue.$when( 'clearTimeout' ).$then( s ).$and( function( opts ) {
                        vue.$clearTimeout();  
                    });
				
                    vue.$when( 'wsreq' ).$then( s ).$and( function( opts ) {
                     	Vue.$_ws( opts );    
                    });
                  
                    if( !vue.$options.reset || !vue.$options.reset.disabled ) {
                        vue.$when( "$reset" ).$then( vue.initial ).$and( function(opts){
                            if( vue.$reset ) {
                                vue.$reset();
                                vue.$debug( "[RESET OK]");
                            }
                        });
                    }
                    
                    //
                    // compile the transitions for the state
                    //
                    for( var e in fsm[s] ) {
                        if( fsm[s].hasOwnProperty(e) ) {
                            var t = fsm[s][e];
                            if( t instanceof Array )
                                t.map( function(t){ compileTransition( vue, e, t ); } );
                            else compileTransition( vue, e, t );
                        }
                    }


                }
            }

        }
        
        //
        // Register state and event listeners defined
        //
        //
        var listen = this.$options.listen;
        if( listen ) {
            
            if( listen.states ) {
                each( listen.states, function( t, states ) {
                    if( "$parent" === t ) t = vue.$parent._name;
                    if( !StateObs[t] ) StateObs[t] = [];
                    if( StateObs[t].$containsUsing( "v", vue._name ) ) {
                        this.$error( "More than 1 instance of name [" + vue._name + "] listening for states on [" + t + "]" )   
                    } else { 
                        StateObs[t].push({ v: vue._name, s: states } );
                        vue.$debug( "listening to [" + t + "] states with " + JSON.stringify(states) );
                    }
                    
                });
            }
			

            if( listen.events ) {
              each( listen.events, function( t, events ) {
                if( "$parent" === t ) t = vue.$parent._name;
                if( !EventObs[t] ) EventObs[t] = [];
                if( EventObs[t].$containsUsing( "v", vue._name) ) {
                  vue.$error( "More than 1 instance of name [" + vue._name + "] listening for events on [" + t + "]" )   
                } else { 
                  EventObs[t].push({ v: vue._name, e: events.simpleCopy() } );
                  vue.$debug( "listening to [" + t + "] events with " + JSON.stringify(events) );
                }
              });
            }
			
            
            
        }
        
        if( this.$options.ws ) {
            Vue.$wsInit( this );
        }
		
        
    };
    
    
    var destroyed = function() {
        this.$unsubAll();
        this.$debug( "[DESTROYED]" );
        delete Vues[ this._name];
    }
		
    //
    // Global registry of vues, indexed by name
    //
    var Vues = {};
    
    //
    // Global registry of state transition observers,
    // indexed by fsm name
    //
    var StateObs = {};
	
	//
	// Global registry of event observers, indexed
	// by fsm name
	//
	var EventObs = {};
    
    //
    // Global, user-defined methods that will be injected
    // on every fsm defined
    //
    var Methods = {};
    
    Vue.$methods = function( m ) {
        $.extend( Methods, m );
    };
    
	//
	// Adds extra properties
	//
	var augment = function( name, opts, target ) {
		if( opts.ui ) {
			opts.ui.forEachEntry( function( prop, rules ) {
				if( !target[prop] ) { 
					target[prop] = rules.$DEFAULT || "";
				} else console.log( "[" + name + "] ui prop " + prop + " will be ignored (overlaps)"  );
			});
		}
        
        
        // adds a state prop for each state
        opts.states.$map( function(k, v){
            target["is_"+k.toLowerCase()]=false;    
        });
        
		return target;
	}
	
	//
    // Defines a new vue  component fsm. This is useful
    // for prototype kind of fsms (dynamically generated)
    //
    Vue.$comp = function( name, opts ){
        if( !opts ) opts = {};
        opts._name = opts.many ? function(){ return name } : name;
        if( !opts.states) opts.states = { DEFAULT: {}};
        var data = augment( name, opts, opts.data || {} );
        opts._defaultData = data.simpleCopy();
		opts.data = function() { return data.simpleCopy(); };
        opts.template = "#" + name;
		opts._type = name;
		opts.created = created;
        opts.destroyed = destroyed;
        Vue.component( name, Vue.extend(opts) );
	}
    
    //
    // Defines a new vue fsm. The most common case
    //
    Vue.$vue  = function( name, opts ){
        if( !opts ) opts = {};
        opts._name = name;
		opts._type = name;
        opts.el = "#" + name;
        if( !opts.states) opts.states = { DEFAULT: {}};
		opts.data = augment( name, opts, opts.data || {} );
        opts._defaultData = opts.data.simpleCopy();
       	opts.created = created;
        opts.destroyed = destroyed;
        new Vue( opts );
    }
    
    //
    // Enqueues the given event (e) with the specified args
    // to the FSM idenfied by its name. If the FSM is not defined
    // in the registry, then an error will be logged.
    //
    Vue.$send = function( name, e, args ){
        if( Vues[name] ) {
            Vues[name].$receive( e, args ) 
        } else console.warn( "[E] [" + name + "] [UNKNOWN] <- " + e + " (Dead Letter)" );
    };
    
    //
    // Implements the ask pattern by appending
    // the sender name into the list of arguments
    //
    Vue.$ask = function( from, to, e, args ) {
        args = args || {};
        args.$from = from; // trick
        Vue.$send( to, e, args );
    };
    
    //
    // Just like send, but for all FSMs defined.
    //
    Vue.$broadcast = function( from, e, args ){
        for( var v in Vues ) {
            if( v != from && Vues.hasOwnProperty( v ) ) {
                Vue.$send( v, e, args );    
            }
        }
    };
    
    //
    // Delays the action of delivering a message to an FSM,
    // by the specified number of seconds
    //
    Vue.$delay_send = function( name, e, args ){
      setImmediate( function() {
        Vue.$send( name, e, args );    
      });
    };
    
    var http_codes = {
        400: 'invalid',
        401: 'forbidden',
        404: 'not_found',
        409: 'conflict',
        403: 'forbidden',
        503: 'error',
        500: 'error',
        501: 'not_implemented'
    }
    
    //
    // Convenience wrapper over jquery's ajax object
    // Returns http responses and errors as messages enqueued 
    // into the caller fsm.
    //
    Vue.$http = function( owner, opts ) {
        var xhr = {
            type:  opts.method || 'GET',
            url: opts.path,
            data: opts.data,
            dataType: opts.dataType || 'json',
			success: function(data, status, resp){
                if( opts.binary ) {
                    data = new Blob( [ new Uint8Array(data) ], { type: resp.getResponseHeader( 'Content-Type' )  } );
                    if( !opts.raw ) data = (window.URL || window.webkitURL).createObjectURL( data );
                }
                
                if( Vue.$config.http.debug ) owner.$debug( "<- " + ( opts.binary ? 'binary' : JSON.stringify(data) ));
                opts.success ? opts.success( data ) : Vue.$send( opts.from, 'data', data );
            },
            error: opts.error || function(e) {
                var recipient = "app";
                if( e.status === 400 || e.status === 409 || e.status === 404 ) recipient = opts.from;
                var msg = e.responseJSON ? e.responseJSON : {};
                msg.status = http_codes[e.status] || 'error';
                if( Vue.$config.http.debug ) owner.$debug( "<- " + JSON.stringify(msg) );
                Vue.$send( recipient, msg.status, msg );
            }
        };
        
		
		
        if( opts.binary ) {
            xhr.dataType = "binary",
            xhr.processData = false;
            xhr.responseType = 'arraybuffer';    
        }
        
        if( Vue.$config.http.debug ) owner.$debug( "-> " + xhr.type + " " + xhr.url + " " + ( opts.data ? JSON.stringify( opts.data) : '' ) );
        if ( xhr.type === "GET" && xhr.data && opts.binary ) xhr.data = xhr.data.$qs(); 
		
		if( xhr.dataType === 'json' ) {
			xhr.contentType = "application/json";
			if( xhr.type === "POST" ) xhr.data = JSON.stringify(xhr.data);
		}
		
        $.ajax(xhr);
    },
    
	$.ajaxPrefilter( function( options, originalOptions, xhr ) {
		var id = Vue.$session();
        if( id ) xhr.setRequestHeader( 'session', id );
        xhr.setRequestHeader( 'app', Vue.$config.app_key );
        xhr.setRequestHeader( 'lang', Vue.$lang() );
	});

    Vue.$upload = function( opts ) {
        $( opts.el ).fileupload({
            url: opts.path,
            type: 'POST',
            paramName: "file",
            formData: opts.data || {},
            send: function(e, data ){
                Vue.$send( opts.from, "uploading" );
                return true;
            },
            
            done: function( e, data ){
                Vue.$send( opts.from || "app", "success" === data.textStatus ? "data" : "error", data.result );   
            },
            
            error: function(e, data){
                Vue.$send( opts.from || "app", "error", data.result );     
            }
        });
    };


    Vue.$download = function( data, name, type ) {
      //saveAs( data, name );
      download( data, name, type );
    }
    
    
    //
    // Very simple config registry
    //
    Vue.$config = {};

    //
    // Registers global configuration settings
    //
    Vue.$configure = function( opts ) {
        $.extend( Vue.$config, opts );   
    };
    
    //
    // Leverages Vue's event emmitter functionality built on every fsm,
    // in order to register for events emmitted by other FSMS. This is useful
    // to keep FSM's loosely coupled.
    //
    Vue.$on = function( target, event, callback ) {
        if( Vues[target] ) { 
            Vues[target].$on( event, callback ); 
        } else {
			console.log( "[E] [" + target + "] [NOT_REGISTERED YET] Can't bind listener for '" + event + "'" );  
		}
    };
    
    //
    // For testing purposes only. 
    // 
    // This method is delayed intentionnally
    // because we typically want to check the state of an fsm after having
    // triggered some event, however this is an asynchronous operation
    // therefore we need to leave enough time before checking
    //
    Vue.$assertState = function( name, state, next ) {
        var v = Vues[name];
        if( !v ) return alert( "[TEST] No such vue: " + name );
        setTimeout( function() {
            if ( v.current.name === state ) { 
                next();
            } else alert( "[TEST] [" + name + "] [" + v.current.name + "] Expected state: " + state ); 
        }, 1000 );
    }
    
    //
    // We allow a single WS connection per app
    //
   	var ws = null;
    
	//
	// Configures the given SockJs instance. Takes 
	// care of reconnections and optionally prints 
	// out every outgoing/incoming message, for debugging
	// purposes
	//
	var config_ws = function( newWS, owner ) {
		
		if( Vue.$config.ws.debug ) {
			var _send = newWS.send;
			newWS.send = function(msg) {
				if( newWS.readyState == 1 ) {
					owner.$debug( '-> ' + msg );
					_send.apply(newWS, [msg]);
				} else owner.$error( "WS ready state: actual=" + ws.readyState + ", expected=1"  );
			}
		}
		
		newWS.onopen = function() {
        	owner.$receive( "connected" );   
       	};
		
		newWS.onclose = function() {
			setTimeout( function() {
				var newWS = wscreate();
				ws = config_ws( newWS, owner );
			}, (Vue.$config.ws.reconnect || 10)*1000 );
			owner.$receive( "disconnected" );   
        };
		
		newWS.onmessage = function(m){
			var msg = JSON.parse(m.data);
            if( Vue.$config.ws.debug ) owner.$debug( '<- ' + JSON.stringify(msg) );
            switch( msg.result ){
                case "not_implemented":
                case "forbidden":
                case "session_invalid":
                    Vue.$send( "app", msg.result, msg );
                    break;
                default:
                    Vue.$send( msg.to, msg.result, msg );
            }
        }
		
		return newWS;
		
	}
	
	// 
    // Configure the Sockjs instance to talk to the specified
    // receiver. In ajax we are able to correlate each request
    // with a different owner state machine, however with websocket
    // all FSMs will share the same connection. We need to identify
    // a single owner in order to manage it.
    //
    //
    Vue.$wsInit = function( owner ) {
		if( !ws ) {
            if( Vue.$config.ws && Vue.$config.ws.url ) {
                var newWS = wscreate();
				ws = config_ws( newWS, owner );
			} else owner.$error( "Please set config.ws" );
        } else owner.$error( "ws already initialized" );
    };
    
	var wscreate = function() {
		return Vue.$config.ws.mock ? Vue.$config.ws.mock() : new SockJS( Vue.$config.ws.url );	
	}
	
	
    // 
    // Convenience wrapper over sockjs
    //
    Vue.$_ws = function( msg ) {
		if( !msg.session ) msg.session = {};
        var id = Vue.$get( Vue.$config.session_key );
        if( id ) msg.session.token = id ;
        msg.session["user-agent"] = navigator.userAgent;
        msg.session.app = Vue.$config.app_key;
        msg.session.lang = Vue.$lang();
        ws.send( JSON.stringify(msg) );
	}
	
   
    
    Vue.$session = function() {
        return Vue.$get( Vue.$config.session_key );
    }
    
	//
	// Access local storage
	//
	Vue.$get = function(k) {
		return $.totalStorage(k);	
	},
        
    Vue.$set = function(k, v) {
        $.totalStorage(k, v);    
    }
    
    Vue.$unset = function(k) {
        $.totalStorage.deleteItem(k);
    }
    
    //
    // Returns the browser language
    //
	Vue.$lang = function() {
        return ( navigator.language || navigator.userLanguage || navigator.browserLanguage || navigator.systemLanguage || "en-us" ).substring(0,2);
    }
    
    //
    // Basic i18n support
    //
    Vue.$i18n = function( key, args ) {
        if( !key || !key.length ) return "";
        var lang = Vue.$lang();
        var k = i18n[key];
        if( k === undefined ) return  '??' + key + '.' + lang + '??';
        var v = k[ lang ];
        if( v === undefined ) return '??' + key + '.' + lang + '??';
        if( !v.length ) return "";
        if( !args || !args.length ) return v;
        return v.$format( args  );
    }
    
    Vue.$fmt_date = function( d, pattern ) {
        return Vue.$i18n( pattern, [ 
            d.date(), 
            Vue.$i18n( "months" )[d.month()], 
            d.year(),
            Vue.$i18n( "weekdays" )[d.weekday()],
            Vue.$i18n( "weekdays_short" )[d.weekday()],
            d.month()+1,
            d.isoWeek(),
            d.format( "HH" ),
            d.format( "mm" )
        ])
    }
    
    Vue.$date = function(arg){
        var m = moment.utc(arg);
        m.locale( Vue.$lang() );
        return m;
    }

    Vue.$week = function() { 
      var m = moment.utc();
      m.locale( Vue.$lang() );
      m.isoWeekday(1);
      m.set({  hour: 12, minute: 0, second: 0, millisecond: 0 });
      return m;
    }
    
    Vue.filter( 'i18n', function ( key, arg1, arg2, arg3 ) {
        return Vue.$i18n( key, [ arg1, arg2, arg3 ] );
    });

    var $inspect = function(name) {
      var v = Vues[name];
      if( !v ) return "[" + name + "] [not found]"; 
      console.log( "[" + v._name + "] [" + v.current.name + "] " + JSON.stringify( v.$data, null, 2 ) );
    }
    
    Vue.$inspect = function(name ) {
      if( name ) return $inspect(name);
      Vues.$map( function(k, v) {
        $inspect( k );
      });
    },
        
    Vue.$subscriptions = function() {
        console.log( EventObs );
    }
    
})();
