(function() {

  var emptyCache = function() {
    me = null;
    profiles = {};
    photos = {};
    acls = null;
    users = [];
    indicators = [];
    organizations = {}; 
    templates = {};
    tags = {};
    stats = {};
  };

  var me = null;

  var getMyProfile = function( owner, ev, refresh ) {
    if( me && !refresh ) {
      owner.$receive( ev, me );
    } else {
      Vue.$http( owner, {
        path: "/api/my.profile.get",
        success: function( data ) {
          me=data;
          owner.$receive( ev, data );
        },
      });
    }
  } 

  var getMyPhoto = function(owner, ev) {
    getPhoto( owner, me.id, ev );
  }

  var stats = {};

  var getStats = function( owner, ev, refresh ) {
    if( me && ev && !refresh ) {
      owner.$receive( ev, stats );
    } else {
      Vue.$http( owner, {
        path: "/api/get.my.stats",
        success: function( data ) {
          stats=data;
          owner.$receive( ev, data );
        },
      });
    }
  }

  var profiles = {};

  var getProfile = function(owner, id, ev ) {
    if( !id ) return owner.$error(  "getProfile - no id specified" );
    if( profiles[id] ) return owner.$receive( ev, profiles[id] );
    Vue.$http( owner, {
      path: "/api/profile.get",
      data: { id: id },
      success: function( data ) {
        profiles[id]=data;
        owner.$receive( ev, data );
      },
      error: function( e ) {
        owner.$receive( "error", e );   
      }
    });
  }
  
  var clearProfile = function(id) {
    delete profiles[id];
  }


  var organizationProfiles = null;

  var getOrganizationProfiles = function(owner, id, ev) {
    if( !id ) return owner.$error(  "getOrganizationProfiles - no organization id specified" );
    if( ev && organizationProfiles ) return owner.$receive( ev, organizationProfiles );
    Vue.$http( owner, {
      path: "/api/get.schedule.profiles",
      data: { organization: id },
      success: function( data ) {
        organizationProfiles=data;
        if( ev ) owner.$receive( ev, data );
      },
      error: function( e ) {
        owner.$receive( "error", e );   
      }
    });
  }

  var getOrganizationProfile = function(id) {
    if( organizationProfiles ) {
      return organizationProfiles.filter( function(p) {
        return p.id === id;
      })[0];
    } else return null;
  }

  var clearOrganizationProfiles = function() {
    organizationProfiles = null;
  }

  var photos = {};

  var syncGetPhoto = function( id ) {
    return photos[id];
  }

  var getPhoto = function(owner, id, ev ) {
    if( !id ) return owner.$error(  "getPhoto - no id specified" );
    if( ev && photos[id]  ) return owner.$receive( ev, photos[id] );
    Vue.$http( owner, {
      path: "/api/avatar.get",
      data: { id: id },
      binary: true,
      success: function( data ) {
        photos[id]=data;
        if( ev ) owner.$receive( ev, data );
      }
    });
  }

  var clearPhoto = function(id) {
    delete photos[id];
  }

  var acls = null;

  var getAcls = function(owner, ev ) {
    if( acls ) return owner.$receive( ev, acls );
    Vue.$http( owner, {
      path: "/api/acls.get",
      success: function( data ) {
        acls = data;
        owner.$receive( ev, data );
      },
      error: function( e ) {
        owner.$receive( "error", e );
      }
    });
  }

  var getAcl = function(owner, name, ev) {
    if( acls ) {
      var acl = acls.filter( function(a){ 
        return name === a.name; 
      })[0];
      if( acl ) {
        owner.$receive( ev, acl );
      } else owner.$receive( "error", name );
    } else owner.$receive( "error", name );
  }


  var users = [];

  var getUsers = function( owner, id, ev, force ) {
    if( !force && users && users.length && ev ) { 
      owner.$receive( ev, users );
    } else {
      Vue.$http( owner, {
        path: "/api/get.organization.members",
        data: { id: id },
        success: function( data ) {
          users = data;
          users.map( function( u ) { getPhoto( owner, u.id ); });
          if( ev ) owner.$receive( ev, users );
        },
        error: function( e ) {
          owner.$receive( "error", e );
        }
      });
    }
  } 

  var syncGetUser = function(id) {
    var u = users.$findUsing( "id", id );  
    if( u ) return u;
    if( profiles && profiles[id] ) return profiles[id];
    if( me.id === id ) return me;
  }

  var indicators = [];

  var getIndicators = function( owner, id, ev, force) {
    if( !force && indicators.length && ev ) { 
      owner.$receive( ev, indicators );
    } else {
      Vue.$http( owner, {
        path: "/api/get.schedule.indicators",
        data: { organization: id },
        success: function( data ) {
          indicators = data;
          if( ev ) owner.$receive( ev, indicators);
        },
        error: function( e ) {
          owner.$receive( "error", e );
        }
      });
    }
  }

  var getIndicator = function( owner, id, ev ) {
    var found = indicators.filter( function(i) {
      return i.id === id;
    });

    if( !found.length ) { 
      owner.$receive( "error", "no such indicator: " + id );
    } else {
      owner.$receive( ev, found[0] );
    }
  }

  var syncGetIndicator = function(id) {
    var found = indicators.filter( function(i) {
      return i.id === id;
    });
    return found[0];
  }

  var organizations = {};

  var getOrganizations = function( owner, ev, force ) {
    if( organizations.length && ev && !force ) return owner.$receive( ev, organizations.$toArray() );
    Vue.$http( owner, {
      path: "/api/get.organizations",
      success: function( data ) {
        data.map( function(org) {
          organizations[org.id]=org
        });
        if( ev ) owner.$receive( ev, data );
      },
      error: function( e ) {
        owner.$receive( "error", e );
      }
    });
  }

  var getOrganization = function( owner, id, ev ) {
    var found = organizations[id];
    if( found ) return owner.$receive( ev, found );
    Vue.$http( owner, {
      path: "/api/get.organization",
      data: { id: id },
      success: function( data ) {
        organizations[id] = data;
        owner.$receive( ev, data );
      },
      error: function( e ) {
        owner.$receive( "error", e );
      }
    });
  }

  var updateOrganization = function( o ){
    organizations[o.id]=o;
  }

  var syncGetOrganization = function( id ) {
    return organizations[id];
  }


  var templates = {};

  var getTemplate = function( owner, o, t, ev, force ) {
    var key = t;
    if( !force && templates[t] && ev ) return owner.$receive( ev, templates[t] );
    Vue.$http( owner, {
      path: "/api/get.schedule.template",
      data: { organization: o, template: t },
      success: function( data ) {
        templates[key] = data;
        if( ev ) owner.$receive( ev, data );
      },
      error: function( e ) {
        owner.$receive( "error", e );
      }
    });
  }

  var updateTemplate = function(t) {
    templates[t.id] = t;
  }

  var tags = {};

  var getTags = function( owner, id, ev, force) {
    if( tags[id] && ev && !force ) {
      owner.$receive( ev, tags[id] );
    } else {
      Vue.$http( owner, {
        path: "/api/get.organization.tags",
        data: {id: id},
        success: function( data ){
          tags[id] = data;
          if( ev ) owner.$receive( ev, tags[id] );
        },

        error: function(e) {
          owner.$receive( 'error', e );
        }
      });
    }
  }

  var issuesUsers = null;

  var getIssuesUsers = function( owner, ev ) {
    if( issuesUsers && ev ) return owner.$receive( ev, issuesUsers );
    Vue.$http( owner, {
      path: "/api/get.issues.users",
      success: function( data ) {
        issuesUsers = data;
        if( ev ) owner.$receive( ev, data );
      },
    });
  }

  
  var userHtml = function( item ) {
    if( item )
      return ( "<div style='padding-top: 5px; padding-bottom: 5px;'><img class='push-left img-circle height-38' src='{3}'/>" + 
              "<div class='condensed-line' style='margin-left: 50px;'><strong>{0} {1}</strong><br/><span class='text-muted nowrap'><i class='fa fa-envelope'></i> {2}</span></div></div>" )
              .$format([ item.first, item.last, item.email, syncGetPhoto(item.id)]);
  };

  var userNameAndEmail = function( item ) {
    if( item )
      return ( "<div style='display: inline-block; margin-left: 5px; vertical-align: middle;'><strong>{0} {1}</strong><br/><span class='text-muted'><i class='fa fa-envelope'></i> {2}</span></div>" )
              .$format([ item.first, item.last, item.email ]);
  }


  Vue.filter( 'userHtml', function ( item ) {
    return userHtml( item ); 
  });

  Vue.filter( "weekday", function( dow ) {
    return i18n.weekdays[ Vue.$lang() ][dow];
  });

  Vue.filter( "shift-style", function( shift ) {
    var key = shift.type;
    if( shift.xtime === 'true' ) key = "xtime";
    if( shift.conflict === 'true' ) key = "conflict";
    return Vue.$config.calendar.styles[key];
  });

  var shiftTypeLabel = function( type ) {
    return "<div><span style='margin-right: 5px;' class='color-{0}'><i class='fa fa-circle'></i></span> {1}</div>"
      .$format([ Vue.$config.calendar.styles[type], Vue.$i18n( type ) ]);
  }

  var shiftUnassignedLabel = function() {
    return "<div><span style='margin-right: 5px;'><i class='fa fa-exclamation-triangle'></i></span> {0}</div>"
    .$format([ 
      Vue.$i18n( 'status_unassigned')
    ]);
  }

  Vue.filter( "shift-style-label", function( type ) {
    return shiftTypeLabel( type );
  });

  var constraintLabel = function( c ) {
    return "<div><span style='margin-right: 5px;' class='color-{0}'><i class='fa fa-circle'></i></span> {1}</div>"
      .$format([ Vue.$config.constraints.styles[c], Vue.$i18n( c + "_constraints" ) ]);
  }

  Vue.filter( "constraint-label", function( c) {
    return constraintLabel(c);
  });

  Vue.filter( "indicator-label", function(id) {
    var i = syncGetIndicator( id );
    return i ? Vue.$i18n( i.name ) : "";
  });

  Vue.filter( "constraint-severity", function(s) {
    if( !s ) return Vue.$i18n( "soft" );
    return Vue.$i18n( "strict" );
  });

  Vue.filter( "constraint-value-comparison", function(c){
    var ref, worst = "";
    switch( c.unit ) {
      case "duration":
        ref = formatDuration(c.ref); 
        worst = formatDuration(c.value);
        break;
      default:  
        ref = c.ref;
        worst = c.value;
        break;
    }

    return Vue.$i18n( "constraint_value_comparison" )
      .$format([ ref, worst]);
    
  });

  var shiftDate = function(shift) {
    var d = Vue.$date();
    d.set({ year: shift.year, month: shift.month-1, date: shift.day });
    return "<div><span style='margin-right: 5px;'><i class='fa fa-calendar'></i></span> {0}</div>"
      .$format([ Vue.$fmt_date( d, "date_fmt_day" ) ]);
  };


  var shiftTimes = function( shift ) {
    var start = Vue.$date();
    start.set({ hour: shift.start_hour, minutes: shift.start_min });
    var end = Vue.$date();
    end.set({ hour: shift.end_hour, minutes: shift.end_min });
    return "<div><span style='margin-right: 5px;'><i class='fa fa-clock-o'></i></span> {0} {1} {2} {3}</div>"
      .$format([
        Vue.$i18n( 'from' ),
        start.format( "HH.mm" ),
        Vue.$i18n( 'till' ),
        end.format( "HH.mm" )
      ]);
  }

  var shiftStaffing = function(shift) {
    if( !shift.staffing ) return "";
    return "<div><span style='margin-right: 5px;'><i class='fa fa-user'></i></span> {0} {1}</div>"
      .$format([
        Vue.$i18n( "staffing" ),
        shift.staffing
      ]);
  }

  var formatDuration = function(t) {
    if( !t || isNaN(t) ) return Vue.$i18n( "unavailable" );
    return ( t < 60 ) ? 
      "{0} {1}".$format([ t, Vue.$i18n( "minutes" )])
      :
      "{0} {1}".$format([ Math.trunc( t*10/60 )/10, Vue.$i18n( "hours" )]);
  }


  Vue.filter( "time", function(t) {
    return formatDuration(t);
  });



  Vue.filter( "constraint-severity-badge", function(v) {
    var strong = v === "true";
    var style = strong ? "warning" : "info";
    var label = Vue.$i18n( strong ? "strict" : "soft" );
    return '<span class="label tag label-{0}">{1}</span>'
      .$format([ style, label]);
  });

  Vue.filter( "constraint-type-badge", function(v) {
    return '<span class="label tag label-info">{0}</span>'
      .$format([ Vue.$i18n( "rule_type_" + v )]);
  });

  Vue.filter( "quality-badge", function(q) {
    if( !q ) return "";

    if( q == 100 ) {
      s = "primary";
    } else if( q >= 50 ) {
      s = "info";
    } else if ( q == 0 ) {
      s = "danger"; 
    } else s = "warning";

    return '<span class="label tag label-{0}" style="margin-right: 10px !important;">{1}%</span>'
      .$format([s, q]);
  });


  var profileBadge = function(p) {
    var profile = getOrganizationProfile(p);
    if( profile ) {
      return '<span class="label tag label-info">{0}</span>'
      .$format([ profile.name]);
    }   
  }

  Vue.filter( "profile-badge", function(p){
    return profileBadge(p);
  });

  var statusBadge = function(s) {
    return '<span class="label tag label-{1}">{0}</span>'
      .$format([ Vue.$i18n( "status_" + s), Vue.$config.schedules.status[s] ]);
  }

  Vue.filter( "status-badge", function(s) {
    return statusBadge(s);
  });


  var severityBadge = function(s) {
    return "<span style='margin-right: 5px;' class='color-{0}'><i class='fa fa-circle'></i></span>"
      .$format([ Vue.$config.severity.styles[s]]);
  }

  Vue.filter( "severity-indicator", function(s){
    return severityBadge(s);
  });
  
  Vue.filter( 'epoch_to_date', function ( v ) {
    return moment.unix(v).format( "DD.MM.YYYY" );
  });

  Vue.filter( 'epoch_to_date_time', function ( v ) {
    return moment.unix(v).format( "DD.MM.YYYY HH:mm" );
  });

  Vue.filter( "hm_time", function(t) {
    return t.format( "HH.mm" );    
  });

  Vue.filter( "weekday", function(d) {
    return Vue.$i18n( "weekdays" )[d];    
  });

  Vue.filter( "hour", function(h) {
    return h<10 ? "0"+h : h;    
  });

  Vue.filter( "minutes", function(m) {
    return m<10 ? "0"+m : m;     
  });

  Vue.$methods({

    clearTimeout: function() {
      this.$clearTimeout();
    },

    printData: function(){
      console.log( "[" + this._name +"] [DATA]", this.$data );
    },

    printMsg: function(msg) {
      console.log( "[" + this._name +"] [MSG]", msg );    
    },

    doughnut: function(data ) {
      var target = this._name + "_chart";
      var $el = document.getElementById( target );
      if( !$el ) return;

      var opts = {
        segmentShowStroke: true,
        segmentStrokeColor: "#fff",
        segmentStrokeWidth: 2,
        percentageInnerCutout: 45, // This is 0 for Pie charts
        animationSteps: 100,
        animationEasing: "easeOutBounce",
        animateRotate: true,
        animateScale: false,
        responsive: true,
      };

      var ctx = $el.getContext("2d");
      return new Chart(ctx).Doughnut(data, opts);
    },

    tasksByDurationChartData: function(stats) {
      return [
        {
          value: Number( stats.tasks_daily_count || 0 ),
          color: "#a3e1d4",
          highlight: "#1ab394",
          label: Vue.$i18n( "daily" )
        },
        {
          value: Number( stats.tasks_weekly_count || 0 ),
          color: "#FFD6AA",
          highlight: "#f8ac59",
          label: Vue.$i18n( "weekly" )
        },
        {
          value: Number( stats.tasks_monthly_count || 0 ),
          color: "#FBA7AF",
          highlight: "#ed5565",
          label: Vue.$i18n( "monthly" )
        },
        {
          value: Number( stats.tasks_longterm_count || 0 ),
          color: "#dedede",
          highlight: "#676a6c",
          label: Vue.$i18n( "longterm" )
        },
      ];   

    },


    tasksByStatusChartData: function(stats) {
      return [{
        value: Number( stats.tasks_complete || 0 ),
        color: "#a3e1d4",
        highlight: "#1ab394",
        label: Vue.$i18n( "complete" )
      },
      {
        value: Number( stats.tasks_active || 0 ),
        color: "#FFD6AA",
        highlight: "#f8ac59",
        label: Vue.$i18n( "active" )
      },
      {
        value: Number( stats.tasks_cancelled || 0 ),
        color: "#dedede",
        highlight: "#888888",
        label: Vue.$i18n( "cancelled" )
      }
      ];    
    }


  });



  Vue.$comp( "empty", {
    many: true
  });

  Vue.$comp( "usermessages", {
    many: true, 

    data: {
      messages: []
    },

    ui: {
      severity: { SUCCESS: "success", INFO: "info", WARN: "warning", ERROR: "danger" }        
    },

    computed: {
      visible: function() {
        return this.messages.length >0 ? "visible" : "hidden";
      }
    },

    states: {
      SUCCESS: {
        success: { then: "SUCCESS", and: "add", timeout: 5 },
        info: { then: "INFO", and: [ "clear", "add" ], timeout: 5 },
        warn: { then: "WARN", and: [ "clear", "add" ], timeout: 5 },
        error: { then: "ERROR", and: [ "clear", "add" ], timeout: 5 },
        clear: { then: "SUCCESS", and: "clear" },
        timeout: { then: "SUCCESS", and: "clear" }

      },

      INFO: {
        success: { then: "SUCCESS", and: [ "clear", "add" ], timeout: 5 },
        info: { then: "INFO", and: "add" , timeout: 5 },
        warn: { then: "WARN", and: [ "clear", "add" ], timeout: 5 },
        error: { then: "ERROR", and: [ "clear", "add" ], timeout: 5 },
        clear: { then: "INFO", and: "clear" },
        timeout: { then: "INFO", and: "clear" }
      },

      WARN: {
        success: { then: "SUCCESS", and: [ "clear", "add" ], timeout: 5 },
        info: { then: "INFO", and: [ "clear", "add" ], timeout: 5 },
        warn: { then: "WARN", and: "add", timeout: 5 },
        error: { then: "ERROR", and: [ "clear", "add" ], timeout: 5},
        clear: { then: "WARN", and: "clear" },
        timeout: { then: "WARN", and: "clear" }
      },

      ERROR: {
        success: { then: "SUCCESS", and: [ "clear", "add" ], timeout: 5 },
        info: { then: "INFO", and: [ "clear", "add" ], timeout: 5 },
        warn: { then: "WARN", and: [ "clear", "add" ] , timeout: 5},
        error: { then: "ERROR", and: "add", timeout: 5 },
        clear: { then: "ERROR", and: "clear" },
        timeout: { then: "ERROR", and: "clear" }
      },
    },

    methods: {
      add: function(msg) {
        this.$data.messages.push(
          { key: (msg.status || msg), args: (msg.reason || msg)  }
        );
      },

      clear: function(){
        this.$data.messages = [];
      }
    },

    listen: {
      states: {
        $parent: { BUSY: "clear" }        
      },

      events: {
        $parent: { error: "error", success: "success", info: "info", warn: "warn", clear: "clear" }
      }
    }
  });




  Vue.$comp( "sign_up", {
    data: {
      first: "",
      last: "",
      email: "",
    },

    ui: {
      sign_up_label: { BUSY: 'sending', $DEFAULT: 'sign_up' },
      form: { $DEFAULT: 'visible', SUCCESS: 'hidden' },
      success: { $DEFAULT: 'hidden', SUCCESS: 'visible' }
    },

    states: {
      READY: {
        clear: { then: "READY", and: [ {unset: ["first", "last", "email"]}, { pub: "clear"} ]},
        sign_in: { then: "READY", and: { send: "sign_in", to: "$parent" } },
        back: { then: "READY", and: { send: "home", to: "$parent" } },
        submit: { then: "BUSY", and: { 
          http: "signup", method: "post",  body: { first: "first", last: "last", email: "email" } 
        }, timeout: 10 }
      },

      BUSY: {
        error: { then: "READY", and: [ "clearTimeout", { send: "error", to: "$parent" } ] },
        data: { then: "SUCCESS", and: "clearTimeout" },
        conflict: { then: "READY", and: [ "clearTimeout", { pub: "warn", msg: "account_exists" }] },
        invalid: { then: "READY", and: [ "clearTimeout", { pub: "warn" } ]},
        timeout: { then: "READY", and: [ "clearTimeout", { pub: "warn", msg: "timeout" } ]}
      },

      SUCCESS: {}
    },

    listen: {

      states: {
        app: { SIGN_UP: "clear" }
      }
    }
  });

  Vue.$comp( "sign_in", {
    data: {
      email: "",
      password: ""
    },

    ui: {
      sign_in_label: { BUSY: 'sending', $DEFAULT: 'sign_in' },
      form: { $DEFAULT: 'visible', SUCCESS: 'hidden' },
      success: { $DEFAULT: 'hidden', SUCCESS: 'visible' },
      password: { BUSY: "" },
    },

    states: {
      READY: {
        clear: { then: "READY", and: [ {unset: ["email", "password"]}, { pub: "clear" }] },
        sign_up: { then: "READY", and: { send: "sign_up", to: "$parent" }},
        forgot_password: { then: "READY", and: { send: "forgot_password", to: "$parent" }},
        back: { then: "READY", and: { send: "home", to: "$parent" } },
        submit: { then: "BUSY", and: { 
          post: "signin", body: { email: "email", password: "password" }} 
        }
      },

      BUSY: {
        error: { then: "READY", and: { send: "error", to: "$parent" } },
        data: { then: "SUCCESS", and: "setSession", timeout: 1 },
        not_found: { then: "READY", and:{ pub: "warn", msg: "invalid_credentials" } },
        invalid_credentials: {then: "READY", and: { pub: "warn" } },
        needs_password_reset: {then: "READY", and: { pub: "warn" } },
        invalid: { then: "READY", and: { pub: "warn" } },
        timeout: { then: "READY", and: { pub: "warn", msg: "timeout" }}
      },

      SUCCESS: {
        timeout: { then: "READY" }
      }
    },

    methods: {

      setSession: function(msg) {
        Vue.$set( Vue.$config.session_key, msg.session );
      }
    },

    listen: {

      states: {
        app: { SIGN_IN: "clear" }
      }
    },
  });


  Vue.$comp( "forgot_password", {
    data: {
      email: "",
    },

    ui: {
      submit_label: { BUSY: 'sending', $DEFAULT: 'send_password' },
      form: { $DEFAULT: 'visible', SUCCESS: 'hidden' },
      success: { $DEFAULT: 'hidden', SUCCESS: 'visible' }
    },

    states: {
      READY: {
        clear: { then: "READY", and: [ {unset: [ "email" ]} , { pub: "clear" }] },
        back: { then: "READY", and: { send: "sign_in", to: "$parent" } },
        submit: { then: "BUSY", and: { 
          http: "passwd.forgot", method: "post", body: { email: "email" } 
        }, timeout: 10 }
      },

      BUSY: {
        error: { then: "READY", and: [ "clearTimeout", { send: "error", to: "$parent" } ] },
        data: { then: "SUCCESS", and: "clearTimeout" },
        invalid: { then: "READY", and: [ "clearTimeout", { pub: "warn" }]},
        timeout: { then: "READY", and: [ "clearTimeout", { pub: "warn", msg: "timeout" } ]},
        not_found: { then: "READY", and: [ "clearTimeout", { pub: "warn", msg: "no_such_account" } ]} 
      },

      SUCCESS: {}
    },

    listen: {

      states: {
        app: { FORGOT_PASSWORD: "clear" }
      }
    }

  });

  Vue.$comp( "reset_password", {
    data: {
      email: "",
      password: "",
      password_confirm: "",
      token: ""
    },

    ui: {
      submit_label: { BUSY: 'sending', $DEFAULT: 'password_reset' },
      form: { $DEFAULT: 'visible', SUCCESS: 'hidden' },
      success: { $DEFAULT: 'hidden', SUCCESS: 'visible' },
      password: { BUSY: "" },
      password_confirm: { BUSY: "" },
    },

    states: {
      READY: {
        back: { then: "READY", and: { send: "home", to: "$parent" } },
        submit: { then: "BUSY", and: { http: "passwd.reset", method: "post",
          body: { email: "email", password: "password", password_confirm: "password_confirm", token: "token" } }, timeout: 10 },
          token: { then: "READY", and: "setToken" }
      },

      BUSY: {
        error: { then: "READY", and: [ "clearTimeout", { send: "error", to: "$parent" } ] },
        data: { then: "SUCCESS", and: "clearTimeout" },
        invalid: { then: "READY", and:  [ "clearTimeout", { pub: "warn" } ]},
        timeout: { then: "READY", and:  [ "clearTimeout", { pub: "warn", msg: "timeout" } ]},
        //invalid_token: { then: "READY", and:  [ "clearTimeout", { pub: "warn" } ]},
        not_found: { then: "READY", and: [ "clearTimeout", {pub: "warn", source: "msg", data: "reason" }]}
      },

      SUCCESS: {}
    },

    listen: {
      events: {
        app:{ token: "token" } 
      }
    },

    methods: {

      setToken: function(arg){
        this.$data.token = arg;    
      }
    }


  });




  Vue.$comp( "connecting" );


  Vue.$comp( "error", {
    data: { status: "", reason: "" },
    states: {
      DEFAULT: {
        display: { then: "DEFAULT", and: "display" }       
      }        
    },

    listen: {
      events: {
        $parent: { error: "display" }
      }
    },

    methods: {
      display: function(msg){
        this.$data.status = msg.status;
        this.$data.reason = msg.reason;
      }
    }
  });


  Vue.$comp( "sign-out", {
    states: {
      READY: { 
        sign_out: { then: "SIGNING_OUT", and: { http: "signout", method: "post"}, timeout: 10 }
      },

      SIGNING_OUT: {
        data: { then: "SIGNED_OUT", and: [ "clearTimeout", "emptyCache", { broadcast: "$reset" } ], timeout: 1 },
        timeout: { then: "READY" }
      },

      SIGNED_OUT: {
        timeout: { then: "READY" }
      }
    },

    methods: {
      emptyCache: function() {
        emptyCache();
      }
    }
  })

  Vue.$comp( "profile-shifts", {
    data: { profile: {}, year: 0, month: 0, day: 0, week: 0  },
    states: {
      INIT: {
        profile: { then: "INIT", and: ["clearCalendar", {set: { profile: "profile" }}]},
        init: { then: "INIT", and: [ "clearCalendar", "setToday" ] },
        period: { then: "WEEK", and: [ "clearCalendar", "setPeriod", "fetch" ]},
      },

      DAY: {
        profile: { then: "DAY", and: [ "clearCalendar", { set: { profile: "profile" }}, "init" ]},
        init: { then: "DAY", and: [ "clearCalendar", "refresh" ] },
        fetch: { then: "DAY", and: { http: "get.daily.shifts", body: { id: "profile.id", year: "year", month: "month", day: "day" }} },
        data: { then: "DAY", and: "pubShifts" },
        week: { then: "WEEK" },
        month: { then: "MONTH" },
        period: { then: "DAY", and: [ "setPeriod", "clearCalendar", "fetch" ] },
        refresh: { then: "DAY", and: "fetch" }
      },

      WEEK: {
        profile: { then: "WEEK", and: [ "clearCalendar", { set: { profile: "profile" }}, "init" ]},
        init: { then: "WEEK", and: [ "clearCalendar", "refresh" ] },
        day: { then: "DAY", and: ["clearCalendar", "fetch"] },
        month: { then: "MONTH" },
        fetch: { then: "WEEK", and: { http: "get.weekly.shifts", body: { id: "profile.id", year: "year", week: "week" }} },
        data: { then: "WEEK", and: "pubShifts" },
        period: { then: "WEEK", and: [ "setPeriod", "clearCalendar", "fetch" ] },
        refresh: { then: "WEEK", and: "fetch" }
      },

      MONTH: {
        profile: { then: "MONTH", and: [ "clearCalendar", { set: { profile: "profile" }}, "init" ]},
        init: { then: "MONTH", and: [ "clearCalendar", "refresh"]  },
        period: { then: "MONTH", and: [ "setPeriod", "clearCalendar", "fetch" ] },
        fetch: { then: "MONTH", and: "fetchMonthlyShifts" },
        data: { then: "MONTH", and: "pubShifts" },
        day: { then: "DAY" },
        week: { then: "WEEK" },
        refresh: { then: "MONTH", and: "fetch" }
      }
    },

    listen: {
      states: {
        $parent: { SHIFTS: "init" },
      },

      events: {
        $parent: { profile: "profile" }
      }
    }, 

    methods: {

      setToday: function() {
        var p = Vue.$date();
        this.$data.year = p.year();
        this.$data.month = p.month() + 1;
        this.$data.day = p.date();
        this.$data.week = p.isoWeek();
      },
     
      setPeriod: function( str ) {
        var p = Vue.$date( str );
        this.$data.year = p.year();
        this.$data.month = p.month() + 1;
        this.$data.day = p.date();
        this.$data.week = p.isoWeek();
      },

      pubShifts: function(items) {
        var self = this;
        items.map( function(i) { self.$pub( "item", i ); });
      },

      clearCalendar: function() {
        this.$pub( "clear", false );
      },

      fetchMonthlyShifts: function() {
        this.$http({ path: "/api/get.monthly.shifts", data: { 
          id: this.$data.profile.id, 
          year: this.$data.year, 
          month: this.$data.month 
        }});
        this.$http({ path: "/api/get.monthly.shifts", data: { 
          id: this.$data.profile.id,
          year: (this.$data.month == 1) ? this.$data.year -1 : this.$data.year, 
          month: (this.$data.month == 1 ) ? 12 : this.$data.month -1 
        }});
        this.$http({ path: "/api/get.monthly.shifts", data: { 
          id: this.$data.profile.id,
          year: (this.$data.month == 12) ? this.$data.year +1 : this.$data.year, 
          month: (this.$data.month == 12 ) ? 1 : this.$data.month +1 
        }});
      },


    }

  });


  /* Vue.$comp( "account", {
    data: { 
      user: {}, 
      photo_data: "", 
      stats: {} 
    },

    states: {
      READY: {
        user: { then: "READY", and: [ { set: "user" }, { pub: "user" }, { pub: "id" }, "stats" ] },
        stats: { then: "STATS", and: { http: "stats.get", body: {id: "user.id" }} },
        show_profile: { then: "READY", and: [{ send: "profile", to: "main", args: { profile: "user", user: "user" } }] },
        current_user: { then: "READY", and: { reply: "user", body: "user" } },
        current_stats: { then: "READY", and: {reply: "stats", body: "stats"} },
        changed: { then: "READY", and: [ { set: "user" }, "clear", { pub: "user" }, { pub: "id" }, "show_profile" ]}
      },

      STATS: {
        data: { then: "READY", and: [ { set: "stats" }, {pub: "stats"}] },
        not_found: { then: "READY" },
        error: { then: "READY" },
        user: { then: "READY", and: "user" }
      }
    },

    listen: {
      events: {
        app: { user: "user" },
        profile: { changed: "changed" },
        tasks: { changed: "stats" },
      }
    },

    methods: {

      $reset: function(){
        this.$data.user = {};
        this.$data.photo_data = "";
        this.$data.stats = {}
      },

      clear: function() {
        clearProfile( this.$data.user.id );
        clearPhoto( this.$data.user.id );
      }
    }
  });*/


  Vue.$comp( "profile-photo", {
    data: { user: {}, photo_data: "", photo: {}, profile: {} },

    computed: {
      readonly: function() {
        return this.user == null || this.profile == null || this.profile.id != this.user.id;
      }    
    },

    states: {
      INIT: { 
        profile: { then: "INIT", and: [ { set: { profile: "profile", user: "user" } }, "photo" ]},
        photo: { then: "FETCHING", and: "getPhoto" }
      },

      FETCHING: {
        data: { then: "READY", and: [ {set: "photo_data"}, "configure" ] },
        error: { then: "READY", and: "configure" }
      },

      READY: {
        profile: { then: "INIT", and: "profile" },
        uploading: { then: "UPLOADING" } 
      },

      UPLOADING: {
        data: { then: "INIT", and: [ "resetCache", "photo", { pub: "changed" } ]  },
        invalid: { then: "READY", and: { pub: "warn" } },
        error: { then: "READY", and: { pub: "error" } }
      }
    },

    listen: {
      events: {
        $parent: { profile: "profile" }
      }
    },

    methods: {

      resetCache: function() {
        clearPhoto( this.$data.profile.id );  
      },

      getPhoto: function(){
        getPhoto( this, this.$data.profile.id, "data" );   
      },

      configure: function(){
        this.$upload({ path: "/api/my.avatar.set", el: "#file" });  
      },
    }

  });


  Vue.$comp( "profile-info", {
    data: { user: {}, profile: {} },

    computed: {
      readonly: function() {
        return this.user == null || this.profile == null || this.profile.id != this.user.id;
      }   
    },

    ui: {
      change_name: { $DEFAULT: "update", BUSY: "sending" }
    },

    states: {
      READY: { 
        profile: { then: "READY", and: [{ set: { profile: "profile", user: "user" }}, "renderSwitches", { pub: "profile" } ]},
        name: { then: "CHANGING_NAME", and: { http: "profile.set", method: "post", body: { first: "profile.first", last: "profile.last" }} },
        changed: { then: "READY", and: [ { set: "profile"}, { pub: "changed" } ] },
        toggle_issue_emails: { then: "TOGGLING_NOTIFICATIONS", and: { post: "toggle.issue.emails" } }
      },

      CHANGING_NAME: {
        data: { then: "READY", and: [ {set: "profile"}, "changed" ]},
        error: { then: "READY", and: { pub: "error" }},
        invalid: { then: "READY", and: { pub: "warn" }}
      },

      TOGGLING_NOTIFICATIONS: {
        data: { then: "READY", and: [ {set: "profile"}, "renderSwitches" ]},
        error: { then: "READY", and: { pub: "error" }},
      }
    },

    listen: {
      events: {
        $parent: { user: "user", profile: "profile" },
        "profile-photo": { changed: "changed" }
      }
    },

    methods: {
      renderSwitches: function() {
        if( this.$data.profile.issue_emails === "true" ) 
          $( '#issue-email-check' ).attr( "checked", "true" );
        else $( '#issue-email-check' ).removeAttr( "checked" );
      }
    }
  });

  Vue.$comp( "week-stats", {
    many: true,
    props: [ "height", "width", "stats" ],
    computed: {
      title: function() {
        return this.weekTitle( this.stats );
      }
    },
    states: {
      READY: {
        init: { then: "READY" }
      }
    },

    ready: function() {
      this.renderBarChart();
    },

    methods: {
      weekTitle: function( stats ) {
          var m = moment();
          m.set( { year: parseInt( stats.year ), isoWeek: parseInt( stats.week )} );
          return Vue.$fmt_date( m, "date_fmt_woy" );
      },

      renderBarChart: function() {
        var $el = $( this.$el ).find( " > canvas" );
        if( $el ) { 
          
          var ctx = $el[0].getContext("2d");

          var barData = {
            labels: [ this.weekTitle( this.$data.stats ) ],
            datasets: [
              {
                label: Vue.$i18n( "std" ),
                fillColor: "rgba(217,237,247,0.5)",
                strokeColor: "rgba(188,233,241,0.8)",
                highlightFill: "rgba(217,237,247,0.75)",
                highlightStroke: "rgba(188,233,241,1)",
                data: [Math.round(10* parseInt( this.$data.stats.total_std_time_w)/60)/10]
              },
                
              {
                label: Vue.$i18n( "training" ),
                fillColor: "rgba(252,248,227,0.5)",
                strokeColor: "rgba(250,235,204,0.8)",
                highlightFill: "rgba(252,248,227,0.75)",
                highlightStroke: "rgba(250,235,204,1)",
                data: [ Math.round(10* parseInt( this.$data.stats.total_training_time_w)/60)/10 ]
              },

              {
                label: Vue.$i18n( "idle" ),
                fillColor: "rgba(223,240,216,0.5)",
                strokeColor: "rgba(214,233,198,0.8)",
                highlightFill: "rgba(223,240,216,0.75)",
                highlightStroke: "rgba(214,233,198,1)",
                data: [ Math.round(10* parseInt( this.$data.stats.total_idle_time_w)/60)/10 ]
              },

              {
                label: Vue.$i18n( "extra" ),
                fillColor: "rgba(242,222,222,0.5)",
                strokeColor: "rgba(235,204,209,0.8)",
                highlightFill: "rgba(242,222,222,0.75)",
                highlightStroke: "rgba(235,204,209,1)",
                data: [ Math.round(10* parseInt( this.$data.stats.total_extra_time_w)/60)/10 ]
              }
          ]};

          var barOptions = {
            scaleBeginAtZero: true,
            scaleShowGridLines: true,
            scaleGridLineColor: "rgba(0,0,0,.05)",
            scaleGridLineWidth: 1,
            barShowStroke: true,
            barStrokeWidth: 1,
            barValueSpacing: 6,
            barDatasetSpacing: 1,
            responsive: true,
            scaleShowHorizontalLines: true,
            scaleShowVerticalLines: false,
            legendTemplate: "",
            customTooltips: function(t) {
              return;
            }
          }

         new Chart(ctx).Bar(barData, barOptions);



        }
      }
    },

    hooks: {
      $init: "init"
    }

  });

  Vue.$comp( "profile-weekly-stats", {
    data: { weeks: [], profile: {}, types: [ "std", "training", "idle", "extra" ] },
    ui: {
      toggle: { PUBLISHED: "published_time", SCHEDULED: "scheduled_time" },
      icon: { PUBLISHED: "fa-thumbs-up", SCHEDULED: "fa-flask" }
    },

    states: {
      INIT: {
        profile: { then: "PUBLISHED", and: { set: { profile: "profile" } }}
      },

      PUBLISHED: {
        fetch: { then: "PUBLISHED", and: { http: "get.all.weekly.stats", body: { type: { value: "published"}, profile: "profile.id" }}},
        data: { then: "PUBLISHED", and: { set: "weeks" }},
        profile: { then: "PUBLISHED", and: [{ set: { profile: "profile" }}, "fetch" ]},
        toggle: { then: "SCHEDULED", and: "fetch" }
      },
      
      SCHEDULED: {
        fetch: { then: "SCHEDULED", and: { http: "get.all.weekly.stats", body: { type: { value: "scheduled" }, profile: "profile.id" }}},
        data: { then: "SCHEDULED", and: { set: "weeks" }},
        profile: { then: "SCHEDULED", and: [{ set: { profile: "profile" }}, "fetch" ]},
        toggle: { then: "PUBLISHED", and: "fetch" }
      }

    },

    listen: {
      states: {
        $parent: { STATS: "fetch" }
      },

      events: {
        $parent: { profile: "profile" }
      }
    }
    

  });
  
  Vue.$comp( "availability-template-list-item", {
    many: true,
    props: [ "item" ],
    states: {
      UNSELECTED: {
        select: { then: "SELECTED", and: { send: "selected", to: "$parent", data: "item" }},
        selected: [
          { if: { eq: "id", source: "msg", data: "item.id" }, then: "SELECTED" },
          { then: "UNSELECTED" }
        ]
      },

      SELECTED: {
        selected: [
          { if: { eq: "id", source: "msg", data: "item.id" }, then: "SELECTED" },
          { then: "UNSELECTED" }
        ]
      }
    },

    listen: {
      events: {
        $parent: { selected: "selected" }
      }
    }
  });


  Vue.$comp( "profile-availability-save-to-template", {
    many: true,
    props: [ "availability" ],
    data: { name: "" },
    ui: {
      submit: { $DEFAULT: "save_availability_changes", SENDING: "sending" }
    },

    states: {
      READY: {
        submit: { then: "SENDING", and: { post: "save.availability", body: { id: "availability.id", name: "name" }}}
      },

      SENDING: {
        data: { then: "READY", and: [ { unset: [ "name" ] }, {send: "updated", to: "$parent" }]},
        conflict: { then: "READY", and: { pub: "warn", msg: "template_name_conflict" }}
      }
    },

  });

  Vue.$comp( "profile-availability-control", {
    many: true,
    props: [ "access", "organization", "year", "week" ],
    data: { template: null, templates: [], availability: {} },
    ui: {
      apply: { ENABLED: "apply_template", APPLYING_TEMPLATE: "sending" }
    },
    computed: {
      can_apply: function() {
        return this.template;
      },

      has_templates: function() {
        return this.templates.length;
      },

      is_dirty: function() {
        return this.availability.dirty === "true";
      },

      readonly: function() {
        return this.access === "read";
      },

      has_template: function() {

        return this.template && this.template != "undefined";
      },

      selectedTemplate: function() {
        if( this.templates.length && this.template && this.template != "undefined") { 
          var t = this.templates.$findUsing( "id", this.template );
          if( t ) return t.name;
        } 
        
        return null;
      }

    },
    states: {
      INIT: { 
        init: { then: "DISABLED" },
        availability: { then: "INIT", and: [ { set: "availability" }, "route" ]},
        route: [
          { if: "isEnabled", then: "ENABLED", and: [ { send: "read", to: "$parent" }, "fetch_templates" ]},
          { if: "isEdit", then: "EDIT", and: [{ send: "edit", to: "$parent"}, "fetch_templates" ]},
          { then: "DISABLED", and: { send: "read", to: "$parent"} }
        ],
        clear: { then: "DISABLED", and: [ {pub: "clear"}, "$reset" ]}
      },
      
      DISABLED: { 
        availability: { then: "INIT", and: "availability" },
        enable: [
          { if: { has: [ "availability" ] }, then: "CREATING", and: { post: "enable.availability", body: { id: "availability.id" }}}, 
          { then: "CREATING", and: { post: "create.availability", body: { organization: "organization.id", year: "year", week: "week" } }}
        ],
        clear: { then: "INIT", and: "clear" },
      },

      CREATING: {
        data: { then: "INIT", and: "availability" }
      },

      FETCHING_TEMPLATES: {
        data: { then: "ENABLED", and: [ {set: "templates"}, { pub: "templates" }, "updateSelectedTemplate", "fetch_shifts" ]},
      },

      ENABLED: { 
        availability: { then: "INIT", and: "availability" },
        disable: { then: "APPLYING", and: [ {unset: [ "templates"]}, { post: "disable.availability", body: { id: "availability.id" }}]},
        edit: { then: "EDIT", and: { post: "edit.availability", body: { id: "availability.id" }}},
        template_selected: { then: "ENABLED", and: [{set: "template"}, "apply" ]},
        apply: { then: "APPLYING", and: { post: "set.availability.template", body: { id: "availability.id", template: "template" }}},
        fetch_shifts: { then: "FETCHING_SHIFTS", and: { http: "get.availability.shifts", body: { id: "availability.id" }}},
        fetch_templates: { then: "FETCHING_TEMPLATES", and: { http: "get.availability.templates", body: { organization: "organization.id" }}},
        clear: { then: "INIT", and: "clear" },
      },

      FETCHING_SHIFTS: {
        data: [
          { if: "isEnabled", then: "ENABLED", and: { send: "shifts", to: "$parent" }},
          { if: "isEdit", then: "EDIT", and: { send: "shifts", to: "$parent" }}
        ]
      },

      FETCHING: {
        data: [
          { if: "isEnabled", then: "ENABLED", and: { set: "availability" }},
          { if: "isEdit", then: "EDIT", and: { set: "availability" }}
        ]
      },

      APPLYING: {
        data: { then: "APPLYING", and: [ {set:"availability"}, { send: "updated", to: "$parent"}, {send: "read", to: "$parent"},  "route" ]},
        route: [
          { if: "isEnabled", then: "ENABLED", and:"fetch_shifts" },
          { then: "DISABLED", and: { send: "disabled", to: "$parent" }}
        ]
      },

      EDIT: { 
        availability: { then: "INIT", and: "availability" },
        fetch_shifts: { then: "FETCHING_SHIFTS", and: { http: "get.availability.shifts", body: { id: "availability.id" }}},
        fetch_templates: { then: "FETCHING_TEMPLATES", and: { http: "get.availability.templates", body: { organization: "organization.id" }}},
        data: { then: "EDIT", and: [ {set: "availability"}, { send: "updated", to: "$parent"}, { send: "edit", to: "$parent" }, "fetch_shifts" ]},
        clear: { then: "INIT", and: "clear" },
        cancel: { then: "DISCARDING_CHANGES", and: { post: "discard.availability.changes", body: { id: "availability.id" } }},
        refresh: { then: "FETCHING", and: { http: "get.availability", body: { id: "availability.id" }}},
        updated: { then: "ENABLED", and: [ { set: "availability" }, { send: "read", to: "$parent"}, "fetch_templates" ]},
        conflict: { then: "EDIT", and: [ {pub: "warn", msg: "shift_conflict"}, { send: "edit", to: "$parent"}, "fetch_shifts" ] },
      },

      DISCARDING_CHANGES: {
        data: { then: "ENABLED", and: [{set: "availability"}, {send: "updated", to: "$parent"}, { send: "read", to: "$parent"}, "fetch_shifts"]}
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      events: {
        $parent: { 
          availability: "availability", 
          shifts: "fetch_shifts", 
          clear: "clear",
          refresh: "refresh",
        }
      }
    },

    methods: {
      
      isEnabled: function() {
        return this.$data.availability.status == "enabled";
      },

      isEdit: function() {
        return this.$data.availability.status == "edit";
      },

      updateSelectedTemplate: function() {
        this.$data.template = this.$data.availability.template;
        this.$pub( "selected", this.$data.template );
      },

      $reset: function() {
        this.$data.templates = [];
        this.$data.template = null;
        this.$data.availability = {};
      }
    }
           
  });

  Vue.$comp( "profile-availability-organization", {
    many: "true",
    props: [ "access", "organization", "year", "week" ],
    data: { availability: {} },
    computed: {
      is_dirty: function() {
        return this.availability.dirty;
      },

      readonly: function() {
        return this.access = "read";
      }
    },
    states: {

      UNSELECTED: {
        select: { then: "SELECTED", and: { send: "organization_selected", to: "$parent", data: { id: "organization.id" }}},
        set: { if: "sameOrganization", then: "UNSELECTED", and: [{ set: "availability" }]},
        clear: { then: "UNSELECTED", and: [{ pub: "clear" }, "$reset" ]},
        selected: [
          { if: { eq: "id", source: "msg", data: "organization.id" }, then: "SELECTED", and: [{ send: "updated", to: "$parent", data: "availability" }, { pub: "availability", data: "availability" }]},
          { then: "UNSELECTED" }
        ],

      },

      SELECTED: {
        unselect: { then: "UNSELECTED" },
        set: { if: "sameOrganization", then: "SELECTED", and: [{ set: "availability" }]},
        selected: [
          { if: { eq: "id", source: "msg", data: "organization.id" }, then: "SELECTED", and: [ { send: "updated", to: "$parent", data: "availability" }, { pub: "availability", data: "availability" }]},
          { then: "UNSELECTED" }
        ],
        shifts: { then: "SELECTED", and: { send: "shifts", to: "$parent" }},
        disabled: { then: "SELECTED", and: { send: "disabled", to: "$parent" }},
        read: { then: "SELECTED", and: { send: "read", to: "$parent" }},
        clear: { then: "SELECTED", and: [ {pub: "clear"}, "$reset" ]},
        edit: [
          { if: { eq: "access", value: "owner" }, then: "SELECTED", and: { send: "edit", to: "$parent" }},
          { then: "SELECTED", and: { send: "read", to: "$parent" }}
        ],
        updated: { then: "SELECTED", and: [ { set: "availability" }, { send: "updated", to: "$parent" }]},
        refresh: { then: "SELECTED", and: { pub: "refresh" }},
        data: { then: "SELECTED", and: { set: "availability" }},
      },
    },

    listen: {
      events: {
        $parent: { selected: "selected", set_availability: "set", clear_availability: "clear", refresh_availability: "refresh" }  
      }
    },


    methods: {
      $reset: function() {
        this.$data.availability = {};
      },
      
      sameOrganization: function(av) {
        return  av.organization === this.$data.organization.id;
      },

    }

  });

  Vue.$comp( "profile-availability", {
    data: { access: "read", profile: {}, user: {}, organizations: [], edit: false, year: 0, month: 0, day: 0, week: 0  },
    computed: {
      is_edit: function() {
        return this.edit;
      },

      readonly: function() {
        return this.access === "read";
      }
    },
    states: {
      INIT: {
        init: { then: "INIT", and: "setToday" },
        profile: { then: "INIT", and: [{ set: { profile : "profile", user: "user"} }, "setAccess", "fetch_organizations" ]},
        fetch_organizations: { then: "FETCHING_ORGANIZATIONS", and: { http: "get.user.organizations", body: { id: "profile.id"} }},
        fetch_availabilities: { then: "FETCHING_AVAILABILITIES", and: [ { http: "get.user.availabilities", body: { id: "profile.id", year: "year", week: "week" }}]},
        period: { then: "INIT", and: [ "clearCalendar", "clearAvailabilities", "setPeriod" ]}
      },

      FETCHING_ORGANIZATIONS: {
        data: { then: "INIT", and: {set: "organizations"}}
      },

      FETCHING_AVAILABILITIES: {
        data: { then: "TEMPLATES", and: [ "pubAvailabilities", "selectFirst" ] }
      },  

      TEMPLATES: {
        profile: { then: "INIT", and: "profile" },
        fetch_availabilities: { then: "INIT", and: [ "fetch_availabilities" ] },
        period: { then: "INIT", and: [ "clearCalendar", "clearAvailabilities", "setPeriod", "fetch_availabilities"]},
        organization_selected: { then: "TEMPLATES", and: [ "clearCalendar", { pub: "selected" }] },
        inspector: { then: "INSPECTOR" },
        keys: { then: "KEYS" },
        shifts: { then: "TEMPLATES", and: "pubShifts" },
        disabled: { then: "TEMPLATES", and: [ "clearCalendar" ] },
        updated: { then: "TEMPLATES", and: [ { pub: "availability" }]},
        read: { then: "TEMPLATES", and: "clearCalendar" },
        edit: { then: "TEMPLATES", and: "editCalendar" },
        slot: { then: "INSPECTOR", and: { pub: "create" }},
        item_selected: [
          { if: { eq: "edit", value: "true" }, then: "INSPECTOR", and: "item_selected" },
        ],
        item_moved: { then: "TEMPLATES", and: { pub: "move" }},
        moved: { then: "TEMPLATES", and: "replaceShift" },
        removed: { then: "TEMPLATES", and: { pub: "delete" }},
        item_moved_next_day: { then: "TEMPLATES", and: { pub: "move_to_next_day" }}, 
        item_moved_prev_day: { then: "TEMPLATES", and: { pub: "move_to_prev_day" }}, 
        item_copied_next_day: { then: "TEMPLATES", and: { pub: "copy_to_next_day" }}, 
        item_copied_prev_day: { then: "TEMPLATES", and: { pub: "copy_to_prev_day" }}, 
        item_moved_earlier: { then: "TEMPLATES", and: { pub: "move_earlier" }}, 
        item_moved_later: { then: "TEMPLATES", and: { pub: "move_later" }}, 
        item_ends_earlier: { then: "TEMPLATES", and: { pub: "end_earlier" }}, 
        item_ends_later: { then: "TEMPLATES", and: { pub: "end_later" }}, 
      },

      INSPECTOR: {
        profile: { then: "INIT", and: "profile" },
        fetch_availabilities: { then: "INIT", and: [ "fetch_availabilities" ] },
        templates: { then: "TEMPLATES" },
        keys: { then: "KEYS" },
        period: { then: "INIT", and: "period" },
        slot: { then: "INSPECTOR", and: { pub: "create" }},
        updated: { then: "INSPECTOR", and: [ "pubShift", { pub: "refresh_availability" }]},
        created: { then: "INSPECTOR", and: [ "pubShift", { pub: "refresh_availability" }]},
        item_selected: { then: "INSPECTOR", and: { pub: "edit" }},
        item_moved: { then: "INSPECTOR", and: { pub: "move" }},
        moved: { then: "INSPECTOR", and: [ "replaceShift", { pub: "refresh_availability" }]},
        removed: { then: "INSPECTOR", and: [{ pub: "delete" }, { pub: "refresh_availability"}]},
        item_moved_next_day: { then: "INSPECTOR", and: { pub: "move_to_next_day" }}, 
        item_moved_prev_day: { then: "INSPECTOR", and: { pub: "move_to_prev_day" }}, 
        item_copied_next_day: { then: "INSPECTOR", and: { pub: "copy_to_next_day" }}, 
        item_copied_prev_day: { then: "INSPECTOR", and: { pub: "copy_to_prev_day" }}, 
        item_moved_earlier: { then: "INSPECTOR", and: { pub: "move_earlier" }}, 
        item_moved_later: { then: "INSPECTOR", and: { pub: "move_later" }}, 
        item_ends_earlier: { then: "INSPECTOR", and: { pub: "end_earlier" }}, 
        item_ends_later: { then: "INSPECTOR", and: { pub: "end_later" }}, 
      },

      KEYS: {
        templates: { then: "TEMPLATES" },
        inspector: { then: "INSPECTOR" },
        period: { then: "INIT", and: "period" },
        slot: { then: "INSPECTOR", and: "create" },
        item_selected: { then: "INSPECTOR", and: "item_selected" },
        item_moved: { then: "INSPECTOR", and: "item_moved" },
        moved: { then: "INSPECTOR", and: "moved" },
        removed: { then: "INSPECTOR", and: "removed" },
        item_moved_next_day: { then: "KEYS", and: { pub: "move_to_next_day" }}, 
        item_moved_prev_day: { then: "KEYS", and: { pub: "move_to_prev_day" }}, 
        item_moved_earlier: { then: "KEYS", and: { pub: "move_earlier" }}, 
        item_moved_later: { then: "KEYS", and: { pub: "move_later" }}, 
        item_ends_earlier: { then: "KEYS", and: { pub: "end_earlier" }}, 
        item_ends_later: { then: "KEYS", and: { pub: "end_later" }}, 

      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      states: {
        $parent: {AVAILABILITY: "fetch_availabilities" }
      },

      events: {
        $parent: {profile: "profile"}
      }
    },

    methods: {

      setAccess: function() {
        this.$data.access = ( this.$data.profile.id === this.$data.user.id ) ? "owner" : "read";
        this.$pub( "access", this.$data.access );
      },

      
      setToday: function() {
        var p = Vue.$date();
        this.$data.year = p.year();
        this.$data.month = p.month() + 1;
        this.$data.day = p.date();
        this.$data.week = p.isoWeek();
      },
     
      setPeriod: function( str ) {
        var p = Vue.$date( str );
        this.$data.year = p.year();
        this.$data.month = p.month() + 1;
        this.$data.day = p.date();
        this.$data.week = p.isoWeek();
      },
      
      clearCalendar: function() {
        this.$data.edit = false;
        this.$pub( "clear", false );
      },

      editCalendar: function() {
        this.$data.edit = true;
        this.$pub( "clear", true );
      },

      clearAvailabilities: function() {
        this.$pub( "clear_availability" );
      },

      replaceShift: function(msg) {
        msg.focus = true;
        this.$pub( "delete", msg );
        this.$pub( "item", msg );
      },

      pubShift: function(msg) {
        msg.focus = true;
        this.$pub( "item", msg );
      },

      pubShifts: function(shifts){
        var self = this;
        shifts.map( function(s) { self.$pub( "item", s ); });
      },
      
      pubAvailabilities: function(msg) {
        var self = this;
        msg.map( function(a) { self.$pub( "set_availability", a ); }); 
      },

      selectFirst: function() {
        if (this.$data.organizations.length ) {
          this.$pub( "selected", this.$data.organizations[0] );
        }
      }
    }
           
  });


  Vue.$comp( "profile", {
    data: { user: {}, profile: {} },

    states: {
      INIT: {
        init: { then: "INIT", and: "getUser" },
        user: { then: "INIT", and: [{ set: "user" }, "profile" ]},
        profile: { then: "INIT", and: [ "printMsg" , "setProfile", "pubProfile", "shifts" ]},
        shifts: { then: "SHIFTS" }
      },

      INFO: {
        init: { then: "INIT", and: "init" },
        changed: { then: "INFO", and: [ { pub: "changed" }, {pub: "id", data: "profile.id" }] },
        profile: { then: "INIT", and: "profile" },
        stats: { then: "STATS" },
        shifts: { then: "SHIFTS" },
        availability: { then: "AVAILABILITY" }
      },

      STATS: {
        info: { then: "INFO" },
        init: { then: "INIT", and: "init" },
        profile: { then: "INIT", and: "profile" },
        shifts: { then: "SHIFTS" },
        availability: { then: "AVAILABILITY" }
      },

      SHIFTS: {
        info: { then: "INFO" },
        init: { then: "INIT", and: "init" },
        profile: { then: "INIT", and: "profile" },
        stats: { then: "STATS" },
        availability: { then: "AVAILABILITY" },
        print: { then: "SHIFTS", and: "print" }
      },

      AVAILABILITY: {
        info: { then: "INFO" },
        init: { then: "INIT", and: "init" },
        profile: { then: "INIT", and: "profile" },
        stats: { then: "STATS" },
        shifts: { then: "SHIFTS" }
      }
    },

    hooks: {
      $init: "init"
    },
    
    listen: {
      states: {
        app: { SIGNED_IN: "init" }
      },

      events: {
        main: { profile: "profile" },
        "profile-info": { changed: "changed" }
      }
    },

    methods: {
      
      getUser: function() {
        getMyProfile( this, "user" ); 
      },

      setProfile: function(msg){
        this.$data.profile = msg && msg.profile ? msg.profile : this.$data.user;
      },
      
      pubProfile: function(){
        this.$pub( "id", this.$data.profile.id );
        this.$pub( "profile", { 
          profile: this.$data.profile,
          user: this.$data.user
        });
      },

      $reset: function() {
        this.$data.user = {};
        this.$data.profile = {};
      },

      print: function() {
        window.print();
      }
    }
  });

  Vue.$comp( "organization-chooser", {
    many: true,
    data: { organizations: [] },

    states: {
      INIT: {
        init: { then: "READY", and: [{set: "organizations"}, "setup", "render" ]},
      },

      READY: {
        init: { then: "INIT", and: "init" },
        selected: { then: "READY", and: { send: "organization_selected", to: "$parent" }},
        refresh: { then: "READY", and: "render" },
        clear: { then: "READY", and: "clear" }
      }
    },

    methods: {

      setup: function() {
        var self = this;
        var $el = $( this.$el );
  
        $el.selectpicker();

        $el.on( "change", function(e) {
          self.$receive( "selected", $el.selectpicker("val")  );    
        });

      },

      render: function() {
        var $el = $( this.$el );
        var self = this;
        this.$nextTick( function() {
          $el.selectpicker( 'refresh' );
          self.$receive( "selected", $el.selectpicker( "val" ) );
        });
      },

      clear: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", null ); 
      }
    },

    listen: {
      events: {
        $parent: { organizations: "init" }
      }
    }
  });


  Vue.$comp( "profile-select", {
    many: true,
    props: [ "readonly" ],
    data: { profiles: [], selectionDisplay: "" },

    states: {
      INIT: {
        init: { then: "READY", and: [{set: "profiles"}, "setup", "render" ]},
        select: { then: "INIT", and: "select" }
      },

      READY: {
        init: { then: "READY", and: [{set: "profiles"}, "render" ]},
        selected: { then: "READY", and: { send: "profile_selected", to: "$parent" }},
        select: { then: "READY", and: "select" },
        refresh: { then: "READY", and: "render" },
        clear: { then: "READY", and: "clear" }
      }
    },

    methods: {

      withEl: function( next ) {
        var self = this;
        if( self.$select ) return next(self, self.$select );
        var $el = $( this.$el );
        if( $el && $el.length ) {
          self.$select = $el;
          next( this, $el ); 
        }
      },

      setup: function() {
        this.withEl( function( self, $el ) {
          $el.selectpicker();
          $el.on( "change", function(e) {
            self.$receive( "selected", $el.selectpicker("val")  );    
          });
        });
      },
      
      render: function() {
        this.withEl( function( self, $el) {
          self.$nextTick( function() {
            $el.selectpicker( 'refresh' );
          });
        });
      },

      select: function( msg ) {
        this.withEl( function( self, $el) {
          var value = msg || null;
          $el.selectpicker( "val", value);
          if( value ) { 
            var found = self.$data.profiles.filter( function(p){
              return p.id === msg;
            });
            self.$data.selectionDisplay = found.length ? found[0].name : "";
          } else self.$data.selectionDisplay = "";
        });
      },

      clear: function() {
        this.withEl( function( self, $el) {
          $el.selectpicker( "val", null ); 
        });
      }
    },

    listen: {
      events: {
        $parent: { profiles: "init", "selected-profile": "select" }
      }
    }
  });

  Vue.$comp( "template-select", {
    many: true,
    data: { templates: [] },

    states: {
      INIT: {
        init: { then: "READY", and: [{set: "templates"}, "setup", "render" ]},
        select: { then: "INIT", and: "select" }
      },

      READY: {
        init: { then: "READY", and: [{set: "templates"}, "render" ]},
        selected: { then: "READY", and: { send: "template_selected", to: "$parent" }},
        select: { then: "READY", and: "select" },
        refresh: { then: "READY", and: "render" },
        clear: { then: "READY", and: "clear" }
      }
    },

    methods: {

      withEl: function( next ) {
        var self = this;
        var $el = $( this.$el );
        if( $el ) next( this, $el ); 
      },

      setup: function() {
        this.withEl( function( self, $el ) {
          $el.selectpicker();
          $el.on( "change", function(e) {
            self.$receive( "selected", $el.selectpicker("val")  );    
          });
        });

      },
      
      render: function() {
        this.withEl( function( self, $el) {
          self.$nextTick( function() {
            $el.selectpicker( 'refresh' );
          });
        });
      },

      select: function( msg ) {
        this.withEl( function( self, $el) {
          $el.selectpicker( "val", msg );
        });
      },

      clear: function() {
        this.withEl( function( self, $el) {
          $el.selectpicker( "val", null ); 
        });
      }
    },

    listen: {
      events: {
        $parent: { templates: "init", selected: "select" }
      }
    }
  });

  Vue.$comp( "user-chooser", {
    many: true,
    props: [ "selected", "readonly" ],
    data: { users: [], user: {}, selectionLabel: "", selectionHtml: ""  },

    states: {
      INIT: {
        users: { then: "READY", and: [ {set: "users" }, "setup", "select" ]}       
      },

      READY: {
        users: { then: "INIT", and: "users" },
        selected: { then: "READY", and: [ { send: "user_selected", to: "$parent" }, {pub: "refresh"} ]},
        refresh: { then: "READY", and: [ "select", { pub: "refresh" }]},
        clear: { then: "READY", and: "reset" }
      },
    },

    methods: {

      userDisplayText: function(item) {
        if( !item ) return;
        var txt = "{0} {1}".$format([item.first, item.last]);  
        return txt;
      },

      setup: function() {
        if( !this.$data.users.length ) this.$error( "no users to setup typeahead" );
        var self = this;
        var $target = $( this.$el ).find( " > .typeahead" );
        $target.typeahead( "destroy" );
        $target.typeahead({
          source: this.$data.users,
          matcher: function(item) {
            return item.first.$match( this.query ) ||
              item.last.$match( this.query ) ||
                item.email.$match( this.query );
          },

          updater: function(item) {
            self.$receive( "selected", item.id );
            return self.userDisplayText( item );
          },

          sorter: function(items) {
            return items;
          },

          highlighter: userHtml
        });
      },

      select: function(msg) {
        var id = ( msg && typeof(msg) === "string" ) ? msg : this.$data.selected;
        if( !id ) return;
        var user = this.$data.users.$findUsing( "id", id );
        if( !user ) {
          this.$error( "no user found for selection: " + id );
        } else {
          this.$data.selected = id;
          this.$data.selectionLabel = this.userDisplayText( user );
          this.$data.selectionHtml = userNameAndEmail( user );
          this.refreshSelection();
          this.$receive( 'selected', user.id );
        }
      },

      refreshSelection: function() {
        var $target = $( this.$el ).find( " > .typeahead" );
        $target.val( this.$data.selectionLabel );
      },

      reset: function() {
        var $target = $( this.$el ).find( " > .typeahead" );
        $target.val( null );
        this.$data.selected = null;
        this.$data.selectionLabel = "";
        this.$data.selectionHtml = "";
      }

    },

    listen: { 
      events: {
        $parent: { users: "users", refresh: "refresh", clear: "clear" }
      }
    }
  });

  Vue.$comp( "user-avatar", {
    many: true,
    silent: true,
    props: [ "id", "user", "size" ],
    data: { profile: {}, photo_data: "" },

    computed: {
      displayText: function() {
        return this.profile.first + " " + this.profile.last;
      },
    },

    states: {
      READY: {
        init: { then: "READY", and: "fetch" },
        id: { then: "READY", and: [ {set: "id" }, "fetch"] },
        refresh: { then: "READY", and: "fetch" },
        user: { then: "READY", and: [ { set: "id" }, "fetch" ] },
        profile_data: { then: "READY", and: [ { set: "profile" }, { send: "profile_data", to: "$parent" }]},
        photo_data: { then: "READY", and: { set: "photo_data" }},
        show_profile: { then: "READY", and: { send: "profile", to: "main", args: { profile: "profile", user: "user" } }}
      }
    },

    hooks: { $init: "init" },

    listen: {
      events: {
        $parent: { id: "id", refresh: "refresh" }
      }    
    },

    methods: {
      fetch: function(msg) {
        var id = msg || this.$data.id;
        this.$data.id = id;
        this.$debug( "fetching photo for: " + this.$data.id );
        if( this.$data.id) {
          getProfile( this, this.$data.id, "profile_data" );
          getPhoto( this, this.$data.id, "photo_data" );
        }
      }
    }
  });

  Vue.$comp( "tasks-init");
  Vue.$comp( "tasks-disabled");


  Vue.$comp( "tasks-user-stats-by-status", {
    data: { stats: {} },

    states: {
      READY: {
        stats: { then: "READY", and:[ { set: "stats" }, "draw" ]}
      }
    },

    methods: {

      draw: function() {
        if( this.chart ) this.chart.destroy();
        this.chart = this.doughnut( 
                                   this.tasksByStatusChartData( this.$data.stats ) );    
      }
    },

    listen: {
      events:{
        $parent: { stats: "stats" }
      }
    }

  });

  Vue.$comp( "tasks-user-stats-by-duration", {
    data: { stats: {}, },

    states: {
      READY: {
        stats: { then: "READY", and:[ { set: "stats" }, "draw" ]},

      }
    },

    methods: {

      draw: function() {
        if( this.chart ) this.chart.destroy();
        this.chart = this.doughnut( 
                                   this.tasksByDurationChartData( this.$data.stats )
                                  );    
      }
    },

    listen: {
      events:{
        $parent: { stats: "stats"  }
      }
    }

  });


  Vue.$comp( "tasks-organization-stats-by-duration", {
    many: true,
    data: { stats: {}, organization: {} },
    states: {
      READY: {
        stats: { then: "READY", and: [{set: "stats"}, "draw"] },
        organization: { then: "READY", and: { set: "organization" } }
      }
    },

    listen: {
      events: {
        $parent: { stats: "stats", organization: "organization" }
      }
    },

    methods: {

      draw: function() {
        var data = this.tasksByDurationChartData( this.$data.stats );
        if( this.chart ) this.chart.destroy();
        this.chart = this.doughnut( data );    
      }
    }
  });

  Vue.$comp( "tasks-organization-stats-by-status", {
    many: true,
    data: { stats: {}, organization: {} },
    states: {
      READY: {
        stats: { then: "READY", and: [{set: "stats"}, "draw"] },
        organization: { then: "READY", and: { set: "organization" } }
      }
    },

    listen: {
      events: {
        $parent: { stats: "stats", organization: "organization" }
      }
    },

    methods: {

      draw: function() {
        var data = this.tasksByStatusChartData( this.$data.stats );
        if( this.chart ) this.chart.destroy();
        this.chart = this.doughnut( data  );    
      }
    }
  });


  Vue.$comp( "tasks-organization-stats", {
    many: true,
    props: { organization: {}, stats: {} },
    states: {
      INIT: {
        organization: { then: "INIT", and: [{set: "organization" }, {pub: "organization"}, "stats"]},
        stats: { then: "INIT", and: { ws: "stats", body: { id: "organization.id"}}},
        data: { then: "READY", and: [{ set: "stats" }, { pub: "stats" } ]}
      },

      READY: {
        organization: { then: "INIT", and: "organization" }    
      }
    },

    listen: {
      events: {
        $parent: { organization: "organization" }
      }
    }
  });


  Vue.$comp( "tasks-active", {
    props: [ "user" ],
    data: { 
      acl: {name: "tasks", left: 0},
      organization: {},
      stats: {}
    },

    ui: {
      continue: { $DEFAULT: "hidden", ORGANIZATION: "visible" }
    },

    states: {
      INIT: {
        acl: { then: "INIT", and: [{ set: "acl"}, "init" ]},
        init: { then: "INIT", and: [{ ask: "account", for: "current_stats" }] },
          stats: { then: "READY", and: [ {set: "stats"}, {pub: "stats"}, {pub: "init"} ]}
      },

      READY: {
        init: { then: "INIT", and: "init"},
        acl: { then: "INIT", and: "acl"},
        organization: { then: "ORGANIZATION", and: [{set: "organization"}, {pub: "organization"} ] },
        stats: { then: "READY", and: [{ set: "stats" }, {pub: "stats"} ] },
      },

      ORGANIZATION: {
        init: { then: "INIT", and: "init"},
        acl: { then: "INIT", and: "acl"},
        continue: { then: "ORGANIZATION", and: 
          { send: "organization", to: "$parent", args: { id: "organization.id", name: "organization.name" }}},
          stats: { then: "ORGANIZATION", and: [{ set: "stats" }, {pub: "stats"} ] },
          organization: { then: "READY", and: "organization" }
      }
    },

    listen: {
      events: {
        tasks: {acl: "acl"},
        account: {stats: "stats" }
      },

    }


  });





  Vue.$comp( "task-list-item", {
    many: true,
    props: [ "user", "task", "organization" ],
    ui: {
      complete_label: { $DEFAULT: 'complete', COMPLETING: 'sending' },
      reopen_label: { $DEFAULT: 'reopen', REOPENING: 'sending' }
    },

    computed: {

      is_mine: function() {
        return (this.user.id === this.task.ownedby)
      }
    },

    states: {
      ACTIVE: {
        init: [
          { if: { eq: "task.status", value: "complete"  }, then: "COMPLETE" },
          { if: { eq: "task.status", value: "cancelled"  }, then: "CANCELLED" }
        ],
        done: { then: "COMPLETING", and: { ws: "complete_task", body: { id: "task.id" } }},
        open: { then: "ACTIVE", and: { send: "open", to: "$parent", args: "task" }}
      },



      COMPLETE: {
        reopen: { then: "REOPENING", and: { ws: "reopen_task", body: { id: "task.id" } }},
        open: { then: "COMPLETE", and: { send: "open", to: "$parent", args: "task" }}
      },



      CANCELLED: {
        reopen: { then: "REOPENING", and: { ws: "reopen_task", body: { id: "task.id" } }},
        open: { then: "CANCELLED", and: { send: "open", to: "$parent", args: "task" }}
      },

      COMPLETING: {
        data: { then: "COMPLETE", and: [{set: "task"}, { send: "changed", to: "tasks"}] }
      },

      REOPENING: {
        data: { then: "ACTIVE", and: [{set: "task"}, { send: "changed", to: "tasks"}]  }    
      },

    },

    hooks: {
      $init: "init"
    },

  });

  Vue.$comp( "organizations-list-item", {
    props: [ "org", "user" ],
    many: true,
    states: {
      DEFAULT: {
        select: { 
          then: "DEFAULT", and: { send: "selected", to: "$parent", args: "org" }
        }
      }
    },
  });

  Vue.$comp( "organization-create", {
    data: { name: "" },

    ui: {
      submit: { CREATING: "sending", $DEFAULT: "create" }  
    },

    states: {
      READY: {
        home: { then: "READY", and: { send: "home", to: "$parent" }},
        cancel: { then: "READY", and: { send: "home", to: "$parent" }},
        init: { then: "READY", and: { unset: ["name"] }},
        submit: { then: "SENDING", and: { post: "create.organization", body: { name: "name"} }}
      },

      SENDING: {
        data: { then: "READY", and: { send: "created", to: "$parent" } },
        invalid: { then: "READY", and: { pub: "warn" } },
        forbidden: { then: "READY", and: { pub: "warn" } },
        conflict: { then: "READY", and: { pub: "warn", msg: "organization_exists" }},
        error: { then: "READY", and: { pub: "error" } },
      },
    },

    listen: {
      states: {
        organizations: { CREATE: "init" } 
      }
    }

  });

  Vue.$comp( "organizations-list", {
    data: { 
      organizations: []
    },

    states: {
      READY: {
        fetch: { then: "FETCHING", and: { get: "get.organizations" } },
        selected: { then: "READY", and: { send: "selected", to: "$parent" } },
        create: { then: "READY", and: { send: "create", to: "$parent" }},
        submit: { then: "CREATING", and: { post: "create.organization", body: { name: "name" }} },
      },

      FETCHING: {
        data: { then: "READY", and: { set: "organizations" }},
        error: { then: "READY", and: { pub: "error" } },
      },
    },

    hooks: {
      $init: "fetch"
    },

    listen: {
      states: {
        organizations: { LIST: "fetch" }
      }
    },

    methods: {
      $reset: function(){
        this.$data.organizations=[];
        this.$data.name ="";
      }
    }
  });

  Vue.$comp( "organization-info", {
    data: { organization: {}, owner: {}, creator: {}, new_owner: "" },
    ui: {
      change_name: { $DEFAULT: "change_name", RENAMING: "sending" },
      assign: { $DEFAULT: "change_owner", CHANGING_OWNER: "sending" },
    },

    computed: {
      readonly: function() {
        return !this.organization.access || this.organization.access != 'owner';
      } 
    },

    states: {

      INIT: { 
        init: { then: "INIT", and: [{set: "organization"}, "fetch_owner" ]},
        fetch_owner: { then: "FETCHING_OWNER", and: "getOwner" },
        fetch_creator: { then: "FETCHING_CREATOR", and: "getCreator" },
        members: { then: "INIT", and: { pub: "users" }},
      },

      FETCHING_OWNER: {
        members: { then: "FETCHING_OWNER", and: { pub: "users" } },
        data: { then: "INIT", and: "fetch_creator" },
        user_selected: { then: "FETCHING_OWNER", and: { set: "new_owner" } },
      },

      FETCHING_CREATOR: {
        members: { then: "FETCHING_CREATOR", and: { pub: "users" } },
        data: {then: "READY", and: {set: "creator" }},
        user_selected: { then: "FETCHING_CREATOR", and: { set: "new_owner" } },
      },

      READY: {
        init: { then: "INIT", and: "init" },
        members: { then: "READY", and: { pub: "users" }},
        change_name: { then: "RENAMING", and: 
          { post: "rename.organization", body: { id: "organization.id", name: "organization.name" } } },
          change_owner: { then: "CHANGING_OWNER", and: 
            { post: "set.organization.owner", body: { id: "organization.id", owner: "new_owner" } } },
            user_selected: { then: "READY", and: { set: "new_owner" } },
            refresh: { then: "READY", and: {pub: "refresh" }}
      },

      RENAMING: {
        data: { then: "READY", and: [{ set: "organization" }, "updateCache", { send: "organization", to: "$parent" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
        conflict: { then: "READY", and: {pub: "warn", msg: "organization_exists"}}
      },

      CHANGING_OWNER: {
        data: { then: "READY", and: [{ set: "organization" }, { send: "organization", to: "$parent" }, { pub: "refresh" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
        conflict: { then: "READY", and: { pub: "warn", msg: "organization_member_name_conflict" }}
      }
    },

    listen: {
      events: {
        $parent: { organization: "init", members: "members" }
      },

      states: {
        $parent: { INFO: "refresh" }
      }
    },



    methods: {
      getOwner: function() {
        getProfile( this, this.$data.organization.ownedby, "data" );
      },

      getCreator: function() {
        getProfile( this, this.$data.organization.createdby, "data" );
      },

      updateCache: function(o) {
        updateOrganization(o);
      }

    }
  });

  Vue.$comp( "organization-member-create", {
    props: ["organization"],
    data: { email: "" },
    ui: {
      submit: { $DEFAULT: "invite", INVITING: "sending" } 
    },

    states: {

      READY: {
        submit: { then: "CREATING", and: { post: "add.organization.member", body: { id: "organization.id", email: "email" } }},
        cancel: { then: "READY", and: { send: "cancel", to: "$parent" }},
      },

      CREATING: {
        data: { then: "READY", and: [ {unset: ["email"]}, { send: "created", to: "$parent" } ] },
        invalid: { then: "READY", and: { pub: "warn" } },
        forbidden: { then: "READY", and: { pub: "warn" } },
        error: { then: "READY", and: { pub: "error" } },
        not_found: { then: "READY", and: { pub: "warn", msg: "no_such_user" } }
      }
    }
  });

  Vue.$comp( "organization-member-edit", {
    props: [ "organization" ],
    data: { member: {}, tags: [], tag: "", new_profile: "" },
    ui: {
      set_profile: { $DEFAULT: "set_profile", $SETTING_PROFILE: "sending" }
    },
    computed: {
      readonly: function(){
        return !this.organization.access || 
          this.organization.access != 'owner';
      },

      owner: function() {
        return this.organization.ownedby === this.member.id;
      },

      profileName: function() {
        var  p = getOrganizationProfile( this.$data.member.profile );
        return p.name;
      } 
    },

    states: {
      INIT: {
        init: { then: "INIT", and: [ { unset: [ "new_profile" ]}, { set: "member" }, {pub: "id", data: "member.id" }, "fetch_profiles" ]},
        organization_tags: { then: "INIT" , and: [{ pub: "suggestions" }, "fetch_tags" ]},
        fetch_profiles: { then: "FETCHING_PROFILES" , and: 
          { http: "get.schedule.profiles", body: { organization: "organization.id" }} 
        },
        fetch_tags: { then: "FETCHING_TAGS", and: { http: "get.organization.member.tags", 
          body: { id: "organization.id", member: "member.id" } } },
      },

      FETCHING_TAGS: { 
        data: { then: "READY", and: [{ set: "tags" } ] },
      },

      FETCHING_PROFILES: {
        data: { then: "INIT", and: [{ pub: "profiles"}, { pub: "selected-profile", data: "member.profile"}, "getOrganizationTags" ]}
      },

      READY: {
        init: { then: "INIT", and: "init" },
        tag_added: { then: "MODIFYING_TAGS", and: [{set: "tag"}, 
          { post: "add.organization.member.tag", body: { id: "organization.id", member: "member.id", tag: "tag" } }]},
        tag_removed: { then: "MODIFYING_TAGS", and: [{set: "tag"}, 
          { post: "remove.organization.member.tag", body: { id: "organization.id", member: "member.id", tag: "tag" } }]},
        remove: { then: "REMOVING", and: { post: "remove.organization.member", body: { id: "organization.id", member: "member.id" }} },
        cancel: { then: "READY", and: { send: "cancel", to: "$parent" }},
        profile_selected: { then: "READY", and: {set: "new_profile"} },
        set_profile: { then: "SETTING_PROFILE", and: {
          post: "set.organization.member.profile", body: { id: "organization.id", member: "member.id", profile: "new_profile" }}
        } 
      },

      MODIFYING_TAGS: {
        data: { then: "INIT", and: "fetch_tags" },
        invalid: { then: "INIT", and: "fetch_tags" },
        error: { then: "INIT", and: "fetch_tags" },
        not_found: { then: "INIT", and: "fetch_tags" }
      },

      REMOVING: {
        data: { then: "INIT", and: [ {unset: [ "member", "tags", "tag" ]}, { send: "removed", to: "$parent" }] },
      },

      SETTING_PROFILE: {
        data: { then: "READY", and: { set: "member" } },
      }
    },

    listen: {
      events: {
        $parent: { member: "init" }
      }
    },

    methods: {
      getOrganizationTags: function() {
        getTags( this, this.$data.organization.id, "organization_tags" );
      },

      getOrganizationProfiles: function() {
        getProfiles( this, this.$data.organization.id, "profiles" );
      }
    }
  });


  Vue.$comp( "organization-member-list-item", {
    many: true,
    props: [ "organization", "member"],

    computed: {
      is_owner: function() {
        return this.member.id === this.organization.ownedby;
      }
    },

    states: {
      READY: {
        select: { then: "READY", and: { send: "selected", to: "$parent", args: "member" }}
      }
    }
  });

  Vue.$comp( "organization-members-list", {
    props: ["organization" ],
    data: { members: [] },
    computed: {
      readonly: function() {
        return !this.organization.access ||
          this.organization.access != 'owner' ;
      }
    },

    states: {

      INIT: {
        init: { then: "INIT", and: "fetch" },
        fetch: { then: "FETCHING", and: { http: "get.organization.members",  body: { id: "organization.id" }} },
      },

      FETCHING: {
        data: { then: "READY", and: { set: "members" }} 
      },

      READY: {
        init: { then: "INIT", and: "init" },
        create: { then: "READY", and: { send: "create", to: "$parent" }},
        selected: { then: "READY", and: { send: "selected", to: "$parent" }}
      } 

    },

    listen: {
      states: { 
        $parent: { LIST: "init" }
      }
    }
  });

  Vue.$comp( "organization-members", {
    props: [ "organization" ],
    data: { member: {} },

    ui: {
      content: { 
        LIST: "organization-members-list",
        CREATE: "organization-member-create",
        EDIT: "organization-member-edit"
      }
    },

    states: {
      INIT: {
        init: { then: "LIST" }
      },

      LIST: {
        init: { then: "INIT", and: "init" },
        create: { then: "CREATE" },
        selected: { then: "EDIT", and: {pub: "member"}},
      },

      CREATE: {
        init: { then: "INIT", and: "init" },
        created: { then: "INIT", and: [ {send: "member", to: "$parent"}, "init" ]},
        cancel: { then: "LIST" },
      },

      EDIT: {
        init: { then: "INIT", and: "init" },
        done: { then: "LIST" },
        cancel: { then: "LIST" },
        removed: { then: "INIT", and: [ {send: "member", to: "$parent"}, "init" ]},
      }
    },


    listen: {
      states: {
        $parent: { MEMBERS: "init" }
      }
    },

    hooks: {
      $init: "init"
    },

  });

  Vue.$comp( "organization-tags", {
    props: ["organization" ],
    data: { profiles: [], tags: [], tag: "" },
    computed: {
      readonly: function() {
        return !this.organization.access || this.organization.access != 'owner';
      }
    },

    states: {
      INIT: {
        init:  {then: "INIT", and: "fetch_profiles" },
        fetch_tags: { then: "FETCHING_TAGS", and: { http: "get.organization.tags", body: { id: "organization.id" } }},
        fetch_profiles: { then: "FETCHING_PROFILES", and: { http: "get.schedule.profiles", body: { organization: "organization.id" }}}  
      },

      FETCHING_PROFILES: {
        data: { then: "INIT", and: [{set: "profiles"}, "fetch_tags" ]}
      },

      FETCHING_TAGS: {
        data: { then: "READY", and: "tags" }
      },

      READY: {
        init: { then: "INIT", and: "init" },
        tags: { then: "READY", and: "filterTags" },
        tag_added: { then: "MODIFYING_TAGS", and: [{set:"tag"}, { post: "add.organization.tag", body: {id: "organization.id", tag: "tag"}}]},
        tag_removed: { then: "MODIFYING_TAGS", and: [{set: "tag"}, { post: "remove.organization.tag", body: {id: "organization.id", tag: "tag"}}]},
      },

      MODIFYING_TAGS: {
        data: { then: "READY", and: [ "refreshTags" ]}
      },

    },

    listen: {
      states: {
        $parent: { TAGS: "init" }
      }
    },

    methods: {
      filterTags: function( tags ) {
        var profiles = this.$data.profiles;
        this.$data.tags = tags.filter( function(t) {
          return !profiles.$contains( function( i ) {
            return i.name.toLowerCase() === t;
          });
        });
      },

      refreshTags: function () {
        getTags( this, this.$data.organization.id, "tags", true );
      }
    }

    // for some reason this does not work...
    //hooks: { $init: "init" }

  });

  Vue.$comp( "organization-profile-list-item", {
    many: true,
    props: [ "profile" ],
    states: {
      READY: {
        select: { then: "READY", and: { send: "selected", to: "$parent", args: "profile" }}
      }
    }
  });

  Vue.$comp( "organization-profiles-list", {
    props: [ "organization" ],
    data: { profiles: {} },
    computed: {
      readonly: function() {
        return !this.organization.access || 
          this.organization.access != 'owner';
      }
    },

    states: {
      INIT: {
        init: { then: "FETCHING", and: { http: "get.schedule.profiles", body: {organization: "organization.id"}}}
      },

      FETCHING: {
        init: { then: "INIT", and: "init" },
        data: { then: "READY", and:{set: "profiles"}}
      },

      READY: {
        init: { then: "INIT", and: "init" },
        create: { then: "READY", and: { send: "create", to: "$parent" }},
        selected: { then: "READY", and: { send: "selected", to: "$parent" }}
      }
    },

    listen: {
      states: {
        $parent: { LIST: "init" }
      }
    },

    hooks: { $init: "init" }
  });

  Vue.$comp( "organization-rule-list-item", {
    many: true,
    props: [ "indicator", "organization", "profile" ],
    data: { rule: {} },
    computed: {
      readonly: function() {
        return !this.organization.access || this.organization.access != 'owner'; 
      },

      enforcement: function() {
        return (this.rule.strong  === 'true') ? 'strong' : 'soft';
      },

      enforcementIcon: function() {
        return (this.enforcement === 'strong' ) ? 'fa-lock' : 'fa-unlock';
      },

      status: function() {
        if (this.rule && this.rule.status ) return this.rule.status;
        return "inactive";
      }
    },

    ui: {
      style: { ENABLED: "primary", $DEFAULT: "disabled" },
      toggle_status: { ENABLED: "disable", $DEFAULT: "enable" }
    },

    states: {

      INIT: {
        init: { then: "INIT", and: "initValue" },
        rule: { if: "indicatorMatch", then: "INIT", and: [{set:"rule"}, "route" ]},
        route: [
          { if: { eq: "rule.status", value: "active" }, then: "ENABLED" },
          { then: "DISABLED" }
        ],
        toggle: { then: "CREATING", and: { post: "create.schedule.profile.rule", body: {
          organization: "organization.id", profile: "profile.id", indicator: "indicator.id", strong: {value: true}, op: {value: "eq"}, value: "indicator.value" }}}
      },

      DISABLED: {
        toggle: { then: "UPDATING", and: { post: "enable.schedule.profile.rule", body: {
          organization: "organization.id", profile: "profile.id", rule: "rule.id" }}},
          rule: { then: "DISABLED" },
          init: { then: "INIT", and: "init" }
      },

      ENABLED: {
        toggle: { then: "UPDATING", and: { post: "disable.schedule.profile.rule", body: {
          organization: "organization.id", profile: "profile.id", rule: "rule.id" }}},
          value: { then: "UPDATING", and: { post: "update.schedule.profile.rule", body: {
            organization: "organization.id", profile: "profile.id", rule: "rule.id", strong: "rule.strong", op: "rule.op", value: "rule.value" }}},
          enforcement: { then: "UPDATING", and: [ "toggleEnforcement", { post: "update.schedule.profile.rule", body: {
            organization: "organization.id", profile: "profile.id", rule: "rule.id", strong: "rule.strong", op: "rule.op", value: "rule.value" }}]},
          rule: { then: "ENABLED" },
          init: { then: "INIT", and: "init" }

      },

      CREATING: {
        data: { then: "INIT", and: "rule" },
        invalid: { then: "INIT" },
        error: { then: "INIT" }
      },

      UPDATING: {
        data: { then: "INIT", and: "rule" },
        invalid: { then: "UPDATING", and: { http: "get.schedule.profile.rule", body: {
          organization: "organization.id", profile: "profile.id", rule: "rule.id" }}}
      },

    },

    listen: {
      events: {
        $parent: { rule: "rule", reset: "init" }
      }
    },

    methods: {
      indicatorMatch: function( rule ) {
        return rule.indicator === this.$data.indicator.id;
      },

      initValue: function() {
        this.$data.rule.value = this.$data.indicator.value;
      },

      toggleEnforcement: function() {
        this.$data.rule.strong = !( this.$data.rule.strong === 'true' );
      }
    },

  });

  Vue.$comp( "organization-profile-edit", {
    props: ["organization"],
    data: { profile: {}, rules: [], wIndicators: [], dIndicators: [], members: [], owner: [], new_owner: "" },
    computed: {
      readonly: function() {
        return !this.profile.access  || this.profile.access != 'owner';
      }
    },

    ui: {
      assign: { $DEFAULT: "change_owner", CHANGING_OWNER: "sending" },
      rename: { $DEFAULT: "rename", CHANGING_OWNER: "sending" }
    },

    states: {
      INIT: {
        init: { then: "INIT", and: [ "clear", {set: "profile"}, "fetch_members"] },
        fetch_members: { then: "FETCHING_MEMBERS", and: "getMembers" },
        fetch_owner: { then: "FETCHING_OWNER", and: "getOwner" },
        fetch_indicators: { then: "FETCHING_INDICATORS", and: "getIndicators" },
        fetch_rules: { then: "FETCHING_RULES", and: 
          { http: "get.schedule.profile.rules", body: { organization: "organization.id", profile: "profile.id" }}},
      },

      FETCHING_RULES: {
        data: { then: "READY", and: "broadcastRules" },
        user_selected: { then: "FETCHING_RULES", and: { set: "new_owner" } }
      },

      FETCHING_INDICATORS: {
        data: { then: "INIT", and: [ "setIndicators", "resetIndicators", "fetch_rules" ]}, 
        user_selected: { then: "FETCHING_INDICATORS", and: { set: "new_owner" } }
      },

      FETCHING_MEMBERS: {
        data: { then: "INIT", and: [{set:"members"}, {pub: "users" }, "fetch_owner" ]},
        user_selected: { then: "FETCHING_MEMBERS", and: { set: "new_owner" } }
      },

      FETCHING_OWNER: {
        data: { then: "INIT", and: [{set: "owner"}, "fetch_indicators" ]},
        user_selected: { then: "FETCHING_OWNER", and: { set: "new_owner" } }
      },

      READY: {
        init: { then: "INIT", and: "init" },
        reset: { then: "INIT", and: "clear" },
        cancel: { then: "INIT", and: [ {unset: ["profile"]}, {send: "cancel", to: "$parent" }]},
        user_selected: { then: "READY", and: { set: "new_owner" }},
        change_name: { then: "RENAMING", and: 
          { post: "rename.schedule.profile", body: { organization: "organization.id", profile: "profile.id", name: "profile.name" } } },
        change_owner: { then: "CHANGING_OWNER", and: {
          post: "set.schedule.profile.owner", body: { organization: "organization.id", profile: "profile.id", owner: "new_owner" } }}
      },

      CHANGING_OWNER: {
        data: { then: "INIT", and: "init" },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
      },

      RENAMING: {
        data: { then: "READY", and: [ { set: "profile" }, "clearCache" ]},
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
      }
    },

    listen: {
      events: {
        $parent: { profile: "init", members: "members" }
      },

      states: {
        $parent: { LIST: "$reset" }
      }
    },

    methods: {

      clear: function( ){
        this.$data.rules = [];
        this.$data.profile = {};
        this.$data.wIndicators = [];
        this.$data.dIndicators = [];
      },

      getIndicators: function(){
        getIndicators( this, this.$data.organization.id, "data" );  
      },

      getMembers: function() {
        getUsers( this, this.$data.organization.id, "data" );
      },

      getOwner: function() {
        getProfile( this, this.$data.profile.ownedby, "data" );
      },

      setIndicators: function( msg ){
        var self = this;
        msg.map( function(i) {
          switch( i.type ) {
            case "w": self.$data.wIndicators.push( i); return;
            case "d": self.$data.dIndicators.push( i); return;
          }
        });
      },

      resetIndicators: function() {
        this.$pub( "reset" );
      },

      broadcastRules: function(rules) {
        var self = this;
        rules.map( function(r) {
          self.$pub( "rule", r );
        });
      },

      clearCache: function() {
        clearOrganizationProfiles();
        getOrganizationProfiles( this, this.$data.organization.id );
      }

    }

  });

  Vue.$comp( "organization-profile-create", {
    props: [ "organization" ],
    data: { name: "" },
    ui: {
      submit: { $DEFAULT: "create", CREATING: "sending" }
    },

    states: {

      READY: {
        submit: { then: "CREATING", and: {post: "create.schedule.profile", body: {organization: "organization.id", name: "name" }}},       
        cancel: { then: "READY", and: {send: "cancel", to: "$parent" }}
      },

      CREATING: {
        data: { then: "READY", and: [{unset: ["name"]}, "refreshCache", {send: "created", to: "$parent"}]},
        invalid: { then: "READY", and: { pub:"warn" }},
        conflict: { then: "READY", and: { pub: "warn", msg: "profile_exists" }}
      }

    },

    listen: {
      states: { 
        $parent: { CREATE: "init" }
      }
    },

    methods: {
      refreshCache: function() {
        clearOrganizationProfiles();
        getOrganizationProfiles( this, this.$data.organization.id );
      }
    }

  });

  Vue.$comp( "organization-profiles", {
    props: [ "organization" ],

    ui: {
      content: { 
        LIST: "organization-profiles-list",
        CREATE: "organization-profile-create",
        EDIT: "organization-profile-edit"
      }
    },

    states: {
      INIT: {
        init: { then: "LIST" }
      },

      LIST: {
        init: { then: "INIT", and: "init" },
        create: { then: "CREATE" },
        selected: { then: "EDIT", and: {pub: "profile"}},
      },

      CREATE: {
        init: { then: "INIT", and: "init" },
        created: { then: "LIST", and: "resetTags" },
        cancel: { then: "LIST" },
      },

      EDIT: {
        init: { then: "INIT", and: "init" },
        updated: { then: "LIST" },
        cancel: { then: "LIST" },
      }
    },


    listen: {
      states: {
        $parent: { PROFILES: "init" }
      }
    },
    
    methods: {
      resetTags: function() {
        tags[ this.$data.organization.id ] = null;
      }
    }


  });

  Vue.$comp( "organization", {
    data: { 
      organization: {}, 
      owner: {},
      user: {},
    },

    ui: {
      content: {
        INFO: "organization-info",
        MEMBERS: "organization-members",
        TAGS: "organization-tags",
        PROFILES: "organization-profiles"
      }
    },

    states: {
      INIT: {
        home: { then: "INIT", and: { send: "home", to: "$parent" }},
        init: { then: "MEMBERS", and: [ {set: "organization"}, "getProfiles", "getIndicators", "getMembers", {pub: "organization"} ]},
        members: { then: "MEMBERS" },
        info: { then: "INFO" },
        tags: { then: "TAGS" },
        profiles: { then: "PROFILES" }
      },

      INFO: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        members: { then: "MEMBERS" },
        tags: { then: "TAGS" },
        profiles: { then: "PROFILES" },
        organization: { then: "INFO", and: {set: "organization" }},
        info: { then: "INIT", and: "info" }
      },

      MEMBERS: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        info: { then: "INFO" },
        tags: { then: "TAGS" },
        profiles: { then: "PROFILES" },
        members: { then: "INIT", and: "members" },
        member: { then: "MEMBERS", and: "getMembers" },
        new_members: { then: "MEMBERS", and: { pub: "members" }},
      },

      TAGS: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        info: { then: "INFO" },
        members: { then: "MEMBERS" },
        profiles: { then: "PROFILES" },
        tags: { then: "INIT", and: "tags" }
      },

      PROFILES: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        info: { then: "INFO" },
        members: { then: "MEMBERS" },
        tags: { then: "TAGS" },
        profiles: { then: "INIT", and: "profiles" }
      }

    },

    methods: {
      $reset: function() {
        this.$data.organization = {};
        this.$data.email  = "";
        this.$data.owner = {};
        this.$data.members = [];
        this.$data.stats = {};
      },

      getMembers: function() {
        getUsers( this, this.$data.organization.id, "new_members", true );
      },

      getIndicators: function() {
        getIndicators( this, this.$data.organization.id, "indicators" );
      }, 
      
      getProfiles: function() {
        getOrganizationProfiles( this, this.$data.organization.id );
      }
    },

    listen: {
      events: {
        $parent: { organization: "init" },
      },
    }
  }); 

  Vue.$comp( "organizations", {
    ui: { 
      content: { 
        LIST: "organizations-list",
        CREATE: "organization-create",
        ORGANIZATION: "organization"
      } 
    },
    states: {

      NONE: {
        init: { then: "LIST" } 
      },

      LIST: {
        selected: { then: "ORGANIZATION", and: [ "resetUsers", { pub: "organization" }] },
        init: { then: "NONE", and: "init" },
        create: { then: "CREATE" }
      },

      CREATE: {
        home: { then: "NONE", and: "init" },
        init: { then: "NONE", and: "init" },
        created: { then: "LIST" }
      },

      ORGANIZATION: {
        home: { then: "NONE", and: "init" },
        init: { then: "NONE", and: "init" }
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      states: {
        main: { ORGANIZATIONS: "init" }
      }
    },

    methods: { 
      resetUsers: function( org ) {
        users = null;
      }
    }
  });


  Vue.$comp( "tags", {
    many: true,
    props: [ "readonly", "tags", "suggestions" ],
    data: { newTag: "" },
    states: {
      READY: {
        suggestions: { then: "READY", and: "setupSuggestions" },
        selected: { then: "READY", and: "add" },
        add: { if: { value: "newTag", isNewIn: "tags" }, then: "READY",  
          and: [{ send: "tag_added", to: "$parent", args: "newTag" }, { unset: ["newTag"]}]},
        remove: { then: "READY", and: { send: "tag_removed", to: "$parent" }},
      }
    },

    listen: {
      events: {
        $parent: { suggestions: "suggestions" }
      }
    },

    methods: {

      setupSuggestions: function( tags ) {
        var self = this;
        var $el = $( this.$el ).find( ".tags-input" );
        $el.typeahead( "destroy" );
        $el.typeahead({
          source: tags,
          matcher: function(item) {
            return item.toLowerCase().$match( this.query.toLowerCase() );
          },

          updater: function(tag) {
            self.$receive( "selected", tag );
            return tag ;
          },

          sorter: function(items) {
            return items;
          },

          highlighter: function(tag) {
            return ( '<div><i class="fa fa-tag"> </i> {0}</div>' )
            .$format([ tag ]);
          },
        });

      }
    }

  });

  Vue.$comp( "template-create", {
    data: { name: "", organization: "", mode: "" },
    ui: {
      submit: { $DEFAULT: "create", CREATING: "sending" }
    },
    computed: {
      title: function() {
        return (this.mode === "schedules") ? 
          "schedule_template_create" : "availability_template_create";
      }
    },
    states: {
      INIT: {
        schedules: { then: "INIT", and: { set: { mode: { value: "schedules" }}}},
        availability: { then: "INIT", and: { set: { mode: { value: "availability" }}}},
        init: { then: "INIT", and: [ "getOrganizations", { send: "mode", to: "$parent" }]},
        organizations: { then: "READY", and: { pub: "organizations" } },
        mode: { then: "INIT", and: { set: "mode" } },
      },

      READY: {
        cancel: { then: "INIT", and: [ { unset: [ "name", "organization" ]}, {send: "cancel", to: "$parent" }]},
        submit: [
          { if: { eq: "mode", value: "schedules" }, then: "CREATING", and: { post: "create.schedule.template",
            body: { organization: "organization", name: "name" } }},
          { if: { eq: "mode", value: "availability" }, then: "CREATING", and: { post: "create.availability.template",
            body: { organization: "organization", name: "name" } }}
        ],
        organization_selected: { then: "READY", and: {set: "organization" } },
        home: { then: "READY", and: "cancel" },
        mode: { then: "READY", and: { set: "mode" }},
      },

      CREATING: {
        data: { then: "INIT", and: [ "reset", { send: "created", to: "$parent" } ]},
        invalid: { then: "READY", and: { pub: "warn" }},
        conflict: { then: "READY", and: { pub: "warn", msg: "template_name_conflict"  }}
      }
    },

    methods: {
      getOrganizations: function() {
        getOrganizations( this, "organizations", true );
      },

      reset: function() {
        this.$data.name = "";
        this.$data.mode = "";
      },

    },

    listen: {
      states: {
        $parent: { CREATE: "init" },
      },

      events: {
        $parent: { mode: "mode" }
      }
    },

    hooks: {
      $init: "init"
    }
           
  });


  Vue.$comp( "shift-type-chooser", {
    many: true,
    data: { types: [ "std", "idle", "training" ], shift: {} },

    states: {
      INIT: {
        init: { then: "READY", and: [ "setup", "render" ]},
      },

      READY: {
        init: { then: "INIT", and: "init" },
        select: { then: "READY", and: [{ set: "shift"}, "refresh" ]},
        selected: { then: "READY", and: { send: "shift_type", to: "$parent" }},
        refresh: { then: "READY", and: "render" },
      }
    },

    methods: {

      setup: function() {
        var self = this;
        var $el = $( this.$el );
        $el.selectpicker();
        $el.on( "change", function(e) {
          self.$receive( "selected", $el.selectpicker("val")  );    
        });
      },

      render: function() {
        var $el = $( this.$el );
        var self = this;
        this.$nextTick( function() {
          $el.selectpicker( 'refresh' );
          self.$receive( "selected", $el.selectpicker( "val" ) );
        });
      },

      refresh: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", this.$data.shift.type );
      },

      clear: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", null ); 
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      events: {
        $parent: { shift: "select"}
      }
    }
  });
  
  Vue.$comp( "shift-inspector", {
    many: true,
    props: [ "mode", "template_mode" ],
    data: { template: {}, schedule: {}, availability: {}, organization: {}, shift: {} , tags: [], tag: "", assignedto: "" },
    ui: {
      submit: { CREATE: "create", EDIT: "update", CREATING: "sending", UPDATING: "sending", SAVING_TAGS: "sending" }
    },

    computed: {

      hasShift: function() {
        return this.shift && !this.shift.$isEmpty(); 
      },

      readonly: function() {
        switch( this.mode ) {
          case "availability" : return this.availability ? (this.availability.status != "edit") : true ; 
          case "template": return this.template ? (this.template.access != 'owner') : true;
          case "schedule": return this.schedule ? (this.schedule.access != 'owner' || this.schedule.auto === "true" ) : true;
          default: return true;
        }
      },

      duration: function() {
        var m1 = this.moment_from( this.$data.shift.start_hour, this.$data.shift.start_min );
        var m2 = this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min );
        return m2.diff( m1, 'minutes' );
      },

      start_time: function() {
        return this.moment_from( this.$data.shift.start_hour, this.$data.shift.start_min ).format( "HH:mm" );
      },

      end_time: function() {
        return this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min ).format( "HH:mm" );
      },

      is_new_shift: function() {
        return !this.shift.id ;
      },

      is_schedule: function() {
        return this.mode === "schedule";
      },

      is_template: function() {
        return (this.mode === "template");
      },

      is_availability: function() {
        return (this.mode === "availability");
      },

      is_schedule_template: function() {
        return this.template_mode === "schedules";
      },

      is_availability_template: function() {
        return this.template_mode === "availability";
      }
    },

    states: {
      INIT: {
        init: { then: "INIT", and: [{ send: "mode", to: "$parent" }, "setup" ]},
        clear: { then: "INIT", and: [ { unset: [ "shift" ] }]},
        template: { then: "INIT", and: [{ unset: ["shift"]}, {set: "template" }, "fetch_organization" ]},
        schedule: { then: "INIT", and: [{ unset: ["shift"]}, {set: "schedule" }, "fetch_template" ]},
        availability: { then: "EDIT", and: [ { set: "availability" } ]},
        fetch_organization: { then: "FETCHING_ORGANIZATION", and: "getOrganization" },
        fetch_template: { then: "FETCHING_TEMPLATE", and: "getTemplate" },
        fetch_members: { then: "FETCHING_MEMBERS", and: "getMembers" },
        fetch_tag_suggestions: { then: "FETCHING_TAG_SUGGESTIONS", and: "getTags" },
        fetch_tags: [
          { if: { eq: "mode", value: "template" }, then: "FETCHING_TAGS", and: { http: "get.schedule.template.shift.tags", 
              body: { organization: "organization.id", template: "template.id", shift: "shift.id" }}},
          { if: { eq: "mode", value: "schedule" }, then: "FETCHING_TAGS", and: { http: "get.schedule.shift.tags", 
              body: { organization: "organization.id", schedule: "shift.schedule", shift: "shift.id" }}}
        ],
        create: { then: "CREATE", and: [ "printMsg", "reset", {pub: "clear"}, "initShift", "updateTimes" ]},
        edit: [
          { if: [{ eq: "mode", value: "template" }, { eq: "template_mode", value: "availability"} ], 
            then: "EDIT", and: [ { set: "shift" }, "updateTimes", "show" ]},
          { if: { eq: "mode", value: "availability" }, 
            then: "EDIT", and: [ { set: "shift" }, "updateTimes", "show" ]},
          { then: "INIT", and: [ { set: "shift" }, {pub: "clear"}, {pub: "refresh", data: "shift.assignedto" }, {pub: "shift"}, "fetch_tags" ]}],
        move: { then: "MOVING", and: [ "moveShift", "submit" ]},
        move_earlier: { if: "canMoveEarlier", then: "MOVING", and: [ "moveEarlier", "submit" ]},
        move_later: { if: "canMoveLater", then: "MOVING", and: [ "moveLater", "submit" ]},
        move_to_prev_day: { if: "canMoveToPrevDay", then: "MOVING", and: [ "moveToPrevDay", "submit" ]},
        move_to_next_day: { if: "canMoveToNextDay", then: "MOVING", and: [ "moveToNextDay", "submit" ]},
        copy_to_prev_day: { if: "canMoveToPrevDay", then: "COPYING", and: [ "copyToPrevDay", "submit" ]},
        copy_to_next_day: { if: "canMoveToNextDay", then: "COPYING", and: [ "copyToNextDay", "submit" ]},
        end_earlier: { if: "canEndEarlier", then: "MOVING", and: [ "endEarlier", "submit" ]},
        end_later: { if: "canEndLater", then: "MOVING", and: [ "endLater", "submit" ]},
        template_mode: { then: "INIT", and: { set: "template_mode" } }
      },

      FETCHING_TEMPLATE: {
        data: { then: "INIT", and: [{set: "template"}, "fetch_organization"] },
        template_mode: { then: "FETCHING_TEMPLATE", and: { set: "template_mode" } }
      },

      FETCHING_ORGANIZATION: {
        data: { then: "INIT", and: [{set: "organization"}, "fetch_members" ]},
        template_mode: { then: "FETCHING_ORGANIZATION", and: { set: "template_mode" } }
      },

      FETCHING_MEMBERS: {
        data: { then: "INIT", and: [{ pub: "users" }, "fetch_tag_suggestions" ]},
        template_mode: { then: "FETCHING_MEMBERS", and: { set: "template_mode" } }
      },

      FETCHING_TAG_SUGGESTIONS: {
        data: { then: "INIT", and: {pub: "suggestions"}},
        template_mode: { then: "FETCHING_TAG_SUGGESTIONS", and: { set: "template_mode" } }
      },

      FETCHING_TAGS: {
        data: { then: "EDIT", and: [{set: "tags"}, "updateTimes", "show" ]},
        template_mode: { then: "FETCHING_TAGS", and: { set: "template_mode" } }
      },
      
      CREATE: {
        init: { then: "INIT", and: "init" },
        availability: { then: "INIT", and: "availability" },
        template: { then: "INIT", and: "template" },
        schedule: { then: "INIT", and: "schedule" },
        create: { then: "INIT", and: "create" },
        edit: { then: "INIT", and: "edit" },
        move: { then: "INIT", and: "move" },
        move_earlier: { then: "INIT", and: "move_earlier" },
        move_later: { then: "INIT", and: "move_later" },
        move_to_prev_day: { then: "INIT", and: "move_to_prev_day" },
        move_to_next_day: { then: "INIT", and: "move_to_next_day" },
        end_earlier: { then: "INIT", and: "end_earlier" },
        end_later: { then: "INIT", and: "end_later" },
        close: { then: "INIT", and: ["reset", { send: "cancel", to: "$parent" } ] },
        start_time: { then: "CREATE", and: "setStartTime" },
        end_time: { then: "CREATE", and: "setEndTime" },
        shift_type: { then: "CREATE", and: "setShiftType" },
        submit: [
          { if: [ { eq: "mode", value: "template" }, { eq: "template_mode", value: "schedules" }, "checkTimes" ], 
            then: "CREATING", and: { post: "add.schedule.template.shift", 
            body: { organization: "organization.id", template: "template.id", 
                    name: "shift.name", staffing: "shift.staffing", type: "shift.type",
                    start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                    end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { if: [ { eq: "mode", value: "template" }, { eq: "template_mode", value: "availability" }, "checkTimes" ], 
            then: "CREATING", and: { post: "add.availability.template.shift", 
            body: { organization: "organization.id", template: "template.id", 
                    start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                    end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { if: [ { eq: "mode", value: "schedule" }, "checkTimes" ], 
            then: "CREATING", and: { post: "add.schedule.shift", 
            body: { organization: "organization.id", schedule: "schedule.id", 
                    name: "shift.name", type: "shift.type",
                    day: "shift.day", month: "shift.month", year: "shift.year",
                    start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                    end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { if: [ "checkTimes", { eq: "mode", value: "availability" }],
            then: "CREATING", 
            and: { post: "create.availability.shift", 
                   body: { id: "availability.id", 
                      day: "shift.day", month: "shift.month", year: "shift.year",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { then: "CREATE", and: { pub: "warn", msg: "invalid_shift_times" }} ],
        user_selected: { then: "CREATE", and: {set: "assignedto" }},
        template_mode: { then: "CREATE", and: { set: "template_mode" } },
        clear: { then: "INIT", and: "clear" },

      },

      CREATING: {
        data: { then: "EDIT", and:[ "onShiftCreated", "assign" ]},
        conflict: { then: "CREATE", and: { pub: "warn", msg: "shift_conflict" }},
        not_found: { then: "CREATE", and: { pub: "warn" }},
        invalid: { then: "CREATE", and: {pub: "warn" } },
        error: { then: "CREATE", and: { pub: "error" } }
      },

      EDIT: {
        init: { then: "INIT", and: "init" },
        template: { then: "INIT", and: "template" },
        schedule: { then: "INIT", and: "schedule" },
        availability: { then: "INIT", and: "availability" },
        create: { then: "INIT", and: "create" },
        edit: { then: "INIT", and: "edit" },
        move: { then: "INIT", and: "move" },
        move_earlier: { if: "canMoveEarlier", then: "INIT", and: "move_earlier" },
        move_later: { if: "canMoveLater", then: "INIT", and: "move_later" },
        move_to_prev_day: { if: "canMoveToPrevDay", then: "INIT", and: "move_to_prev_day" },
        move_to_next_day: { if: "canMoveToNextDay", then: "INIT", and: "move_to_next_day" },
        copy_to_prev_day: { if: "canMoveToPrevDay", then: "INIT", and: "copy_to_prev_day" },
        copy_to_next_day: { if: "canMoveToNextDay", then: "INIT", and: "copy_to_next_day" },
        end_earlier: { if: "canEndEarlier", then: "INIT", and: "end_earlier" },
        end_later: { if: "canEndLater", then: "INIT", and: "end_later" },
        close: { then: "INIT", and: [ "reset", { send: "cancel", to: "$parent" }] },
        start_time: { then: "EDIT", and: "setStartTime" },
        end_time: { then: "EDIT", and: "setEndTime" },
        shift_type: { then: "EDIT", and: "setShiftType" },
        remove: { then: "REMOVING", and: "remove" },
        tag_added: [
          { if: { eq: "mode", value: "template"},  then: "UPDATING_TAGS", and: [{set: "tag"}, { post: "add.schedule.template.shift.tag",
            body: { organization: "organization.id", template: "template.id", shift: "shift.id", tag: "tag" }}]},
          { if: { eq: "mode", value: "schedule"},  then: "UPDATING_TAGS", and: [{set: "tag"}, { post: "add.schedule.shift.tag",
            body: { organization: "shift.organization", schedule: "shift.schedule", shift: "shift.id", tag: "tag" }}]}
        ],
        tag_removed: [
          { if: { eq: "mode", value: "template"}, then: "UPDATING_TAGS", and: [{set: "tag"}, { post: "remove.schedule.template.shift.tag",
            body: { organization: "organization.id", template: "template.id", shift: "shift.id", tag: "tag" }}]},
          { if: { eq: "mode", value: "schedule"},  then: "UPDATING_TAGS", and: [{set: "tag"}, { post: "remove.schedule.shift.tag",
            body: { organization: "shift.organization", schedule: "shift.schedule", shift: "shift.id", tag: "tag" }}]}
        ],
        submit: [
          { if: [ "checkTimes", { eq: "template_mode", value: "schedules"}, { eq: "mode", value: "template"}], 
            then: "UPDATING", and: { post: "update.schedule.template.shift", 
            body: { organization: "organization.id", template: "template.id", shift: "shift.id",
                    name: "shift.name", staffing: "shift.staffing", type: "shift.type",
                    start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                    end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { if: [ "checkTimes", { eq: "template_mode", value: "availability" }, { eq: "mode", value: "template"}], 
            then: "UPDATING", and: { post: "update.availability.template.shift", 
            body: { organization: "organization.id", template: "template.id", shift: "shift.id",
                    start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                    end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { if: [ "checkTimes", { eq: "mode", value: "schedule" }], then: "UPDATING", and: { post: "update.schedule.shift", 
            body: { organization: "shift.organization", schedule: "shift.schedule", shift: "shift.id",
                    name: "shift.name", type: "shift.type",
                    day: "shift.day", month: "shift.month", year: "shift.year",
                    start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                    end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { if: [ "checkTimes", { eq: "mode", value: "availability" }],
            then: "UPDATING", 
            and: { post: "update.availability.shift", 
                   body: { id: "availability.id", shift: "shift.id",
                      day: "shift.day", month: "shift.month", year: "shift.year",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          { then: "EDIT", and: { pub: "warn", msg: "invalid_shift_times" }} ],
        user_selected: { then: "EDIT", and: {set: "assignedto" }},
        assign: [
          { if: [{ notEmpty: "assignedto", source: "data" }, { eq: "mode", value: "schedule" }], then: "ASSIGNING", and: { post: "assign.schedule.shift", 
            body: { organization: "schedule.organization", schedule: "schedule.id", shift: "shift.id", assignedto: "assignedto"  } }}
        ],
        template_mode: { then: "EDIT", and: { set: "template_mode" } },
        clear: { then: "INIT", and: "clear" },
      },

      UPDATING: {
        data: { then: "EDIT", and:[{set: "shift"}, { send: "updated", to: "$parent" }, "assign" ]},
        invalid: { then: "UPDATING", and: {pub: "warn" } },
        error: { then: "UPDATING", and: { pub: "error" } }
      },

      UPDATING_TAGS: {
        data: { then: "EDIT", and: { set: "tags" }},
        invalid: { then: "EDIT", and: {pub: "warn" }},
        error: { then: "EDIT", and: { pub: "error" }}
      },

      MOVING: {
        submit: [
          { if: [{ eq: "mode", value: "template" }, { eq: "template_mode", value: "schedules" }],
            then: "MOVING", 
            and: { post: "update.schedule.template.shift", 
                   body: { organization: "organization.id", template: "template.id", shift: "shift.id",
                      name: "shift.name", staffing: "shift.staffing", type: "shift.type",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          
          { if: [{ eq: "mode", value: "template" }, { eq: "template_mode", value: "availability" }],
            then: "MOVING", 
            and: { post: "update.availability.template.shift", 
                   body: { organization: "organization.id", template: "template.id", shift: "shift.id",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
                  
          { if: { eq: "mode", value: "schedule" }, then: "MOVING", and: { post: "update.schedule.shift", 
            body: { organization: "shift.organization", schedule: "shift.schedule", shift: "shift.id",
                  name: "shift.name", type: "shift.type",
                  day: "shift.day", month: "shift.month", year: "shift.year",
                  start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                  end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
        
          { if: { eq: "mode", value: "availability" },
            then: "MOVING", 
            and: { post: "update.availability.shift", 
                   body: { id: "availability.id", shift: "shift.id",
                      day: "shift.day", month: "shift.month", year: "shift.year",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}}],
        data: { then: "EDIT", and: [ "onShiftMoved", "updateTimes" ]},
        conflict: { then: "EDIT", and: [{ pub: "warn", msg: "shift_conflict" }]},
        error: { then: "EDIT", and: [{ send: "moved", to: "$parent" }, "reset" ]},
        close: { then: "INIT", and: [ "reset", { send: "cancel", to: "$parent" }]},
      },

      COPYING: {
        submit: [
          { if: [{ eq: "mode", value: "template" }, { eq: "template_mode", value: "schedules" }],
            then: "COPYING", 
            and: { post: "copy.schedule.template.shift", 
                   body: { organization: "organization.id", template: "template.id", shift: "shift.id",
                      name: "shift.name", staffing: "shift.staffing", type: "shift.type",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
          
          { if: [{ eq: "mode", value: "template" }, { eq: "template_mode", value: "availability" }],
            then: "COPYING", 
            and: { post: "copy.availability.template.shift", 
                   body: { organization: "organization.id", template: "template.id", shift: "shift.id",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
                  
          { if: { eq: "mode", value: "schedule" }, 
            then: "COPYING", and: { post: "copy.schedule.shift", 
            body: { organization: "shift.organization", schedule: "shift.schedule", shift: "shift.id",
                  name: "shift.name", type: "shift.type",
                  day: "shift.day", month: "shift.month", year: "shift.year",
                  start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                  end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}},
        
          { if: { eq: "mode", value: "availability" },
            then: "COPYING", 
            and: { post: "copy.availability.shift", 
                   body: { id: "availability.id", shift: "shift.id",
                      day: "shift.day", month: "shift.month", year: "shift.year",
                      start_day: "shift.start_day", start_hour: "shift.start_hour", start_min: "shift.start_min",
                      end_day: "shift.end_day", end_hour: "shift.end_hour", end_min: "shift.end_min" }}}],
        data: { then: "EDIT", and: [ "onShiftCopied", "edit" ]},
        conflict: { then: "EDIT", and: [{ pub: "warn", msg: "shift_conflict" }]},
        error: { then: "EDIT", and: [{ send: "moved", to: "$parent" }, "reset" ]},
        close: { then: "INIT", and: [ "reset", { send: "cancel", to: "$parent" }]},

      },

      ASSIGNING: {
        data: { then: "EDIT", and:[ "onShiftUpdated", "reset" ]},
        invalid: { then: "UPDATING", and: {pub: "warn" } },
        error: { then: "UPDATING", and: { pub: "error" } }
      },

      REMOVING: {
        remove: [
            { if: [{ eq: "mode", value: "template" }, { eq: "template_mode", value: "schedules"}], 
              then: "REMOVING", 
              and: { post: "remove.schedule.template.shift", body: { organization: "organization.id", template: "template.id", shift: "shift.id" }}},
            { if: [{ eq: "mode", value: "template" }, { eq: "template_mode", value: "availability"}], 
              then: "REMOVING", 
              and: { post: "remove.availability.template.shift", body: { organization: "organization.id", template: "template.id", shift: "shift.id" }}},
            { if: { eq: "mode", value: "schedule" }, then: "REMOVING", and: { post: "remove.schedule.shift",
              body: { organization: "shift.organization", schedule: "shift.schedule", shift: "shift.id" }}},
            { if: { eq: "mode", value: "availability" }, then: "REMOVING", and: { post: "remove.availability.shift",
              body: { id: "availability.id", shift: "shift.id" }}}
        ],
        data: { then: "INIT", and: [ {send: "removed", to: "$parent", data: "shift" }, {unset: ["shift"]} ] }
      }

    },

    ready: function() {
      this.setup();
    },

    methods: {
      
      getOrganization: function() {
        getOrganization( this, this.$data.template.organization, "data" );
      },
      
      getTemplate: function(){
        getTemplate( this, this.$data.schedule.organization, this.$data.schedule.template, "data" );
      },
      
      getMembers: function() {
        getUsers( this, this.$data.organization.id, "data" );
      },

      getTags: function() {
        getTags( this, this.$data.organization.id, "data" );
      },

      setup: function() {
        
        var self = this;

        $( "#start" ).timepicker({
          template: false,
          showInputs: false,
          minuteStep: 15,
          snapToStep: true,
          showMeridian: false
        });

        $( "#end" ).timepicker({
          template: false,
          showInputs: false,
          minuteStep: 15,
          snapToStep: true,
          showMeridian: false
        });

        $('#start').timepicker().on('changeTime.timepicker', function(e) {
          self.$receive( "start_time", { hour: e.time.hours, min: e.time.minutes });
        });

        $('#end').timepicker().on('changeTime.timepicker', function(e) {
          self.$receive( "end_time", { hour: e.time.hours, min: e.time.minutes });
        });

      },

      updateTimes: function() {
        this.updateTime( "#start", this.$data.shift.start_hour, this.$data.shift.start_min );
        this.updateTime( "#end", this.$data.shift.end_hour, this.$data.shift.end_min );
      },

      updateTime: function( id, hour, min ) { 
        $( id ).timepicker( 'setTime', this.moment_from( hour, min ).format( "HH:mm") );
      },

      setStartTime: function(msg) {
        this.$data.shift.start_hour = msg.hour;
        this.$data.shift.start_min = msg.min;
      },

      setEndTime: function(msg) {
        this.$data.shift.end_hour = msg.hour;
        this.$data.shift.end_min = msg.min;
      },

      moment_from: function( hour, min ) {
        var m = Vue.$date();
        m.set({ hour: hour, minutes: min, seconds: 0 });
        return m;
      },

      moment_to_minutes: function(m) {
        return (m.hour() * 60 + m.minutes());
      },

      is_moment_after: function( m1, m0 ) {
        return this.moment_to_minutes(m1) > this.moment_to_minutes(m0); 
      },

      checkTimes: function() {
        var minHour = Vue.$config.calendar.start_hour;
        var maxHour = Vue.$config.calendar.start_hour + Vue.$config.calendar.hours;
        var m0 = this.moment_from( minHour, 0 );
        var m1 = this.moment_from( this.$data.shift.start_hour, this.$data.shift.start_min );
        var m2 = this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min );
        var m3 = this.moment_from( maxHour, 0 );
        return m2.isAfter( m1 ) && !m1.isBefore( m0 ) && !m2.isAfter( m3 );
      },

      setShiftType: function( v ) {
        this.$data.shift.type = v;
      },

      reset: function() {
        this.$data.tags = [];
        this.$data.shift = {};
        this.$data.assignedto = "";
        this.$data.tag = "";
      },

      initShift: function(msg) {
        this.$data.shift = {
          start_day: msg.day,
          start_hour: msg.hour,
          start_min: msg.min,
          duration: 60,
          end_day: msg.day,
          end_hour: msg.hour + 1,
          end_min: msg.min,
          staffing: 1,
          type: "std",
          year: msg.year,
          month: msg.month + 1,
          day: msg.date
        };
      },

      moveShift: function(msg) {
        this.$data.shift = msg.item;
        var oldStartDay = this.$data.shift.start_day;
        this.$data.shift.start_day = msg.day;
        this.$data.shift.end_day = msg.day;
        var diff_hours = msg.time - this.$data.shift.start_hour ;
        this.$data.shift.start_hour = msg.time;
        this.$data.shift.end_hour += diff_hours;
        var maxHour = Vue.$config.calendar.start_hour + Vue.$config.calendar.hours;
        var m2 = this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min );
        var m3 = this.moment_from( maxHour, 0 );
        if (m2.isAfter( m3 )) {
          this.$data.shift.end_hour = maxHour;
          this.$data.shift.end_min = 0;
        }

        if( this.$data.mode === "schedule" || this.$data.mode === "availability" ) {
          var newStartDay = msg.day;
          if( newStartDay == 0 ) newStartDay = 7;
          if( oldStartDay === 0 ) oldStartDay = 7;
          var daysDiff = newStartDay - oldStartDay;
          this.moveShiftByNDays (daysDiff);
        }
      },

      moveShiftByNDays: function( n ) {
          var m = Vue.$date();
          m.set({ year: this.$data.shift.year, month: this.$data.shift.month, date: this.$data.shift.day });
          m.add( n, "days");
          this.$data.shift.year = m.year();
          this.$data.shift.month = m.month();
          this.$data.shift.day = m.date();
      },

      canMoveEarlier: function( msg ) {
        var minHour = Vue.$config.calendar.start_hour;
        var m0 = this.moment_from( minHour, 0 );
        var m1 = this.moment_from( msg.item.start_hour, msg.item.start_min );
        return this.is_moment_after( m1, m0 );
      },

      moveEarlier: function(msg) {
        this.$data.shift = msg.item;
        var minHour = Vue.$config.calendar.start_hour;
        var startInMins = 60 * this.$data.shift.start_hour + this.$data.shift.start_min;
        var m1 = this.moment_from( this.$data.shift.start_hour, this.$data.shift.start_min );
        var m2 = this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min );
        var m0 = this.moment_from( minHour, 0 );
        var diff = Math.min( 15, startInMins );
        m1.add( -1 * diff, 'minutes' );
        m2.add( -1 * diff, 'minutes' );
        this.$data.shift.start_hour = m1.hour();
        this.$data.shift.start_min = m1.minutes();
        this.$data.shift.end_hour = m2.hour();
        this.$data.shift.end_min = m2.minutes();
      },

      canMoveLater: function( msg) {
        var maxHour = Vue.$config.calendar.start_hour + Vue.$config.calendar.hours;
        var m1 = this.moment_from( msg.item.end_hour, msg.item.end_min );
        var m0 = this.moment_from( maxHour, 0 );
        return !this.is_moment_after( m1.add( 15, 'minutes'), m0 );
      },

      moveLater: function( msg ) {
        this.$data.shift = msg.item;
        var m1 = this.moment_from( this.$data.shift.start_hour, this.$data.shift.start_min );
        var m2 = this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min );
        m1.add( 15, 'minutes' );
        m2.add( 15, 'minutes' );
        this.$data.shift.start_hour = m1.hour();
        this.$data.shift.start_min = m1.minutes();
        this.$data.shift.end_hour = m2.hour();
        this.$data.shift.end_min = m2.minutes();
      },

      canEndEarlier: function( msg ) {
        var m1 = this.moment_from( msg.item.end_hour, msg.item.end_min );
        var m0 = this.moment_from( msg.item.start_hour, msg.item.start_min );
        return this.is_moment_after( m1.add( -15, 'minutes'), m0 ); 
      },

      endEarlier: function( msg ) {
        this.$data.shift = msg.item;
        var m1 = this.moment_from( this.$data.shift.start_hour, this.$data.shift.start_min );
        var m2 = this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min );
        m2.add( -15, 'minutes' );
        this.$data.shift.end_hour = m2.hour();
        this.$data.shift.end_min = m2.minutes();
      },

      canEndLater: function( msg ) {
        var maxHour = Vue.$config.calendar.start_hour + Vue.$config.calendar.hours;
        var m1 = this.moment_from( msg.item.end_hour, msg.item.end_min );
        var m0 = this.moment_from( maxHour, 0 );
        return this.is_moment_after( m0, m1 ); 
      },

      endLater: function( msg ) {
        this.$data.shift = msg.item;
        var maxHour = Vue.$config.calendar.start_hour + Vue.$config.calendar.hours;
        var minutesToEnd = maxHour*60 - (60 * this.$data.shift.end_hour + this.$data.shift.end_min);
        var m1 = this.moment_from( this.$data.shift.end_hour, this.$data.shift.end_min );
        var diff = Math.min ( 15, minutesToEnd );
        m1.add( diff, 'minutes' );
        this.$data.shift.end_hour = m1.hour();
        this.$data.shift.end_min = m1.minutes();
      },

      canMoveToNextDay: function(msg) {
        return msg.item.start_day != 0;
      },

      moveToNextDay: function(msg) {
        this.$data.shift = msg.item;
        switch( this.$data.shift.start_day ) {
          case 6: 
            this.$data.shift.start_day = 0;
            this.$data.shift.end_day = 0;
            break;
          default:
            this.$data.shift.start_day = this.$data.shift.start_day +1;
            this.$data.shift.end_day = this.$data.shift.end_day +1;
        }

        if( this.$data.mode === "schedule"  
          || this.$data.mode === "availability" )  this.moveShiftByNDays (1);
      },

      copyToNextDay: function(msg) {
        this.moveToNextDay( msg ); 
      },

      canMoveToPrevDay: function(msg) {
        return msg.item.start_day != 1;
      },

      moveToPrevDay: function(msg ){
        this.$data.shift = msg.item;
        switch( this.$data.shift.start_day ) {
          case 0: 
            this.$data.shift.start_day = 6;
            this.$data.shift.end_day = 6;
            break;
          default:
            this.$data.shift.start_day = this.$data.shift.start_day -1;
            this.$data.shift.end_day = this.$data.shift.end_day -1;
        }

        if( this.$data.mode === "schedule"  
          || this.$data.mode === "availability" )  this.moveShiftByNDays (-1);
      },
      
      copyToPrevDay: function(msg) {
        this.moveToPrevDay( msg ); 
      },
  
      keepShortcuts: function(msg) {
        msg.shift_key = this.$data.shift.shift_key;
        msg.alt_key = this.$data.shift.alt_key;
        this.$data.shift = msg;
      },

      onShiftMoved: function(msg){
        this.keepShortcuts(msg);
        this.$sendParent( "moved", this.$data.shift );
      },

      onShiftUpdated: function(msg) {
        this.keepShortcuts(msg);
        this.$sendParent( "updated", this.$data.shift );
      },

      onShiftCreated: function(msg) {
        this.keepShortcuts(msg);
        this.$sendParent( "created", this.$data.shift );
      },

      onShiftCopied: function(msg) {
        this.keepShortcuts(msg);
        this.$sendParent( "created", this.$data.shift );
      },

    },
    
    listen: {
      events: {
        $parent: { 
          template: "template", 
          schedule: "schedule",
          availability: "availability",
          access: "access",
          create: "create", 
          edit: "edit",
          move: "move",
          mode: "template_mode",
          clear: "clear",
          move_earlier: "move_earlier",
          move_later: "move_later",
          move_to_prev_day: "move_to_prev_day",
          move_to_next_day: "move_to_next_day",
          copy_to_prev_day: "copy_to_prev_day",
          copy_to_next_day: "copy_to_next_day",
          end_earlier: "end_earlier",
          end_later: "end_later"
        }
      }
    },

    hook: {
      $init: "init"
    }

  });


  Vue.$comp( "template-calendar", {
    data: { template: {}, organization: {}, mode: "" },
    computed: {
      readonly: function(){
        return !this.template.access || this.template.access != 'owner'
      }
    },

    states: {
      INIT: {
        init: { then: "INIT", and: [{send: "mode", to: "templates"}, {set: "template"}, "clearCalendar", {pub: "template"} ]},
        organization: { then: "INIT", and: [ {set: "organization" }, "fetch_shifts" ]},
        fetch_shifts: [
          { if: { eq: "mode", value: "availability" }, then: "FETCHING_SHIFTS", and: { 
            http: "get.availability.template.shifts", body: { organization: "organization.id", template: "template.id" }}},
          { then: "FETCHING_SHIFTS", and: { 
            http: "get.schedule.template.shifts", body: { organization: "organization.id", template: "template.id" }}}],
        mode: { then: "INIT", and: [{set: "mode"}, { pub: "mode" }]},
      },

      FETCHING_SHIFTS: {
        data: { then: "READY", and: "pubShifts" }
      },

      READY: {
        init: { then: "INIT", and: "init" },
        slot: { then: "READY", and: { send: "create", to: "$parent" }},
        shift: { then: "READY", and: "pubShift" },
        replace: { then: "READY", and: "replaceShift" },
        remove: { then: "READY", and: { pub: "delete" }},
        item_selected: { if: "canEdit", then: "READY", and: { send: "edit", to: "$parent" }},
        item_moved: { then: "READY", and: { send: "move", to: "$parent" }},
        item_moved_next_day: { then: "READY", and: { send: "move_next_day", to: "$parent" }}, 
        item_moved_prev_day: { then: "READY", and: { send: "move_prev_day", to: "$parent" }}, 
        item_copied_next_day: { then: "READY", and: { send: "copy_next_day", to: "$parent" }}, 
        item_copied_prev_day: { then: "READY", and: { send: "copy_prev_day", to: "$parent" }}, 
        item_moved_earlier: { then: "READY", and: { send: "move_earlier", to: "$parent" }}, 
        item_moved_later: { then: "READY", and: { send: "move_later", to: "$parent" }}, 
        item_ends_earlier: { then: "READY", and: { send: "end_earlier", to: "$parent" }}, 
        item_ends_later: { then: "READY", and: { send: "end_later", to: "$parent" }}, 
        mode: { then: "READY", and: [{set: "mode"}, { pub: "mode" }]}
      },

    },
    
    listen: {
      events: {
        $parent: { 
          template: "init", 
          organization: "organization", 
          shift: "shift", 
          replace: "replace",
          remove: "remove",
          mode: "mode"
        }
      }
    },

    methods: {

      canEdit: function() {
        return !this.readonly;
      },

      replaceShift: function( msg ) {
        msg.focus = true;
        this.$pub( "delete", msg );
        this.$pub( "item", msg );
      },
        
      pubShift: function(msg) {
        msg.focus = true;
        this.$pub( "item", msg );
      },

      pubShifts: function(msg) {
        var self =this;
        msg.map( function(s) { self.$pub( "item", s ) });
      },

      pubShiftDeleted: function(msg) {
        this.$pub( "delete", msg );
      },

      clearCalendar: function() {
        this.$pub( "clear", !this.readonly );
      }

    }

  });

  Vue.$comp( "template-controls", {
    states: {
      READY: {
        inspector: { then: "READY", and: { send:"inspector", to: "$parent" }},
        keyshortcuts: { then: "READY", and: { send:"keyshortcuts", to: "$parent" }}
      }
    }
  });

  Vue.$comp( "key-shortcuts", {
    many: true,
    data: { keys: [
      { key: 'arrow-up', hint: 'move_shift_earlier' },
      { key: 'arrow-down', hint: 'move_shift_later' },
      { key: 'arrow-up', hint: 'end_shift_earlier', modifier: "Shift" },
      { key: 'arrow-down', hint: 'end_shift_later', modifier: "Shift" },
      { key: 'arrow-left', hint: 'move_shift_to_prev_day' },
      { key: 'arrow-right', hint: 'move_shift_to_next_day' },
      { key: 'arrow-left', hint: 'copy_shift_to_prev_day', modifier: "Shift" },
      { key: 'arrow-right', hint: 'copy_shift_to_next_day', modifier: "Shift" },
    ]}
  });

  Vue.$comp( "template-shifts", { 
    data: { template: {}, organization: {}, members: [], keys: false, inspector: true},
    computed: {
      readonly: function() {
        return !this.template || !this.template.access || this.template.access != "owner";
      },

      access_info: function() {
        return this.readonly ?  "read_access" : "write_access"; 
      }
    },
    states: {
      INIT: {
        init: { then: "INIT", and: [ {send: "mode", to: "templates"}, {set: "template"}, {pub: "template"}, "fetch_organization" ]},
        fetch_organization: { then: "FETCHING_ORGANIZATION", and: "getOrganization" },
        fetch_members: { then: "FETCHING_MEMBERS", and: "getMembers" },
        mode: { then: "INIT", and: { pub: "mode" }}
      },
      
      FETCHING_ORGANIZATION: {
        data: { then: "INIT", and: [{set: "organization"}, {pub: "organization"},  "fetch_members"] },
        mode: { then: "FETCHING_ORGANIZATION", and: { pub: "mode" }}
      },

      FETCHING_MEMBERS: {
        data: { then: "READY", and: [{set: "members"}  ] },
        mode: { then: "FETCHING_MEMBERS", and: { pub: "mode" }}
      },

      READY: {
        init: { then: "INIT", and: "init" },
        create: { then: "READY", and: [ { pub: "create" }]},
        edit: { then: "READY", and: [ { pub: "edit" }]},
        move: { then: "READY", and: { pub: "move" }},
        move_earlier:  { then: "READY", and: { pub: "move_earlier" }},
        move_later:  { then: "READY", and: { pub: "move_later" }},
        start_earlier:  { then: "READY", and: { pub: "start_earlier" }},
        start_later:  { then: "READY", and: { pub: "start_later" }},
        end_earlier:  { then: "READY", and: { pub: "end_earlier" }},
        end_later:  { then: "READY", and: { pub: "end_later" }},
        move_next_day: { then: "READY", and: { pub: "move_to_next_day" } },
        move_prev_day: { then: "READY", and: { pub: "move_to_prev_day" } },
        copy_next_day: { then: "READY", and: { pub: "copy_to_next_day" } },
        copy_prev_day: { then: "READY", and: { pub: "copy_to_prev_day" } },
        moved: { then: "READY", and: { pub: "replace" }},
        updated: { then: "READY", and: { pub: "shift" }},
        removed: { then: "READY", and: { pub: "remove" }},
        created: { then: "READY", and: { pub: "shift" }},
        inspector: { then: "READY", and: "showInspector" },
        mode: { then: "READY", and: { pub: "mode" }},
        keys: { then: "READY", and: "showKeys" }
      },

    },

    methods: {
      getOrganization: function() {
        getOrganization( this, this.$data.template.organization, "data" );
      },
      
      getMembers: function() {
        getUsers( this, this.$data.template.organization, "data" );
      },
      
      showInspector: function() {
        this.$data.keys = false;
        this.$data.inspector = true;
      },

      showKeys: function() {
        this.$data.keys = true;
        this.$data.inspector = false;
      },

    },

    listen: {
      events: {
        $parent: { template: "init", mode: "mode" }
      },
    },
  });

  Vue.$comp( "template-info", { 
    data: { template: {}, organization: { }, tags: [], tag: "", owner: {}, creator: {}, new_owner: "", mode: "" },
    computed: {
      readonly: function() {
        return !this.template.access || this.template.access != 'owner';
      },

      is_schedules: function() {
        return this.mode === "schedules";
      },

      is_availability: function() {
        return this.mode === "availability";
      },

      category: function() {
        return this.mode + "_templates";
      }
      
    },

    ui: {
      change_name: { $DEFAULT: "change_name", CHANGING_NAME: "sending" },
      change_owner: { $DEFAULT: "change_owner", CHANGING_OWNER: "sending" },
    },
    
    states: {

      INIT: { 
        init: { then: "INIT", and: [{send: "mode", to: "templates" }, {set: "template"}, "fetch_organization" ]},
        fetch_organization: { then: "FETCHING_ORGANIZATION", and: "getOrganization" },
        fetch_owner: { then: "FETCHING_OWNER", and: "getOwner" },
        fetch_creator: { then: "FETCHING_CREATOR", and: "getCreator" },
        fetch_members: { then: "FETCHING_MEMBERS", and: "getMembers" },
        user_selected: { then: "INIT", and: { set: "new_owner" } },
        fetch_tags: { then: "FETCHING_TAGS", and: { http: "get.schedule.template.tags", 
          body: { organization: "organization.id", template: "template.id" } }},
        fetch_tag_suggestions: { then: "FETCHING_TAG_SUGGESTIONS", and: "getOrganizationTags" },
        mode: { then: "INIT", and: [{ set: "mode"}, {pub: "mode"}]}
      },

      FETCHING_ORGANIZATION: {
        data: { then: "INIT", and: [{set: "organization"}, "fetch_members"] },
        mode: { then: "FETCHING_ORGANIZATION", and: [{ set: "mode"}, {pub: "mode"}]}
      },

      FETCHING_MEMBERS: {
        data: { then: "INIT", and: [{pub: "users" }, "fetch_owner" ]},
        mode: { then: "FETCHING_MEMBERS", and: [{ set: "mode"}, {pub: "mode"}]}
      },

      FETCHING_OWNER: {
        data: { then: "INIT", and: [ {set: "owner" }, "fetch_creator" ]},
        user_selected: { then: "FETCHING_OWNER", and: { set: "new_owner" } },
        mode: { then: "FETCHING_OWNER", and: [{ set: "mode"}, {pub: "mode"}]}
      },

      FETCHING_CREATOR: {
        data: [
          { if: { eq: "mode", value: "schedules"}, then: "INIT", and: [{set: "creator" }, "fetch_tag_suggestions" ]},
          { if: { eq: "mode", value: "availability"}, then: "READY", and: {set: "creator" }}
        ],  
        user_selected: { then: "FETCHING_CREATOR", and: { set: "new_owner" } },
        mode: { then: "FETCHING_CREATOR", and: [{ set: "mode"}, {pub: "mode"}]}
      },

      FETCHING_TAG_SUGGESTIONS: {
        data: { then: "INIT" , and: [{ pub: "suggestions" }, "fetch_tags" ]},
      },

      FETCHING_TAGS: {
        data: { then: "READY", and: {set: "tags"}}
      },

      READY: {
        init: { then: "INIT", and: "init" },
        change_name: [
          { if: { eq: "mode", value: "schedules"}, then: "RENAMING", and: 
            { post: "rename.schedule.template", body: { organization: "organization.id", template: "template.id",  name: "template.name" } } },
          { if: { eq: "mode", value: "availability"}, then: "RENAMING", and: 
            { post: "rename.availability.template", body: { organization: "organization.id", template: "template.id",  name: "template.name" } } }],
        change_owner: [
          { if: { eq: "mode", value: "schedules"}, then: "CHANGING_OWNER", and: 
            { post: "set.schedule.template.owner", body: { organization: "organization.id", template: "template.id", owner: "new_owner" } } },
          { if: { eq: "mode", value: "availability"}, then: "CHANGING_OWNER", and: 
            { post: "set.availability.template.owner", body: { organization: "organization.id", template: "template.id", owner: "new_owner" } } }],
        user_selected: { then: "READY", and: { set: "new_owner" } },
        cancel: { then: "INIT", and: { send: "cancel", to: "$parent" }},
        tag_added: { then: "MODIFYING_TAGS", and: [{set: "tag"}, 
          { post: "add.schedule.template.tag", body: { organization: "organization.id", template: "template.id", tag: "tag" } }]},
        tag_removed: { then: "MODIFYING_TAGS", and: [{set: "tag"}, 
          { post: "remove.schedule.template.tag", body: { organization: "organization.id", template: "template.id", tag: "tag" } }]},
      },

      RENAMING: {
        data: { then: "READY", and: [{ set: "template" }, { send: "changed", to: "$parent" } ] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
        conflict: { then: "READY", and: {pub: "warn", msg: "template_name_conflict" }}
      },

      CHANGING_OWNER: {
        data: { then: "INIT", and: ["init", { send: "template", to: "$parent" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
        conflict: { then: "READY", and: { pub: "warn", msg: "template_name_conflict" }}
      }, 
      
      MODIFYING_TAGS: {
        data: { then: "INIT", and: "fetch_tags" },
        invalid: { then: "INIT", and: "fetch_tags" },
        error: { then: "INIT", and: "fetch_tags" },
        not_found: { then: "INIT", and: "fetch_tags" }
      },
    },

    listen: {
      events: {
        $parent: { template: "init", mode: "mode" }
      }
    },

    methods: {
      getOrganization: function() {
        getOrganization( this, this.$data.template.organization, "data" );
      },

      getOwner: function() {
        getProfile( this, this.$data.template.ownedby, "data" );
      },
      
      getCreator: function() {
        getProfile( this, this.$data.template.createdby, "data" );
      },

      getMembers: function() {
        getUsers( this, this.$data.template.organization, "data" );
      },

      getOrganizationTags: function() {
        getTags( this, this.$data.template.organization, "data" );
      }

    }

  });


  Vue.$comp( "template-duplicate", {
    data: { template: {}, organization: {}, mode: "", name: "" },
    ui: {
      submit: { $DEFAULT: "duplicate", CREATING: "sending" }
    },
    computed: {
      title: function() {
        return this.mode === "schedules" ? "schedule_template_duplicate" : "availability_template_duplicate";
      }
    },
    
    states: {
      INIT: {
        init: { then: "INIT", and: [{ set: "template" }, { send: "mode", to: "$parent" }, "getOrganization" ] },
        organization: { then: "READY", and: [{ set: "organization" }]},
        mode: { then: "INIT", and: { set: "mode" } },
      },

      READY: {
        init: { then: "INIT", and: "init" },
        cancel: { then: "INIT", and: [ "$reset", {send: "cancel", to: "$parent" }]},
        submit: [
          { if: { eq: "mode", value: "schedules" }, then: "SENDING", and: { post: "duplicate.schedule.template",
            body: { organization: "organization.id", template: "template.id", name: "name" } }},
          { if: { eq: "mode", value: "availability" }, then: "SENDING", and: { post: "duplicate.availability.template",
            body: { organization: "organization.id", template: "template.id", name: "name" } }}
        ],
        home: { then: "READY", and: "cancel" },
        mode: { then: "READY", and: { set: "mode" }},
      },

      SENDING: {
        data: { then: "INIT", and: [ "$reset", { send: "duplicated", to: "$parent" } ]},
        invalid: { then: "READY", and: { pub: "warn" }},
        conflict: { then: "READY", and: { pub: "warn", msg: "template_name_conflict"  }}
      }
    },
    
    listen: {
      events: {
        $parent: { template: "init", mode: "mode" }
      },
    },

    methods: {
      getOrganization: function() {
        getOrganization( this, this.$data.template.organization, "organization" );
      },

      updateCache: function(t) {
        updateTemplate(t);
      },

      $reset: function() {
        this.$data.organization = {};
        this.$data.template = {};
        this.$data.name = "";
      } 
    }

  });

  Vue.$comp( "template-edit", {
    data: { template: {}, organization: {}, mode: "" },
    computed: {
      detail_title: function() {
        return this.mode === "schedules" ? "schedule_template_detail" : "availability_template_detail";
      }
    },
    states: {
      INIT: {
        init: { then: "INIT", and: [{ send: "mode", to: "$parent" }, { set: "template" }, "getOrganization" ] },
        organization: { then: "SHIFTS", and: [{set: "organization" }, {pub: "organization"}, {pub: "template", data: "template" }]},
        shifts: { then: "SHIFTS" },
        info: { then: "INFO" },
        mode: { then: "INIT", and: [{set: "mode"}, {pub: "mode" }]},
      },

      SHIFTS: {
        init: { then: "INIT", and: "init" }, 
        info: { then: "INFO" },
        shifts: { then: "INIT", and: "shifts" },
        home: { then: "INIT", and: { send: "cancel", to: "$parent" }},
        mode: { then: "SHIFTS", and: [{set: "mode"}, { pub: "mode" }]}
      },

      INFO: {
        init: { then: "INIT", and: "init" },
        shifts: { then: "SHIFTS" },
        info: { then: "INIT", and: "info" },
        home: { then: "INIT", and: { send: "cancel", to: "$parent" }},
        template: { then: "INFO", and: [{ set: "template" }, {pub: "template"} ]},
        changed: { then: "INFO", and: [{ set: "template" }, "updateCache"] },
        mode: { then: "INFO", and: [{set: "mode"}, { pub: "mode" }]}
      }
    },

    listen: {
      events: {
        $parent: { template: "init", mode: "mode" }
      },
    },

    methods: {
      getOrganization: function() {
        getOrganization( this, this.$data.template.organization, "organization" );
      },

      updateCache: function(t) {
        updateTemplate(t);
      }
    }
            
  });

  Vue.$comp( "template-list-item", {
    props: [ "template" ], 
    many: true,
    data: { organization: {} },
    states: {
      INIT: {
        init: { then: "INIT", and: "getOrganization" },
        organization: { then: "READY", and: { set: "organization" } }
      },

      READY: {
        select: { then: "READY", and: { send: "selected", to: "$parent", args: "template" }},
        duplicate: { then: "READY", and: { send: "duplicate", to: "$parent", args: "template" }}
      }
    },

    hooks: {
      $init: "init"
    },

    methods: {
      getOrganization: function() {
        getOrganization( this, this.$data.template.organization, "organization" );
      }
    }
  });

  Vue.$comp( "templates-list", {
    data: { templates: []  },
    ui: {
      create: { SCHEDULES: "schedule_template_create", AVAILABILITY: "availability_template_create" }
    },
    computed: {
      no_data: function() {
        return !this.templates.length; 
      }
    },
    states: {
      INIT: {
        fetch: { then: "INIT", and: "schedules" },
        schedules: { then: "SCHEDULES", and: [{ send: "schedules", to: "$parent" }, "fetch" ]},
        availability: { then: "AVAILABILITY", and:[{ send: "availability", to: "$parent" }, "fetch" ]}
      },

      SCHEDULES: {
        fetch: { then: "SCHEDULES_FETCHING", and: { http: "get.all.schedule.templates"  }},
        create: { then: "SCHEDULES", and: { send: "create", to: "$parent" }},
        selected: { then: "SCHEDULES", and: { send: "selected", to: "$parent" }},
        duplicate: { then: "SCHEDULES", and: { send: "duplicate", to: "$parent" }},
        availability: { then: "AVAILABILITY", and:[{ send: "availability", to: "$parent" }, "fetch" ]}
      },

      SCHEDULES_FETCHING: {
        data:{ then: "SCHEDULES", and: { set: "templates" } } 
      },

      AVAILABILITY: {
        fetch: { then: "AVAILABILITY_FETCHING", and: { http: "get.all.availability.templates"  }},
        create: { then: "AVAILABILITY", and: { send: "create", to: "$parent" }},
        selected: { then: "AVAILABILITY", and: { send: "selected", to: "$parent" }},
        duplicate: { then: "AVAILABILITY", and: { send: "duplicate", to: "$parent" }},
        schedules: { then: "SCHEDULES", and: [{ send: "schedules", to: "$parent" }, "fetch" ]}
      },

      AVAILABILITY_FETCHING: {
        data:{ then: "AVAILABILITY", and: { set: "templates" } } 
      },

    },

    listen: {
      states: {
        $parent: { LIST: "fetch" }
      }
    },

    hooks: {
      $init: "schedules"
    },

    methods: {
      mock: function() {
        var self = this;
        setTimeout( function() { self.$receive( "data", [] ) ; }, 1000)
      } 
    }

  });
 

  Vue.$comp( "templates", {
    data: { mode: "" },

    ui: {
      content: {
        LIST: "templates-list",
        CREATE: "template-create",
        EDIT: "template-edit",
        DUPLICATE: "template-duplicate"
      }
    },

    states: {
      INIT: { 
        init: { then: "LIST" }
      },

      LIST: {
        init: { then: "INIT", and: "init" },
        create: { then: "CREATE" },
        selected: { then: "EDIT", and: { pub: "template" } },
        schedules: { then: "LIST", and: [{ set: { mode: { value: "schedules" }}}, "mode"]},
        availability: { then: "LIST", and: [{ set: { mode: { value: "availability" }}}, "mode"]},
        mode: { then: "LIST", and: { pub: "mode", data: "mode" }},
        duplicate: { then: "DUPLICATE", and: { pub: "template" }}
      },

      CREATE: {
        init: { then: "INIT", and: "init" },
        cancel: { then: "LIST" },
        created: { then: "EDIT", and: { pub: "template" }},
        mode: { then: "CREATE", and: { pub: "mode", data: "mode" }},
      },

      EDIT: {
        init: { then: "INIT", and: "init" },
        cancel: { then: "LIST" },
        mode: { then: "EDIT", and: { pub: "mode", data: "mode" }},
      },

      DUPLICATE: {
        init: { then: "INIT", and: "init" },
        cancel: { then: "LIST" },
        mode: { then: "DUPLICATE", and: { pub: "mode", data: "mode" }},
        duplicated: { then: "EDIT", and: { pub: "template" }}
      }

    },

    listen: {
      states: {
        $parent: { TEMPLATES: "init" },
      }
    },

    hooks: {
      $init: "init"
    }

  });

  Vue.$comp( "template-chooser", {
    many: true,
    data: { templates: [] },

    states: {
      INIT: {
        templates: { then: "READY", and: [ {set: "templates" }, "reset", "setup" ]}       
      },

      READY: {
        templates: { then: "INIT", and: "templates" },
        selected: { then: "READY", and: { send: "template_selected", to: "$parent" }},
        reset: { then: "READY", and: "reset" }
      },
    },

    methods: {

      displayText: function(t) {
        if( !t ) return;
        return "{0} ({1})"
          .$format([ t._name,  t.organization.name ]);
      },

      setup: function() {
        var self = this;
        var $target = $( this.$el ).find( " > .typeahead" );
        $target.typeahead( "destroy" );
        $target.typeahead({
          source: this.$data.templates,
          matcher: function(item) {
            var q = this.query.toLowerCase();
            return item._name.toLowerCase().$match( q ) ||
              item.organization.name.toLowerCase().$match( q );
          },

          updater: function(item) {
            self.$receive( "selected", {
              template: item.id,
              organization: item.organization.id
            });
            return self.displayText( item );
          },

          sorter: function(items) {
            return items;
          },

          highlighter: function(item) { 
            return "<strong>{0}</strong> <span class='text-muted'>{1}</span>"
              .$format([ item._name, item.organization.name ]);
          }
        });
      },

      reset: function() {
        $( this.$el ).find( " > .typeahead" ).val( '' );
      }

    },

    listen: { 
      events: {
        $parent: { refresh: "reset", templates: "templates" }
      }
    }
           
  });
  
  Vue.$comp( "week-chooser", {
    many: true,
    data: { weeks: [], week: {} },
    states: {
      INIT: {
        init: { then: "READY", and: [ "initWeeks", "setup", "setCurrentWeek" ] },
        week: { then: "INIT", and: [ { set: "week" }, "select" ]}
      },

      READY: {
        init: { then: "INIT", and: "init" },
        selected: { then: "READY", and: [ "setWeek", { send: "week", to: "$parent", data: "week" } ]},
        reset: { then: "READY", and: [ "setCurrentWeek", "render" ] },
        refresh: { then: "READY", and: "render" },
        week: { then: "READY", and: [ { set: "week" }, "select" ]}
      }
    },

    hooks: {
      $init: "init" 
    },

    listen: {
      events: {
        $parent: {
          refresh: "reset",
          week: "week"
        }
      }
    },

    methods: {
      initWeeks: function() {
        var weeks = [];
        var now = Vue.$week();
        weeks.push({ week: now.isoWeek(), year: now.year(), id: now.valueOf() });
        for( var i=1; i<11; i++ ){
          now.add(1, 'weeks' );
          weeks.push({ week: now.isoWeek(), year: now.year(), id: now.valueOf()  });
        }
        this.$data.weeks = weeks;
      },

      setCurrentWeek: function() {
        var now = Vue.$week();
        $( this.$el ).selectpicker( "val", now.valueOf() );
      },

      select: function() {
        var s = this.currentSelection();
        if( s ) {
           $( this.$el ).selectpicker( "val", s.id );  
        }
      },

      currentSelection: function() {
        var selectedWeek = this.$data.week;
        return this.$data.weeks.filter( function(w) {
          return w.week === selectedWeek.week && w.year === selectedWeek.year;
        })[0];
      },

      setWeek: function(msg){
        var m = moment( new Date( parseInt(msg) ) );
        this.$data.week = { year: m.year(), week: m.isoWeek() };
      },

      setup: function() {
        var self = this;
        var $el = $( this.$el );
        $el.selectpicker();
        $el.on( "change", function(e) {
          self.$receive( "selected", $el.selectpicker("val")  );    
        });
      },

      render: function() {
        var $el = $( this.$el );
        var self = this;
        this.$nextTick( function() {
          $el.selectpicker( 'refresh' );
          self.$receive( "selected", $el.selectpicker( "val" ) );
        });
      },

      clear: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", null ); 
      }

    }
  });

  Vue.$comp( "schedule-create", {
    data: { week: {}, weekTitle: "", template: null, organization: null },
    ui: {
      submit: { $DEFAULT: "create", CREATING: "sending" }
    },

    states: {
      INIT: { 
        init: { then: "INIT", and: [ "initWeek", "clear", "displayWeek", { pub: "refresh" }, "fetch_templates" ] },
        week: { then: "INIT", and: [ "printMsg", "setWeek", "displayWeek" ]},
        fetch_templates: { then: "FETCHING_TEMPLATES", and: { 
          http: "get.all.owned.schedule.templates" } }
      },

      FETCHING_TEMPLATES: { 
        data: { then: "READY", and: [ "setTemplates", { pub: "templates", data: "templates" } ]}

      },

      READY: { 
        init: { then: "INIT", and: [ "clear", "fetch_templates"] },
        home: { then: "READY", and: "cancel" },
        week: { then: "READY", and: [ "printMsg" , "setWeek", "displayWeek" ]},
        cancel: { then: "READY", and: { send: "cancel", to: "$parent" }},
        template_selected: { then: "READY", and: "setTemplate" },
        submit: { then: "CREATING", and: { post: "create.schedule", body: {
          organization: "organization", template: "template", week: "week.week", year: "week.year" }}}

      },

      CREATING: {
        data: { then: "INIT", and: [ "clear", { send: "created", to: "$parent" } ] },
        conflict: { then: "READY", and: { pub: "warn", msg: "schedule_conflict" }},
        invalid: { then: "READY", and: { pub: "warn"}},
        error: { then: "READY", and: { pub: "error" }}
      }

    },
   
    hooks: {
      $init: "init" 
    },

    listen: {
      states: {
        $parent: { CREATE: "init" }
      },

      events: {
        $parent: { week: "week" }
      }
    },

    methods: {
      
      initWeek: function() {
        var now = Vue.$date();
        this.$data.week = {
          year: now.year(),
          week: now.isoWeek()
        };
      },

      clear: function() {
        this.$data.template = null;
        this.$data.organization = null;
      },

      setTemplates: function(items){
        this.$data.templates = items.map( function(t){
          t._name = t.name;
          delete t.name;
          return t;
        });
      },

      setTemplate: function(msg) {
        this.$data.template = msg.template;
        this.$data.organization = msg.organization;
      },

      setWeek: function(msg) {
        this.$data.week = msg;
      },

      displayWeek: function() {
        var p = Vue.$date();
        p.set( { year: this.$data.week.year, isoWeek: this.$data.week.week });
        this.$data.weekTitle = Vue.$fmt_date( p, "date_fmt_woy" ); 
      }
    }
           
  });

  Vue.$comp( "schedule-calendar", {
    data: { schedule: {}, template: {}, organization: {} },
    computed: {
      readonly: function(){
        //var now = Vue.$date();
        //now_w = now.isoWeek();
        //now_y = now.year();
        //var is_past = (now_y + now_w) > (parseInt(this.schedule.year) + parseInt(this.schedule.week));
        
        
        return (!this.schedule.access || this.schedule.access != 'owner') 
          || this.schedule.auto === "true" ;
      },
    },

    states: {
      INIT: {
        init: { then: "INIT", and: [{set: "schedule"}, "clearCalendar", {pub: "schedule"}, "fetch_shifts" ]},
        organization: { then: "INIT", and: {set: "organization" }},
        template: {then: "INIT", and: {set: "template" }},
        fetch_shifts: { then: "FETCHING_SHIFTS", and: { 
          http: "get.schedule.shifts", body: { organization: "schedule.organization", schedule: "schedule.id" }}}
      },

      FETCHING_SHIFTS: {
        data: { then: "READY", and: "pubShifts" }
      },

      READY: {
        init: { then: "INIT", and: "init" },
        slot: { then: "READY", and: { send: "create", to: "$parent" }},
        shift: { then: "READY", and: "pubShift" },
        replace: { then: "READY", and: "replaceShift" },
        item_selected: { then: "READY", and: { send: "edit", to: "$parent" }},
        item_moved: { then: "READY", and: { send: "move", to: "$parent" }},
        removed: { then: "READY", and: { pub: "delete" }},
        item_moved_next_day: { then: "READY", and: { send: "move_next_day", to: "$parent" }}, 
        item_moved_prev_day: { then: "READY", and: { send: "move_prev_day", to: "$parent" }}, 
        item_copied_next_day: { then: "READY", and: { send: "copy_next_day", to: "$parent" }}, 
        item_copied_prev_day: { then: "READY", and: { send: "copy_prev_day", to: "$parent" }}, 
        item_moved_earlier: { then: "READY", and: { send: "move_earlier", to: "$parent" }}, 
        item_moved_later: { then: "READY", and: { send: "move_later", to: "$parent" }}, 
        item_ends_earlier: { then: "READY", and: { send: "end_earlier", to: "$parent" }}, 
        item_ends_later: { then: "READY", and: { send: "end_later", to: "$parent" }}, 
      },

    },
    
    listen: {
      events: {
        $parent: { 
          schedule: "init",
          template: "template", 
          organization: "organization", 
          shift: "shift", 
          replace: "replace",
          removed: "removed"
        }
      }
    },

    methods: {

      replaceShift: function( msg ) {
        msg.focus = true;
        this.$pub( "delete", msg );
        this.$pub( "item", msg );
      },
        
      pubShift: function(msg) {
        msg.focus = true;
        this.$pub( "item", msg );
      },

      pubShifts: function(msg) {
        var self =this;
        msg.map( function(s) { self.$pub( "item", s ) });
      },

      pubShiftDeleted: function(msg) {
        this.$pub( "delete", msg );
      },

      clearCalendar: function() {
        var editable = ! this.readonly;
        this.$pub( "clear", editable  );
        this.$pub( "week", { 
          year: this.$data.schedule.year, 
          week: this.$data.schedule.week
        });
      }

    }

  });

  Vue.$comp( "schedule-controls", {
    data: { schedule: {} },

    ui: {
      label: { 
        $DEFAULT: "schedule_auto_mode", 
        DISABLED_OFF: "schedule_auto_mode_off", 
        DISABLED_ON: "schedule_auto_mode_on" 
      },

      autoModeIcon: {
        ENABLED_ON: "lock",
        ENABLED_OFF: "unlock"
      },

      autoModeLabel: {
        ENABLED_ON: "schedule_auto_mode_on",
        ENABLED_OFF: "schedule_auto_mode_off"
      }
    },

    computed: {
      readonly: function(){
        return !this.schedule.access || this.schedule.access != 'owner'; 
      },

      can_publish: function() {
        return this.schedule.status === 'computed';
      }
    },

    states: {
      INIT: {
        schedule: { then: "INIT", and: [ {set: "schedule"}, "route" ]},
        route: [
          { if: [ "canToggle", {eq: "schedule.auto", value: "true" }], then: "ENABLED_ON" },
          { if: [ "canToggle", {eq: "schedule.auto", value: "false" }], then: "ENABLED_OFF" },
          { if: "canToggle", then: "ENABLED_OFF" },
          { if: {eq: "schedule.auto", value: "false" }, then: "DISABLED_OFF" },
          { if: {eq: "schedule.auto", value: "true" }, then: "DISABLED_ON" }
        ]
      },

      ENABLED_ON: {
        schedule: { then: "INIT", and: "schedule" },
        toggle: { then: "BUSY", and: { post: "set.schedule.auto.mode", 
          body: { organization: "schedule.organization", schedule: "schedule.id", auto: { value: "false" } }}},
        run: { then: "BUSY", and: { post: "run.schedule", body: { organization: "schedule.organization", schedule: "schedule.id" }}},
        publish: { then: "BUSY", and: { post: "publish.schedule", body: { organization: "schedule.organization", schedule: "schedule.id" }}},
        refresh: { then: "BUSY", and: { http: "get.schedule.by.id", body: { organization: "schedule.organization", schedule: "schedule.id" } }},
        inspector: { then: "ENABLED_ON", and: { send: "inspector", to: "$parent" }},
        print: { then: "ENABLED_ON", and: "print" }
      },

      ENABLED_OFF: {
        schedule: { then: "INIT", and: "schedule" },
        toggle: { then: "BUSY", and: { post: "set.schedule.auto.mode", 
          body: { organization: "schedule.organization", schedule: "schedule.id", auto: { value: "true" } }}},
        run: { then: "BUSY", and: { post: "run.schedule", body: { organization: "schedule.organization", schedule: "schedule.id" }}},
        publish: { then: "BUSY", and: { post: "publish.schedule", body: { organization: "schedule.organization", schedule: "schedule.id" }}},
        refresh: { then: "BUSY", and: { http: "get.schedule.by.id", body: { organization: "schedule.organization", schedule: "schedule.id" } }},
        inspector: { then: "ENABLED_OFF", and: { send: "inspector", to: "$parent" }},
        print: { then: "ENABLED_OFF", and: "print" }
      },

      DISABLED_ON: {
        schedule: { then: "INIT", and: "schedule" },
        print: { then: "DISABLED_ON", and: "print" }

      },

      DISABLED_OFF: {
        schedule: { then: "INIT", and: "schedule" },
        print: { then: "DISABLED_OFF", and: "print" }
      },

      BUSY: { 
        data: { then: "INIT", and: [ "schedule", { send: "schedule", to: "$parent" }]},
        forbidden: { then: "INIT", and: "route" }
      }

    },
    
    listen: {
      events: {
        $parent: { schedule: "schedule" }
      }
    },

    methods: {
      canToggle: function() {
        return this.$data.schedule.template &&
          this.$data.schedule.status != 'queued' &&
          this.$data.schedule.status != 'running';
      },

      print: function() {
        window.print();
      }
    }
  });


  
  Vue.$comp( "schedule-shifts", { 
    data: { schedule: {}, template: {}, organization: {}, keys: false, inspector: true},
    computed: {
      readonly: function() {
        return !this.schedule || this.schedule.auto === "true" || this.schedule.access != "owner";
      }
    },
    states: {
      INIT: {
        init: { then: "INIT", and: [ {set: "schedule"}, {pub: "schedule"} ]},
        organization: { then: "INIT", and: [{set: "organization"}, {pub: "organization"}]},
        template: { then: "INIT", and: [{set: "template"}, {pub: "template"}]},
        members: { then: "READY", and: [{pub: "members" }] }
      },
      
      READY: {
        init: { then: "INIT", and: "init" },
        create: { then: "READY", and: [ { pub: "create" }]},
        edit: { then: "READY", and: [ { pub: "edit" }]},
        move: { then: "READY", and: { pub: "move" }},
        moved: { then: "READY", and: { pub: "replace" }},
        removed: { then: "READY", and: { pub: "removed" }},
        created: { then: "READY", and: { pub: "shift" }},
        updated: { then: "READY", and: { pub: "shift" }},
        schedule: { then: "READY", and: [
          { set: "schedule" }, { pub:"schedule"}, { pub: "organization", data: "organization" }, { pub: "template", data: "template" },
          { send: "schedule", to: "$parent" }
        ]},
        refresh: { then: "READY", and: { http: "get.schedule.by.id", body: { organization: "schedule.organization", schedule: "schedule.id" }}},
        data: { then: "READY", and: "schedule" },
        inspector: { then: "READY", and: "showInspector" },
        keys: { then: "READY", and: "showKeys" },
        move_earlier:  { then: "READY", and: { pub: "move_earlier" }},
        move_later:  { then: "READY", and: { pub: "move_later" }},
        end_earlier:  { then: "READY", and: { pub: "end_earlier" }},
        end_later:  { then: "READY", and: { pub: "end_later" }},
        move_next_day: { then: "READY", and: { pub: "move_to_next_day" } },
        move_prev_day: { then: "READY", and: { pub: "move_to_prev_day" } },
        copy_next_day: { then: "READY", and: { pub: "copy_to_next_day" } },
        copy_prev_day: { then: "READY", and: { pub: "copy_to_prev_day" } },
      }

    },

    methods: {
      
      toggleInspector: function() {
        this.$data.inspector = !this.$data.inspector;
      },

      showInspector: function( ){
        this.$data.inspector = true;
        this.$data.keys = false;
      },

      showKeys: function() {
        this.$data.inspector = false;
        this.$data.keys = true;
      },


    },

    listen: {
      events: {
        $parent: { 
          schedule: "init", 
          organization: "organization", 
          template: "template",
          members: "members" 
        }
      },

      states: {
        $parent: {
          SHIFTS: "calendar"
        }
      }
    },
  });


  Vue.$comp( "schedule-participant", {
    props: [ "participant" ],
    data: { profile: {}, counters: [] },
    many: true,
    computed: {
      av: function() {
        return this.participant.av_time_w ? 
          parseInt( this.participant.av_time_w ) : 0;
      },

      std: function() {
        return this.participant.s_std_time_w ? 
          parseInt( this.participant.s_std_time_w ) : 0;
      },

      training: function() {
        return this.participant.s_training_time_w ? 
          parseInt( this.participant.s_training_time_w ) : 0;
      },

      idle: function() {
        return this.participant.s_idle_time_w ? 
          parseInt( this.participant.s_idle_time_w ) : 0;
      },

      extra: function() {
        return this.participant.s_extra_time_w ? 
          parseInt( this.participant.s_extra_time_w ) : 0;
      },

      total: function() {
        var non_av = this.std + this.training + this.idle + this.extra;
        return this.av > non_av ? this.av : non_av;
      },

      std_ratio: function() {
        return this.total ? Math.floor( this.std/this.total*100 ) : 0;
      },

      extra_ratio: function() {
        return this.total ? Math.floor( this.extra/this.total*100 ) : 0;
      },

      idle_ratio: function() {
        return this.total ? Math.floor( this.idle/this.total*100 ) : 0;
      },

      training_ratio: function() {
        return this.total ? Math.floor( this.training/this.total*100 ) : 0;
      },

      av_ratio: function() {
        return this.total ? Math.floor( this.av/this.total*100 ) : 0;
      }


    },
    states: {
      READY:{
        profile_data: { then: "READY", and: [{ set: "profile" }, "setCounters" ]},
      }
    },

    methods: {
      setCounters: function() {
        var self = this;
        this.$data.counters = 
          [ "s_std_time_w", "s_extra_time_w", "s_idle_time_w", "s_training_time_w", "av_time_w" ]
          .map( function(i) {
            var v = self.$data.participant[i] ? parseInt(self.$data.participant[i]) : 0;
            var c = { name: i, value: v, unit: "mins" };
            if ( c.value >= 60 ) { 
              c.value = c.value/60;
              c.unit = "hours"
            }
            return c; 
          });
      }
    }

  });


  Vue.$comp( "schedule-participants", {
    data: { participants: [] },
    states: {
      INIT: {
        schedule: { then: "READY", and: { set: "schedule" }} 
      },

      READY: {
        schedule: { then: "INIT", and: "schedule" },
        fetch: { then: "FETCHING", and: { 
          http: "get.schedule.participants", body: { schedule: "schedule.id" } } 
        }
      },

      FETCHING: {
        data: { then: "READY", and: { set: "participants" }}
      }

    },

    listen: {
      states: {
        $parent: { PARTICIPANTS: "fetch" }
      },

      events: {
        $parent: { schedule: "schedule" }
      }
    }

  });


  Vue.$comp( "schedule-info", { 
    data: { schedule: {}, template: {}, organization: {}, owner: {}, creator: {}, new_owner: "" },
    computed: {
      readonly: function() {
        return true;
        //return !this.schedule.access || this.schedule.access != 'owner';
      },

    },

    ui: {
      change_owner: { $DEFAULT: "change_owner", CHANGING_OWNER: "sending" },
    },
    
    states: {

      INIT: { 
        init: { then: "INIT", and: {set: "schedule"}},
        organization: { then: "INIT", and: {set: "organization" }},
        template: { then: "INIT", and: [ {set: "template" } ]},
        members: { then: "INIT", and: [ {pub: "users" }, "fetch_owner" ]},
        fetch_owner: { then: "FETCHING_OWNER", and: "getOwner" },
        fetch_creator: { then: "FETCHING_CREATOR", and: "getCreator" },
        user_selected: { then: "INIT", and: { set: "new_owner" } },
      },


      FETCHING_OWNER: {
        data: { then: "INIT", and: [ {set: "owner" }, "fetch_creator" ]},
        user_selected: { then: "FETCHING_OWNER", and: { set: "new_owner" } },
      },

      FETCHING_CREATOR: {
        data: {then: "READY", and: {set: "creator" }},
        user_selected: { then: "FETCHING_CREATOR", and: { set: "new_owner" } },
      },

      READY: {
        init: { then: "INIT", and: "init" },
        change_owner: { then: "CHANGING_OWNER", and: 
            { post: "set.schedule.owner", body: { organization: "organization.id", schedule: "schedule.id", owner: "new_owner" } } },
        user_selected: { then: "READY", and: { set: "new_owner" } },
        cancel: { then: "INIT", and: { send: "cancel", to: "$parent" }},
      },

      CHANGING_OWNER: {
        data: { then: "INIT", and: ["init", { send: "schedule", to: "$parent" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
      }, 
      
    },

    listen: {
      events: {
        $parent: { 
          schedule: "init", 
          organization: "organization", 
          template: "template",
          members: "members"
        }
      }
    },

    methods: {

      getOwner: function() {
        getProfile( this, this.$data.schedule.ownedby, "data" );
      },
      
      getCreator: function() {
        getProfile( this, this.$data.schedule.createdby, "data" );
      },
    }

  });


  var scheduleEventParseFn = function(e) {
    var res = { type: e.type, severity: e.severity, created: e.created };
    if( !e.info ) return res;
    var entries = e.info.split(",");
    entries.map( function(entry) {
      var parts = entry.split( ":" );
      var key = parts[0];
      var value = parts[1];
      res[ parts[0] ] = isNaN( value ) ? value: parseInt(value);
    });
    return res;
  };


  var dummyUser = { first: Vue.$i18n( "unavailable"), last: Vue.$i18n( "unavailable") };


  var scheduleEventDisplayFn = function(e) {
    var str = Vue.$i18n( "event_" + e.type );
    if( e.uid ) {
      var user = syncGetUser( e.uid );
      if( !user ) user =  dummyUser;
      if( e.hasOwnProperty("odh")){ 
        str = str.$format([ user.first, user.last, e.odh, e.ods, e.pdh, e.pds ]); 
      } else {
        str = str.$format([ user.first, user.last ]);
      }

    }

    return str;
  }

  Vue.filter( "scheduleEvent", function(e) {
    return scheduleEventDisplayFn( e );
  });

    
  Vue.$comp( "schedule-chart", {
    data: { constraints: [ "strong", "soft"], events: [] },
    states: {
      NODATA: {
        events: { then: "READY", and: "events" },
        schedule: { then: "NODATA", and: "reset" }
      },
      
      READY: {
        schedule: { then: "NODATA", and: "schedule" },
        render: { then: "READY", and: "render" },
        events: { then: "READY", and: "setEvents" },
        nodata: { then: "NODATA" }
      }

    },

    listen: {
      events: {
        $parent: { schedule: "schedule", events: "events" }
      },

      states: {
        "schedule-edit" : { SUMMARY: "render" } 
      }
    },
    
    methods: {
      reset: function() {
        this.$data.events = [];
        if( this.chart ) {
          this.chart.destroy();
          this.chart = null;
        }
      },

      setEvents: function(msg) {
        if( !msg.length ) return this.$receive( "nodata" );

        msg = msg.filter( function(e) {
          switch( e.type ){
            case "assigned":
            case "matched":
            case "swapped":
              return true;
            default: return false;
          }
        });

        this.$data.events = msg.map( scheduleEventParseFn );
        if( this.$data.events.length == 0 ) this.$receive( "nodata" );
      },

      render: function() {

        var $el = $( this.$el ).find( " > canvas" );
        if( !$el || !$el[0] ) return; 
          
        var ctx = $el[0].getContext("2d");
        if( !this.$data.events.length ) return;



        var lineData = {
          labels: [ Vue.$i18n( "event_created") ].concat( this.$data.events.map( function(e) { return Vue.$i18n( 'event_' + e.type ); } )),
          datasets: [
            {
              label: Vue.$i18n( "soft_contraints" ),
              fillColor: "rgba(220,220,220,0.5)",
              strokeColor: "rgba(220,220,220,1)",
              pointColor: "rgba(220,220,220,1)",
              pointStrokeColor: "#fff",
              pointHighlightFill: "#fff",
              pointHighlightStroke: "rgba(220,220,220,1)",
              data: [0].concat( this.$data.events.map( function(e) { return e.soft; } ))
            },
            {
              label: Vue.$i18n( "hard_contraints" ),
              fillColor: "rgba(26,179,148,0.5)",
              strokeColor: "rgba(26,179,148,0.7)",
              pointColor: "rgba(26,179,148,1)",
              pointStrokeColor: "#fff",
              pointHighlightFill: "#fff",
              pointHighlightStroke: "rgba(26,179,148,1)",
              data: [0].concat( this.$data.events.map( function(e) { return e.hard; } ) )
            }
          ]
        };

        var lineOptions = {
          scaleShowGridLines: true,
          scaleGridLineColor: "rgba(0,0,0,.05)",
          scaleGridLineWidth: 1,
          bezierCurve: true,
          bezierCurveTension: 0.4,
          pointDot: true,
          pointDotRadius: 4,
          pointDotStrokeWidth: 1,
          pointHitDetectionRadius: 20,
          datasetStroke: true,
          datasetStrokeWidth: 2,
          datasetFill: true,
          responsive: false,
        };
        
        if( this.chart ) this.chart.destroy();
        this.chart = new Chart(ctx).Line(lineData, lineOptions);
        

      }
    }
  });
  
  Vue.$comp( "schedule-stats", {
    data: { schedule: {} },
    computed: {
      q_staffing: function() {
        return this.getQuality( this.schedule.stats.staffing, this.schedule.stats.staffing_total ); 
      },
      
      q_hard: function() {
        return this.getQuality( this.schedule.stats.hard, this.schedule.stats.hard_total ); 
      },

      q_soft: function() {
        return this.getQuality( this.schedule.stats.soft, this.schedule.stats.soft_total ); 
      },

      has_staffing_constraints: function() {
        return this.getTotal( "staffing" );
      },

      has_strong_constraints: function() {
        return this.getTotal( "hard" );
      },

      has_soft_constraints: function() {
        return this.getTotal( "soft" );
      }
    },
    
    states: {
      READY: {
        schedule: { then: "READY", and: [{ set: "schedule"}, { pub: "schedule" }]}
      }
    },

    listen: {
      events: {
        $parent: { schedule: "schedule" }
      }
    },

    methods: {
      
      getTotal: function(s) {
        return parseInt( this.$data.schedule.stats[ s + '_total' ]);
      },

      getQuality: function( achieved, total ) {
        achieved = parseInt( achieved );
        total = parseInt( total );
        if( !achieved || !total ) return 0;
        return Math.floor( parseInt( achieved ) / parseInt( total ) * 100 );
      }
    }
  });

  Vue.$comp( "schedule-log", {
    data: { schedule: {}, events: {}, start: 0 },
    states: {
      NODATA: {
        schedule: { then: "NODATA", and: [{ set: "schedule"}, { pub: "schedule" }]},
        events: { then: "READY", and: "setEvents" }
      },

      READY: {
        schedule: { then: "NODATA", and: "schedule" },
        nodata: { then: "NODATA" }
      }
    },

    listen: {
      events: {
        $parent: { schedule: "schedule", events: "events" }
      }
    },

    methods: {
      
      setEvents: function(msg) {
        if( !msg.length ) return this.$receive( "nodata" );  
        var origin = msg[0].created;
        this.$data.start = origin;
        this.$data.events = msg.map( function(e) {
          e = scheduleEventParseFn(e);
          e.elapsed = parseFloat((e.created - origin)/1000000).toFixed(2);
          return e;
        });
      
        if( !this.$data.events.length ) this.$receive( "nodata" );
      
      }

    }
           
  });

  Vue.$comp( "schedule-constraint-list-item", {
    many: true,
    props: [ "constraint" ]

  });

  Vue.$comp( "schedule-constraints", {
    data: { schedule: {}, constraints: [], verified: false },
    computed: {
      isVerified: function(){
        return this.verified;
      },
      
      isEmpty: function() {
        return !this.constraints.length;
      },

      buttonLabel: function() {
        return this.isVerified ? "verified" : "non_verified";
      },

      buttonIcon: function() {
        return this.isVerified ? "check-square-o" : "exclamation-triangle";
      }

    },
    states: {
      INIT: {
        schedule: { then: "INIT", and: [ "reset", { set: "schedule" }, "fetch_indicators"] },
        fetch_constraints: { then: "FETCHING_CONSTRAINTS", and: { http: "get.schedule.constraints", body: { 
          organization: "schedule.organization", schedule: "schedule.id", verified: "verified" }}},
        fetch_indicators: { then: "FETCHING_INDICATORS", and: "getIndicators" }
      },

      FETCHING_INDICATORS: {
        data: { then: "INIT", and: "fetch_constraints" } 
      },

      FETCHING_CONSTRAINTS: {
        data: { then: "READY", and: { set: "constraints" } }
      },

      READY: {
        schedule: { then: "INIT", and: "schedule" },
        toggle: { then: "INIT", and: [ "toggle", "fetch_constraints" ] },
      }

    },

    listen: {
      events: {
        $parent: { schedule: "schedule" }
      }
    },

    methods: {
      
      reset: function() {
        this.$data.verified = false;
      },

      toggle: function() {
        this.$data.verified = !this.$data.verified;
      },

      getIndicators: function(){
        getIndicators( this, this.$data.schedule.organization, "data" );
      },

      mock: function() {
        this.$receive( "data", [
          { id: 1, strong: true, indicator: "166e6ca8-71f0-4f95-b6e8-097a87ec9a9b", reference: "10", value: "3", user: "ebee6e1b-e7b1-497c-8072-c4d96702c954"},
          { id: 2, strong: false, indicator: "65f0e7a7-fcf3-4651-a0ee-3049e2baadce", reference: "15", value: "35", user: "1abbd745-17be-4e94-ba9d-de2bc38eee58" },
        ]);
      }
    }
  });

  Vue.$comp( "schedule-summary", {
    data: { schedule: {} },
    states: {
      READY: {
        schedule: { then: "READY", and: [{ set: "schedule"}, { pub: "schedule" }, "fetch_events"]},
        fetch_events: { then: "READY", and: { http: "get.schedule.events", body: { organization: "schedule.organization", schedule: "schedule.id"}}},
        data: { then: "READY", and: { pub: "events" }}
      }
    },

    listen: {
      events: {
        $parent: { schedule: "schedule" }
      }
    }
  });

  Vue.$comp( "schedule-edit", {
    data: { schedule: {}, organization: {}, template: {} },

    states: {
      INIT: {
        init: { then: "INIT", and: [{ set: "schedule" }, {pub: "schedule"}, "fetch_organization" ] },
        fetch_organization: { then: "FETCHING_ORGANIZATION", and: "getOrganization" },
        fetch_template: { then: "FETCHING_TEMPLATE", and: "getTemplate" },
        fetch_members: { then: "FETCHING_MEMBERS", and: "getMembers" },
        shifts: { then: "SHIFTS" },
        info: { then: "INFO" },
        summary: { then: "SUMMARY" },
        participants: { then: "PARTICIPANTS" }
      },

      FETCHING_ORGANIZATION: {
        data: { then: "INIT", and: [{set: "organization"}, { pub: "organization" }, "fetch_template" ]}
      },

      FETCHING_TEMPLATE: {
        data: { then: "INIT", and: [{set: "template" }, { pub: "template" }, "fetch_members" ]}
      },

      FETCHING_MEMBERS: {
        data: { then: "INIT", and: [{pub: "members" }, "shifts" ]}
      },

      SHIFTS: {
        init: { then: "INIT", and: "init" }, 
        info: { then: "INFO" },
        summary: { then: "SUMMARY" },
        shifts: { then: "INIT", and: "shifts" },
        home: { then: "INIT", and: { send: "cancel", to: "$parent" }},
        schedule: { then: "INIT", and: "init" },
        participants: { then: "PARTICIPANTS" }
      },

      SUMMARY: { 
        init: { then: "INIT", and: "init" }, 
        info: { then: "INFO" },
        summary: { then: "INIT", and: "summary" },
        shifts: { then: "SHIFTS" },
        home: { then: "INIT", and: { send: "cancel", to: "$parent" }},
        schedule: { then: "INIT", and: "init" }, 
        participants: { then: "PARTICIPANTS" }
      },
      
      PARTICIPANTS: { 
        init: { then: "INIT", and: "init" }, 
        info: { then: "INFO" },
        summary: { then: "SUMMARY" },
        shifts: { then: "SHIFTS" },
        home: { then: "INIT", and: { send: "cancel", to: "$parent" }},
        schedule: { then: "INIT", and: "init" },
        participants: { then: "INIT", and: "participants" }
      },

      INFO: {
        init: { then: "INIT", and: "init" },
        shifts: { then: "SHIFTS" },
        summary: { then: "SUMMARY" },
        info: { then: "INIT", and: "info" },
        home: { then: "INIT", and: { send: "cancel", to: "$parent" }},
        schedule: { then: "INIT", and: "init" },
        participants: { then: "PARTICIPANTS" }
      }
    },

    listen: {
      events: {
        $parent: { schedule: "init" }
      }
    },

    methods: {
      getOrganization: function() {
        getOrganization( this, this.$data.schedule.organization, "data" );
      },

      getTemplate: function(){
        getTemplate( this, this.$data.schedule.organization, this.$data.schedule.template, "data" );
      },
      
      getMembers: function() {
        getUsers( this, this.$data.schedule.organization, "data" );
      },
    }
            
  });

  Vue.$comp( "schedule-list-item", {
    many: true,
    props: [ "schedule" ], 
    data: { organization: {}, template: {} },
    computed: {
      readonly: function(){
        return !this.schedule.access || this.schedule.access != 'owner'; 
      },

      autoIcon: function() {
        return this.schedule.auto === "true" ? "lock" : "unlock";
      } 
    },

    states: {
      READY: {
        init: { then: "READY", and: [ "getOrganization", "getTemplate" ]},
        organization: { then: "READY", and: { set: "organization" } },
        template: { then: "READY", and: { set: "template" }},
        select: { then: "READY", and: { send: "selected", to: "$parent", data: "schedule" }},
        run: { then: "RUNNING", and: { post: "run.schedule", body: { organization: "schedule.organization", schedule: "schedule.id" }}},
        publish: { then: "PUBLISHING", and: { post: "publish.schedule", body: { organization: "schedule.organization", schedule: "schedule.id" }}},
      },

      RUNNING: {
        data: { then: "READY", and: { send: "running", to: "$parent" }}
      },

      PUBLISHING: {
        data: { then: "READY", and: { send: "published", to: "$parent" }}
      }


    },

    hooks: {
      $init: "init"
    },

    methods: {
      getOrganization: function() {
        getOrganization( this, this.$data.schedule.organization, "organization" );
      },

      getTemplate: function() {
        getTemplate( this, this.$data.schedule.organization, this.$data.schedule.template, "template" );
      }
    }
  });


  
  Vue.$comp( "schedules-list", {
    data: { schedules: [], mode: "w", mine: true, year: 0, week: 0, month: 0 },
    computed: {
      has_schedules: function() {
        return this.schedules.length;
      }
    },
    states: {
      INIT: {
        init: { then: "INIT", and: [ "initWeek" ] },
        week: { then: "FETCHING", and: [ {unset: ["schedules"]}, "setWeek", "setPeriod", "notifyPeriod", "fetch" ]},
        month: { then: "FETCHING", and: [ {unset: ["schedules"]}, "setMonth", "setPeriod", "notifyPeriod", "fetch" ]},
        fetch: { then: "FETCHING", and: [ {unset: ["schedules"]}, "fetch"] }
      },

      FETCHING: {
        fetch: [
          { if: [ "isMine", "isMonth" ], then: "FETCHING", and: { http: "get.my.schedules.by.month", body: { year: "year", month: "month" } } },
          { if: [ "isMine", "isWeek" ], then: "FETCHING", and: { http: "get.my.schedules.by.week", body: { year: "year", week: "week" } } },
          { if: [ "isMonth" ], then: "FETCHING", and: { http: "get.schedules.by.month", body: { year: "year", month: "month" } } },
          { then: "FETCHING", and: { http: "get.schedules.by.week", body: { year: "year", week: "week" } } },
        ],
        data: { then: "READY", and: {set: "schedules"}}
      },
      
      READY: {
        create: { then: "READY", and: { send: "create", to: "$parent" }},
        selected: { then: "READY", and: { send: "selected", to: "$parent" }},
        running: { then: "READY", and: ["init", { pub: "running" }] },
        published: { then: "READY", and: [ "init", { pub: "published" }] },
        week: { then: "INIT", and: "week" },
        month: { then: "INIT", and: "month" },
        mine: { then: "INIT", and: [ "setMine", "fetch" ]},
        all: { then: "INIT", and: [ "setAll", "fetch" ]},
        fetch: { then: "INIT", and: "fetch" }
      }
    },

    listen: {
      states: {
        $parent: { LIST: "fetch" }
      }
    },

    hooks: {
      $init: "init"
    },

    methods: {
      setPeriod: function( msg ) {
        msg = Vue.$date( msg );
        this.$data.year = msg.year();
        this.$data.month = msg.month()+1;
        this.$data.week = msg.isoWeek();
      },

      notifyPeriod: function(msg) {
        this.$sendParent( "week", { 
          year: this.$data.year,
          week: this.$data.week
        });
      },
      
      initWeek: function() {
        var now = Vue.$date();
        this.$data.year = now.year();
        this.$data.week = now.isoWeek();
        this.$data.month = now.year() + 1;
      },

      setWeek: function() {
        this.$data.mode = "w";
      },

      setMonth: function() {
        this.$data.mode = "m";
      },

      setMine: function() {
        this.$data.mine = true;
      },

      setAll: function() {
        this.$data.mine = false;
      },

      isWeek: function() {
        return this.$data.mode === "w";
      },

      isMonth: function() {
        return this.$data.mode === "m";
      },

      isMine: function() {
        return this.$data.mine;
      }

    }


  });
 
  Vue.$comp( "schedules", {
    ui: {
      content: {
        LIST: "schedules-list",
        CREATE: "schedule-create",
        EDIT: "schedule-edit"
      }
    },

    states: {
      INIT: { 
        init: { then: "LIST" }
      },

      LIST: {
        init: { then: "INIT", and: "init" },
        selected: { then: "EDIT", and: { pub: "schedule" }},
        create: { then: "CREATE" },
        week: { then: "LIST", and: { pub: "week" } }
      },

      CREATE: {
        init: { then: "INIT", and: "init" },
        cancel: { then: "LIST" },
        created: { then: "INIT", and: "init" }
      },

      EDIT: {
        init: { then: "INIT", and: "init" },
        cancel: { then: "INIT", and: "init" }
      }

    },

    listen: {
      states: {
        $parent: { SCHEDULES: "init" }
      }
    },

    hooks: {
      $init: "init"
    }

  }); 

  Vue.$comp( "notifications-nav-item", {
    data: { stats: {} },
    computed: {
      unread: function() {
        return parseInt(this.stats.notifications_unread);
      }
    },
    states: {
      READY: {
        refresh: { then: "READY", and: "refreshStats" },
        data: { then: "READY", and: { set: "stats" }}, 
        notifications: { then: "READY", and: { send: "notifications", to: "main" } }
      },
    },

    hooks: {
      $init: "refresh" 
    },

    listen: {
      events: {
        notifications: { refresh: "refresh" },
        "schedules-list" : { published: "refresh", running: "refresh" }
      },

      states: {
        app: { SIGNED_IN: "refresh" }
      }
    },

    methods: {
      refreshStats: function(){
        getStats( this, "data", true );
      },

      $reset: function() {
        this.$data.stats = {};
      }
    }

  });

  var NOTIFICATION_ICONS = {
    membership_created: "sitemap",
    membership_removed: "sitemap",
    shift_assigned: "calendar",
    shift_unassigned: "trash-o",
    shift_modified: "calendar",
    shift_conflict: "exclamation-triangle"
  }

  var shiftStart = function(shift){
    var d = Vue.$date();
    d.set({ year: parseInt(shift.year), month: parseInt(shift.month)-1, date: parseInt(shift.day) });
    d.set({ hour: parseInt(shift.start_hour), minutes: parseInt( shift.start_min) });
    return d;
  }

  var shiftEnd = function(shift){
    var d = Vue.$date();
    d.set({ year: parseInt(shift.year), month: parseInt(shift.month)-1, date: parseInt(shift.day) });
    d.set({ hour: parseInt(shift.end_hour), minutes: parseInt( shift.end_min) });
    return d;
  }

  Vue.$comp( "notification-list-item", {
    many: true,
    props: [ "notification" ],
    computed: {
      icon: function() {
        return NOTIFICATION_ICONS[this.notification.type];
      },

      summary: function() {
        var str = Vue.$i18n( "notifications_" + this.notification.type );
        switch( this.notification.type ) {
          case "membership_created":
          case "membership_removed":
            return str.$format([ this.notification.organization.name ]);
          default:
            var shift = this.notification.shift;
            var start = shiftStart( shift );
            var end = shiftEnd( shift );
            return str.$format([ 
              Vue.$fmt_date( start, "date_fmt_dow" ),
              Vue.$fmt_date( start, "date_fmt_day" ),
              Vue.$fmt_date( start, "date_fmt_time" ),
              Vue.$fmt_date( end, "date_fmt_time" ),
            ]);
        }
      },

      style: function() {
        return this.notification.status === 'unread' ? 'bold' : 'muted';
      }
    },
    states: {
      INIT: {
        init: [
          { if: { eq: "notification.status", value: "unread" }, then: "UNREAD" },
          { then: "READ" }
        ]
      },

      UNREAD: {
        read: { then: "UNREAD", and: { post: "read.notification", body: { id: "notification.id" } } },
        data: { then: "READ", and: { send: "read", to: "$parent" } }
      },

      READ: {
        unread: { then: "READ", and: { post: "unread.notification", body: { id: "notification.id" } } },
        data: { then: "UNREAD", and: { send: "unread", to: "$parent"} }
      }
    },

    hooks: {
      $init: "init"
    }

  });

  Vue.$comp( "notifications", {
    data: { notes: [] },
    ui: {
      toggle: { $DEFAULT: "show_read", READ: "show_unread" },
      purge: { $DEFAULT: "purge_read", PURGING: "sending" }
    },
    states: {
      INIT: {
        init: { then: "UNREAD", and: "fetch" }
      },

      UNREAD: {
        init: { then: "INIT", and: "init" },
        home: { then: "UNREAD", and: { send: "home", to: "sidebar" }},
        clear: { then: "UNREAD", and: { unset: [ "notes" ]}},
        fetch: { then: "UNREAD", and: { http: "get.unread.notifications" }},
        mark_all_read: { then: "UNREAD_READING", and: { post: "read.all.notifications" } },
        toggle: { then: "READ", and: [ "clear", "fetch" ] },
        data: { then: "UNREAD", and: [{set: "notes"}, { pub: "refresh" }] },
        read: { then: "UNREAD", and: [ "clear", "fetch" ] },
        unread: { then: "UNREAD", and: [ "clear",  "fetch" ] }
      },

      READ: {
        init: { then: "INIT", and: "init" },
        home: { then: "READ", and: { send: "home", to: "sidebar" }},
        clear: { then: "READ", and: { unset: [ "notes" ]}},
        fetch: { then: "READ", and: { http: "get.read.notifications" }},
        data: { then: "READ", and: [{set: "notes"}, { pub: "refresh" } ] },
        toggle: { then: "UNREAD", and: [ "clear", "fetch" ] },
        mark_all_read: { then: "ALL_READING", and: { post: "read.all.notifications" } },
        purge: { then: "PURGING", and: { post: "purge.read.notifications" }},
        read: { then: "READ", and: [ "clear",  "fetch" ]},
        unread: { then: "READ", and: [ "clear",  "fetch" ]}
      },

      UNREAD_READING: {
        data: { then: "UNREAD", and: [ "clear", "fetch" ] },
      },
      
      READ_READING: {
        data: { then: "READ", and: [ "clear", "fetch" ] },
      },

      PURGING: {
        data: { then: "READ", and: "fetch" }
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      states: {
        main: { NOTIFICATIONS: "init" }
      }
    },

    methods: {
      $reset: function() {
        this.$data.notes = [];
      }
    }
  });
  
  


  Vue.$comp( "sidebar", {
    data: { acls: [], photo:"" },
    computed: {

      has_organizations: function() {
        var acl = this.acls.$findUsing( "name", "organizations");
        return (acl && acl.left > 0);
      },

      has_schedules: function() {
        var acl = this.acls.$findUsing( "name", "schedules");
        return (acl && acl.left > 0);
      },

      has_templates: function() {
        var acl = this.acls.$findUsing( "name", "schedules");
        return (acl && acl.left > 0);
      },

      has_issues: function() {
        var acl = this.acls.$findUsing( "name", "issues");
        return (acl && acl.left > 0);
      },

      has_dashboard: function() {
        var acl = this.acls.$findUsing( "name", "dashboard");
        return (acl && acl.left > 0);
      }

    },

    states: {

      INIT: {
        init: { then: "INIT", and: "getAcls" },
        error: { then: "INIT", and: { send: "error", to: "app" }},
        acls: { then: "INIT", and: [{ set: "acls" }, "getMyPhoto" ] },
        photo: { then: "PROFILE", and: [{ set: "photo" }] },
        schedules: { then: "SCHEDULES" },
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        home: { then: "HOME" },
        profile: { then: "PROFILE" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      PROFILE: {
        schedules: { then: "SCHEDULES" },
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        profile: { then: "INIT", and: "profile" },
        home: { then: "INIT", and: "home" },
        issues: { then: "ISSUES" },
        photo_changed: { then: "PROFILE", and: "getMyPhoto" },
        photo: { then: "PROFILE", and: { set: "photo" } },
        dashboard: { then: "DASHBOARD" }
      },

      HOME: {
        schedules: { then: "SCHEDULES" },
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        profile: { then: "PROFILE" },
        home: { then: "INIT", and: "home" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      ORGANIZATIONS: {
        templates: { then: "TEMPLATES" },
        schedules: { then: "SCHEDULES"  },
        profile: { then: "PROFILE" },
        organizations: {  then: "INIT", and: "organizations" },
        home: { then: "HOME" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      TEMPLATES: {
        organizations:{ then: "ORGANIZATIONS" },
        templates: { then: "INIT", and: "templates" },
        schedules: { then: "SCHEDULES" },
        home: { then: "HOME" },
        profile: { then: "PROFILE" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      SCHEDULES: {
        organizations:{ then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "INIT", and: "schedules" },
        home: { then: "HOME" },
        profile: { then: "PROFILE" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      ISSUES: {
        organizations:{ then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "SCHEDULES" },
        home: { then: "HOME" },
        profile: { then: "PROFILE" },
        issues: { then: "INIT", and: "issues" },
        dashboard: { then: "DASHBOARD" }
      },
      
      DASHBOARD: {
        organizations:{ then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "SCHEDULES" },
        home: { then: "HOME" },
        profile: { then: "PROFILE" },
        dashboard: { then: "INIT", and: "dashboard" },
        issues: { then: "ISSUES" }
      },


    },

    hooks: {
      $init: "init"
    },


    methods: {

      getAcls: function() {
        getAcls( this, "acls" );
      },

      getMyPhoto: function(){
        getMyPhoto( this, "photo" );
      }
    },

    listen: {
      states: {
        app: { SIGNED_IN: "init" }
      },

      events: {
        "profile-photo" : { changed: "photo_changed" }
      }
    }

  });

  Vue.$comp( "sidebar-status", {
    ui: {
      icon: { 
        EXPANDED: "fa-caret-square-o-left", 
        COLLAPSED: "fa-caret-square-o-right" 
      },

      title: {
        EXPANDED: "collapse_sidebar", 
        COLLAPSED: "expand_sidebar" 
      }
    },

    states: {
      INIT: {
        init: { then: "COLLAPSED", and: "collapse" },
      },
      
      COLLAPSED: {
        toggle: { then: "EXPANDED", and: "expand" }
      },

      EXPANDED: {
        toggle: { then: "COLLAPSED", and: "collapse" }
      }
    },

    hooks: {
      $init: "init"
    },

    methods: {
      collapse: function(){
        $("body").addClass('mini-navbar');
      },

      expand: function() {
        $("body").removeClass('mini-navbar');
      }
    },

    listen: {
      states: {
        app: { SIGNED_IN: "init" }
      }
    }

  });

  Vue.$comp( "main", {
    ui: {
      content: "$state"
    },

    states: {
      INIT: {
        profile: { then: "PROFILE" },
        organizations: { then: "ORGANIZATIONS" },
        schedules: { then: "SCHEDULES" },
        templates: { then: "TEMPLATES" },
        home: { then: "HOME" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      HOME: {
        profile: { then: "PROFILE", and: { pub: "profile" } },
        organizations: { then: "ORGANIZATIONS" },
        schedules: { then: "SCHEDULES" },
        templates: { then: "TEMPLATES" },
        home: { then: "INIT", and: "home" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      PROFILE: {
        profile: { then: "PROFILE", and: { pub: "profile" } },
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "SCHEDULES" },
        home: { then: "HOME" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      ORGANIZATIONS: {
        profile: { then: "PROFILE", and: { pub: "profile" } },
        schedules: { then: "SCHEDULES" },
        templates: { then: "TEMPLATES" },
        organizations: { then: "INIT", and: "organizations" },
        home: { then: "HOME" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      TEMPLATES: {
        profile: { then: "PROFILE", and: { pub: "profile" } },
        schedules: { then: "SCHEDULES" },
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "INIT", and: "templates" },
        home: { then: "HOME" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },
      
      SCHEDULES: {
        profile: { then: "PROFILE", and: { pub: "profile" } },
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "INIT", and: "schedules" },
        home: { then: "HOME" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      NOTIFICATIONS: {
        profile: { then: "PROFILE", and: { pub: "profile" }},
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "SCHEDULES" },
        home: { then: "HOME" },
        notifications: { then: "INIT", and: "notifications" },
        issues: { then: "ISSUES" },
        dashboard: { then: "DASHBOARD" }
      },

      ISSUES: {
        profile: { then: "PROFILE", and: { pub: "profile" }},
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "SCHEDULES" },
        home: { then: "HOME" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "INIT", and: "issues" },
        dashboard: { then: "DASHBOARD" }
      },

      DASHBOARD: {
        profile: { then: "PROFILE", and: { pub: "profile" }},
        organizations: { then: "ORGANIZATIONS" },
        templates: { then: "TEMPLATES" },
        schedules: { then: "SCHEDULES" },
        home: { then: "HOME" },
        notifications: { then: "NOTIFICATIONS" },
        issues: { then: "ISSUES" },
        dashboard: { then: "INIT", and: "dashboard" }
      }

    },

    listen: {

      states: {
        sidebar : {
          TASKS: "tasks",
          ORGANIZATIONS: "organizations",
          SCHEDULES: "schedules",
          TEMPLATES: "templates",
          HOME: "home",
          PROFILE: "profile",
          ISSUES: "issues",
          DASHBOARD: "dashboard"
        }
      }
    }

  });

  Vue.$comp( "issue-create", {
    data: { summary: "" },

    ui: {
      submit: { CREATING: "sending", $DEFAULT: "create" }  
    },

    states: {
      READY: {
        home: { then: "READY", and: { send: "home", to: "$parent" }},
        cancel: { then: "READY", and: { send: "home", to: "$parent" }},
        init: { then: "READY", and: { unset: [ "summary" ] }},
        submit: { then: "SENDING", and: { post: "create.issue", body: { summary: "summary"} }}
      },

      SENDING: {
        data: { then: "READY", and: { send: "created", to: "$parent" } },
        invalid: { then: "READY", and: { pub: "warn" } },
        forbidden: { then: "READY", and: { pub: "warn" } },
        conflict: { then: "READY", and: { pub: "warn", msg: "issue_exists" }},
        error: { then: "READY", and: { pub: "error" } },
      }
    },


    listen: {
      states: {
        issues: { CREATE: "init" } 
      }
    }

  });

  Vue.$comp( "issue", {
    data: { 
      issue: {}, 
      owner: {},
      creator: {},
    },

    ui: {
      content: {
        INFO: "issue-info",
        DISCUSSION: "issue-discussion",
        FILES: "issue-files",
        ACTIVITY: "issue-activity"
      }
    },

    states: {
      INIT: {
        home: { then: "INIT", and: { send: "home", to: "$parent" }},
        init: { then: "INFO", and: [ {set: "issue"},{pub: "issue"} ]},
        info: { then: "INFO" },
        files: { then: "FILES" },
        discussion: { then: "DISCUSSION" },
        activity: { then: "ACTIVITY" },
      },

      INFO: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        info: { then: "INIT", and: "info" },
        issue: { then: "INFO", and: [{ set: "issue" }, {pub: "issue"}]},
        activity: { then: "ACTIVITY" },
        discussion: { then: "DISCUSSION" },
        files: { then: "FILES" }
      },

      FILES: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        info: { then: "INFO" },
        files: { then: "INIT", and: "files" },
        discussion: { then: "DISCUSSION" },
        activity: { then: "ACTIVITY" }
      },

      ACTIVITY: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        activity: { then: "INIT", and: "activity" },
        info: { then: "INFO" },
        discussion: { then: "DISCUSSION" },
        files: { then: "FILES" }
      },

      DISCUSSION: {
        home: { then: "INIT", and: "home" },
        init: { then: "INIT", and: "init" },
        activity: { then: "ACTIVITY" },
        info: { then: "INFO" },
        discussion: { then: "INIT", and: "discussion" },
        files: { then: "FILES" }
      }
    },

    methods: {
      $reset: function() {
        this.$data.issue= {};
        this.$data.owner = {};
        this.$data.creator = {}
      },
    },

    listen: {
      events: {
        $parent: { issue: "init" },
      },
    }
  }); 


  var issueStatusBadge = function(s) {
    return '<span class="label tag label-{1}">{0}</span>'
      .$format([ Vue.$i18n( "status_" + s), Vue.$config.issues.status[s] ]);
  }

  Vue.filter( "issue-status-badge", function(s) {
    return issueStatusBadge(s);
  });

  var issueEstimateBadge = function(s) {
    return '<span class="label tag label-{1}">{0}</span>'
      .$format([ Vue.$i18n( "estimate_" + s), Vue.$config.issues.estimates[s] ]);
  }
  
  var noEstimateBadge = function(s) {
    return '<span class="label tag label-default">{0}</span>'
      .$format([ Vue.$i18n( "no_estimate" ) ]);
  }

  Vue.filter( "issue-estimate-badge", function(s) {
    return s ? issueEstimateBadge(s) : noEstimateBadge();
  });

  var issueReleaseBadge = function(r) {
    return '<span class="label tag label-{1}">{0}</span>'
      .$format([ r, Vue.$config.issues.releases[r] ]);
  }
  
  var noReleaseBadge = function(s) {
    return '<span class="label tag label-default">{0}</span>'
      .$format([ Vue.$i18n( "no_release" ) ]);
  }

  Vue.filter( "issue-release-badge", function(s) {
    return s ? issueReleaseBadge(s) : noReleaseBadge();
  });


  var ruleStatusBadge = function(s) {
    return '<span class="label tag label-{1}">{0}</span>'
      .$format([ Vue.$i18n( "status_" + s), Vue.$config.rules.status[s] ]);
  }

  Vue.filter( "rule-status-badge", function(s) {
    return ruleStatusBadge(s);
  });

  var statusIndicator = function(s) {
    return "<span style='margin-right: 5px;' class='color-{0}'><i class='fa fa-circle'></i></span>"
      .$format([ Vue.$config.status.styles[s] ]);
  } 

  Vue.filter( "status-indicator", function(s){
    return statusIndicator(s);
  });

  var statusLabel = function( s ) {
    return "<div><span style='margin-right: 5px;' class='color-{0}'><i class='fa fa-circle'></i></span> {1}</div>"
      .$format([ Vue.$config.status.styles[s], Vue.$i18n( 'status_' + s ) ]);
  };

  Vue.filter( "status-label", function( s) {
    return statusLabel( s );
  });

  var estimateLabel = function( s ) {
    return "<div><span style='margin-right: 5px;' class='color-{0}'><i class='fa fa-circle'></i></span> {1}</div>"
      .$format([ Vue.$config.estimates.styles[s], Vue.$i18n( 'estimate_' + s ) ]);
  };

  Vue.filter( "estimate-label", function( s) {
    return estimateLabel( s );
  });


  var releaseLabel = function( r ) {
    return "<div><span style='margin-right: 5px;' class='color-{0}'><i class='fa fa-circle'></i></span> {1}</div>"
      .$format([ Vue.$config.releases.styles[r], r ]);
  };

  Vue.filter( "release-label", function(r) {
    return releaseLabel(r);
  });


  Vue.$comp( "status-chooser", {
    many: true,
    data: { statuses: [ "new", "progress", "resolved", "closed" ], selected: "" },

    states: {
      INIT: {
        init: { then: "READY", and: [ "setup", "render" ]},
      },

      READY: {
        init: { then: "INIT", and: "init" },
        select: { then: "READY", and: [{ set: "selected"}, "refresh" ]},
        selected: { then: "READY", and: { send: "status", to: "$parent" }},
        refresh: { then: "READY", and: "render" },
      }
    },

    methods: {

      setup: function() {
        var self = this;
        var $el = $( this.$el );
        $el.selectpicker();
        $el.on( "change", function(e) {
          self.$receive( "selected", $el.selectpicker("val")  );    
        });
      },

      render: function() {
        var $el = $( this.$el );
        var self = this;
        this.$nextTick( function() {
          $el.selectpicker( 'refresh' );
          self.$receive( "selected", $el.selectpicker( "val" ) );
        });
      },

      refresh: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", this.$data.selected );
      },

      clear: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", null ); 
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      events: {
        $parent: { status: "select"}
      }
    }
  });

  var RELEASES = [ "MVP", "V1.0" ]; 

  Vue.$comp( "release-chooser", {
    many: true,
    data: { releases: RELEASES, selected: null },
    states: {
      INIT: {
        init: { then: "READY", and: [ "setup", "render" ]},
      },

      READY: {
        init: { then: "INIT", and: "init" },
        select: { then: "READY", and: [{ set: "selected"}, "refresh" ]},
        selected: { then: "READY", and: { send: "release", to: "$parent" }},
        refresh: { then: "READY", and: "render" },
        clear: { then: "READY", and: "clear" }
      }
    },

    methods: {

      setup: function() {
        var self = this;
        var $el = $( this.$el );
        $el.selectpicker();
        $el.on( "change", function(e) {
          self.$receive( "selected", $el.selectpicker("val")  );    
        });
      },

      render: function() {
        var $el = $( this.$el );
        var self = this;
        this.$nextTick( function() {
          $el.selectpicker( 'refresh' );
          self.$receive( "selected", $el.selectpicker( "val" ) );
        });
      },

      refresh: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", 
          this.$data.selected && this.$data.selected.length ? this.$data.selected : null );
      },

      clear: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", null ); 
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      events: {
        $parent: { release: "select", clear: "clear" }
      }
    }
  });
  
  var ESTIMATE_WORST_CASE = {
    "two_days" : 2,
    "one_week" :7,
    "two_three_weeks" : 21,
    "one_month" : 60
  }

  Vue.$comp( "estimate-chooser", {
    many: true,
    data: { estimates: [ 
      "needs_info", 
      "two_days", 
      "one_week", 
      "two_three_weeks", 
      "one_month" 
    ], selected: null },

    states: {
      INIT: {
        init: { then: "READY", and: [ "setup", "render" ]},
      },

      READY: {
        init: { then: "INIT", and: "init" },
        select: { then: "READY", and: [{ set: "selected"}, "refresh" ]},
        selected: { then: "READY", and: { send: "estimate", to: "$parent" }},
        refresh: { then: "READY", and: "render" },
      }
    },

    methods: {

      setup: function() {
        var self = this;
        var $el = $( this.$el );
        $el.selectpicker();
        $el.on( "change", function(e) {
          self.$receive( "selected", $el.selectpicker("val")  );    
        });
      },

      render: function() {
        var $el = $( this.$el );
        var self = this;
        this.$nextTick( function() {
          $el.selectpicker( 'refresh' );
          self.$receive( "selected", $el.selectpicker( "val" ) );
        });
      },

      refresh: function() {
        var $el = $( this.$el );
        this.$debug( "estimate chooser value" , this.$data.selected );
        $el.selectpicker( "val", 
          this.$data.selected && this.$data.selected.length ? this.$data.selected : null );
      },

      clear: function() {
        var $el = $( this.$el );
        $el.selectpicker( "val", null ); 
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      events: {
        $parent: { estimate: "select"}
      }
    }
  });

  var EVENT_ICONS = {
    status: "wrench",
    asset: "upload",
    comment: "comment",
    release: "road"
  };

  Vue.$comp( "issue-event-list-item", {
    props: [ "event" ],
    many: true,
    computed: {
      icon: function() {
        return EVENT_ICONS[this.event.type];
      },

      summary: function() {
        var str = Vue.$i18n( "issue_event_" + this.event.type );
        switch( this.event.type ) {
          case "status": 
            return str.$format([ Vue.$i18n( 'status_' + this.event.value ) ]);
          default:
            return str.$format([ this.event.value ]);
        }
      }
    }
  });

  Vue.$comp( "issue-activity", {
    data: { issue: {}, events: [] },
    states: {
      INIT: {
        init: { then: "INIT", and: {set: "issue"}},
        //fetch: { then: "FETCHING", and: "mock" },
        fetch: { then: "FETCHING", and: { http: "get.issue.events", body: { id: "issue.id" } }}
      },

      READY: {
        init: { then: "INIT", and: "init" }
      },

      FETCHING: {
        data: { then: "READY", and: { set: "events" } },
      }
    },

    listen: {
      states: {
        $parent: { ACTIVITY: "fetch" }
      },

      events: {
        $parent: { issue: "init" }
      }
    },

    methods: {
      mock: function() {
        this.$receive( "data", [
          { type: "status", ownedby: "6c9b95bf-3d2f-40cc-9bef-33e8d4d1ce54", created: "1459366137", value: "new" },
          { type: "status", ownedby: "6c9b95bf-3d2f-40cc-9bef-33e8d4d1ce54", created: "1459366137", value: "progress" }, 
          { type: "status", ownedby: "6c9b95bf-3d2f-40cc-9bef-33e8d4d1ce54", created: "1459366137", value: "resolved" },
          { type: "status", ownedby: "6c9b95bf-3d2f-40cc-9bef-33e8d4d1ce54", created: "1459366137", value: "progress" }, 
          { type: "status", ownedby: "6c9b95bf-3d2f-40cc-9bef-33e8d4d1ce54", created: "1459366137", value: "closed" } 
        ]);
      }
    }

  });
  

  Vue.$comp( "issue-discussion-comment", {
    many: true,
    props: [ "comment" ],
    states: {
      READY: {}
    }
  });

  Vue.$comp( "issue-discussion-create", {
    data: { issue: {}, new_comment: "" },
    ui: {
      submit: { $DEFAULT: "create", CREATING: "sending" }
    },

    states: {
      READY: {
        issue: { then: "READY", and: [ {unset: [ "new_comment" ]}, { set: "issue" }]},
        submit: { then: "CREATING", and: { post: "create.issue.comment", 
          body: { id: "issue.id", comment: "new_comment" }}} 
      },

      CREATING: {
        error: { then: "READY", and: { pub: "warn" }},
        invalid: { then: "READY", and: { pub: "warn", msg: "invalid_comment" }},
        data: { then: "READY", and: [ {unset: ["new_comment"] }, { pub: "created" } ]}
      }
    },
    
    listen: {
      events: {
        $parent: { issue: "issue" }
      }
    }
  });

  Vue.$comp( "issue-discussion-list", { 
    data: { issue: {}, comments: [] },
    computed: {
      has_comments: function() {
        return this.comments.length;
      }
    },
    
    states: {
      READY: {
        issue: { then: "READY", and: [{set: "issue"}, {unset: ["comments"]}, "fetch" ]},
        fetch: { then: "FETCHING", and: { http: "get.issue.comments", body: { id: "issue.id" }} }
      },

      FETCHING: {
        data: { then: "READY", and: {set: "comments"}}
      }
    },
    
    listen: {
      events: {
        $parent: { issue: "issue" },
        "issue-discussion-create": { created: "fetch" }
      }
    },

  });

  Vue.$comp( "issue-discussion", {
    data: {},
    states: {
      READY: { 
        issue: { then: "READY", and: { pub: "issue" }},
      }
    },

    listen: {
      events: {
        $parent: { issue: "issue" }
      }
    }
  });

  Vue.$comp( "issue-file", {
    many: true,
    props: [ "file" ],
    data: { src: ""},
    computed: {
      is_image: function() {
        return this.file.type === "image/jpeg" || 
          this.file.type === "image/png";
      }
    },
    states: {
      INIT: {
        fetch: [
          { if: "isImage", then: "FETCHING", and: { http: "asset.get", binary: true, body: { id: "file.id" } }},
          { then: "READY" }
        ]
      },

      FETCHING: {
        data: { then: "READY", and: { set: "src" } }
      },

      READY: {
        download: { then: "DOWNLOADING", and: { http: "asset.get", binary: true, raw: true, body: { id: "file.id" } }}
      },

      DOWNLOADING: {
        data: { then: "READY", and: "save" }
      }
    },

    methods: {
      isImage: function() {
        return this.$data.file.type === "image/jpeg" ||
          this.$data.file.type === "image/png";
      },

      save: function( data ) {
        this.$download( data, this.$data.file.name, this.$data.file.type );
      }
    },

    hooks: {
      $init: "fetch" 
    }
  });

  Vue.$comp( "issue-files", {
    data: { files: [], issue: {}, asset: {} },
    states: {
      INIT: {
        init: { then: "INIT", and: [{ set: "issue" }, "fetch" ]},
        fetch: { then: "FETCHING", and: { http: "get.issue.assets", body: { id: "issue.id" }}}
      },

      FETCHING: {
        data: { then: "READY", and: [ {set: "files" }, "configure" ]}
      },

      READY: {
        init: { then: "INIT", and: "init" },
        uploading: { then: "UPLOADING" }
      },

      UPLOADING: {
        data: { then: "LINKING", and: [ {set: "asset"}, "link" ]},
        error: { then: "READY", and: { pub: "error" }},
        invalid: { then: "READY", and: { pub: "warn" }}
      },

      LINKING: {
        link: { then: "LINKING", and: { post: "link.issue.asset", body: { id: "issue.id", asset: "asset.id" } }},
        data: { then: "INIT", and: "fetch"  },
        error: { then: "READY", and: { pub: "error" }},
        warn: { then: "READY", and: { pub: "warn" }}
      }

    },

    methods: {
      configure: function(){
        this.$upload({ path: "/api/asset.create", el: "#file" });  
      },
    },

    listen: {
      events: {
        $parent: { issue: "init" }
      }
    }
           
  });
  
  Vue.$comp( "issue-info", {
    data: { issue: {}, owner: {}, creator: {}, new_owner: "", new_status: "", new_estimate: null, new_release: null },
    ui: {
      summary: { $DEFAULT: "change_summary", RENAMING: "sending" },
      assign: { $DEFAULT: "change_owner", CHANGING_OWNER: "sending" },
      change_status: { $DEFAULT: "change_status", CHANGING_STATUS: "sending" },
      change_estimate: { $DEFAULT: "change_estimate", CHANGING_STATUS: "sending" },
      change_release: { $DEFAULT: "change_release", CHANGING_RELEASE: "sending" },
    },

    computed: {
      readonly: function() {
        return !this.issue.access || this.issue.access != 'owner';
      } 
    },

    states: {

      INIT: { 
        init: { then: "INIT", and: [ 
          { set: "issue" }, 
          { pub: "status", data: "issue.status" }, 
          { pub: "estimate", data: "issue.estimate" }, 
          { pub: "release", data: "issue.release" }, 
          "fetch_users" 
        ]},
        fetch_users: { then: "FETCHING_USERS", and: "getUsers" },
        fetch_owner: { then: "FETCHING_OWNER", and: "getOwner" },
        fetch_creator: { then: "FETCHING_CREATOR", and: "getCreator" },
        users: { then: "INIT", and: { pub: "users" }},
      },

      FETCHING_USERS: {
        data: { then: "INIT", and: [{pub: "users"}, "fetch_owner" ]},
      },

      FETCHING_OWNER: {
        data: { then: "INIT", and: [ {set: "owner"}, "fetch_creator" ]},
        user_selected: { then: "FETCHING_OWNER", and: { set: "new_owner" } },
      },

      FETCHING_CREATOR: {
        data: {then: "READY", and: {set: "creator" }},
        user_selected: { then: "FETCHING_CREATOR", and: { set: "new_owner" } },
      },

      READY: {
        init: { then: "INIT", and: "init" },
        users: { then: "READY", and: { pub: "users" }},
        summary: { then: "RENAMING", and: 
          { post: "rename.issue", body: { id: "issue.id", summary: "issue.summary" } } },
        owner: { then: "CHANGING_OWNER", and: 
            { post: "set.issue.owner", body: { id: "issue.id", owner: "new_owner" } } },
        user_selected: { then: "READY", and: { set: "new_owner" } },
        refresh: { then: "READY", and: {pub: "refresh" }},
        status: { then: "READY", and: {set: "new_status"}},
        change_status: { then: "CHANGING_STATUS", and: {
          post: "update.issue.status", body: {id: "issue.id", status: "new_status" }}},
        estimate: { then: "READY", and: {set: "new_estimate"}},
        change_estimate: { then: "CHANGING_ESTIMATE", and: {
          post: "update.issue.estimate", body: {id: "issue.id", estimate: "new_estimate" }}},
        release: { then: "READY", and: {set: "new_release"}},
        change_release: { then: "CHANGING_RELEASE", and: {
          post: "update.issue.release", body: {id: "issue.id", release: "new_release" }}}

      },

      RENAMING: {
        data: { then: "READY", and: [{ set: "issue" }, { send: "issue", to: "$parent" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
        conflict: { then: "READY", and: {pub: "warn", msg: "issue_exists"}}
      },

      CHANGING_OWNER: {
        data: { then: "READY", and: [{ set: "issue" }, { send: "issue", to: "$parent" }, { pub: "refresh" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
        conflict: { then: "READY", and: { pub: "warn", msg: "issue_owner_name_conflict" }}
      },

      CHANGING_STATUS: {
        data: { then: "READY", and: [{ set: "issue" }, { send: "issue", to: "$parent" }, { pub: "refresh" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
      },

      CHANGING_ESTIMATE: {
        data: { then: "READY", and: [{ set: "issue" }, { send: "issue", to: "$parent" }, { pub: "refresh" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
      },
      
      CHANGING_RELEASE: {
        data: { then: "READY", and: [{ set: "issue" }, { send: "issue", to: "$parent" }, { pub: "refresh" }] },
        invalid: { then: "READY", and: { pub: "warn"  }},
        error: { then: "READY", and: { pub: "error" }},
      },

    },

    listen: {
      events: {
        $parent: { issue: "init", users: "users" }
      } ,

      states: {
        $parent: { INFO: "refresh" }
      }
    },

    methods: {

      getUsers: function() {
        getIssuesUsers( this, "data" );
      },

      getOwner: function() {
        getProfile( this, this.$data.issue.ownedby, "data" );
      },

      getCreator: function() {
        getProfile( this, this.$data.issue.createdby, "data" );
      },

      $reset: function() {
        this.$data.new_owner = "";
        this.$data.new_status = "";
        this.$data.issue = {};
        this.$data.owner = {};
        this.$data.creator = {};
      }

    }
  });


  Vue.$comp( "issue-list-item", {
    props: [ "issue" ],
    many: true,
    states: {
      DEFAULT: {
        select: { 
          then: "DEFAULT", and: { send: "selected", to: "$parent", args: "issue" }
        }
      }
    },
  });
  
  Vue.filter( 'completion-date', function(p) {
    if( p.total === 0 ) return Vue.$i18n( 'no_completion_date' );
    var daysLeft = p.total - p.achieved;
    var m = Vue.$date();
    m.add( daysLeft, 'days' );
    return Vue.$fmt_date( m, "date_fmt_day" );
  });

  Vue.$comp( "issues-list", {
    data: { issues: [], progress: { total: 0, achieved: 0 }, release: Vue.$config.releases.defaultRelease },
    ui: {
      toggle: { CLOSED: "showing_closed", OPEN: "showing_open" },
      toggle_icon: { CLOSED: "folder", OPEN: "folder-open-o" }
    },

    computed: {
      progressPercent: function() {
        if(this.progress.total == 0) return 0;
        return Math.floor(100 * this.progress.achieved / this.progress.total );
      }
    },

    states: {

      OPEN: {
        fetch: { then: "FETCHING_OPEN", and: { http: "get.open.issues"}},
        selected: { then: "OPEN", and: { send: "selected", to: "$parent" } },
        create: { then: "OPEN", and: { send: "create", to: "$parent" }},
        toggle: { then: "CLOSED", and: "fetch" },
        release: { then: "OPEN", and: [ { set: "release" }, "fetch" ] },
        clear_filter: { then: "OPEN", and: [ { unset: [ "release" ] }, { pub: "clear" }, "fetch" ]}
      },

      FETCHING_OPEN: {
        data: { then: "OPEN", and: [ { set: "issues" }, "filter", "estimate" ]},
        error: { then: "OPEN", and: { pub: "error" }},
      },

      CLOSED: {
        fetch: { then: "FETCHING_CLOSED", and: { http: "get.closed.issues"}},
        selected: { then: "CLOSED", and: { send: "selected", to: "$parent" } },
        create: { then: "CLOSED", and: { send: "create", to: "$parent" }},
        toggle: { then: "OPEN", and: "fetch" },
        release: { then: "CLOSED", and: [ { set: "release" }, "fetch" ] },
        clear_filter: { then: "OPEN", and: [ { unset: [ "release" ] }, { pub: "clear" }, "fetch" ]}
      },

      FETCHING_CLOSED: {
        data: { then: "CLOSED", and: [ {set: "issues" }, "filter", "estimate" ]},
        error: { then: "CLOSED", and: { pub: "error" } },
      }

    },

    hooks: {
      $init: "fetch"
    },

    listen: {
      states: {
        issues: { LIST: "fetch" }
      }
    },

    methods: {
      $reset: function(){
        this.$data.issues=[];
      },

      filter: function() {
        if( this.$data.release && this.$data.release.length ) {
          var self = this;
          this.$data.issues = this.$data.issues.filter( function(i) {
            return i.release === self.$data.release;
          });
        }
      },

      estimate: function(msg) {
        var self = this;
        this.$data.progress.total = 0;
        this.$data.progress.achieved = 0;
        
        this.$data.issues.map( function(i) {
          var days = ESTIMATE_WORST_CASE[i.estimate];
          if (days && !isNaN(days)) {
            self.$data.progress.total += days;
            if( i.status === "resolved" || i.status === "closed" ) self.$data.progress.achieved += days;
          }
        });
      }
    }
  });

  Vue.$comp( "issues", {
    ui: { 
      content: { 
        LIST: "issues-list",
        CREATE: "issue-create",
        ISSUE: "issue"
      } 
    },
    states: {

      NONE: {
        init: { then: "LIST" } 
      },

      LIST: {
        selected: { then: "ISSUE", and: { pub: "issue" }},
        init: { then: "NONE", and: "init" },
        create: { then: "CREATE" }
      },

      CREATE: {
        home: { then: "NONE", and: "init" },
        init: { then: "NONE", and: "init" },
        created: { then: "ISSUE", and: { pub: "issue" } }
      },

      ISSUE: {
        home: { then: "NONE", and: "init" },
        init: { then: "NONE", and: "init" }
      }
    },

    hooks: {
      $init: "init"
    },

    listen: {
      states: {
        main: { ISSUES: "init" }
      }
    }

  });


  Vue.$vue( "app", {
    data: { error_code: "", error_params: "", profile: {} },
    reset: {disabled: true},
    ws: false, 
    ui: {
      landing: { $DEFAULT: "hidden", HOME: "visible" },
      style: { $DEFAULT: "gray-bg", HOME: "landing-page", SIGNED_IN: "none mini-navbar" },

      content: { 
        $DEFAULT: "connecting",
        ERROR: "error",
        HOME: "empty",
        SIGN_IN: "sign_in", 
        SIGN_UP: "sign_up", 
        FORGOT_PASSWORD: "forgot_password", 
        RESET_PASSWORD: "reset_password",
        SIGNED_IN: "main"
      }
    },
    states: {

      CONNECTING: {
        connected: [
          { then: "FETCHING_PROFILE", if: "hasSession", and: "getMyProfile" },
          { then: "HOME", and: [ "navigate", "showLandingPage" ] } ],
          error: { then: "ERROR", and: "displayError"},
          not_implemented: { then: "ERROR", and: "displayError" },
          session_invalid: { then: "ERROR", and: ["clearSession", "displaySessionInvalid"] },
          forbidden: [
            { if: { eq: "reason", source: "msg", value: "invalid_session" }, then: "CONNECTING", and: "session_invalid" },
            { then: "ERROR", and: "displayError" }
          ],
      },

      FETCHING_PROFILE: {
        data: { then: "FETCHING_PHOTO", and: [ {set: "profile"}, "getMyPhoto" ]},
        forbidden: [
          { if: { eq: "reason", source: "msg", value: "invalid_session" }, then: "CONNECTING", and: "session_invalid" },
          { then: "ERROR", and: "displayError" }
        ],
      },

      FETCHING_PHOTO: {
        data: { then: "SIGNED_IN", and: { pub: "user", data: "profile" }},
        error: { then: "ERROR", and: "displayError" } 
      },

      HOME: {
        disconnected: { then: "CONNECTING" },
        sign_in: { then: "SIGN_IN" },
        sign_up: { then: "SIGN_UP" },
        reset_password: { then: "RESET_PASSWORD", and: { pub: "token"} },
        error: { then: "ERROR", and: "displayError"},
        not_implemented: { then: "ERROR", and: "displayError" },
        session_invalid: { then: "ERROR", and: ["clearSession", "displayError"] },
        forbidden: { then: "ERROR", and: "displayError" }
      },

      SIGN_UP: {
        sign_in: { then: "SIGN_IN" },
        home: { then: "HOME" },
        disconnected: { then: "CONNECTING" },
        error: { then: "ERROR", and: "displayError"},
        not_implemented: { then: "ERROR", and: "displayError" },
        session_invalid: { then: "ERROR", and: ["clearSession", "displayError"] },
        forbidden: { then: "ERROR", and: "displayError" }
      },

      SIGN_IN: {
        sign_up: { then: "SIGN_UP" },
        forgot_password: { then: "FORGOT_PASSWORD" },
        home: { then: "HOME" },
        disconnected: { then: "CONNECTING" },
        error: { then: "ERROR", and: "displayError"},
        not_implemented: { then: "ERROR", and: "displayError" },
        session_invalid: { then: "ERROR", and: ["clearSession", "displayError"] },
        success: { then: "FETCHING_PROFILE", and: "getMyProfile" },
        forbidden: { then: "ERROR", and: "displayError" }
      },

      FORGOT_PASSWORD: {
        sign_in: { then: "SIGN_IN" },
        error: { then: "ERROR", and: "displayError"},
        disconnected: { then: "CONNECTING" },
        not_implemented: { then: "ERROR", and: "displayError" },
        session_invalid: { then: "ERROR", and: ["clearSession", "displayError"] },
        forbidden: { then: "ERROR", and: "displayError" }
      },

      RESET_PASSWORD: {
        home: { then: "HOME" },
        error: { then: "ERROR", and: "displayError"},
        disconnected: { then: "CONNECTING" },
        not_implemented: { then: "ERROR", and: "displayError" },
        session_invalid: { then: "ERROR", and: ["clearSession", "displayError"] },
        forbidden: { then: "ERROR", and: "displayError" }
      },

      ERROR: {},

      SIGNED_IN: {
        data: { then: "SIGNED_IN", and: { pub: "user" }},
        sign_out: { then: "HOME", and: [ "clearSession", "showLandingPage" ]},
        error: { then: "ERROR", and: "displayError"},
        disconnected: { then: "CONNECTING" },
        not_implemented: { then: "ERROR", and: "displayError" },
        session_invalid: { then: "ERROR", and: ["clearSession", "displaySessionInvalid"] },
        forbidden: [
          { if: { eq: "reason", source: "msg", value: "invalid_session" } , then: "SIGNED_IN", and: "session_invalid" },
          { then: "ERROR", and: "displayError" }
        ]
      }

    },

    methods: {

      showLandingPage: function(){
        $("#landing").removeAttr("style");    
      },

      clearSession: function(){
        Vue.$unset( Vue.$config.session_key );
        this.$data.profile = {};
      },

      hasSession: function() {
        return Vue.$get( Vue.$config.session_key ) != null;
      },

      displayError: function(msg){
        this.$pub( "error", msg );
      },

      displaySessionInvalid: function(msg) {
        this.$pub( "error", { status: "session_invalid", reason: "session_invalid" });
      },

      navigate: function() {
        var qs = window.location.hash;
        if( qs ) {
          qs = queryString.parse( qs );
          if( qs.e ) this.$receive( qs.e, qs.a );
        }    
      },

      getMyProfile: function() {
        getMyProfile( this, "data", true );  
      },

      getMyPhoto: function() {
        getPhoto( this, this.$data.profile.id, "data" );
      }
    },

    listen: {
      states: {
        sign_in: { SUCCESS: "success" },
        "sign-out": { SIGNED_OUT: "sign_out" }
      }
    },

    hooks: {
      $init: "connected"
    }

  });


  Vue.$comp( "list-nav", {
    many: true,
    props: [ "day", "week", "month" ],
    data: { period: null, title: "", mine: true, all: false },
    computed: {
      is_day_enabled: function() {
        return this.day;
      },

      is_week_enabled: function() {
        return this.week;
      },

      is_month_enabled: function() {
        return this.month;
      },
    },
    
    states: {
      WEEK: {
        init: { then: "WEEK", and: "now" },
        now: { then: "WEEK", and: [ "setToday", "title", "notify" ]},
        notify: { then: "WEEK", and: { send: "week", to: "$parent", data: "period" } },
        title: { then: "WEEK", and: "displayWeek" },
        prev: { then: "WEEK", and: [ "prevWeek", "title", "notify" ]},
        next: { then: "WEEK", and: [ "nextWeek" , "title", "notify" ]},
        day: { then: "DAY", and: [ "title", "notify" ]},
        month: { then: "MONTH", and: [ "title", "notify" ]},
        create:{ then: "WEEK", and: { send: "create", to: "$parent" }},
        all: { then: "WEEK", and: [ "setAll", { send: "all", to: "$parent" }]},
        mine: { then: "WEEK", and: [ "setMine", { send: "mine", to: "$parent" }]},
      },

      MONTH: {
        init: { then: "MONTH", and: "now" },
        now: { then: "MONTH", and: [ "setToday", "title", "notify" ]},
        notify: { then: "MONTH", and: { send: "month", to: "$parent", data: "period" } },
        title: { then: "MONTH", and: "displayMonth" },
        prev: { then: "MONTH", and: [ "prevMonth", "title", "notify" ]},
        next: { then: "MONTH", and: [ "nextMonth" , "title", "notify" ]},
        day: { then: "DAY", and: [ "title", "notify"  ]},
        week: { then: "WEEK", and: [ "title", "notify" ]},
        create:{ then: "MONTH", and: { send: "create", to: "$parent" }},
        all: { then: "MONTH", and: [ "setAll", { send: "all", to: "$parent" }]},
        mine: { then: "MONTH", and: [ "setMine", { send: "mine", to: "$parent" }]},
      },

      DAY: {
        init: { then: "DAY", and: "now" },
        now: { then: "DAY", and: [ "setToday", "title", "notify" ]},
        notify: { then: "DAY", and: { send: "day", to: "$parent", data: "period" } },
        title: { then: "DAY", and: "displayDay" },
        prev: { then: "DAY", and: [ "prevDay", "title", "notify" ]},
        next: { then: "DAY", and: [ "nextDay" , "title", "notify" ]},
        month: { then: "MONTH", and: [ "title", "notify" ]},
        week: { then: "WEEK", and: [ "title", "notify" ]},
        create:{ then: "DAY", and: { send: "create", to: "$parent" }},
        all: { then: "DAY", and: [ "setAll", { send: "all", to: "$parent" }]},
        mine: { then: "DAY", and: [ "setMine", { send: "mine", to: "$parent" }]},
      },

    },

    hooks: {
      $init: "init" 
    },

    methods: {

      setMine: function() {
        this.$data.mine = true;
        this.$data.all = false;
      },

      setAll: function() {
        this.$data.mine = false;
        this.$data.all = true;
      },

      setToday: function() {
        this.$data.period = Vue.$date();
      },

      prevDay: function() {
        this.$data.period =this.$data.period.add( -1, "d" );
      },

      nextDay: function() {
        this.$data.period = this.$data.period.add( 1, "d" );  
      },

      displayDay: function() {
        this.$data.title = Vue.$fmt_date( this.$data.period, "date_fmt_day" );
      },

      prevWeek: function() {
        this.$data.period = this.$data.period.add( -1, "w" );            
      },

      nextWeek: function() {
        this.$data.period = this.$data.period.add( 1, "w" );     
      },

      displayWeek: function() {
        this.$data.title = Vue.$fmt_date( this.$data.period, "date_fmt_woy" );   
      },

      prevMonth: function() {
        this.$data.period =this.$data.period.add( -1, "M" );     
      },

      nextMonth: function() {
        this.$data.period =this.$data.period.add( 1, "M" );     
      },

      displayMonth: function() {
        this.$data.title = Vue.$fmt_date( this.$data.period, "date_fmt_month" );
      } 
  
    }

  });

  Vue.$comp( "calendar", {
    many: true,
    props: [ "view", "mode", "nav", "day", "week", "month" ],
    data: { period: null, title: "" },
    silent: true,
    ui: {
      content: { 
        $DEFAULT: "empty", DAY: "calendar-day", WEEK: "calendar-week", MONTH: "calendar-month" 
      }
    },

    computed: {
      isTemplate: function() {
        return this.mode === "template";
      },

      is_day_enabled: function() {
        return this.day != "disabled"; 
      },
      
      is_week_enabled: function() {
        return this.week != "disabled"; 
      },

      is_month_enabled: function() {
        return this.month != "disabled"; 
      },

    },

    states: {

      INIT: {
        now: [
          { if: { eq: "view", value: "day" }, then: "DAY", and: "now" },
          { if: { eq: "view", value: "week" }, then: "WEEK", and: "now" },
          { if: { eq: "view", value: "month" }, then: "MONTH", and: "now" },
          { then: "DAY", and: "now" }
        ],

        clear: { then: "INIT", and: "now" }
      },

      DAY: {
        now: { then: "DAY", and: [ "setToday", "title", {pub: "period", data: "period" } ] },
        week: { then: "WEEK", and: [ "title", { send: "week", to: "$parent"}, {pub: "period", data: "period" } ] },
        month: { then: "MONTH" , and: [ "title", {send: "month", to: "$parent"}, {pub: "period", data: "period" } ]  },
        prev: { then: "DAY", and: [ "prevDay", "title", {pub: "period", data: "period" } ]},
        next: { then: "DAY", and: [ "nextDay" , "title", {pub: "period", data: "period" } ]},
        title: { then: "DAY", and: "displayDay" },
        period: { then: "DAY", and: { send: "period", to: "$parent" }},
        item: { then: "DAY", and:  { pub: "item" } },
        clear: { then: "DAY", and: { pub: "clear" } },
        moved_earlier: { then: "DAY", and: { send: "item_moved_earlier", to: "$parent" }},
        moved_later: { then: "DAY", and: { send: "item_moved_later", to: "$parent" }},
        end_earlier: { then: "DAY", and: { send: "item_ends_earlier", to: "$parent" }},
        end_later: { then: "DAY", and: { send: "item_ends_later", to: "$parent" }},
      },

      WEEK: {
        now: { then: "WEEK", and: [ "setToday", "title", {pub: "period", data: "period" }]},
        day: { then: "DAY" , and:  [ "title", {send: "day", to: "$parent"}, {pub: "period", data: "period" } ] },
        month: { then: "MONTH" , and:  [ "title", { send: "month", to: "$parent" }, {pub: "period", data: "period" } ] },
        prev: { then: "WEEK", and: [ "prevWeek", "title", {pub: "period", data: "period" } ] },
        next: { then: "WEEK", and: [ "nextWeek", "title", {pub: "period", data: "period" } ] },
        title: { then: "WEEK", and: "displayWeek" },
        slot: { then: "WEEK", and: { send: "slot", to: "$parent" } },
        selected: { then: "WEEK", and: { send: "item_selected", to: "$parent" } },
        moved: { then: "WEEK", and: { send: "item_moved", to: "$parent" } },
        item: { then: "WEEK", and:  { pub: "item" } },
        clear: { then: "WEEK", and: { pub: "clear" } },
        delete: { then: "WEEK", and: { pub: "delete" } },
        week: { then: "WEEK", and: [ "setWeek", "title", { pub: "period", data: "period" } ]},
        period: { then: "WEEK", and: { send: "period", to: "$parent" }},
        moved_prev_day: { then: "WEEK", and: { send: "item_moved_prev_day", to: "$parent" }},
        moved_next_day: { then: "WEEK", and: { send: "item_moved_next_day", to: "$parent" }},
        copy_prev_day: { then: "WEEK", and: { send: "item_copied_prev_day", to: "$parent" }},
        copy_next_day: { then: "WEEK", and: { send: "item_copied_next_day", to: "$parent" }},
        moved_earlier: { then: "WEEK", and: { send: "item_moved_earlier", to: "$parent" }},
        moved_later: { then: "WEEK", and: { send: "item_moved_later", to: "$parent" }},
        end_earlier: { then: "WEEK", and: { send: "item_ends_earlier", to: "$parent" }},
        end_later: { then: "WEEK", and: { send: "item_ends_later", to: "$parent" }},

      },

      MONTH: {
        now: { then: "MONTH", and: [ "setToday", "title", {pub: "period", data: "period" } ] },
        day: { then: "DAY"  , and: [ "title", {send: "day", to: "$parent"}, {pub: "period", data: "period" } ],  },
        week: { then: "WEEK" , and: [ "title", { send: "week", to: "$parent"}, {pub: "period", data: "period" } ]  },
        prev: { then: "MONTH", and: [ "prevMonth", "title", {pub: "period", data: "period" } ] },
        next: { then: "MONTH", and: [ "nextMonth", "title", {pub: "period", data: "period" } ] },
        title: { then: "MONTH", and: "displayMonth"},
        slot: { then: "MONTH", and: { send: "slot", to: "$parent" } },
        selected: { then: "MONTH", and: { send: "item_selected", to: "$parent" } },
        moved: { then: "MONTH", and: { send: "item_moved", to: "$parent" } },
        item: { then: "MONTH", and:  { pub: "item" } },
        clear: { then: "MONTH", and: { pub: "clear" } },
        delete: { then: "MONTH", and: { pub: "delete" } },
        //month: { then: "MONTH", and: [ "setMonth", "title", { pub: "period", data: "period" } ]},
        period: { then: "MONTH", and: { send: "period", to: "$parent" }}
      }
    },

    hooks: {
      $init: "now"
    },

    methods: {

      setToday: function() {
        this.$data.period = Vue.$date();
      },

      prevDay: function() {
        this.$data.period =this.$data.period.add( -1, "d" );
      },

      nextDay: function() {
        this.$data.period = this.$data.period.add( 1, "d" );  
      },

      displayDay: function() {
        this.title = Vue.$fmt_date( this.$data.period, "date_fmt_day" );    
      },

      prevWeek: function() {
        this.$data.period = this.$data.period.add( -1, "w" );            
      },

      nextWeek: function() {
        this.$data.period = this.$data.period.add( 1, "w" );     
      },

      setWeek: function( msg ) {
        if( msg ) {
          this.$data.period = this.$data.period.year( msg.year );
          this.$data.period = this.$data.period.isoWeek( msg.week );
        }
      },

      setMonth: function( msg ) {
        this.$debug( "TODO: setMonth: " + JSON.stringify( msg ));
      },

      displayWeek: function() {
        this.title = Vue.$fmt_date( this.$data.period, "date_fmt_woy" );   
      },

      prevMonth: function() {
        this.$data.period =this.$data.period.add( -1, "M" );     
      },

      nextMonth: function() {
        this.$data.period =this.$data.period.add( 1, "M" );     
      },

      displayMonth: function() {
        this.title = Vue.$fmt_date( this.$data.period, "date_fmt_month" );   
      },

    },

    listen: {
      events: {
        $parent: { item: "item", clear: "clear", delete: "delete", week: "week" }
      }
    }

  });


  

  Vue.$comp( "calendar-day", {
    many: true,
    data: { slots: [], day: { today: false }, period: null, title: "", editable: false },
    silent: true,
    states: {
      INIT: {
        period: { then: "READY", and: [ "setPeriod", "build", "setTitle", {pub: "subscribe"}, {send: "period", to: "$parent"}]}
      },

      IDLE: {
        activate: { then: "READY" }
      },

      READY: {
        idle: { then: "IDLE" },
        period: { then: "READY", and: [ "setPeriod", "rebuild", "setTitle", {pub: "subscribe"}, {send: "period", to: "$parent" }]},
        item: { then: "READY", and: "pubItem" },
        clear: { then: "READY", and: [ {set: "editable"}, { pub: "clear" }] },
        moved_prev_day: { then: "READY", and: { send: "moved_prev_day", to: "$parent" }},
        moved_next_day: { then: "READY", and: { send: "moved_next_day", to: "$parent" }},
        copy_prev_day: { then: "READY", and: { send: "copy_prev_day", to: "$parent" }},
        copy_next_day: { then: "READY", and: { send: "copy_next_day", to: "$parent" }},
        moved_earlier: { then: "READY", and: { send: "moved_earlier", to: "$parent" }},
        moved_later: { then: "READY", and: { send: "moved_later", to: "$parent" }},
        end_earlier: { then: "READY", and: { send: "end_earlier", to: "$parent" }},
        end_later: { then: "READY", and: { send: "end_later", to: "$parent" }},
      }
    },

    methods: {

      $reset: function() {
        //this.$unsubAll();
        this.$data.slots = [];
      },

      build: function() {
        this.$reset();
        var m = moment();
        m.set({ hour: Vue.$config.calendar.start_hour, minutes: 0, seconds: 0 });
        for (var i=0; i<Vue.$config.calendar.hours; i++ ) {
          this.$data.slots.push({ time: m.clone() });
          m.add(1, "h" );
        }

        var today = Vue.$date();
        this.$data.day.today = today.isSame( this.$data.period, "day" );
      },

      rebuild: function() {
        

        var m = this.$data.period.clone();
        m.set({ hour: Vue.$config.calendar.start_hour, minutes: 0, seconds: 0 });
        for (var i=0; i<Vue.$config.calendar.hours; i++ ) {
          this.$data.slots[i].time = m.clone();
          m.add(1, "h" );
        }

        var today = Vue.$date();
        this.$data.day.today = today.isSame( this.$data.period, "day" );
      },

      setPeriod: function(arg) {
        this.$data.period = Vue.$date( arg );    
      },

      setTitle: function() {
        this.$data.title = Vue.$fmt_date( this.$data.period, "date_fmt_dow" );
      },

      pubItem: function(item) {
        this.$pub( "item-" + item.start_day + "-" + item.start_hour, item );  
      },

      pubDelete: function(item) {
        this.$pub( "delete-" + item.id, item );   
      }

    },

    listen: {
      events: {
        $parent: { period: "period", item: "item", clear: "clear", delete: "delete" }
      },

      states: {
        $parent: { DAY: "activate", MONTH: "idle", WEEK: "idle" }
      }
    }


  });


  Vue.$comp( "calendar-week", {
    many: true,
    data: { slots: [], days: [], period: null, isTemplate: false, editable: false },
    silent: true,
    states: {
      INIT: {
        period: { then: "READY", and: [ "setPeriod", "build", { send: "period", to: "$parent" }]}
      },
      
      IDLE: {
        activate: { then: "READY" }
      },

      READY: {
        idle: { then: "IDLE" },
        period: { then: "READY", and: [ "setPeriod", "rebuild", {send: "period", to: "$parent" }]},
        slot: { then: "READY", and: { send: "slot", to: "$parent" } },
        item: { then: "READY", and: "pubItem" },
        clear: { then: "READY", and: [{set: "editable"}, { pub: "clear" }] },
        selected: { then: "READY", and: { send: "selected", to: "$parent" } },
        moved: { then: "READY", and: { send: "moved", to: "$parent" } },
        delete: { then: "READY", and: "pubDelete" },
        moved_prev_day: { then: "READY", and: { send: "moved_prev_day", to: "$parent" }},
        moved_next_day: { then: "READY", and: { send: "moved_next_day", to: "$parent" }},
        copy_prev_day: { then: "READY", and: { send: "copy_prev_day", to: "$parent" }},
        copy_next_day: { then: "READY", and: { send: "copy_next_day", to: "$parent" }},
        moved_earlier: { then: "READY", and: { send: "moved_earlier", to: "$parent" }},
        moved_later: { then: "READY", and: { send: "moved_later", to: "$parent" }},
        end_earlier: { then: "READY", and: { send: "end_earlier", to: "$parent" }},
        end_later: { then: "READY", and: { send: "end_later", to: "$parent" }},
      }
    },

    methods: {
  
      $reset: function() {
        //this.$unsubAll();
        this.$data.slots = [];
        this.$data.days = [];
      },

      build: function() {
        this.$reset();
        this.$data.isTemplate = this.$parent.isTemplate;

        var m = this.$data.period.clone();
        m.set({ hour: Vue.$config.calendar.start_hour, minutes: 0, seconds: 0 });
        for (var i=0; i<Vue.$config.calendar.hours; i++ ) {
          this.$data.slots.push({ time: m.clone() });
          m.add(1, "h" );
        }

        var today = Vue.$date();
        m = this.$data.period.clone();
        m.startOf( "isoWeek" );
        for (var i=0; i<7; i++ ) {
          var c = m.clone();
          this.$data.days.push({ 
            time: c, 
            title: Vue.$fmt_date( c, this.$parent.isTemplate? "date_fmt_dow_short" : "date_fmt_dow_date" ), 
            today: this.$parent.isTemplate ? false : today.isSame(c, "day") 
          });
          m.add(1, "d" );       
        }

      },

      rebuild: function() {
        var m = this.$data.period.clone();
        m.set({ hour: Vue.$config.calendar.start_hour, minutes: 0, seconds: 0 });
        for (var i=0; i<Vue.$config.calendar.hours; i++ ) {
          this.$data.slots[i].time = m.clone();
          m.add( 1, "h" );
        }


        var today = Vue.$date();
        m = this.$data.period.clone();
        m.startOf( "isoWeek" );
        for (var i=0; i<7; i++ ) {
          var c = m.clone();
          this.$data.days[i].time = c;
          this.$data.days[i].title = Vue.$fmt_date( c, this.$parent.isTemplate? "date_fmt_dow_short" : "date_fmt_dow_date" );
          this.$data.days[i].today = this.$parent.isTemplate ? false : today.isSame(c, "day");
          m.add(1, "d" ); 
        }

      },

      setPeriod: function(arg) {
        this.$data.period = Vue.$date( arg );    
      },

      pubItem: function(item) {
        this.$pub( "item-" + item.start_day + "-" + item.start_hour, item );  
      },

      pubDelete: function(item) {
        this.$pub( "delete-" + item.id, item );   
      }
    },

    listen: {
      events: {
        $parent: { period: "period", item: "item", clear: "clear", delete: "delete" }
      },

      states: {
        $parent: { DAY: "idle", MONTH: "idle", WEEK: "activate" }
      }
    }


  });


  Vue.$comp( "calendar-month", {
    many: true,
    data: { weeks: [], days: [], period: null },
    silent: true,
    states: {
      INIT: {
        period: { then: "READY", and: [ "setPeriod", "build", {pub: "subscribe"}, { send: "period", to: "$parent" }]}
      },

      IDLE: {
        activate: { then: "READY" }
      },

      READY: {
        idle: { then: "IDLE" },
        period: { then: "READY", and: [ "setPeriod", "rebuild", { pub: "subscribe"}, { send: "period", to: "$parent" } ] },
        slot: { then: "READY", and: { send: "slot", to: "$parent" } },
        item: { then: "READY", and: "pubItem" },
        clear: { then: "READY", and: [{set: "editable"}, { pub: "clear" }] },
        selected: { then: "READY", and: { send: "selected", to: "$parent" } },
        moved: { then: "READY", and: { send: "moved", to: "$parent" } },
        delete: { then: "READY", and: "pubDelete" },

      }
    },

    methods: {
      
      $reset: function() {
        //this.$unsubAll();
        this.$data.days = [];
        this.$data.weeks = [];
      },
      
      build: function() {
        this.$reset();

        var today = Vue.$date();

        m = today.clone();
        m.set({ hour: 0, minutes: 0, seconds: 0 });
        m.startOf( "isoWeek" );
        for (var i=0; i<7; i++ ) {
          var c = m.clone();
          this.$data.days.push({ title: Vue.$fmt_date( c, "date_fmt_dow" )});
          m.add(1, "d" );       
        }

        m = this.$data.period.clone();
        m.set({ hour: 0, minutes: 0, seconds: 0 });
        m.startOf( "month" );
        m.startOf( "isoWeek" );

        for( var i=0; i<5; i++ ) {
          var days = [];
          for( var j=0; j<7; j++) {
            var c = m.clone();
            days.push({ 
              time: c, 
              today: today.isSame(c, "day"), 
              month: this.$data.period.isSame(c, "month"), 
              title: Vue.$fmt_date( c, "date_fmt_dom") 
            });
            m.add(1, "d" );
          }
          this.$data.weeks.push({ days: days });
        }

      },


      rebuild: function() {
        var today = Vue.$date();

        m = today.clone();
        m.set({ hour: 0, minutes: 0, seconds: 0 });
        m.startOf( "isoWeek" );
        for (var i=0; i<7; i++ ) {
          var c = m.clone();
          this.$data.days[i].title = Vue.$fmt_date( c, "date_fmt_dow" );
          m.add(1, "d" );       
        }

        m = this.$data.period.clone();
        m.set({ hour: 0, minutes: 0, seconds: 0 });
        m.startOf( "month" );
        m.startOf( "isoWeek" );

        for( var i=0; i<5; i++ ) {
          for( var j=0; j<7; j++) {
            var c = m.clone();
            this.$data.weeks[i].days[j].time = c;
            this.$data.weeks[i].days[j].today = today.isSame(c, "day");
            this.$data.weeks[i].days[j].month = this.$data.period.isSame(c, "month");
            this.$data.weeks[i].days[j].title = Vue.$fmt_date( c, "date_fmt_dom") ;
            m.add(1, "d" );
          }
        }

      },

      setPeriod: function(arg) {
        this.$data.period= Vue.$date( arg );    
      },

      pubItem: function(item) {
        this.$pub( "item-" + item.year + "-" + item.month + "-" + item.day, item );  
      },

      pubDelete: function(item) {
        this.$pub( "delete-" + item.id, item );   
      }

    },

    listen: {
      events: {
        $parent: { period: "period", item: "item", clear: "clear", delete: "delete" }
      },

      states: {
        $parent: { DAY: "idle", MONTH: "activate", WEEK: "idle" }
      }
    }


  });



  Vue.$comp( "calendar-slot", {
    many: true,
    silent: true, 
    data: { items: [], key: "", editable: true },
    props: [ "time", "day" ],
    states: {
      READY: {
        init: { then: "READY", and: [ "setupDrop", "subscribe" ] },
        subscribe: { then: "READY", and: "subscribe" },
        add: { if: { eq: "editable", value: "true" }, then: "READY", and: "notifySelectedSlot" },
        draw: { then: "READY", and: "push" },
        clear: { then: "READY", and: [ "clearItems", "setupDrop" ]},
        selected: {then: "READY", and: { send: "selected", to: "$parent" } },
        delete: { then: "READY", and: "deleteItem" },
        moved: { then: "READY", and: "movedItem" },
        moved_prev_day: { if: { eq: "editable", value: "true" }, then: "READY", and: "movedPrevDay" },
        moved_next_day: { if: { eq: "editable", value: "true" }, then: "READY", and: "movedNextDay" },
        copy_prev_day: { if: { eq: "editable", value: "true" }, then: "READY", and: "copyPrevDay" },
        copy_next_day: { if: { eq: "editable", value: "true" }, then: "READY", and: "copyNextDay" },
        moved_earlier: { if: { eq: "editable", value: "true" }, then: "READY", and: "movedEarlier" },
        moved_later: { if: { eq: "editable", value: "true" }, then: "READY", and: "movedLater" },
        end_earlier: { if: { eq: "editable", value: "true" }, then: "READY", and: "endEarlier" },
        end_later: { if: { eq: "editable", value: "true" }, then: "READY", and: "endLater" },
      }
    },

    listen: {
      events: {
        $parent: {clear: "clear", subscribe: "subscribe" }
      }
    },

    hooks: {
      $init: "init"
    },

    methods: {
      
      $reset: function() {
        //this.$unsubAll();
        this.clearItems();
      },

      subscribe: function() {
        if( this.$data.key ) this.$unsub( "$parent", this.$data.key ); 
        this.$data.key = "item-" + this.$data.day.weekday() + "-" + this.$data.time.hour();
        this.$sub( "$parent", this.$data.key, "draw" );
      },

      clearItems: function(msg) {
        this.$data.editable = msg;
        this.$data.items=[];  
      },

      push: function(msg) {
        this.$data.items.$removeUsing( "id", msg.id );
        msg.start_day = parseInt(msg.start_day);
        msg.start_hour = parseInt(msg.start_hour);
        msg.start_min = parseInt(msg.start_min);
        msg.end_day = parseInt(msg.end_day);
        msg.end_hour = parseInt(msg.end_hour);
        msg.end_min = parseInt(msg.end_min);                  
        this.$data.items.push(msg);
        this.$sub( "$parent", "delete-" + msg.id, "delete");
      },

      deleteItem: function(msg) {
        this.$data.items.$removeUsing( "id", msg.id );    
      },

      movedItem: function(item) {
        this.$sendParent( "moved", { item: item, day: this.$data.day.weekday(), time: this.$data.time.hour() });
      },

      movedNextDay: function( item ) {
        this.$sendParent( "moved_next_day", { item: item } );
      },

      movedPrevDay: function( item ) {
        this.$sendParent( "moved_prev_day", { item: item } );
      },

      copyNextDay: function( item ) {
        this.$sendParent( "copy_next_day", { item: item } );
      },

      copyPrevDay: function( item ) {
        this.$sendParent( "copy_prev_day", { item: item } );
      },

      movedEarlier: function( item ) {
        this.$sendParent( "moved_earlier", { item: item } );
      },

      movedLater: function( item ) {
        this.$sendParent( "moved_later", { item: item } );
      },

      endEarlier: function( item) {
        this.$sendParent( "end_earlier", { item: item } );
      },

      endLater: function( item ) {
        this.$sendParent( "end_later", { item: item } );
      },

      setupDrop: function() {
        var self = this;
        var $el = this.$el;

        if( !this.dragEvents ) {
          
          this.dragEvents = {
            dragover: function(e) {
              if (e.preventDefault) e.preventDefault();
              e.dataTransfer.dropEffect = 'move';
              return false;
            },

            dragenter: function(e) {
              if (e.preventDefault) e.preventDefault();
              $($el).addClass( "over" );
            },

            dragleave: function(e) {
              if (e.preventDefault) e.preventDefault();
              $($el).removeClass( "over" );
            },

            drop: function(e) {
              if (e.preventDefault) e.preventDefault(); 
              if (e.stopPropagation) e.stopPropagation();
              $($el).removeClass( "over" );
              var item = e.dataTransfer.getData( "item" );
              if( item ) {
                item = JSON.parse( e.dataTransfer.getData( "item" ) );
                self.$receive( "moved", item );
              } else self.$receive( "error", "no drop data at 'item'" );
            }
          }
        }

        this.dragEvents.$map( function( e, f ) {
          $el.removeEventListener( e, f );
          if( self.$data.editable ) { 
            $el.addEventListener( e, f );
          }
        });

      },

      notifySelectedSlot: function() {
        this.$sendParent( "slot", { 
          year: this.$data.day.year(),
          month: this.$data.day.month(),
          date: this.$data.day.date(),
          hour: this.$data.time.hour(),
          min: this.$data.time.minutes(),
          day: this.$data.day.weekday()
        });  
      }

    }

  });

  Vue.$comp( "calendar-month-slot", {
    many: true,
    silent: true, 
    data: { items: [], key: "", editable: true },
    props: [ "day" ],
    states: {
      READY: {
        init: { then: "READY", and: [ "setupDrop", "subscribe" ] },
        subscribe: { then: "READY", and: "subscribe" },
        add: { if: { eq: "editable", value: "true" }, then: "READY", and: "notifySelectedSlot" },
        draw: { then: "READY", and: "push" },
        clear: { then: "READY", and: [ "clearItems", "setupDrop" ]},
        selected: {then: "READY", and: { send: "selected", to: "$parent" } },
        delete: { then: "READY", and: "deleteItem" },
        moved: { then: "READY", and: "movedItem" }
      }
    },

    listen: {
      events: {
        $parent: {clear: "clear", subscribe: "subscribe" }
      }
    },

    hooks: {
      $init: "init"
    },

    methods: {

      $reset: function() {
        //this.$unsubAll();
        this.clearItems();
        //this.$unsub( "$parent", this.$data.key );
      },

      subscribe: function() {
        if( this.$data.key.length ) this.$unsub( "$parent", this.$data.key);
        var m = this.$data.day.time;
        this.$data.key = "item-" + m.year() + "-" + (m.month()+ 1) + "-" + m.date();
        this.$sub( "$parent", this.$data.key, "draw" );
      },

      clearItems: function(msg) {
        this.$data.editable = msg;
        this.$data.items=[];  
      },

      push: function(msg) {
        this.$data.items.$removeUsing( "id", msg.id );
        msg.start_day = parseInt(msg.start_day);
        msg.start_hour = parseInt(msg.start_hour);
        msg.start_min = parseInt(msg.start_min);
        msg.end_day = parseInt(msg.end_day);
        msg.end_hour = parseInt(msg.end_hour);
        msg.end_min = parseInt(msg.end_min);                  
        this.$data.items.push(msg);
        this.$sub( "$parent", "delete-" + msg.id, "delete");
      },

      deleteItem: function(msg) {
        this.$data.items.$removeUsing( "id", msg.id );    
      },

      movedItem: function(item) {
        this.$sendParent( "moved", { item: item, day: this.$data.day.time });
      },

      setupDrop: function() {
        var self = this;
        var $el = this.$el;

        if( !this.dragEvents ) {
          
          this.dragEvents = {
            dragover: function(e) {
              if (e.preventDefault) e.preventDefault();
              e.dataTransfer.dropEffect = 'move';
              return false;
            },

            dragenter: function(e) {
              if (e.preventDefault) e.preventDefault();
              $($el).addClass( "over" );
            },

            dragleave: function(e) {
              if (e.preventDefault) e.preventDefault();
              $($el).removeClass( "over" );
            },

            drop: function(e) {
              if (e.preventDefault) e.preventDefault(); 
              if (e.stopPropagation) e.stopPropagation();
              $($el).removeClass( "over" );
              var item = e.dataTransfer.getData( "item" );
              if( item ) {
                item = JSON.parse( e.dataTransfer.getData( "item" ) );
                self.$receive( "moved", item );
              } else self.$receive( "error", "no drop data at 'item'" );
            }
          }
        }

        this.dragEvents.$map( function( e, f ) {
          $el.removeEventListener( e, f );
          if( self.$data.editable ) { 
            $el.addEventListener( e, f );
          }
        });

      },

      notifySelectedSlot: function() {
        var m = this.$data.day.time;
        this.$sendParent( "slot", { 
          year: m.year(),
          month: m.month() + 1,
          day: m.date()
        });  
      }

    }

  });




  Vue.filter( 'shift-date', function( shift ){
    var lang = Vue.$lang();
    if( shift.day && shift.month && shift.year ) {
      return "{0} {1} {2}".$format([
        shift.day,
        i18n.months[ lang ][shift.month-1],
        shift.year
      ]);
    } else if( shift.start_day ) {
      return i18n.weekdays[ lang ][shift.start_day];  
    }
  });

  var shiftConflict = function(shift){ 
    return "<div><i class='fa fa-exclamation-triangle'></i> " + Vue.$i18n( "shift_conflict" ) + "</div>";
  }

  var shiftXtime = function(shift){ 
    return "<div><i class='fa fa-exclamation-triangle'></i> " + Vue.$i18n( "shift_xtime" ).$format([ shift.xduration ]) + "</div>";
  }


  var shiftName = function(shift) {
    return "<div><span style='margin-right: 5px;'><i class='fa fa-info-circle'></i></span> {0}</div>"
      .$format([
        shift.name
      ]);
  }

  var shiftAssignment = function(shift) {
    if( shift.status === 'assigned' && shift.assignedto ){
      var user = syncGetUser( shift.assignedto );
      return "<div><span style='margin-right: 5px;'><i class='fa fa-user'></i></span> {0}</div>"
        .$format([ user? (user.first + " " + user.last) : "-" ]);
    } return shiftUnassignedLabel();
  }


  var shiftAssignee = function(shift) {
    if( shift.status === 'assigned' && shift.assignedto ) {
      var user = syncGetUser( shift.assignedto );
      if( user ) return user.first + " " + user.last;
    }
    return "";
  }

  Vue.filter( 'shift-assignee', function(shift) {
    return shiftAssignee (shift);
  });

  Vue.filter( 'shift-preview', function( shift ) {
    var html = "<div class='text-left nowrap'>"
 
    if( shift.type != 'available' ) html += shiftName( shift );
    if( !shift.staffing && shift.type != 'available' ) html += shiftAssignment( shift ); 
    
    html += shiftTypeLabel( shift.type );

    if( shift.day && shift.month && shift.year ) html += shiftDate( shift );
    html += shiftTimes( shift );
    if( shift.staffing ) html += shiftStaffing( shift ); 
    if( shift.conflict === 'true' ) html += shiftConflict( shift);
    if( shift.xtime === 'true' ) html += shiftXtime( shift);
    html+= "</div>";
    html+= "</div>";
    return html;
  });

  Vue.$comp( "calendar-item", {
    many: true,
    silent: true,
    props: [ "item", "sibblings", "index", "editable" ],
    computed: {
      duration: function() {
        var start = Vue.$date();
        start.day( this.item.start_day );
        start.hour( this.item.start_hour );
        start.minute( this.item.start_min );
        start.second( 0 );
        var end = Vue.$date();
        end.day( this.item.end_day );
        end.hour( this.item.end_hour );
        end.minute( this.item.end_min );
        end.second( 0 );
        return end.diff( start, "minutes" );
      },

      tabindex: function() {
        return this.item.focus ? -1 : 1;
      },

      width: function() {
        var sep = this.sibblings - 1;
        return (Math.floor(( (this.editable ? 85 : 100)-sep)/this.sibblings ));
      },

      left: function() {
        return this.index * ( this.width + 1);
      },

      tooltipPlacement: function() {
        if( this.item.end_hour > 17)
          return "top";
        else return "bottom";
      },

      conflict: function() {
        return this.item.conflict === 'true' ? 'conflict' : '';
      },

      xtime: function() {
        return this.item.xtime === 'true' ? 'xtime' : '';
      }

    },

    ready: function() {
      $( this.$el ).tooltip();
      if( this.$data.item.focus ) { 
        $(this.$el).focus();
      }
    },

    states: {
      READY: {
        init: { then: "READY", and: "setDrag" },
        open: { then: "READY", and: "notifySelected" },
        shift_down: { then: "READY", and: "shiftDown" },
        shift_up: { then: "READY", and: "shiftUp" },
        up: { then: "READY", and: "notifyUp" },
        down: { then: "READY", and: "notifyDown" },
        alt_down: { then: "READY", and: "altDown" },
        alt_up: { then: "READY", and: "altUp" },
        left: { then: "READY", and: "notifyLeft" },
        right: { then: "READY", and: "notifyRight" },
      }
    },

    hooks: {
      $init: "init"
    },

    methods: {
      setDrag: function() {
        var $el = this.$el;
        if( !$el ) return ;

        var item = this.$data.item;
        var self = this;

        if( !this.dragEvents ) {
          this.dragEvents = {
            dragstart: function(e) {
              e.dataTransfer.setData('item', JSON.stringify(item));
            }
          }
        }

        this.dragEvents.$map( function( e, f ){
          $el.removeEventListener( e, f );
          if( self.$data.editable ) $el.addEventListener( e, f );
        });

      },

      notifySelected: function() {
        this.$sendParent( "selected", this.$data.item );
      },

      shiftUp: function() {
        this.$data.item.shift_key = false;
      },

      shiftDown: function() {
        this.$data.item.shift_key = true;
      },

      altDown: function() {
        this.$data.item.alt_key = true;
      },

      altUp: function() {
        this.$data.item.alt_key = false;
      },

      notifySelected: function() {
        this.$sendParent( "selected", this.$data.item );
      },

      notifyLeft: function(){ 
        this.$sendParent( this.$data.item.shift_key ? "copy_prev_day" : "moved_prev_day", this.$data.item );
      },

      notifyRight: function(){
        this.$sendParent( this.$data.item.shift_key ? "copy_next_day" : "moved_next_day", this.$data.item );
      },

      notifyUp: function() {
        if( this.$data.item.shift_key ) {
          this.$sendParent( "end_earlier", this.$data.item );
        } else this.$sendParent( "moved_earlier", this.$data.item );
      },

      notifyDown: function() {
        if( this.$data.item.shift_key ) {
          this.$sendParent( "end_later", this.$data.item );
        } else this.$sendParent( "moved_later", this.$data.item );
      }


    }
  });

  Vue.$comp( "calendar-month-item", {
    many: true,
    silent: true,
    props: [ "item", "sibblings", "editable" ],
    computed: {
      duration: function() {
        var start = Vue.$date();
        start.day( this.item.start_day );
        start.hour( this.item.start_hour );
        start.minute( this.item.start_min );
        start.second( 0 );
        var end = Vue.$date();
        end.day( this.item.end_day );
        end.hour( this.item.end_hour );
        end.minute( this.item.end_min );
        end.second( 0 );
        return end.diff( start, "minutes" );
      },

      tooltipPlacement: function() {
        //if( this.item.start_day == 6 || this.item.start_day == 0 
        //   || this.sibblings == 1 ) 
         return "bottom";
        //else return "right";
      },

    },

    ready: function() {
      $( this.$el ).tooltip();
    },

    states: {
      READY: {
        init: { then: "READY", and: "setDrag" },
        open: { then: "READY", and: "notifySelected" },
      }
    },

    hooks: {
      $init: "init"
    },

    methods: {
      setDrag: function() {
        var $el = this.$el;
        if( !$el ) return ;

        var item = this.$data.item;
        var self = this;

        if( !this.dragEvents ) {
          this.dragEvents = {
            dragstart: function(e) {
              e.dataTransfer.setData('item', JSON.stringify(item));
            }
          }
        }

        this.dragEvents.$map( function( e, f ){
          $el.removeEventListener( e, f );
          if( self.$data.editable ) $el.addEventListener( e, f );
        });

      },
      
    }
  });

  Vue.$comp( "loading", { many: true });
  Vue.$comp( "spinner", { many: true });


  Vue.$comp( "dashboard" );


})();
