var should = require( 'should'),
    shouldHttp = require('should-http'),
    fs = require( 'fs' ),
    Client = require( '../support/client.js' ),
    util = require('../support/util' ),
    moment = require( 'moment' ),
    fse = require('fs-extra'),
    path = require( 'path' ),
    http = util.http,
    withId = util.withId,
    RETRIES = 1000;

module.exports = function() {
  
  var w = this;

  var WEEKDAYS = { sunday: 0, monday: 1, tuesday: 2, wednesday: 3, thursday: 4, friday: 5, saturday: 6 };
  var UPLOAD_DIR = path.resolve( __dirname, "../../uploads" );
  
  this.Then(/^it should be ok$/, function (callback) {
    w.r.should.have.status(200);
    callback();
  });

  this.Then(/^property (\w+) should exist$/, function (prop, callback) {
    w.d.should.have.property(prop);
    callback();
  });

  this.Then(/^property (\w*) should equal (.+)$/, function (prop, value, callback) {
    w.d.should.have.property(prop);
    util.convert(w.d[prop]).should.equal(util.convert(value));
    callback();
  });

  this.Then(/^it should be forbidden$/, function (callback) {
    w.r.should.have.status(401);
    callback();
  });

  this.Then(/^there should be a conflict$/, function (callback) {
    w.r.should.have.status(409);
    callback();
  });

  this.Then(/^it should be not found$/, function (callback) {
    w.r.should.have.status(404);
    callback();
  });

  this.Then(/^response should have header (.+) equal to (.+)$/, function(h, v, callback) {
    w.r.headers.should.have.property(h);
    util.convert(w.r.headers[h]).should.equal( util.convert(v));
    callback();
  });

  this.Then(/^response should not be empty$/, function (callback) {
    w.r.headers.should.have.property( 'content-length' );
    w.r.headers['content-length'].should.be.above(0);
    callback();
  });

  this.Then(/^response should not have property (\w*)$/, function (p, callback) {
    w.d.should.not.have.property(p);
    callback();
  });

  this.Then(/^response should have property (\w*)$/, function (p, callback) {
    w.d.should.have.property(p);
    callback();
  });


  this.Then(/^response should have property (\w*) equal to the current year$/, function ( p, callback) {
    w.d.should.have.property(p)
    util.convert( w.d[p]).should.equal(util.convert(moment().year()));
    callback();
  });

  this.Then(/^response should have property (\w*) equal to the current week$/, function (p, callback) {
    w.d.should.have.property(p)
    util.convert( w.d[p]).should.equal(util.convert(moment().isoWeek()));
    callback();
  });

  this.Then(/^response should have property (\w*) equal to ([a-zA-z0-9_]*)$/, function (p, v, callback) {
    w.d.should.have.property(p, v);
    callback();
  });

  this.Then(/^response should have (\d+) items$/, function (count, callback) {
    w.d.should.have.length( util.convert(count) );
    callback();
  });

  this.Then(/^response should have property (\w*) with length (\d+)/, function (p, length, callback) {
    w.d.should.have.property(p);
    w.d[p].should.have.property('length', util.convert(length));
    callback();
  });

  this.Then(/^item (\d+) should have property (\w*)$/, function (i, p, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.have.property(p);
    callback();
  });

  this.Then(/^item (\d+) should not have property (\w*)$/, function (i, p, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.not.have.property(p);
    callback();
  });


  this.Then(/^item (\d+) should have property (\w*) equal to the current year$/, function (i, p, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.have.property(p)
    util.convert( w.d[i][p]).should.equal(util.convert(moment().year()));
    callback();
  });

  this.Then(/^item (\d+) should have property (\w*) equal to the current week$/, function (i, p, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.have.property(p)
    util.convert( w.d[i][p]).should.equal(util.convert(moment().isoWeek()));
    callback();
  });
  

  this.Then(/^item (\d+) should have property (\w*) equal to ([a-zA-Z0-9_]*)$/, function (i, p, v, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.have.property(p)
    util.convert( w.d[i][p]).should.equal(util.convert(v));
    callback();
  });
  
  this.Then(/^item (\d+) should have property (\w*) with length (\d+)$/, function (i, p, length, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.have.property(p);
    w.d[i][p].should.have.property('length', util.convert(length));
    callback();
  });

  this.Then(/^item (\d+) should have property (\w*) not set$/, function (i, p, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.have.property(p)
    should(w.d[i][p]).not.be.ok;
    callback();
  });

  this.Then(/^item (\d+) should have property (\w*) set$/, function (i, p, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.have.property(p)
    should(w.d[i][p]).be.ok;
    callback();
  });

  this.Then(/^item at position (\d+) should equal (.+)$/, function (i, value, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.equal( util.convert(value));
    callback();
  });
 
  this.Then(/^item (\d+) should equal (.+)$/, function (i, value, callback) {
    w.d.length.should.be.above(i);
    w.d[i].should.equal( util.convert(value));
    callback()
  });

  this.Then(/^response should have value (\w*) at item (\d+) of property (\w*)$/, function (v, i, p, callback) {
    w.d.should.have.property(p);
    w.d[p].should.have.property('length');
    util.convert(w.d[p][i]).should.equal(util.convert (v));
    callback();
  });


  this.When(/^I retrieve the app$/, function (callback) {
    new Client(w).getApp().addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      callback();
    });
  });
  
  this.When(/^I signup with name (\w*) (\w*) and email (.+)$/, {timeout: 15000}, function (first, last, email, callback ) {
    new Client(w).signup(first, last, email).addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      callback();
    });
  });
  
  this.When(/^I reset my password to (\w*) using (\w*) and email (.+)$/, function (p, t, e, callback) {
    new Client(w).resetPassword(p, w.d[t], e).addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      callback();
    });  
  });
  
  this.When(/^I login as user (.+) and password (\w*)$/, function (u, p, callback) {
    new Client(w).login(u, p).addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      w.session = w.d.session;
      callback();
    });    
  });
  
  this.When(/^I logout$/, function (callback) {
    new Client(w).logout().addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      callback();
    });
  });
  
  this.When(/^I forgot my password as (.+)$/, function (e, callback) {
    new Client(w).forgotPassword(e).addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      callback();
    });    
  });
  
  this.When(/^I set my first name to (\w*) and and my last name to (\w*)$/, function (f, l, callback) {
    new Client(w).setProfile(f, l).addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      callback();
    });      
  });
  
  this.When(/^I get my profile$/, function (callback) {
    new Client(w).getMyProfile().addListener( 'complete', function(d, r) {
      w.d = d;
      w.r = r;
      callback();
    }); 
  });
  
  this.When(/^I get profile using property (\w*)$/, function (prop, callback) {
    new Client(w).getProfile(w[prop]).addListener( 'complete', function(d, r){
      w.d = d;
      w.r = r;
      callback();
    });      
  });
  
  this.When(/^I remember property (\w*)$/, function (prop, callback) {
    w[prop]=w.d[prop];
    callback();
  });
  
  this.When(/^I upload (\w*) file (.+)$/, function (type, path, callback) {
    fs.stat( path, function (err, stats ) {
      if( err ) callback.fail( err );
      new Client(w).createAsset(path, stats.size, type).addListener( 'complete', function(d, r) {
        w.d = d;
        w.r = r;
        callback();
      }); 
    });    
  });
  
  this.When(/^I fetch asset using property (\w*)$/, function (prop, callback) {
    new Client(w).getAsset(w[prop]).addListener( 'complete', function(d, r) {
      w.d = d;
      w.r = r;
      callback();
    }); 
  });

  this.When(/^I get my avatar$/, function (callback) {
    new Client(w).getMyAvatar().addListener( 'complete', function(d, r) {
      w.d = d;
      w.r = r;
      callback();
    }); 
  });
  
  this.When(/^I set my avatar to (.+)$/, function (file, callback) {
    new Client(w).setMyAvatar(file).addListener( 'complete', function(d, r) {
      w.d = d;
      w.r = r;
      callback();
    });
  });
    
  this.When(/^I get my acls$/, function (callback) {
    new Client(w).getAcls().addListener( 'complete', function(d, r) {
        w.d = d;
        w.r = r;
        callback();
    });    
  });
  
  this.When(/^I create an organization with name (\w*)$/, function (n, callback) {
    http( w, callback, function(c) { return c.createOrganization(n) });
  });

  this.When(/^I get my organizations$/, function (callback) {
    http( w, callback, function(c) { return c.getOrganizations() });
  });

  this.When(/^I remember my organization at position (\d+) as the current organization$/, function (i, callback) {
    new Client(w).getOrganizations().addListener( 'complete', function( d, r){
      r.should.have.status(200);
      d.should.have.property( 'length' );
      d.length.should.be.above(i);
      w.currentOrganization = d[i].id;
      callback();
    });
  });
 
  this.When(/^I invite user with email (.+) to the current organization$/, function (email, callback) {
    http( w, callback, function(c) { 
      return c.addOrganizationMember( w.currentOrganization,  email); 
    });
  });
   
  this.When(/^I get the members of the current organization$/, function (callback) {
    http( w, callback, function(c) { 
      return c.getOrganizationMembers( w.currentOrganization ); 
    });
  });

  this.When(/^I remove user with email (.+) from the current organization$/, function (email, callback) {
    withOrganizationMember( w, w.currentOrganization, "email", email, function(user) {
      http( w, callback, function(c) { 
        return c.removeOrganizationMember( w.currentOrganization, user.id );
      });
    });
  });

  this.When(/^I set user with email (.+) as owner of the current organization$/, function (email, callback) {
    withOrganizationMember( w, w.currentOrganization, "email", email, function(user) {
      http( w, callback, function(c) { 
        return c.setOrganizationOwner( w.currentOrganization, user.id );
      });
    });
  });

  this.When(/^I rename the current organization as (\w*)$/, function (n, callback) {
    http( w, callback, function(c) { 
      return c.renameOrganization( w.currentOrganization, n );
    });
  });

  var withOrganizationMember = function( w, org, prop, value, next ) {
    new Client(w).getOrganizationMembers( org ).addListener( 'complete', function( d, r ) {
      r.should.have.status(200);
      d.should.have.property( 'filter' );
      var m = d.filter( function(i) { return util.convert(i[prop]) === util.convert( value); });
      m.should.have.length(1);
      next( m[0] );
    }); 
  }

  this.When(/^I add tag (\w*) to member with (\w*) (.+) of the current organization$/, function (tag, prop, value, callback) {
    withOrganizationMember( w, w.currentOrganization, prop, value, function(m) {
      http( w, callback, function(c) { 
        return c.addTagToOrganizationMember( w.currentOrganization, m.id, tag );
      });
    });
  });

  this.When(/^I remove tag (\w*) from member with (\w*) (.+) of the current organization$/, function (tag, prop, value, callback) {
    withOrganizationMember( w, w.currentOrganization, prop, value, function(m) {
      http( w, callback, function(c) { 
        return c.removeTagFromOrganizationMember( w.currentOrganization, m.id, tag );
      });
    });
  });
  
  this.When(/^I get the tags of member with (\w*) (.+) of the current organization$/, function (prop, value, callback) {
    withOrganizationMember( w, w.currentOrganization, prop, value, function(m) {
      http( w, callback, function(c) { 
        return c.getOrganizationMemberTags( w.currentOrganization, m.id );
      });
    });
  });

  this.When(/^I create a schedule template with name (\w*) in the current organization$/, function (n, callback) {
    http( w, callback, function(c){
      return c.createScheduleTemplate( w.currentOrganization, n );
    });
  });
      
  this.When(/^I get my schedule templates in the current organization$/, function (callback) {
    http( w, callback, function(c) {
      return c.getScheduleTemplates( w.currentOrganization );
    });
  });

  this.When(/^I remember my schedule template at position (\d+) as the current template$/, function (i, callback) {
    new Client(w).getScheduleTemplates( w.currentOrganization ).addListener( 'complete', function( d, r){
      d.should.have.property( 'length' );
      d.length.should.be.above(i);
      w.currentTemplate = d[i].id;
      callback();
    });
  });
      
  this.When(/^I rename the current schedule template as (\w*)$/, function (name, callback) {
    http( w, callback, function(c) {
      return c.renameScheduleTemplate( w.currentOrganization, w.currentTemplate, name);
    });
  });

  this.When(/^I add tag (\w*) to the current schedule template$/, function (t, callback) {
    http( w, callback, function(c) {
      return c.addScheduleTemplateTag( w.currentOrganization, w.currentTemplate, t );
    });
  }); 

  this.Given(/^tag (\w*) in the current template$/, function (tag, callback) {
    new Client(w).addScheduleTemplateTag( w.currentOrganization, w.currentTemplate, tag ).addListener( 'complete', function(d, r ) {
      r.should.have.status(200);
      w.d = d;
      w.r = r;
      callback();
    });
  });

  this.When(/^I get the tags for the current schedule template$/, function (callback) {
    http( w, callback, function(c) {
      return c.getScheduleTemplateTags( w.currentOrganization, w.currentTemplate );
    });
  });

  this.When(/^I get the current schedule template$/, function (callback) {
    http( w, callback, function(c) {
      return c.getScheduleTemplate( w.currentOrganization, w.currentTemplate );
    });
  });

  this.When(/^I remove tag (\w*) from the current schedule template$/, function (t, callback) {
    http( w, callback, function(c) {
      return c.removeScheduleTemplateTag( w.currentOrganization, w.currentTemplate, t);
    });
  });

  this.When(/^I set user with email (.+) as owner of the current template$/, function (email, callback) {
    withOrganizationMember( w, w.currentOrganization, "email", email, function(user) {
      http( w, callback, function(c) { 
        return c.setScheduleTemplateOwner( w.currentOrganization, w.currentTemplate, user.id );
      });
    });
  });

  this.Given(/^shift of type (\w*) for (\w*) between (\d+):(\d+) and (\d+):(\d+) with staffing (\d+) in the current template$/, function (type, sd, sh, sm, eh, em, staffing,callback) {
    sd  = WEEKDAYS[sd];
    new Client(w).addScheduleTemplateShift( w.currentOrganization, w.currentTemplate, type, staffing, sd, sh, sm, sd, eh, em ).addListener( 'complete', function(d, r) {
      r.should.have.status(200);
      w.d = d;
      w.r = r;
      callback();
    });
  });

  this.When(/^I add a new shift to the current schedule template for (\w*) between (\d+):(\d+) and (\d+):(\d+) with staffing (\d+)$/, 
      function (sday, shour, smin, ehour, emin, staffing, callback) {
    http( w, callback, function(c) {
      sday = WEEKDAYS[sday];
      return c.addScheduleTemplateShift( w.currentOrganization, w.currentTemplate, "std", staffing, sday, shour, smin, sday, ehour, emin );
    });
  }); 

  this.When(/^I add a new shift of type (\w*) to the current schedule template for (\w*) between (\d+):(\d+) and (\d+):(\d+) with staffing (\d+)$/, 
      function (type, sday, shour, smin, ehour, emin, staffing, callback) {
    http( w, callback, function(c) {
      sday = WEEKDAYS[sday];
      return c.addScheduleTemplateShift( w.currentOrganization, w.currentTemplate, type, staffing, sday, shour, smin, sday, ehour, emin );
    });
  });


  this.When(/^I set the current template shift for (\w*) between (\d+):(\d+) and (\d+):(\d+) with staffing (\d+)$/, function (sday, shour, smin, ehour, emin, staffing, callback) {
    http( w, callback, function(c) {
      sday = WEEKDAYS[sday];
      return c.updateScheduleTemplateShift( w.currentOrganization, w.currentTemplate, w.currentShift, "std", staffing, sday, shour, smin, sday, ehour, emin );
    });
  });

  this.When(/^I get the shifts for the current schedule template$/, function (callback) {
    http( w, callback, function(c){
      return c.getScheduleTemplateShifts( w.currentOrganization, w.currentTemplate );
    });
  });

  this.When(/^I delete the current template shifts $/, function (callback) {
    http( w, callback, function(c){
      return c.removeScheduleTemplateShift( w.currentOrganization, w.currentTemplate, w.currentShift );
    });
  });

  this.When(/^I get the current template shift$/, function (callback) {
    http( w, callback, function(c){
      return c.getScheduleTemplateShift( w.currentOrganization, w.currentTemplate, w.currentShift );
    });
  });

  this.When(/^I remember my template shift at position (\d+) as the current shift$/, function (i, callback) {
    new Client(w).getScheduleTemplateShifts( w.currentOrganization, w.currentTemplate ).addListener( 'complete', function( d, r){
      d.should.have.property( 'length' );
      d.length.should.be.above(i);
      w.currentShift = d[i].id;
      callback();
    });
  });

  this.Given(/^the root user$/, function (callback) {
    w.session = "010d8164-b8d4-445e-b4c7-ef9b1188c2ff";
    callback();
  });
  
  this.Given(/^an anonymous user$/, function (callback) {
    delete w.session;
    callback();
  });

  this.Given(/^a new app$/, function (callback) {
    util.initUploads();
    w.session = "010d8164-b8d4-445e-b4c7-ef9b1188c2ff";
    new Client(w).dropApp().addListener( 'complete', function(d, r){
      new Client(w).createApp().addListener( 'complete', function( d, r ){
        r.should.have.status(200);
        callback();
      });
    });
  });
  
  
  this.Given(/^user with name (\w*) (\w*), email (.+) and password (\w*)$/, function (first, last, email, password, callback) {
    delete w.session;
    new Client(w).signup(first, last, email).addListener( 'complete', function(d, r){
      new Client(w).resetPassword(password, d.token, email).addListener( 'complete', function(d, r){
        w.d = d;
        w.r = r;
        callback();
      });  
    });
  });
  
  this.Given(/^file with name (.+) and content (.+)$/, function (path, content, callback) {
    fs.writeFile( path, content, function(err) {
      if( err ) callback.fail(err);
      callback();
    });    
  });

  this.Given(/^organization with name (\w*) created by user (.+) with password (.+)$/, function (n, email, password, callback) {
    delete w.session;
    new Client(w).login(email, password).addListener( 'complete', function(d, r){
      r.should.have.status(200);
      w.session = d.session;
      new Client(w).createOrganization(n).addListener( 'complete', function(d, r) {
        r.should.have.status(200);
        w.r = r;
        w.d = d;
        w.currentOrganization = d.id;
        callback();
      });
    });
  });

  this.Given(/^schedule template with name (\w*) created by user (.+) with password (.+)$/, function (n, email, password, callback) {
    delete w.session;
    new Client(w).login(email, password).addListener( 'complete', function(d, r){
      r.should.have.status(200);
      w.session = d.session;
      new Client(w).createScheduleTemplate(w.currentOrganization, n).addListener( 'complete', function(d, r) {
        r.should.have.status(200);
        w.r = r;
        w.d = d;
        w.currentTemplate = d.id;
        callback();
      });
    });
  });

  this.Given(/^template shift of type (\w*) for (\w*) between (\d+):(\d+) and (\d+):(\d+) with staffing (\d+) created by user (.+) with password (.+)$/, 
      function (type, sday, sh, sm, eh, em, staffing, email, password, callback) {
    delete w.session;
    new Client(w).login(email, password).addListener( 'complete', function(d, r){
      r.should.have.status(200);
      w.session = d.session;
      sday = WEEKDAYS[sday];
      new Client(w).addScheduleTemplateShift( w.currentOrganization, w.currentTemplate, type, staffing, sday, sh, sm, sday, eh, em).addListener( 'complete', function(d, r) {
        r.should.have.status(200);
        w.r = r;
        w.d = d;
        w.currentShift = d.id;
        callback();
      });
    });
  });

  this.When(/^I add tag (\w*) to the current template shift$/, function (tag, callback) {
    http( w, callback, function(c){
      return c.addScheduleTemplateShiftTag( w.currentOrganization, w.currentTemplate, w.currentShift, tag );
    });
  });

  this.When(/^I remove tag (\w*) from the current template shift$/, function (tag, callback) {
    http( w, callback, function(c){
      return c.removeScheduleTemplateShiftTag( w.currentOrganization, w.currentTemplate, w.currentShift, tag );
    });
  });

  this.When(/^I add tag (\w*) to the current organization$/, function (tag, callback) {
    http( w, callback, function(c){
      return c.addOrganizationTag( w.currentOrganization, tag );
    });
  });

  this.Given(/^tag (\w*) defined for the current organization$/, function (tag, callback) {
    new Client(w).addOrganizationTag( w.currentOrganization, tag ).addListener( 'complete', function( d, r) {
      r.should.have.status( 200 );
      w.d = d;
      w.r = r;
      callback();
    });
  });

  this.When(/^I get the tags of the current organization$/, function (callback) {
    http( w, callback, function(c){
      return c.getOrganizationTags( w.currentOrganization );
    });
  });


  this.When(/^I create a new schedule profile with name (\w*) for the current organization$/, function (n, callback) {
    http( w, callback, function(c){
      return c.createScheduleProfile( w.currentOrganization, n );
    });
  });
  
  this.When(/^I get the schedule profiles for the current organization$/, function (callback) {
    http( w, callback, function(c){
      return c.getScheduleProfiles( w.currentOrganization );
    });
  });

  this.When(/^I remember schedule profile at position (\d+) as the current schedule profile$/, function (i, callback) {
    new Client(w).getScheduleProfiles( w.currentOrganization ).addListener( 'complete', function( d, r){
      d.should.have.property( 'length' );
      d.length.should.be.above(i);
      w.currentScheduleProfile = d[i].id;
      callback();
    });
  });
  
  this.When(/^I rename the current schedule profile as (\w*)$/, function (n, callback) {
    http( w, callback, function(c){
      return c.renameScheduleProfile( w.currentOrganization, w.currentScheduleProfile, n );
    });
  });

  this.When(/^I get the schedule indicators for the current organization$/, function (callback) {
    http( w, callback, function(c){
      return c.getScheduleIndicators( w.currentOrganization );
    });
  });

  this.Given(/^schedule profile with name (\w*) for the current organization$/, function (n, callback) {
    new Client(w).createScheduleProfile( w.currentOrganization, n ).addListener( 'complete', function( d, r ) {
      r.should.have.status(200);
      w.d = d;
      w.r = r;
      w.currentScheduleProfile = w.d.id;
      callback();
    });  
  });

  this.When(/^I add a (\w*) schedule rule with indicator (\w*) with value (\d+) to the current schedule profile$/, function (t, i, v, callback) {
    new Client(w).getScheduleIndicators( w.currentOrganization ).addListener( 'complete', function( d, r ) {
      r.should.have.status(200);
      d.should.have.property( 'length' );
      w.d = d;
      util.withId( w, 'name', i, callback, function(id) {
        new Client(w).createScheduleProfileRule( w.currentOrganization, w.currentScheduleProfile, id, ("strong" === t), "eq", v ).addListener( 'complete', function(d, r ) {
          w.d = d;
          w.r = r;
          callback();
        });
      });
    });
  });

  this.Given(/^(\w*) rule with indicator (\w*) and value (\d+) for the current schedule profile$/, function (t, i, v, callback) {
    new Client(w).getScheduleIndicators( w.currentOrganization ).addListener( 'complete', function( d, r ) {
      r.should.have.status(200);
      d.should.have.property( 'length' );
      w.d = d;
      util.withId( w, 'name', i, callback, function(id) {
        new Client(w).createScheduleProfileRule( w.currentOrganization, 
          w.currentScheduleProfile, id, ("strong" === t), "eq", v ).addListener( 'complete', function(d, r ) {
          w.d = d;
          w.r = r;
          callback();
        });
      });
    });
  });

  this.When(/^I get the rules for the current schedule profile$/, function (callback) {
    http( w, callback, function(c){
      return c.getScheduleProfileRules( w.currentOrganization, w.currentScheduleProfile );
    });
  });

  this.When(/^I remember item at position (\d+) as the current schedule rule$/, function (i, callback) {
    new Client(w).getScheduleProfileRules( w.currentOrganization, w.currentScheduleProfile ).addListener( 'complete', function( d, r){
      r.should.have.status(200);
      d.should.have.property( 'length' );
      d.length.should.be.above(i);
      w.currentScheduleProfileRule = d[i].id;
      callback();
    });
  });

  this.When(/^I remove the current schedule rule from the current schedule profile$/, function (callback) {
    http( w, callback, function(c){
      return c.removeScheduleProfileRule( w.currentOrganization, w.currentScheduleProfile, w.currentScheduleProfileRule );
    });
  });
      
  this.When(/^I update the current schedule rule to be (\w*) with value (\d+)$/, function (t, v, callback) {
    http( w, callback, function(c){
      return c.updateScheduleProfileRule( w.currentOrganization, w.currentScheduleProfile, w.currentScheduleProfileRule, ("strong" === t), "eq", v );
    });
  });

  this.When(/^I get the schedules for the current organization$/, function (callback) {
    http( w, callback, function(c){
      return c.getSchedules( w.currentOrganization );
    });
  });

  this.When(/^I get the schedule for the current template and the current week$/, function (callback) {
    var now = moment();
    http( w, callback, function(c){
      return c.getScheduleByWeek( w.currentOrganization, w.currentTemplate, now.isoWeek(), now.year() );
    });
  });
  
  this.When(/^I apply the current schedule template to the current week$/, function (callback) {
    var now = moment();
    http( w, callback, function(c){
      return c.applyScheduleTemplate( w.currentOrganization, w.currentTemplate, now.isoWeek(), now.year() );
    });
  });

  this.When(/^I remember schedule at position (\d+) as the current schedule$/, function (i, callback) {
    new Client(w).getSchedules( w.currentOrganization ).addListener( 'complete', function( d, r){
      r.should.have.status(200);
      d.should.have.property( 'length' );
      d.length.should.be.above(i);
      w.currentSchedule = d[i].id;
      callback();
    });
  });

  this.When(/^I get the shifts for the current schedule$/, function (callback) {
    http( w, callback, function(c){
      return c.getScheduleShifts( w.currentOrganization, w.currentSchedule );
    });
  });

  this.When(/^I start the schedule engine for the current schedule$/, function (callback) {
    http( w, callback, function(c){
      return c.startScheduleEngine( w.currentOrganization, w.currentSchedule );
    });
  });
  
  this.Given(/^the current schedule is in state (\w*)$/, function (s, callback) {
    util.retry( RETRIES, w, callback, function(c) {
      return c.getScheduleById( w.currentOrganization, w.currentSchedule );
    }, function(d, r) {
      return d.status === s;
    }, function(d, r) {
      return "expected ["+s+"] but got ["+d.status+"]"; 
    });
  });

  this.When(/^I get the stats for the current schedule$/, function (callback) {
    http( w, callback, function(c){
      return c.getScheduleStats( w.currentOrganization, w.currentSchedule );
    });
  });

  this.Given(/^user with email (.+) member of the current organization$/, function (e, callback) {
    new Client(w).addOrganizationMember( w.currentOrganization, e).addListener( 'complete', function( d, r) {
      w.r.should.have.status(200);
      w.d = d;
      w.r = r;
      callback();
    });
  });

  this.Given(/^tag (\w*) for user with email (.+) in the current organization$/, function (t, e, callback) {
     withOrganizationMember( w, w.currentOrganization, 'email', e, function(m) {
       new Client(w).addTagToOrganizationMember( w.currentOrganization, m.id, t).addListener( 'complete', function( d, r) {
         r.should.have.status(200);
         w.d = d;
         w.r = r;
         callback();
       });
     });
  });

  this.Given(/^tag (\w*) for the template shift at position (\d+)$/, function (t, i, callback) {
    new Client(w).getScheduleTemplateShifts( w.currentOrganization, w.currentTemplate ).addListener( 'complete', function (d, r) {
      r.should.have.status(200);
      d.length.should.be.above(i);
      new Client(w).addScheduleTemplateShiftTag( w.currentOrganization, w.currentTemplate, d[i].id, t ).addListener( 'complete', function( d, r) {
        r.should.have.status(200);
        w.d = d;
        w.r = r;
        callback();
      }); 
    });
  });
  
  this.Given(/^schedule from the current template and the current week$/, function (callback) {
    var now = moment();
    new Client(w).applyScheduleTemplate( w.currentOrganization, w.currentTemplate, now.isoWeek(), now.year() ).addListener( 'complete', function( d, r ) {
      r.should.have.status(200);
      w.d = d ;
      w.r = r;
      d.should.have.property('id');
      w.currentSchedule = d.id;
      callback();
    });
  });

  this.When(/^I publish the current schedule$/, function (callback) {
    http( w, callback, function(c){
      return c.publishSchedule( w.currentOrganization, w.currentSchedule );
    });
    
  });

  this.When(/^I get my shifts for the current week$/, function (callback) {
    var now = moment();        
    http( w, callback, function(c){
      return c.getMyWeeklyShifts( now.isoWeek(), now.year() );
    });
  });


  this.When(/^I get my shifts for the current month$/, function (callback) {
    var now = moment();        
    http( w, callback, function(c){
      return c.getMyMonthlyShifts(  now.month() +1, now.year() );
    });
  });

  this.When(/^I get the current schedule$/, function (callback) {
    http( w, callback, function(c) {
      return c.getScheduleById( w.currentOrganization, w.currentSchedule );
    });
  });

  this.Given(/^user with email (.+) invited to the current organization by user (.+) with password (\w*)$/, function (email1, email2, passwd, callback) {
    delete w.session;
    new Client(w).login(email2, passwd).addListener( 'complete', function(d, r){
      r.should.have.status(200);
      w.session = d.session;
      new Client(w).addOrganizationMember( w.currentOrganization, email1 ).addListener( 'complete', function(d, r){
        r.should.have.status(200);
        w.d = d;
        w.r = r;
        callback();
      });
    });
  });

  this.When(/^I add a new shift of type (\w*) to the current schedule for (\w*) of the current week between (\d+):(\d+) and (\d+):(\d+)$/, function (type, sday, sh, sm, eh, em, callback) {
    http( w, callback, function(c) { 
      sday = WEEKDAYS[sday];
      var m = moment();
      m.isoWeekday(sday);
      return c.addScheduleShift( w.currentOrganization, w.currentSchedule, type, m.year(),  m.month() + 1 , m.date(), sday, sh, sm, sday, eh, em );
    });
  });

  this.When(/^I remember the current response as the current shift$/, function (callback) {
    w.d.should.have.property('id');
    w.currentShift = w.d.id;  
    callback();
  });

  this.When(/^I assign the current shift to user with email (.+)$/, function (email, callback) {
    withOrganizationMember( w, w.currentOrganization, "email", email, function(user) {
      http( w, callback, function(c) { 
        return c.assignScheduleShift( w.currentOrganization, w.currentSchedule, w.currentShift, user.id );
      });
    });
  });

}
