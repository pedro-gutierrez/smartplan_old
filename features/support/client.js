var rest = require( 'restler' ),
    path = require( 'path' ),
    fs  = require( 'fs' );
    

var CONTENT_TYPES = {
  text : "text/plain", 
  json: "application/json",
  png: "image/png"
}


module.exports = rest.service( function(w) { 

    this.w = w;
    this.headers = {};

    if( w && w.session ) this.headers.session = w.session;
    this.headers.lang = "en";
    this.headers.app = "a66b914d-ef72-43d4-b8b2-c19e9035e8e2"

  }, {}, {

  url_for: function( path ) {
      return ( this.w.baseUrl ||  'http://127.0.0.1:7004')+path;
  },

  post: function( path, d ) {
      if( !d ) {
        return rest.post( this.url_for(path), { headers: this.headers } );
      } else {
        return rest.json( this.url_for(path), d, { headers: this.headers }, 'POST' );
      }
  },

  postFile: function( path, opts ){
      return rest.post( this.url_for(path), { multipart: true, data: { 
        //'title' : opts.title,
        //'type' : opts.type,
        'file'  : rest.file( opts.file, null, opts.size, null, CONTENT_TYPES[opts.type] ),
      }, headers: this.headers } ); 
  },

  put: function( path, d ) {
      if( !d ) {
        return rest.put( this.url_for(path), { headers: this.headers } );
      } else {
        return rest.json( this.url_for(path), d, { headers: this.headers }, 'PUT' );
      }
  },

  del: function( path, q ) {
      return rest.del( this.url_for(path), { headers: this.headers, query:q } );
  },

  get: function( path, q ) {
      return rest.get( this.url_for(path), { headers: this.headers, query:q } );
  },

  dropApp: function() {
      return this.post( '/api/app.drop' );
  },

  createApp: function() {
      return this.post( '/api/app.init' );  
  },

  getApp: function() {
      return this.post( '/api/app.get' );  
  },

  signup: function(first, last, email) {
      return this.post( '/api/signup', {first: first, last: last, email: email} );  
  },

  resetPassword: function(password, token, email) {
      return this.post( '/api/passwd.reset', {password: password, password_confirm: password, token: token, email: email} );  
  },

  forgotPassword: function(email) {
      return this.post( '/api/passwd.forgot', {email: email});  
  },

  login: function(u, p) {
      return this.post( '/api/signin', {email: u, password: p} );  
  },

  logout: function(u, p) {
      return this.post( '/api/signout', {email: u, password: p} );  
  },

  setProfile: function(first, last) {
      return this.post( '/api/profile.set', {first: first, last: last});  
  },

  getProfile: function(id) {
      return this.get( '/api/profile.get', {id: id} );  
  },

  getMyProfile: function() {
      return this.get( '/api/my.profile.get' );
  },

  createAsset: function(path, size, type) {
      return this.postFile( '/api/asset.create', { file: path, size: size, type: type })
  },

  getAsset: function(id) {
      return this.get( '/api/asset.get', {id: id} );  
  },

  getMyAvatar: function(id) {
      return this.get( '/api/my.avatar.get' );
  },

  setMyAvatar: function(file) {
      var asset = path.resolve( __dirname, file );
      var ext = path.extname( asset ).substr(1);
      var stats = fs.statSync(asset);
      return this.postFile( '/api/my.avatar.set', { file: asset, size: stats.size, type: ext });  
  },

  getAcls: function() {
      return this.get( '/api/acls.get' );
  },
  
  createOrganization: function(n) {
    return this.post( '/api/create.organization', {name: n});
  },
  
  getOrganizations: function(n) {
    return this.get( '/api/get.organizations');
  },

  addOrganizationMember: function(id, email) {
    return this.post( '/api/add.organization.member', {id: id, email: email});
  },

  getOrganizationMembers: function(id){
    return this.get( '/api/get.organization.members', {id:id});
  },

  removeOrganizationMember: function(id, member) {
    return this.get( '/api/remove.organization.member', {id: id, member: member});
  },

  setOrganizationOwner: function( id, owner ) {
    return this.post( '/api/set.organization.owner', {id: id, owner: owner}); 
  },
    
  renameOrganization: function( id, name ) {
    return this.post( '/api/rename.organization', {id: id, name: name}); 
  },

  addOrganizationTag: function( id, tag ) {
    return this.post( '/api/add.organization.tag', {id: id, tag: tag});
  },

  getOrganizationTags: function( id ) {
    return this.get( '/api/get.organization.tags', { id: id } );
  },

  addTagToOrganizationMember: function( organization, member, tag ) {
    return this.post( '/api/add.organization.member.tag', {id: organization, member: member, tag: tag });
  },
  
  removeTagFromOrganizationMember: function( organization, member, tag ) {
    return this.post( '/api/remove.organization.member.tag', {id: organization, member: member, tag: tag });
  },

  getOrganizationMemberTags: function( organization, member ) {
    return this.get( '/api/get.organization.member.tags', {id: organization, member: member});
  },

  createScheduleTemplate: function( o, n ) {
    return this.post( '/api/create.schedule.template', { organization: o, name: n });
  },

  getScheduleTemplates: function( o ) {
    return this.get( '/api/get.schedule.templates', { organization: o } );
  },

  renameScheduleTemplate: function( o, t, n ) {
    return this.post( '/api/rename.schedule.template', { organization: o, template: t, name: n } );
  },

  getScheduleTemplate: function( o, t ) {
    return this.get( '/api/get.schedule.template', { organization: o, template: t });
  },

  getScheduleTemplateTags: function( o, t ) {
    return this.get( '/api/get.schedule.template.tags', {organization: o, template: t});
  },

  addScheduleTemplateTag: function( o, t, tag ) {
    return this.post( '/api/add.schedule.template.tag', {organization: o, template: t, tag: tag });
  },

  removeScheduleTemplateTag: function( o, t, tag ) {
    return this.post( '/api/remove.schedule.template.tag', {organization: o, template: t, tag: tag });
  },

  setScheduleTemplateOwner: function( o, t, owner ) {
    return this.post( '/api/set.schedule.template.owner', {organization: o, template: t, owner:owner });
  },

  addScheduleTemplateShift: function( o, t, type, staffing, sd, sh, sm, ed, eh, em ) {
    return this.post( '/api/add.schedule.template.shift', 
        {organization: o, template: t, type: type, staffing: staffing, start_day: sd, start_hour: sh, start_min: sm, end_day: ed, end_hour: eh, end_min: em });
  },

  addScheduleShift: function( o, s, type, y, m, d, sd, sh, sm, ed, eh, em ) {
    return this.post( '/api/add.schedule.shift', 
        {organization: o, schedule: s, type: type, year: y, month: m, day: d, start_day: sd, start_hour: sh, start_min: sm, end_day: ed, end_hour: eh, end_min: em });
  },

  assignScheduleShift: function( o, s, shift, uid) {
    return this.post( '/api/assign.schedule.shift',
        {organization: o, schedule: s, shift: shift, assignedto: uid });
  },
  
  updateScheduleTemplateShift: function( o, t, shift, type, staffing, sd, sh, sm, ed, eh, em ) {
    return this.post( '/api/update.schedule.template.shift',
        {organization: o, template: t, shift: shift, type: type, staffing: staffing, start_day: sd, start_hour: sh, start_min: sm, end_day: ed, end_hour: eh, end_min: em });
  },

  removeScheduleTemplateShift: function( o, t, sh ) {
    return this.post( '/api/move.schedule.template.shift', {organization: o, template: t, shift: sh });
  },

  getScheduleTemplateShifts: function( o, t ) {
    return this.get( '/api/get.schedule.template.shifts', {organization: o, template: t });
  },

  addScheduleTemplateShiftTag: function( o, t, sh, tag ) {
    return this.post( '/api/add.schedule.template.shift.tag', {organization: o, template: t, shift: sh, tag: tag });
  },

  removeScheduleTemplateShiftTag: function( o, t, sh, tag ) {
    return this.post( '/api/remove.schedule.template.shift.tag', {organization: o, template: t, shift: sh, tag: tag });
  },

  getScheduleTemplateShift: function( o, t, s ) {
    return this.post ('/api/get.schedule.template.shift', {organization: o, template: t, shift: s});
  },

  createScheduleProfile: function( o, n ) {
    return this.post( '/api/create.schedule.profile', { organization: o, name: n } );
  },

  getScheduleProfiles: function( o ) {
    return this.get( '/api/get.schedule.profiles', {organization: o} );
  },
  
  renameScheduleProfile: function( o, p, n ) {
    return this.post( '/api/rename.schedule.profile', {organization: o, profile: p, name: n });
  },

  getScheduleProfile: function( o, p ) {
    return this.get( '/api/get.schedule.profile', {organization: o, profile: p} );
  },

  getScheduleIndicators: function(o) {
    return this.get( '/api/get.schedule.indicators', {organization: o} );
  },

  createScheduleProfileRule: function( o, p, i, s, op, v ) {
    return this.post( '/api/create.schedule.profile.rule', {organization:o, profile: p, indicator: i, strong: s, op: op, value: v }); 
  },

  getScheduleProfileRules: function( o, p ) {
    return this.get( '/api/get.schedule.profile.rules', {organization:o, profile: p });
  },

  removeScheduleProfileRule: function( o, p, r ) {
    return this.post( '/api/remove.schedule.profile.rule', {organization: o, profile: p, rule: r });
  },

  updateScheduleProfileRule: function( o, p, r, s, op, v ) {
    return this.post( '/api/update.schedule.profile.rule', {organization:o, profile: p, rule: r, strong: s, op: op, value: v }); 
  },
  
  getSchedules: function( o ) {
    return this.get( '/api/get.schedules', {organization: o} );
  },

  getScheduleByWeek: function( o, t, w, y ) {
    return this.get( '/api/get.schedule.by.week', {organization: o, template: t, week: w, year: y }); 
  },

  getScheduleById: function( o, s ) {
    return this.get( '/api/get.schedule.by.id', {organization: o, schedule:s });
  },

  applyScheduleTemplate: function( o, t, w, y ) {
    return this.post( '/api/create.schedule', {organization: o, template: t, week: w, year: y });
  },

  getScheduleShifts: function( o, s ) {
    return this.get( '/api/get.schedule.shifts', {organization: o, schedule: s} );
  },

  getScheduleStats: function( o, s ) {
    return this.get( '/api/get.schedule.stats', {organization: o, schedule: s} );
  },
  
  startScheduleEngine: function( o, s ) {
    return this.post( '/api/run.schedule', { organization: o, schedule: s } );
  },

  publishSchedule: function(o, s) {
    return this.post( '/api/publish.schedule', { organization: o, schedule: s } );
  },

  getMyWeeklyShifts: function(w, y) {
    return this.get( '/api/get.my.weekly.shifts', { week: w, year: y } );
  },

  getMyMonthlyShifts: function(m, y) {
    return this.get( '/api/get.my.monthly.shifts', { month: m, year: y } );
  }


}); 
