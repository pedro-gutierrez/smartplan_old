(function() {

  Vue.$configure({
    verbose: true,
    session_key: "smsess",	
    app_key: "a66b914d-ef72-43d4-b8b2-c19e9035e8e2",
    ws: { 
      url: '/ws', 
      debug: true,
      reconnect: 3
    },

    http: {
      debug: true
    },

    calendar: {
      start_hour: 7,
      hours: 16,
      styles: {
        std: "info-background",
        extra: "danger-background",
        idle: "success-background",
        available: "success-background",
        training: "warning-background",
        conflict: "danger-background",
        xtime: "danger-background"
      }
    },

    status: {
      styles: {
        progress: "info-foreground",
        new: "warning-foreground",
        resolved: "success-foreground",
        closed: "primary-background",
      }
    },

    estimates: {
      styles: {
        needs_info: "info-foreground",
        two_days: "primary-foreground",
        one_week: "success-foreground",
        two_three_weeks: "warning-foreground",
        one_month: "danger-foreground"
      }
    },
    
    releases: {
      styles: {
        MVP: "info-foreground",
        "V1.0": "warning-foreground"
      },

      defaultRelease: "MVP"
    },


    rules: {
      status: {
        inactive: 'default',
        active: 'success'
      }
    },

    schedules: {
      status: {
        computed: 'success',
        running: 'info',
        queued: 'warning',
        pending: 'warning',
        draft: 'plain',
        published: 'primary',
        error: 'danger',
        unassigned: 'warning'
      }
    },

    issues: {
      status: {
        new: "warning",
        progress: "info",
        resolved: "success",
        closed: "primary",
      },
      
      estimates: {
        needs_info: "info",
        two_days: "primary",
        one_week: "success",
        two_three_weeks: "warning",
        one_month: "danger"
      },
      
      releases: {
        MVP: "info",
        "V1.0": "warning"
      }
    },

    severity: {
      styles: {
        normal: "success",
        info: "success",
        warn: "warning",
        error: "danger"
      }
    },

    constraints: {
      styles: {
        strong: "chart-green",
        soft: "chart-lightgray"
      }
    }
  });


})();
