Feature: Schedules

  Background:
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And tag devs defined for the current organization
    And tag testers defined for the current organization
    And schedule template with name Template1 created by user sicozu@mac.com with password foo
    And schedule profile with name permanent for the current organization
    And strong rule with indicator std_time_w and value 2 for the current schedule profile
    And schedule profile with name contractor for the current organization
    And strong rule with indicator std_time_w and value 1 for the current schedule profile

  Scenario: exact staff, with 1 user, no tags, no constraints
    Given shift of type std for monday between 12:00 and 14:00 with staffing 1 in the current template
    When I get the schedules for the current organization
    Then it should be ok
    And response should have 0 items
    When I get the schedule for the current template and the current week
    Then it should be not found
    When I apply the current schedule template to the current week
    Then it should be ok
    When I get the schedules for the current organization
    Then it should be ok
    And response should have 1 items
    And item 0 should have property status equal to draft 
    And item 0 should have property year equal to the current year
    And item 0 should have property week equal to the current week
    When I get the schedule for the current template and the current week
    Then it should be ok
    And response should have property status equal to draft 
    And response should have property year equal to the current year
    And response should have property week equal to the current week
    When I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    Then it should be ok
    Given the current schedule is in state computed 
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 1 items
    And item 0 should have property status equal to assigned
    And item 0 should have property year
    And item 0 should have property month
    And item 0 should have property day
    And item 0 should have property assignedto set
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 100

  Scenario: not enough staff, with 2 users, no tags, no contraints
    Given shift of type std for monday between 12:00 and 14:00 with staffing 2 in the current template
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    When I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 50
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 2 items

  Scenario: exact staff, with 2 users, no tags, no contraints
    Given an anonymous user
    When I login as user sicozu@mac.com and password foo
    And I invite user with email sicozu@me.com to the current organization
    Given shift of type std for monday between 12:00 and 14:00 with staffing 2 in the current template
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    When I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 100
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 2 items
    And item 0 should have property status equal to assigned
    And item 1 should have property status equal to assigned
    And item 0 should have property assignedto set
    And item 1 should have property assignedto set

  Scenario: overstaffing, with 2 users, no tags, no constraints
    Given an anonymous user
    When I login as user sicozu@mac.com and password foo
    And I invite user with email sicozu@me.com to the current organization
    Given shift of type std for monday between 12:00 and 14:00 with staffing 1 in the current template
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    When I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 100
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 1 items
    And item 0 should have property status equal to assigned
    And item 0 should have property assignedto set

  Scenario: exact staffing, with 2 users, 2 shifts, 2 shift tags, no constraints
    Given user with email sicozu@me.com member of the current organization
    And tag devs for user with email sicozu@mac.com in the current organization
    And tag testers for user with email sicozu@me.com in the current organization
    And shift of type std for monday between 12:00 and 14:00 with staffing 1 in the current template
    And shift of type std for tuesday between 12:00 and 14:00 with staffing 1 in the current template
    And tag devs for the template shift at position 0 
    And tag testers for the template shift at position 1
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    When I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 100
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 2 items
    And item 0 should have property status equal to assigned
    And item 0 should have property assignedto set
    And item 1 should have property status equal to assigned
    And item 1 should have property assignedto set

  Scenario: over and under staffing, with 2 users, 2 shifts, no matching tags for 1 shift, no constraints
    Given user with email sicozu@me.com member of the current organization
    And tag devs for user with email sicozu@mac.com in the current organization
    And tag devs for user with email sicozu@me.com in the current organization
    And shift of type std for monday between 12:00 and 14:00 with staffing 1 in the current template
    And shift of type std for tuesday between 12:00 and 14:00 with staffing 1 in the current template
    And tag devs for the template shift at position 0 
    And tag testers for the template shift at position 1
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    When I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 50 
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 2 items

  Scenario: exact staffing, with 1 user, 1 shift, no shift tags, 1 hard constraint not met
    Given tag permanent for user with email sicozu@mac.com in the current organization
    And shift of type std for monday between 12:00 and 13:00 with staffing 1 in the current template
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    And I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 50 
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 1 items
    And item 0 should have property status equal to assigned
    And item 0 should have property assignedto set
  
  Scenario: exact staffing, with 1 user, 2 shift, no shift tags, all constraints met
    Given tag permanent for user with email sicozu@mac.com in the current organization
    And shift of type std for monday between 12:00 and 13:00 with staffing 1 in the current template
    And shift of type std for monday between 15:00 and 16:00 with staffing 1 in the current template
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    And I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 100 
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 2 items
    And item 0 should have property status equal to assigned
    And item 0 should have property assignedto set
    And item 1 should have property status equal to assigned
    And item 1 should have property assignedto set

  Scenario: exact staffing, with 2 users, 1 shift, 1 shift tag, 1 hard constraint met, 1 user does not participate
    Given user with email sicozu@me.com member of the current organization
    And tag permanent for user with email sicozu@mac.com in the current organization
    And tag devs for user with email sicozu@mac.com in the current organization
    And tag permanent for user with email sicozu@me.com in the current organization
    And shift of type std for monday between 12:00 and 14:00 with staffing 1 in the current template
    And tag devs for the template shift at position 0 
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    And I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 100 
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 1 items
    And item 0 should have property status equal to assigned
    And item 0 should have property assignedto set

  @current 
  Scenario: exact staffing, with 1 user, 1 shift, no shift gags, std time satisfied, idle time violated, extra time satisfied
    Given schedule profile with name home for the current organization
    And strong rule with indicator std_time_w and value 2 for the current schedule profile
    And strong rule with indicator idle_time_w and value 1 for the current schedule profile
    And soft rule with indicator extra_time_w and value 1 for the current schedule profile
    And tag home for user with email sicozu@mac.com in the current organization
    And shift of type std for monday between 12:00 and 14:00 with staffing 1 in the current template
    And shift of type idle for monday between 14:00 and 14:45 with staffing 1 in the current template
    And shift of type std for monday between 15:00 and 15:40 with staffing 1 in the current template
    When I apply the current schedule template to the current week
    And I remember schedule at position 0 as the current schedule
    And I start the schedule engine for the current schedule
    And I get the schedules for the current organization
    Given the current schedule is in state computed 
    When I get the stats for the current schedule
    Then it should be ok
    And response should have property quality equal to 83
    When I get the shifts for the current schedule
    Then it should be ok
    And response should have 3 items
    And item 0 should have property status equal to assigned
    And item 0 should have property assignedto set
    And item 1 should have property status equal to assigned
    And item 1 should have property assignedto set
    And item 2 should have property status equal to assigned
    And item 2 should have property assignedto set

  @current
  Scenario: 1 shift, staffing 7, 1 user, 1 profile, 3 rules 


       

      
