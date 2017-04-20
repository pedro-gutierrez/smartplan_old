Feature: Customize schedules

  Background:
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And user with email sicozu@me.com invited to the current organization by user sicozu@mac.com with password foo
    And schedule template with name Template1 created by user sicozu@mac.com with password foo
    And shift of type std for monday between 12:00 and 14:00 with staffing 2 in the current template
    And schedule from the current template and the current week
  
  Scenario: Modify published schedule
    When I start the schedule engine for the current schedule
    Given the current schedule is in state computed 
    When I publish the current schedule
    Then it should be ok
    And response should have property status equal to published
    When I get the shifts for the current schedule
    Then response should have 2 items
    And item 0 should have property published equal to true
    And item 1 should have property published equal to true
    When I add a new shift of type std to the current schedule for tuesday of the current week between 12:00 and 14:00
    Then it should be ok
    When I remember the current response as the current shift
    And I assign the current shift to user with email sicozu@mac.com
    Then it should be ok
    When I get the current schedule
    Then it should be ok
    And response should have property status equal to draft
    When I get the shifts for the current schedule
    Then response should have 3 items
    And item 0 should have property published equal to true
    And item 1 should have property published equal to true
    And item 2 should have property published equal to false 
    When I publish the current schedule
    Then it should be ok
    When I get the shifts for the current schedule
    Then response should have 3 items
    And item 0 should have property published equal to true
    And item 1 should have property published equal to true
    And item 2 should have property published equal to true
    Given an anonymous user
    When I login as user sicozu@mac.com and password foo
    And I get my shifts for the current week
    Then it should be ok 
    And response should have 2 items
    And item 0 should have property published equal to true
    And item 1 should have property published equal to true
    When I get my shifts for the current month
    Then it should be ok 
    And response should have 2 items
    And item 0 should have property published equal to true
    And item 1 should have property published equal to true
    Given an anonymous user
    When I login as user sicozu@me.com and password bar 
    And I get my shifts for the current week
    Then it should be ok 
    And response should have 1 items
    And item 0 should have property published equal to true

