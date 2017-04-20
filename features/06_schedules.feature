Feature: Schedule Templates

  Scenario: Manage my templates
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    Given an anonymous user 
    When I login as user sicozu@mac.com and password foo
    And I remember my organization at position 0 as the current organization
    And I create a schedule template with name Template1 in the current organization
    Then it should be ok
    When I create a schedule template with name Template1 in the current organization
    Then there should be a conflict
    When I create a schedule template with name Template2 in the current organization
    Then it should be ok
    When I get my schedule templates in the current organization
    Then it should be ok
    And response should have 2 items
    And item 0 should have property name equal to Template1
    And item 1 should have property name equal to Template2
    When I remember my schedule template at position 0 as the current template
    And I rename the current schedule template as Template2
    Then there should be a conflict
    When I rename the current schedule template as Template3
    Then it should be ok
    When I get my schedule templates in the current organization
    Then it should be ok
    And response should have 2 items
    And item 0 should have property name equal to Template2
    And item 1 should have property name equal to Template3 
    When I rename the current schedule template as Template3
    Then it should be ok  
  
  Scenario: Change owner of template
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And schedule template with name Template1 created by user sicozu@mac.com with password foo
    And I invite user with email sicozu@me.com to the current organization
    And I set user with email sicozu@me.com as owner of the current template
    Then it should be ok
    When I rename the current schedule template as Template3
    Then it should be forbidden
    Given an anonymous user
    When I login as user sicozu@me.com and password bar 
    When I rename the current schedule template as Template3
    Then it should be ok
    When I set user with email sicozu@mac.com as owner of the current template
    Then it should be ok
    When I rename the current schedule template as Template3
    Then it should be forbidden
    Given an anonymous user
    When I login as user sicozu@mac.com and password foo 
    When I rename the current schedule template as Template3
    Then it should be ok


  Scenario: Template shifts 
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And schedule template with name Template1 created by user sicozu@mac.com with password foo
    When I remember my organization at position 0 as the current organization
    When I remember my schedule template at position 0 as the current template
    When I add a new shift to the current schedule template for monday between 12:00 and 14:00 with staffing 1
    Then it should be ok
    When I get the shifts for the current schedule template
    Then it should be ok
    And response should have 1 items
    And item 0 should have property start_day equal to 1
    And item 0 should have property start_hour equal to 12
    And item 0 should have property start_min equal to 0
    And item 0 should have property end_day equal to 1
    And item 0 should have property end_hour equal to 14
    And item 0 should have property end_min equal to 0
    And item 0 should have property staffing equal to 1
    When I remember my template shift at position 0 as the current shift
    And I set the current template shift for thursday between 08:00 and 22:00 with staffing 3
    Then it should be ok
    When I get the shifts for the current schedule template
    Then it should be ok
    And response should have 1 items
    And item 0 should have property type equal to std
    And item 0 should have property start_day equal to 4 
    And item 0 should have property start_hour equal to 8 
    And item 0 should have property start_min equal to 0
    And item 0 should have property end_day equal to 4
    And item 0 should have property end_hour equal to 22 
    And item 0 should have property end_min equal to 0
    And item 0 should have property staffing equal to 3 

  @current  
  Scenario: idle time template shift
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And schedule template with name Template1 created by user sicozu@mac.com with password foo
    When I remember my organization at position 0 as the current organization
    When I remember my schedule template at position 0 as the current template
    When I add a new shift of type idle to the current schedule template for monday between 12:00 and 14:00 with staffing 1
    Then it should be ok
    When I get the shifts for the current schedule template
    Then it should be ok
    And response should have 1 items
    And item 0 should have property type equal to idle

