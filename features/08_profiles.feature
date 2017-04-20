Feature: Schedule Profiles

  Scenario: Adding and renaming scheduling profiles
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    When I create a new schedule profile with name 35h for the current organization
    Then it should be ok
    When I create a new schedule profile with name 35h for the current organization
    Then there should be a conflict
    When I get the schedule profiles for the current organization
    Then it should be ok
    And response should have 1 items
    And item 0 should have property name equal to 35h
    When I get the tags of the current organization
    Then it should be ok
    And response should have 1 items
    And item at position 0 should equal 35h
    When I remember schedule profile at position 0 as the current schedule profile
    And I rename the current schedule profile as 40h
    Then it should be ok
    When I get the schedule profiles for the current organization
    Then it should be ok
    And response should have 1 items
    And item 0 should have property name equal to 40h
    And I rename the current schedule profile as 35h
    Then it should be ok
    When I get the schedule profiles for the current organization
    Then it should be ok
    And response should have 1 items
    And item 0 should have property name equal to 35h

  Scenario: Conflict with an existing tag
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    When I add tag 35h to the current organization
    Then it should be ok
    When I create a new schedule profile with name 35h for the current organization
    Then there should be a conflict
    When I get the schedule profiles for the current organization
    Then it should be ok
    And response should have 0 items

  Scenario: Scheduling Indicators
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    When I get the schedule indicators for the current organization
    Then it should be ok
    And response should have 3 items
    And item 0 should have property name equal to extra_time_w
    And item 1 should have property name equal to idle_time_w
    And item 2 should have property name equal to std_time_w

  Scenario: Manage profile rules  
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And schedule profile with name 35h for the current organization
    When I add a strong schedule rule with indicator std_time_w with value 35 to the current schedule profile
    Then it should be ok
    When I add a strong schedule rule with indicator idle_time_w with value 5 to the current schedule profile
    Then it should be ok
    When I add a weak schedule rule with indicator extra_time_w with value 10 to the current schedule profile
    Then it should be ok
    When I get the rules for the current schedule profile
    Then it should be ok
    And response should have 3 items
    And item 0 should have property value equal to 10
    And item 0 should have property strong equal to false
    And item 1 should have property strong equal to true
    And item 1 should have property value equal to 5
    And item 2 should have property strong equal to true
    And item 2 should have property value equal to 35
    When I remember item at position 2 as the current schedule rule
    And I remove the current schedule rule from the current schedule profile
    Then it should be ok
    When I get the rules for the current schedule profile
    Then it should be ok
    And response should have 2 items
    When I remember item at position 0 as the current schedule rule
    And I update the current schedule rule to be strong with value 8
    Then it should be ok
    When I get the rules for the current schedule profile
    Then it should be ok
    And item 0 should have property value equal to 8 
    And item 0 should have property strong equal to true 
    And item 1 should have property strong equal to true
    And item 1 should have property value equal to 5


    
