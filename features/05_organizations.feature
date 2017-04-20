Feature: Organizations
  As a user
  I want to be able to manage my organizations

  Scenario: create and list my organizations
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    When I login as user sicozu@mac.com and password foo
    And I create an organization with name Organization1
    Then it should be ok
    When I create an organization with name Organization2
    Then it should be ok
    When I get my organizations
    Then it should be ok
    And response should have 2 items
    And item 0 should have property name equal to Organization1
    And item 1 should have property name equal to Organization2
    When I create an organization with name Organization2
    Then there should be a conflict
    When I get my organizations
    Then it should be ok
    And response should have 2 items

  Scenario: rename organization
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    When I login as user sicozu@mac.com and password foo
    And I create an organization with name Organization1
    And I create an organization with name Organization2
    And I remember my organization at position 0 as the current organization
    And I rename the current organization as Organization2
    Then there should be a conflict
    When I get my organizations
    Then item 0 should have property name equal to Organization1
    And I rename the current organization as Organization3
    Then it should be ok
    When I get my organizations
    Then item 0 should have property name equal to Organization2
    And item 1 should have property name equal to Organization3 
    When I remember my organization at position 1 as the current organization
    And I rename the current organization as Organization1
    Then it should be ok
    When I get my organizations
    Then item 0 should have property name equal to Organization1
    And item 1 should have property name equal to Organization2
    When I rename the current organization as Organization1
    Then it should be ok


  Scenario: invite a member
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar 
    When I login as user sicozu@mac.com and password foo
    And I create an organization with name Organization1
    And I create an organization with name Organization2
    And I remember my organization at position 0 as the current organization
    And I invite user with email sicozu@me.com to the current organization
    Then it should be ok
    When I get the members of the current organization
    Then it should be ok
    And response should have 2 items
    And item 0 should have property first equal to Pedro
    And item 1 should have property first equal to Perico
    Given an anonymous user
    When I login as user sicozu@me.com and password bar
    And I get my organizations
    Then it should be ok
    And response should have 1 items
    And item 0 should have property name equal to Organization1
    When I get the members of the current organization
    Then it should be ok
    And response should have 2 items
    And item 0 should have property first equal to Pedro
    And item 1 should have property first equal to Perico
  
  Scenario: remove member from organization
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar 
    When I login as user sicozu@mac.com and password foo
    And I create an organization with name Organization1
    And I remember my organization at position 0 as the current organization
    And I invite user with email sicozu@me.com to the current organization 
    Given an anonymous user
    When I login as user sicozu@me.com and password bar
    And I get my organizations
    Then response should have 1 items
    Given an anonymous user
    When I login as user sicozu@mac.com and password foo
    And I remove user with email sicozu@me.com from the current organization
    Then it should be ok
    Given an anonymous user
    When I login as user sicozu@me.com and password bar
    And I get my organizations
    Then response should have 0 items

  Scenario: try remove the organization owner
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    When I login as user sicozu@mac.com and password foo
    And I create an organization with name Organization1
    And I remember my organization at position 0 as the current organization
    When I remove user with email sicozu@mac.com from the current organization
    Then it should be forbidden
    When I get my organizations
    Then it should be ok
    And response should have 1 items
    When I get the members of the current organization
    Then it should be ok
    And response should have 1 items
  
  Scenario: change owner
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar 
    When I login as user sicozu@mac.com and password foo
    And I create an organization with name Organization1
    And I remember my organization at position 0 as the current organization
    And I invite user with email sicozu@me.com to the current organization
    Then it should be ok
    When I set user with email sicozu@me.com as owner of the current organization
    Then it should be ok
    When I set user with email sicozu@mac.com as owner of the current organization 
    Then it should be forbidden

  Scenario: name conflict when changing owner
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And user with name Perico Palotes, email sicozu@me.com and password bar 
    When I login as user sicozu@mac.com and password foo
    And I create an organization with name Organization1
    Then it should be ok
    Given an anonymous user
    When I login as user sicozu@me.com and password bar 
    And I create an organization with name Organization1
    Then it should be ok
    When I remember my organization at position 0 as the current organization
    And I invite user with email sicozu@mac.com to the current organization
    And I set user with email sicozu@mac.com as owner of the current organization
    Then there should be a conflict
    When I rename the current organization as Organization2
    Then it should be ok
    When I set user with email sicozu@mac.com as owner of the current organization
    Then it should be ok
