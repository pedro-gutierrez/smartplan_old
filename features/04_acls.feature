Feature: Acls
  As a user
  I want to be able to get my acls

  @current
  Scenario: Default Acls
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    And I get my acls
    Then it should be ok
    And response should have 4 items
    And item 0 should have property name equal to admin
    And item 0 should have property left equal to 30
    And item 0 should have property expires
    And item 1 should have property name equal to organizations
    And item 1 should have property left equal to 30
    And item 1 should have property expires
    And item 2 should have property name equal to schedules 
    And item 2 should have property left equal to 30
    And item 2 should have property expires
    And item 3 should have property name equal to tasks
    And item 3 should have property left equal to 30
    And item 3 should have property expires
        
        
    
