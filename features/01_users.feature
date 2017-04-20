Feature: Users
  As a user
  I want to be able to sign up and sign out
  
  @current
  Scenario: Sign up
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    Then it should be ok
    And property token should exist
    Given the root user
    When I retrieve the app
    Then property emails should equal 1
    And property users should equal 1
  
  Scenario: Signup twice
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    Then it should be ok
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    Then there should be a conflict
  
  Scenario: Reset password
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    Then it should be ok
    And property token should exist
    When I reset my password to foo using token and email sicozu@mac.com
    Then it should be ok
  
  Scenario: Login
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    Then it should be ok
    And property session should exist
  
  Scenario: Login with invalid credentials
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    Then it should be ok
    And response should have property session
    Given an anonymous user
    When I login as user sicozu@mac.com and password plip
    Then it should be not found
    And response should not have property session

  Scenario: Logout
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    And I logout 
    Then it should be ok
    When I logout
    Then it should be forbidden
  
 
  Scenario: Forgot password
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I forgot my password as sicozu@mac.com
    Then it should be ok
    And property token should exist

  Scenario: Profile
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    And I set my first name to Pepe and and my last name to Gotera
    Then it should be ok
    And property first should equal Pepe
    And property last should equal Gotera
    When I set my first name to Perico and and my last name to Palotes
    And I get my profile
    Then it should be ok
    And property first should equal Perico
    And property last should equal Palotes
    
