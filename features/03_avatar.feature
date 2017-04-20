Feature: Avatars
  As a user
  I want to be able to see and update my avatar
  
  @current
  Scenario: Get my avatar
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    When I get my avatar
    Then it should be ok
    And response should have header content-length equal to 14143
    And response should have header content-type equal to image/png
  
  Scenario: Set my avatar
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    When I set my avatar to avatar2.png
    Then it should be ok
    When I get my avatar
    Then it should be ok
    And response should have header content-length equal to 36534
    And response should have header content-type equal to image/png
    
    
    
  
  