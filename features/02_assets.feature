Feature: Assets
  As a user
  I want to be able to upload assets
  
  Scenario: Anonymous upload
    Given a new app
    And an anonymous user
    And file with name /tmp/test.txt and content Hello
    When I upload text file /tmp/test.txt
    Then it should be forbidden
  
  Scenario: User upload
    Given a new app
    And an anonymous user
    When I signup with name Pedro Gutierrez and email sicozu@mac.com
    And I reset my password to foo using token and email sicozu@mac.com
    And I login as user sicozu@mac.com and password foo
    Given file with name /tmp/test.txt and content Hello
    When I upload text file /tmp/test.txt
    Then it should be ok
    When I remember property id
    Given the root user
    When I retrieve the app
    Then property assets should equal 1
    And property assetsSize should equal 5
    Given an anonymous user
    And I login as user sicozu@mac.com and password foo
    When I fetch asset using property id
    Then it should be ok
    And response should have header content-length equal to 5
    And response should have header content-type equal to text/plain
    
  
    