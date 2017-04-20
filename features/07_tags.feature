Feature: Tags

  Scenario: Organization tags
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    When I add tag owners to the current organization
    Then it should be ok
    When I add tag owners to the current organization
    Then it should be ok
    When I add tag managers to the current organization
    Then it should be ok
    When I get the tags of the current organization
    Then it should be ok
    And item 0 should equal managers
    And item 1 should equal owners
  
  Scenario: Organization member tags
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo 
    When I add tag owners to member with email sicozu@mac.com of the current organization
    Then it should be not found 
    When I get the tags of member with email sicozu@mac.com of the current organization
    Then it should be ok
    And response should have 0 items
    When I add tag owners to the current organization
    And I add tag owners to member with email sicozu@mac.com of the current organization
    Then it should be ok 
    When I get the tags of member with email sicozu@mac.com of the current organization
    Then it should be ok
    And response should have 1 items
    And item at position 0 should equal owners
    When I remove tag owners from member with email sicozu@mac.com of the current organization
    Then it should be ok
    When I get the tags of member with email sicozu@mac.com of the current organization
    Then it should be ok
    And response should have 0 items

  Scenario: Schedule Template tags
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And schedule template with name Template1 created by user sicozu@mac.com with password foo
    And I add tag peak to the current schedule template 
    Then it should be not found 
    When I get the tags for the current schedule template
    Then it should be ok
    And response should have 0 items
    When I add tag peak to the current organization
    Then it should be ok
    When I add tag peak to the current schedule template 
    Then it should be ok
    When I get the tags for the current schedule template
    Then response should have 1 items
    And item at position 0 should equal peak 
    When I remove tag peak from the current schedule template
    Then it should be ok
    When I get the tags for the current schedule template
    Then it should be ok
    And response should have 0 items
  
  @current
  Scenario: Template shift tags
    Given a new app
    And user with name Pedro Gutierrez, email sicozu@mac.com and password foo
    And organization with name Organization1 created by user sicozu@mac.com with password foo
    And schedule template with name Template1 created by user sicozu@mac.com with password foo
    And template shift of type std for monday between 12:00 and 14:00 with staffing 1 created by user sicozu@mac.com with password foo
    When I add tag critical to the current template shift
    Then it should be not found
    When I get the current template shift
    Then it should be ok
    And response should have property tags with length 0
    When I add tag critical to the current organization
    Then it should be ok
    When I add tag critical to the current template shift
    Then it should be ok 
    When I add tag critical to the current template shift
    Then it should be ok 
    And response should have 1 items
    And item 0 should equal critical
    When I remove tag critical from the current template shift
    Then it should be ok
    And response should have 0 items 
