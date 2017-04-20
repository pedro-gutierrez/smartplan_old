Feature: App
  As an administrator
  I want to define an app
  So that I can configure stuff and run tests

  Scenario: Empty app
    Given a new app
    When I retrieve the app
    Then it should be ok
    And property created should exist
