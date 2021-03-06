.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary
    Suite Setup         News Setup
    Suite Teardown      News Teardown
    Test Teardown       Error Bar Should Not Be Visible

.. code:: robotframework

    *** Keywords ***
    News Setup
        Login As    ${VALID USER}

    News Teardown
        Logout

Messages
========
Messages can be opened by clicking main menu item "Messages", which should
always be available for logged in users.

Keywords
--------

Test Cases
----------

.. code:: robotframework

    *** Test Cases ***
    Opening News Page
        [Tags]   news   smoke
        Click Link   Messages
        Wait Until Page Contains   Latest news

User submitted news
+++++++++++++++++++
User submitted news are meant for sending to members of user's own faction.
They contain short free form text and user selectable icon.

.. code:: robotframework

    Submitting User Written Article
        [Tags]   news
        Wait Until Element Is Visible   id:user-news-input
        Input Text   id:user-news-input   Ready for blast-off!
        Click Element   id:news-icon-jubilation-user-news
        Click Button   Submit
        Wait Until Page Contains   Ready for blast-off!
