.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary
    Suite Setup         Units Setup
    Suite Teardown      Units Teardown
    Test Teardown       Error Bar Should Not Be Visible

.. code:: robotframework

    *** Keywords ***
    Units Setup
        Login As    ${VALID USER}

    Units Teardown
        Logout

Units
=====
Units view can be opened by clicking main menu item "Units", which should
always be available for logged in users.

Keywords
--------


Test Cases
----------

All units are displayed on a fleet page that is available from top menu.

.. code:: robotframework

    *** Test Cases ***
    Opening Units Page
        [Tags]   development   units   smoke
        Click Link   Fleet
        Wait Until Page Contains   Placeholder for fleet
