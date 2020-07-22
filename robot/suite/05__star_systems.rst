.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary
    Suite Setup         Star Systems Setup
    Suite Teardown      Star Systems Teardown
    Test Teardown       Error Bar Should Not Be Visible

.. code:: robotframework

    *** Keywords ***
    Star Systems Setup
        Login As    ${VALID USER}

    Star Systems Teardown
        Logout

Star Systems
============

Star systems view shows all known star systems to the user.

Keywords
--------

Star system details can be viewed by clicking the respective row on the list.

.. code:: robotframework

    View Star System on Row
        [Arguments]   ${row_id}
        ${id}=   Catenate   SEPARATOR=   system-name-   ${row_id}
        Click Element   id:${id}
        Wait Until Data Has Finished Loading

Name of currently viewed star system is visible in the breadcrumb path.

.. code:: robotframework

    Breadcrumb Text
        ${system_name}=   Get Text   id:breadcrumb-system-name
        Return From Keyword   ${system_name}

Test cases
----------

Star systems view can always be opened from the top menu.

.. code:: robotframework

    *** Test Cases ***
    Opening Star Systems View
        [Tags]   star systems   smoke
        Click Link   Star Systems
        Wait Until Data Has Finished Loading

Star system details can be viewed by clicking its entry on the list. While
the star system is viewed, its name is displayed in the breadcrumb path.

.. code:: robotframework

    Viewing Star System Details
        [Tags]   star systems
        ${system_name_from_list}=   Get Text   id:system-name-1
        View Star System on Row   1
        ${system_name_from_view}=   Get Text   id:system-name
        ${system_name_from_breadcrumb}=   Breadcrumb Text
        Should Be Equal   ${system_name_from_list}   ${system_name_from_view}
        Should Be Equal   ${system_name_from_list}   ${system_name_from_breadcrumb}
        Go Back
        Wait Until Data Has Finished Loading
