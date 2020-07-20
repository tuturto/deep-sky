.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary
    Suite Setup         Designer Setup
    Suite Teardown      Designer Teardown
    Test Teardown       Error Bar Should Not Be Visible

.. code:: robotframework

    *** Variables ***
    ${test_vehicle_name}        Test Vehicle

.. code:: robotframework

    *** Keywords ***
    Designer Setup
        Login As    ${VALID USER}


    Designer Teardown
        Logout

Designer
========

Designer is used to design blueprints for ships and vehicles. These blueprints
are faction specific.

Keywords
--------

Design can be opened by clicking the respective row on the designs list.

.. code:: robotframework

    View Design on Row
        [Arguments]   ${design_row_id}
        ${id}=   Catenate   SEPARATOR=   design-entry-   ${design_row_id}
        Click Element   id:${id}
        Wait Until Data Has Finished Loading

    Close Design Without Saving
        Click Element   id:clear-button
        Wait Until Data Has Finished Loading

Chassis is can be selected from a list that shows all currently available chassis.

.. code:: robotframework

    Select Chassis
        [Arguments]   ${chassis_name}
        Click Element   xpath=//*[@id="chassis-list"]/tbody/tr/td[text()[contains(., '${chassis_name}')]]
        Wait Until Data Has Finished Loading

Components are added to design by clicking them. Currently available components
are filtered by available technology and selected chassis.

.. code:: robotframework

    Add Component
        [Arguments]   ${component_name}
        Click Element   xpath=//div[contains(@class, 'available-component')][div//div//div//text()[contains(., '${component_name}')]]
        Wait Until Data Has Finished Loading

    Name Design
        [Arguments]   ${design_name}
        Input Text   id:ship-name-input   ${design_name}

A valid design can be saved by clicking save button. This saves the design, but
does not close the designer. In order to return to main view of the designer,
user has to click clear button. Deleting a design can be done from the main view.

.. code:: robotframework

    Save Design And Return
        Click Element   id:save-button
        Wait Until Data Has Finished Loading
        Click Element   id:clear-button
        Wait Until Data Has Finished Loading

    Delete Design
        [Arguments]   ${design_name}
        Click Element   xpath://*[@id="design-table"]/tbody/tr[td//text()[contains(., '${design_name}')]]/td[4]/i[2]
        Wait Until Data Has Finished Loading

Test cases
----------

Designer can always be opened from the top menu.

.. code:: robotframework

    *** Test Cases ***
    Opening Designer
        Click Link   Designer
        Wait Until Data Has Finished Loading

Existing design can be opened by clicking its entry on the list.

.. code:: robotframework

    Viewing Existing Desig
        View Design on Row   1
        Close Design Without Saving

A design is created by first selecting suitable chassis and then filling in
components and name. Until required components have been filled in, the design
can't be saved.

.. code:: robotframework

    Creating a New Design
        Select Chassis   SUV
        Add Component   Wheeled
        Name Design   ${test_vehicle_name}
        Wait Until Page Contains   Design ok
        Save Design And Return
        Wait Until Page Contains   ${test_vehicle_name}

Clicking remove button will immediately delete the design.

.. code:: robotframework

    Deleting an Existing Design
        Delete Design   ${test_vehicle_name}
        Wait Until Data Has Finished Loading
        Page Should Not Contain    ${test_vehicle_name}
