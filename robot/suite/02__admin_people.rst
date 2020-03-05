.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary
    Suite Setup         Admin Setup
    Suite Teardown      Admin Teardown
    Test Teardown       Error Bar Should Not Be Visible

.. code:: robotframework

    *** Keywords ***
    Admin Setup
        Login As    ${VALID USER}

    Admin Teardown
        Logout

These two keywords are used to navigate to next and previous page in paginated
search results. Navigating to next page might trigger loading of more data from
the server. Navigating to previous page does not trigger the loading, as data
has already been loaded and cached.

.. code:: robotframework

    Navigate To Next Page
        Click Element   id:next-page
        Wait Until Data Has Finished Loading

    Navigate To Previous Page
        Click Element   id:previous-page

When list of people are displayed, they can be opened by clicking respective
row. Application will then display the detailed person data. Each person row
has two ids: ``person-id-`` for id field and ``person-name-`` for person name.
Both are followed by 1 based index of the table. Name of the first person
can thus be found on element ``person-name-1`` and id of 3rd person on
element ``person-id-3``.

.. code:: robotframework

    View Person on Row
        [Arguments]   ${person_row_id}
        ${id}=   Catenate   SEPARATOR=   person-id-   ${person_row_id}
        Click Element   id:${id}

Admin page
==========
Admin page is only visible for users with sufficient rights. They can use it
to monitor game status and administer it. Admin page can be opened by clicking
the respective link on main menu bar.

.. code:: robotframework

    *** Test Cases ***
    Opening Admin Page
        Click Link   link:Admin
        Wait Until Data Has Finished Loading

People
------
People section shows all people in the system. As there can be lot of data,
this data is paginated. Clicking a single row will open details of respective
person.

Known issues
++++++++++++
- 13_: No people are shown in admin view

.. code:: robotframework

    Viewing List of People
        Click Link   link:People
        Wait Until Data Has Finished Loading
        Page Should Not Contain   No data
        ${person_name}=   Get Text   id:person-name-1
        Should Not Be Equal   ${person_name}   ${EMPTY}

    Viewing Single Person
        View Person on Row   1
        Wait Until Data Has Finished Loading
        Go Back
        Wait Until Data Has Finished Loading

    Viewing Different Pages of Paginated Data
        ${personId1}=   Get Text   id:person-id-1
        Navigate To Next Page
        ${personId2}=   Get Text   id:person-id-1
        Navigate To Previous Page
        ${personId3}=   Get Text   id:person-id-1
        Should Be Equal As Integers   ${personId1}   ${personId3}
        Should Not Be Equal As Integers   ${personId1}   ${personId2}

.. _13: https://github.com/tuturto/deep-sky/issues/13
