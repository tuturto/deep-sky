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

    Viewing Single Person
        Click Element   xpath:/html/body/div[4]/div/div[2]/div[2]/div[2]/div/table/tbody/tr[1]/td[1]
        Wait Until Page Contains    Update
        Go Back
        Wait Until Element Is Visible   xpath:/html/body/div[4]/div/div[2]/div[2]/div[2]/div/table/tbody/tr[1]/td[1]

    Viewing Different Pages of Paginated Data
        ${personId1}=   Get Text   xpath:/html/body/div[4]/div/div[2]/div[2]/div[2]/div/table/tbody/tr[1]/td[1]
        Navigate To Next Page
        ${personId2}=   Get Text   xpath:/html/body/div[4]/div/div[2]/div[2]/div[2]/div/table/tbody/tr[1]/td[1]
        Navigate To Previous Page
        ${personId3}=   Get Text   xpath:/html/body/div[4]/div/div[2]/div[2]/div[2]/div/table/tbody/tr[1]/td[1]
        Should Be Equal As Integers   ${personId1}   ${personId3}
        Should Not Be Equal As Integers   ${personId1}   ${personId2}

.. _13: https://github.com/tuturto/deep-sky/issues/13
