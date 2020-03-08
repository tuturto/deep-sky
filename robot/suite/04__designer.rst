.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary
    Suite Setup         Designer Setup
    Suite Teardown      Designer Teardown
    Test Teardown       Error Bar Should Not Be Visible

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
        Fail

A design is created by first selecting suitable chassis and then filling in
components and name. Until required components have been filled in, the design
can't be saved.

.. code:: robotframework

    Creating a New Design
        Fail

Clicking remove button will immediately delete the design.

.. code:: robotframework

    Deleting an Existing Design
        Fail

Known problems
++++++++++++++
