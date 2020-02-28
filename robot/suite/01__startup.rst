.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary

    *** Test Cases ***
    Open Main Page
        Go To       ${MAIN URL}
        Error Bar Should Not Be Visible
