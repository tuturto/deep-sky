.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary

    *** Test Cases ***
    Opening Main Page
        [Tags]   smoke
        Go To       ${MAIN URL}
        Wait Until Data Has Finished Loading
        Error Bar Should Not Be Visible
