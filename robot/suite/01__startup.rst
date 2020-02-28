.. code:: robotframework

    *** Settings ***
    Resource            ./settings.rst
    Library             SeleniumLibrary
    Suite Setup         Start Testing

    *** Test Cases ***
    Open Main Page
        Go To       ${MAIN URL}
