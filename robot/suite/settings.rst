.. code:: robotframework

    *** Settings ***
    Documentation     A resource file with reusable keywords and variables.
    ...
    ...               The system specific keywords created here form our own
    ...               domain specific language. They utilize keywords provided
    ...               by the imported SeleniumLibrary.
    Library           SeleniumLibrary

    *** Variables ***
    ${SERVER}           localhost:3000
    ${BROWSER}          Firefox
    ${DELAY}            0
    ${VALID USER}       tuukka
    ${MAIN URL}         http://${SERVER}/
    ${MESSAGES URL}     ${MAIN URL}message/
    ${LOGOUT URL}       ${MAIN URL}auth/logout


    *** Keywords ***
    Start Testing
        Open Browser   ${MAIN URL}   ${BROWSER}
        Maximize Browser Window
        Set Selenium Speed   ${DELAY}

    Stop Testing
        Close Browser

    Login As
        [Arguments]   ${user_name}
        Click Link   link:Login
        Input Text   name:ident   ${user_name}
        Submit Form   xpath:/html/body/div[2]/div/div/form[3]
        Wait Until Page Contains   You are now logged in

    Logout
        Go To   ${LOGOUT URL}
        Wait Until Page Contains   Login

    Error Bar Should Not Be Visible
        Element Should Not Be Visible   class:error-bar
