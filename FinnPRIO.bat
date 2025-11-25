@echo off
setlocal enabledelayedexpansion

set "RootDir=%~dp0"
set "LogFile=%RootDir%launcher.log"
echo [%DATE% %TIME%] Script started. > "%LogFile%"

set "RPathFound=false"
echo [%DATE% %TIME%] Searching for R in portable directory... >> "%LogFile%"

:: First, try to find R in the portable directory
if exist "%RootDir%R\App\R-Portable\bin\x64\R.exe" (
    set "R=%RootDir%R\App\R-Portable\bin\x64\R.exe"
    set "RPathFound=true"
    echo [%DATE% %TIME%] Found R in portable directory: !R! >> "%LogFile%"
)

:: If not found, search for R in default installation paths
if not !RPathFound! == true (
    echo [%DATE% %TIME%] Searching for R in default installation paths... >> "%LogFile%"
    for %%d in ("C:\Program Files\R" "C:\Program Files (x86)\R") do (
        for /d %%i in (%%d\R-*) do (
            if exist "%%i\bin\x64\R.exe" (
                set "R=%%i\bin\x64\R.exe"
                set "RPathFound=true"
                echo [%DATE% %TIME%] Found R in default path: !R! >> "%LogFile%"
                goto :findDir
            )
        )
    )
)

:: If R is still not found, prompt the user
:promptRPath
if not !RPathFound! == true (
    echo [%DATE% %TIME%] R not found. Prompting user for path... >> "%LogFile%"
    set /p "R=Cannot detect R installation location. Please specify the full path to your R installation (R.exe): "
    if not exist "!R!" (
        echo [%DATE% %TIME%] Invalid path entered: !R! >> "%LogFile%"
        echo The specified path does not exist. Please try again.
        goto :promptRPath
    ) else (
        set "RPathFound=true"
        echo [%DATE% %TIME%] User provided valid R path: !R! >> "%LogFile%"
        echo.
        goto :findDir
    )
)

:: Assuming every folder contains a Shiny app, get a list of folders
:findDir
echo [%DATE% %TIME%] Searching for Shiny app folders... >> "%LogFile%"
set /a "appCount=0"
for /d %%i in ("%RootDir%*") do (
    if "%%~nxi" neq "R" if "%%~nxi" neq "browser" (
        set /a "appCount+=1"
        set "app[!appCount!]=%%~nxi"
        echo [%DATE% %TIME%] Found app folder: %%~nxi >> "%LogFile%"
    )
)

:: Exit if no folders were found
if !appCount! EQU 0 (
    echo [%DATE% %TIME%] No Shiny apps found. >> "%LogFile%"
    echo No Shiny apps were found, make sure you have at least one folder containing a Shiny app.
    pause >nul
    exit /b 1
)

:: For a single folder, skip app selection
if !appCount! EQU 1 (
    set "AppDir=%RootDir%!app[1]!"
    echo [%DATE% %TIME%] Only one app found. Launching: !AppDir! >> "%LogFile%"
    goto :runApp
)

echo ====== Shiny App Launcher ======
echo.
for /l %%i in (1,1,!appCount!) do (
    echo %%i. !app[%%i]!
)
echo.

:: Prompt the user to select an application
:selectApp
set /p opt="Select application to launch (1-!appCount!): "
set "opt=%opt:"=%"

:: Validate user input
echo %opt% | findstr /r /c:"^[1-9][0-9]*$" >nul 2>&1
if errorlevel 1 (
    echo [%DATE% %TIME%] Invalid input: %opt% >> "%LogFile%"
    echo Invalid input. Please enter a number between 1 and !appCount!.
    goto :selectApp
) else (
    if !opt! LEQ !appCount! (
        set "AppDir=%RootDir%!app[%opt%]!"
        echo [%DATE% %TIME%] User selected app: !AppDir! >> "%LogFile%"
        goto :runApp
    ) else (
        echo [%DATE% %TIME%] Input out of range: %opt% >> "%LogFile%"
        echo Invalid input. Please enter a number between 1 and !appCount!.
        goto :selectApp
    )
)

:runApp
echo [%DATE% %TIME%] Launching app with R: !R! >> "%LogFile%"
echo [%DATE% %TIME%] Running: !AppDir! >> "%LogFile%"
"%R%" --no-save --slave -f "%RootDir%run.R" --args "%AppDir%" >> "%LogFile%" 2>&1

echo [%DATE% %TIME%] Script completed. >> "%LogFile%"
exit /b 0
