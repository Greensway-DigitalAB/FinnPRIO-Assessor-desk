# Deploy FinnPrio-Assessor Shiny app locally on Windows
This repo prodives the app code wrapped in an Inno setup structure that will produce an executable file to deploy the Shiny App as a standalone app.

## Bundle a portable web browser

Download and install [Google Chrome Portable](https://portableapps.com/apps/internet/google_chrome_portable), then copy the contents of **GoogleChromePortable\App\Chrome-bin\\** into the **chrome** folder.

Other suitable browsers may be also be used, but make sure to change *browser_path* in **run.R**.

## Create installer executable

See setup.iss for an example compilation script using [Inno Setup](https://www.jrsoftware.org/isinfo.php).

Make sure to generate a new AppId and change the name, version, etc.
