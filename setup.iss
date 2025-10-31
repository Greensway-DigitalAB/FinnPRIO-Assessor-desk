#define MyAppName "FinnPRIO Assessor"
#define MyAppVersion "0.9"
#define MyAppPublisher "Greensway Digital AB"
#define MyAppURL "https://greensway.se/digital-plattform/"
#define MyAppExeName "FinnPRIO.bat"

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId=44C2CD5E-89CE-4FB1-A0B1-6824C17CD234
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DisableDirPage=yes
DefaultGroupName={#MyAppName}
ArchitecturesInstallIn64BitMode=x64
PrivilegesRequired=admin
;PrivilegesRequired=lowest
AllowNoIcons=yes
OutputBaseFilename=finnpriosetup
OutputDir=.
Compression=lzma
SolidCompression=yes
SetupIconFile=bug-slash-solid-full.ico

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "*"; DestDir: "{app}"; Excludes: "finnpriosetup.exe, setup.iss, .git*"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\bug-slash-solid-full.ico"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\bug-slash-solid-full.ico"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Flags: shellexec postinstall nowait skipifsilent
