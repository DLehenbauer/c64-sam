# Setup build environment for Windows command line.

# Path to VICE emululator (for 'test' and 'debug')
$vice_path = "$Env:ProgramFiles\GTK3VICE-*"
$Env:VICE_HOME=(Get-ChildItem -Path $vice_path | Select-Object -First 1)[0].FullName

# Default path to 'make.exe' when installed via 'Winget install GnuWin32.make'
$make_path = "${Env:ProgramFiles(x86)}\GnuWin32\bin"

# Default path to CC65 when installed via the 'VS65 Debugger' VSCode extension
$Env:CC65_HOME=(Join-Path (Get-ChildItem -Path "$Env:UserProfile\.vscode\extensions\entan-gl.cc65-vice-*" | Select-Object -First 1)[0].FullName "dist\cc65")

# Add the above to the user's current path.
$Env:PATH="$Env:PATH;$Env:CC65_HOME\bin_win32_x64;$Env:VICE_HOME\bin;$make_path"
