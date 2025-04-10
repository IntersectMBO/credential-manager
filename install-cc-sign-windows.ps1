Invoke-RestMethod -Uri "https://github.com/IntersectMBO/credential-manager/releases/download/0.1.3.0/cc-sign-windows.zip" -OutFile cc-sign-windows.zip
Expand-Archive cc-sign-windows.zip -DestinationPath $env:USERPROFILE\cc-sign -Force
[System.Environment]::SetEnvironmentVariable("Path", "$env:Path;$env:USERPROFILE\cc-sign\result\bin", [System.EnvironmentVariableTarget]::User)
$env:Path += ";$env:USERPROFILE\cc-sign\result\bin"
Remove-Item cc-sign-windows.zip

