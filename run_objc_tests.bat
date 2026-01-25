@echo off
setlocal

set ROOT=%~dp0
set TEST_DIR=%ROOT%tests
set LOG=%TEST_DIR%\objc_compiler_output.txt

if not exist "%TEST_DIR%" (
  echo Tests folder not found: "%TEST_DIR%"
  exit /b 1
)

del "%LOG%" 2>nul

for %%f in ("%TEST_DIR%\*.m") do (
  echo ===== %%~nxf =====>> "%LOG%"
  "%ROOT%objc_compiler.exe" "%%f" >> "%LOG%" 2>&1
  echo.>> "%LOG%"
)

echo Done. Output: "%LOG%"
endlocal
