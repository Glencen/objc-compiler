@echo off
setlocal

set ROOT=%~dp0
set TEST_DIR=%ROOT%tests
set OUT_DIR=%TEST_DIR%\clang_build
set LOG=%TEST_DIR%\clang_output.txt
set CLANG=%CLANG%

if not exist "%TEST_DIR%" (
  echo Tests folder not found: "%TEST_DIR%"
  exit /b 1
)

if not exist "%OUT_DIR%" (
  mkdir "%OUT_DIR%"
)

if "%CLANG%"=="" set CLANG=C:\msys64\mingw64\bin\clang.exe
if not exist "%CLANG%" (
  set CLANG=clang.exe
)

del "%LOG%" 2>nul

for %%f in ("%TEST_DIR%\*.m") do (
  echo ===== %%~nxf =====>> "%LOG%"
  "%CLANG%" "%%f" -o "%OUT_DIR%\%%~nf.exe" >> "%LOG%" 2>&1
  echo.>> "%LOG%"
)

echo Done. Output: "%LOG%"
endlocal
