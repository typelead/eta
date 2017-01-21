@echo off
set INSTALL_DIR=%~1
if not defined INSTALL_DIR (
  for /f "delims=" %%a in ('stack path --local-bin') do @set INSTALL_DIR=%%a
  if not defined INSTALL_DIR (
    for /f "delims=" %%a in ('stack path --local-bin-path') do @set INSTALL_DIR=%%a
  )
)
shift
set "ARG_LINE= "
:parse_args
if "%~1" NEQ "" (
 set ARG_LINE=%ARG_LINE% "%~1"
 shift
 goto :parse_args
)
stack install --local-bin-path="%INSTALL_DIR%" && stack exec eta-build -- clean && stack exec eta-build -- uninstall && stack exec eta-build -- install %ARG_LINE%