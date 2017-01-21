set INSTALL_DIR=%~1
if not defined INSTALL_DIR  (
  echo entro
  for /f "delims=" %%a in ('stack path --local-bin') do @set INSTALL_DIR=%%a
  if not defined INSTALL_DIR (
    for /f "delims=" %%a in ('stack path --local-bin-path') do @set INSTALL_DIR=%%a
  )
) 
shift
set "arg_line= "
:parse_args
if "%~1" NEQ "" (
 set arg_line=%arg_line% "%~1"
 shift
 goto :parse_args
)

echo %INSTALL_DIR%
echo %arg_line%