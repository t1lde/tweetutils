{ runCommand
, app
, appExe
, shell
, name ? "${appExe}-completions.${shell}"
, installedLocation ? "${app}/bin/${appExe}"
}:
runCommand name {}
  ''
  ${app}/bin/${appExe} --${shell}-completion-script '${installedLocation}' > $out
  ''
