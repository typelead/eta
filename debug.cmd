if not exist rts\build\rts.jar (
   echo Error: rts.jar not found!
   echo Run install.cmd first.
)

if not exist sample\build\mapandsum.jar (
   echo Error: mapandsum.jar not found!
   echo Run install.cmd first.
)

jdb -classpath rts\build\rts.jar;sample\build\mapandsum.jar mapandsum.Main %*