make

exe=./a.exe
time ($exe > run.log 2>&1) > run.time 2>&1 &
