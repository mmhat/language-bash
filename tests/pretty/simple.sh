declare foo
declare foo=bar
declare foo1=bar1 foo2=bar2
declare -i foo
declare -a foo
declare -r foo

cmd;

cmd1 | cmd2;
time cmd1 | cmd2;
time -p cmd1 | cmd2;
! cmd1 | cmd2;
time -p ! cmd1 | cmd2;

cmd1 && cmd2
cmd1 || cmd2
