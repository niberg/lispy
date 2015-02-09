# lispy
My own LISP dialect, following www.buildyourownlisp.com.

Depends on _editline_ if compiling on Linux.
On Debian-based distributions you can install editline with "sudo apt-get install libedit-dev". On Fedora you can use the command su -c "yum install libedit-dev*".

Compile with "cc --std=c99 lispy.c mpc.c -ledit -lm -o lispy.
Run with "./lispy".

