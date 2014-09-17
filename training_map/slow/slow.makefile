MPICC=mpicc
MPIF90=mpif90
MPIRUN=mpirun

all:
# Flags for compiler inlining: MAP works whether inlining is on or off,
# but you'll typically see more intuitive stacks with it turned off.
# The major compilers are discussed here:
#
# Intel: -g -fno-inline-functions -O3 is recommended. At O3 the compiler doesn't
# produce enough unwind info even with -debug inline-debug-info set.
#
# PGI: -g -Mprof=func -O3 is recommended. Other settings dont produce enough
# unwind information for inlined functions otherwise. This adds some
# performance penalty - around 8% is typical.
#
# GNU: -g -O3 -fno-inline is recommended. You might be lucky without -fno-inline,
# as it should produce enough information to unwind those calls. You will see
# my_function [inlined] in the MAP stack for functions that were inline.
# -fno-inline-functions appears with newer gnu compilers, just to confuse

# gnu
	${MPIF90} -g -O3 -fno-inline slow.f90 -o slow_f -lm -lrt
# intel
#       ${MPIF90} -g -fno-inline-functions -O3 slow.f90 -o slow_f -lm -lrt
# pgi
#       ${MPIF90} -g -Mprof=func -O3 slow.f90 -o slow_f -lm -lrt

check:
	${MPIRUN} -np 4 ./slow_f 3

clean:
	-rm -f ./slow_f
