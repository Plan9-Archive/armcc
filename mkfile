</$objtype/mkfile

TARG=cc
OFILES=\
	main.$O\
	lex.$O\
	y.tab.$O\
	ast.$O\
	ir.$O\
	set.$O\
	ssa.$O\
	dvn.$O\

YFILES=grammar.y

HFILES=dat.h fns.h

</sys/src/cmd/mkone

lex.$O: y.tab.c

YFLAGS=$YFLAGS -D1
