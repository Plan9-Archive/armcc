#include <u.h>
#include <libc.h>
#include <bio.h>
#include "dat.h"
#include "fns.h"

void
main(void)
{
	extern void yyparse(void);
	extern Biobuf *inp;
	typedef int fmt(Fmt *);
	extern fmt astfmt, opfmt, typefmt, irfmt, irtargfmt, setfmt;

	fmtinstall('A', astfmt);
	fmtinstall('T', typefmt);
	fmtinstall('O', opfmt);
	fmtinstall('I', irfmt);
	fmtinstall('J', irtargfmt);
	fmtinstall('S', setfmt);
	lexinit();
	curline.filen = "test";
	curline.lineno = 1;
	inp = Bopen("/fd/0", OREAD);
	yyparse();
}

void *
emalloc(ulong sz)
{
	void *v;
	
	v = malloc(sz);
	if(v == nil)
		sysfatal("malloc: %r");
	memset(v, 0, sz);
	setmalloctag(v, getcallerpc(&sz));
	return v;
}
