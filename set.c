#include <u.h>
#include <libc.h>
#include <bio.h>
#include "dat.h"
#include "fns.h"

BitSet *
bsnew(int max)
{
	int n;
	BitSet *b;
	
	n = max + 31 >> 5;
	b = emalloc(sizeof(BitSet) + n * 4);
	b->n = n;
	return b;
}

void
bsreset(BitSet *b)
{
	int i;

	for(i = 0; i < b->n; i++)
		b->p[i] = 0;
}

void
bscopy(BitSet *d, BitSet *s)
{
	int i;
	
	for(i = 0; i < d->n; i++)
		d->p[i] = s->p[i];
}

int
bsadd(BitSet *b, int i)
{
	u32int c, r;

	c = 1 << (i & 31);
	r = b->p[i / 32] & c;
	b->p[i / 32] |= c;
	return r;
}

int
bstest(BitSet *b, int i)
{
	return b->p[i / 32] & 1<<(i & 31);
}

int
popcnt(u32int v)
{
	v = v - ((v >> 1) & 0x55555555);
	v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
	v = (v + (v >> 4)) & 0x0f0f0f0f;
	v = v + (v >> 8);
	v = v + (v >> 16);
	return v & 0x3f;
}

int
bsiter(BitSet *b, int i)
{
	u32int v, j;
	
	i++;
	j = i >> 5;
	i = i & 31;
	if(j >= b->n)
		return -1;
	v = b->p[j] & -(1 << i);
	while(v == 0){
		if(++j >= b->n)
			return -1;
		v = b->p[j];
	}
	return 32 - popcnt(v | -v) | j << 5;
}

int
setfmt(Fmt *f)
{
	int rc, k, fi;
	BitSet *b;

	b = va_arg(f->args, BitSet *);
	rc = fmtrune(f, '(');
	fi = 0;
	for(k = -1; (k = bsiter(b, k)) >= 0; ){
		if(fi++ != 0)
			rc += fmtrune(f, ',');
		rc += fmtprint(f, "%d", k);
	}
	rc += fmtrune(f, ')');
	return rc;
}
