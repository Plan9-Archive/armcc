#include <u.h>
#include <libc.h>
#include "dat.h"
#include "fns.h"

typedef struct ValNum ValNum;
typedef struct VNScope VNScope;

enum {
	UNK,
	OP,
	CONST,
	ADDR,
};

enum { CONSTMAP = 127 };

struct ValNum {
	Targ *t;
	
	int src, op;
	ValNum *y, *z;
	
	long lval;
	Symbol *base;
	
	ValNum *next;
};

struct VNScope {
	ValNum **v;
	int nv;
	VNScope *up;
};

Targ **targs;
ValNum **targval;
int ntargs, natargs;
VNScope *vscope;
ValNum **consts;

static void
dvnup(void)
{
	VNScope *vs;

	vs = emalloc(sizeof(VNScope));
	vs->nv = 32;
	vs->v = emalloc(sizeof(ValNum *) * vs->nv);
	vs->up = vscope;
	vscope = vs;
}

static void
dvndown(void)
{
	VNScope *vs;
	
	vs = vscope;
	vscope = vs->up;
	free(vs->v);
	free(vs);
}

static uintptr
dvnhash(int op, ValNum *y, ValNum *z)
{
	return op + (uintptr) y + (uintptr) z;
}

static ValNum *
dvnlook(int op, ValNum *y, ValNum *z)
{
	uintptr h;
	VNScope *vs;
	ValNum *v;
	
	h = dvnhash(op, y, z);
	for(vs = vscope; vs != nil; vs = vs->up)
		for(v = vscope->v[h % vscope->nv]; v != nil; v = v->next)
			if(v->src == OP && v->op == op && v->y == y && v->z == z)
				return v;
	return nil;
}

static ValNum *
dvnput(int op, ValNum *y, ValNum *z, Targ *t)
{
	ValNum *v;
	ValNum **p;
	uintptr h;
	
	v = emalloc(sizeof(ValNum));
	v->src = OP;
	v->op = op;
	v->y = y;
	v->z = z;
	v->t = t;
	if(op == OPLD || op == OPST)
		return v;
	h = dvnhash(op, y, z);
	p = &vscope->v[h % vscope->nv];
	v->next = *p;
	*p = v;
	return v;
}

static ValNum *
getconst(ulong lval, Targ *t, Symbol *b)
{
	ValNum *v, **p;
	
	for(p = &consts[lval % CONSTMAP]; (v = *p) != nil; p = &v->next)
		if(v->lval == lval && v->base == b)
			return v;
	v = emalloc(sizeof(ValNum));
	v->src = b != nil ? ADDR : CONST;
	v->lval = lval;
	v->base = b;
	if(t == nil)
		if(v->base != nil)
			t = targ(TARGADDR, b, lval);
		else
			t = targ(TARGCONST, lval);
	v->t = t;
	*p = v;
	return v;
}

static void
addtarg(Targ *t)
{
	switch(t->t){
	case TARGSYM:
	case TARGRETV:
	case TARGFP:
		return;
	case TARGIND:
		addtarg(t->link);
		return;
	case TARGSSA:
	case TARGTEMP:
	case TARGCONST:
	case TARGADDR:
		break;
	default:
		print("addtarg: unhandled type %J (%d)\n", t, t->t);
		return;
	}
	if(t->num >= 0)
		return;
	if(ntargs >= natargs){
		natargs += 64;
		targs = realloc(targs, natargs * sizeof(Targ **));
	}
	t->num = ntargs;
	targs[ntargs++] = t;
}

static void
findtarg(void)
{
	IRBlock *b, *l;
	IR *i;
	int j;
	
	l = &curfunc->bllist;
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = i->next)
			switch(OPTYPE(i->op)){
			case OPNORM:
				if(i->r != nil)
					addtarg(i->r);
				if(i->a != nil)
					addtarg(i->a);
				if(i->b != nil)
					addtarg(i->b);
				break;
			case OPPHI:
				if(i->φ.r != nil)
					addtarg(i->φ.r);
				for(j = 0; j < i->φ.n; j++)
					if(i->φ.a[j] != nil)
						addtarg(i->φ.a[j]);
			case OPBRANCH:
				break;
			default:
				print("findtarg: unhandled type %#x\n", OPTYPE(i->op));
			}
}

static ValNum *
getval(Targ *t)
{
	ValNum *v;

	if(t == nil)
		return nil;
	if(t->num < 0)
		return nil;
	if(targval[t->num] != nil)
		return targval[t->num];
	if(t->t == TARGCONST)
		return getconst(t->lval, t, nil);
	if(t->t == TARGADDR)
		return getconst(t->n, t, t->sym);
	v = emalloc(sizeof(ValNum));
	v->t = t;
	targval[t->num] = v;
	return v;
}

static int
isconst(ValNum *v, long c)
{
	return v != nil && v->src == CONST && v->lval == c;
}

static void
dvntrav(IRBlock *b)
{
	ValNum *y, *z, *w;
	IRBlock *c;
	IR *i;
	int j, k;
	long r;

	dvnup();
	for(i = b->instr.next; i != &b->instr; i = i->next)
		switch(OPTYPE(i->op)){
		case OPNORM:
			y = getval(i->a);
			z = getval(i->b);
			if(y != nil)
				i->a = y->t;
			if(z != nil)
				i->b = z->t;
			if(y == nil && i->a != nil || z == nil && i->b != nil || i->r == nil || i->r->num < 0)
				continue;
			if(i->op == OPMOV){
				targval[i->r->num] = y;
				continue;
			}
			if(y->src == CONST && (z == nil || z->src == CONST) && longop(i, i->op, y->lval, z != nil ? z->lval : 0, &r)){
				w = getconst(r, nil, nil);
				goto move;
			}
			if((i->op == OPADD || i->op == OPMUL || i->op == OPAND || i->op == OPOR || i->op == OPXOR) && y > z){
				w = y;
				y = z;
				z = w;
			}
			switch(i->op){
			case OPAND:
				if(y == z) goto noop0;
				if(isconst(y, -1)) goto noop1;
				if(isconst(z, -1)) goto noop0;
				if(isconst(y, 0) || isconst(z, 0)) goto zero;
				break;
			case OPOR:
				if(y == z) goto noop0;
				if(isconst(y, 0)) goto noop1;
				if(isconst(z, 0)) goto noop0;
				break;
			case OPXOR:
				if(y == z) goto zero;
				if(isconst(y, 0)) goto noop1;
				if(isconst(z, 0)) goto noop0;
				break;
			case OPADD:
				if(isconst(y, 0)) goto noop1;
				if(isconst(z, 0)) goto noop0;
				if(y->src == CONST && z->src == ADDR) goto addr1;
				if(z->src == CONST && y->src == ADDR) goto addr0;
				if(y->src == CONST && y->lval < 0){
					w = y;
					y = z;
					z = w;
				}
				if(z->src == CONST && z->lval < 0){
					z = getconst(-z->lval, nil, nil);
					i->b = z->t;
					i->op = OPSUB;
				}
				break;
			case OPSUB:
				if(y == z) goto zero;
				if(isconst(y, 0)) goto noop1;
				if(isconst(z, 0)) goto noop0;
				if(z->src == CONST && y->src == ADDR) goto addr0;
				if(z->src == CONST && z->lval < 0){
					z = getconst(-z->lval, nil, nil);
					i->b = z->t;
					i->op = OPADD;
				}
				break;
			case OPMUL:
				if(isconst(y, 0) || isconst(z, 0)) goto zero;
				if(isconst(y, 1)) goto noop1;
				if(isconst(z, 1)) goto noop0;
				break;
			case OPDIV:
				if(isconst(z, 1)) goto noop0;
				break;
			case OPLSH:
			case OPRSH:
			case OPASR:
				if(isconst(z, 0)) goto noop0;
				break;
			}
			w = dvnlook(i->op, y, z);
			if(w != nil){
			move:
				i->op = OPMOV;
				i->b = nil;
				i->a = w->t;
			}else
				w = dvnput(i->op, y, z, i->r);
			targval[i->r->num] = w;
			break;
		zero:
			w = getconst(0, nil, nil);
			goto move;
		noop0:
			w = y;
			goto move;
		noop1:
			w = z;
			goto move;
		addr0:
			longop(i, i->op, y->lval, z->lval, &r);
			w = getconst(r, nil, y->base);
			goto move;
		addr1:
			longop(i, i->op, y->lval, z->lval, &r);
			w = getconst(r, nil, z->base);
			goto move;
		case OPBRANCH:
			break;
		case OPPHI:
			w = nil;
			for(j = 0; j < i->φ.n; j++)
				if(i->φ.a[j] != nil){
					if(w == nil)
						w = getval(i->φ.a[j]);
					else if(w != getval(i->φ.a[j]))
						break;
				}
			if(j == i->φ.n){
				if(i->r->num >= 0)
					targval[i->r->num] = w;
			}
			break;
		default:
			print("dvntrav: unhandled type %#x\n", OPTYPE(i->op));
		}
	for(j = 0; j < b->nto; j++){
		c = b->to[j];
		for(i = c->instr.next; i->op == OPPHI; i = i->next)
			for(k = 0; k < i->φ.n; k++)
				if(i->φ.a[k] != nil && (y = getval(i->φ.a[k])) != nil)
					i->φ.a[k] = y->t;
	}
	for(c = b->domch; c != nil; c = c->chnext)
		dvntrav(c);
	dvndown();
}

void
dvn(void)
{
	findtarg();
	targval = emalloc(sizeof(ValNum *) * ntargs);
	consts = emalloc(sizeof(ValNum *) * CONSTMAP);
	dvntrav(curfunc->bllist.next);
	free(targval);
	free(consts);
}

void
deflabel(void)
{
	IRBlock *b, *l;
	IR *i;
	
	l = &curfunc->bllist;
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = i->next)
			switch(OPTYPE(i->op)){
			case OPNORM:
				if(i->r != nil)
					i->r->def = i;
				break;
			case OPPHI:
				if(i->φ.r != nil)
					i->φ.r->def = i;
				break;
			case OPBRANCH:
				break;
			default:
				print("deflabel: unimplemented %#x\n", OPTYPE(i->op));
			}
}

static void
markir(IR ***p, IR *i)
{
	if(i == nil || (i->sz & IRMARK) != 0)
		return;
	i->sz |= IRMARK;
	**p = i;
	i->link = nil;
	*p = &i->link;
}

void
dead(void)
{
	IRBlock *b, *l;
	IR *i, *in, *w, **p;
	int j;
	
	deflabel();
	l = &curfunc->bllist;
	w = nil;
	p = &w;
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = i->next){
			if(OPTYPE(i->op) == OPBRANCH)
				goto useful;
			switch(i->op){
			case OPLD:
			case OPST:
			case OPCMP:
			useful:
				markir(&p, i);
				break;
			case OPMOV:
				switch(i->r->t){
				case TARGRETV:
				case TARGSYM:
					goto useful;
				}
			default:
				i->sz &= ~IRMARK;
			}
		}
	for(; w != nil; w = w->link)
		switch(OPTYPE(w->op)){
		case OPNORM:
			if(w->a != nil)
				markir(&p, w->a->def);
			if(w->b != nil)
				markir(&p, w->b->def);
			break;
		case OPPHI:
			for(j = 0; j < w->φ.n; j++)
				if(w->φ.a[j] != nil)
					markir(&p, w->φ.a[j]->def);
			break;
		case OPBRANCH:
			break;
		default:
			print("dead: unimplemented %#x\n", OPTYPE(w->op));
		}
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = in){
			in = i->next;
			if((i->sz & IRMARK) != 0)
				i->sz &= ~IRMARK;
			else{
				i->prev->next = i->next;
				i->next->prev = i->prev;
			}
		}
}
