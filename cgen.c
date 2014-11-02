#include <u.h>
#include <libc.h>
#include "dat.h"
#include "fns.h"

static BitSet **def, **use;
BitSet **livein, **liveout;

static void
usetarg(int i, Targ *t)
{
	if(t == nil)
		return;
	if(t->num >= 0)
		if(!bstest(def[i], t->num))
			bsadd(use[i], t->num);
	else if(t->t == TARGSH){
		usetarg(i, t->a);
		usetarg(i, t->b);
	}
}

static void
deftarg(int i, Targ *t)
{
	if(t == nil || t->num < 0)
		return;
	bsadd(def[i], t->num);
}

static void
calclive(void)
{
	IRBlock *b;
	int j, k, l;
	IR *i;
	int ch;
	BitSet *old, *s;

	def = emalloc(sizeof(BitSet *) * nblocks);
	use = emalloc(sizeof(BitSet *) * nblocks);
	livein = emalloc(sizeof(BitSet *) * nblocks);
	liveout = emalloc(sizeof(BitSet *) * nblocks);
	old = bsnew(ntargs);
	
	for(j = 0; j < nblocks; j++){
		b = blocks[j];
		def[j] = bsnew(ntargs);
		use[j] = bsnew(ntargs);
		liveout[j] = bsnew(ntargs);
		livein[j] = bsnew(ntargs);
		for(i = b->instr.next; i != &b->instr; i = i->next)
			switch(OPTYPE(i->op)){
			case OPNORM:
				usetarg(j, i->a);
				usetarg(j, i->b);
				deftarg(j, i->r);
				break;
			case OPBRANCH:
				break;
			case OPPHI:
				deftarg(j, i->φ.r);
				break;
			default:
				print("calcdefuse: unknown type %#x\n", OPTYPE(i->op));
			}
	}
	do{
		ch = 0;
		for(j = nblocks - 1; j >= 0; j--){
			b = blocks[j];
			s = liveout[j];
			bscopy(old, s);
			bsreset(s);
			for(k = 0; k < b->nto; k++){
				bsunion(s, s, livein[b->to[k]->num]);
				for(l = 0; l < b->to[k]->nfrom; l++)
					if(b->to[k]->from[l] == b)
						break;
				for(i = b->to[k]->instr.next; OPTYPE(i->op) == OPPHI; i = i->next)
					if(l < i->φ.n && i->φ.a[l] != nil && i->φ.a[l]->num >= 0)
						bsadd(s, i->φ.a[l]->num);
			}		
			ch |= bscmp(old, s);
			s = livein[j];
			bscopy(old, s);
			bsminus(s, liveout[j], def[j]);
			bsunion(s, s, use[j]);
			ch |= bscmp(old, s);
		}
	}while(ch != 0);
/*	for(j = 0; j < nblocks; j++){
		print("(");
		for(k = -1; (k = bsiter(livein[j], k)) >= 0; )
			print("%J ", targs[k]);
		print(")");
		print("(");
		for(k = -1; (k = bsiter(liveout[j], k)) >= 0; )
			print("%J ", targs[k]);
		print(")\n");
	}*/
}

static void
alutest(BitSet *cand, Targ *t)
{
	if(t == nil || t->num < 0 || !bstest(cand, t->num))
		return;
	bsrem(cand, t->num);
}

static int
log2(ulong n)
{
	int i;

	i = 0;
	if(n >= 0x10000) {n >>= 16; i += 16;}
	if(n >= 0x100) {n >>= 8; i += 8;}
	if(n >= 0x10) {n >>= 4; i += 4;}
	if(n >= 0x04) {n >>= 2; i += 2;}
	return i + (n / 2);
}

typedef struct Edge Edge;
struct Edge {
	IR *t, *a, *b;
	Edge *prev, *next;
	int ok, tp, bp, pri;
};

static Edge cgraph = {.next = &cgraph, .prev = &cgraph};

static Edge *
newedge(IR *t, int tp, IR *a, IR *b, int bp)
{
	Edge *e;
	
	e = emalloc(sizeof(Edge));
	e->t = t;
	e->tp = tp;
	e->a = a;
	e->b = b;
	e->bp = bp;
	e->next = &cgraph;
	e->prev = cgraph.prev;
	e->next->prev = e;
	e->prev->next = e;
	return e;
}

static void
findcand(void)
{
	IRBlock *b, *l;
	Edge *e, *f, *g;
	IR *i;
	int j, re, pri;
	Targ *t;
	
	l = &curfunc->bllist;
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = i->next){
			switch(i->op){
			case OPLSH:
			case OPRSH:
			case OPASR:
			case OPROR:
				if(i->b != nil && i->b->def != nil && i->b->def->a->t == TARGCONST)
					i->b = i->b->def->a;
				break;
			case OPMUL:
				if(i->b != nil && i->b->def != nil && i->b->def->a->t == TARGCONST){
					j = i->b->def->a->lval;
					if(j != 0 && (j & j-1) == 0){
						i->op = OPLSH;
						i->b = targ(TARGCONST, log2(j));
					}
				}else if(i->a != nil && i->a->def != nil && i->a->def->a->t == TARGCONST){
					j = i->a->def->a->lval;
					if(j != 0 && (j & j-1) == 0){
						i->op = OPLSH;
						i->a = i->b;
						i->b = targ(TARGCONST, log2(j));
					}
				}
				break;
			}
			
			switch(OPTYPE(i->op)){
			case OPNORM:
				if(i->a != nil && i->a->def != nil)
					newedge(i, 0, i->a->def, nil, 0);
				if(i->b != nil && i->b->def != nil)
					newedge(i, 1, i->b->def, nil, 0);
				break;
			case OPPHI:
				for(j = 0; j < i->φ.n; j++)
					if(i->φ.a[j] != nil && i->φ.a[j]->def != nil)
						newedge(i, j, i->φ.a[j]->def, nil, 0);
				break;
			case OPBRANCH:
				break;
			default:
				print("findcand: unimplemented type %#x\n", OPTYPE(i->op));
			}
		}
	for(e = cgraph.next; e != &cgraph; e = e->next)
		switch(e->t->op){
		case OPADD: case OPSUB: case OPAND: case OPOR: case OPXOR: case OPCMP: case OPCOM: case OPMOV:
			switch(e->a->op){
			case OPMOV:
				if(e->a->a->t == TARGCONST && legalc(e->a->a->lval))
					e->ok = 1;
				break;
			case OPLSH: case OPRSH: case OPASR: case OPROR:
				e->ok = 1;
				break;
			}
			break;
		case OPLD: case OPLDB: case OPLDSB: case OPLDH: case OPLDSH: case OPLDD:
		case OPST: case OPSTB: case OPSTH: case OPSTD:
			if(e->tp == 0)
				switch(e->a->op){
				case OPADD: case OPSUB:
					if(e->a->a != nil && e->a->a->def != nil && e->a->op == OPADD)
						newedge(e->t, e->tp, e->a->a->def, e->a, 0);
					if(e->a->b != nil && e->a->b->def != nil)
						newedge(e->t, e->tp, e->a->b->def, e->a, 1);
					e->ok = 1;
					break;
				case OPLSH: case OPRSH: case OPROR: case OPASR:
					if(e->a->b != nil && e->a->b->t == TARGCONST)
						e->ok = 1;
					break;
				case OPMOV:
					if(e->b != nil && e->a->a->t == TARGCONST && (e->a->a->lval & ~0xfff) == 0)
						e->ok = 1;
					break;
				}
			break;
		}
again:
	for(e = cgraph.next; e != &cgraph; e = e->next){
		if(!e->ok)
			continue;
		for(f = cgraph.next; f != &cgraph; f = f->next){
			if(e->a == f->a && f->b == nil && !f->ok){
				for(g = cgraph.next; g != &cgraph; g = g->next)
					if((f->a == g->a && f->t == g->b || f->a == g->b && f->t == g->t) && g->ok)
						goto no;
				e->ok = 0;
				break;
			}
			if(e->b == f->a && f->b == nil && !f->ok){
				for(g = cgraph.next; g != &cgraph; g = g->next)
					if((f->a == g->a && f->t == g->b || f->a == g->b && f->t == g->t) && g->ok)
						goto no;
				e->ok = 0;
				break;
			}
		no: ;
		}
	}
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = i->next){
			pri = 0x7fffffff;
			for(e = cgraph.next; e != &cgraph; e = e->next)
				if(e->ok && (e->t == i || e->b == i)){
					j = 0;
					for(f = cgraph.next; f != &cgraph; f = f->next)
						if(f->a == e->a && f->ok)
							j += 2;
					if(e->a->op == OPMOV)
						j--;
					if(e->b != nil)
						j -= 0x40000000;
					e->pri = j;
					if(j < pri)
						pri = j;
				}
			re = 0;
			for(e = cgraph.next; e != &cgraph; e = e->next)
				if(e->ok && (e->t == i || e->b == i) && e->pri > pri){
					e->ok = 0;
					re++;
				}
			if(re)
				goto again;
			}
/*	for(e = cgraph.next; e != &cgraph; e = e->next)
		if(e->b != nil)
			print("%d %I -> %I(%d) -> %I(%d)\n", e->ok, e->a, e->b, e->bp, e->t, e->tp);
		else
			print("%d %I -> %I(%d)\n", e->ok, e->a, e->t, e->tp);*/
	for(e = cgraph.next; e != &cgraph; e = e->next){
		if(!e->ok)
			continue;
		switch(e->a->op){
		case OPMOV:
			t = e->a->a;
			break;
		case OPLSH: case OPRSH: case OPASR: case OPROR: case OPADD: case OPSUB:
			t = targ(TARGSH, e->a->op, e->a->a, e->a->b);
			break;
		default:
			print("findcand: unimplemented op %#x\n", e->a->op);
			continue;
		}
		if(e->b != nil){
			if(!e->bp)
				t = targ(TARGSH, e->b->op, t, e->b->b);
			else
				t = targ(TARGSH, e->b->op, e->b->a, t);
			e->t->a = t;
		}else
			switch(e->t->op){
			case OPADD: case OPSUB: case OPAND: case OPOR: case OPXOR: case OPCMP: case OPCOM: case OPMOV:
				if(!e->tp){
					e->t->a = e->t->b;
					if(e->t->op == OPSUB)
						e->t->op = OPRSB;
				}
				e->t->b = t;
				break;
			default:
				if(e->tp)
					e->t->b = t;
				else
					e->t->a = t;
			}
	}
}

void
livetarg(BitSet *live, Targ *t)
{
	if(t == nil)
		return;
	if(t->num >= 0)
		bsadd(live, t->num);
	else if(t->t == TARGSH){
		livetarg(live, t->a);
		livetarg(live, t->b);
	}
}

static void
cleanup(void)
{
	IRBlock *b, *l;
	IR *i;
	BitSet *live;
	
	l = &curfunc->bllist;
	live = bsnew(ntargs);
	for(b = l->next; b != l; b = b->next){
		bscopy(live, liveout[b->num]);
		for(i = b->instr.prev; i != &b->instr; i = i->prev)
			switch(OPTYPE(i->op)){
			case OPNORM:
				if(i->r != nil && i->r->num >= 0 && !bstest(live, i->r->num)){
					i->prev->next = i->next;
					i->next->prev = i->prev;
					continue;
				}
				livetarg(live, i->a);
				livetarg(live, i->b);
				break;
			case OPBRANCH:
				break;
			case OPPHI:
				break;
			default:
				print("cleanup: unhandled type %#x\n", OPTYPE(i->op));
			}
	}
	bsfree(live);
}

void
codegen(void)
{
	deflabel();
	findcand();
	calclive();
	cleanup();
}
