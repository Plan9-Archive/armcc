#include <u.h>
#include <libc.h>
#include "dat.h"
#include "fns.h"

IRBlock **blocks;
int nblocks;
Symbol **syms;
BitSet **sblocks;
int nsyms;

void
addφ(IRBlock *b, IR *i)
{
	i->prev = &b->instr;
	i->next = b->instr.next;
	i->next->prev = i;
	i->prev->next = i;
}

IR *
newφ(Line *l, int n)
{
	IR *i;

	i = emalloc(sizeof(IR) + n * sizeof(Targ *));
	i->Line = *l;
	i->φ.a = (Targ **) (i + 1);
	i->φ.n = n;
	i->op = OPPHI;
	return i;
}

static int
irdfsvisit(IRBlock *b, int n)
{
	if(b == nil)
		return n;
	if(b->num != -1)
		return n;
	b->num = -2;
	n = irdfsvisit(b->branch, n);
	if(OPTYPE(b->instr.prev->op) == OPBRANCH)
		n = irdfsvisit(b->instr.prev->targ, n);
	b->num = n++;
	return n;
}

static void
irdfs(void)
{
	IRBlock *b, *l, **p;
	int nb, i;
	
	l = &curfunc->bllist;
	nb = -1;
	for(b = l->next; b != l; b = b->next){
		b->num = -1;
		nb++;
	}
	irdfsvisit(l->next, 0);
	for(b = l->next; b != l; b = b->next)
		b->num = nb - b->num;
	nb++;
	nblocks = nb;
	blocks = p = emalloc(sizeof(*p) * nb);
	for(b = l->next; b != l; b = b->next)
		p[b->num] = b;
	l->next = l->prev = l;
	for(i = 0; i < nb; i++){
		p[i]->next = l;
		p[i]->prev = l->prev;
		p[i]->next->prev = p[i];
		p[i]->prev->next = p[i];
	}
}

static IRBlock *
irinter(IRBlock *i, IRBlock *j)
{
	if(i == nil)
		return j;
	while(i != j){
		while(i->num > j->num)
			i = i->idom;
		while(j->num > i->num)
			j = j->idom;
	}
	return i;
}

void
irdom(void)
{
	IRBlock *b, *l, *c, ***p;
	int ch, i;
	
	l = &curfunc->bllist;
	for(b = l->next; b != l; b = b->next)
		b->idom = nil;
	l->next->idom = l->next;
	do{
		ch = 0;
		for(b = l->next->next; b != l; b = b->next){
			c = nil;
			for(i = 0; i < b->nfrom; i++)
				if(b->from[i]->idom != nil)
					c = irinter(c, b->from[i]);
			if(b->idom != c){
				ch++;
				b->idom = c;
			}
		}
	}while(ch != 0);
	p = emalloc(sizeof(IRBlock **) * nblocks);
	for(b = l->next; b != l; b = b->next){
		b->domch = nil;
		p[b->num] = &b->domch;
	}
	for(b = l->next->next; b != l; b = b->next)
		if(b->idom != nil && b->idom != b){
			*p[b->idom->num] = b;
			p[b->idom->num] = &b->chnext;
		}
	free(p);
}

static void
addfront(IRBlock *b, IRBlock *c)
{
	int i;
	
	for(i = 0; i < b->nfront; i++)
		if(b->front[i] == c)
			return;
	if(b->nafront == i){
		b->nafront += 5;
		b->front = realloc(b->front, b->nafront * sizeof(IRBlock *));
	}
	b->front[i] = c;
	b->nfront++;
}

static void
irfront(void)
{
	IRBlock *b, *l, *r;
	int i;
	
	l = &curfunc->bllist;
	for(b = l->next; b != l; b = b->next){
		if(b->nfrom < 2)
			continue;
		for(i = 0; i < b->nfrom; i++){
			r = b->from[i];
			while(r != b->idom){
				addfront(r, b);
				r = r->idom;
			}
		}
	}
}

static void
irindmov(void)
{
	IRBlock *b, *l;
	IR *i;
	
	l = &curfunc->bllist;
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = i->next)
			if(i->op == OPMOV){
				if(i->a != nil && i->a->t == TARGIND){
					i->op = OPLD;
					i->a = i->a->link;
				}
				if(i->r != nil && i->r->t == TARGIND){
					i->op = OPST;
					i->b = i->a;
					i->a = i->r->link;
					i->r = nil;
				}
			}
}

static void
iraddsym(Symbol *s, int *nap)
{
	if((s->class & (BMEM|BSTATIC)) != 0 || s->num >= 0)
		return;
	if(nsyms >= *nap){
		*nap += 10;
		syms = realloc(syms, *nap * sizeof(Symbol *));
		sblocks = realloc(sblocks, *nap * sizeof(BitSet *));
	}
	syms[nsyms] = s;
	sblocks[nsyms] = bsnew(nblocks);
	s->num = nsyms++;
}

static void
numvars(void)
{
	int na, j;
	IRBlock *b, *l;
	IR *i;
	
	na = 0;
	l = &curfunc->bllist;
	for(b = l->next; b != l; b = b->next)
		for(i = b->instr.next; i != &b->instr; i = i->next)
			switch(OPTYPE(i->op)){
			case OPNORM:
				if(i->a != nil && i->a->t == TARGSYM)
					iraddsym(i->a->sym, &na);
				if(i->b != nil && i->b->t == TARGSYM)
					iraddsym(i->b->sym, &na);
				if(i->r != nil && i->r->t == TARGSYM)
					iraddsym(i->r->sym, &na);
				break;
			case OPBRANCH:
				break;
			case OPPHI:
				if(i->φ.r != nil && i->φ.r->t == TARGSYM)
					iraddsym(i->φ.r->sym, &na);
				for(j = 0; j < i->φ.n; j++)
					if(i->φ.a[j] != nil && i->φ.a[j]->t == TARGSYM)
						iraddsym(i->φ.a[j]->sym, &na);
				break;
			default:
				print("numvars: unimplemented %#x\n", OPTYPE(i->op));
			}
}

static void
irφinsert(void)
{
	BitSet *varkill, *globals, *visit, *hasφ;
	IR *i;
	Targ *t;
	IRBlock *b, *c, *l, *wl, **p;
	int j, k;
	Symbol *s;

	l = &curfunc->bllist;
	varkill = bsnew(nsyms);
	globals = bsnew(nsyms);
	for(b = l->next; b != l; b = b->next){
		bsreset(varkill);
		for(i = b->instr.next; i != &b->instr; i = i->next)
			switch(OPTYPE(i->op)){
			case OPNORM:
				if(i->a != nil && i->a->t == TARGSYM && i->a->sym->num >= 0 && !bstest(varkill, i->a->sym->num))
					bsadd(globals, i->a->sym->num);
				if(i->b != nil && i->b->t == TARGSYM && i->b->sym->num >= 0 && !bstest(varkill, i->b->sym->num))
					bsadd(globals, i->b->sym->num);
				if(i->r != nil && i->r->t == TARGSYM && i->r->sym->num >= 0){
					bsadd(varkill, i->r->sym->num);
					bsadd(sblocks[i->r->sym->num], b->num);
				}
				break;
			case OPPHI:
				if(i->φ.r != nil && i->φ.r->t == TARGSYM && i->φ.r->sym->num >= 0){
					bsadd(varkill, i->φ.r->sym->num);
					bsadd(sblocks[i->φ.r->sym->num], b->num);
				}
				for(j = 0; j < i->φ.n; j++){
					t = i->φ.a[j];
					if(t != nil && t->t == TARGSYM && t->sym->num >= 0 && !bstest(varkill, t->sym->num))
						bsadd(globals, t->sym->num);
				}
				break;		
			case OPBRANCH:
				break;
			default:
				print("irφinsert: unimplemented %#x\n", OPTYPE(i->op));
			}
	}
	visit = bsnew(nblocks);
	hasφ = bsnew(nblocks);
	for(j = -1; (j = bsiter(globals, j)) >= 0; ){
		s = syms[j];
		wl = nil;
		p = &wl;
		bscopy(visit, sblocks[j]);
		bsreset(hasφ);
		for(k = -1; (k = bsiter(sblocks[j], k)) >= 0; ){
			blocks[k]->link = nil;
			*p = blocks[k];
			p = &blocks[k]->link;
		}
		for(b = wl; b != nil; b = b->link)
			for(k = 0; k < b->nfront; k++){
				c = b->front[k];
				if(!bsadd(hasφ, c->num)){
					i = newφ(b->instr.next, c->nfrom);
					i->φ.r = s->targ;
					addφ(c, i);
				}
				if(!bstest(visit, c->num)){
					bsadd(visit, c->num);
					c->link = nil;
					*p = c;
					p = &c->link;
				}
			}
	}
	free(hasφ);
	free(visit);
	free(varkill);
	free(globals);
}

int *φctr;
Targ **φstack;

static Targ *
irφnewname(int n)
{
	return φstack[n] = targ(TARGSSA, syms[n], φctr[n]++, φstack[n]);
}

static void
irφrenblk(IRBlock *b)
{
	IR *i;
	int j, k;
	IRBlock *c;
	
	for(i = b->instr.next; i != &b->instr; i = i->next)
		switch(OPTYPE(i->op)){
		case OPNORM:
			if(i->a != nil && i->a->t == TARGSYM && i->a->sym->num >= 0)
				if(φstack[i->a->sym->num] == nil){
					warn(i, "used but not set: %s", i->a->sym->name);
					i->a = irφnewname(i->a->sym->num);
				}else
					i->a = φstack[i->a->sym->num];
			if(i->b != nil && i->b->t == TARGSYM && i->b->sym->num >= 0)
				if(φstack[i->b->sym->num] == nil){
					warn(i, "used but not set: %s", i->b->sym->name);
					i->b = irφnewname(i->b->sym->num);
				}else
					i->b = φstack[i->b->sym->num];
			if(i->r != nil && i->r->t == TARGSYM && i->r->sym->num >= 0)
				i->r = irφnewname(i->r->sym->num);
			break;
		case OPPHI:
			if(i->φ.r->t == TARGSYM)
				i->φ.r = irφnewname(i->φ.r->sym->num);
			break;
		case OPBRANCH:
			break;
		default:
			print("irφrenblk: unhandled %#x\n", OPTYPE(i->op));
		}
	for(j = 0; j < b->nto; j++){
		c = b->to[j];
		for(k = 0; k < c->nfrom && c->from[k] != b; k++)
			;
		if(k == c->nfrom){
			print("%p not in %p's 'from' list\n", b, c);
			continue;
		}
		for(i = c->instr.next; i->op == OPPHI; i = i->next)
			if(i->φ.r->t == TARGSYM || i->φ.r->t == TARGSSA)
				i->φ.a[k] = φstack[i->φ.r->sym->num];
	}
	for(c = b->domch; c != nil; c = c->chnext)
		irφrenblk(c);
	for(i = b->instr.next; i != &b->instr; i = i->next)
		switch(OPTYPE(i->op)){
		case OPNORM:
			if(i->r != nil && i->r->t == TARGSSA)
				φstack[i->r->sym->num] = φstack[i->r->sym->num]->up;
			break;
		case OPPHI:
			if(i->φ.r->t == TARGSSA)
				φstack[i->φ.r->sym->num] = φstack[i->φ.r->sym->num]->up;
			break;
		case OPBRANCH:
			break;
		default:
			print("irφrenblk: unhandled %#x\n", OPTYPE(i->op));
		}
}

static void
irφrename(void)
{
	φctr = emalloc(nsyms * sizeof(int));
	φstack = emalloc(nsyms * sizeof(Targ));
	irφrenblk(blocks[0]);
	free(φctr);
	free(φstack);
}

void
irtossa(void)
{
	int i;

	irdfs();
	irdom();
	irfront();
	irindmov();
	numvars();
	irφinsert();
	irφrename();
	
	for(i = 0; i < nsyms; i++)
		free(sblocks[i]);
	free(syms);
}
