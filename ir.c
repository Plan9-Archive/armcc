#include <u.h>
#include <libc.h>
#include "dat.h"
#include "fns.h"

Function *curfunc;

typedef struct Patch Patch;

struct Patch {
	Line;
	IRBlock **p;
	Symbol *sym;
	Patch *next;
};

Patch *patches;

void
newfunc(Type *t)
{
	curfunc = emalloc(sizeof(Function));
	if(t->t != TFUNC)
		sysfatal("newfunc: not a function type");
	curfunc->type = t;
	curfunc->bllist.next = curfunc->bllist.prev = &curfunc->bllist;
}

static void
addir(IRBlock *b, IR *i)
{
	i->prev = b->instr.prev;
	i->next = &b->instr;
	i->prev->next = i;
	i->next->prev = i;
}

IR *
irop(Line *l, int op)
{
	IR *i;
	
	i = emalloc(sizeof(*i));
	i->Line = *l;
	i->op = op;
	return i;
}

static void
delir(IR *i)
{
	i->next->prev = i->prev;
	i->prev->next = i->next;
}

Targ *
targ(int n, ...)
{
	va_list va;
	Targ *t;
	
	va_start(va, n);
	t = emalloc(sizeof(*t));
	t->num = -1;
	t->Rn = -1;
	t->t = n;
	switch(n){
	case TARGRETV:
		t->type = va_arg(va, Type *);
		break;
	case TARGFP:
	case TARGCONST:
	case TARGARG:
		t->off = va_arg(va, int);
		t->type = regtype;
		break;
	case TARGSYM:
		t->sym = va_arg(va, Symbol *);
		t->type = t->sym->t;
		break;
	case TARGSSA:
		t->sym = va_arg(va, Symbol *);
		t->type = t->sym->t;
		t->n = va_arg(va, int);
		t->up = va_arg(va, Targ *);
		break;
	case TARGIND:
		t->link = va_arg(va, Targ *);
		t->type = va_arg(va, Type *);
		break;
	case TARGADDR:
		t->sym = va_arg(va, Symbol *);
		t->n = va_arg(va, int);
		break;
	case TARGSH:
		t->op = va_arg(va, int);
		t->a = va_arg(va, Targ *);
		t->b = va_arg(va, Targ *);
		break;
	default:
		sysfatal("targ: unhandled %d", n);
	}
	va_end(va);
	return t;
}

static Targ *
newtemp(Type *type)
{
	Targ *t;
	
	t = emalloc(sizeof(*t));
	t->t = TARGTEMP;
	t->tn = curfunc->tempcnt++;
	t->num = -1;
	t->Rn = -1;
	t->type = type;
	return t;
}

static IRBlock *
newblock(void)
{
	IRBlock *bl;
	
	bl = emalloc(sizeof(*bl));
	bl->instr.next = bl->instr.prev = &bl->instr;
	bl->next = &curfunc->bllist;
	bl->prev = curfunc->bllist.prev;
	bl->next->prev = bl;
	bl->prev->next = bl;
	return bl;
}

static IRBlock *
delblock(IRBlock *b)
{
	IRBlock *bn;
	
	bn = b->next;
	b->next->prev = b->prev;
	b->prev->next = b->next;
	free(b);
	return bn;
}

int
legalc(ulong c)
{
	c |= c >> 12 | c << 20;
	c |= c >> 6 | c << 26;
	c |= c >> 3 | c << 29;
	c |= (c >> 1 | c << 30) | (c >> 2 | c << 29);
	return ~c & 0x55555555;
}

static Targ *
irread(Line *l, Targ *t, IRBlock *b, int arith)
{
	Type *ty;
	Symbol *s;
	IR *i;

	ty = regtype;
	switch(t->t){
	case TARGSYM:
		s = t->sym;
		if((s->class & BMEM) != 0){
			ty = s->t;
			goto move;
		}
		return t;
	default:
		print("irread: unhandled %d\n", t->t);
		goto move;
	case TARGCONST:
	/*	if(arith && legalc(t->lval))
			return t;*/
		goto move;
	case TARGADDR:
		goto move;
	case TARGTEMP:
		return t;
	case TARGIND:
		ty = t->type;
		break;
	}
move:
	i = irop(l, OPMOV);
	i->r = newtemp(ty);
	i->a = t;
	addir(b, i);
	return i->r;	
}

static Targ *
ircast(Line *l, IRBlock *b, Targ *t, int type)
{
	IR *i;
	int o;

	if(type)
		o = OPSXTB + ((type - 1) >> 1);
	else
		o = OPZXTB + ((type - 1) >> 1);
	i = irop(l, o);
	i->r = newtemp(t->type);
	i->a = t;
	addir(b, i);
	return i->r;
}

static Targ *irexpr(ASTNode *, IRBlock *, IRBlock **);

static Targ *
addrof(ASTNode *n, IRBlock *b, IRBlock **br)
{
	IR *i;

	switch(n->t){
	case ASTSYM:
		*br = b;
		return targ(TARGADDR, n->sym, 0);
	case ASTMEMB:
		i = irop(n, OPADD);
		i->r = newtemp(regtype);
		i->a = addrof(n->n, b, &b);
		i->b = targ(TARGCONST, n->mb.m != nil ? n->mb.m->off : 0);
		addir(b, i);
		*br = b;
		return i->r;
	case ASTDEREF:
		return irexpr(n->n, b, br);
	default:
		sysfatal("addrof: unhandled %A", n->t);
		return nil;
	}
}

static void condbranch(ASTNode *, IRBlock *, IRBlock *, IRBlock *);

static Targ *
irexpr(ASTNode *n, IRBlock *b, IRBlock **br)
{
	IRBlock *b0, *b1, *b2;
	Targ *t, *l, *r, **a;
	IR *i;
	int k, o;
	Type *ty;

	*br = b;
	switch(n->t){
	case ASTSYM:
		if((t = n->sym->targ) != nil)
			return t;
		return n->sym->targ = targ(TARGSYM, n->sym);
	case ASTCLONG:
		return targ(TARGCONST, n->lval);
	case ASTADDROF:
		return addrof(n->n, b, br);
	case ASTBIN:
		switch(OPTYPE(n->op)){
		case OPNORM:
			k = n->type == &types[TFLOAT] || n->type == &types[TDOUBLE] ? OPFADD - OPADD : 0;
			i = irop(n, n->op + k);
			i->a = irexpr(n->n1, b, &b);
			i->a = irread(n, i->a, b, 1);
			i->b = irexpr(n->n2, b, &b);
			i->b = irread(n, i->b, b, 1);
			i->r = newtemp(n->type);
			addir(b, i);
			*br = b;
			return i->r;
		case OPBRANCH:
			b0 = newblock();
			b1 = newblock();
			b2 = newblock();
			condbranch(n, b, b0, b1);
			i = irop(n, OPMOV);
			i->a = targ(TARGCONST, 1);
			i->r = t = newtemp(regtype);
			addir(b0, i);
			b0->branch = b2;
			i = irop(n, OPMOV);
			i->a = targ(TARGCONST, 0);
			i->r = r = newtemp(regtype);
			addir(b1, i);
			b1->branch = b2;
			i = newφ(n, 2);
			i->φ.r = l = newtemp(regtype);
			i->φ.a[0] = t;
			i->φ.a[1] = r;
			addφ(b2, i);
			*br = b2;
			return l;
		default:
			error(n, "irexpr: can't handle binary op %#x", OPTYPE(n->op));
			return nil;
		}
	case ASTUN:
		i = irop(n, n->op);
		i->a = irexpr(n->n1, b, &b);
		i->a = irread(n, i->a, b, 1);
		i->r = newtemp(n->type);
		addir(b, i);
		*br = b;
		return i->r;
	case ASTDEREF:
		t = irexpr(n->n, b, &b);
		t = targ(TARGIND, irread(n, t, b, 0), n->type);
		*br = b;
		return t;
	case ASTMEMB:
		return targ(TARGIND, addrof(n, b, br), n->type);
	case ASTASS:
		if(n->op == OPMOV){
			i = irop(n, OPMOV);
			t = irexpr(n->n2, b, &b);
			i->a = t = irread(n, t, b, 0);
			i->r = irexpr(n->n1, b, &b);
			addir(b, i);
			*br = b;
		}else{
			i = irop(n, n->op);
			i->b = irexpr(n->n2, b, &b);
			l = irexpr(n->n1, b, &b);
			i->a = irread(n, l, b, 1);
			i->r = t = newtemp(n->type);
			addir(b, i);
			if(n->op <= OPMOD && n->type->t < TUINT)
				t = ircast(n, b, t, n->type->t);
			i = irop(n, OPMOV);
			i->r = l;
			i->a = t;
			addir(b, i);
			*br = b;
		}
		return t;
	case ASTINC:
		k = n->type->t == TIND ? n->type->link->size : 1;
		i = irop(n, n->op <= POSTINC ? OPADD : OPSUB);
		i->r = t = newtemp(n->type);
		l = irexpr(n->n1, b, &b);
		i->a = r = irread(n, l, b, 1);
		i->b = targ(TARGCONST, k);
		addir(b, i);
		if(n->type->t < TUINT)
			t = ircast(n, b, t, n->type->t);
		if((n->op & 1) != 0){
			i = irop(n, OPMOV);
			i->a = r;
			i->r = r = newtemp(n->type);
			addir(b, i);
		}else
			r = t;
		i = irop(n, OPMOV);
		i->r = l;
		i->a = t;
		addir(b, i);
		return r;
	case ASTTERN:
		b0 = newblock();
		b1 = newblock();
		b2 = newblock();
		l = newtemp(n->type);
		condbranch(n->cond, b, b0, b1);
		t = irexpr(n->block, b0, &b0);
		i = irop(n, OPMOV);
		i->a = t;
		i->r = t = newtemp(n->type);
		addir(b0, i);
		b0->branch = b2;
		r = irexpr(n->elsebl, b1, &b1);
		i = irop(n, OPMOV);
		i->a = r;
		i->r = r = newtemp(n->type);
		addir(b1, i);
		b1->branch = b2;
		i = newφ(n, 2);
		i->φ.r = l;
		i->φ.a[0] = t;
		i->φ.a[1] = r;
		addφ(b2, i);
		*br = b2;
		return l;
	case ASTCOMMA:
		irexpr(n->n1, b, &b);
		return irexpr(n->n2, b, br);
	case ASTCALL:
		a = emalloc(sizeof(Targ*) * n->func.argn);
		for(k = 0; k < n->func.argn; k++)
			a[k] = irexpr(n->func.args[k], b, &b);
		o = 0;
		for(k = 0; k < n->func.argn; k++){
			i = irop(n, OPMOV);
			i->a = a[k];
			ty = n->func.args[k]->type;
			if(k > 0 || ty->t == TFLOAT || ty->t == TDOUBLE)
				i->r = targ(TARGIND, targ(TARGARG, o), ty);
			else
				i->r = targ(TARGRETV, regtype);
			addir(b, i);
			o += ty->size;
			o = o + 3 & -4;
		}
		free(a);
		i = irop(n, OPCALL);
		i->a = irread(n, addrof(n->func.n, b, &b), b, 0);
		addir(b, i);
		i = irop(n, OPMOV);
		i->r = newtemp(n->type);
		i->a = targ(TARGRETV, n->type);
		addir(b, i);
		*br = b;
		return i->r;
	default:
		sysfatal("irexpr: unhandled %A", n->t);
		return nil;
	}
}

static void
branch(IRBlock *cur, IRBlock *next)
{
	if(cur != nil)
		cur->branch = next;
}

static void
condbranch(ASTNode *n, IRBlock *cur, IRBlock *true, IRBlock *false)
{
	IR *i;
	IRBlock *b0;

	switch(n->t){
	case ASTBIN:
		if(n->op == OPLAND){
			b0 = newblock();
			condbranch(n->n1, cur, b0, false);
			condbranch(n->n2, b0, true, false);
			return;
		}
		if(n->op == OPLOR){
			b0 = newblock();
			condbranch(n->n1, cur, true, b0);
			condbranch(n->n2, b0, true, false);
			return;
		}
		if(OPTYPE(n->op) == OPBRANCH){
			i = irop(n, OPCMP);
			i->a = irexpr(n->n1, cur, &cur);
			i->a = irread(n, i->a, cur, 1);
			i->b = irexpr(n->n2, cur, &cur);
			i->b = irread(n, i->b, cur, 1);
			addir(cur, i);
			i = irop(n, n->op);
			i->targ = true;
			addir(cur, i);
			cur->branch = false;
			return;
		}
	case ASTUN:
		if(n->op == OPISZ){
			condbranch(n->n1, cur, false, true);
			return;
		}
	default:
		i = irop(n, OPTST);
		i->a = irexpr(n, cur, &cur);
		addir(cur, i);
		i = irop(n, OPNE);
		i->targ = true;
		addir(cur, i);
		cur->branch = false;
		break;
	case ASTCLONG:
		if(n->lval != 0)
			cur->branch = true;
		else
			cur->branch = false;
		return;
	}
}

static IRBlock *
irstat(ASTNode *n, IRBlock *b, IRBlock *brk, IRBlock *cont)
{
	IRBlock *b1, *b2, *b3;
	ASTNode *m;
	Patch *p;
	IR *i;

	if(n == nil)
		return b;
	if(b == nil){
		error(n, "unreachable code %A", n->t);
		return nil;
	}
	switch(n->t){
	default:
		irexpr(n, b, &b);
		return b;
	case ASTIF:
		b1 = newblock();
		b2 = newblock();
		if(n->elsebl != nil){
			b3 = newblock();
			condbranch(n->cond, b, b1, b2);
			b1 = irstat(n->block, b1, brk, cont);
			b2 = irstat(n->elsebl, b2, brk, cont);
			branch(b1, b3);
			branch(b2, b3);
			return b3;
		}else{
			condbranch(n->cond, b, b1, b2);
			b1 = irstat(n->block, b1, brk, cont);
			branch(b1, b2);
			return b2;
		}
	case ASTWHILE:
		b1 = newblock();
		b2 = newblock();
		b3 = newblock();
		condbranch(n->cond, b, b1, b3);
		branch(irstat(n->block, b1, b3, b2), b2);
		condbranch(n->cond, b2, b1, b3);
		return b3;
	case ASTDOWHILE:
		b1 = newblock();
		b2 = newblock();
		b3 = newblock();
		b->branch = b1;
		branch(irstat(n->block, b1, b3, b2), b2);
		condbranch(n->cond, b2, b1, b3);
		return b3;
	case ASTFOR:
		if(n->forl.once != nil)
			irexpr(n->forl.once, b, &b);
		b1 = newblock();
		b2 = newblock();
		b3 = newblock();
		if(n->forl.cond != nil)
			condbranch(n->forl.cond, b, b1, b3);
		else
			b->branch = b1;
		branch(irstat(n->forl.block, b1, b3, b2), b2);
		if(n->forl.iter != nil)
			irexpr(n->forl.iter, b2, &b2);
		if(n->forl.cond != nil)
			condbranch(n->forl.cond, b2, b1, b3);
		else
			b2->branch = b1;
		return b3;
	case ASTBLOCK:
		for(m = n->first; m != nil; m = m->next)
			b = irstat(m, b, brk, cont);
		return b;
	case ASTBREAK:
		b->branch = brk;
		return nil;
	case ASTCONTINUE:
		b->branch = cont;
		return nil;
	case ASTLABEL:
		return n->sym->label = b->branch = newblock();
	case ASTGOTO:
		if(n->sym->label != nil)
			b->branch = n->sym->label;
		else{
			p = emalloc(sizeof(*p));
			p->Line = n->Line;
			p->sym = n->sym;
			p->p = &b->branch;
			p->next = patches;
			patches = p;
		}
		return nil;
	case ASTRETURN:
		if(n->n != nil){
			i = irop(n, OPMOV);
			i->a = irexpr(n->n, b, &b);
			i->r = targ(TARGRETV, n->n->type);
			addir(b, i);
		}
		b->branch = nil;
		return nil;
	}
}

void
irprint(void)
{
	IRBlock *b;
	IR *i;
	int j;

	for(b = curfunc->bllist.next; b != &curfunc->bllist; b = b->next){
		print("\n%p (", b);
		for(j = 0; j < b->nfrom; j++){
			if(j != 0)
				print(", ");
			print("%p", b->from[j]);
		}
		print("):\n");
		for(i = b->instr.next; i != &b->instr; i = i->next)
			print("\t%I\n", i);
		if(b->branch != nil)
			print("\tB %p\n", b->branch);
		else
			print("\tRET\n");
	}
}

static void
irpatch(void)
{
	Patch *p, *q;
	
	for(p = patches; p != nil; p = q){
		q = p->next;
		if(p->sym->label == nil)
			error(p, "no such label '%s'", p->sym->name);
		else
			*p->p = p->sym->label;
		free(p);
	}
}

static void
irmerge(void)
{
	IRBlock *b, *c;
	int ch, n;
	
restart:
	for(b = curfunc->bllist.next; b != &curfunc->bllist; b = b->next)
		b->nfrom = 0;
	curfunc->bllist.next->nfrom++;
	for(b = curfunc->bllist.next; b != &curfunc->bllist; b = b->next){
		if(b->branch != nil)
			b->branch->nfrom++;
		if(OPTYPE(b->instr.prev->op) == OPBRANCH && b->instr.prev->targ != nil)
			b->instr.prev->targ->nfrom++;
	}
	ch = 0;
	for(b = curfunc->bllist.next; b != &curfunc->bllist; )
		if(b->nfrom == 0){
			b = delblock(b);
			ch++;
		}else if(b->branch != nil && b->branch->nfrom == 1 && b->branch != b && OPTYPE(b->instr.prev->op) != OPBRANCH){
			c = b->branch;
			b->branch = c->branch;
			b->instr.prev->next = c->instr.next;
			c->instr.next->prev = b->instr.prev;
			c->instr.prev->next = &b->instr;
			b->instr.prev = c->instr.prev;
			c->instr.prev = c->instr.next = &c->instr;
			delblock(c);
		}else if(b->branch != nil && b->branch->instr.next == &b->branch->instr && b->branch != b->branch->branch && b->branch != b){
			b->branch->nfrom--;
			if(b->branch->branch != nil)
				b->branch->branch->nfrom++;
			b->branch = b->branch->branch;
			ch++;
			b = b->next;
		}else if(OPTYPE(b->instr.prev->op) == OPBRANCH && (c = b->instr.prev->targ, c != nil && c->instr.next == &c->instr)){
			c->nfrom--;
			if(c->branch != nil)
				c->branch->nfrom++;
			b->instr.prev->targ = c->branch;
			ch++;
		}else if(OPTYPE(b->instr.prev->op) == OPBRANCH && b->instr.prev->targ == b->branch){
			b->branch->nfrom--;
			delir(b->instr.prev);
			ch++;
		}else
			b = b->next;
	if(ch)
		goto restart;
	n = 0;
	for(b = curfunc->bllist.next; b != &curfunc->bllist; b = b->next){
		b->from = emalloc(b->nfrom * sizeof(IRBlock));
		b->nfrom = 0;
		n++;
	}
	for(b = curfunc->bllist.next; b != &curfunc->bllist; b = b->next){
		c = b->branch;
		if(c != nil){
			b->to[b->nto++] = c;
			c->from[c->nfrom++] = b;
		}
		if(OPTYPE(b->instr.prev->op) == OPBRANCH && (c = b->instr.prev->targ) != nil){
			b->to[b->nto++] = c;
			c->from[c->nfrom++] = b;
		}
	}
}

void
irgen(ASTNode *n)
{
	IRBlock *b;
	Symbol *s;
	int i, o;
	IR *ir;

	b = newblock();
	o = 0;
	for(i = 0; i < curfunc->type->nmemb; i++)
		if((s = curfunc->arg[i]) != 0){
			ir = irop(n, OPMOV);
			ir->r = s->targ = targ(TARGSYM, s);
			if(i > 0 || s->t == &types[TFLOAT] || s->t == &types[TDOUBLE])
				ir->a = targ(TARGIND, targ(TARGFP, o), s->t);
			else
				ir->a = targ(TARGRETV, regtype);
			o = o + s->t->size + 3 & -4;
			addir(b, ir);
		}
	irstat(n, b, nil, nil);
	irpatch();
	irmerge();
	irtossa();
	dvn();
	dead();
	codegen();
	regalloc();
	irprint();
}
