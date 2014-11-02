#include <u.h>
#include <libc.h>
#include "dat.h"
#include "fns.h"

enum {
	NIREG = 4,
	NFREG = 4,
	RELBLK = 8,
};

static BitSet *floats;

typedef struct Node Node;

extern BitSet **liveout, **livein;

struct Node {
	Targ *t;
	BitSet *adj;
	Node **rel;
	int nrel;
	IR *luse;
};

static Node *nodes, *stack;

static void
addrel(Targ *a, Targ *b)
{
	Node *p;

	if(a == nil || b == nil || a->num < 0 || b->num < 0)
		return;
	p = &nodes[a->num];
	if((p->nrel & RELBLK - 1) == 0)
		p->rel = realloc(p->rel, (p->nrel + RELBLK) * sizeof(Node *));
	p->rel[p->nrel++] = &nodes[b->num];
}

static void
findadj(void)
{
	IRBlock *b, *l;
	IR *i;
	BitSet *live;
	int j;
	extern void livetarg(BitSet *, Targ *);
	
	l = &curfunc->bllist;
	live = bsnew(ntargs);
	for(b = l->next; b != l; b = b->next){
		bscopy(live, liveout[b->num]);
		for(i = b->instr.prev; i != &b->instr; i = i->prev)
			switch(OPTYPE(i->op)){
			case OPNORM:
				if(i->r != nil && i->r->num >= 0)
					bsrem(live, i->r->num);
				livetarg(live, i->a);
				livetarg(live, i->b);
				for(j = -1; (j = bsiter(live, j)) >= 0; )
					bsunion(nodes[j].adj, nodes[j].adj, live);
				if(i->op == OPMOV){
					addrel(i->a, i->b);
					addrel(i->b, i->a);
				}
				break;
			}
	}
	bsfree(live);
}

static void
getuserec(Targ *t, int *u, int *nu)
{
	if(t == nil)
		return;
	if(t->t == TARGSH){
		getuserec(t->a, u, nu);
		getuserec(t->b, u, nu);
	}else if(t->num >= 0){
		assert(*nu < MAXUSE);
		u[(*nu)++] = t->num;
	}
}

static int
getuse(IR *i, int *u)
{
	int nu;
	
	nu = 0;
	getuserec(i->a, u, &nu);
	getuserec(i->b, u, &nu);
	return nu;
}

static int *press;
static int *nuselb, **nusein, **nuseout, *nuseloc;
static int *spillloc;
static int spfree;
static BitSet *ntmpuse, *ntmpdef, **nusedin;

static void
sptransuse(Targ *t, int i, int d)
{
	if(t == nil)
		return;
	if(t->num >= 0){
		bsadd(nusedin[i], t->num);
		if(!bstest(ntmpdef, t->num) && !bstest(ntmpuse, t->num)){
			bsadd(ntmpuse, t->num);
			nusein[i][t->num] = d;
		}
	}else if(t->t == TARGSH){
		sptransuse(t->a, i, d);
		sptransuse(t->b, i, d);
	}
}

static void
sptransfer(IRBlock *b)
{
	int *in, *out;
	IR *i;
	int j, k, d;
	IRBlock *c;
	
	in = nusein[b->num];
	out = nuseout[b->num];
	bsreset(ntmpuse);
	bsreset(ntmpdef);
	for(i = b->instr.next, d = 0; i != &b->instr; i = i->next, d++)
		switch(OPTYPE(i->op)){
		case OPNORM:
			sptransuse(i->a, b->num, d);
			sptransuse(i->b, b->num, d);
			if(i->r != nil && i->r->num >= 0)
				bsadd(ntmpdef, i->r->num);
			break;
		case OPBRANCH:
			break;
		case OPPHI:
		/*	if(i->φ.r != nil && i->φ.r->num >= 0)
				bsadd(ntmpdef, i->φ.r->num);*/
			break;
		default:
			print("sptransfer: unimplemented op type %#x\n", OPTYPE(i->op));
		}
	for(j = 0; j < b->nto; j++){
		if((c = b->to[j]) == nil)
			continue;
		for(k = 0; k < c->nfrom; k++)
			if(c->from[k] == b)
				break;
		for(i = c->instr.next; OPTYPE(i->op) == OPPHI; i = i->next)
			if(i->φ.n > k)
				sptransuse(i->φ.a[k], b->num, d);
	}
	for(j = 0; j < ntargs; j++){
		if(!bstest(ntmpuse, j))
			in[j] = out[j] + (out[j] >= 0 ? d : 0);
		if(bstest(ntmpdef, j))
			in[j] = -1;
		if(in[j] >= 0)
			in[j] += nuselb[b->num];
	}
}

static void
sptransunion(int *a, int *b)
{
	int n;
	
	for(n = ntargs; n--; a++, b++)
		if(*a < 0 || *b < *a && *b >= 0)
			*a = *b;
}

static void
splive(void)
{
	int i, j, ch;
	IRBlock *b, *c, *l;
	int *tmp;
	Loop *lp;

	l = &curfunc->bllist;
	nusein = emalloc(sizeof(int*) * nblocks);
	nuseout = emalloc(sizeof(int*) * nblocks);
	nusedin = emalloc(sizeof(IRBlock*) * nblocks);
	nuselb = emalloc(sizeof(int) * nblocks);
	press = emalloc(sizeof(int) * 2 * nblocks);
	for(b = l->next; b != l; b = b->next){
		nusedin[b->num] = bsnew(ntargs);
		press[b->num] = -1;
		for(i = 0; i < b->nfrom; i++){
			c = b->from[i];
			for(lp = curfunc->loops; lp != nil; lp = lp->next)
				if(bstest(lp->bl, c->num) && !bstest(lp->bl, b->num)){
					nuselb[b->num] += 100000;
					break;
				}
		}
	}
	tmp = emalloc(sizeof(int) * ntargs);
	for(i = 0; i < nblocks; i++){
		nusein[i] = emalloc(sizeof(int) * ntargs);
		nuseout[i] = emalloc(sizeof(int) * ntargs);
		memset(nuseout[i], -1, sizeof(int) * ntargs);
		memset(nusein[i], -1, sizeof(int) * ntargs);
	}
	ntmpuse = bsnew(ntargs);
	ntmpdef = bsnew(ntargs);
	do{
		ch = 0;
		for(b = l->prev; b != l; b = b->prev){
			memset(tmp, -1, sizeof(int) * ntargs);
			for(j = 0; j < b->nto; j++)
				sptransunion(tmp, nusein[b->to[j]->num]);
			if(memcmp(tmp, nuseout[b->num], sizeof(int) * ntargs) != 0)
				ch++;
			memcpy(nuseout[b->num], tmp, sizeof(int) * ntargs);
			memcpy(tmp, nusein[b->num], sizeof(int) * ntargs);
			sptransfer(b);
			if(memcmp(tmp, nusein[b->num], sizeof(int) * ntargs) != 0)
				ch++;
/*			print("%p ", b);
			for(j = 0; j < ntargs; j++){
				if(nusein[b->num][j] >= 0)
					print("%J:%d ", targs[j], nusein[b->num][j]);
				if(nuseout[b->num][j] >= 0)
					print("%J;%d ", targs[j], nuseout[b->num][j]);
				}
			print("\n");*/
		}
	}while(ch);
	
}

static void
pressuse(Targ *t, BitSet *live, int *nl)
{
	if(t == nil)
		return;
	if(t->t == TARGSH){
		pressuse(t->a, live, nl);
		pressuse(t->b, live, nl);
	}else if(t->num >= 0)
		if(!bsadd(live, t->num))
			nl[bstest(floats, t->num)]++;
}

static void
nlcheck(int *nl, int *mp)
{
	if(nl[0] > mp[0])
		mp[0] = nl[0];
	if(nl[1] > mp[1])
		mp[1] = nl[1];
}

static void
pressure(IRBlock *b, int *mp)
{
	int nl[2], j, k, *nuse;
	IR *i;
	BitSet *live;
	IRBlock *c;

	if(press[2*b->num] >= 0){
		nlcheck(&press[2*b->num], mp);
		return;
	}
	live = bsnew(ntargs);
	nuse = nuseout[b->num];
	nl[0] = 0;
	nl[1] = 0;
	for(j = 0; j < ntargs; j++)
		if(nuse[j] >= 0){
			bsadd(live, j);
			nl[bstest(floats, j)]++;
		}
	nlcheck(nl, mp);
	for(j = 0; j < b->nto; j++){
		c = b->to[j];
		for(k = 0; k < c->nfrom; k++)
			if(c->from[k] == b)
				break;
		for(i = c->instr.next; OPTYPE(i->op) == OPPHI; i = i->next)
			if(i->φ.n > k){
				pressuse(i->φ.a[k], live, nl);
				nlcheck(nl, mp);
			}
	}
	for(i = b->instr.prev; i != &b->instr; i = i->prev){
		switch(OPTYPE(i->op)){
		case OPNORM:
			pressuse(i->a, live, nl);
			pressuse(i->b, live, nl);
			if(i->r != nil && i->r->num >= 0)
				if(bsrem(live, i->r->num))
					nl[bstest(floats, i->r->num)]--;
			break;
		case OPPHI:
		case OPBRANCH:
			break;
		default:
			print("pressure: unhandled type %d", OPTYPE(i->op));
		}
		nlcheck(nl, mp);
	}
	bsfree(live);
	press[2*b->num] = mp[0];
	press[2*b->num+1] = mp[1];
}

static BitSet **wend, **send;

static void
pick(BitSet *take, BitSet *cand, int fl, int n, int *pri)
{
	static int l[32];
	int i, j, k, m, p, t;

	if(n <= 0)
		return;	
	m = 0;
	for(i = -1; (i = bsiter(cand, i)) >= 0; ){
		if((p = pri[i]) < 0 || (targs[i]->type->t == TFLOAT || targs[i]->type->t == TDOUBLE) != fl)
			continue;
		if(m < n){
			l[m] = i;
			for(j = m; j > 0; j = k){
				k = j - 1 >> 1;
				if(pri[l[j]] > pri[l[k]]){
					t = l[k];
					l[k] = l[j];
					l[j] = t;
				}else
					break;
			}
			m++;
		}else{
			if(p >= pri[l[0]])
				continue;
			l[0] = i;
			for(j = 0; ; j = k){
				if(pri[l[j]] < pri[l[2*j+1]] && pri[l[2*j+1]] >= pri[l[2*j+2]])
					k = 2*j+1;
				else if(pri[l[j]] < pri[l[2*j+2]])
					k = 2*j+2;
				else
					break;
				if(k >= n)
					break;
				t = l[k];
				l[k] = l[j];
				l[j] = t;
			}
		}
	}
	for(i = 0; i < m; i++)
		bsadd(take, l[i]);
}

static void
initusual(IRBlock *b)
{
	BitSet *take, *cand;
	int i, j, f, ni, nf;
	
	take = bsnew(ntargs);
	wend[b->num] = take;
	if(b->nfrom == 0)
		return;
	cand = bsnew(ntargs);
	ni = nf = 0;
	for(i = 0; i < ntargs; i++){
		f = 0;
		for(j = 0; j < b->nfrom; j++)
			if(bstest(wend[b->from[j]->num], i))
				f++;
		if(f == b->nfrom){
			bsadd(take, i);
			if(targs[i]->type->t == TFLOAT || targs[i]->type->t == TDOUBLE)
				nf++;
			else
				ni++;
		}else if(f != 0)
			bsadd(cand, i);
	}
	pick(take, cand, 0, NIREG - ni, nusein[b->num]);
	pick(take, cand, 1, NFREG - nf, nusein[b->num]);
	bsfree(cand);
}

static void
initloop(IRBlock *b, Loop *lp)
{
	BitSet *alive, *cand, *take;
	int j, *in;
	int nf, ni, nia, nfa, mp[2];
	IR *i;
	
	alive = bsnew(ntargs);
	take = bsnew(ntargs);
	wend[b->num] = take;
	cand = bsnew(ntargs);
	in = nusein[b->num];
	for(j = 0; j < ntargs; j++)
		if(in[j] >= 0)
			bsadd(alive, j);
	for(i = b->instr.next; OPTYPE(i->op) == OPPHI; i = i->next)
		if(i->φ.r != nil && i->φ.r->num >= 0)
			bsadd(alive, i->φ.r->num);
	mp[0] = mp[1] = 0;
	for(j = -1; (j = bsiter(lp->bl, j)) >= 0; ){
		bsunion(cand, cand, nusedin[j]);
		pressure(blocks[j], mp);
	}
	bsinter(cand, cand, alive);
	bsminus(alive, alive, cand);
	bscntgr(cand, floats, &nf, &ni);
	if(ni < NIREG || nf < NFREG)
		bscntgr(alive, floats, &nfa, &nia);
	if(ni < NIREG){
		bsminus(take, cand, floats);
		pick(take, alive, 0, NIREG - mp[0], in);
	}else
		pick(take, cand, 0, NIREG, in);
	for(j = 0; j < ntargs; j++)
		print("%J %d\n", targs[j], in[j]);
	if(nf < NFREG){
		bsinter(cand, cand, floats);
		bsunion(take, take, cand);
		pick(take, alive, 1, NFREG - mp[1], in);
	}else
		pick(take, cand, 1, NFREG, in);
	bsfree(cand);
}

static void
calcnuse(IRBlock *b, IR *i)
{
	int k, l, c;
	IR *j;
	BitSet *s;
	int u[MAXUSE], nu;

	memcpy(nuseloc, nuseout[b->num], sizeof(int) * ntargs);
	s = nusedin[b->num];
	for(k = -1; (k = bsiter(s, k)) >= 0; ){
		for(j = i, c = 0; j != &b->instr; j = j->next, c++)
			switch(OPTYPE(j->op)){
			case OPNORM:
				nu = getuse(j, u);
				for(l = 0; l < nu; l++)
					if(u[l] == k){
						nuseloc[k] = c;
						goto next;
					}
				if(j->r != nil && j->r->num == k){
					nuseloc[k] = -1;
					goto next;
				}
				break;
			case OPPHI:
			case OPBRANCH:
				break;
			default:
				print("calcnuse: unhandled type %x\n", OPTYPE(j->op));
			}
		next: ;
	}
}

static void
spill(IRBlock *b, int t, IR *i, int rel)
{
	IR *j;
	int l;

	if(spillloc[t] == 0)
		spillloc[t] = spfree -= targs[t]->type->size + 3 & -4;
	l = spillloc[t];
	j = irop(i, rel ? OPLD : OPST);
	if(rel)
		j->r = targs[t];
	else
		j->b = targs[t];
	j->a = targ(TARGFP, l);
	j->prev = i->prev;
	j->next = i;
	j->prev->next = j;
	j->next->prev = j;
}

static void
edgespill(IRBlock *b, IRBlock *c, int t, int rel)
{
	IR *i;

	if(c->nto == 1){
		spill(c, t, c->instr.prev, rel);
		return;
	}
	if(b->nfrom != 1)
		error(b->instr.next, "irreducible control flow in edgespill() -- shouldn't happen");
	for(i = b->instr.next; OPTYPE(i->op) == OPPHI; i = i->next)
		;
	spill(b, t, i, rel);
}

static void
limit(IRBlock *b, BitSet *w, BitSet *s, IR *i, int ki, int kf)
{
	int ni, nf, j;
	BitSet *reg;

	bscntgr(w, floats, &ni, &nf);
	if(ni <= ki && nf <= kf)
		return;
	reg = bsnew(ntargs);
	calcnuse(b, i);
	pick(reg, w, 0, ki, nuseloc);
	pick(reg, w, 1, kf, nuseloc);
	print("%+S\n", reg);
	bsminus(w, w, reg);
	for(j = -1; (j = bsiter(w, j)) >= 0; ){
		if(!bstest(s, j) && nuseloc[j] >= 0)
			spill(b, j, i, 0);
		bsrem(s, j);
	}
	bscopy(w, reg);
	bsfree(reg);
}

static void
regblock(IRBlock *b)
{
	Loop *lp;
	BitSet *w, *s;
	IR *i;
	IRBlock *c;
	int u[MAXUSE], nu, j, k;
	
	for(lp = curfunc->loops; lp != nil; lp = lp->next)
		if(lp->head == b)
			break;
	if(lp != nil)
		initloop(b, lp);
	else
		initusual(b);
	s = send[b->num] = bsnew(ntargs);
	w = wend[b->num];
	for(j = 0; j < b->nfrom; j++){
		if((c = b->from[j])->num >= b->num)
			continue;
		bsunion(s, s, send[c->num]);
	}
	bsinter(s, s, w);
	print("%p %+S %+S\n", b, w, s);
	for(j = 0; j < b->nfrom; j++){
		if((c = b->from[j])->num >= b->num)
			continue;
		for(k = -1; (k = bsiter(w, k)) >= 0; )
			if(!bstest(wend[c->num], k))
				edgespill(b, c, k, 1);
		for(k = -1; (k = bsiter(s, k)) >= 0; )
			if(bstest(wend[c->num], k) && !bstest(send[c->num], k))
				edgespill(b, c, k, 0);
	}
	for(i = b->instr.next; i != &b->instr; i = i->next)
		switch(OPTYPE(i->op)){
		case OPNORM:
			print("%I %+S\n", i, w);
			nu = getuse(i, u);
			for(j = 0; j < nu; j++)
				if(!bstest(w, u[j])){
					bsadd(w, u[j]);
					bsadd(s, u[j]);
				}else
					u[j] = -1;
			print("%I %+S\n", i, w);
			limit(b, w, s, i, NIREG, NFREG);
			if(i->r != nil && i->r->num >= 0){
				if(bstest(floats, i->r->num))
					limit(b, w, s, i->next, NIREG, NFREG - 1);
				else
					limit(b, w, s, i->next, NIREG - 1, NFREG);
				bsadd(w, i->r->num);
			}
			for(j = 0; j < nu; j++)
				if(u[j] >= 0)
					spill(b, u[j], i, 1);
			break;
		case OPPHI:
			bsadd(w, i->r->num);
			break;
		case OPBRANCH:
			break;
		default:
			print("regblock: unhandled type %x\n", OPTYPE(i->op));
		}
}

static void
lusetarg(BitSet *live, Targ *t, IR *i)
{
	if(t == nil)
		return;
	if(t->num >= 0){
		if(!bsadd(live, t->num))
			nodes[t->num].luse = i;
	}else if(t->t == TARGSH){
		lusetarg(live, t->a, i);
		lusetarg(live, t->b, i);
	}	
}

void
color(IRBlock *b)
{
	IR *i;
	BitSet *live;
	u64int ass;
	u32int v;
	int k;
	IRBlock *c;
	
	live = bsdup(liveout[b->num]);
	for(i = b->instr.prev; i != &b->instr; i = i->prev){
		switch(OPTYPE(i->op)){
		case OPNORM:
			if(i->r != nil && i->r->num >= 0)
				bsrem(live, i->r->num);
			lusetarg(live, i->a, i);
			lusetarg(live, i->b, i);
			break;
		case OPBRANCH:
		case OPPHI:
			break;
		default:
			print("color: unhandled op %#x\n", OPTYPE(i->op));
		}
	}
	bsfree(live);
	live = livein[b->num];
	ass = 0;
	for(k = -1; (k = bsiter(live, k)) >= 0; )
		if(targs[k]->Rn >= 0)
			ass |= 1LL<<targs[k]->Rn;
	for(i = b->instr.next; i != &b->instr; i = i->next)
		switch(OPTYPE(i->op)){
		case OPNORM:
			if(i->a != nil && i->a->num >= 0 && nodes[i->a->num].luse == i && i->a->Rn >= 0)
				ass &= ~((i->a->type->t == TDOUBLE ? 3LL : 1LL)<<i->a->Rn);
			if(i->b != nil && i->b->num >= 0 && nodes[i->b->num].luse == i && i->b->Rn >= 0)
				ass &= ~((i->b->type->t == TDOUBLE ? 3LL : 1LL)<<i->b->Rn);
		case OPPHI:
			if(i->r != nil && i->r->num >= 0)
				switch(i->r->type->t){
				case TFLOAT:
					i->r->Rn = 32 + scanset(~ass >> 32);
					ass |= 1LL<<i->r->Rn;
					break;
				case TDOUBLE:
					v = ~ass >> 32;
					i->r->Rn = 32 + scanset(v & v >> 1 & 0x55555555);
					ass |= 3LL<<i->r->Rn;
					break;
				default:
					i->r->Rn = scanset(~ass & (1<<NIREG)-1);
					if(i->r->Rn == 32)
						error(i, "out of registers");
					ass |= 1LL<<i->r->Rn;
					break;
				}
		}
	for(c = b->domch; c != nil; c = c->chnext)
		color(c);
}

void
regalloc(void)
{
	int i;
	IRBlock *b, *l;

	l = &curfunc->bllist;
	nodes = emalloc(sizeof(Node) * ntargs);
	for(i = 0; i < ntargs; i++){
		nodes[i].t = targs[i];
		nodes[i].adj = bsnew(ntargs);
	}
	floats = bsnew(ntargs);
	for(i = 0; i < ntargs; i++)
		if(targs[i]->type->t == TFLOAT || targs[i]->type->t == TDOUBLE)
			bsadd(floats, i);
	findadj();
	splive();
	wend = emalloc(sizeof(BitSet *) * nblocks);
	send = emalloc(sizeof(BitSet *) * nblocks);
	nuseloc = emalloc(sizeof(int) * ntargs);
	spillloc = emalloc(sizeof(int) * ntargs);
	spfree = 0;
	for(b = l->next; b != l; b = b->next)
		regblock(b);

	for(b = l->next; b != l; b = b->next){
		bsfree(wend[b->num]);
		bsfree(send[b->num]);
	}
	free(wend);
	free(send);
	free(nuseloc);
//	color(curfunc->bllist.next);
}
