#include <u.h>
#include <libc.h>
#include "dat.h"
#include "fns.h"

SymTab global;
SymTab *scope = &global;
int curlevel;

Type types[NTYPE] = {
	{TXXX, 0, 0, 0},
	{TUCHAR, 0, 1, 0},
	{TCHAR, 1, 1, 0},
	{TUSHORT, 0, 2, 2},
	{TSHORT, 1, 2, 2},
	{TUINT, 0, 4, 4},
	{TINT, 1, 4, 4},
	{TUVLONG, 0, 8, 4},
	{TVLONG, 1, 8, 4},
	{TFLOAT, 1, 4, 4},
	{TDOUBLE, 1, 8, 4},
	{TVOID, 0},
};
Type *typelist;
static char *astname[] = {
	"ASTCLONG",
	"ASTBIN",
	"ASTASS",
	"ASTTERN",
	"ASTDEREF",
	"ASTADDROF",
	"ASTUN",
	"ASTINC",
	"ASTIF",
	"ASTSYM",
	"ASTARRAY",
	"ASTFUNCTYPE",
	"ASTCAST",
	"ASTCOMMA",
	"ASTBLOCK",
	"ASTWHILE",
	"ASTDOWHILE",
	"ASTFOR",
	"ASTBREAK",
	"ASTCONTINUE",
	"ASTRETURN",
	"ASTLABEL",
	"ASTGOTO",
	"ASTMEMB",
	"ASTCALL",
};
static char *copname[] = {
	[OPMOV] "MOV",
	[OPUPLUS] "unary +",
	[OPNEG] "unary -",
	[OPISZ] "!",
	[OPADD] "+",
	[OPSUB] "-",
	[OPMUL] "*",
	[OPDIV] "/",
	[OPMOD] "%",
	[OPEQ] "==",
	[OPNE] "!=",
	[OPLT] "<",
	[OPLE] "<=",
	[OPGT] ">",
	[OPGE] ">=",
	[OPB] "unsigned <",
	[OPBE] "unsigned <=",
	[OPA] "unsigned >",
	[OPAE] "unsigned >=",
	[OPAND] "&",
	[OPOR] "|",
	[OPXOR] "^",
	[OPLSH] "<<",
	[OPRSH] "unsigned >>",
	[OPASR] "signed >>",
	[OPCOM] "~",
	[OPLOR] "||",
	[OPLAND] "&&",
};
static char *iropname[] = {
	[OPMOV] "MOV",
	[OPNEG] "NEG",
	[OPISZ] "ISZ",
	[OPTST] "TST",
	[OPLD] "LD",
	[OPST] "ST",
	[OPLDB] "LDB",
	[OPLDSB] "LDSB",
	[OPSTB] "STB",
	[OPLDH] "LDH",
	[OPLDSH] "LDSH",
	[OPSTH] "STH",
	[OPLDD] "LDD",
	[OPSTD] "STD",
	[OPADD] "ADD",
	[OPSUB] "SUB",
	[OPRSB] "RSB",
	[OPMUL] "MUL",
	[OPDIV] "DIV",
	[OPMOD] "MOD",
	[OPCMP] "CMP",
	[OPEQ] "BEQ",
	[OPNE] "BNE",
	[OPLT] "BLT",
	[OPLE] "BLE",
	[OPGT] "BGT",
	[OPGE] "BGE",
	[OPB] "BB",
	[OPBE] "BBE",
	[OPA] "BA",
	[OPAE] "BAE",
	[OPAND] "AND",
	[OPOR] "OR",
	[OPXOR] "XOR",
	[OPLSH] "LSH",
	[OPRSH] "RSH",
	[OPASR] "ASR",
	[OPCOM] "COM",
	[OPSXTB] "SXTB",
	[OPSXTH] "SXTH",
	[OPSXTW] "SXTW",
	[OPZXTB] "ZXTB",
	[OPZXTH] "ZXTH",
	[OPZXTW] "ZXTW",
	[OPPHI] "φ",
	[OPCALL] "CALL",
	[OPFADD] "FADD",
	[OPFSUB] "FSUB",
	[OPFMUL] "FMUL",
	[OPFDIV] "FDIV",
	[OPFCMP] "FCMP",
};
static char *typename[] = {
	"XXX",
	"uchar",
	"char",
	"ushort",
	"short",
	"uint",
	"int",
	"uvlong",
	"vlong",
	"float",
	"double",
	"void",
};

static uint
hash(char *s)
{
	uint c;

	c = 0;
	while(*s != 0)
		c += *s++;
	return c;
}

static Symbol *
makesym(char *n)
{
	SymTab *st;
	Symbol *s;
	int h;

	if(curlevel != scope->level){
		st = emalloc(sizeof(*st));
		st->up = scope;
		scope = st;
	}
	s = emalloc(sizeof(*s));
	s->num = -1;
	s->name = strdup(n);
	h = hash(n) % SYMHASH;
	s->next = scope->sym[h];
	s->Line = curline;
	s->level = curlevel;
	scope->sym[h] = s;
	return s;
}

Symbol *
getsym(char *n)
{
	SymTab *st;
	Symbol *s;
	int h;
	
	h = hash(n) % SYMHASH;
	for(st = scope; st != nil; st = st->up)
		for(s = st->sym[h]; s != nil; s = s->next)
			if(strcmp(s->name, n) == 0)
				return s;
	return makesym(n);
}

void
checksym(Symbol *s)
{
	if(s->t == nil){
		error(nil, "'%s' undeclared", s->name);
		s->t = &types[TINT];
	}
}

static void
funcparams(void)
{
	Member *m, *me;
	Symbol *s, **sp;
	
	me = curfunc->type->memb + curfunc->type->nmemb;
	curfunc->arg = sp = emalloc(sizeof(Member *) * curfunc->type->nmemb);
	for(m = curfunc->type->memb; m < me; m++, sp++)
		if(m->name != nil){
			s = makesym(m->name);
			s->t = m->type;
			s->off = m->off;
			*sp = s;
		}
}

void
levelch(int n)
{
	SymTab *st, *stn;
	
	curlevel += n;
	if(curlevel == 1 && n > 0)
		funcparams();
	assert(curlevel >= 0);
	if(n < 0){
		st = scope;
		while(st->level > curlevel){
			stn = st->up;
			free(st);
			st = stn;
			assert(st != nil);
		}
		scope = st;
	}
}

int
opfmt(Fmt *f)
{
	int t;
	
	t = va_arg(f->args, int);
	if(t >= nelem(copname) || copname[t] == nil)
		return fmtprint(f, "??? (%d)", t);
	return fmtstrcpy(f, copname[t]);
}

int
astfmt(Fmt *f)
{
	int t;
	
	t = va_arg(f->args, int);
	if(t >= nelem(astname))
		return fmtprint(f, "??? (%d)", t);
	return fmtstrcpy(f, astname[t]);
}

int
typefmtrec(Fmt *f, Type *t)
{
	int rc, i;

	if(t == nil)
		return fmtstrcpy(f, "??? (nil)");
	switch(t->t){
	case TIND:
		rc = typefmtrec(f, t->link);
		rc += fmtstrcpy(f, "*");
		return rc;
	case TFUNC:
		rc = typefmtrec(f, t->link);
		rc += fmtstrcpy(f, "()(");
		if(t->nmemb != 0)
			for(i = 0;;){
				rc += typefmtrec(f, t->memb[i].type);
				if(++i != t->nmemb)
					rc += fmtstrcpy(f, ", ");
				else
					break;
			}
		rc += fmtstrcpy(f, ")");
		return rc;
	case TSTRUCT:
		if(t->tag != nil)
			rc = fmtprint(f, "struct %s", t->tag);
		else
			rc = fmtprint(f, "struct %p", t);
		return rc;
	case TUNION:
		if(t->tag != nil)
			rc = fmtprint(f, "union %s", t->tag);
		else
			rc = fmtprint(f, "union %p", t);
		return rc;
	}
	if(t->t >= nelem(typename))
		return fmtprint(f, "??? (%d)", t->t);
	return fmtstrcpy(f, typename[t->t]);
}

int
typefmt(Fmt *f)
{
	return typefmtrec(f, va_arg(f->args, Type *));
}

int
irtargfmt(Fmt *f)
{
	Targ *t;
	int rc;
	
	t = va_arg(f->args, Targ *);
	if(t == nil)
		return fmtstrcpy(f, "nil");
	switch(t->t){
	case TARGSYM:
		rc = fmtstrcpy(f, t->sym->name);
		break;
	case TARGSSA:
		rc = fmtprint(f, "%s:%d", t->sym->name, t->n);
		break;
	case TARGTEMP:
		rc = fmtprint(f, "t%d", t->tn);
		if(t->type != nil)
			switch(t->type->t){
			case TFLOAT: rc += fmtrune(f, 'f'); break;
			case TDOUBLE: rc += fmtrune(f, 'd'); break;
			}
		break;
	case TARGCONST:
		rc = fmtprint(f, "$%#ux", t->lval);
		break;
	case TARGRETV:
		rc = fmtprint(f, t->type == &types[TFLOAT] || t->type == &types[TDOUBLE] ? "F0" : "R0");
		break;
	case TARGFP:
		rc = fmtprint(f, "FP(%d)", t->off);
		break;
	case TARGARG:
		rc =fmtprint(f, "ARG(%d)", t->off);
		break;
	case TARGIND:
		rc = fmtprint(f, "(%J)", t->link);
		break;
	case TARGADDR:
		rc = fmtprint(f, "&%s%+d", t->sym->name, t->n);
		break;
	case TARGSH:
		rc = fmtprint(f, "%s(%J, %J)", iropname[t->op], t->a, t->b);
		break;
	default:
		return fmtprint(f, "???(%d)", t->t);
	}
	if(t->Rn >= 32)
		rc += fmtprint(f, "<F%d>", t->Rn - 32);
	else if(t->Rn >= 0)
		rc += fmtprint(f, "<R%d>", t->Rn);
	return rc;
}

int
irfmt(Fmt *f)
{
	IR *i;
	int rc, j, fi;
	
	i = va_arg(f->args, IR *);
	if(i->op >= nelem(iropname) || iropname[i->op] == nil)
		rc = fmtprint(f, "???(%#x)", i->op);
	else
		rc = fmtstrcpy(f, iropname[i->op]);
	switch(OPTYPE(i->op)){
	case OPNORM:
		if(i->a != nil)
			rc += fmtprint(f, " %J", i->a);
		if(i->b != nil && i->a != nil)
			rc += fmtrune(f, ',');
		if(i->b != nil)
			rc += fmtprint(f, " %J", i->b);
		if((i->a != nil || i->b != nil) && i->r != nil)
			rc += fmtrune(f, ',');
		if(i->r != nil)
			rc += fmtprint(f, " %J", i->r);
		return rc;
	case OPBRANCH:
		rc += fmtprint(f, " %p", i->targ);
		return rc;
	case OPPHI:
		fi = 0;
		if(i->r != nil){
			rc += fmtprint(f, " %J", i->r);
			fi++;
		}
		for(j = 0; j < i->φ.n; j++){
			if(fi++ != 0)
				rc += fmtrune(f, ',');
			rc += fmtprint(f, " %J", i->φ.a[j]);
		}
		return rc;
	default:
		rc += fmtprint(f, " ???(%#x)", OPTYPE(i->op));
		return rc;
	}
}

ASTNode*
node(int type, ...)
{
	va_list va;
	ASTNode *n, *l, *m;
	int i;

	n = emalloc(sizeof(*n));
	n->t = type;
	n->Line = curline;
	va_start(va, type);
	switch(type){
	case ASTCLONG:
		n->lval = va_arg(va, vlong);
		break;
	case ASTSYM:
	case ASTGOTO:
	case ASTLABEL:
		n->sym = va_arg(va, Symbol *);
		break;
	case ASTBIN:
	case ASTASS:
		n->op = va_arg(va, int);
	case ASTARRAY:
	case ASTCOMMA:
		n->n1 = va_arg(va, ASTNode *);
		n->n2 = va_arg(va, ASTNode *);
		break;
	case ASTMEMB:
		n->mb.n = va_arg(va, ASTNode *);
		n->mb.name = va_arg(va, char *);
		break;
	case ASTUN:
	case ASTINC:
		n->op = va_arg(va, int);
		n->n1 = va_arg(va, ASTNode *);
		break;
	case ASTFUNCTYPE:
		n->name = va_arg(va, ASTNode *);
		n->memb = va_arg(va, Member *);
		n->nmemb = va_arg(va, int);
		break;
	case ASTDEREF:
	case ASTRETURN:
	case ASTADDROF:
		n->n = va_arg(va, ASTNode *);
		break;
	case ASTBLOCK:
		n->last = &n->first;
		break;
	case ASTTERN:
	case ASTIF:
		n->cond = va_arg(va, ASTNode *);
		n->block = va_arg(va, ASTNode *);
		n->elsebl = va_arg(va, ASTNode *);
		break;
	case ASTWHILE:
	case ASTDOWHILE:
		n->cond = va_arg(va, ASTNode *);
		n->block = va_arg(va, ASTNode *);
		n->elsebl = nil;
		break;
	case ASTFOR:
		n->forl.once = va_arg(va, ASTNode*);
		n->forl.cond = va_arg(va, ASTNode*);
		n->forl.iter = va_arg(va, ASTNode*);
		n->forl.block = va_arg(va, ASTNode*);
		break;
	case ASTCAST:
		n->cast.n = va_arg(va, ASTNode *);
		n->cast.t = va_arg(va, Type *);
		break;
	case ASTBREAK:
	case ASTCONTINUE:
		break;
	case ASTCALL:
		n->func.n = va_arg(va, ASTNode *);
		l = va_arg(va, ASTNode *);
		for(i = 0, m = l; m != nil; i++)
			m = m->next;
		n->func.argn = i;
		n->func.args = emalloc(sizeof(ASTNode *) * i);
		for(i = 0, m = l; m != nil; i++){
			l = m->next;
			m->next = nil;
			n->func.args[i] = m;
			m = l;
		}
		break;
	default:
		sysfatal("node: unknown type %A", type);
	}
	va_end(va);
	return n;
}

Type *
type(int t, ...)
{
	Type *s, **p;
	Type *l;
	Member *memb;
	int nmemb, i, j;
	va_list va;
	char *st;

	if(t <= TVOID)
		return &types[t];
	va_start(va, t);
	switch(t){
	case TIND:
		l = va_arg(va, Type *);
		va_end(va);
		for(p = &typelist; (s = *p) != nil; p = &s->next)
			if(s->t == TIND && s->link == l)
				return s;
		s = emalloc(sizeof(Type));
		s->t = TIND;
		s->size = PTRSZ;
		s->link = l;
		*p = s;
		return s;
	case TFUNC:
		l = va_arg(va, Type *);
		memb = va_arg(va, Member *);
		nmemb = va_arg(va, int);
		va_end(va);
		if(nmemb == 1 && memb->type->t == TVOID)
			nmemb = 0;
		for(p = &typelist; (s = *p) != nil; p = &s->next){
			if(s->t != TFUNC || s->link != l || s->nmemb != nmemb)
				continue;
			for(i = 0; i < nmemb; i++)
				if(s->memb[i].type != memb[i].type)
					goto next;
			for(i = 0; i < nmemb; i++)
				s->memb[i].name = memb[i].name;
			return s;
		next: ;
		}
		s = emalloc(sizeof(Type));
		s->t = TFUNC;
		s->link = l;
		s->memb = memb;
		s->nmemb = nmemb;
		*p = s;
		return s;
	case TARRAY:
		l = va_arg(va, Type *);
		nmemb = va_arg(va, int);
		va_end(va);
		for(p = &typelist; (s = *p) != nil; p = &s->next){
			if(s->t != TARRAY || s->link != l || s->nmemb != nmemb)
				continue;
			return s;
		}
		s = emalloc(sizeof(Type));
		s->t = TARRAY;
		s->link = l;
		s->size = l->size * nmemb;
		s->nmemb = nmemb;
		*p = s;
		return s;
	case TSTRUCT:
	case TUNION:
		st = va_arg(va, char *);
		memb = va_arg(va, Member *);
		nmemb = va_arg(va, int);
		va_end(va);
		assert(st != nil);
		for(p = &typelist; (s = *p) != nil; p = &s->next){
			if(s->t != TSTRUCT && s->t != TUNION || strcmp(s->tag, st) != 0)
				continue;
			if(nmemb >= 0){
				if(s->nmemb < 0)
					break;
				error(nil, "tag '%s' redefined", st);
			}else
				return s;
		}
		if(s == nil){
			s = emalloc(sizeof(Type));
			s->t = t;
			s->tag = strdup(st);
			*p = s;
		}
		j = 0;
		for(i = 0; i < nmemb; i++){
			if(memb[i].type->align > s->align)
				s->align = memb[i].type->align;
			if(memb[i].type->align != 0)
				j = -(-j & -memb[i].type->align);
			memb[i].off = j;
			if(t == TSTRUCT)
				j += memb[i].type->size;
			else if(memb[i].type->size > s->size)
				s->size = memb[i].type->size;
		}
		if(t == TSTRUCT)
			s->size = j;
		s->memb = memb;
		s->nmemb = nmemb;
		return s;
	}
	va_end(va);
	sysfatal("type: unknown type %d", t);
	return nil;
}

int
typeor(int a, int b)
{
	int t, s;

	if((a & b) != 0 && (b != BLONG || (a & BVLONG) != 0)){
		error(nil, "duplicate specifier");
		return a;
	}
	t = a + b;
	s = t & (BEXTERN|BSTATIC|BTYPEDEF);
	if((s & s-1) != 0){
		error(nil, "more than one storage class");
		return a;
	}
	return t;
}

Type *
bittype(int t)
{
	int uns;
	
	uns = (t & BUNSIGNED) != 0;
	switch(t & BMASK){
	case BCHAR:
		return &types[TCHAR - uns];
	case BSHORT:
	case BSHORT|BINT:
		return &types[TSHORT - uns];
	case 0:
	case BINT:
	case BLONG:
	case BLONG|BINT:
		return &types[TINT - uns];
	case BVLONG:
	case BVLONG|BINT:
		return &types[TVLONG - uns];
	case BFLOAT:
	case BDOUBLE:
	case BDOUBLE|BLONG:
		if((t & (BSIGNED|BUNSIGNED)) != 0)
			error(nil, "signed/unsigned on floating point type");
		return &types[TFLOAT + ((t & BMASK) != BFLOAT)];
	case BVOID:
		return &types[TVOID];
	default:
		error(nil, "conflicting type specifiers");
		return &types[TINT];
	}
}

static Type *
asttype(ASTNode **np, Type *t, int par)
{
	ASTNode *n;

	n = *np;
	while(n->t != ASTSYM)
		switch(n->t){
		case ASTDEREF:
			t = type(TIND, t);
			n = n->n;
			break;
		case ASTFUNCTYPE:
			t = type(TFUNC, t, n->memb, n->nmemb);
			n = n->name;
			break;
		case ASTARRAY:
			if(par){
				t = type(TIND, t);
				n = n->n1;
				break;
			}
			if(n->n2 != nil){
				n->n2 = astfold(n->n2);
				if(n->n2->t != ASTCLONG){
					error(n, "array size not an integer constant");
					n->n2 = nil;
				}
			}
			t = type(TARRAY, t, n->n2 != nil ? n->n2->lval : 0);
			n = n->n1;
			break;
		default:
			sysfatal("asttype: can't handle %A", n->t);
		}
	*np = n;
	return t;
}

Type *
dodecl(ASTNode *n, Type *t, int cl)
{
	Symbol *s;

	t = asttype(&n, t, 0);
	s = n->sym;
	if(s->t != nil)
		if(s->level != curlevel)
			s = makesym(s->name);
		else{
			error(n, "'%s' redeclared", s->name);
			return t;
		}
	s->t = t;
	s->class = cl;
	if(t->size == 0 && t->t != TFUNC && (cl & (BEXTERN|BTYPEDEF)) == 0)
		error(n, "undefined type");
	if(s->level == 0 || (s->class & BSTATIC) != 0 || t->t == TARRAY)
		s->class |= BMEM;
	return t;
}

Member *
doparam(ASTNode *n, Type *t, Member *m, int nm)
{
	Member *r;

	if(n != nil)
		t = asttype(&n, t, 1);
	m = r = realloc(m, sizeof(Member) * (nm + 1));
	m += nm;
	memset(m, 0, sizeof(Member));
	if(n != nil)
		m->name = strdup(n->sym->name);
	m->type = t;
	return r;
}

Member *
dostruct(ASTNode *n, Type *t, Member *m, int nm, int *nmp)
{
	Member *r;
	ASTNode *p;
	int c, i;

	c = 0;
	for(p = n; p != nil; p = p->next)
		c++;
	m = r = realloc(m, sizeof(Member) * (*nmp = nm + c));
	m += nm;
	memset(m, 0, sizeof(Member) * c);
	for(p = n, i = 0; p != nil; p = p->next, i++){
		m[i].type = asttype(&p, t, 0);
		if(m[i].type->size == 0)
			error(n, "'%s' incomplete size", p->sym->name);
		m[i].name = strdup(p->sym->name);
	}
	return r;
}

void
numcast(ASTNode **n, int t)
{
	int t0;

	t0 = (*n)->type->t;
	if(t0 == t)
		return;
	if(t0 > TBASIC){
		error(*n, "can't cast %T to %s", (*n)->type, typename[t]);
		return;
	}
	if(t0 >= TFLOAT || t >= TFLOAT){
		error(*n, "floating point cast not implemented");
		return;
	}
	if(types[t0].size <= types[t].size){
		(*n)->type = &types[t];
		return;
	}
	if(types[t].sign)
		*n = node(ASTUN, OPSXTB + ((t - 1) >> 1), *n);
	else
		*n = node(ASTUN, OPZXTB + ((t - 1) >> 1), *n);
	(*n)->type = &types[t];
}

void
implcast(ASTNode **n, Type *t)
{
	Type *t0;
	
	t0 = (*n)->type;
	if(t == t0)
		return;
	if(t->t <= TBASIC){
		numcast(n, t->t);
		return;
	}
	switch(t->t){
	case TIND:
		if(t0->t == TIND && (t->link->t == TVOID || t0->link->t == TVOID))
			return;
		if((*n)->t == ASTCLONG && (*n)->lval == 0)
			return;
	default:
		error(*n, "can't cast %T to %T", t0, t);
	}
}

ASTNode *
explcast(ASTNode *n, Type *t)
{
	int t0;

	if(n->type == t)
		return n;
	if(t->t <= TBASIC){
		if(n->type->t == TIND)
			n->type = &types[TINT];
		numcast(&n, t->t);
		return n;
	}
	t0 = n->type->t;
	switch(t->t){
	case TIND:
		if(t0 < TFLOAT || t0 == TINT)
			break;
	default:
		error(n, "can't cast %T to %T", n->type, t);
	}
	n->type = t;
	return n;
}

static void
binaryconv(ASTNode *n)
{
	int t1, t2, *tp;
	static int tr[] = {TDOUBLE, TFLOAT, TUVLONG, TVLONG, TUINT, -1};
	
	t1 = n->n1->type->t;
	t2 = n->n2->type->t;
	if(t1 == TIND && t2 < TFLOAT && (n->op == OPADD || n->op == OPSUB)){
		numcast(&n->n2, TUINT);
		if(n->n1->type->link->size == 0)
			error(n, "pointer arithmetic on undefined type");
		n->n2 = node(ASTBIN, OPMUL, n->n2, node(ASTCLONG, n->n1->type->link->size));
		n->type = n->n2->type = n->n1->type;
		return;
	}
	if(t2 == TIND && t1 < TFLOAT && (n->op == OPADD || n->op == OPSUB)){
		numcast(&n->n1, TUINT);
		if(n->n2->type->link->size == 0)
			error(n, "pointer arithmetic on undefined type");
		n->n1 = node(ASTBIN, OPMUL, n->n1, node(ASTCLONG, n->n2->type->link->size));
		n->type = n->n1->type = n->n2->type;
		return;
	}
	if(t1 == TIND && OPTYPE(n->op) == OPBRANCH){
		implcast(&n->n2, n->n1->type);
		return;
	}
	if(t2 == TIND && OPTYPE(n->op) == OPBRANCH){
		implcast(&n->n1, n->n2->type);
		return;
	}
	if(t1 == TXXX || t1 > TBASIC){
		error(n, "invalid type %T in operation %O", n->n1->type, n->op);
		n->type = &types[TINT];
		return;
	}
	if(t2 == TXXX || t2 > TBASIC){
		error(n, "invalid type %T in operation %O", n->n2->type, n->op);
		n->type = &types[TINT];
		return;
	}
	for(tp = tr; *tp != -1; tp++)
		if(t1 == *tp || t2 == *tp){
			numcast(&n->n1, *tp);
			numcast(&n->n2, *tp);
			n->type = &types[*tp];
			if((n->type == &types[TFLOAT] || n->type == &types[TDOUBLE]) && (n->op < OPADD || n->op > OPDIV) && OPTYPE(n->op) == OPBRANCH)
				error(n, "not a valid floating point operation");
			return;
		}
	numcast(&n->n1, TINT);
	numcast(&n->n2, TINT);
	n->type = &types[TINT];
}

static void
unaryconv(ASTNode *n)
{
	int t1;
	
	t1 = n->n1->type->t;
	if(t1 == TXXX || t1 > TBASIC || n->op != OPNEG && n->op != OPMOV && t1 >= TFLOAT){
		error(n, "invalid type %T in operation %O", n->n1->type, n->op);
		n->type = &types[TINT];
		return;
	}
	if(t1 == TCHAR || t1 == TSHORT)
		numcast(&n->n1, TINT);
	if(t1 == TUCHAR || t1 == TUSHORT)
		numcast(&n->n2, TUINT);
	n->type = n->n1->type;
}

void
scalar(ASTNode *n)
{
	int t;
	
	t = n->type->t;
	if(t <= TBASIC || t == TIND)
		return;
	error(n, "non-scalar type %T", n->type);
}

void
lvalcheck(ASTNode *n)
{
	switch(n->t){
	case ASTSYM:
	case ASTDEREF:
		return;
	default:
		error(n, "not an l-value (%A)", n->t);
	}
}

Member *
membfind(Line *l, Type *t, char *n)
{
	Member *m;
	
	for(m = t->memb; m < t->memb + t->nmemb; m++)
		if(strcmp(m->name, n) == 0)
			return m;
	error(l, "'%s' not a member", n);
	return nil;
}

void
funccheck(ASTNode *n)
{
	ASTNode **p;
	Type *t;
	int i;

	if(n->func.n->t == ASTSYM){
		n->func.n->type = n->func.n->sym->t;
		if(n->func.n->type == nil)
			n->func.n->type = type(TFUNC, &types[TINT], nil, -1);
	}else
		n->func.n = typecheck(n->func.n);
	if(n->func.n->type->t == TIND){
		n->func.n = node(ASTDEREF, n->func.n);
		typecheck(n->func.n);
	}
	if(n->func.n->type->t != TFUNC){
		error(n, "type %T is not a function", n->func.n->type);
		n->type = &types[TINT];
		return;
	}
	t = n->func.n->type;
	n->type = t->link;
	for(p = n->func.args, i = 0; p < n->func.args + n->func.argn; p++, i++){
		*p = typecheck(*p);
		if(i == t->nmemb)
			error(n, "too many arguments");
		else if(i < t->nmemb)
			implcast(p, t->memb[i].type);
	}
	if(i < t->nmemb)
		error(n, "too few arguments");
	if(t->nmemb == -1)
		warn(n, "function args not checked");
}

ASTNode *
typecheck(ASTNode *n)
{
	ASTNode *n1;

	if(n == nil)
		return nil;
	switch(n->t){
	case ASTASS:
		n->n1 = typecheck(n->n1);
		n->n2 = typecheck(n->n2);
		implcast(&n->n2, n->n1->type);
		lvalcheck(n->n1);
		n->type = n->n1->type;
		return n;
	case ASTINC:
		n->n1 = typecheck(n->n1);
		lvalcheck(n->n1);
		n->type = n->n1->type;
		return n;
	case ASTBIN:
		n->n1 = typecheck(n->n1);
		n->n2 = typecheck(n->n2);
		binaryconv(n);
		switch(n->op){
		case OPRSH:
			if(n->type->sign)
				n->op = OPASR;
			break;
		case OPLT:
		case OPLE:
		case OPGT:
		case OPGE:
			if(!(n->n1->type->sign && n->n2->type->sign))
				n->op += 4;
		case OPEQ:
		case OPNE:
		case OPLAND:
		case OPLOR:
			n->type = &types[TINT];
			break;
		}
		return n;
	case ASTUN:
		n->n1 = typecheck(n->n1);
		if(n->op == OPUPLUS)
			n->op = OPMOV;
		unaryconv(n);
		return n;
	case ASTSYM:
		checksym(n->sym);
		if(n->sym->t->t == TARRAY){
			n1 = node(ASTADDROF, n);
			n1->type = type(TIND, n->sym->t->link);
			return n1;
		}
		n->type = n->sym->t;
		return n;
	case ASTMEMB:
		n->mb.n = typecheck(n->mb.n);
		if(n->mb.n->type->t != TSTRUCT && n->mb.n->type->t != TUNION)
			error(n, "%T is not a struct/union", n->mb.n->type);
		else
			n->mb.m = membfind(n, n->mb.n->type, n->mb.name);
		if(n->mb.m != nil)
			n->type = n->mb.m->type;
		else
			n->type = &types[TINT];
		return n;
	case ASTCLONG:
		n->type = &types[TINT];
		return n;
	case ASTDEREF:
		n->n = typecheck(n->n);
		if(n->n->type->t != TIND){
			error(n, "dereferencing non-pointer");
			n->type = &types[TINT];
		}else
			n->type = n->n->type->link;
		return n;
	case ASTADDROF:
		n->n = typecheck(n->n);
		lvalcheck(n->n);
		if(n->n->t == ASTDEREF)
			return n->n->n;
		if(n->n->t == ASTSYM)
			n->n->sym->class |= BMEM;
		n->type = type(TIND, n->n->type);
		return n;
	case ASTCOMMA:
		n->n1 = typecheck(n->n1);
		n->n2 = typecheck(n->n2);
		n->type = n->n2->type;
		return n;
	case ASTIF:
	case ASTWHILE:
	case ASTDOWHILE:
		n->cond = typecheck(n->cond);
		scalar(n->cond);
		n->block = typecheck(n->block);
		if(n->elsebl != nil)
			n->elsebl = typecheck(n->elsebl);
		return n;
	case ASTTERN:
		n->cond = typecheck(n->cond);
		scalar(n->cond);
		n->block = typecheck(n->block);
		n->elsebl = typecheck(n->elsebl);
		if(n->block->type == n->elsebl->type)
			n->type = n->block->type;
		else
			error(n, "ternary with different types not implemented");
		return n;
	case ASTFOR:
		n->forl.once = typecheck(n->forl.once);
		if(n->forl.cond != nil){
			n->forl.cond = typecheck(n->forl.cond);
			scalar(n->forl.cond);
		}
		n->forl.iter = typecheck(n->forl.iter);
		n->forl.block = typecheck(n->forl.block);
		return n;
	case ASTBREAK:
	case ASTCONTINUE:
		return n;
	case ASTRETURN:
		if(n->n != nil){
			if(curfunc->type->link->t == TVOID)
				error(n, "valued return of void function");
			n->n = typecheck(n->n);
			implcast(&n->n, curfunc->type->link);
		}else if(curfunc->type->link->t != TVOID)
			error(n, "null return of typed function %d", curfunc->type->link->t);
		return n;
	case ASTBLOCK:
	case ASTLABEL:
	case ASTGOTO:
		return n;
	case ASTCAST:
		n->cast.n = typecheck(n->cast.n);
		return explcast(n->cast.n, n->cast.t);
	case ASTCALL:
		funccheck(n);
		return n;
	default:
		sysfatal("typecheck: unhandled type %A", n->t);
		return nil;
	}
}

int
longop(Line *l, int op, long a, long b, long *r)
{
	switch(op){
	case OPADD: *r = a + b; return 1;
	case OPSUB: *r = a - b; return 1;
	case OPRSB: *r = b - a; return 1;
	case OPMUL: *r = a * b; return 1;
	case OPDIV:
		if(b == 0){
			error(l, "division by zero");
			return 0;
		}
		*r = a / b;
		return 1;
	case OPMOD:
		if(b == 0){
			error(l, "division by zero");
			return 0;
		}
		*r = a % b;
		return 1;
	case OPAND: *r = a & b; return 1;
	case OPOR: *r = a | b; return 1;
	case OPXOR: *r = a ^ b; return 1;
	case OPLSH:
	case OPRSH:
		if(b < 0){
			warn(l, "negative shift amount");
			return 0;
		}
		if(b >= 32){
			warn(l, "large shift");
			*r = 0;
			return 1;
		}
		if(op == OPLSH)
			*r = a << b;
		else
			*r = (ulong)a >> b;
		return 1;
	case OPASR:
		if(b < 0){
			warn(l, "negative shift amount");
			return 0;
		}
		if(b >= 32){
			if(b > 32)
				warn(l, "large shift");
			*r = -((ulong)a >> 31);
			return 1;
		}
		*r = (-((ulong)a >> 31)) << 32 - b | (ulong)a >> b;
		return 1;
	case OPNEG: *r = -a; return 1;
	case OPCOM: *r = ~a; return 1;
	case OPMOV: case OPSXTW: case OPZXTW: *r = a; return 1;
	case OPISZ: *r = !a; return 1;
	case OPLOR: *r = a && b; return 1;
	case OPLAND: *r = a || b; return 1;
	case OPEQ: *r = a == b; return 1;
	case OPNE: *r = a != b; return 1;
	case OPLT: *r = a < b; return 1;
	case OPGT: *r = a > b; return 1;
	case OPLE: *r = a <= b; return 1;
	case OPGE: *r = a >= b; return 1;
	case OPSXTB: *r = (signed char) a; return 1;
	case OPSXTH: *r = (signed short) a; return 1;
	case OPZXTB: *r = (unsigned char) a; return 1;
	case OPZXTH: *r = (unsigned short) a; return 1;
	case OPLD: case OPLDB: case OPLDSB: case OPLDH: case OPLDSH: case OPLDD:
	case OPST: case OPSTB: case OPSTH: case OPSTD:
		return 0;
	default:
		error(l, "longop: unimplemented %O", op);
		return 0;
	}
}

ASTNode *
astfold(ASTNode *n)
{
	ASTNode *m, *m0, *m1, **p;
	int i;
	long r;
	
	if(n == nil)
		return nil;
	switch(n->t){
	case ASTCLONG:
	case ASTSYM:
	case ASTLABEL:
	case ASTGOTO:
	case ASTBREAK:
	case ASTCONTINUE:
		return n;
	case ASTBIN:
		n->n1 = astfold(n->n1);
		n->n2 = astfold(n->n2);
		if(n->n1->t == ASTCLONG && n->n2->t == ASTCLONG && longop(n, n->op, n->n1->lval, n->n2->lval, &r)){
			m = node(ASTCLONG, r);
			m->type = n->type;
			return m;
		}
		return n;
	case ASTUN:
		n->n1 = astfold(n->n1);
		if(n->n1->t == ASTCLONG && longop(n, n->op, n->n1->lval, 0, &r))
			return node(ASTCLONG, r);
		return n;
	case ASTINC:
		n->n1 = astfold(n->n1);
		return n;
	case ASTDEREF:
	case ASTADDROF:
		n->n = astfold(n->n);
		return n;
	case ASTMEMB:
		n->mb.n = astfold(n->mb.n);
		return n;
	case ASTASS:
		n->n1 = astfold(n->n1);
		n->n2 = astfold(n->n2);
		return n;
	case ASTCAST:
		n->cast.n = astfold(n->cast.n);
		return n;
	case ASTTERN:
		n->cond = astfold(n->cond);
		n->block = astfold(n->block);
		n->elsebl = astfold(n->elsebl);
		if(n->cond->t == ASTCLONG)
			return n->cond->lval ? n->block : n->elsebl;
		return n;
	case ASTRETURN:
		n->n = astfold(n->n);
		return n;
	case ASTCOMMA:
		n->n1 = astfold(n->n1);
		n->n2 = astfold(n->n2);
		return n;
	case ASTIF:
	case ASTWHILE:
	case ASTDOWHILE:
		n->cond = astfold(n->cond);
		n->block = astfold(n->block);
		return n;
	case ASTFOR:
		n->forl.once = astfold(n->forl.once);
		n->forl.cond = astfold(n->forl.cond);
		n->forl.iter = astfold(n->forl.iter);
		n->forl.block = astfold(n->forl.block);
		return n;
	case ASTBLOCK:
		for(p = &n->first; (m = *p) != nil; p = &m->next){
			m0 = m->next;
			m1 = astfold(m);
			if(m1 != m){
				*p = m1;
				m1->next = m0;
				m = m1;
			}
		}
		return n;
	case ASTCALL:
		n->func.n = astfold(n->func.n);
		for(i = 0; i < n->func.argn; i++)
			n->func.args[i] = astfold(n->func.args[i]);
		return n;
	default:
		print("astfold: unhandled type %A\n", n->t);
		return n;
	}
}
