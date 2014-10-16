%{
#include <u.h>
#include <libc.h>
#include "dat.h"
#include "fns.h"

Type *lasttype;
int lastclass;
%}

%union {
	Symbol *sym;
	ASTNode *node;
	Type *typ;
	struct {
		Type *t;
		int i;
	} ti;
	struct {
		Member *m;
		int mn;
	} memb;
	char *str;
	long lval;
	vlong vval;
}

%token <sym> LSYMBOL LTYPE
%token <lval> LCONST

%type <node> expr stat declar dirdeclar block statlist zexpr structdecl xdeclar xdirdeclar
%type <ti> bctype ctype
%type <lval> tname cname tcname cnames btype justitype
%type <typ> dname justtype casttype
%type <memb> parlist rparlist structbody
%type <sym> tag

%token LIF LELSE LINT LCHAR LSHORT LLONG LUNSIGNED LSIGNED LDOUBLE LFLOAT LEXTERN LSTATIC LTYPEDEF LVOID LDO LWHILE LFOR
%token LBREAK LCONTINUE LGOTO LRETURN LSTRUCT LUNION

%left ','
%right '=' LPLEQ LMIEQ LMULEQ LDIVEQ LMODEQ LANDEQ LXOREQ LOREQ LLSHEQ LRSHEQ
%right '?'
%left LOR
%left LAND
%left '|'
%left '^'
%left '&'
%left LEQ LNE
%left '<' '>' LLE LGE
%left LLSH LRSH
%left '+' '-'
%left '*' '/' '%'
%left LDEREF
%left LINC LDEC '!' '~'
%left '.' LARROW '['

%%

program:
	| program extdecl

extdecl: funcdef | declspec xdecllist ';' | xdecllist ';' | declspec ';'

xdecllist: xdecl | xdecllist ',' xdecl
xdecl: xdeclar { dodecl($1, lasttype, lastclass); }

funcdef: declspec xdeclar { newfunc(dodecl($2, lasttype, lastclass)); } block { irgen(astfold($block)); }
| xdeclar { newfunc(dodecl($1, lasttype, lastclass)); } block { irgen(astfold($block)); } 

declar:
	dirdeclar
	| '*' declar { $$ = node(ASTDEREF, $2); }
dirdeclar:
	LSYMBOL { $$ = node(ASTSYM, $1); }
	| LTYPE { $$ = node(ASTSYM, $1); }
	| '(' declar ')' { $$ = $2; }
	| dirdeclar '[' ']' { $$ = node(ASTARRAY, $1, nil); }
	| dirdeclar '[' expr ']' { $$ = node(ASTARRAY, $1, $3); }
	| dirdeclar '(' parlist ')' { $$ = node(ASTFUNCTYPE, $1, $3); }

xdeclar:
	xdirdeclar
	| '*' xdeclar { $$ = node(ASTDEREF, $2); }
xdirdeclar:
	LSYMBOL { $$ = node(ASTSYM, $1); }
	| '(' xdeclar ')' { $$ = $2; }
	| xdirdeclar '[' ']' { $$ = node(ASTARRAY, $1, nil); }
	| xdirdeclar '[' expr ']' { $$ = node(ASTARRAY, $1, $3); }
	| xdirdeclar '(' parlist ')' { $$ = node(ASTFUNCTYPE, $1, $3); }


parlist: { $$.m = nil; $$.mn = 0; } | rparlist;
rparlist:
	justtype declar { $$.m = doparam($2, $1, nil, 0); $$.mn = 1; } 
	| casttype { $$.m = doparam(nil, $1, nil, 0); $$.mn = 1; }
	| rparlist ',' justtype declar { $$.m = doparam($4, $3, $1.m, $1.mn); $$.mn = $1.mn + 1; }
	| rparlist ',' casttype { $$.m = doparam(nil, $3, $1.m, $1.mn); $$.mn = $1.mn + 1; }

block: '{' { levelch(1); } decllist statlist '}' { levelch(-1); $$ = $statlist; }

decllist: | decllist decl
decl: declspec initdecllist ';' | declspec ';'

declspec: cnames bctype { lasttype = $2.t; lastclass = typeor($1, $2.i); }

bctype: btype { $$.t = bittype($1); $$.i = $1 & ~BMASK; } | ctype
btype: tname | btype tcname { $$ = typeor($1, $2); }
tcname: tname | cname
ctype: dname { $$.t = $1; $$.i = 0; } | ctype cname { $$.t = $1.t; $$.i = typeor($1.i, $2); }
cnames: { $$ = 0; } | cnames cname { $$ = typeor($1, $2); }

justtype: justitype { $$ = bittype($1); } | dname;
justitype: tname | justitype tname { $$ = typeor($1, $2); };

casttype:
	justtype
	| justtype '*' { $$ = type(TIND, $1); }
	| justtype '(' '*' ')' '(' parlist ')' { $$ = type(TIND, type(TFUNC, $1, $6.m, $6.mn)); }

tname:
	LINT {$$ = BINT;}
	| LCHAR {$$ = BCHAR;}
	| LSHORT {$$ = BSHORT;}
	| LLONG {$$ = BLONG;}
	| LUNSIGNED {$$ = BUNSIGNED;}
	| LSIGNED {$$ = BSIGNED;}
	| LDOUBLE {$$ = BDOUBLE;}
	| LFLOAT {$$ = BFLOAT;}

cname:
	LEXTERN {$$ = BEXTERN;}
	| LSTATIC {$$ = BSTATIC;}
	| LTYPEDEF {$$ = BTYPEDEF;}

dname:
	LTYPE { $$ = $1->t; }
	| LVOID { $$ = type(TVOID); }
	| LSTRUCT '{' structbody '}' { $$ = type(TSTRUCT, nil, $3.m, $3.mn); }
	| LSTRUCT tag '{' structbody '}' { $$ = type(TSTRUCT, $2->name, $4.m, $4.mn); }
	| LSTRUCT tag { $$ = type(TSTRUCT, $2->name, nil, -1); }
	| LUNION '{' structbody '}' { $$ = type(TUNION, nil, $3.m, $3.mn); }
	| LUNION tag '{' structbody '}' { $$ = type(TUNION, $2->name, $4.m, $4.mn); }
	| LUNION tag { $$ = type(TUNION, $2->name, nil, -1); }
tag: LSYMBOL | LTYPE
structbody: { $$.m = nil; $$.mn = 0; }
	| structbody declspec structdecl ';' { $$.m = dostruct($3, lasttype, $1.m, $1.mn, &$$.mn); }
structdecl: declar { $1->last = &$1->next; } | structdecl ',' declar { *$1->last = $3; $1->last = &$3->next; }

initdecllist: initdecl | initdecllist ',' initdecl
initdecl: declar { dodecl($1, lasttype, lastclass); }

statlist: { $$ = node(ASTBLOCK); }
	| statlist stat
		{
			ASTNode *n, *nn;
		
			if($2 != nil){
				n = $2;
				while(n != nil){
					nn = n->next;
					n = typecheck(n);
					n->next = nil;
					*$1->last = n;
					$1->last = &n->next;
					n = nn;
				}
			}
			$$ = $1;
		}

stat:
	';' { $$ = nil; }
	| expr ';'
	| LIF '(' expr ')' stat { $$ = node(ASTIF, $expr, $stat, nil); }
	| LIF '(' expr ')' stat LELSE stat { $$ = node(ASTIF, $3, $5, $7); }
	| LWHILE '(' expr ')' stat { $$ = node(ASTWHILE, $expr, $stat); }
	| LDO stat LWHILE '(' expr ')' ';' { $$ = node(ASTDOWHILE, $expr, $stat); }
	| LFOR '(' zexpr ';' zexpr ';' zexpr ')' stat { $$ = node(ASTFOR, $zexpr#1, $zexpr#2, $zexpr#3, $stat); }
	| block
	| LBREAK ';' { $$ = node(ASTBREAK); }
	| LCONTINUE ';' { $$ = node(ASTCONTINUE); }
	| LRETURN ';' { $$ = node(ASTRETURN, nil); }
	| LRETURN expr ';' { $$ = node(ASTRETURN, $expr); }
	| LSYMBOL ':' stat { $$ = node(ASTLABEL, $1); $$->next = $3; }
	| LGOTO LSYMBOL ';' { $$ = node(ASTGOTO, $2); }
	
zexpr: expr | { $$ = nil; }

expr:
	'(' expr ')' { $$ = $2; }
	| LCONST { $$ = node(ASTCLONG, $1); }
	| LSYMBOL { checksym($1); $$ = node(ASTSYM, $1); }
	| expr ',' expr { $$ = node(ASTCOMMA, $1, $3); }
	| expr '=' expr { $$ = node(ASTASS, OPMOV, $1, $3); }
	| expr LPLEQ expr { $$ = node(ASTASS, OPADD, $1, $3); }
	| expr LMIEQ expr { $$ = node(ASTASS, OPSUB, $1, $3); }
	| expr LMULEQ expr { $$ = node(ASTASS, OPMUL, $1, $3); }
	| expr LDIVEQ expr { $$ = node(ASTASS, OPDIV, $1, $3); }
	| expr LMODEQ expr { $$ = node(ASTASS, OPMOD, $1, $3); }
	| expr LANDEQ expr { $$ = node(ASTASS, OPAND, $1, $3); }
	| expr LOREQ expr { $$ = node(ASTASS, OPOR, $1, $3); }
	| expr LXOREQ expr { $$ = node(ASTASS, OPXOR, $1, $3); }
	| expr LLSHEQ expr { $$ = node(ASTASS, OPLSH, $1, $3); }
	| expr LRSHEQ expr { $$ = node(ASTASS, OPRSH, $1, $3); }
	| expr '?' expr ':' expr %prec '?' { $$ = node(ASTTERN, $1, $3, $5); }
	| expr LOR expr { $$ = node(ASTBIN, OPLOR, $1, $3); }
	| expr LAND expr { $$ = node(ASTBIN, OPLAND, $1, $3); }
	| expr '|' expr { $$ = node(ASTBIN, OPOR, $1, $3); }
	| expr '^' expr { $$ = node(ASTBIN, OPXOR, $1, $3); }
	| expr '&' expr { $$ = node(ASTBIN, OPAND, $1, $3); }
	| expr LEQ expr { $$ = node(ASTBIN, OPEQ, $1, $3); }
	| expr LNE expr { $$ = node(ASTBIN, OPNE, $1, $3); }
	| expr '<' expr { $$ = node(ASTBIN, OPLT, $1, $3); }
	| expr '>' expr { $$ = node(ASTBIN, OPGT, $1, $3); }
	| expr LLE expr { $$ = node(ASTBIN, OPLE, $1, $3); }
	| expr LGE expr { $$ = node(ASTBIN, OPGE, $1, $3); }
	| expr LLSH expr { $$ = node(ASTBIN, OPLSH, $1, $3); }
	| expr LRSH expr { $$ = node(ASTBIN, OPRSH, $1, $3); }
	| expr '+' expr { $$ = node(ASTBIN, OPADD, $1, $3); }
	| expr '-' expr { $$ = node(ASTBIN, OPSUB, $1, $3); }
	| expr '*' expr { $$ = node(ASTBIN, OPMUL, $1, $3); }
	| expr '/' expr { $$ = node(ASTBIN, OPDIV, $1, $3); }
	| expr '%' expr { $$ = node(ASTBIN, OPMOD, $1, $3); }
	| '&' expr %prec LINC { $$ = node(ASTADDROF, $expr); }
	| '+' expr %prec LINC { $$ = node(ASTUN, OPUPLUS, $expr); }
	| '-' expr %prec LINC { $$ = node(ASTUN, OPNEG, $expr); }
	| '!' expr { $$ = node(ASTUN, OPISZ, $expr); }
	| '~' expr { $$ = node(ASTUN, OPCOM, $expr); }
	| LINC expr { $$ = node(ASTINC, PREINC, $expr); }
	| LDEC expr { $$ = node(ASTINC, PREDEC, $expr); } 
	| expr LINC { $$ = node(ASTINC, POSTINC, $expr); }
	| expr LDEC { $$ = node(ASTINC, POSTDEC, $expr); }
	| '*' expr %prec LDEREF { $$ = node(ASTDEREF, $expr); }
	| expr '[' expr ']' { $$ = node(ASTDEREF, node(ASTBIN, OPADD, $1, $3)); }
	| '(' casttype ')' expr %prec LINC { $$ = node(ASTCAST, $4, $2); }
	| expr '.' tag { $$ = node(ASTMEMB, $1, $3->name); }
	| expr LARROW tag { $$ = node(ASTMEMB, node(ASTDEREF, $1), $3->name); }
