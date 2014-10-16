#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>
#include "dat.h"
#include "fns.h"
#include "y.tab.h"

Biobuf *inp;
Line curline;
typedef struct keyword keyword;

struct keyword {
	char *name;
	int tok;
};

keyword kwtable[] = {
	{"int", LINT},
	{"char", LCHAR},
	{"short", LSHORT},
	{"long", LLONG},
	{"unsigned", LUNSIGNED},
	{"signed", LSIGNED},
	{"float", LFLOAT},
	{"double", LDOUBLE},
	{"void", LVOID},
	{"extern", LEXTERN},
	{"static", LSTATIC},
	{"typedef", LTYPEDEF},
	{"if", LIF},
	{"else", LELSE},
	{"do", LDO},
	{"while", LWHILE},
	{"for", LFOR},
	{"break", LBREAK},
	{"continue", LCONTINUE},
	{"goto", LGOTO},
	{"return", LRETURN},
	{"struct", LSTRUCT},
	{"union", LUNION},
	{nil, 0},
};

keyword optable[] = {
	{"!=", LNE},
	{"%=", LMODEQ},
	{"&&", LAND},
	{"&=", LANDEQ},
	{"*=", LMULEQ},
	{"++", LINC},
	{"+=", LPLEQ},
	{"--", LDEC},
	{"-=", LMIEQ},
	{"->", LARROW},
	{"/=", LDIVEQ},
	{"<<", LLSH},
	{"<=", LLE},
	{"==", LEQ},
	{">=", LGE},
	{">>", LRSH},
	{"|=", LOREQ},
	{"||", LOR},
	{"^=", LXOREQ},
	{nil, 0},
};

keyword *oplook[128];

int
isident(int c)
{
	return isalpha(c) || c == '_' || c >= 0x80;
}

void
lexinit(void)
{
	keyword *kw;
	uchar c;
	
	for(kw = optable; kw->name != nil;){
		c = *kw->name;
		oplook[c] = kw;
		do
			kw++;
		while(kw->name != nil && *kw->name == c);
	}
}

int
yylex(void)
{
	char buf[64];
	char c, d, *p;
	int n;
	keyword *kw;
	
	do{
		c = Bgetc(inp);
		if(c == '\n')
			curline.lineno++;
	}while(isspace(c));
	if(isdigit(c)){
		n = c - '0';
		while(isdigit(c = Bgetc(inp)))
			n = n * 10 + c - '0';
		Bungetc(inp);
		yylval.lval = n;
		return LCONST;
	}
	if(isident(c)){
		p = buf;
		*p++ = c;
		while(isident(c = Bgetc(inp)))
			if(p < buf + sizeof(buf) - 1)
				*p++ = c;
		Bungetc(inp);
		*p = 0;
		for(kw = kwtable; kw->name != nil; kw++)
			if(strcmp(kw->name, buf) == 0)
				return kw->tok;
		yylval.sym = getsym(buf);
		if((yylval.sym->class & BTYPEDEF) != 0)
			return LTYPE;
		return LSYMBOL;
	}
	if((uchar)c < 0x80 && (kw = oplook[c]) != nil){
		d = Bgetc(inp);
		do{
			if(kw->name[1] == d){
				if(kw->tok == LLSH || kw->tok == LRSH)
					if(Bgetc(inp) == '=')
						return kw->tok == LLSH ? LLSHEQ : LRSHEQ;
					else
						Bungetc(inp);
				return kw->tok;
			}
		}while((++kw)->name != nil && *kw->name == c);
		Bungetc(inp);
	}
	return c;
}

void
yyerror(char *s)
{
	fprint(2, "%s:%d %s\n", curline.filen, curline.lineno, s);
}

void
error(Line* l, char *s, ...)
{
	va_list va;

	if(l == nil)
		l = &curline;
	va_start(va, s);
	fprint(2, "%s:%d ", l->filen, l->lineno);
	vfprint(2, s, va);
	fprint(2, "\n");
	va_end(va);
}
