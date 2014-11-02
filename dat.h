typedef struct ASTNode ASTNode;
typedef struct Symbol Symbol;
typedef struct SymTab SymTab;
typedef struct Type Type;
typedef struct Line Line;
typedef struct Loop Loop;
typedef struct IR IR;
typedef struct IRBlock IRBlock;
typedef struct Targ Targ;
typedef struct Function Function;
typedef struct Member Member;
typedef struct BitSet BitSet;

enum {
	SYMHASH = 32,
	PTRSZ = 4,
};

struct Type {
	char t, sign;
	int size, align;
	Type *next, *link;
	Member *memb;
	int nmemb;
	char *tag;
};

struct Member {
	Type *type;
	char *name;
	int off;
};

enum {
	TXXX,
	TUCHAR,
	TCHAR,
	TUSHORT,
	TSHORT,
	TUINT,
	TINT,
	TUVLONG,
	TVLONG,
	TFLOAT,
	TDOUBLE,
	TVOID,
	TIND,
	TFUNC,
	TARRAY,
	TSTRUCT,
	TUNION,
	NTYPE,
	
	TBASIC = TDOUBLE,
};
#define regtype (&types[TUINT])
extern Type types[];

enum {
	BINT = 1<<0,
	BCHAR = 1<<1,
	BSHORT = 1<<2,
	BLONG = 1<<3,
	BVLONG = 1<<4,
	BFLOAT = 1<<5,
	BDOUBLE = 1<<6,
	BVOID = 1<<7,
	BUNSIGNED = 1<<8,
	BSIGNED = 1<<9,
	BEXTERN = 1<<10,
	BSTATIC = 1<<11,
	BTYPEDEF = 1<<12,
	BMEM = 1<<13,
	
	BMASK = 0xff,
};

struct Line {
	char *filen;
	int lineno;
};

struct ASTNode {
	int t;
	Type *type;
	union {
		int lval;
		vlong vval;
		Symbol *sym;
		ASTNode *n;
		struct {
			int op;
			ASTNode *n1, *n2;
		};
		struct {
			ASTNode *name;
			Member *memb;
			int nmemb;
		};
		ASTNode *first;
		struct {
			ASTNode *cond;
			ASTNode *block;
			ASTNode *elsebl;
		};
		struct {
			ASTNode *once;
			ASTNode *cond;
			ASTNode *iter;
			ASTNode *block;
		} forl;
		struct {
			ASTNode *n;
			Type *t;
		} cast;
		struct {
			ASTNode *n;
			char *name;
			Member *m;
		} mb;
		struct {
			ASTNode *n;
			ASTNode **args;
			int argn;
		} func;
	};
	ASTNode *next;
	ASTNode **last;
	Line;
};

enum {
	ASTCLONG,
	ASTBIN,
	ASTASS,
	ASTTERN,
	ASTDEREF,
	ASTADDROF,
	ASTUN,
	ASTINC,
	ASTIF,
	ASTSYM,
	ASTARRAY,
	ASTFUNCTYPE,
	ASTCAST,
	ASTCOMMA,
	ASTBLOCK,
	ASTWHILE,
	ASTDOWHILE,
	ASTFOR,
	ASTBREAK,
	ASTCONTINUE,
	ASTRETURN,
	ASTLABEL,
	ASTGOTO,
	ASTMEMB,
	ASTCALL,
};

enum {
	OPMOV,
	OPUPLUS,
	OPNEG,
	OPTST,
	OPCALL,

	OPADD = 0x10,
	OPSUB,
	OPMUL,
	OPDIV,
	OPMOD,
	OPCMP,
	OPRSB,
	
	OPAND = 0x20,
	OPOR,
	OPXOR,
	OPLSH,
	OPRSH,
	OPASR,
	OPROR,
	OPCOM,
	
	OPSXTB,
	OPSXTH,
	OPSXTW,
	OPZXTB,
	OPZXTH,
	OPZXTW,
	
	OPLD = 0x30,
	OPLDB,
	OPLDSB,
	OPLDH,
	OPLDSH,
	OPLDD,
	OPST,
	OPSTB,
	OPSTH,
	OPSTD,
	
	OPFNEG = OPNEG + 0x40,
	OPFADD = 0x50,
	OPFSUB,
	OPFMUL,
	OPFDIV,
	OPFCMP = 0x55,

	OPEQ = 0x100,
	OPNE,
	OPLT,
	OPLE,
	OPGT,
	OPGE,
	OPB,
	OPBE,
	OPA,
	OPAE,
	
	OPLOR = 0x110,
	OPLAND,
	OPISZ,
	
	OPPHI = 0x200,
};

#define OPTYPE(o) ((o) & 0xf00)
#define OPGROUP(o) ((o) & 0xff0)

enum {
	OPNORM = 0,
	OPBRANCH = 0x100,
};

enum {
	PREINC,
	POSTINC,
	PREDEC,
	POSTDEC,
};

struct Symbol {
	int class;
	char *name;
	Symbol *next;
	Type *t;
	int level;
	int off;
	Line;
	Targ *targ;
	IRBlock *label;
	int num;
};

struct SymTab {
	Symbol *sym[SYMHASH];
	int level;
	SymTab *up;
};

struct Targ {
	int t;
	union {
		struct {
			Symbol *sym;
			int n;
			Targ *up;
		};
		Targ *link;
		int lval;
		int tn;
		int off;
		struct {
			Targ *a;
			Targ *b;
			int op;
		};
	};
	int num;
	IR *def;
	Type *type;
	int Rn;
};

enum {
	TARGXXX,
	TARGSYM,
	TARGSSA,
	TARGTEMP,
	TARGCONST,
	TARGRETV,
	TARGFP,
	TARGIND,
	TARGADDR,
	TARGARG,
	TARGSH,
};

enum {
	MAXUSE = 5,
};

struct IR {
	short op;
	char sz;
	union {
		struct {
			Targ *r, *a, *b;
		};
		struct {
			IRBlock *targ;
			IR *gotolink;
		};
		struct {
			Targ *r;
			Targ **a;
			int n;
		} φ;
	};
	IR *prev, *next;
	IR *link;
	Line;
};

enum {
	IRMARK = 1<<7,
};

struct IRBlock {
	int num;
	IR instr;
	IRBlock *next, *prev;
	IRBlock *branch;
	
	int nfrom, nto;
	IRBlock **from, *to[2];
	
	IRBlock *idom;
	IRBlock *domch, *chnext;
	
	IRBlock **front;
	int nfront, nafront;
	
	IRBlock *link;
};

struct Loop {
	BitSet *bl;
	IRBlock *head;
	Loop *next;
};

struct Function {
	IRBlock bllist;
	int tempcnt;
	Type *type;
	Symbol **arg;
	Loop *loops;
};

struct BitSet {
	int n;
	u32int p[1];
};

#pragma varargck type "A" int
#pragma varargck type "O" int
#pragma varargck type "T" Type *
#pragma varargck type "I" IR *
#pragma varargck type "J" Targ *
#pragma varargck type "S" BitSet *

extern Line curline;
extern Function *curfunc;
extern IRBlock **blocks;
extern int nblocks;
extern Targ **targs;
extern int ntargs;
