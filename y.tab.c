
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "code-optimizer.y"

	# include <stdio.h>
	# include <stdlib.h>
	# include <string.h>
	# include <limits.h>
	# define INF INT_MAX
	# define MAXS 100
	# define ENC 128
	typedef struct trienode {
		struct trienode* children[ENC];
		char strval[MAXS];
		int intval;
	} TrieNode;
	typedef struct pendingupdates {
		TrieNode* tn[MAXS];
		char strval[MAXS][MAXS];
		int intval[MAXS], front, rear;
	} PendingUpdates;
	void initChildren(TrieNode*);
	TrieNode* retrieve(TrieNode*, char*);
	void setVarSub(TrieNode*, char*, char*, int);	
	int setExpSub(TrieNode*, char*, char*);
	void push(TrieNode*, char*, int);
	void execute();
	TrieNode *varSub, *expSub, *temptn;
	PendingUpdates *penUp;
	extern FILE *yyin, *yyout;
	char ip[MAXS], op[MAXS], tempstr[MAXS];
	int flag;


/* Line 189 of yacc.c  */
#line 105 "y.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     var = 258,
     cnst = 259,
     IF = 260,
     PRINT = 261,
     NEQL = 262,
     EQL = 263,
     GEQL = 264,
     LEQL = 265
   };
#endif
/* Tokens.  */
#define var 258
#define cnst 259
#define IF 260
#define PRINT 261
#define NEQL 262
#define EQL 263
#define GEQL 264
#define LEQL 265




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 31 "code-optimizer.y"

	char strval[100];
	int intval;



/* Line 214 of yacc.c  */
#line 168 "y.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 180 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  26
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   261

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  25
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  10
/* YYNRULES -- Number of rules.  */
#define YYNRULES  70
/* YYNRULES -- Number of states.  */
#define YYNSTATES  113

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   265

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    18,     2,     2,
      22,    23,    16,    14,     2,    15,     2,    17,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    24,    19,
      10,     7,    11,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    20,     2,    21,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     8,     9,    12,    13
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    11,    14,    17,    20,    23,
      26,    29,    32,    35,    38,    42,    46,    50,    53,    56,
      60,    64,    68,    72,    76,    80,    84,    88,    92,    96,
     100,   104,   108,   110,   114,   118,   122,   126,   130,   134,
     138,   142,   146,   150,   154,   158,   162,   166,   170,   174,
     178,   182,   186,   190,   194,   198,   202,   206,   210,   214,
     218,   222,   226,   230,   234,   238,   242,   246,   250,   252,
     256
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      26,     0,    -1,    27,    -1,    26,    27,    -1,    27,    19,
      -1,    31,    19,    -1,    32,    19,    -1,    30,    19,    -1,
      28,    19,    -1,    29,    19,    -1,    33,    29,    -1,    33,
      28,    -1,    34,    29,    -1,    34,    28,    -1,    20,    31,
      21,    -1,    20,    32,    21,    -1,    20,    30,    21,    -1,
       6,    31,    -1,     6,    32,    -1,     3,     7,    31,    -1,
      31,    14,    31,    -1,    31,    15,    31,    -1,    31,    16,
      31,    -1,    31,    17,    31,    -1,    31,    18,    31,    -1,
      31,     9,    31,    -1,    31,     8,    31,    -1,    31,    10,
      31,    -1,    31,    13,    31,    -1,    31,    11,    31,    -1,
      31,    12,    31,    -1,    22,    31,    23,    -1,     4,    -1,
       3,     7,    32,    -1,    32,    14,    31,    -1,    31,    14,
      32,    -1,    32,    15,    31,    -1,    31,    15,    32,    -1,
      32,    16,    31,    -1,    31,    16,    32,    -1,    32,    17,
      31,    -1,    31,    17,    32,    -1,    32,    18,    31,    -1,
      31,    18,    32,    -1,    32,     9,    31,    -1,    31,     9,
      32,    -1,    32,     8,    31,    -1,    31,     8,    32,    -1,
      32,    10,    31,    -1,    31,    10,    32,    -1,    32,    13,
      31,    -1,    31,    13,    32,    -1,    32,    11,    31,    -1,
      31,    11,    32,    -1,    32,    12,    31,    -1,    31,    12,
      32,    -1,    32,    14,    32,    -1,    32,    15,    32,    -1,
      32,    16,    32,    -1,    32,    17,    32,    -1,    32,    18,
      32,    -1,    32,     9,    32,    -1,    32,     8,    32,    -1,
      32,    10,    32,    -1,    32,    13,    32,    -1,    32,    11,
      32,    -1,    32,    12,    32,    -1,    22,    32,    23,    -1,
       3,    -1,     5,    31,    24,    -1,     5,    32,    24,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    44,    44,    45,    46,    50,    51,    56,    60,    61,
      66,    74,    81,    86,    90,    96,   102,   103,   104,   105,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   157,   163,   169,
     170,   171,   177,   183,   189,   195,   201,   207,   208,   217,
     218
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "var", "cnst", "IF", "PRINT", "'='",
  "NEQL", "EQL", "'<'", "'>'", "GEQL", "LEQL", "'+'", "'-'", "'*'", "'/'",
  "'%'", "';'", "'{'", "'}'", "'('", "')'", "':'", "$accept", "start",
  "stmt", "cblock", "vblock", "func", "cexp", "vexp", "ccond", "vcond", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,    61,   262,   263,
      60,    62,   264,   265,    43,    45,    42,    47,    37,    59,
     123,   125,    40,    41,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    25,    26,    26,    27,    27,    27,    27,    27,    27,
      27,    27,    27,    27,    28,    29,    29,    30,    30,    31,
      31,    31,    31,    31,    31,    31,    31,    31,    31,    31,
      31,    31,    31,    32,    32,    32,    32,    32,    32,    32,
      32,    32,    32,    32,    32,    32,    32,    32,    32,    32,
      32,    32,    32,    32,    32,    32,    32,    32,    32,    32,
      32,    32,    32,    32,    32,    32,    32,    32,    32,    33,
      34
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     3,     3,     3,     2,     2,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     1,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     1,     3,
       3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    68,    32,     0,     0,     0,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    17,
      18,     0,     0,     0,     0,     0,     1,     3,     4,     8,
       9,     7,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     6,    11,    10,    13,    12,
      19,    33,    69,    70,    16,    14,    15,    31,    67,    26,
      47,    25,    45,    27,    49,    29,    53,    30,    55,    28,
      51,    20,    35,    21,    37,    22,    39,    23,    41,    24,
      43,    46,    62,    44,    61,    48,    63,    52,    65,    54,
      66,    50,    64,    34,    56,    36,    57,    38,    58,    40,
      59,    42,    60
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     7,     8,     9,    10,    11,    12,    13,    14,    15
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -10
static const yytype_int16 yypact[] =
{
      53,    16,   -10,    18,    18,    57,    18,     4,    -8,    -7,
       6,     8,   190,   202,    42,    42,    18,    96,   113,   214,
     225,     7,   162,   176,   130,   146,   -10,    -8,   -10,   -10,
     -10,   -10,    18,    18,    18,    18,    18,    18,    18,    18,
      18,    18,    18,   -10,    18,    18,    18,    18,    18,    18,
      18,    18,    18,    18,    18,   -10,   -10,   -10,   -10,   -10,
     214,   225,   -10,   -10,   -10,   -10,   -10,   -10,   -10,   234,
     243,   234,   243,     0,    52,     0,    52,     0,    52,     0,
      52,    60,    99,    60,    99,   -10,   -10,   -10,   -10,   -10,
     -10,   234,   243,   234,   243,     0,    52,     0,    52,     0,
      52,     0,    52,    60,    99,    60,    99,   -10,   -10,   -10,
     -10,   -10,   -10
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -10,   -10,    64,    -9,     5,    59,    -3,    49,   -10,   -10
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      17,    19,    22,    24,    26,    56,    58,     1,     2,     3,
       4,    28,    29,    60,    38,    39,    40,    41,    42,    57,
      59,     1,     2,    16,     5,    30,     6,    31,    64,    69,
      71,    73,    75,    77,    79,    81,    83,    85,    87,    89,
       6,    91,    93,    95,    97,    99,   101,   103,   105,   107,
     109,   111,    18,    20,    23,    25,     1,     2,     3,     4,
       1,     2,     5,     4,    21,    61,    50,    51,    52,    53,
      54,    27,     0,     5,     0,     6,    40,    41,    42,     6,
       0,    70,    72,    74,    76,    78,    80,    82,    84,    86,
      88,    90,     0,    92,    94,    96,    98,   100,   102,   104,
     106,   108,   110,   112,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    52,    53,    54,     0,     0,
      62,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,    63,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,     0,
       0,     0,     0,    67,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,    68,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,     0,     0,    65,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,    66,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    46,    47,    48,    49,    50,    51,    52,
      53,    54
};

static const yytype_int8 yycheck[] =
{
       3,     4,     5,     6,     0,    14,    15,     3,     4,     5,
       6,    19,    19,    16,    14,    15,    16,    17,    18,    14,
      15,     3,     4,     7,    20,    19,    22,    19,    21,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      22,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     3,     4,     5,     6,     3,     4,     5,     6,
       3,     4,    20,     6,     5,    16,    14,    15,    16,    17,
      18,     7,    -1,    20,    -1,    22,    16,    17,    18,    22,
      -1,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    16,    17,    18,    -1,    -1,
      24,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    -1,    -1,    -1,    -1,    24,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      -1,    -1,    -1,    23,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    -1,    -1,    -1,    23,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    -1,    21,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    -1,    21,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    10,    11,    12,    13,    14,    15,    16,
      17,    18
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,    20,    22,    26,    27,    28,
      29,    30,    31,    32,    33,    34,     7,    31,    32,    31,
      32,    30,    31,    32,    31,    32,     0,    27,    19,    19,
      19,    19,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    28,    29,    28,    29,
      31,    32,    24,    24,    21,    21,    21,    23,    23,    31,
      32,    31,    32,    31,    32,    31,    32,    31,    32,    31,
      32,    31,    32,    31,    32,    31,    32,    31,    32,    31,
      32,    31,    32,    31,    32,    31,    32,    31,    32,    31,
      32,    31,    32,    31,    32,    31,    32,    31,    32,    31,
      32,    31,    32
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:

/* Line 1455 of yacc.c  */
#line 46 "code-optimizer.y"
    {	
							fprintf(yyout, "%s;\n", (yyvsp[(1) - (2)].strval));
							execute();
						}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 50 "code-optimizer.y"
    {	execute();							}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 51 "code-optimizer.y"
    {	
							if(strlen((yyvsp[(1) - (2)].strval)) != 0)
								fprintf(yyout, "%s;\n", (yyvsp[(1) - (2)].strval));
							execute();
						}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 56 "code-optimizer.y"
    {	
							fprintf(yyout, "%s;\n", (yyvsp[(1) - (2)].strval));
							execute();
						}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 60 "code-optimizer.y"
    {	execute();							}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 61 "code-optimizer.y"
    {	
							if(strlen((yyvsp[(1) - (2)].strval)) != 0)
								fprintf(yyout, "%s;\n", (yyvsp[(1) - (2)].strval));	
							execute();
						}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 66 "code-optimizer.y"
    {	
							if((yyvsp[(1) - (2)].intval))	{
								if(strlen((yyvsp[(2) - (2)].strval)) != 0)
									fprintf(yyout, "%s;\n", (yyvsp[(2) - (2)].strval));
								execute();
							}
							penUp->front = penUp->rear = 0;
						}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 74 "code-optimizer.y"
    {	
							if((yyvsp[(1) - (2)].intval))	{
								fprintf(yyout, "%s;\n", (yyvsp[(2) - (2)].strval));
								execute();
							}
							penUp->front = penUp->rear = 0;
						}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 81 "code-optimizer.y"
    {
							if(strlen((yyvsp[(2) - (2)].strval)) != 0)
								fprintf(yyout, "%s\t%s\n", (yyvsp[(1) - (2)].strval), (yyvsp[(2) - (2)].strval));
							penUp->front = penUp->rear = 0;
						}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 86 "code-optimizer.y"
    {	
							fprintf(yyout, "%s\t%s\n", (yyvsp[(1) - (2)].strval), (yyvsp[(2) - (2)].strval));
							penUp->front = penUp->rear = 0;
						}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 90 "code-optimizer.y"
    {	
							if(tempstr[0] == '\0')
								sprintf((yyval.strval), "{%s}", (yyvsp[(2) - (3)].intval));
							else
								sprintf((yyval.strval), "{%s}", tempstr);
						}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 96 "code-optimizer.y"
    {	
							if(strlen((yyvsp[(2) - (3)].strval)) != 0)
								sprintf((yyval.strval), "{%s}", (yyvsp[(2) - (3)].strval));	
							else
								strcpy((yyval.strval), "");
						}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 102 "code-optimizer.y"
    {	sprintf((yyval.strval), "{%s}", (yyvsp[(2) - (3)].strval));			}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 103 "code-optimizer.y"
    {	sprintf((yyval.strval), "print %d", (yyvsp[(2) - (2)].intval));		}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 104 "code-optimizer.y"
    {	sprintf((yyval.strval), "print %s", (yyvsp[(2) - (2)].strval));		}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 105 "code-optimizer.y"
    {	
							setVarSub(varSub, (yyvsp[(1) - (3)].strval), (yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].intval));	
							sprintf(tempstr, "%s = %d", (yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].intval));
							(yyval.intval) = (yyvsp[(3) - (3)].intval);
						}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 110 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) + (yyvsp[(3) - (3)].intval);						}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 111 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) - (yyvsp[(3) - (3)].intval);						}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 112 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) * (yyvsp[(3) - (3)].intval);						}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 113 "code-optimizer.y"
    {	if((yyvsp[(3) - (3)].intval) != 0) (yyval.intval) = (yyvsp[(1) - (3)].intval) / (yyvsp[(3) - (3)].intval);			}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 114 "code-optimizer.y"
    {	if((yyvsp[(3) - (3)].intval) != 0) (yyval.intval) = (yyvsp[(1) - (3)].intval) % (yyvsp[(3) - (3)].intval);			}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 115 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) == (yyvsp[(3) - (3)].intval);						}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 116 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) != (yyvsp[(3) - (3)].intval);						}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 117 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) < (yyvsp[(3) - (3)].intval);						}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 118 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) <= (yyvsp[(3) - (3)].intval);						}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 119 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) > (yyvsp[(3) - (3)].intval);						}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 120 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (3)].intval) >= (yyvsp[(3) - (3)].intval);						}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 121 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(2) - (3)].intval);							}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 122 "code-optimizer.y"
    {	(yyval.intval) = (yyvsp[(1) - (1)].intval);							}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 123 "code-optimizer.y"
    {	
							if(setExpSub(expSub, (yyvsp[(3) - (3)].strval), (yyvsp[(1) - (3)].strval)))
								sprintf((yyval.strval), "%s = %s", (yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval));
							else 
								strcpy((yyval.strval), "");
						}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 129 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s+%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 130 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s+%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 131 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s-%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 132 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s-%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 133 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s*%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 134 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s*%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 135 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s/%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 136 "code-optimizer.y"
    {	sprintf((yyval.strval), "%d/%s", (yyvsp[(1) - (3)].intval),(yyvsp[(3) - (3)].strval));		}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 137 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s%%%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 138 "code-optimizer.y"
    {	sprintf((yyval.strval), "%d%%%s", (yyvsp[(1) - (3)].intval),(yyvsp[(3) - (3)].strval));		}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 139 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s==%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 140 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s==%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 141 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s!=%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 142 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s!=%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 143 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s<%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 144 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s>%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 145 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s<=%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 146 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s>=%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 147 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s>%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 148 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s<%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 149 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s>=%d", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].intval));		}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 150 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s<=%d", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].intval));		}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 151 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s+%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s+%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));
						}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 157 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s-%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s-%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 163 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s*%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s*%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 169 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s/%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));		}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 170 "code-optimizer.y"
    {	sprintf((yyval.strval), "%s%%%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));		}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 171 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s==%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s==%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 177 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s!=%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s!=%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 183 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s<%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s>%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 189 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s<=%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s>=%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 195 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s>%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s<%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 201 "code-optimizer.y"
    {	
							if(strcmp((yyvsp[(1) - (3)].strval), (yyvsp[(3) - (3)].strval)) <= 0)
								sprintf((yyval.strval), "%s>=%s", (yyvsp[(1) - (3)].strval),(yyvsp[(3) - (3)].strval));
							else								
								sprintf((yyval.strval), "%s<=%s", (yyvsp[(3) - (3)].strval),(yyvsp[(1) - (3)].strval));	
						}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 207 "code-optimizer.y"
    {	sprintf((yyval.strval), "(%s)", (yyvsp[(2) - (3)].strval));			}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 208 "code-optimizer.y"
    {	
							temptn = retrieve(varSub, (yyvsp[(1) - (1)].strval));
							if(!temptn) 
								strcpy((yyval.strval), (yyvsp[(1) - (1)].strval));
							else if(temptn->intval != INF)
								sprintf((yyval.strval), "%d", temptn->intval);
							else 
								sprintf((yyval.strval), "%s", temptn->strval);	
						}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 217 "code-optimizer.y"
    { 	(yyval.intval) = (yyvsp[(2) - (3)].intval);							}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 218 "code-optimizer.y"
    {	sprintf((yyval.strval), "if %s:\n", (yyvsp[(2) - (3)].strval));		}
    break;



/* Line 1455 of yacc.c  */
#line 2075 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 219 "code-optimizer.y"

void yyerror (char const *s) {
   fprintf (stderr, "%s\n", s);
}
void push(TrieNode* x, char* a, int b) {
	if((penUp->rear + 1) % MAXS == penUp->front)
		return;
	int i = penUp->rear++;
	penUp->tn[i] = x;
	strcpy(penUp->strval[i], a);
	penUp->intval[i] = b;
}
void execute() {
	TrieNode* x;
	int i;
	while(penUp->front != penUp->rear) {
		i = penUp->front++;
		penUp->front %= MAXS;
		x = penUp->tn[i];
		strcpy(x->strval, penUp->strval[i]);
		x->intval = penUp->intval[i] < INF ? penUp->intval[i] : INF;
	}
}
void initChildren(TrieNode* x) {
	for(int i = 0; i < 128; i++) {
		x->children[i] = NULL;
		strcpy(x->strval, "");
		x->intval = INF;
	}
}
TrieNode* retrieve(TrieNode* x, char* key) {
	for(int i = 0; key[i] != '\0'; i++) {
		char node = key[i];
		if(!x->children[node]) 
			return NULL;
		x = x->children[node];
	}
	return x;
}
void setVarSub(TrieNode* x, char* key, char* a, int b) {
	for(int i = 0; key[i] != '\0'; i++) {
		char node = key[i];
		if(!x->children[node]) {
			x->children[node] = (TrieNode*) malloc(sizeof(TrieNode));
			initChildren(x->children[node]);
		}
		x = x->children[node];
	}
	strcpy(x->strval, a);
	x->intval = INF;
	push(x, a, b);
}
int setExpSub(TrieNode* x, char* key, char* a) {
	int flag = 0;
	for(int i = 0; key[i] != '\0'; i++) {
		char node = key[i];
		if(node == ' ') 
			continue;
		if(!x->children[node]) {
			x->children[node] = (TrieNode*) malloc(sizeof(TrieNode));
			initChildren(x->children[node]);
			flag = 1;
		}
		x = x->children[node];
	}
	if(flag) {
		push(x, a, INF);
		setVarSub(varSub, a, a, INF);
		return 1;
	}
	setVarSub(varSub, a, x->strval, INF);
	return 0;
}
long int findSize(FILE* fp)
{
    fseek(fp, 0L, SEEK_END);
    long int res = ftell(fp);
    return res;
}
int main(int argc, char *argv[]) {
	if(argc != 2) {
		printf("Invalid Arguments");
		return 0;
	}
	strcpy(ip, argv[1]);
	sprintf(op, "%s_.txt", ip);
	varSub = (TrieNode*) malloc(sizeof(TrieNode));
	expSub = (TrieNode*) malloc(sizeof(TrieNode));
	penUp = (PendingUpdates*) malloc(sizeof(PendingUpdates));
	flag = 1;
	while(flag) {		
		yyin = fopen(ip, "r");
		if(!yyin) {
			printf("File Not Found");
			return 0;
		}
		yyout = fopen(op, "w");		
		initChildren(varSub); 
		initChildren(expSub);
		penUp->front = penUp->rear = 0;
		yyparse();
		if(findSize(yyout) == 0 || findSize(yyin) == findSize(yyout))
			flag = 0;
		fclose(yyin);
		fclose(yyout);
		strcpy(ip, op);
		sprintf(op, "%s_.txt", ip);
	}
	return 0;
}
