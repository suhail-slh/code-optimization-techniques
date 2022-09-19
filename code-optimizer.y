%{
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
%}
%union{
	char strval[100];
	int intval;
}
%token var cnst IF PRINT
%type <strval> var vexp vcond stmt vblock cblock func IF PRINT
%type <intval> cnst cexp ccond
%right '='
%left EQL NEQL
%left '<' LEQL '>' GEQL
%left '+' '-'
%left '*' '/' '%'
%%
start:stmt;
|start stmt;
stmt:stmt';'			{	
							fprintf(yyout, "%s;\n", $1);
							execute();
						}
|cexp';'				{	execute();							}
|vexp';'				{	
							if(strlen($1) != 0)
								fprintf(yyout, "%s;\n", $1);
							execute();
						}
|func';'				{	
							fprintf(yyout, "%s;\n", $1);
							execute();
						}
|cblock';'				{	execute();							}
|vblock';'				{	
							if(strlen($1) != 0)
								fprintf(yyout, "%s;\n", $1);	
							execute();
						}
|ccond vblock			{	
							if($1)	{
								if(strlen($2) != 0)
									fprintf(yyout, "%s;\n", $2);
								execute();
							}
							penUp->front = penUp->rear = 0;
						}
|ccond cblock			{	
							if($1)	{
								fprintf(yyout, "%s;\n", $2);
								execute();
							}
							penUp->front = penUp->rear = 0;
						}
|vcond vblock			{
							if(strlen($2) != 0)
								fprintf(yyout, "%s\t%s\n", $1, $2);
							penUp->front = penUp->rear = 0;
						}
|vcond cblock			{	
							fprintf(yyout, "%s\t%s\n", $1, $2);
							penUp->front = penUp->rear = 0;
						}
cblock:'{'cexp'}'		{	
							if(tempstr[0] == '\0')
								sprintf($$, "{%s}", $2);
							else
								sprintf($$, "{%s}", tempstr);
						}
vblock:'{'vexp'}'		{	
							if(strlen($2) != 0)
								sprintf($$, "{%s}", $2);	
							else
								strcpy($$, "");
						}
|'{'func'}'				{	sprintf($$, "{%s}", $2);			}
func:PRINT cexp			{	sprintf($$, "print %d", $2);		}
|PRINT vexp				{	sprintf($$, "print %s", $2);		}
cexp:var'='cexp			{	
							setVarSub(varSub, $1, $1, $3);	
							sprintf(tempstr, "%s = %d", $1, $3);
							$$ = $3;
						}
|cexp'+'cexp			{	$$ = $1 + $3;						}
|cexp'-'cexp			{	$$ = $1 - $3;						}
|cexp'*'cexp			{	$$ = $1 * $3;						}
|cexp'/'cexp			{	if($3 != 0) $$ = $1 / $3;			}
|cexp'%'cexp			{	if($3 != 0) $$ = $1 % $3;			}
|cexp EQL cexp			{	$$ = $1 == $3;						}
|cexp NEQL cexp			{	$$ = $1 != $3;						}
|cexp'<'cexp			{	$$ = $1 < $3;						}
|cexp LEQL cexp			{	$$ = $1 <= $3;						}
|cexp'>'cexp			{	$$ = $1 > $3;						}
|cexp GEQL cexp			{	$$ = $1 >= $3;						}
|'('cexp')'				{	$$ = $2;							}
|cnst					{	$$ = $1;							}
vexp:var'='vexp			{	
							if(setExpSub(expSub, $3, $1))
								sprintf($$, "%s = %s", $1, $3);
							else 
								strcpy($$, "");
						}
|vexp'+'cexp			{	sprintf($$, "%s+%d", $1,$3);		}
|cexp'+'vexp			{	sprintf($$, "%s+%d", $3,$1);		}
|vexp'-'cexp			{	sprintf($$, "%s-%d", $1,$3);		}
|cexp'-'vexp			{	sprintf($$, "%s-%d", $3,$1);		}
|vexp'*'cexp			{	sprintf($$, "%s*%d", $1,$3);		}
|cexp'*'vexp			{	sprintf($$, "%s*%d", $3,$1);		}
|vexp'/'cexp			{	sprintf($$, "%s/%d", $1,$3);		}
|cexp'/'vexp			{	sprintf($$, "%d/%s", $1,$3);		}
|vexp'%'cexp			{	sprintf($$, "%s%%%d", $1,$3);		}
|cexp'%'vexp			{	sprintf($$, "%d%%%s", $1,$3);		}
|vexp EQL cexp			{	sprintf($$, "%s==%d", $1,$3);		}
|cexp EQL vexp			{	sprintf($$, "%s==%d", $3,$1);		}
|vexp NEQL cexp			{	sprintf($$, "%s!=%d", $1,$3);		}
|cexp NEQL vexp			{	sprintf($$, "%s!=%d", $3,$1);		}
|vexp'<'cexp			{	sprintf($$, "%s<%d", $1,$3);		}
|cexp'<'vexp			{	sprintf($$, "%s>%d", $3,$1);		}
|vexp LEQL cexp			{	sprintf($$, "%s<=%d", $1,$3);		}
|cexp LEQL vexp			{	sprintf($$, "%s>=%d", $3,$1);		}
|vexp'>'cexp			{	sprintf($$, "%s>%d", $1,$3);		}
|cexp'>'vexp			{	sprintf($$, "%s<%d", $3,$1);		}
|vexp GEQL cexp			{	sprintf($$, "%s>=%d", $1,$3);		}
|cexp GEQL vexp			{	sprintf($$, "%s<=%d", $3,$1);		}
|vexp'+'vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s+%s", $1,$3);
							else								
								sprintf($$, "%s+%s", $3,$1);
						}
|vexp'-'vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s-%s", $1,$3);
							else								
								sprintf($$, "%s-%s", $3,$1);	
						}
|vexp'*'vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s*%s", $1,$3);
							else								
								sprintf($$, "%s*%s", $3,$1);	
						}
|vexp'/'vexp			{	sprintf($$, "%s/%s", $1,$3);		}
|vexp'%'vexp			{	sprintf($$, "%s%%%s", $1,$3);		}
|vexp EQL vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s==%s", $1,$3);
							else								
								sprintf($$, "%s==%s", $3,$1);	
						}
|vexp NEQL vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s!=%s", $1,$3);
							else								
								sprintf($$, "%s!=%s", $3,$1);	
						}
|vexp'<'vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s<%s", $1,$3);
							else								
								sprintf($$, "%s>%s", $3,$1);	
						}
|vexp LEQL vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s<=%s", $1,$3);
							else								
								sprintf($$, "%s>=%s", $3,$1);	
						}
|vexp'>'vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s>%s", $1,$3);
							else								
								sprintf($$, "%s<%s", $3,$1);	
						}
|vexp GEQL vexp			{	
							if(strcmp($1, $3) <= 0)
								sprintf($$, "%s>=%s", $1,$3);
							else								
								sprintf($$, "%s<=%s", $3,$1);	
						}
|'('vexp')'				{	sprintf($$, "(%s)", $2);			}
|var					{	
							temptn = retrieve(varSub, $1);
							if(!temptn) 
								strcpy($$, $1);
							else if(temptn->intval != INF)
								sprintf($$, "%d", temptn->intval);
							else 
								sprintf($$, "%s", temptn->strval);	
						}						
ccond:IF cexp':'		{ 	$$ = $2;							}
vcond:IF vexp':'		{	sprintf($$, "if %s:\n", $2);		}
%%
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