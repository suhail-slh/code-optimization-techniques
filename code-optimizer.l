%{
	# include "y.tab.h"
	# include <string.h>
%}
%%
[a-zA-Z]+		{	
					if(strcmp(yytext, "if") == 0) 
						return IF;
					if(strcmp(yytext, "print") == 0)
						return PRINT;
					strcpy(yylval.strval, yytext);
					return var;		
				}
[0-9]+			{	
					yylval.intval = atoi(yytext);	
					return cnst;					
				}
[\+\-\*\/\%\=\<\>\!]+	{	
					if(strcmp(yytext, "==") == 0)
						return EQL;
					if(strcmp(yytext, "!=") == 0)
						return NEQL;
					if(strcmp(yytext, "<=") == 0)
						return LEQL;
					if(strcmp(yytext, ">=") == 0)
						return GEQL;
					return yytext[0];												
				}
[(){}:;,]		{	return yytext[0];												}
.				{	;																}
\n				{	;																}
%%
int yywrap() {
	return 1;
}