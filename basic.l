%{

#include <stdlib.h>
#include "basic.tab.h"
#include <string.h>
%}

%x comment

newLine		\\n
close		\]		
lineNum		^[0-9]+[[:space:]]
digit		[0-9]+
variable	[a-zA-Z]+[[:space:]]
plus		"+"
minus		"-"
multiply	"*"
divide		"/"
modulo		"%"
equal		"="
inequal		"<>"
and		"&"
or		"|"
greater		"<"
smaller		">"

negation	"!"

rem		(REM[[:space:]])

rstart		{rem}
rstop		"\n"
rinside		[0-9a-zA-Z\ .*]+

goto		(GOTO[[:space:]])

let		(LET[[:space:]])
input		(INPUT[[:space:]])
dim		(DIM[[:space:]])
as		(AS[[:space:]])
print_command	(PRINT[[:space:]])

if		(IF[[:space:]])
then		(THEN[[:space:]])
else		(ELSE[[:space:]])
elseif		(ELSEIF[[:space:]])
endif		(ENDIF[[:space:]])

dquote		\"
dinside		[^"]+
string		{dquote}{dinside}{dquote}

space		[[:space:]]+

%%
{lineNum}	{yylval.digit = atoi(yytext); return LINENUM;}
{newLine}	{printf("new line\n");yylval.string = strdup(yytext); return NEWLINE;}
{digit}		{yylval.digit = atoi(yytext); return DIGIT;}
{rstart}	{BEGIN(comment); return REM;}
{goto}{space}*	{yylval.string = strdup(yytext);  return GOTO;}

<comment>{rstop}	{BEGIN(INITIAL); return *yytext;}
<comment>{rinside}	{return COMMENT;}

{input}{space}*			{yylval.string = strdup(yytext); return INPUT;}

{divide}{space}*		{ yylval.string = strdup(yytext); return DIVIDE;}
{plus}{space}*			{ yylval.string = strdup(yytext); return PLUS;}
{minus}{space}*		 	{ yylval.string = strdup(yytext); return MINUS;}
{multiply}{space}*		{ yylval.string = strdup(yytext); return MULTI;}
{modulo}{space}*		{ yylval.string = strdup(yytext); return MODULO;}

{equal}{space}*			{ yylval.string = strdup(yytext); return EQUAL;}
{inequal}{space}*		{ yylval.string = strdup(yytext); return INEQUAL;}
{greater}{space}*		{ yylval.string = strdup(yytext); return GREATER;}
{smaller}{space}*		{ yylval.string = strdup(yytext); return SMALLER;}
{and}{space}*			{ yylval.string = strdup(yytext); return AND;}
{or}{space}*			{ yylval.string = strdup(yytext); return OR;}
{negation}{space}*		{ yylval.string = strdup(yytext); return NEGATION;}
{greater}{equal}{space}*	{ yylval.string = strdup(yytext); return GREATEQUAL;}
{smaller}{equal}{space}*	{ yylval.string = strdup(yytext); return SMALLEQUAL;}

{dim}{space}*			{ yylval.string = strdup(yytext); return DIM;}
{as}{space}*			{ yylval.string = strdup(yytext); return AS;}
{let}{space}*			{ yylval.string = strdup(yytext); return LET;}

{if}{space}*			{ yylval.string = strdup(yytext); return IF;}
{then}{space}*			{ yylval.string = strdup(yytext); return THEN;}
{else}{space}*			{ yylval.string = strdup(yytext); return ELSE;}
{endif}{space}*			{ yylval.string = strdup(yytext); return ENDIF;}
{elseif}{space}*		{ yylval.string = strdup(yytext); return ELSEIF;}

{print_command}{space}*		{ yylval.string = strdup(yytext); return PRINT;}

{variable}{space}*	{yylval.string = strdup(yytext); return VAR;}

{space}*{close}{space}*	{printf("close lex\n"); yylval.string = strdup(yytext); return CLOSE;}


{space}			{printf("space\n");yylval.string = strdup(yytext);}
{string}		{yylval.string = strdup(yytext); return STRING;}
.			{printf("default??\n"); yylval.string = strdup(yytext); return *yytext;} 		
%%

int yywrap(void)
{
	return 1;
}
