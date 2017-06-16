%{
#include <ctype.h>
#include <stdio.h>
#include "ast.h"
extern FILE * yyin;
%}

%union
{
 	int digit;
	char* string;
}

%token <string> NEWLINE
%token <string> COMMENT
%token <digit> LINENUM
%token <string> REM
%token <string> GOTO
%token <string> LET

%token <stirng> DIM
%token <string> AS

%token <string> PRINT
%token <string> INPUT

%token <string> VAR

%token <string> PLUS
%token <string> MINUS
%token <string> MODULO
%token <string> MULTI
%token <string> DIVIDE
%token <string> GREATER
%token <string> SMALLER
%token <string> GREATEQUAL
%token <string> SMALLEQUAL
%token <string> INEQUAL
%token <string> EQUAL
%token <string> AND
%token <string> OR

%token <string> NEGATION

%token <string> IF
%token <string> THEN
%token <string> ELSEIF
%token <string> ELSE
%token <string> ENDIF

%token <digit> DIGIT
%token <string> STRING

%token <string> CLOSE

%token <string> END
%token <string> WHILE

%type <digit> INTEGER
%type <string> text

%type <string> command

%type <string> expr2
%type <string> expr
%type <string> factor
%type <string> term
%type <string> ifstmt
%type <string> bracket

%%

program	: line							
	| line program			
	; 

line	: LINENUM command 			{make_triple($1,$2);}
	| NEWLINE				
	;

command : REM COMMENT				{sprintf($$,"%s%s",$1,$2);}	
	| GOTO INTEGER				{sprintf($$,"%s%d",$1,$2); nodeG = TRUE;  astGen(COMMAND_AST, GOTO_AST, NULL, NULL); }
	| LET VAR EQUAL expr			{sprintf($$,"%s%s%s%s",$1,$2,$3,$4); nodeG = TRUE; nodeG2 = FALSE; astGen(COMMAND_AST, LET_AST1, $2, NULL); }
	| LET VAR '[' expr bracket EQUAL expr	{sprintf($$,"%s %s [%s] %s %s",$1,$2,$4,$6,$7);nodeG2 = FALSE; astGen(COMMAND_AST, LET_AST2, $2, NULL);}
	| DIM VAR AS '[' expr bracket		{sprintf($$,"DIM %s %s [%s]",$2,$3,$5); nodeG = TRUE; nodeG2 = FALSE; astGen(COMMAND_AST, DIM_AST, $2, NULL);}
	| PRINT expr				{sprintf($$,"%s%s",$1,$2); nodeG = TRUE;  astGen(COMMAND_AST, PRINT_AST, NULL, NULL);}
	| PRINT text				{sprintf($$,"%s %s",$1,$2); nodeG = TRUE; astGen(COMMAND_AST, PRINT_TEXT_AST, NULL, $2);}
	| INPUT VAR				{sprintf($$,"%s %s",$1,$2); nodeG = TRUE; astGen(COMMAND_AST, INPUT_AST, $2, NULL); }
	| ifstmt				{strcpy($$,$1); nodeG = TRUE;}
	| WHILE IF expr				{sprintf($$,"%s %s %s",$1,$2, $3);nodeG = TRUE; astGen(WHILE_AST,WHILE_IF_AST, NULL, NULL); }
	| END WHILE				{sprintf($$,"%s %s",$1,$2);nodeG = TRUE; astGen(WHILE_AST,END_WHILE, NULL, NULL); }
	;

ifstmt	: IF expr THEN INTEGER		{astGen(IFSTMT_AST, IFTHEN_AST, NULL, NULL); nodeG = TRUE; sprintf($$,"%s %s %s %d",$1,$2,$3,$4);}
	| ELSEIF expr THEN INTEGER	{astGen(IFSTMT_AST, IFELSE_AST, NULL, NULL); nodeG = TRUE;  sprintf($$,"%s %s %s %d",$1,$2,$3,$4);}
	| ELSE THEN INTEGER		{astGen(IFSTMT_AST, ELSE_AST, NULL, NULL); nodeG = TRUE; sprintf($$,"%s %s %d",$1,$2,$3);}
	| ENDIF				{sprintf($$,"%s",$1);}
	;	
	
text	: STRING			{strcpy($$,$1);}
	;

expr	: expr GREATER expr2		{sprintf($$,"%s %s %s",$1,$2,$3); exprNodeGen(GREAT_AST, NULL, BINARY, 0);}
	| expr SMALLER expr2		{sprintf($$,"%s %s %s",$1,$2,$3); exprNodeGen(SMALLER_AST, NULL, BINARY, 0);}
	| expr GREATEQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3);exprNodeGen(GREATEQUAL_AST, NULL, BINARY, 0);}
	| expr SMALLEQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3);exprNodeGen(SMALLEQUAL_AST, NULL, BINARY, 0);}
	| expr EQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3); exprNodeGen(EQUAL_AST, NULL, BINARY, 0);}
	| expr INEQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3); exprNodeGen(INEQUAL_AST, NULL, BINARY, 0);}
	| expr OR expr2			{sprintf($$,"%s %s %s",$1,$2,$3);exprNodeGen(OR_AST, NULL, BINARY, 0);}
	| expr AND expr2		{sprintf($$,"%s %s %s",$1,$2,$3); exprNodeGen(AND_AST, NULL, BINARY, 0);}
	| expr2				
	;


expr2	: expr2 PLUS term		{sprintf($$,"%s%s%s",$1,$2,$3);	exprNodeGen(PLUS_AST, NULL, BINARY, 0); }
	| expr2 MINUS term 		{sprintf($$,"%s %s %s",$1,$2,$3);	exprNodeGen(MINUS_AST, NULL, BINARY, 0);}
	| term				
	;

term	: term DIVIDE factor		{sprintf($$,"%s %s %s",$1,$2, $3); exprNodeGen(DIVIDE_AST, NULL, BINARY, 0);}
	| term MULTI  factor		{sprintf($$,"%s %s %s",$1,$2,$3); exprNodeGen(MULTI_AST, NULL, BINARY, 0);}
	| term MODULO factor		{sprintf($$,"%s %s %s",$1, $2, $3);	exprNodeGen(MODULO_AST, NULL, BINARY, 0);}
	| factor			
	;

factor	: '(' expr ')'			{sprintf($$,"(%s)",$2);}
	
	| MINUS factor			{sprintf($$,"%s %s",$1,$2); exprNodeGen(MINUS_AST, NULL, UNARY, 0); }
	| NEGATION factor		{sprintf($$,"%s %s",$1,$2);	exprNodeGen(NEGATION_AST, NULL, UNARY, 0);}
	| VAR				{sprintf($$,"%s",$1);	exprNodeGen(0, $1, VARIABLE, 0); }
	| INTEGER			{char str[20]; int i = $1; sprintf(str, "%d", i), $$ = strdup(str); }
	;

INTEGER : INTEGER DIGIT			
	| DIGIT				{exprNodeGen(0, NULL, OPERAND, $1);}
	;

bracket : CLOSE				{sprintf($$,"%s",$1);nodeG = TRUE; nodeG2 = TRUE; }
	;
%%


int yyerror(char const* s)
{
	fprintf(stderr, "%s\n", s);
	exit(1);
}


int main(int argc, char** argv)
{
	char filename[128];
	strcpy(filename,argv[1]);
	strcat(filename,".bas");
	yyin = fopen(filename, "r");
	//open_file(argv[1]);
	
	if(!yyin)
	{
		printf("Error: opening file %s\n", argv[1]);
		return 0;
	}	
	else
	{
		printf("Open \"%s\"\n",filename);
	}
	yyparse();
	
	char user_input[20];
	int line;
	
	while(1){
		fflush(stdin); 
		user_input[0] = '\0';
		
		printf(">");
		scanf("%s",user_input);
		
		if(!strcmp(user_input,"LIST") )
		{
			show_document();
		}
		else if(is_number(user_input))
		{
			show_line(atoi(user_input));
		}
		else if(!strcmp(user_input,"RUN") ){
			initVarTable_0();
			initVarTable_1();
			
			runProgram();	
		}
		else if(!strcmp(user_input,"QUIT") )
		{
			break;
		}
		else
		{	
			printf("\nError:invalid command\n");
			printf("==comands==\n");
			printf("LIST\n");
			printf("#number of line\n");
			printf("RUN\n");
			printf("QUIT\n");
			printf("==========\n");
		}
	}
	
	fclose(yyin);
	return 0;
}


