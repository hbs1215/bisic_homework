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
%token <string> ENDIF
%token <string> ELSEIF
%token <string> ELSE

%token <digit> DIGIT
%token <string> STRING

%token <string> CLOSE

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

line	: LINENUM command 			{printf("line: %d %s",$1,$2);make_triple($1,$2);}
	| NEWLINE				
	;

command : REM COMMENT				{sprintf($$,"%s%s",$1,$2);printf("This is d comment\n");}	
	| GOTO INTEGER				{sprintf($$,"%s%d",$1,$2); nodeG = TRUE; printf("goto statement number: %d\n", $2); astGen(COMMAND_AST, GOTO_AST, NULL, NULL); printf("check3\n");}
	| LET VAR EQUAL expr			{sprintf($$,"%s%s%s%s",$1,$2,$3,$4);printf("this is assginment statemnt"); nodeG = TRUE; nodeG2 = FALSE; astGen(COMMAND_AST, LET_AST1, $2, NULL); printf("let var: %s\n", $2);}
	| LET VAR '[' expr bracket EQUAL expr	{sprintf($$,"%s %s [%s] %s %s",$1,$2,$4,$6,$7);printf("2d array statment\n"); nodeG2 = FALSE; astGen(COMMAND_AST, LET_AST2, $2, NULL); printf("let 2 var: %s\n", $2);}
	| DIM VAR AS '[' expr bracket		{sprintf($$,"DIM %s %s [%s]",$2,$3,$5); nodeG = TRUE; astGen(COMMAND_AST, DIM_AST, $2, NULL); printf("dim %s\n", $2);}
	| PRINT expr				{sprintf($$,"%s%s",$1,$2); nodeG = TRUE;  astGen(COMMAND_AST, PRINT_AST, NULL, NULL);}
	| PRINT text				{sprintf($$,"%s %s",$1,$2); nodeG = TRUE; astGen(COMMAND_AST, PRINT_TEXT_AST, NULL, $2);}
	| INPUT VAR				{sprintf($$,"%s %s",$1,$2); nodeG = TRUE; astGen(COMMAND_AST, INPUT_AST, $2, NULL); printf("input var %s\n", $2);}
	| ifstmt				{strcpy($$,$1); nodeG = TRUE; astGen(IFSTMT_AST,IFTHEN_AST, NULL, NULL); }
	;

ifstmt	: IF expr THEN INTEGER		{sprintf($$,"%s %s %s %d",$1,$2,$3,$4);printf("if then \n");}
	;
text	: STRING			{strcpy($$,$1);printf("stirng bison\n");}
	;

expr	: expr GREATER expr2		{sprintf($$,"%s %s %s",$1,$2,$3);printf(" > bison \n");}
	| expr SMALLER expr2		{sprintf($$,"%s %s %s",$1,$2,$3);printf(" < bison \n");}
	| expr GREATEQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3);printf(" >= bison \n");}
	| expr SMALLEQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3);printf(" <= bison \n");}
	| expr EQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3);printf(" = bison \n");}
	| expr INEQUAL expr2		{sprintf($$,"%s %s %s",$1,$2,$3);printf(" <> bison \n");}
	| expr OR expr2			{sprintf($$,"%s %s %s",$1,$2,$3);printf("or\n");}
	| expr AND expr2		{sprintf($$,"%s %s %s",$1,$2,$3);printf("and\n");}
	| expr2				
	;


expr2	: expr2 PLUS term		{sprintf($$,"%s%s%s",$1,$2,$3);printf("expr2+ %s, %s, %s\n",$1,$2,$3);
					exprNodeGen(PLUS_AST, NULL, BINARY, 0); printf("node2 : %d\n", nodeG2);}
	| expr2 MINUS term 		{sprintf($$,"%s %s %s",$1,$2,$3);printf("-\n");
					exprNodeGen(MINUS_AST, NULL, BINARY, 0);}
	| term				
	;

term	: term DIVIDE factor		{printf("before sprintf $$: %s, $3 factor: %s\n", $$, $3); sprintf($$,"%s %s %s",$1,$2, $3); printf("divide $1 : %s\n", $1); printf("-- / %s -- \n", $$);
					exprNodeGen(DIVIDE_AST, NULL, BINARY, 0);}
	| term MULTI  factor		{sprintf($$,"%s %s %s",$1,$2,$3);printf("*\n");
					exprNodeGen(MULTI_AST, NULL, BINARY, 0);}
	| term MODULO factor		{sprintf($$,"%s %s %s",$1, $2, $3);printf("modulo\n");
					exprNodeGen(MODULO_AST, NULL, BINARY, 0);}
	| factor			
	;

factor	: '(' expr ')'			{sprintf($$,"(%s)",$2);printf("parenthesis\n");}
	
	| MINUS factor			{sprintf($$,"%s %s",$1,$2);printf("unary minus\n");
					exprNodeGen(MINUS_AST, NULL, UNARY, 0); }
	| NEGATION factor		{sprintf($$,"%s %s",$1,$2);printf("unary negation\n");
					exprNodeGen(NEGATION_AST, NULL, UNARY, 0);}
	| VAR				{sprintf($$,"%s",$1);printf("variable factor %s\n", $1);
					exprNodeGen(0, $1, VARIABLE, 0); printf("name %s", $1);}
	| INTEGER			{char str[20]; int i = $1; printf("ddd %d\n", i); sprintf(str, "%d", i), $$ = strdup(str); printf("dd %d\n", $1); printf("factor int %s\n", $$);}	
	;

INTEGER : INTEGER DIGIT			{printf("integer %d\n", $1);}
	| DIGIT				{printf("digit: %d\n", $1); exprNodeGen(0, NULL, OPERAND, $1);}
	;

bracket : CLOSE				{sprintf($$,"%s",$1);printf("close\n"); nodeG = TRUE; nodeG2 = TRUE; printf("hi\n");}
	;
%%


int yyerror(char const* s)
{
	fprintf(stderr, "%s\n", s);
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


