#include <stdio.h>
#include <string.h>
#include <stdlib.h>
//2:25 
#define COMMAND_AST		4
#define GOTO_AST		5
#define LET_AST1		10
#define LET_AST2		11
#define DIM_AST			15
#define PRINT_TEXT_AST		20
#define PRINT_AST		21
#define INPUT_AST		25
#define IFSTMT_AST		30
#define IFTHEN_AST		35
#define IFELSE_AST		40
#define ELSEIF_AST		45

#define EXPR_AST		50
#define DECL_AST		55

#define PLUS_AST	57
#define MINUS_AST	58
#define MODULO_AST	59
#define MULTI_AST	60
#define DIVIDE_AST	61
#define EQUAL_AST	62
#define INEQUAL_AST	63
#define NEGATION_AST	64
#define GREAT_AST	65
#define SMALLER_AST	66
#define GREATEQUAL_AST	67
#define SMALLEQUAL_AST	68
#define AND_AST	69
#define OR_AST	70

#define FALSE 0
#define TRUE 1

#define BINARY	75
#define COMPARE	76
#define UNARY	77
#define OPERAND 78
#define VARIABLE 79

//decalaration
typedef struct ast_node ast_node;
typedef struct triple triple;
typedef struct varTable_0 varTable_0;
typedef struct varTable_1 varTable_1;
typedef struct element element;
typedef struct expr_node expr_node;
typedef struct arrayElem;

void triple_sort();
void exprNodeGen(int op, char* varName, int opType, int value);
int calculator(ast_node* curAST, int count, int lineNum);
void make_triple(int line_num, char* command);
void astGen(int type, int subtype, char* commandVar, char* text);
void divideZero(int num);
int is_number(char* str);
int registerVar(char* varName, int lineNum);
void runProgram();
void initVarTable_0();
void show_document();
void show_line();
void updateVar(int index, int value);
void printExpr();



//struct
typedef struct varTable_0
{
	int used;
	char varName[30];
	int value;
	int lineNum;
}varTable_0;

typedef struct arrayElem
{
	int value;
	int index;
}arrayElem;

typedef struct varTable_1
{
	arrayElem* elem;
	int size;
	char arrayName[30];
	int lineNum;
}varTable_1;
	

typedef struct element
{
	char* varName;
	int value;
	int op;
	int opType;
	int flag;
}element;

typedef struct expr_node
{
	int count;
	element elem[100];

}expr_node;

struct ast_node
{
	int type;
	int subtype;
	int integerLine;
	char* string;
	char* varName;
	expr_node expr1;
	expr_node expr2;
	ast_node* child;
};


struct triple
{
   int num;
   ast_node* CommandAst;
   char* string;
   triple* next;
   int index;
};

//variable
varTable_0 table_0[999];
varTable_1 table_1[999];
triple** triple_table;
int triple_table_size=0;
ast_node* curAST;

expr_node* curNode1;
expr_node* curNode2;
expr_node* temp;
int nodeG = TRUE;
int nodeG2 = FALSE;


//function
void exprNodeGen(int op, char* varName, int opType, int value)
{
	if(nodeG == TRUE)
	{
		temp = (expr_node*)(malloc(sizeof(expr_node)));
		temp->count = 0;
		nodeG = FALSE;
	}

	switch(opType)
	{
	case BINARY: case COMPARE: case UNARY:
		temp->elem[temp->count].op = op;
		temp->elem[temp->count].opType = opType;
		break;
	case OPERAND:
		temp-> elem[temp->count].value = value;
		temp->elem[temp->count].opType = opType;
		break;
	case VARIABLE:
		temp->elem[temp->count].varName = strdup(varName);
		printf("checck 1\n");
		temp->elem[temp->count].opType = opType;
		break;
	default: temp->count--;
	break;
	}
	temp->elem[temp->count].flag = FALSE;
	temp->count = temp->count+1;
	if(nodeG2 == TRUE)
		curNode2 = temp;
	else
		curNode1 = temp;
}

void printExpr()
{

}

void make_triple(int line_num, char* command)
{
   triple_table_size++;
      triple* new_triple = malloc(sizeof(triple));
      new_triple->num = line_num;
      new_triple->string = command;
      triple_table = realloc(triple_table, sizeof(triple)*(triple_table_size+1));
   triple_table[triple_table_size-1] = new_triple;
   new_triple->CommandAst = curAST;
   new_triple->index = triple_table_size - 1;
   printf("\ntriple_info: %d %s\n",new_triple->num, new_triple->string);
}

void astGen(int type, int subtype, char* commandVar, char* text)
{
	curAST = (ast_node*)(malloc(sizeof(ast_node)));
	curAST->type = type;
	curAST->subtype = subtype;

	if(curAST->type == COMMAND_AST || curAST->type == IFSTMT_AST)
	{
		switch(curAST->subtype)
		{
		case GOTO_AST: curAST->integerLine = curNode1->elem[0].value;
			printf("goto ast %d\n", curAST->integerLine);
			break;
		case LET_AST1:
			curAST->varName = strdup(commandVar);
			curAST->expr1 = *curNode1;
			break;
		case LET_AST2:
			curAST->varName = strdup(commandVar);
			curAST->expr1 = *curNode1;
			curAST->expr2 = *curNode2;
			break;
		case DIM_AST :
			curAST->varName = strdup(commandVar);
			curAST->expr1 = *curNode1;
			break;
		case INPUT_AST:
			curAST->varName = strdup(commandVar);
			break;
		case IFSTMT_AST :
			curAST->expr1 = *curNode1;
			curAST->integerLine = curNode1->elem[curNode1->count-1].value;
			break;
		case PRINT_TEXT_AST : curAST->string = strdup(text);
			printf("ast comment: %s\n", curAST->string);
			break;
		case PRINT_AST : curAST->expr1 = *curNode1;
			break;
		}
	}else if(curAST->type == DECL_AST)
	{
		printf("function");
		//function
	}
	printf("check2\n");
}


int* findVar(char* varName, int lineNum)
{
	int i = 0;
	int* result = NULL;

	int name[30];
	int j = 0;
	while((varName[j] > 64 && varName[j] < 91)||(varName[j]>96 && varName[j] <123))
	{
		name[j] = varName[j];
		j++;
	}
	name[j] = '\0';
	j++;

	for( ; i< sizeof(table_0)/sizeof(varTable_0) && table_0[i].used != -1; i++)
	{
	//	int j = 0;
	//	while((table_0[i].varName[j] >= 65 && table_0[i].varName[j] <=90)
	//		&& table_0[i]varName[j] >= 97 &&  )
	//	{	printf("table: %d ", table_0[i].varName[j]);	j++;}
	//	printf("find var j : %d\n", j);
	//	table_0[i].varName[j-1] = '\0';
	//	j = 0;
	//	printf("\n");
	//	while(name[j] != '\0')
	//	{	printf("name: %d ", name[j]);	j++;}
	//	printf("find var j2: %d\n", j);
	//	name[j-1] = '\0';
		if(strcmp(table_0[i].varName, (const char*)name) ==0 && table_0[i].lineNum <= lineNum)
		{

			if(table_0[i].used == FALSE)
				return result;
			else
			{
				result = &table_0[i].value;
				return result;
			}
		}
	}
	return result;
}

void divideZero(int num)
{
	if(num == 0)
	{
		printf("runtime error - divided by zero\n");
		exit(1);
	}
	else
	{ return;}
}

int calculator(ast_node* curAST, int count, int lineNum)
{
	int operand[999] = {0};
	int result =0;
	int type = curAST->subtype;
	expr_node temp;
	int tempResult = 0;

	if(count == 0)
		temp = curAST->expr1;
	else if(count == 1)
		temp = curAST->expr2;
	else
		printf("wrong value in calculator\n");

	printf("cur AST type : %d\n", curAST->type);
	if(curAST->type == COMMAND_AST || curAST->type == IFSTMT_AST)
	{
		if(type == LET_AST1||type==DIM_AST||type==IFSTMT_AST||type==PRINT_AST||type==LET_AST2)
		{
			int i = 0;
			int oprCount = 0;
			int j = 0;

			if(temp.count ==1)
			{
				if(temp.elem[0].opType == OPERAND)
				{
					printf("count = 1, LET value : %d\n", temp.elem[0].value);
					return temp.elem[0].value;
				}else if(temp.elem[0].opType == VARIABLE)
				{
					printf("find var %s\n", table_0[0].varName);
					int* varValue = findVar(temp.elem[0].varName, lineNum);
					if(varValue == NULL)
					{
						printf("no matching variable -cmpile error 2\n");
						exit(1);
					}else
						return *varValue;
				}
			}
			for(i = 0;i < temp.count; i++)
			{


				if(temp.elem[i].opType == OPERAND)
				{
					operand[oprCount] = temp.elem[i].value;
					oprCount++;
				}else if(temp.elem[i].opType == VARIABLE)
				{
					int* varValue = findVar(temp.elem[i].varName, lineNum);
					printf("return success\n");
					if(varValue == NULL)
					{
						printf("no matching value - compile error 1\n");
						exit(1);
					}
					operand[oprCount] = *varValue;
					oprCount++;
				}else if(temp.elem[i].opType == BINARY)
				{
					printf("opr1: %d, opr2: %d\n", operand[oprCount-2], operand[oprCount-1]);

					switch(temp.elem[i].op)
					{
						case PLUS_AST: tempResult = operand[oprCount-2] + operand[oprCount-1];
							printf("cal plus %d, %d\n", operand[oprCount-2], operand[oprCount-1]);

							break;
						case MINUS_AST:tempResult = operand[oprCount-2] - operand[oprCount-1];
							break;
						case MODULO_AST:tempResult = operand[oprCount-2] - operand[oprCount-1];
							break;
						case MULTI_AST:tempResult = operand[oprCount-2] * operand[oprCount-1];
							break;
						case DIVIDE_AST: divideZero(operand[oprCount-1]);
								tempResult = operand[oprCount-2] / operand[oprCount-1];
								break;
						case EQUAL_AST: if(operand[oprCount-2] == operand[oprCount-1])
									tempResult = 1;
								else
									tempResult = 0;
								break;
						case INEQUAL_AST: if(operand[oprCount-2] != operand[oprCount-1])
									tempResult = 1;
								  else
									tempResult = 0;
								break;
						case GREAT_AST: if(operand[oprCount-2] > operand[oprCount-1])
									tempResult = 1;
								else
									tempResult = 0;
								break;
						case SMALLER_AST: if(operand[oprCount-2] < operand[oprCount-2])
									tempResult = 1;
								  else
									tempResult = 0;
								break;
						case GREATEQUAL_AST: if(operand[oprCount-2] >= operand[oprCount-2])
									tempResult = 1;
								     else
									tempResult = 0;
								     break;
						case SMALLEQUAL_AST:if(operand[oprCount-2] <= operand[oprCount-2])
									tempResult = 1;
								    else
									tempResult = 0;
								break;
						default:
							printf("worng check 8\n");
					}
					oprCount = oprCount-2;
					operand[oprCount] = tempResult;
					oprCount++;

				}else if(temp.elem[i].opType == UNARY)
				{
					if(temp.elem[i].opType == NEGATION_AST)
					{
						tempResult = 1 - operand[oprCount-1];
					}
					else if(temp.elem[i].opType == MINUS_AST)
					{
						tempResult = -operand[oprCount-1];
					}
					oprCount--;
					operand[oprCount] = tempResult;
				}else
					printf("wrong calculator check 4.5\n");

			}
		}
		else
		{
			printf("wrong check 5 \n");
		}
	}else
	{
		printf("check calculator2\n");
	}
	printf("checkc\n");
	result = operand[0];
	return result;
}


FILE* open_file(char* filename)
{
	FILE* fp = NULL;

	if(strlen(filename) <= 1)
	{
		printf("invalid input file name\n");
		return NULL;
	}
	char filename_extention[128];
	strcat(filename_extention,filename);
	strcat(filename_extention,".bas");

	fp = fopen(filename_extention, "r");
	if(!fp)
	{
		printf("Error opening %s\n", filename_extention);
		return NULL;
	}
	return fp;

}

int is_number(char* str){
	int i;
	for(i=0;i<strlen(str);i++){
		if(isdigit(str[i]))
		{

		}
		else
			return 0;
	}
	return 1;
}


void show_document()
{
	int i;
	if(triple_table_size>0)
	{
		triple_sort();
		/*for(i=0;i<triple_table_size;i++)
		{
			triple_table[i]->num=(i+1)*LINEINTERVAL;

		}*/
		for(i=0;i<triple_table_size;i++)
		{
			printf("%d %s\n",triple_table[i]->num, triple_table[i]->string);
		}
	}
}

void show_line(int request_num)
{
	int i;
	int find = 0;
	if(triple_table_size>0)
	{
		for(i=0;i<triple_table_size;i++)
		{
			if(request_num==triple_table[i]->num)
			{
				printf("%d %s\n",request_num, triple_table[i]->string);
				find = 1;
			}
		}
	}
	if(find == 0)
		printf("Error: no matching line\n");

}

void triple_sort()
{
	int i,j,k;

	triple * temp;
	for(i=0;i<triple_table_size;i++)
	{
		for(j=i;j<triple_table_size;j++)
		{
			if(triple_table[i]->num > triple_table[j]->num)
			{
				temp = triple_table[i];
				triple_table[i] = triple_table[j];
				triple_table[j] = temp;
			}
		}
	}
}

int registerVar(char* varName, int lineNum)
{
	int i = 0;
	int* result = NULL;
	int name[30];
	int j = 0;
	while((varName[j] > 64 && varName[j] < 91)||(varName[j]>96 && varName[j] <123))
	{
		name[j] = varName[j];
		j++;
	}
	name[j] = '\0';
	j++;

	for( ; i< sizeof(table_0)/sizeof(varTable_0) && table_0[i].used != -1 ; i++)
	{
	//	while(varName[j] != '\0')
	//	{	printf("reg name: %d ", varName[j]);	j++;}
	//	table_0[i].varName[strlen(table_0[i].varName)-1] = '\0';
	//	varName[strlen(varName)-1] = '\0';
		if(strcmp(table_0[i].varName, (const char*)name) ==0 && table_0[i].lineNum <= lineNum)
		{
			return i;
		}
	}
	strcpy(table_0[i].varName, (const char*)name);
	printf("count var %d\n", i);
	int k = 0;
	while(k<j)
		printf("reg check %d\n", table_0[i].varName[k++]);
	table_0[i].lineNum = lineNum;
	table_0[i].value = 0;
	table_0[i].used = FALSE;
	return i;

}

void updateVar(int index, int value)
{
	table_0[index].value = value;
	table_0[index].used = TRUE;
	printf("update var name :%s, value: %d\n", table_0[index].varName, table_0[index].value);
}

void initVarTable_0()
{
	int i = 0;
	for(;i<999;i++)
	{
		table_0[i].used = -1;
		table_0[i].value = 0;
		memset(table_0[i].varName, 0, 30);
	}
}


void runProgram()
{
	int tableSize = triple_table_size;
	int i = 0;
	printf("table size %d\n", tableSize);
	int result;
	ast_node* temp_ast;
	int varTableIdx = 0;
	while( i < tableSize)
	{
		temp_ast = triple_table[i]->CommandAst;
		int type = temp_ast->type;
		int subtype = temp_ast->subtype;
		if(type == COMMAND_AST)
		{
			switch(subtype)
			{
			case GOTO_AST:

			break;
			case LET_AST1:
				varTableIdx = registerVar(temp_ast->varName, triple_table[i]->num);
				result = calculator(temp_ast, 0, triple_table[i]->num);
			//	printf("result let : %d  %d\n ", result, triple_table[i]->num);
				updateVar(varTableIdx, result);
			//	printf("variable reg  %s\n", table_0[0].varName);
				break;
			case LET_AST2:

			break;
			case DIM_AST:

			break;
			case INPUT_AST: varTableIdx = registerVar(temp_ast->varName, triple_table[i]->num);
					printf("intput");
					scanf("%d", &result);
					updateVar(varTableIdx, result);
			break;
			case PRINT_TEXT_AST:
				printf("\n\n%s\n\n", temp_ast->string);
			break;
			case PRINT_AST:
				result = calculator(temp_ast, 0, triple_table[i]->num);
				printf("\n\n%d\n\n", result);
			break;
			}
		}else if(type == IFSTMT_AST)
		{



		}else if(type == DECL_AST)
		{

		}else
		{
			printf("run programm error1 %d\n", type);
			return;
		}
		i++;
	}
}
int line_checker(int linenum)
{
	int i;
	int find = 0;
	if(triple_table_size>0)
	{
		for(i=0;i<triple_table_size;i++)
		{
			if(linenum==triple_table[i]->num)
			{
				find = 1;
			}
		}
	}

	if(find == 0)
	{
		printf("RUNTIME Error: undefined line number\n");
		return -1;
	}
	else return 1;
}

int integer_checker(char* num_str)
{
	int num = atoi(num_str);
	if(num<-2147483648||num>2147483647)
	{
		printf("RUNTIME Error: integer overflow\n");
		return -1;
	}
	else return 1;
}
