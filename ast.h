#include <stdio.h>
#include <string.h>
#include <stdlib.h>
//5:25
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
#define ELSE_AST		45

#define WHILE_AST		46
#define WHILE_IF_AST	47
#define END_WHILE		48

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
#define ARRAYELEM 80

//decalaration
typedef struct ast_node ast_node;
typedef struct triple triple;
typedef struct varTable_0 varTable_0;
typedef struct varTable_1 varTable_1;
typedef struct element element;
typedef struct expr_node expr_node;
typedef struct arrayElem arrayElem;
typedef struct loop_range loop_range;
typedef struct whileVar whileVar;

int table_0_count = 0;
int local_count = 0;

void arrayBoundCheck(int tableIndex, int arrayIndex);
void triple_sort();
void exprNodeGen(int op, char* varName, int opType, int value);
int calculator(ast_node* curAST, int count, int lineNum, int whileFlag);
void make_triple(int line_num, char* command);
void astGen(int type, int subtype, char* commandVar, char* text);
void divideZero(int num);
int is_number(char* str);
int registerVar(char* varName, int lineNum, int dim, int whileFlag);
int registerLocalVar(char* varName, int lineNum, int input);
void runProgram();
void initVarTable_0();
void show_document();
void show_line();
void updateVar(int index, int value);
void updateLocalVar(int index, int value);
void updateArray(int tableIndex, int arrayIndex, int value, int whileFlag);
void printExpr();
void initVarTable_1();
void initWhileVar();
int findArrayName(char* varName, int lineNum);
int* findVar(char* varName, int lineNum);
int* findLocalVar(char* varName, int lineNum);
int integer_checker(int num);
int line_checker(int linenum, int flag);
void integer_overflow_check(int a, int b, int c);

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
	int used;

}varTable_1;

typedef struct whileVar
{
	int used;
	char varName[30];
	int value;
	int lineNum;
	int whileStart;
	int whileEnd;
}whileVar;

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

struct loop_range
{
   int start_num;
   int end_num;
   int match;
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
whileVar table_local[100];
triple** triple_table;
int triple_table_size=0;
ast_node* curAST;

expr_node* curNode1;
expr_node* curNode2;
expr_node* temp;
int nodeG = TRUE;
int nodeG2 = FALSE;

loop_range** loop_range_table;
int loop_range_size=0;

void initWhileVar()
{
	int i = 0;
	for(;i<100;i++)
	{
		if(table_local[i].used = -1)
		{
			memset(table_local[i].varName, 0, 30);
		}
		table_local[i].used = -1;
		table_local[i].value = 0;


	 	table_local[i].whileStart = 0;
		table_local[i].whileEnd = 0;
	}
}

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
		//printf("checck 1\n");
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
	new_triple->CommandAst = curAST;
	new_triple->index = triple_table_size - 1;
	if(new_triple->CommandAst->type==WHILE_AST)
	{
		if(new_triple->CommandAst->subtype==WHILE_IF_AST)
		{
			loop_range_size++;
			loop_range* new_loop_range = malloc(sizeof(loop_range));
			new_loop_range->start_num = line_num;
			new_loop_range->match = 0;
			loop_range_table = realloc(loop_range_table, sizeof(loop_range)*(loop_range_size+1));
			loop_range_table[loop_range_size-1] = new_loop_range;
		}
		else if(new_triple->CommandAst->subtype==END_WHILE)
		{
			if(loop_range_size<1||loop_range_table[loop_range_size-1]->match!=0)
			{
				return;
			}
			else if(loop_range_table[loop_range_size-1]->match==0)
			{
				loop_range_table[loop_range_size-1]->match = 1;
				loop_range_table[loop_range_size-1]->end_num = line_num;
			}
		}
	}

	triple_table[triple_table_size-1] = new_triple;
}

void astGen(int type, int subtype, char* commandVar, char* text)
{
	curAST = (ast_node*)(malloc(sizeof(ast_node)));
	curAST->type = type;
	curAST->subtype = subtype;

	if(curAST->type == COMMAND_AST || curAST->type == IFSTMT_AST || curAST->type == WHILE_AST)
	{
		switch(curAST->subtype)
		{
		case GOTO_AST: curAST->integerLine = curNode1->elem[0].value;
			//printf("goto ast %d\n", curAST->integerLine);
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
		case IFTHEN_AST :
			curAST->expr1 = *curNode1;
			curAST->integerLine = curNode1->elem[curNode1->count-1].value;
			break;
		case IFELSE_AST :
			curAST->expr1 = *curNode1;
			curAST->integerLine = curNode1->elem[curNode1->count-1].value;
			break;
		case ELSE_AST :
			curAST->integerLine = curNode1->elem[curNode1->count-1].value;
			break;
		case PRINT_TEXT_AST : curAST->string = strdup(text);
			//printf("ast comment: %s\n", curAST->string);
			break;
		case PRINT_AST : curAST->expr1 = *curNode1;
			break;
		case WHILE_IF_AST : curAST->expr1 = *curNode1;
			break;
		case END_WHILE :
			break;

		}
	}else
	{
		//printf("ast gen check: 4");
		//function
	}
	//printf("check2\n");
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


int* findLocalVar(char* varName, int lineNum)
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


	for( ; i< 100 && table_local[i].used != -1; i++)
	{

		if(strcmp(table_local[i].varName, (const char*)name) ==0 && table_local[i].lineNum <= lineNum)
		{

			if(table_local[i].used == FALSE)
				return result;
			else
			{
				result = &table_local[i].value;
				return result;
			}
		}
	}
	return result;

}

int findArrayName(char* varName, int lineNum)
{
	int i = 0;

	int name[30];
	int j = 0;
	while((varName[j] > 64 && varName[j] < 91)||(varName[j]>96 && varName[j] <123))
	{
		name[j] = varName[j];
		j++;
	}
	name[j] = '\0';
	j++;


	for(i=0 ; i< 999 && table_1[i].used != -1; i++)
	{
		if(strcmp(table_1[i].arrayName, (const char*)name) ==0 && table_1[i].lineNum <= lineNum)
		{
			return i;
		}
	}
	return -1;

}


void divideZero(int num)
{
	if(num == 0)
	{
		printf("Error: divided by zero\n");
		exit(1);
	}
	else
	{ return;}
}

int calculator(ast_node* curAST, int count, int lineNum, int whileFlag)
{
	//printf("check 21\n");
	int operand[999] = {0};
	int result =0;
	int type = curAST->subtype;
	expr_node temp;
	int tempResult = 0;
	int arrayExist = 0;
	if(count == 0)
		temp = curAST->expr1;
	else if(count == 1)
		temp = curAST->expr2;
	else
		printf("Error: wrong value");

	//printf("cur AST type : %d\n", curAST->type);
	if(curAST->type == COMMAND_AST || curAST->type == IFSTMT_AST || curAST->type ==  WHILE_AST)
	{
		if(type == LET_AST1||type==DIM_AST||type==IFTHEN_AST || type == IFELSE_AST
			||type==PRINT_AST||type==LET_AST2|| type == GOTO_AST || type == WHILE_IF_AST)
		{
			int i = 0;
			int oprCount = 0;
			int j = 0;
			//printf("count :%d\n", temp.count);
			if(temp.count ==1)
			{
				if(temp.elem[0].opType == OPERAND)
				{
					//printf("count = %d, LET value : %d\n", count, temp.elem[0].value);
					integer_checker(temp.elem[0].value);
					return temp.elem[0].value;
				}else if(temp.elem[0].opType == VARIABLE)
				{
			//		printf("find var %s\n", table_0[0].varName);

					int* varValue = findVar(temp.elem[0].varName, lineNum);
			//		printf("%s= ", temp.elem[0].varName);

					if(varValue == NULL)
					{

						varValue = findLocalVar(temp.elem[0].varName, lineNum);
						if(varValue == NULL)
						{
							printf("Error: no matching variable %s %dn", temp.elem[0].varName, whileFlag);
							exit(1);
						}

					}else{
						integer_checker(*varValue);
						return *varValue;
					}

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
					int* varValue = NULL;
					varValue = findVar(temp.elem[i].varName, lineNum);
					if(varValue == NULL)
						varValue = findLocalVar(temp.elem[i].varName, lineNum);

					if(varValue == NULL)
					{
						printf("no matching value");
						exit(1);
					}
					operand[oprCount] = *varValue;
					oprCount++;
				}else if(temp.elem[i].opType == BINARY)
				{
					switch(temp.elem[i].op)
					{
						case PLUS_AST:
							integer_overflow_check(operand[oprCount-2], operand[oprCount-1],0);
							tempResult = operand[oprCount-2] + operand[oprCount-1];
							break;
						case MINUS_AST:
							integer_overflow_check(operand[oprCount-2], operand[oprCount-1],1);
							tempResult = operand[oprCount-2] - operand[oprCount-1];
							break;
						case MODULO_AST:tempResult = operand[oprCount-2] % operand[oprCount-1];
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
						case INEQUAL_AST: if(operand[oprCount-2] != operand[oprCount-1]){
									tempResult = 1;}
								  else{
									tempResult = 0;}
								break;
						case GREAT_AST: if(operand[oprCount-2] > operand[oprCount-1]){
									tempResult = 1;}
								else{
									tempResult = 0;
								}
								break;
						case SMALLER_AST: if(operand[oprCount-2] < operand[oprCount-1]){
									tempResult = 1;}
								  else{
									tempResult = 0;}
								break;
						case GREATEQUAL_AST: if(operand[oprCount-2] >= operand[oprCount-1])
									tempResult = 1;
								     else
									tempResult = 0;
								     break;
						case SMALLEQUAL_AST:if(operand[oprCount-2] <= operand[oprCount-1])
									tempResult = 1;
								    else
									tempResult = 0;
								break;
						default:;
					}
					oprCount = oprCount-2;
					operand[oprCount] = tempResult;
			//		printf("tempResult : %d", tempResult);
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
		printf("Error\n");
	}

	//printf("checkc\n");
	result = operand[0];
	integer_checker(result);
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
	{
		printf("Error: no matching line\n");
	}

}

void triple_sort()
{
	int i,j,k;
	int temp_index;
	triple * temp;
	for(i=0;i<triple_table_size;i++)
	{
		for(j=i;j<triple_table_size;j++)
		{
			if(triple_table[i]->num > triple_table[j]->num)
			{
				temp = triple_table[i];
				temp_index = triple_table[i]->index;
				triple_table[i]->index = triple_table[j]->index;
				triple_table[i] = triple_table[j];
				triple_table[j] = temp;
				triple_table[j]->index = temp_index;
			}
		}
	}
}


//variable table index return
int registerVar(char* varName, int lineNum, int dim, int whileFlag)
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
	if(whileFlag == FALSE)
	{
			//dim == 2 -> table 0, 1 both should be checked
		if(dim == 1)
		{
			//printf("check pt8\n");
			for( ; i< sizeof(table_0)/sizeof(varTable_0) && table_0[i].used != -1 ; i++)
			{
				if(strcmp(table_0[i].varName, (const char*)name) ==0 && table_0[i].lineNum <= lineNum)
				{
					return i;
				}
			}
			strcpy(table_0[i].varName, (const char*)name);
		//	printf("count var %d\n", i);
			int k = 0;
		//	while(k<j)
			//	printf("reg check %d\n", table_0[i].varName[k++]);
			table_0[i].lineNum = lineNum;
			table_0[i].value = 0;
			table_0[i].used = FALSE;
			table_0_count++;
			return i;
		}
		else if(dim == 2)
		{
			for(i=0 ; i< 999 && table_0[i].used != -1 ; i++)
			{
				if(strcmp(table_0[i].varName, (const char*)name) ==0 && table_0[i].lineNum <= lineNum)
				{
					return -1;
				}
			}

			for(i = 0; i<999 && table_1[i].used != -1; i++)
			{
				if(strcmp(table_1[i].arrayName, (const char*)name) ==0 && table_1[i].lineNum <= lineNum)
				{
					return -1;
				}
			}

			strcpy(table_1[i].arrayName, (const char*)name);
			table_1[i].lineNum = lineNum;
			table_1[i].used = FALSE;

			return i;
		}else
		{
			printf("Error: variable caanot be declared\n");
			exit(1);
		}
	}else if(whileFlag == TRUE)
	{
		if(dim == 1)
		{
			for( ; i< table_0_count && table_0[i].used != -1 && strlen(table_0[i].varName) > 0; i++)
			{

				if(strcmp(table_0[i].varName, (const char*)name) ==0 && table_0[i].lineNum <= lineNum)
				{
					return i;
				}else
				{
					;
				}
			}
		//	printf("check pt11\n");
			return -1;
		}
		else
		{
			printf("Error: variable caanot be declared\n");
			exit(1);
		}
	}
}

int new = 0;

int registerLocalVar(char* varName, int lineNum, int input)
{
//	printf("local var\n");
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
	i = 0;

	for( ; i< 100 && i < local_count ; i++)
	{
		if(strcmp(table_local[i].varName, (const char*)name) ==0 && table_local[i].lineNum <= lineNum)
		{
			return i;
		}
	}

	if(input == TRUE)
	{
		return -1;
	}
	strcpy(table_local[i].varName, (const char*)name);

	table_local[i].lineNum = lineNum;
	table_local[i].value = 0;
	table_local[i].used = FALSE;
	local_count++;
	new = 1;
	return i;
}

void updateVar(int index, int value)
{
	integer_checker(value);
	table_0[index].value = value;
	table_0[index].used = TRUE;
}

void updateLocalVar(int index, int value)
{
	integer_checker(value);
	table_local[index].value = value;
	table_local[index].used = TRUE;
}

void arrayBoundCheck(int tableIndex, int arrayIndex)
{
	if(arrayIndex >= table_1[tableIndex].size)
	{
		printf("size: %d, arrayIndex: %d\n", arrayIndex, table_1[tableIndex].size);
		printf("Error: index out of range\n");
		exit(1);
	}
}

void updateArray(int tableIndex, int arrayIndex, int value, int whileFlag)
{
	arrayBoundCheck(tableIndex, arrayIndex);
	table_1[tableIndex].elem[arrayIndex].value = value;
	table_1[tableIndex].elem[arrayIndex].index = arrayIndex;
	table_1[tableIndex].used = TRUE;
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


void initVarTable_1()
{
	int i = 0;
	for(;i<999;i++)
	{
		table_1[i].used = -1;
		table_1[i].size = 0;
		if(table_1[i].elem != NULL)
			free(table_1[i].elem);
	}
}

void runProgram()
{

	int localVar = FALSE;
	//initWhileVar();
	int whileFlag = FALSE;
	int whileStart = 0;
	int whileEnd = 0;
	int tableSize = triple_table_size;
//	printf("table size %d\n", tableSize);
	int i = 0;
	int result;
	ast_node* temp_ast;
	int varTableIdx = 0;
	int ifFlag = FALSE;
	int prevTrueFlag = FALSE;
	while( i < tableSize)
	{
	//	printf("main variable in run : %d\n", table_0[0].value);
		temp_ast = triple_table[i]->CommandAst;
		int type = temp_ast->type;
		int subtype = temp_ast->subtype;
		if(type == COMMAND_AST)
		{
			switch(subtype)
			{
			case GOTO_AST:
				i = line_checker(temp_ast->integerLine, whileFlag);
				i--;
			break;
			case LET_AST1:
				localVar = FALSE;
				if(whileFlag == 0)
				{
					varTableIdx = registerVar(temp_ast->varName, triple_table[i]->num, 1, whileFlag);
					localVar = FALSE;
				}
				else
				{
					varTableIdx = registerVar(temp_ast->varName, triple_table[i]->num, 1, whileFlag);
					int temp = varTableIdx;
					if(temp < 0)
					{
						varTableIdx = registerLocalVar(temp_ast->varName, triple_table[i]->num, 0);
						localVar = TRUE;
					}
				}
				result = calculator(temp_ast, 0, triple_table[i]->num, localVar);
				if(localVar == FALSE)
				{
					updateVar(varTableIdx, result);
				}
				else if(localVar == TRUE)
				{
					updateLocalVar(varTableIdx, result);
				}
				break;
			case LET_AST2:
				if(whileFlag == TRUE)
				{
					printf("Error: Array is not allowed in While-loop\n");
					exit(1);
				}

				varTableIdx = findArrayName(temp_ast->varName, triple_table[i]->num);
				if(varTableIdx < 0)
				{
					printf("Error: No declared array\n");
					exit(1);
				}else
				{
					int index = 0, value = 0;
					index = calculator(temp_ast, 0, triple_table[i]->num, whileFlag);
					value = calculator(temp_ast, 1, triple_table[i]->num, whileFlag);
					updateArray(varTableIdx, index, value, whileFlag);
				}
			break;
			case DIM_AST:
				if(whileFlag == TRUE)
				{
					printf("Error: Array is not allowed in While-loop\n");
					exit(1);
				}
				varTableIdx = registerVar(temp_ast->varName, triple_table[i]->num, 2, whileFlag);
				if(varTableIdx == -1)
				{
					printf("Error: Variable is redeclared\n");
					exit(1);
				}else
				{
					int size = 0;
					size = calculator(temp_ast, 0, triple_table[i]->num, whileFlag);
					if(size > 0)
					{
						table_1[varTableIdx].elem = (arrayElem*)(malloc(sizeof(arrayElem)*size));
					}else
					{
						printf("Error: Array size should be more than 1");
						exit(1);
					}
					table_1[varTableIdx].size = size;

				}
			break;
			case INPUT_AST:
					if(whileFlag == FALSE)
					{
						varTableIdx = registerVar(temp_ast->varName, triple_table[i]->num, 1, TRUE);
						if(varTableIdx < 0)
						{
							printf("Error: variable should be declared before\n");
							exit(1);
						}else
						{
							printf("?");
							scanf("%d", &result);
							updateVar(varTableIdx, result);
						}
					}else
					{
						varTableIdx = registerVar(temp_ast->varName, triple_table[i]->num, 1, TRUE);
						if(varTableIdx < 0)
						{

							varTableIdx = registerLocalVar(temp_ast->varName, triple_table[i]->num, 1);
							if(varTableIdx < 0)
							{
								printf("Error: Variable should be declared before\n");
								exit(1);
							}else
							{
								printf("?");
								scanf("%d", &result);
								updateLocalVar(varTableIdx, result);
							}
						}else
						{
							printf("?");
							scanf("%d", &result);
							updateVar(varTableIdx, result);
						}
					}

					break;
			case PRINT_TEXT_AST:
				printf("%s\n", temp_ast->string);
			break;
			case PRINT_AST:
				result = calculator(temp_ast, 0, triple_table[i]->num, whileFlag);
				printf("%d\n", result);
			break;
			}
			ifFlag = FALSE;
			prevTrueFlag = FALSE;
		}else if(type == IFSTMT_AST)
		{
			switch(subtype)
			{
			case IFTHEN_AST	:
				result = calculator(temp_ast, 0, triple_table[i]->num, whileFlag);
				if(result != 0)
				{
					if(whileFlag == FALSE)
					{
						i = line_checker(temp_ast->integerLine, whileFlag);
						i--;
						ifFlag = FALSE;
						prevTrueFlag = TRUE;
					}else
					{
						i = line_checker(temp_ast->integerLine, whileFlag);
						i--;
						ifFlag = FALSE;
						prevTrueFlag = TRUE;
						if(i < whileStart && i > whileEnd){
							printf("Error: new\n");
							exit(1);
						}
					}

				}else
				{
					ifFlag = TRUE;
					prevTrueFlag = FALSE;
				}
				break;
			case IFELSE_AST	:
				if(ifFlag == FALSE)
				{
					printf("Error: missing if\n");
					exit(1);
				}
				if(prevTrueFlag == TRUE)
				{
					break;
				}else if(prevTrueFlag == FALSE)
				{
					result = calculator(temp_ast, 0, triple_table[i]->num, whileFlag);
					if(result != 0)
					{
					//	printf("else if true\n");
						i = line_checker(temp_ast->integerLine, whileFlag);
						i--;
						ifFlag = FALSE;
						prevTrueFlag = TRUE;
					}else
					{
					//	printf("else if false::\n");
						ifFlag = TRUE;
						prevTrueFlag = FALSE;
					}
				}else
				{
					printf("Error if-statment\n");
					exit(1);
				}

				break;
			case ELSE_AST:
				if(ifFlag == FALSE)
				{
					printf("Error: missing if\n");
					exit(1);
				}
				if(prevTrueFlag == FALSE)
				{
				//	printf("else true\n");
						i = line_checker(temp_ast->integerLine, whileFlag);
						i--;
						ifFlag = FALSE;
						prevTrueFlag = TRUE;

				}
				ifFlag = FALSE;
				break;
			}
		}else if(type == WHILE_AST)
		{
			whileFlag = TRUE;
			if(subtype == WHILE_IF_AST)
			{
				whileStart = i;
			//	printf("while start %d\n", i);
				int k = 0;
				int j = 0;
				for(j = 0; j< loop_range_size; j++)
				{
					if(loop_range_table[j]->start_num == triple_table[i]->num)
					{
						whileEnd = loop_range_table[j]->end_num;
				//		printf("while end from loop range %d\n", whileEnd);
						for(k = 0; k< triple_table_size;k++)
						{
							if(triple_table[k]->num == whileEnd)
							{	whileEnd = k;	break;}
						}
					}
				}
				result = calculator(temp_ast, 0, triple_table[i]->num, whileFlag);

				if(result == 0)
				{

					i = whileEnd;

					whileFlag = FALSE;
				}
			}else if(subtype == END_WHILE)
			{
				i = whileStart;
				i--;
				local_count = 0;
			}
		}
		else

		{
			printf("Runtime Error: run programm\n");
			exit(1);
			return;
		}
		i++;
	}
}

int line_checker(int linenum, int flag)
{
	int i;
	int find = 0;
	int result = -1;
	if(triple_table_size>0)
	{
		for(i=0;i<triple_table_size;i++)
		{
			if(linenum==triple_table[i]->num)
			{
				result = i;
			}
		}
	}

	if(flag == TRUE)
	{
		;
	}else
	{
		if(loop_range_size>0)
		{
			for(i=0;i<loop_range_size;i++)
			{
				if(linenum<loop_range_table[i]->end_num&&linenum>loop_range_table[i]->start_num)
				{
					printf("RUNTIME Error: GOTO cannot into While loop\n");
					exit(1);
				}
			}
		}
	}


	if(result < 0)
	{
		printf("RUNTIME Error: undefined line \n");
		exit(1);
	}else
	{
		return result;
	}
}

void loop_range_make(int start,int end)
{
	loop_range_size++;
    loop_range* new_loop_range = malloc(sizeof(loop_range));
	new_loop_range->start_num = start;
	new_loop_range->end_num = end;
	loop_range_table = realloc(loop_range_table, sizeof(loop_range)*(loop_range_size+1));
	loop_range_table[loop_range_size-1] = new_loop_range;
}

int integer_checker(int num)
{
	if(num < -2147483647||num>2147483647)
	{
		printf("RUNTIME Error: integer overflow\n");
		exit(1);
	}
	else return 1;
}

void integer_overflow_check(int a, int b, int c)
{
	//c=0 plus c=1 minus
	int sum;
	if(c==1)
	b= -b;

	sum = a + b;
	if (a >= 0&&b>=0) {
		  	if (sum < a||sum<b)
		   	{
				printf("RUNTIME Error: integer overflow\n");
				exit(1);
			}
	} else if(a<0 &&b<0){
		   	if (sum > a||sum>b)
		   	{
				printf("RUNTIME Error: integer overflow\n");
				exit(1);
			}
	}
}
