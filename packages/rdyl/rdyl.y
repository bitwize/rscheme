%{
#pragma alloca
#define YYDEBUG 1
#include <stdio.h>
#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <stdarg.h>

void yyerror( const char *msg );

int yylineno;
int did_read_eof;
obj yyroot;

int my_yylex( void )
{
  int rc = yylex();

  if (rc == 0)
    did_read_eof = 1;
  return rc;
}

void yyerror( const char *msg )
{
}

#define yylex() my_yylex()

#define S(x) lookup_symbol(# x)

#define car(x)    pair_car(x)
#define cdr(x)   pair_cdr(x)

#define list1(x) cons(x,NIL_OBJ)
#define quoted(x) list(2,S(quote),x)

obj app( obj lst, obj item )
{
obj cell = cons( item, NIL_OBJ );

    if (EQ(lst,NIL_OBJ))
	return cell;
    else
    {
    obj p;
    
	for (p=lst; !EQ(cdr(p),NIL_OBJ); p=cdr(p));
	pair_set_cdr( p, cell );
	return lst;
    }
}

obj list( int argc, ... )
{
obj l = NIL_OBJ;
va_list va;
int i;

    va_start( va, argc );
    for (i=0; i<argc; i++)
	l = app( l, va_arg( va, obj ) );
    return l;
}

%}

%union {
    obj o;
}

%token <o> TOK_STRING
%token <o> TOK_SYMBOL TOK_NUMBER
%token <o> TOK_ID TOK_CHAR TOK_BOOL TOK_KEYWORD

%token KWD_BEGIN KWD_END KWD_DEFINE KWD_VARIABLE KWD_CONSTANT
%token KWD_METHOD KWD_LET KWD_LOCAL KWD_GENERIC KWD_CLASS KWD_SLOT
%token KWD_IF KWD_ELSEIF KWD_ELSE
%token BEGIN_VECTOR BEGIN_LIST

%token TYPE_IS LEADS_TO REST 
%token <o> OP_ASSIGN OP_EQ OP_NE OP_LE OP_GE 
%token <o> '-' '~' '=' '<' '>' '+' '*' '/' '&' '|'

%type <o> root body ne_body body_form local_decl 
%type <o> local_methods local_method method_def bindings
%type <o> expr method bin_expr
%type <o> body_OPT signature method_body parameter  
%type <o> parameter_list parameter_list_OPT parameters
%type <o> args ne_args variable variables variable_list variable_list_1 special_op
%type <o> if_expr elseifs cond_clause else_OPT
%type <o> literal constant constants_OPT constants strings
%type <o> definitions definition class_def
%type <o> slot_specs  ne_slot_specs slot_spec gf_body arg

%type <o> bin_op

%right OP_ASSIGN
%left '&' '|'
%left OP_EQ OP_LE OP_NE OP_GE '<' '=' '>'
%left '+' '-'
%left '*' '/'
%right U_MINUS '~' '('
%left '.'

%%

root : definitions semi_OPT			{ yyroot = $$; }

definitions : definitions ';' definition	{ $$ = app( $1, $3 ); }
	    | definition			{ $$ = list1($1); }

definition : KWD_DEFINE local_method 			{ $$ = list(2,S(defmeth),$2); }
	   | KWD_DEFINE KWD_VARIABLE bindings '=' expr	{ $$ = list(3,S(defvar),$3,$5); }
	   | KWD_DEFINE KWD_CONSTANT bindings '=' expr	{ $$ = list(3,S(defconst),$3,$5); }
	   | KWD_DEFINE KWD_GENERIC TOK_ID gf_body	{ $$ = list(3,S(defgf),$3,$4); }
	   | class_def
           | expr  { $$ = list(2,S(expr),$1); }
	   
class_def : KWD_DEFINE KWD_CLASS TOK_ID '(' args ')' slot_specs KWD_END KWD_CLASS
							{ $$ = list(4,S(defclass),$3,$5,$7); }

slot_specs : ne_slot_specs semi_OPT
	   | /* empty */		{ $$ = list(0); }

ne_slot_specs : ne_slot_specs ';' slot_spec	{ $$ = app( $1, $3 ); }
	      | slot_spec			{ $$ = list1($1); }

slot_spec : KWD_SLOT TOK_ID			{ $$ = list(3,S(slot),$2,FALSE_OBJ); }
          | KWD_SLOT TOK_ID TYPE_IS TOK_ID	{ $$ = list(3,S(slot),$2,list(2,S(ref),$4)); }

body : ne_body semi_OPT	

ne_body : ne_body ';' body_form		{ $$ = app($1,$3); }
        | body_form			{ $$ = list1($1); }

body_form : expr

local_decl : KWD_LET bindings '=' expr	{ $$ = list(3,S(bind),$2,$4); }
	   | KWD_LOCAL local_methods	{ $$ = list(2,S(letrec),$2); }

local_methods : local_methods ',' local_method { $$ = app($1,$3); }
	      | local_method 		       { $$ = list1($1); }

local_method : KWD_METHOD method_def		{ $$ = $2; }

method_def : TOK_ID method_body KWD_END KWD_METHOD { $$ = list(2,$1,$2); }

expr : TOK_ID					{ $$ = list(2,S(ref),$1); }
     | KWD_BEGIN body KWD_END			{ $$ = list(2,S(begin),$2); }
     | '-' expr   %prec U_MINUS			{ $$ = list(3,S(op),S(-),$2); }
     | '~' expr					{ $$ = list(3,S(op),S(~),$2); }
     | bin_expr
     | method
     | '(' expr ')'			{ $$ = $2; }
     | expr '(' args ')'		{ $$ = list(3,S(combo),$1,$3); }
     | expr '[' args ']'		{ $$ = list(3,S(aref),$1,$3); }
     | expr '.' TOK_ID			{ $$ = list(3,S(sel),$1,$3); }
     | local_decl ';' body		{ $$ = list(3,S(decl),$1,$3); }
     | if_expr
     | literal

if_expr : KWD_IF cond_clause elseifs else_OPT KWD_END
					{ $$ = list(4,S(if),$2,$3,$4); }

elseifs : elseifs KWD_ELSEIF cond_clause	{ $$ = app( $1, $3 ); }
        | /* empty */			{ $$ = list(0); }

cond_clause : '(' expr ')' body 	{ $$ = list(3,S(clause),$2,$4); }

else_OPT : KWD_ELSE body		{ $$ = $2; }
         | /* empty */			{ $$ = FALSE_OBJ; }
	 
bin_op : OP_EQ		
       | OP_NE 		
       | OP_LE 		
       | OP_GE 		
       | '<' 		
       | '=' 		
       | '>'		
       | '+' 		
       | '-' 
       | '*' 	
       | '/' 

special_op : '&' | '|'

bin_expr : expr bin_op expr		{ $$ = list(4,S(op),$2,$1,$3); }
	 | expr special_op expr		{ $$ = list(4,S(special),$2,$1,$3); }
	 | expr OP_ASSIGN expr		{ $$ = list(3,$2,$1,$3); }
	 
args : ne_args
     | /* empty */			{ $$ = list(0); }

ne_args : ne_args ',' arg		{ $$ = app($1,$3); }
        | arg				{ $$ = list1($1); }

arg : expr				{ $$ = list(2,S(positional),$1); }
    | TOK_KEYWORD expr			{ $$ = list(3,S(keyword),$1,$2); }

method : KWD_METHOD method_body KWD_END KWD_METHOD {$$=$2;}

method_body : signature body_OPT		{$$=list(3,S(method),$1,$2); }

signature : '(' parameter_list_OPT ')' LEADS_TO '(' variable_list ')' semi_OPT
		{ $$ = list(3,S(signature),$2,$6); }
        | '(' parameter_list_OPT ')' LEADS_TO variable ';'
		{ $$ = list(3,S(signature),$2,list1($5)); }
        | '(' parameter_list_OPT ')' semi_OPT
		{ $$ = list(3,S(signature),$2,list(0)); }

gf_body : '(' parameter_list_OPT ')' LEADS_TO '(' variable_list ')'
		{ $$ = list(3,S(signature),$2,$6); }
        | '(' parameter_list_OPT ')' LEADS_TO variable 
		{ $$ = list(3,S(signature),$2,list1($5)); }
        | '(' parameter_list_OPT ')'
		{ $$ = list(3,S(signature),$2,list(0)); }

semi_OPT : ';' | /* empty */

body_OPT : body | /* empty */ { $$ = list(0); }

parameter_list_OPT : parameter_list | /* empty */ { $$ = list(0); }

parameter_list : parameters		{ $$ = list(3,S(parms),$1,FALSE_OBJ); }
	       | parameters ',' REST TOK_ID { $$ = list(3,S(parms),$1,$4); }

parameters : parameters ',' parameter		{ $$ = app( $1, $3 ); }
           | parameter				{ $$ = list1($1); }

parameter : variable
          | TOK_ID OP_EQ expr			{ $$ = list(3,S(eq),$1,$3); }

bindings : variable_list_1			{ $$ = $1; }
	 | '(' variable_list ')'		{ $$ = $2; }

variable_list_1 : variable			{ $$ = list(3,S(varlist),list1($1),FALSE_OBJ); }

variable_list : variables			{ $$ = list(3,S(varlist),$1,FALSE_OBJ); }
      | variables ',' REST TOK_ID		{ $$ = list(3,S(varlist),$1,$4); }
      | REST TOK_ID				{ $$ = list(3,S(varlist),list(0),$2); }

variables : variables ',' variable		{ $$ = app($1,$3); }
          | variable				{ $$ = list1($1); }

variable : TOK_ID				{ $$ = list(3,S(var),$1,FALSE_OBJ); }
	 | TOK_ID TYPE_IS TOK_ID		
	 	{ $$ = list(3,S(var),$1,list(2,S(ref),$3)); }
	 		/* non-trivial expressions
			   have to be in parenthesis,
			   because bindings are "variable '=' ..."
			*/
	 | TOK_ID TYPE_IS '(' expr ')'		{ $$ = list(2,$1,$4); }

literal : strings				{ $$ = list(2,S(qstr),$1); }
	| TOK_SYMBOL				{ $$ = quoted($1); }
	| TOK_NUMBER				{ $$ = quoted($1); }
	| TOK_CHAR				{ $$ = quoted($1); }
	| TOK_BOOL				{ $$ = quoted($1); }
	| BEGIN_VECTOR constants_OPT ']'	{ $$ = list(2,S(qvec),$2); }
	| BEGIN_LIST constants_OPT ')'	{ $$ = list(3,S(qlist),$2,NIL_OBJ); }
	| BEGIN_LIST constants '.' constant ')'	{ $$ = list(3,S(qlist),$2,$4); }

constants_OPT : constants
	      | /* empty */			{ $$ = list(0); }

constants : constants ',' constant		{ $$ = app($1,$3); }
	  | constant				{ $$ = list1($1); }

constant : literal
	 | TOK_KEYWORD

strings : strings TOK_STRING			{ $$ = app($1,$2); }
        | TOK_STRING				{ $$ = list1($1); }
