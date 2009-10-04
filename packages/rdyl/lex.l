%{
#include <rscheme/scheme.h>
#include "rdyltab.h"
#include <string.h>

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) do { if (strm_left) \
					     { \
						result = 1;\
						 strm_left--; \
						  *buf = *strm_ptr++; \
					     } else \
					     { result = YY_NULL; } \
					 } while (0)
static char *strm_ptr;
static size_t strm_left;

#define SYM(str) lookup_symbol( str )

#define yywrap() 1
#define ASCH(ch) MAKE_ASCII_CHAR(ch)
int isatty(int fd);
extern int yylineno;

obj OP( char *operator )
{
    if (operator[0] == '|')
	return SYM("or");
    else if (operator[0] == '&')
	return SYM("and");
    return SYM(operator);
}

int atox( char *s )
{
int d, n = 0;

    while (*s)
    {
	d = *s++ - '0';
	if (d >= 'a' - '0')
	    d = d - 'a' + 'A';
	if (d >= 'A' - '0')
	    d = d - 'A' + '0';
	n = (n * 16) + d;
    }
    return n;
}


int atoo( char *s )
{
int n = 0;

    while (*s)
	n = (n * 8) + (*s++ - '0');
    return n;
}

int atob( char *s )
{
int n = 0;

    while (*s)
	n = (n * 2) + (*s++ - '0');
    return n;
}

static struct {
    const char *ch_name;
    obj         named_ch;
} char_names[] = { 
    { "nul", ASCH(0) },
    { "soh", ASCH(1) },
    { "n", ASCH(10) },
    { "t", ASCH(9) },
    NULL };

obj lookupch( char *name )
{
int i;

    for (i=0; char_names[i].ch_name; i++)
    {
	if (strcasecmp(char_names[i].ch_name,name) == 0)
	    return char_names[i].named_ch;
    }
    /* lexical error! */
    yyerror( "lexical error (bad char name)" );
    return ASCH(name[0]);
}

%}

id1                  	[-A-Za-z<>$*_@!?%^&=+|/]
id2                  	[-A-Za-z<>$*_@!?%^&=+|/0-9]
op			[-~=+*/&|<>]
%%

[ \t]			;
\n			{ yylineno++; }
\/\/[^\n]*		;
\/\*([^/][^*]*\*)*\*\/  ;
\/\*[^*]*\*\/  ;
'[^'\\]'		{ yylval.o = ASCH(yytext[1]); return TOK_CHAR; }
'\\0'			{ yylval.o = ASCH(0); return TOK_CHAR; }
'\\[A-Za-z]{1,3}'	{ yytext[yyleng-1] = 0;
			  yylval.o = lookupch(yytext+2); return TOK_CHAR; }
\\{op}			{ yylval.o = OP(yytext+1); return TOK_ID; }
begin			{ return KWD_BEGIN; }
end			{ return KWD_END; }
let			{ return KWD_LET; }
method			{ return KWD_METHOD; }
local			{ return KWD_LOCAL; }
define			{ return KWD_DEFINE; }
variable		{ return KWD_VARIABLE; }
constant		{ return KWD_CONSTANT; }
generic			{ return KWD_GENERIC; }
class			{ return KWD_CLASS; }
slot			{ return KWD_SLOT; }
if			{ return KWD_IF; }
elseif			{ return KWD_ELSEIF; }
else			{ return KWD_ELSE; }
=>			{ return LEADS_TO; }
\"[^\"\n]*\"		{ yytext[yyleng-1] = 0;
			  yylval.o = make_string( yytext + 1);
			    return TOK_STRING; }
#\"[^\"\n]*\"		{ yytext[yyleng-1] = 0; 
			  yylval.o = SYM(yytext+2); return TOK_SYMBOL; }
[0-9]+			{ yylval.o = int2fx(atoi(yytext)); return TOK_NUMBER; }
:: 			{ return TYPE_IS; }
:=			{ yylval.o = SYM("set!"); return OP_ASSIGN; }
==			{ yylval.o = SYM(yytext); return OP_EQ; }
~=			{ yylval.o = SYM(yytext); return OP_NE; }
\<=			{ yylval.o = SYM(yytext); return OP_LE; }
>=			{ yylval.o = SYM(yytext); return OP_GE; }
#rest			{ return REST; }
#\[			{ return BEGIN_VECTOR; }
#\(			{ return BEGIN_LIST; }
#t			{ yylval.o = TRUE_OBJ; return TOK_BOOL; }
#f			{ yylval.o = FALSE_OBJ; return TOK_BOOL; }
#x[0-9A-Fa-f]+		{ yylval.o = int2fx(atox(yytext+2)); return TOK_NUMBER; }
#o[0-7]+		{ yylval.o = int2fx(atoo(yytext+2)); return TOK_NUMBER; }
#b[01]+			{ yylval.o = int2fx(atob(yytext+2)); return TOK_NUMBER; }
{op}			{ yylval.o = OP(yytext); return yytext[0]; }
{id1}{id2}*:		{ yylval.o = SYM(yytext); return TOK_KEYWORD; }
{id1}{id2}*		{ yylval.o = SYM(yytext); return TOK_ID; }
.			{ return yytext[0]; }


%%

obj try_to_parse( obj source, int *left )
{
  extern obj yyroot;
  int rc;
  YY_BUFFER_STATE s;

  s = yy_create_buffer( NULL, 500 );
  yy_switch_to_buffer( s );

  strm_ptr = string_text(source);
  strm_left = string_length(source);

  rc = yyparse();

  yy_delete_buffer( s );

  *left = strm_left;

  if (rc)
    return FALSE_OBJ;
  else
    return yyroot;
}
