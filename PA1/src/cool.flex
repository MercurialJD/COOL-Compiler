/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Dont remove anything that was here initially
 */

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int comment_level = 0;  /* Counts the level of nested comments */
int str_char_cnt = 0;   /* Counts the characters in the string */

/* Check if the string buffer can push cnt value(s) */
#define CHECK_BUFF(cnt) \
  if (str_char_cnt + cnt >= MAX_STR_CONST) { \
    yylval.error_msg = "String constant too long"; \
    BEGIN(STR_SKIP_COND); \
    return ERROR; \
  }

%}

%option noyywrap

/* Start conditions */
%x COMMENT_COND STR_COND STR_SKIP_COND

/*
 * Define names for regular expressions here.
 */

/* Some defenitions for complex patterns */
digits      [[:digit:]]+
bool        (t(?i:rue))|(f(?i:alse))
typeid      [[:upper:]]([[:alnum:]]{+}[_])*
objectid    [[:lower:]]([[:alnum:]]{+}[_])*
operator    ";"|"{"|"}"|"("|")"|","|":"|"@"|"."|"+"|"-"|"*"|"/"|"~"|"<"|"="

%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */

<INITIAL>{            /* Normal context */
  --.*          ;     /* Do nothing with one-line comments */

  "(*" {              /* Begining of the first level comment */
    ++comment_level;
    BEGIN(COMMENT_COND);
  }

  "*)" {              /* Unmatched end of comment */
    yylval.error_msg = "Unmatched *)";
    BEGIN(0);
    return ERROR;
  }

  "\"" {              /* Begining of a string */
    str_char_cnt = 0;
    string_buf_ptr = string_buf;
    BEGIN(STR_COND);
  }

  [ \f\r\t\v]   ;     /* Do nothing with blank chars (excluding \n) */
  \n                  ++curr_lineno;

  /* Defined in cool-parse.h */
  (?i:class)          return CLASS;
  (?i:else)           return ELSE;
  (?i:fi)             return FI;
  (?i:if)             return IF;
  (?i:in)             return IN;
  (?i:inherits)       return INHERITS;
  (?i:let)            return LET;
  (?i:loop)           return LOOP;
  (?i:pool)           return POOL;
  (?i:then)           return THEN;
  (?i:while)          return WHILE;
  (?i:case)           return CASE;
  (?i:esac)           return ESAC;
  (?i:of)             return OF;
  "=>"                return DARROW;
  (?i:new)            return NEW;
  (?i:isvoid)         return ISVOID;
  (?i:not)            return NOT;
  "<-"                return ASSIGN;
  "<="                return LE;

  {operator}          return * yytext;

  {digits} {
    yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
  }

  {bool} {
    if(yytext[0] == 't')
      yylval.boolean = 1;
    else
      yylval.boolean = 0;
    return BOOL_CONST;
  }

  {typeid} {
    yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
  }

  {objectid} {
    yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
  }

  . {                           /* Any other character is undefined. ERROR! */
    yylval.error_msg = yytext;
    BEGIN(0);
    return ERROR;
  }
}

<COMMENT_COND>{                     /* Inside comment */
  "(*"  ++comment_level;            /* Comment level up */

  "*)"  {                           /* End of a level of comment */
    if(--comment_level == 0) {
      BEGIN(0);
    }
  }

  <<EOF>> {                         /* EOF should not show up in a comment */
    yylval.error_msg = "EOF in comment";
    BEGIN(0);
    return ERROR;
  }

  \n    ++curr_lineno;
  .     ;
}

<STR_COND>{                         /* Inside a string */
  \" {                              /* The end of a string */
    yylval.symbol = stringtable.add_string(string_buf, str_char_cnt);
    BEGIN(0);
    return STR_CONST;
  }

  \n {                              /* Unescaped \n is not allowed in string */
    ++curr_lineno;
    yylval.error_msg = "Unterminated string constant";
    BEGIN(0);
    return ERROR;
  }

  \0 {                              /* Null character is not allowed in string */
    yylval.error_msg = "String contains null character.";
    BEGIN(STR_SKIP_COND);
    return ERROR;
  }

  \\\n  {                           /* Escaped \n is allowed, record \n */
    ++curr_lineno;
    CHECK_BUFF(1);
    *string_buf_ptr++ = '\n';
    ++str_char_cnt;
  }

  <<EOF>> {                         /* EOF should not show up in a string */
    yylval.error_msg = "EOF in string constant";
    BEGIN(0);
    return ERROR;
  }

  \\[btnf] {                        /* Convert escaped characters */
    CHECK_BUFF(1);
    switch(yytext[yyleng - 1]) {
      case 'b':
        *string_buf_ptr++ = '\b';
        break;
      case 't':
        *string_buf_ptr++ = '\t';
        break;
      case 'n':
        *string_buf_ptr++ = '\n';
        break;
      case 'f':
        *string_buf_ptr++ = '\f';
        break;
    }
    ++str_char_cnt;
  }

  \\[^bntf\0]     |                 /* Convert non-escaped characters */
  . {                               /* Add normal characters to the buffer */
    CHECK_BUFF(1);
    *string_buf_ptr++ = yytext[yyleng - 1];
    ++str_char_cnt;
  }
}

<STR_SKIP_COND>{
  \n {                              /* Resume after \n */
    ++curr_lineno;
    BEGIN(0);
  }

  "\""            BEGIN(0);         /* Resume after the closing " */
  \\\"            |                 /* Don't be fooled by \" */
  .               ;
}

%%
