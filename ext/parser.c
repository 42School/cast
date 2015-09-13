#include "cast.h"

/* Modules and classes.
 */
VALUE cast_mC;
VALUE cast_cParser;
VALUE cast_eParseError;

/* Lexer symbols.
 */
VALUE cast_sym_AUTO;
VALUE cast_sym_BREAK;
VALUE cast_sym_CASE;
VALUE cast_sym_CHAR;
VALUE cast_sym_CONST;
VALUE cast_sym_CONTINUE;
VALUE cast_sym_DEFAULT;
VALUE cast_sym_DO;
VALUE cast_sym_DOUBLE;
VALUE cast_sym_ELSE;
VALUE cast_sym_ENUM;
VALUE cast_sym_EXTERN;
VALUE cast_sym_FLOAT;
VALUE cast_sym_FOR;
VALUE cast_sym_GOTO;
VALUE cast_sym_IF;
VALUE cast_sym_INT;
VALUE cast_sym_LONG;
VALUE cast_sym_REGISTER;
VALUE cast_sym_RETURN;
VALUE cast_sym_SHORT;
VALUE cast_sym_SIGNED;
VALUE cast_sym_SIZEOF;
VALUE cast_sym_STATIC;
VALUE cast_sym_STRUCT;
VALUE cast_sym_SWITCH;
VALUE cast_sym_TYPEDEF;
VALUE cast_sym_UNION;
VALUE cast_sym_UNSIGNED;
VALUE cast_sym_VOID;
VALUE cast_sym_VOLATILE;
VALUE cast_sym_WHILE;
VALUE cast_sym_INLINE;
VALUE cast_sym_RESTRICT;
VALUE cast_sym_BOOL;
VALUE cast_sym_COMPLEX;
VALUE cast_sym_IMAGINARY;

VALUE cast_sym_FCON;
VALUE cast_sym_ICON;
VALUE cast_sym_ID;
VALUE cast_sym_SCON;
VALUE cast_sym_CCON;
VALUE cast_sym_TYPENAME;

VALUE cast_sym_ADDEQ;
VALUE cast_sym_ANDAND;
VALUE cast_sym_ANDEQ;
VALUE cast_sym_DEC;
VALUE cast_sym_ARROW;
VALUE cast_sym_DIVEQ;
VALUE cast_sym_ELLIPSIS;
VALUE cast_sym_EQEQ;
VALUE cast_sym_GEQ;
VALUE cast_sym_INC;
VALUE cast_sym_LEQ;
VALUE cast_sym_LSHIFT;
VALUE cast_sym_LSHIFTEQ;
VALUE cast_sym_MODEQ;
VALUE cast_sym_MULEQ;
VALUE cast_sym_NEQ;
VALUE cast_sym_OREQ;
VALUE cast_sym_OROR;
VALUE cast_sym_RSHIFT;
VALUE cast_sym_RSHIFTEQ;
VALUE cast_sym_SUBEQ;
VALUE cast_sym_XOREQ;

VALUE cast_sym_SEMICOLON;
VALUE cast_sym_LBRACE;
VALUE cast_sym_RBRACE;
VALUE cast_sym_COMMA;
VALUE cast_sym_COLON;
VALUE cast_sym_EQ;
VALUE cast_sym_LPAREN;
VALUE cast_sym_RPAREN;
VALUE cast_sym_LBRACKET;
VALUE cast_sym_RBRACKET;
VALUE cast_sym_DOT;
VALUE cast_sym_AND;
VALUE cast_sym_BANG;
VALUE cast_sym_NOT;
VALUE cast_sym_SUB;
VALUE cast_sym_ADD;
VALUE cast_sym_MUL;
VALUE cast_sym_DIV;
VALUE cast_sym_MOD;
VALUE cast_sym_LT;
VALUE cast_sym_GT;
VALUE cast_sym_XOR;
VALUE cast_sym_OR;
VALUE cast_sym_QUESTION;

VALUE cast_Parser_alloc(VALUE klass) {
  VALUE ret;
  cast_Parser *ret_p;
  ret_p = ALLOC(cast_Parser);
  memset(ret_p, 0, sizeof(cast_Parser));
  ret = Wrap_Struct(ret_p, Parser, klass);
  ret_p->lineno = 1;
  ret_p->token = rb_ary_new2(2);
  rb_ary_push(ret_p->token, Qnil);
  rb_ary_push(ret_p->token, Qnil);
  ret_p->line = 0;
  ret_p->col  = 0;
  return ret;
}

void cast_Parser_mark(cast_Parser *parser) {
  rb_gc_mark(parser->token);
}

void cast_Parser_free(cast_Parser *parser) {
  /* nothing to do */
}


///// SPLIT

char  is_separator(char c)
{
  return (c == '\n' || c == '\0');
}

int   calc_nbr_words(char *str)
{
  int i = 0;
  int word = 0;
  
  for (; str[i] != '\0'; i++)
  {
    if (is_separator(str[i]))
      word++;
  }
  return word;
}

void  copy_words(char **tab, char *str)
{
  int i = 0;
  int word = 0;
  int start = 0;
  int length = 0;
  
  for (; str[i] != '\0'; i++)
  {
    if (is_separator(str[i]))
    {
      length = i - start;
      tab[word] = malloc(sizeof(**tab) * (length + 1));
      strncpy(tab[word], str + start, length);
      tab[word][length] = '\0';
      word++;
      start = i + 1;
    }
  }
  tab[word] = NULL;
}

char  **split(char *str)
{
  char  **tab;
  int   nbr_words;
  
  nbr_words = calc_nbr_words(str);
  tab = malloc(sizeof(*tab) * (nbr_words + 1));
  copy_words(tab, str);
  return (tab);
}

void  freetab(char **tab)
{
  for (int i = 0; tab[i]; i++)
    free(tab[i]);
  free(tab);
}

///// \Split


/* (Private.)  Called by #parse to prepare for lexing.
 */
VALUE cast_Parser_prepare_lexer(VALUE self, VALUE string) {
  cast_Parser *self_p;
  char *b, *e;

  Get_Struct(self, Parser, self_p);
  string = rb_convert_type(string, T_STRING, "String", "to_s");


  b = RSTRING_PTR(string);
  e = b + RSTRING_LEN(string) + 1;

  self_p->file = split(b);
  self_p->bot = b;
  self_p->tok = b;
  self_p->ptr = b;
  self_p->cur = b;
  self_p->pos = b;
  self_p->lim = e;
  self_p->top = e;
  self_p->eof = e;
  self_p->lineno = 1;

  return Qnil;
}

/* (Private.)  Called by #parse to get the next token (as required by
 * racc).  Returns a 2-element array: [TOKEN_SYM, VALUE].
 */
VALUE cast_Parser_next_token(VALUE self) {
  cast_Parser *self_p;
  VALUE token, pos;
  Get_Struct(self, Parser, self_p);

  /* clear the token val */
  rb_ary_store(self_p->token, 1, Qnil);

  /* call the lexer */
  yylex(self, self_p);

  /* return nil if EOF */
  if (rb_ary_entry(self_p->token, 0) == Qnil)
  {
    freetab(self_p->file);
    return Qnil;
  }

  //////////////////////////////////////////////

  int     diff = strlen(self_p->bot) - strlen(self_p->tok);
  int     line = 0;
  int     col = 0;
  int     count = 0;

  while (self_p->file[line] && count + strlen(self_p->file[line]) < diff)
  {
      count += strlen(self_p->file[line]) + 1;
      line += 1;
  }
  col = diff - count;

  //////////////////////////////////////////////////////
  /* set self.pos */
  pos = rb_iv_get(self, "@pos");

  rb_funcall(pos, rb_intern("col_num="), 1, LONG2NUM(col));
  rb_funcall(pos, rb_intern("line_num="), 1, LONG2NUM(line + 1));
  /* make token */
  token = rb_funcall(rb_const_get(cast_cParser, rb_intern("Token")),
                     rb_intern("new"), 2,
                     rb_funcall2(pos, rb_intern("dup"), 0, NULL),
                     rb_ary_entry(self_p->token, 1));
  /* put the token in the array */
  rb_ary_store(self_p->token, 1, token);
  return self_p->token;
}

/* Called from Init_cast_ext to initialize the Parser section.
 */
void cast_init_parser(void) {
  VALUE cRaccParser;
  rb_require("racc/parser");
  cRaccParser = rb_const_get(rb_const_get(rb_cObject, rb_intern("Racc")),
                             rb_intern("Parser"));
  cast_cParser = rb_define_class_under(cast_mC, "Parser", cRaccParser);
  rb_define_alloc_func(cast_cParser, cast_Parser_alloc);
  cast_eParseError = rb_define_class_under(cast_mC, "ParseError", rb_eStandardError);

  rb_define_private_method(cast_cParser, "next_token", cast_Parser_next_token, 0);
  rb_define_private_method(cast_cParser, "prepare_lexer", cast_Parser_prepare_lexer, 1);

  cast_sym_AUTO        = ID2SYM(rb_intern("AUTO"));
  cast_sym_BREAK       = ID2SYM(rb_intern("BREAK"));
  cast_sym_CASE        = ID2SYM(rb_intern("CASE"));
  cast_sym_CHAR        = ID2SYM(rb_intern("CHAR"));
  cast_sym_CONST       = ID2SYM(rb_intern("CONST"));
  cast_sym_CONTINUE    = ID2SYM(rb_intern("CONTINUE"));
  cast_sym_DEFAULT     = ID2SYM(rb_intern("DEFAULT"));
  cast_sym_DO          = ID2SYM(rb_intern("DO"));
  cast_sym_DOUBLE      = ID2SYM(rb_intern("DOUBLE"));
  cast_sym_ELSE        = ID2SYM(rb_intern("ELSE"));
  cast_sym_ENUM        = ID2SYM(rb_intern("ENUM"));
  cast_sym_EXTERN      = ID2SYM(rb_intern("EXTERN"));
  cast_sym_FLOAT       = ID2SYM(rb_intern("FLOAT"));
  cast_sym_FOR         = ID2SYM(rb_intern("FOR"));
  cast_sym_GOTO        = ID2SYM(rb_intern("GOTO"));
  cast_sym_IF          = ID2SYM(rb_intern("IF"));
  cast_sym_INT         = ID2SYM(rb_intern("INT"));
  cast_sym_LONG        = ID2SYM(rb_intern("LONG"));
  cast_sym_REGISTER    = ID2SYM(rb_intern("REGISTER"));
  cast_sym_RETURN      = ID2SYM(rb_intern("RETURN"));
  cast_sym_SHORT       = ID2SYM(rb_intern("SHORT"));
  cast_sym_SIGNED      = ID2SYM(rb_intern("SIGNED"));
  cast_sym_SIZEOF      = ID2SYM(rb_intern("SIZEOF"));
  cast_sym_STATIC      = ID2SYM(rb_intern("STATIC"));
  cast_sym_STRUCT      = ID2SYM(rb_intern("STRUCT"));
  cast_sym_SWITCH      = ID2SYM(rb_intern("SWITCH"));
  cast_sym_TYPEDEF     = ID2SYM(rb_intern("TYPEDEF"));
  cast_sym_UNION       = ID2SYM(rb_intern("UNION"));
  cast_sym_UNSIGNED    = ID2SYM(rb_intern("UNSIGNED"));
  cast_sym_VOID        = ID2SYM(rb_intern("VOID"));
  cast_sym_VOLATILE    = ID2SYM(rb_intern("VOLATILE"));
  cast_sym_WHILE       = ID2SYM(rb_intern("WHILE"));
  cast_sym_INLINE      = ID2SYM(rb_intern("INLINE"));
  cast_sym_RESTRICT    = ID2SYM(rb_intern("RESTRICT"));
  cast_sym_BOOL        = ID2SYM(rb_intern("BOOL"));
  cast_sym_COMPLEX     = ID2SYM(rb_intern("COMPLEX"));
  cast_sym_IMAGINARY   = ID2SYM(rb_intern("IMAGINARY"));

  cast_sym_FCON        = ID2SYM(rb_intern("FCON"));
  cast_sym_ICON        = ID2SYM(rb_intern("ICON"));
  cast_sym_ID          = ID2SYM(rb_intern("ID"));
  cast_sym_SCON        = ID2SYM(rb_intern("SCON"));
  cast_sym_CCON        = ID2SYM(rb_intern("CCON"));
  cast_sym_TYPENAME    = ID2SYM(rb_intern("TYPENAME"));

  cast_sym_ADDEQ       = ID2SYM(rb_intern("ADDEQ"));
  cast_sym_ANDAND      = ID2SYM(rb_intern("ANDAND"));
  cast_sym_ANDEQ       = ID2SYM(rb_intern("ANDEQ"));
  cast_sym_DEC         = ID2SYM(rb_intern("DEC"));
  cast_sym_ARROW       = ID2SYM(rb_intern("ARROW"));
  cast_sym_DIVEQ       = ID2SYM(rb_intern("DIVEQ"));
  cast_sym_ELLIPSIS    = ID2SYM(rb_intern("ELLIPSIS"));
  cast_sym_EQEQ        = ID2SYM(rb_intern("EQEQ"));
  cast_sym_GEQ         = ID2SYM(rb_intern("GEQ"));
  cast_sym_INC         = ID2SYM(rb_intern("INC"));
  cast_sym_LEQ         = ID2SYM(rb_intern("LEQ"));
  cast_sym_LSHIFT      = ID2SYM(rb_intern("LSHIFT"));
  cast_sym_LSHIFTEQ    = ID2SYM(rb_intern("LSHIFTEQ"));
  cast_sym_MODEQ       = ID2SYM(rb_intern("MODEQ"));
  cast_sym_MULEQ       = ID2SYM(rb_intern("MULEQ"));
  cast_sym_NEQ         = ID2SYM(rb_intern("NEQ"));
  cast_sym_OREQ        = ID2SYM(rb_intern("OREQ"));
  cast_sym_OROR        = ID2SYM(rb_intern("OROR"));
  cast_sym_RSHIFT      = ID2SYM(rb_intern("RSHIFT"));
  cast_sym_RSHIFTEQ    = ID2SYM(rb_intern("RSHIFTEQ"));
  cast_sym_SUBEQ       = ID2SYM(rb_intern("SUBEQ"));
  cast_sym_XOREQ       = ID2SYM(rb_intern("XOREQ"));

  cast_sym_SEMICOLON   = ID2SYM(rb_intern("SEMICOLON"));
  cast_sym_LBRACE      = ID2SYM(rb_intern("LBRACE"));
  cast_sym_RBRACE      = ID2SYM(rb_intern("RBRACE"));
  cast_sym_COMMA       = ID2SYM(rb_intern("COMMA"));
  cast_sym_COLON       = ID2SYM(rb_intern("COLON"));
  cast_sym_EQ          = ID2SYM(rb_intern("EQ"));
  cast_sym_LPAREN      = ID2SYM(rb_intern("LPAREN"));
  cast_sym_RPAREN      = ID2SYM(rb_intern("RPAREN"));
  cast_sym_LBRACKET    = ID2SYM(rb_intern("LBRACKET"));
  cast_sym_RBRACKET    = ID2SYM(rb_intern("RBRACKET"));
  cast_sym_DOT         = ID2SYM(rb_intern("DOT"));
  cast_sym_AND         = ID2SYM(rb_intern("AND"));
  cast_sym_BANG        = ID2SYM(rb_intern("BANG"));
  cast_sym_NOT         = ID2SYM(rb_intern("NOT"));
  cast_sym_SUB         = ID2SYM(rb_intern("SUB"));
  cast_sym_ADD         = ID2SYM(rb_intern("ADD"));
  cast_sym_MUL         = ID2SYM(rb_intern("MUL"));
  cast_sym_DIV         = ID2SYM(rb_intern("DIV"));
  cast_sym_MOD         = ID2SYM(rb_intern("MOD"));
  cast_sym_LT          = ID2SYM(rb_intern("LT"));
  cast_sym_GT          = ID2SYM(rb_intern("GT"));
  cast_sym_XOR         = ID2SYM(rb_intern("XOR"));
  cast_sym_OR          = ID2SYM(rb_intern("OR"));
  cast_sym_QUESTION    = ID2SYM(rb_intern("QUESTION"));
}
