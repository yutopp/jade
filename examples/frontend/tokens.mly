(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

%token <string>         ID STRING
%token <(int*int*bool)> INT
%token <string>         BIN_OP
%token                  KEYWORD_MODULE
                        KEYWORD_STRUCT
                        KEYWORD_EXTERN
%token                  KEYWORD_DEF
%token                  KEYWORD_IF KEYWORD_ELSE
%token                  KEYWORD_LET
%token                  KEYWORD_TRUE KEYWORD_FALSE
%token                  LPAREN RPAREN
%token                  LBLOCK RBLOCK
%token                  EQ
%token                  DOT COMMA COLON SEMICOLON
%token                  EOF

%%
