%{
  (* milog - a Prolog interpreter.

     Copyright (C) 2006, 2007 Dan Hipschman <dsh@linux.ucla.edu>

     This program is free software; you can redistribute it and/or
     modify it under the terms of the GNU General Public License as
     published by the Free Software Foundation; either version 2, or
     (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
     02110-1301, USA. *)

  open Types

%}

%token <string> ATOM STRING VAR
%token <int> NUM
%token LPAREN RPAREN LBRAC RBRAC VBAR PERIOD QUESTION COMMA IF EXCLAM
%token JUNK EQ IS LT LTEQ GT GTEQ EQEQ NEQEQ PLUS MINUS STAR SLASH
%token DUMP FORGET LOAD SAVE HELP

%nonassoc EQ IS LT LTEQ GT GTEQ EQEQ NEQEQ
%left PLUS MINUS
%left STAR SLASH

%start main
%type <Types.request> main

%%

main:
    clause PERIOD        { Tell $1 }
  | terms QUESTION       { Ask $1 }
  | DUMP                 { Dump }
  | FORGET aterm PERIOD  { Forget $2 }
  | LOAD STRING          { Load $2 }
  | SAVE STRING          { Save $2 }
  | HELP                 { Help }
  ;

clause:
    aterm               { $1, [] }
  | aterm IF terms      { $1, $3 }
  ;

terms:
    aterm                    { [$1] }
  | aterm COMMA terms        { $1 :: $3 }
  ;

aterm:
    ATOM LPAREN terms RPAREN   { Struct($1, $3) }
  | ATOM                       { Atom $1 }
  | VAR                        { Var $1 }
  | LBRAC list_args RBRAC      { $2 }
  | EXCLAM                     { Atom "!" }
  | arith                      { $1 }
  | NUM                        { Num $1 }
  ;

list_args:
    /* empty */             { Atom "[]" }
  | aterm                   { Struct(".", [$1; Atom "[]"]) }
  | aterm COMMA list_args   { Struct(".", [$1; $3]) }
  | aterm VBAR VAR          { Struct(".", [$1; Var $3]) }
  ;

arith:
    expr EQ expr         { Struct("=", [$1; $3]) }
  | expr IS expr         { Struct("is", [$1; $3]) }
  | expr LT expr         { Struct("<", [$1; $3]) }
  | expr LTEQ expr       { Struct("<=", [$1; $3]) }
  | expr GT expr         { Struct(">", [$1; $3]) }
  | expr GTEQ expr       { Struct(">=", [$1; $3]) }
  | expr EQEQ expr       { Struct("=:=", [$1; $3]) }
  | expr NEQEQ expr      { Struct("=\\=", [$1; $3]) }
  | expr PLUS expr       { Struct("+", [$1; $3]) }
  | expr MINUS expr      { Struct("-", [$1; $3]) }
  | expr STAR expr       { Struct("*", [$1; $3]) }
  | expr SLASH expr      { Struct("/", [$1; $3]) }
  | LPAREN expr RPAREN   { $2 }
  ;

expr:
    arith    { $1 }
  | num      { $1 }
  ;

num:
    NUM    { Num $1 }
  | VAR    { Var $1 }
  ;
