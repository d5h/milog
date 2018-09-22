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

type term =
  | Atom of string
  | Var of string
  | Num of int
  | Struct of string * term list

type statement = term * term list

type request =
  | Ask of term list
  | Tell of statement
  | Dump
  | Forget of term
  | Load of string
  | Save of string
  | Help
  | Babble

type opassoc =
  | Left
  | Non

let ops = [
  ("=", (700, Non));
  ("is", (700, Non));
  ("<", (700, Non));
  ("<=", (700, Non));
  (">", (700, Non));
  (">=", (700, Non));
  ("=:=", (700, Non));
  ("=\\=", (700, Non));
  ("+", (500, Left));
  ("-", (500, Left));
  ("*", (400, Left));
  ("/", (400, Left))
]

let isop op = List.mem_assoc op ops

let prec = function
  | Struct(op, [_; _]) ->
      (try fst (List.assoc op ops) with Not_found -> 0)
  | Struct(op, _) when isop op -> 1500
  | _ -> 0

let rec output_args c = function
  | [] -> ()
  | [arg] -> output_term c arg
  | arg :: args ->
      output_term c arg;
      output_string c ", ";
      output_args c args

and output_list c = function
  | Atom "[]" -> ()
  | Struct(".", [head; tail]) ->
      output_string c ", ";
      output_term c head;
      output_list c tail
  | tail ->
      output_string c "|";
      output_term c tail

and output_term c = function
  | Atom nm | Var nm -> output_string c nm
  | Num n -> output_string c (string_of_int n)
  | Struct(op, [lhs; rhs]) when isop op -> output_expr c op lhs rhs
  | Struct(".", [head; tail]) ->
      output_string c "[";
      output_term c head;
      output_list c tail;
      output_string c "]"
  | Struct(nm, args) ->
      output_string c (nm ^ "( ");
      output_args c args;
      output_string c ")"

and output_expr c op lhs rhs =
  let lprec = prec lhs in
  let rprec = prec rhs in
  let oprec, opass = List.assoc op ops in
    (if oprec < lprec || oprec = lprec && opass = Non then
       output_pterm else output_term) c lhs;
    output_string c (" " ^ op ^ " ");
    (if oprec <= rprec then output_pterm else output_term) c rhs

and output_pterm c t =
  output_string c "(";
  output_term c t;
  output_string c ")"

let rec output_kb c = function
  | [] -> output_string c "\n"
  | (head, body) :: rest_kb ->
      output_string c "\n";
      output_term c head;
      (match body with
         | [] -> ()
         | [t] ->
             output_string c " :- ";
             output_term c t
         | t :: ts ->
             output_string c " :-\n  ";
             output_term c t;
             List.iter (fun u ->
                          output_string c ",\n  ";
                          output_term c u) ts);
      output_string c ".\n";
      output_kb c rest_kb

let print_term = output_term stdout
