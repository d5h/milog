{
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
  open Parser
  open Lexing
  open Parsing
  open Sys

  exception Eof

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        {pos with
           pos_lnum = pos.pos_lnum + 1;
           pos_bol = pos.pos_cnum}
}

let lc = ['a'-'z']
let uc = ['A'-'Z' '_']
let di = ['0'-'9']
let id = lc | uc | di

rule token =
  parse
      "is"                { IS }
    | lc id* as name      { ATOM name }
    | uc id* as name      { VAR name }
    | di+ as digits       { NUM (int_of_string digits) }
    | [' ' '\t']+         { token lexbuf }
    | '\n'           { next_line lexbuf; token lexbuf }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | '['            { LBRAC }
    | ']'            { RBRAC }
    | '|'            { VBAR }
    | '.'            { PERIOD }
    | '?'            { QUESTION }
    | ','            { COMMA }
    | ":-"           { IF }
    | '!'            { EXCLAM }
    | '+'            { PLUS }
    | '-'            { MINUS }
    | '*'            { STAR }
    | '/'            { SLASH }
    | '='            { EQ }
    | '<'            { LT }
    | "<="           { LTEQ }
    | '>'            { GT }
    | ">="           { GTEQ }
    | "=:="          { EQEQ }
    | "=\\="         { NEQEQ }
    | "#dump"        { DUMP }
    | "#forget"      { FORGET }
    | "#load"        { LOAD }
    | "#save"        { SAVE }
    | "#help"        { HELP }
    | '"' ([^ '"' '\n']* as str) '"'    { STRING str }
    | '%' [^ '\n']*                     { token lexbuf }
    | eof            { raise Eof }
    | _              { JUNK }

{
  let rec getval env = function
    | Var nm as x -> (try getval env (List.assoc nm env)
                      with Not_found -> x)
    | Struct(nm, args) -> Struct(nm, List.map (getval env) args)
    | v -> v

  let next_id = ref 0

  let fresh_var () =
    next_id := !next_id + 1;
    Var ("_G" ^ (string_of_int !next_id))

  let rec fresh_list env = function
    | [] -> [], env
    | t :: ts ->
        let newt, newenv = fresh_term env t in
        let newts, newenv = fresh_list newenv ts in
          newt :: newts, newenv

  and fresh_term env = function
    | Var "_" as t -> t, env
    | Var nm -> (try List.assoc nm env, env with Not_found ->
                   let v = fresh_var () in v, (nm, v) :: env)
    | Struct(nm, args) ->
        let newargs, newenv = fresh_list env args in
          Struct(nm, newargs), newenv
    | t -> t, env

  let fresh (head, body) =
    let newhead, env = fresh_term [] head in
    let newbody, _ = fresh_list env body in
      newhead, newbody

  let rec unify_lists env = function
    | [], [] -> Some env
    | [], _ | _, [] -> None
    | x :: xs, y :: ys ->
        (match unify env x y with
           | None -> None
           | Some newenv -> unify_lists newenv (xs, ys))

  and unify env x y =
    let v = getval env x in
    let w = getval env y in
      match v, w with
        | Atom m, Atom n -> if m = n then Some env else None
        | Num m, Num n -> if m = n then Some env else None
        | Var "_", _ | _, Var "_" -> Some env
        | Var m, Var n -> Some (if m = n then env else ((m, w) :: env))
        | Var m, _ -> Some ((m, w) :: env)
        | _, Var n -> Some ((n, v) :: env)
        | Struct(m, ps), Struct(n, qs) ->
            if m = n then unify_lists env (ps, qs) else None
        | _ -> None

  let print_var env (name, value) acc =
    (if name.[0] <> '_' then
       (print_string ("\n" ^ name ^ " = ");
        print_term (getval env value);
        false)
     else true)
    && acc

  let print_env_or_not env = List.fold_right (print_var env) env true

  let print_env env =
    (print_env_or_not env) && (print_endline "\nYes\n"; true)

  let ask_more () =
    print_string "\n\nMore (Y/n)? ";
    let line = String.lowercase (read_line ()) in
    let no = (line = "n" || line = "no") in
      if no then print_endline ""; no

  let comparisons = [
    ("<", (<));
    ("<=", (<=));
    (">", (>));
    (">=", (>=));
    ("=:=", (=));
    ("=\\=", (<>))
  ]

  let special nm =
    nm = "=" || nm = "is" || List.mem_assoc nm comparisons

  exception Eval_error of string

  let div n = function
    | 0 -> raise (Eval_error "Division by zero")
    | d -> n / d

  let binops = [
    ("+", ( + ));
    ("-", ( - ));
    ("*", ( * ));
    ("/", div)
  ]

  let rec eval env = function
    | Num n -> n
    | Var nm as x ->
        let v = getval env x in
          if x = v then
            raise (Eval_error ("Uninstantiated variable " ^ nm))
          else eval env v
    | Struct(op, [lhs; rhs]) ->
        (try let f = List.assoc op binops in
           f (eval env lhs) (eval env rhs)
         with Not_found ->
           raise (Eval_error (op ^ "/2 is not a function")))
    | Atom nm | Struct(nm, _) ->
        raise (Eval_error (nm ^ "/x is not a function"))

  let add_parent p = List.map (fun elt -> elt, p)

  exception Cut of term

  exception Stop
  let stop = ref false

  let rec scan goal more_goals env kb = function
    | [] -> false
    | clause :: more_clauses ->
        let head, body = fresh clause in
          (match unify env goal head with
             | None -> scan goal more_goals env kb more_clauses
             | Some newenv ->
                 let new_goals = add_parent goal body in
                   (match (try Some (ask kb newenv (new_goals @ more_goals))
                           with Cut parent ->
                             if parent == goal then None
                             else raise (Cut parent))
                    with None -> false
                      | Some ans -> ans
                          || scan goal more_goals env kb more_clauses))

  and ask kb env = function
    | [] -> (print_env env) || (ask_more ())
    | (goal, parent) :: more_goals ->
        if !stop then raise Stop;
        (match goal with
           | Atom "fail" -> false
           | Atom "!" -> ask kb env more_goals || raise (Cut parent)
           | Var nm as x ->
               let v = getval env x in
                 if x = v then
                   raise (Eval_error ("Uninstantiated variable " ^ nm))
                 else ask kb env ((v, parent) :: more_goals)
           | Struct(s, [lhs; rhs]) when special s ->
               (try let cmp = List.assoc s comparisons in
                  cmp (eval env lhs) (eval env rhs) && ask kb env more_goals
                with Not_found ->
                  let res = if s = "is" then Num (eval env rhs) else rhs in
                    (match unify env lhs res with
                       | None -> false
                       | Some newenv -> ask kb newenv more_goals))
           | _ -> scan goal more_goals env kb kb)

  let error e = prerr_endline ("\n" ^ e ^ "\n")

  let parse_err lexbuf =
    let pos = lexbuf.lex_curr_p in
    let tokbeg = (Lexing.lexeme_start lexbuf) - pos.pos_bol + 1 in
    let tokend = (Lexing.lexeme_end lexbuf) - pos.pos_bol in
    let msg =
      if tokbeg = tokend then
        (Printf.sprintf
           "Parse error on token '%s'; line %i; character %i"
           (Lexing.lexeme lexbuf) pos.pos_lnum tokbeg)
      else
        (Printf.sprintf
           "Parse error on token '%s'; line %i; characters %i-%i"
           (Lexing.lexeme lexbuf) pos.pos_lnum tokbeg tokend) in
      error msg

  let rec loadkb c kb lexbuf =
      match (try Some (main token lexbuf) with
               | Eof -> None
               | Parse_error -> Some Babble) with
        | None -> kb
        | Some (Tell k) -> loadkb c (k :: kb) lexbuf
        | Some Babble -> parse_err lexbuf; []
        | _ -> loadkb c kb lexbuf

  let forget t (head, _) = (unify [] t head) = None

  let help () =
    print_endline
      "\nDirectives:\n\n\
       #dump\n\n\
       \tDisplay all the rules in the working knowledge base (KB).\n\n\
       #load \"<filename>\"\n\n\
       \tRead rules from the file <filename> and add them to the working\n\
       \tknowledge base.  Directives and queries are ignored in files\n\
       \tread by #load.  If an error occurs while parsing the file, then\n\
       \tall of its contents will be ignored.\n\n\
       #save \"<filename>\"\n\n\
       \tSave the working KB to the file <filename>.\n\n\
       #forget <term>.\n\n\
       \tRemove all rules from the working KB whose heads are unifiable\n\
       \twith <term>.  The removed rules are listed.\n\n\
       #help\n\n\
       \tPrint this help.\n"

  let rec runkb kb lexbuf =
    match (try main token lexbuf with Parse_error -> Babble) with
      | Tell k -> runkb (k :: kb) lexbuf
      | Ask goals ->
          stop := false;
          let new_goals = add_parent (Atom "") goals in
            (try if not (ask (List.rev kb) [] new_goals) then
               print_endline "\nNo\n";
             with
               | Eval_error e -> error e
               | Cut _ -> print_endline "\nNo\n"
               | Stop -> print_endline "\nQuery aborted\n"
               | Stack_overflow -> print_endline "\nInsufficient stack\n");
            runkb kb lexbuf
      | Dump ->
          output_kb stdout (List.rev kb);
          flush stdout;
          runkb kb lexbuf
      | Forget t ->
          let newkb, forgotten = List.partition (forget t) kb in
            (match forgotten with
               | [] -> print_endline "\nNothing matched\n"
               | _ ->
                   print_string "\nForgetting about\n";
                   output_kb stdout (List.rev forgotten);
                   flush stdout);
            runkb newkb lexbuf
      | Save nm ->
          (try let c = open_out nm in
             output_kb c (List.rev kb);
             close_out c
           with Sys_error e -> error e);
          runkb kb lexbuf
      | Load nm ->
          let newkb = (try
                         let c = open_in nm in
                         let k = loadkb c [] (from_channel c) in
                           close_in c; k
                       with Sys_error e -> error e; []) in
            runkb (newkb @ kb) lexbuf
      | Help -> help (); runkb kb lexbuf
      | Babble -> parse_err lexbuf; runkb kb lexbuf

  let sigint_handler _ = stop := true

  let run () =
    set_signal sigint (Signal_handle sigint_handler);
    print_endline
      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
       @@@                                                              @@@\n\
       @@@        Welcome to Milog (C) Dan Hipschman 2006, 2007.        @@@\n\
       @@@                                                              @@@\n\
       @@@ For more information about this, and other programs, please  @@@\n\
       @@@  see the author's website at <http://linux.ucla.edu/~dsh>.   @@@\n\
       @@@                                                              @@@\n\
       @@@           To see a list of directives, type #help.           @@@\n\
       @@@                                                              @@@\n\
       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n";
    try runkb [] (from_channel stdin) with
      | Eof -> exit 0

  let _ = run()
}
