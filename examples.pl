
% This file contains examples of supported features including lists,
% arithmetic, and cut.  More advanced features such as op/3 and grammars
% are unsupported at this time.  Milog also uses a restricted syntax.
% For example, strings are not allowed except in directives (see
% below).  Most of these examples are from PROLOG Programming for
% Artificial Intelligence by Ivan Bratko.
%
% With a working OCaml installation, and Markus Mottl's OCamlMakefile,
% you should be able to build a byte-code executable with this command:
%
% $ make -f OCamlMakefile
%
% You may need to edit the path in OCamlMakefile to the main rules file
% if it is incorrect on your system (I'm using a standard Ubuntu 5.10
% installation).  If you don't have an OCaml development environment
% and you don't want to install one, there is a native Linux x86 binary
% included in the tarball containing this file.  You can then run Milog
% with this command:
%
% $ ./milog
%
% If you also have ledit installed, I recommend using it.  Once in the
% interpreter, you can use the following directive to load this file.
%
% #load "examples.pl"
%
% and then try some of the queries included in the following comments.
%
%
% Examples of lists.

conc( [], L, L).
conc( [X|L1], L2, [X|L3]) :- conc(L1, L2, L3).

% Example queries.  There is no special input mode for entering queries.
% Queries simply end with a "?"
%
% conc([a,b],[c,d],L)?
%
% L = [a, b, c, d]
%
% More (Y/n)?
%
% No
%
% conc([a,b],L,[c])?
%
% No
%
% conc([a,b],L,[a,b,c])?
%
% L = [c]
%
% More (Y/n)?
%
% No
%
% conc(L,L,[a,b,a,b])?
%
% L = [a, b]
%
% More (Y/n)?
%
% No
%
% conc(L,L,[a,b,a,b,c])?
%
% No
%
% conc(L1,L2,[a,b,a,b,c])?
%
% L1 = []
% L2 = [a, b, a, b, c]
%
% More (Y/n)?
%
% L1 = [a]
% L2 = [b, a, b, c]
%
% More (Y/n)?
%
% L1 = [a, b]
% L2 = [a, b, c]
%
% More (Y/n)?
%
% L1 = [a, b, a]
% L2 = [b, c]
%
% More (Y/n)?
%
% L1 = [a, b, a, b]
% L2 = [c]
%
% More (Y/n)?
%
% L1 = [a, b, a, b, c]
% L2 = []
%
% More (Y/n)?
%
% No
%
%
% Examples of arithmetic.  Only the operations of addition (+),
% subtraction (-), multiplication (*), and integer division (/) are
% supported, and only numbers and variables are allowed in
% arithmetic expressions.

fac( 0, 1).

fac( N, F) :-
  N > 0,
  N1 is N - 1,
  fac( N1, F1),
  F is N * F1.

rem( X, Y, R) :- Q is X / Y, R is X - Q * Y.

neg( X, N) :- N is 0 - X.

% Example query:
%
% fac( 6, F)?
%
% F = 720
%
% More (Y/n)?
%
% No
%
%
% Examples of using cut.

max( X, Y, M) :- X >= Y, !, M = X.
max( X, Y, M) :- M = Y.

mem1( X, [X|L]) :- !.
mem1( X, [Y|L]) :- mem1( X, L).

% The not (\+) operator is not included, but can be defined:

not( X) :- X, !, fail.
not(_).


% This is the monkey and banana problem:

move( state( middle, onbox, middle, hasnot),
      grasp,
      state( middle, onbox, middle, has)).

move( state( P, onfloor, P, H),
      climb,
      state( P, onbox, P, H)).

move( state( P1, onfloor, P1, H),
      push( P1, P2),
      state( P2, onfloor, P2, H)).

move( state( P1, onfloor, B, H),
      walk( P1, P2),
      state( P2, onfloor, B, H)).

canget( state(_, _, _, has), []).

canget( State, [Action | Actions]) :-
  move( State, Action, NewState),
  canget( NewState, Actions).

%
% canget( state( atdoor, onfloor, atwindow, hasnot), Actions)?
%
% Actions = [walk(atdoor, atwindow), push(atwindow, middle), climb, grasp]
%
% More (Y/n)? n
%
%
% Some more notes:
%
% Disjunction is not supported, so the following is an error:
%
% foo :- bar; baz.
%
% Parse error on token ';'; line 1; character 11
%
% But this shouldn't be too much of a problem since you can separate
% the rules explicitly:
%
% foo :- bar.
% foo :- baz.
%
