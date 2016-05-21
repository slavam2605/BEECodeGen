     PL-SATsolver by Amit Metodi
-------------------------------------

PL-SATsolver is a (SWI) Prolog interface to MiniSAT based
SAT Solvers which was originally described in the paper: 
Logic Programming with Satisfiability; 
Michael Codish, Vitaly Lagoon and Peter Stuckey.

PL-SATsolver have several predicates which receives CNF,
solve it with a SAT solver, and assign the variables with
satisfied assignment if exists.

PL-SATsolver supports four SAT solvers:
1. MiniSAT v2.0.2 by Niklas Een, Niklas Sorensson.
2. CryptoMinisat 2.5.1 by Mate Soos.
3. Glucose v2.2 by Gilles Audemard, Laurent Simon.
4. Glucose v4.0 by Gilles Audemard, Laurent Simon.

PL-SATsolver web-page: http://amit.metodi.me/research/plsatsolver/



Compile PL-SATsolver
-------------------------
PL-SATsolver requires compilation.
Compilation requires SWI-Prolog version 6.0.x (or above)
which is accessible for any folder.
(Previous version of SWI-Prolog such as 5.6.x - 5.10.x
may work but require changes in the 'configure' and 'makefile',
such as calling 'pl' instead of 'swipl', for each individual SAT solver).


Linux:
------
Complication is done by using the attached makefile.
To compile all four SAT solver execute "> make satSolvers"
from "satsolver_src" folder.

The makefile will create "satsolver" folder (next to satsolver_src),
and compile the requested SAT solvers to it.

Windows:
--------
Executing one (or all) of the four 'make-<solver>.cmd' files
will compile the requested SAT solver using 'swipl-ld'.

Note that recent SWI-Prologs (v6.6.x) compilation is done by 
calling 'g++' by default and therefore the compilation requires 
installation of MinGW.




At the end of the compilation process 'satsolver' folder
will contain the following files:
'satsolver.pl'      - Prolog interface.
'pl-minisat.so'     - MiniSAT v2.0.2 library.
'pl-crypminisat.so' - CryptoMinisat v2.5.1 library.
'pl-glucose.so'     - Glucose v2.2 library.
(In Windows .dll files instead of .so files).



Using PL-SATsolver:
-------------------------

Prolog and CNF
-------------------------
A formula in conjunctive normal form is represented as a list of list of literals (a literal is a variable or its negation).
The elements of the outer list are viewed as a conjunction. Each inner list is a disjunction.
For example the CNF formula (A+(-B))*((-A)+B) is represented as [[A,-B],[-A,B]].

PL-CryptoMinisat supports XOR clauses by adding the atom 'x' at the beginning of the clause list.
For example the CNF formula ((A xor B xor C)*(A+B)*((-B)+(-C))) is represented as [[x,A,B,C],[A,B],[-B,-C]]].


Usage
-------------------------
To solve a CNF using PL-SATsolver, first consult 'satsolver/satsolver.pl' file.
Then use one of the pre-defined predicates, such as 'sat(Cnf)', to solve 'Cnf'
using one of the SAT solver (the default SAT solver is CryptoMinisat).

For example:
?- [satsolver].
% SWI-Prolog interface to CryptoMiniSAT v2.5.1 ... OK
% satsolver compiled into satsolver 0.06 sec, 1,305 clauses
true. 
?- sat([[A,-B],[-A,B]]).
A= -1, B= -1.


To use different SAT solver call 'nb_satval(satSolver_module,<value>)' before
consulting 'satsolver.pl'. Value can be: 'cryptominisat', 'minisat', or 'glucose'.

?- nb_setval(satSolver_module, glucose).
?- [satsolver].
% SWI-Prolog interface to Glucose v2.2 ... OK
% satsolver compiled into satsolver 0.05 sec, 1,305 clauses
true. 
?- sat([[A,-B],[-A,B]]).
A= -1, B= -1.




For more examples and information visit: http://amit.metodi.me/research/plsatsolver/