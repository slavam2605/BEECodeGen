% Author: Amit Metodi
% Last Updated: 07/06/2013

:- module(bcITE, [ ]).
:- use_module('../auxs/auxLiterals').

%%% ------------------------- %%%
%%% add constraints to parser %%%
%%% ------------------------- %%%
:- Head=bool_ite(If,Then,Else,ConstrsH-ConstrsT),
   Body=(
       !,
       bcITE:iteType(ITEType),
       bcITE:iteSimplify(bc(ITEType,[If,Then,Else]),Constr,_),
       (Constr==none ->
           ConstrsH=ConstrsT
       ;
           ConstrsH=[Constr|ConstrsT]
       )
   ),
   bParser:addConstraint(Head,Body).

:- Head=bool_ite_reif(If,Then,Else,Reif,ConstrsH-ConstrsT),
   Body=(
       !,
       bcITE:iteReifType(ITEType),
       bcITE:iteReifSimplify(bc(ITEType,[If,Then,Else,Reif]),Constr,_),
       (Constr==none ->
           ConstrsH=ConstrsT
       ;
           ConstrsH=[Constr|ConstrsT]
       )
   ),
   bParser:addConstraint(Head,Body).

%%% ------------------------- %%%
%%% constraints types         %%%
%%% ------------------------- %%%
iteType([
        bcITE:iteSimplify,
        skipSimplify,
        0,
        bcITE:ite2cnf,
        ite
       ]):-!.

iteReifType([
        bcITE:iteReifSimplify,
        skipSimplify,
        0,
        bcITE:iteReif2cnf,
        ite
       ]):-!.

% -------------------------------
% | Simplify predicates         |
% -------------------------------
	   
iteSimplify(Constr,NewConstr,Changed):-!,
    Constr=bc(_Type,[If,Then,Else]),
    (ground(If) ->
        Changed=1,
		NewConstr=none,
        (If =:= 1 ->
           litAsgnTrue(Then)
        ;
           litAsgnTrue(Else)
        ) ;
	((ground(Then), Then =:= -1) ->
	    Changed=1,
		NewConstr=none,
		litAsgnTrue(Else),
		litAsgnFalse(If) ;
	((ground(Else), Else =:= -1) ->
	    Changed=1,
		NewConstr=none,
		litAsgnTrue(Then),
		litAsgnTrue(If) ;
	(litIsEqual(Then,Else) ->
	    Changed=1,
		NewConstr=none,
		litAsgnTrue(Then) ;
    NewConstr=Constr
    )))).

	
	
iteReifSimplify(Constr,NewConstr,Changed):-!,
    Constr=bc(_Type,[If,Then,Else,Reif]),
	(ground(Reif) ->
        Changed=1,
		iteType(ITEType),
	    (Reif =:= 1 ->
	       iteSimplify(bc(ITEType,[If,Then,Else]),NewConstr,_)
		;
	       iteSimplify(bc(ITEType,[If,-Then,-Else]),NewConstr,_)
		)
	;
	(ground(If) ->
	    Changed=1,
		NewConstr=none,
		(If =:= 1 ->
		    litUnify(Then,Reif)
		;
		    litUnify(Else,Reif)
		)
	;
	(litIsEqual(Then,Else) ->
	    Changed=1,
		NewConstr=none,
        litUnify(Then,Reif) ;
	NewConstr=Constr
	))).

% -------------------------------
% | Encode predicates           |
% -------------------------------

ite2cnf(bc(_,[If,Then,Else]),CnfH-CnfT):-!,
    CnfH=[
	      [-If, Then],
		  [ If, Else],
		  [Then,Else]
		  |CnfT].

iteReif2cnf(bc(_,[If,Then,Else,Reif]),CnfH-CnfT):-!,
    CnfH=[
	      [-Reif,-If, Then],
		  [-Reif, If, Else],
		  [-Reif,Then,Else],
	      [ Reif,-If,-Then],
		  [ Reif, If,-Else],
		  [ Reif,-Then,-Else]
		  |CnfT].
