% Author: Amit Metodi
% Last Updated: 14/09/2015

:- module(bcDirectRels, [ ]).
:- use_module('../auxs/auxLiterals').

%% --------------------
%% | lt/gt/leq/geq    |
%% --------------------
:- Head=direct_leq(A,B,ConstrsH-ConstrsT),
   Body=(!,
         bcInteger:getDirectNumber(A,Ad),
         bcInteger:getDirectNumber(B,Bd),
         bcDirectRels:directLeqType(Type),
         bcDirectRels:directLeqSimplify(bc(Type,[Ad,Bd]), Constr, 1),
         (Constr==none ->
             ConstrsH=ConstrsT
         ;
             ConstrsH=[Constr|ConstrsT]
         )
   ),
   bParser:addConstraint(Head,Body).
:- Head=direct_lt(A,B,ConstrsH-ConstrsT),
   Body=(!,
         bcInteger:getDirectNumber(A,Ad),
         bcInteger:getDirectNumber(B,Bd),
         auxDirectnum:directnumAddConst(Ad,1,Ad1),
         bcDirectRels:directLeqType(Type),
         bcDirectRels:directLeqSimplify(bc(Type,[Ad1,Bd]), Constr, 1),
         (Constr==none ->
             ConstrsH=ConstrsT
         ;
             ConstrsH=[Constr|ConstrsT]
         )
   ),
   bParser:addConstraint(Head,Body).

:- Head=direct_gt(A,B,ConstrsH-ConstrsT),
   Body=(!,bParser:direct_lt(B,A,ConstrsH-ConstrsT)),
   bParser:addConstraint(Head,Body).
   
:- Head=direct_geq(A,B,ConstrsH-ConstrsT),
   Body=(!,bParser:direct_leq(B,A,ConstrsH-ConstrsT)),
   bParser:addConstraint(Head,Body).


directLeqType([
         bcDirectRels:directLeqSimplify,
         skipSimplify,
         0,
         bcDirectRels:directLeq2cnf,
         directLeq
        ]):-!.
        
directLeqSimplify(Constr,NewConstr,Changed):-!,
    Constr=bc(Type,[A,B]),
    auxDirectnum:directnumIsChangedOrConst(A,NA,Changed),
    auxDirectnum:directnumIsChangedOrConst(B,NB,Changed),
    (Changed==1 ->
        directLeq_rangeProp(NA,NB,FA,FB,Drop),
        (Drop==1 ->
           NewConstr=none
        ;
           NewConstr=bc(Type,[FA,FB])
        )
    ;
        NewConstr=Constr
    ).

% A <= B
directLeq_rangeProp(A,B, NewA, NewB, Drop):-!,
   A=(Amin,Amax,_),
   B=(Bmin,Bmax,_),
   (Amax =< Bmin ->
       Drop=1 ;
   (Amin == Bmax ->
       Drop=1,
       auxDirectnum:directnumSetMax(A,Bmax,_),
       auxDirectnum:directnumSetMin(B,Amin,_) ;
   (Amin > Bmax ->
       throw(unsat) ;
   (Amin==Amax ->
       Drop=1,
       auxDirectnum:directnumSetMin(B,Amin,_) ;
   (Bmin==Bmax ->
       Drop=1,
       auxDirectnum:directnumSetMax(A,Bmax,_) ;
   (Amin > Bmin ->
       auxDirectnum:directnumSetMin(B,Amin,NB),
       directLeq_rangeProp(A,NB, NewA, NewB, Drop) ;
   (Amax > Bmax ->
       auxDirectnum:directnumSetMax(A,Bmax,NA),
       directLeq_rangeProp(NA,B, NewA, NewB, Drop) ;
   NewA=A,
   NewB=B
   ))))))).


% --------------------------------
% | Encoding (A <= B)            |
% --------------------------------
directLeq2cnf(bc(_,[A,B]), CnfH-CnfT):-!,
    A=(Amin,Amax,Abits,_),
    B=(Bmin,Bmax,Bbits,_),
    Amin =< Bmin,
    DropA is Bmin-Amin,
    auxLists:listDropFrom(DropA,Abits,RAbits),!,
    Akeep is Amax - Bmin + 1,
    Bkeep is Bmax - Bmin + 1,
    auxLists:listKeepFrom(Akeep,RAbits,AVec),!,
    auxLists:listKeepFrom(Bkeep,Bbits,BVec),!,
    directLeqEnc(BVec,AVec,CnfH-CnfT).

    
directLeqEnc([B|Bbits],[_|Abits],CnfH-CnfT):-!,
    nxiORny(Abits,B,CnfH-CnfM),
    directLeqEnc(Bbits,Abits,CnfM-CnfT).
directLeqEnc(_,[],Cnf-Cnf):-!.
directLeqEnc([],_,Cnf-Cnf):-!.

nxiORny([X|Xs],Y,[[-X,-Y]|CnfH]-CnfT):-!,
    nxiORny(Xs,Y,CnfH-CnfT).
nxiORny([],_,Cnf-Cnf):-!.




%% ------------------
%% | eq/neq         |
%% ------------------

:- Head=direct_eq(A,B,Constrs-Constrs),
   Body=(!,bParser:int_eq(A,B,Constrs-Constrs)),
   bParser:addConstraint(Head,Body).
   
:- Head=direct_neq(A,B,ConstrsH-ConstrsT),
   Body=(!,
         bcInteger:getDirectNumber(A,Ad),
         bcInteger:getDirectNumber(B,Bd),
         bcDirectRels:overlapVectors(Ad,Bd,Avec,Bvec),
         (Avec=[] ->
             ConstrsH=ConstrsT
         ;
             bcDirectRels:directNeqType(Type),
             ConstrsH=[bc(Type,[Avec,Bvec])|ConstrsT]
         )
   ),
   bParser:addConstraint(Head,Body).

:- Head=direct_eq_reif(A,B,Z,ConstrsH-ConstrsT),
   Body=(!,bParser:direct_neq_reif(A,B,-Z,ConstrsH-ConstrsT)),
   bParser:addConstraint(Head,Body).
   
:- Head=direct_neq_reif(A,B,Z,ConstrsH-ConstrsT),
   Body=(!,
       (ground(Z) ->
           (Z=:=1 ->
               bParser:direct_neq(A,B,ConstrsH-ConstrsT)
           ;
               bParser:direct_eq(A,B,ConstrsH-ConstrsT)
           ) ;
         bcInteger:getDirectNumber(A,Ad),
         bcInteger:getDirectNumber(B,Bd),
         bcDirectRels:directNeqReifType(Type),
         bcDirectRels:directNeqReifSimplify(bc(Type,[Z,Ad,Bd]),NewConstr,1),
         (NewConstr==none ->
             ConstrsH=ConstrsT
         ;
             ConstrsH=[bc(Type,[Z,Ad,Bd])|ConstrsT]
         ))
   ),
   bParser:addConstraint(Head,Body).


% --------------------------------
% |          (A != B)            |
% --------------------------------

overlapVectors((Amin,Amax,Abits,_),(Bmin,Bmax,Bbits,_),A,B):-
    ((Amin>Bmax ; Bmin>Amax) -> A=[], B=[] ;
    ABmin is max(Amin,Bmin),
    ABmax is min(Amax,Bmax),
    ABkeep is ABmax-ABmin+1,
    Adrop is ABmin - Amin,
    auxLists:listDropFrom(Adrop,Abits,Abits1),
    auxLists:listKeepFrom(ABkeep,Abits1,A),
    Bdrop is ABmin - Bmin,
    auxLists:listDropFrom(Bdrop,Bbits,Bbits1),
    auxLists:listKeepFrom(ABkeep,Bbits1,B)
    ).


directNeqType([
         bcDirectRels:directNeqSimplify,
         skipSimplify,
         0,
         bcDirectRels:directNeq2cnf,
         directNeq
        ]):-!.

directNeqSimplify(Constr,NewConstr,Changed):-!,
    Constr=bc(Type,[Abits,Bbits]),
    directNeqSimplify_loop(Abits,Bbits,FA,FB,Changed,Drop),
    (Changed==1 ->
        ((Drop==1 ; FA=[]) ->
            NewConstr=none
        ;
            NewConstr=bc(Type,[FA,FB])
        )
    ;
        NewConstr=Constr
    ).


directNeqSimplify_loop([A|As],[B|Bs],FA,FB,Changed,Drop):-!,
    (ground(A) ->
        Changed=1,
        (A =:= 1 ->
            litAsgnFalse(B),
            Drop=1 ;
        ((ground(B), B=:=1) ->
             Drop=1 ;
        directNeqSimplify_loop(As,Bs,FA,FB,_,Drop)
        )) ;
    (ground(B) ->
        Changed=1,
        (B =:= 1->
            litAsgnFalse(A),
            Drop=1 ;
        directNeqSimplify_loop(As,Bs,FA,FB,_,Drop)
        ) ;
    FA=[A|MA], FB=[B|MB],
    directNeqSimplify_loop(As,Bs,MA,MB,Changed,Drop)
    )).

directNeqSimplify_loop([],[],[],[],_,_):-!.


directNeq2cnf(bc(_,[A,B]), CnfH-CnfT):-!,
    nxiORnyi(A,B,CnfH-CnfT).
    
nxiORnyi([A|As],[B|Bs],[[-A,-B]|CnfH]-CnfT):-!,
    nxiORnyi(As,Bs,CnfH-CnfT).
nxiORnyi([],[],Cnf-Cnf):-!.



% --------------------------------
% |      (A != B)<->R            |
% --------------------------------
directNeqReifType([
         bcDirectRels:directNeqReifSimplify,
         skipSimplify,
         0,
         bcDirectRels:directNeqReif2cnf,
         directNeqReif
        ]):-!.

directNeqReifSimplify(Constr,NewConstr,Changed):-!,
    Constr=bc(Type,[R,A,B]),
    (ground(R) ->
        (R =:= -1 ->
            auxDirectnum:directnumEquals(A,B),
            Changed=1,
            NewConstr=none
        ;
            bcDirectRels:overlapVectors(A,B,Avec,Bvec),
            (Avec=[] ->
                 NewConstr=none
            ;
                 bcDirectRels:directNeqType(DiffType),
                 NewConstr=bc(DiffType,[Avec,Bvec])
            )
        )
        ;
            auxDirectnum:directnumIsChangedOrConst(A,NA,Changed),
            auxDirectnum:directnumIsChangedOrConst(B,NB,Changed),!,
            (Changed==1 ->
                NA=(AMin,AMax,_),
                NB=(BMin,BMax,_),
                ((AMin>BMax ; AMax<BMin) ->
                    litAsgnTrue(R),
                    NewConstr=none
                ;
                (AMin==AMax ->
                    Skip is AMin - BMin,
                    NB=(_,_,Bvec,_),
                    nth0(Skip,Bvec,BBit),
                    litUnify(BBit,-R),
                    NewConstr=none
                ;
                (BMin==BMax ->
                    Skip is BMin - AMin,
                    NA=(_,_,Avec,_),
                    nth0(Skip,Avec,ABit),
                    litUnify(ABit,-R),
                    NewConstr=none
                ;
                (AMin==BMax ->
                    NA=(_,_,[AMbit|_],_),
                    NB=(_,_,_,[BMbit|_]),
                    bcOr:orType(OrType),
                    auxLiterals:lits2plits([-AMbit,-BMbit],OrVec),
                    bcOr:orSimplify(bc(OrType,[R|OrVec]),NewConstr,_)
                ;
                (AMax==BMin ->
                    NB=(_,_,[BMbit|_],_),
                    NA=(_,_,_,[AMbit|_]),
                    bcOr:orType(OrType),
                    auxLiterals:lits2plits([-AMbit,-BMbit],OrVec),
                    bcOr:orSimplify(bc(OrType,[R|OrVec]),NewConstr,_)
                ;
                NewConstr=bc(Type,[R,NA,NB])
                )))))
            ;
                NewConstr=Constr
            )
        ).

directNeqReif2cnf(bc(_,[R,A,B]), CnfH-CnfT):-!,
    A=(Amin,Amax,Avec,_),
    B=(Bmin,Bmax,Bvec,_),
    (Amin=<Bmin ->
        Bmin1 is Bmin -1,
        neqReif2cnf_aux1(Amin,Bmin1,Avec,R,LAvec,CnfH-CnfM),
        (Amax=<Bmax ->
             neqReif2cnf_aux2(Bmin,Amax,LAvec,Bvec,R,LBvec,CnfM-CnfX),
             Amax1 is Amax+1,
             neqReif2cnf_aux1(Amax1,Bmax,LBvec,R,_,CnfX-CnfT)
        ;
             neqReif2cnf_aux2(Bmin,Bmax,Bvec,LAvec,R,LLAvec,CnfM-CnfX),
             Bmax1 is Bmax+1,
             neqReif2cnf_aux1(Bmax1,Amax,LLAvec,R,_,CnfX-CnfT)
        )
    ;
        Amin1 is Amin - 1,
        neqReif2cnf_aux1(Bmin,Amin1,Bvec,R,LBvec,CnfH-CnfM),
        (Amax=<Bmax ->
            neqReif2cnf_aux2(Amin,Amax,Avec,LBvec,R,LLBvec,CnfM-CnfX),
            Amax1 is Amax + 1,
            neqReif2cnf_aux1(Amax1,Bmax,LLBvec,R,_,CnfX-CnfT)
        ;
            neqReif2cnf_aux2(Amin,Bmax,LBvec,Avec,R,LAvec,CnfM-CnfX),
            Bmax1 is Bmax + 1,
            neqReif2cnf_aux1(Bmax1,Amax,LAvec,R,_,CnfX-CnfT)
        )
    ).

neqReif2cnf_aux1(I,J,Xs,_,Xs,Cnf-Cnf):-I>J,!.
neqReif2cnf_aux1(I,J,[X|Xs],R,LXs,[[-X,R]|CnfH]-CnfT):-!,
    I1 is I + 1,
    neqReif2cnf_aux1(I1,J,Xs,R,LXs,CnfH-CnfT).

neqReif2cnf_aux2(I,J,_,Ys,_,Ys,Cnf-Cnf):-I>J,!.
neqReif2cnf_aux2(I,J,[X|Xs],[Y|Ys],R,LYs,CnfH-CnfT):-!,
    I1 is I + 1,
    CnfH=[[-X,-Y,-R],[-X,Y,R],[X,-Y,R]|CnfM],
    neqReif2cnf_aux2(I1,J,Xs,Ys,R,LYs,CnfM-CnfT).

% --------------------------------
% |       all-diffrenet          |
% --------------------------------
:- Head=direct_array_allDiff(Ints,ConstrsH-ConstrsT),
   Body=(!,
         ((Ints=[] ; Ints=[_]) ->
             ConstrsH=ConstrsT
         ;
             bcDirectRels:array_allDiff_decompose(Ints,ConstrsH-ConstrsT)
         )
   ),
   bParser:addConstraint(Head,Body).

array_allDiff_decompose([X|Xs],ConstrsH-ConstrsT):-!,
    array_allDiff_decompose_aux(Xs,X,ConstrsH-ConstrsM),
    array_allDiff_decompose(Xs,ConstrsM-ConstrsT).
array_allDiff_decompose([],Constrs-Constrs):-!.

array_allDiff_decompose_aux([Y|Ys],X,ConstrsH-ConstrsT):-!,
    bParser:direct_neq(X,Y,ConstrsH-ConstrsM),!,
    array_allDiff_decompose_aux(Ys,X,ConstrsM-ConstrsT).
array_allDiff_decompose_aux([],_,Constrs-Constrs):-!.