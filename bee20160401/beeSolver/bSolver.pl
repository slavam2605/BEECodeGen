% Author: Amit Metodi
% Last Updated: 26/12/2013

:- module(bSolver, [bSolver/0]).
:- nb_setval(bee_useXorClauses, true).
:- ['../beeCompiler/bCompiler'].
:- ['bReader'].
:- ['satSolverInterface'].
:- ['bChecker'].

bSolver:-
    writef('%s\n%s\n%s\n%\n',
           ["%  \\'''/ //      BumbleBEE       / \\_/ \\_/ \\",
            "% -(|||)(')     (26/12/2013)     \\_/ \\_/ \\_/",
            "%   ^^^        by Amit Metodi    / \\_/ \\_/ \\"]),
    flush_output,
    current_prolog_flag(argv, Args),
    (Args=[_,BEEfile|MoreArgs] ->
        (exists_file(BEEfile) ->
            writef('%s',["%  reading BEE file ... "]),flush_output,
            (readBeeFile(BEEfile,BEEmodel) ->
                BEEmodel=[Constrs,Goal,Output],
                writef('done\n'),flush_output,
                (MoreArgs=['-check'] ->
                    writef('%s',["%  Checking BEE model ... \n"]),flush_output,
                    bChecker:checkSolverInput(Constrs,Goal,Output)
                ;
                    %trim_stacks,
                    writef('%s',["%  load pl-satSolver ... "]),flush_output,
                    satSolverInterface:loadSat,
                    %writef('done\n'),flush_output,
                    bSolve(Constrs,Output,Goal)
                )
            ;
                writef('\n\nERROR: Could not read BEE input file (%w) !\n',[BEEfile])
            )
        ;
            writef('\nERROR: Could not find BEE input file (%w) !\n',[BEEfile])
        )
    ;
        writeln('\nUSAGE: BumbleBEE <BEE input file> [-check]')
    ),
    flush_output,
    halt.
bSolver:-halt.


bSolve(Constrs,Output,Goal):-!,
    writef('%s',["%  encoding BEE model ... "]),flush_output,
    (catch(bCompile2satsolver(Constrs,Output,OutVars),unsat,(!,fail)) ->
        writef('done\n%  solving CNF '),flush_output,
        bSolveResults(Goal,Output,OutVars)
    ;
        writeln('done\n=====UNSATISFIABLE=====')
    ),!,
    satSolverKillSolver.


bCompile2satsolver(Constrs,Output,OutVars):-!,
    bParser:parse(Constrs,Bconstrs-[]),!,
    bCompiler:simplify(Bconstrs,normal,SimplBconstrs,_),!,
    bCompiler:decomposeNsimplify(SimplBconstrs,BasicConstrs),!,
    satSolverNewSolver,
    satSolverAddClause([1]),!,
    numberFirstOuput(Output,2,OutVars),!,
    generateCnf2solver(BasicConstrs,OutVars).

% ----- solve goal = satisfy ----- %
bSolveResults(satisfy,Output,_OutVars):-
    writef('(satisfy) ...\n'),flush_output,
    (satSolverSolve ->
        bSolveWriteResults(Output),
        bSolveWriteDONE
    ;
        bSolveWriteUNSAT
    ).

% ----- solve goal = satisfy(X) ----- %
bSolveResults(satisfy(X),Output,OutVars):-
    writef('(satisfy(%w)) ...\n',[X]),flush_output,
    (satSolverSolve ->
        bSolveWriteResults(Output),
        X1 is X - 1,
        bSolveResultsAll(X1,Output,OutVars)
    ;
        bSolveWriteUNSAT
    ).

% ----- solve goal = minimize ----- %

bSolveResults(minimize(X),Output,_OutVars):-!,
    writef('(minimize) ...\n'),flush_output,
    (satSolverSolve ->
        bSolveWriteResults(Output),
        (bcInteger:haveUnaryNumber(X) ->
            bcInteger:getUnaryNumber(X,(_,_,Bits,_)),!,
            reverse(Bits,RBits),
            bSolveResultsMin(RBits,Output)
        ;
            writef('\nERROR: minimize must be done on order encoding integer.\n'),flush_output
        )
    ;
        bSolveWriteUNSAT
    ).

% ----- solve goal = maximize ----- %

bSolveResults(maximize(X),Output,_OutVars):-!,
    writef('(maximize) ...\n'),flush_output,
    (satSolverSolve ->
        bSolveWriteResults(Output),
        (bcInteger:haveUnaryNumber(X) ->
            bcInteger:getUnaryNumber(X,(_,_,Bits,_)),!,
            auxLiterals:litNotReverseXs(Bits,NRBits),
            bSolveResultsMin(NRBits,Output)
        ;
            writef('\nERROR: maximize must be done on order encoding integer.\n'),flush_output
        )
    ;
        bSolveWriteUNSAT
    ).
    
% ----- solve goal = minimize/maximize/all auxiliary ----- %

bSolveResultsAll(0,_Output,_OutVars):-!,
    bSolveWriteDONE.
bSolveResultsAll(X,Output,OutVars):-!,
    (satSolverOtherSol(OutVars) ->
        bSolveWriteResults(Output),
        X1 is X - 1,
        bSolveResultsAll(X1,Output,OutVars)
    ;
        bSolveWriteDONE
    ).

bSolveResultsMin([X|Xs],Output):-!,
    (ground(X) ->
        satSolverGetBoolVal(X,Xval),
        (Xval== -1 ->
            bSolveResultsMin(Xs,Output)
        ;
            Xi is -X,
            ((satSolverAddClause([Xi]),!,satSolverSolve) ->
                bSolveWriteResults(Output),
                bSolveResultsMin(Xs,Output)
            ;
                bSolveWriteDONE
            )
        )
    ;
        auxLiterals:litAsgnFalse(X),
        bSolveResultsMin(Xs,Output)
    ).

bSolveResultsMin([],_):-!,
    bSolveWriteDONE.




% ##################################################################
% # Encode to CNF                                                  #
% ##################################################################
getEncodeFunction(bc([_,_,_,Func|_],_),Func):-!.

generateCnf2solver([Constr|Constrs], VarId):-!,
    getEncodeFunction(Constr,CnfFunc),!,
    (call(CnfFunc, Constr, Cnf-[]) ; throw(bug(encode,CnfFunc))),!,
    writeCnf2solver(Cnf,VarId,NVarID),!,
    generateCnf2solver(Constrs,NVarID).
generateCnf2solver([], VarId):-!,
    satSolverEnsureVarCnt(VarId).

writeCnf2solver(Cnf,VarId,NVarID):-
      term_variables(Cnf, Vars),!,
      numberOutputBits(Vars,VarId,NVarID),!,
      satSolverAddCnf(Cnf),!.

numberFirstOuput([(_Name,Type,Var)|OutputVars],CurVar,StrtVar):-!,
    numberOutputVar(Type,Var,CurVar,UpdVar),
    numberFirstOuput(OutputVars,UpdVar,StrtVar).
numberFirstOuput([],StrtVar,StrtVar):-!.

numberOutputVar(bool,Var,CurVar,UpdVar):-!,
    term_variables(Var,PBits),
    numberOutputBits(PBits,CurVar,UpdVar).
numberOutputVar(int,(int,_,_,Reps),CurVar,UpdVar):-!,
    extractIntRepsBits(Reps,Bits),
    term_variables(Bits,PBits),
    numberOutputBits(PBits,CurVar,UpdVar).
numberOutputVar(bool_array,Bits,CurVar,UpdVar):-!,
    term_variables(Bits,PBits),
    numberOutputBits(PBits,CurVar,UpdVar).
numberOutputVar(int_array,Ints,CurVar,UpdVar):-!,
    collectIntsBits(Ints,IntsBits),
    term_variables(IntsBits,PBits),
    numberOutputBits(PBits,CurVar,UpdVar).

collectIntsBits([(int,_,_,Reps)|Ints],[Bits|IntsBits]):-!,
    extractIntRepsBits(Reps,Bits),
    collectIntsBits(Ints,IntsBits).
collectIntsBits([],[]):-!.

numberOutputBits([X|Bits],CurVar,FinalVar):-!,
   X is CurVar,
   UpdVar is CurVar + 1,
   numberOutputBits(Bits,UpdVar,FinalVar).
numberOutputBits([],FinalVar,FinalVar):-!.

extractIntRepsBits([],[]):-!.
extractIntRepsBits([(_,Data)|Reps],[Data|Bits]):-!,
    extractIntRepsBits(Reps,Bits).

% ##################################################################
% # Write results                                                  #
% ##################################################################
bSolveWriteUNSAT:-!,
    writeln('=====UNSATISFIABLE=====').
bSolveWriteDONE:-!,
    writeln('==========').

bSolveWriteResults([(Name,Type,Var)|Xs]):-!,
    writef('%w = ',[Name]),
    bSolveWriteVar(Type,Var),!,
    nl,
    bSolveWriteResults(Xs).
bSolveWriteResults([]):-!,
    writeln('----------').

bSolveWriteVar(bool,Var):-!,
    satSolverGetBoolVal(Var,Val),
    (Val == 1 ->
        write(true)
    ;
        write(false)
    ).
bSolveWriteVar(int,Int):-!,
    (bcInteger:haveUnaryNumber(Int) ->
        bcInteger:getUnaryNumber(Int,(Min,_,Bits,_)),
		decodeUnaryNumber(Min,Bits,Val),!,
		write(Val) ;
	(bcInteger:haveDirectNumber(Int) ->
        bcInteger:getDirectNumber(Int,(Min,_,Bits,_)),
		decodeDirectNumber(Min,Bits,Val),!,
        write(Val) ;
	write('UNKNOWN REPRESENTATION')
	)).

bSolveWriteVar(bool_array,VarArray):-!,
    write('['),
    bSolveWriteVar_array(VarArray,bool),
    write(']').
bSolveWriteVar(int_array,VarArray):-!,
    write('['),
    bSolveWriteVar_array(VarArray,int),
    write(']').

bSolveWriteVar(_Type,_Var):-
    %% TODO
    write('?').

bSolveWriteVar_array([],_):-!.
bSolveWriteVar_array([X],Type):-!,
    bSolveWriteVar(Type,X).
bSolveWriteVar_array([X|Xs],Type):-!,
    bSolveWriteVar(Type,X),
    write(', '),
    bSolveWriteVar_array(Xs,Type).
	
	
% ########## decode ints ##########
decodeUnaryNumber(Min,[Var|Vars],Val):-
    satSolverGetBoolVal(Var,BVal),
    (BVal == 1 ->
	    Min1 is Min + 1,
        decodeUnaryNumber(Min1,Vars,Val)
    ;
	    Val=Min
	).
decodeUnaryNumber(Min,[],Min):-!.

decodeDirectNumber(Min,[Var|Vars],Val):-
    satSolverGetBoolVal(Var,BVal),
    (BVal == 1 ->
	    Val=Min
    ;
	    Min1 is Min + 1,
        decodeUnaryNumber(Min1,Vars,Val)
	).
