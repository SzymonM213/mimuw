% Szymon Mrozicki 439978

:- ensure_loaded(library(lists)).
:- op(700, xfx, <>).

valid_filename(ProgramFile) :-
    catch(see(ProgramFile), _, fail), % try to open file
    seen.                             % close file

parse_program(ProgramFile, Program) :-
    open(ProgramFile, read, Stream),
    read(Stream, variables(Variables)),
    read(Stream, arrays(Arrays)),
    read(Stream, program(Instructions)),
    close(Stream),
    Program = program(Variables, Arrays, Instructions).

% constList(Value, List) - List is a list of the same value Value
constList(Value, [Value]).
constList(Value, [Value | T]) :-
    constList(Value, T).

% initState(Program, N, State) - State is an initial state of the program.
% My states have: list of variables and arrays (both with their values), list of
% positions of each process (which instruction is currently executed) and
% id of the active process
initState(program(Variables, Arrays, _), N, 
          state(Vars, Arrs, Positions, PrId)) :-
    length(Positions, N),
    constList(1, Positions),
    createVars(Variables, Vars),
    createArrays(Arrays, N, Arrs),
    PrId = 0.

createVars([], []).
createVars([Var|Vars], [(Var, 0)|T]) :-
    createVars(Vars, T).

createArrays([], _, []).
createArrays([Array|Arrays], N, [(Array, ArrayState)|T]) :-
    length(ArrayState, N),
    constList(0, ArrayState),
    createArrays(Arrays, N, T).

% replace(List, Index, Value, NewList) - NewList is a list List with Value
% at index Index
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

% step(Program, StanWe, StanWy) - StanWy is a state after executing the next
% instruction in the state StanWe
step(program(_, _, Instructions), 
     state(Vars, Arrs, Positions, PrId), StanWy) :-
    nth0(PrId, Positions, PrPosition),
    nth1(PrPosition, Instructions, Instruction),
    executeInstruction(Instruction, state(Vars, Arrs, Positions, PrId), StanWy).

% executeInstruction(Instruction, Program, State, NewState) - NewState is a 
% state after executing the Instruction in the State
executeInstruction(assign(array(Var,IndexExpr),Expr), 
                   state(Vars, Arrs, Positions, PrId), StanWy) :-
    evalExpr(IndexExpr, Vars, Arrs, PrId, Index),
    evalExpr(Expr, Vars, Arrs, PrId, Value),
    updateArr(Arrs, Var, Index, Value, ArrsNew),
    nth0(PrId, Positions, PrPosition),
    NewPrPosition is PrPosition + 1,
    replace(Positions, PrId, NewPrPosition, NewPositions),
    StanWy = state(Vars, ArrsNew, NewPositions, PrId).

executeInstruction(assign(Var, Expr), state(Vars, Arrs, Positions, PrId), 
                   StanWy) :-
    evalExpr(Expr, Vars, Arrs, PrId, Value),
    updateVar(Var, Value, Vars, VarsNew),
    nth0(PrId, Positions, PrPosition),
    NewPrPosition is PrPosition + 1,
    replace(Positions, PrId, NewPrPosition, NewPositions),
    StanWy = state(VarsNew, Arrs, NewPositions, PrId).

executeInstruction(goto(NewPosition), state(Vars, Arrays, Positions, PrId), 
                   StanWy) :-
    replace(Positions, PrId, NewPosition, NewPositions),
    StanWy = state(Vars, Arrays, NewPositions, PrId).
    
executeInstruction(sekcja, state(Vars, Arrays, Positions, PrId), StanWy) :-
    nth0(PrId, Positions, PrPosition),
    NewPrPosition is PrPosition + 1,
    replace(Positions, PrId, NewPrPosition, NewPositions),
    StanWy = state(Vars, Arrays, NewPositions, PrId).

executeInstruction(condGoto(Expr, NewPosition), 
                   state(Vars, Arrs, Positions, PrId), StanWy) :-
    evalExpr(Expr, Vars, Arrs, PrId, true),
    replace(Positions, PrId, NewPosition, NewPositions),
    StanWy = state(Vars, Arrs, NewPositions, PrId).

executeInstruction(condGoto(Expr, _), state(Vars, Arrs, Positions, PrId),
                   StanWy) :-
    nth0(PrId, Positions, PrPosition),
    evalExpr(Expr, Vars, Arrs, PrId, false),
    NewPrPosition is PrPosition + 1,
    replace(Positions, PrId, NewPrPosition, NewPositions),
    StanWy = state(Vars, Arrs, NewPositions, PrId).

% evalExpr(Expr, Vars, Arrs, PrId, Value) - Value is evaluated expression Expr
% in the context of variables Vars, arrays Arrs and process with id PrId
evalExpr(Num, _, _, _, Num) :-
    number(Num).

evalExpr(pid, _, _, PrId, PrId).

evalExpr(Var, Vars, _, _, Value) :-
    atom(Var),
    Var \= pid,
    member((Var, Value), Vars).

evalExpr(array(ArrName, IndexExpr), Vars, Arrs, PrId, Value) :-
    evalExpr(IndexExpr, Vars, Arrs, PrId, Index),
    member((ArrName, Arr), Arrs),
    nth0(Index, Arr, Value).

evalExpr(Var <> IndexExpr, Vars, Arrs, PrId, Value) :-
    evalExpr(IndexExpr, Vars, Arrs, PrId, Index),
    member((Var, Arr), Vars),
    nth0(Index, Arr, Value).

evalExpr(Var1 + Var2, Vars, Arrs, PrId , Value) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value is Value1 + Value2.

evalExpr(Var1 - Var2, Vars, Arrs, PrId , Value) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value is Value1 - Value2.

evalExpr(Var1 * Var2, Vars, Arrs, PrId , Value) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value is Value1 * Value2.

evalExpr(Var1 / Var2, Vars, Arrs, PrId, Value) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value is Value1 // Value2.

evalExpr(Var1 < Var2, Vars, Arrs, PrId, true) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value1 < Value2.

evalExpr(Var1 < Var2, Vars, Arrs, PrId, false) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value1 >= Value2.

evalExpr(Var1 = Var2, Vars, Arrs, PrId, true) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value1 =:= Value2.

evalExpr(Var1 = Var2, Vars, Arrs, PrId, false) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value1 =\= Value2.

evalExpr(Var1 <> Var2, Vars, Arrs, PrId, true) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value1 =\= Value2.

evalExpr(Var1 <> Var2, Vars, Arrs, PrId, false) :-
    evalExpr(Var1, Vars, Arrs, PrId, Value1),
    evalExpr(Var2, Vars, Arrs, PrId, Value2),
    Value1 =:= Value2.

% updateVar(Var, Value, Vars, VarsNew) - VarsNew is a list Vars with updated
% value of variable Var to Value
updateVar(_, _, [], []).
updateVar(Var, Value, [(Var, _) | T], [(Var, Value) | T]).
updateVar(Var, Value, [H | T], [H | TNew]) :-
    H \= (Var, _),
    updateVar(Var, Value, T, TNew).

% updateArr(Arrs, ArrName, Index, Value, ArrsNew) - ArrsNew is a list Arrs with
% updated value of array ArrName at index Index to Value
updateArr([], _, _, _, []).
updateArr([(ArrName, Arr) | T], ArrName, Index, 
          Value, [(ArrName, NewArr) | T]) :-
    replace(Arr, Index, Value, NewArr).
updateArr([H | T], ArrName, Index, Value, [H | TNew]) :-
    H \= (ArrName, _),
    updateArr(T, ArrName, Index, Value, TNew).

% getInSection(Program, State, P1, P2) - P1 and P2 are processes in the section
% in the state State, or -1 if there is no collision
getInSection(program(_, _, Instructions), state(_, _, Positions, _), P1, P2) :-
    nth0(P1, Positions, P1Pos),
    nth0(P2, Positions, P2Pos),
    nth1(P1Pos, Instructions, sekcja),
    nth1(P2Pos, Instructions, sekcja),
    P1 \= P2.

getInSection(_, _, -1, -1).

% writePath(Path) - writes the path of the program execution
writePath([]).
writePath([state(_, _, Positions, PrId) | T]) :-
    nth0(PrId, Positions, PrPosition),
    write('   Proces '), write(PrId), write(': '), write(PrPosition), nl,
    writePath(T).

collision(Path, P1, P2) :-
    write('Program jest niepoprawny.\n'),
    write('Niepoprawny przeplot:\n'),
    writePath(Path),
    write('Procesy w sekcji: '), write(P1), write(', '), write(P2), nl.

% dfs(N, Program, Vertex, VisitedVertices, Path, NewVisitedVertices, Unsafe)- 
% depth-first search algorithm to verify every achievable state of the program.
% Vertices are states of the program and edges are changes of the active process
% or execution of the next instruction of the active process
dfs(_, _, State, Visited, _, Visited, _) :-
    member(State, Visited).              % Vertex already visited

dfs(N, Program, state(Vars, Arrays, Positions, PrId), Visited, Path, 
    NewVisited, Unsafe) :-
    (
        \+ member(state(Vars, Arrays, Positions, PrId), Visited),
        getInSection(Program, state(Vars, Arrays, Positions, PrId),
                     P1, P2),
        (P2 >= 0, collision(Path, P1, P2), Unsafe = true ;
        (
            ChangePrId is (PrId + 1) mod N,
            ChangePrIdState = state(Vars, Arrays, Positions, ChangePrId),
            NewVisited1 = [state(Vars, Arrays, Positions, PrId) | Visited],
            % change active process
            dfs(N, Program, ChangePrIdState, NewVisited1, Path, 
                NewVisited2, Unsafe),
            append(Path, [state(Vars, Arrays, Positions, PrId)], NewPath),
            step(Program, state(Vars, Arrays, Positions, PrId), NewState),
            % execute next instruction of the active process
            dfs(N, Program, NewState, NewVisited2, NewPath, NewVisited, Unsafe)
        ))
    ).

dfs(_, _, _, Visited, _, Visited, _).

% Main predicate to verify the safety of the program
verify(_, ProgramFile) :-
    \+ valid_filename(ProgramFile),
    write('Error: brak pliku o nazwie - '), write(ProgramFile), write('\n').

verify(N, _) :-
    N < 1,
    write('Error: parametr '), write(N), write(' powinien byc liczba > 0\n').

verify(N, ProgramFile) :-
    parse_program(ProgramFile, Program),
    initState(Program, N, InitialState),
    dfs(N, Program, InitialState, [], [], _, Unsafe),
    (Unsafe = false, write('Program jest poprawny (bezpieczny).\n') ; 
    Unsafe = true), 
    !.
