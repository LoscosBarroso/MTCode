
:- module(_,_,[datafacts,hiord]).

:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(engine(runtime_control)).


:- meta_predicate(record_call(goal,?,?)).

:- data(recorded_calls/6).
:- data(recording/1). % debugging data for is_recording/1
:- data(notified/3).
:- data(total_time/1).

log(Module,Print,X):-
    display('I was asked to record '), display(Module), display(':'), display(Print),
    display(' inside a call to '), display(X), display('. Not recorded.'), nl.

record_call(Pred,Print,Module):-
    recording(X),!,
    (call(Pred) -> Succ=true;Succ=false ),
    %( '$metachoice'(C1),call(Pred),'$metachoice'(C2) -> Succ=true;Succ=false ),
    %( C1 == C2 -> true ; throw(error(nondet(Name,A), record_call/3)) ),
    %log(Module,Print,X),
    Succ=true.

record_call(Pred,Print,Module):-
    assertz_fact(recording(Pred)),
    statistics('walltime',[T1,_]),
    (call(Pred) -> Succ=true;Succ=false ),
    %( '$metachoice'(C1),call(Pred),'$metachoice'(C2) -> Succ=true;Succ=false ),
    statistics('walltime',[T2,_]),
    retractall_fact(recording(_)),
    T is T2 - T1,
    functor(Print,Name,A),
    %( C1 == C2 -> true; tell_non_det(Module,Name,A)),
    add_record(Name,A,T,Module),
    Succ=true.

add_record(FullName,A,T,M):-
    atom_concat(Name,'$_auxiliar',FullName),
    retract_fact(recorded_calls(M,Name,A,N,OT,_)), !,
    N1 is N + 1,
    OT1 is OT + T,
    Avg1 is OT1 / N1,
    assertz_fact(recorded_calls(M,Name,A,N1,OT1,Avg1)).
add_record(FullName,A,T,M):-
    atom_concat(Name,'$_auxiliar',FullName),
    assertz_fact(recorded_calls(M,Name,A,1,T,T)).

gen_to_print(Ls, T) :-
    % [(F,A) | Ls]
    ( member((F,A), Ls),
      functor(Call, F, A),
      call(Call),
      T = Call,
      pprint(T), nl,
      add_time(T),
      fail
    ;
        true
    ).

list_recorded_calls:-
    gen_to_print([
        (recorded_calls,6)],_),
    nl, display('Total recorded time: '),
    total_time(X), display(X), display(' ms.\n').

tell_non_det(M,N,A) :- notified(M,N,A),!.
tell_non_det(M,N,A) :-
    asserta_fact(notified(M,N,A)),
    display(nondet(M,N,A)),nl.

add_time(recorded_calls(_Module,_Name,_Arity,_NumCalls, TotalT,_AvgT)):-
    total_time(X),!,
    X1 is X + TotalT,
    retractall_fact(total_time(_)),
    asserta_fact(total_time(X1)).
add_time(recorded_calls(_Module,_Name,_Arity,_NumCalls, TotalT,_AvgT)):-
    asserta_fact(total_time(TotalT)).

clean_records:-
    retractall_fact(recorded_calls(_,_,_,_,_,_)),
    retractall_fact(recording(_)),
    retractall_fact(total_time(_)),
    retractall_fact(notified(_,_,_)).


% TODO: Set name width to 30 spaces.
%       Formatting.
pprint(recorded_calls(Module,Name,Arity,NumCalls, TotalT, AvgT)):-
    display(Module), display(': '),
    display(Name), display('/'), display(Arity), display(':\n'), display(NumCalls),
    display(' calls \t'), display(TotalT), display(' ms.\t\t'), display(AvgT),
    display(' ms/call'),!.
pprint(X):-display(X).