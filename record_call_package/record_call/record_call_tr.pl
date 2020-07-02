:- module(_, _, [datafacts]).

:- use_module(library(compiler/c_itf), [defines_module/2, decl/2]).
:- use_module(library(streams)).

:- data use_record_call/3.
:- data recorded/3.

record_call_sentence_tr(0,_,_M) :- !, fail.

record_call_sentence_tr(end_of_file,
                        [
                            (:- use_module(library(record_call/record_call_rt), [record_call/3])),
                            end_of_file
                        ]
                        , M) :- !,
    clean_db(M). % cleans all data used

record_call_sentence_tr((:- use_record_call(Pred/Arity)),(:- use_record_call(Pred/Arity)),M) :- !,
    assertz_fact(use_record_call(Pred,Arity,M)),
    display(M), display(': '), display(Pred), display('/'), display(Arity),nl. % assertz data use_record_calls/1 for each pred

record_call_sentence_tr((:- _Decl),_,_M) :- !, fail.

record_call_sentence_tr((Head :- Cls),New,M) :- !,
    functor(Head,Name,Arity),
    use_record_call(Name,Arity,M),
    Head =.. [Name|Args],
    atom_concat(Name,'$_auxiliar',Name1),
    NewHead =.. [Name1|Args],
    build_substitution(Head,Name,Arity,NewHead,Cls,New,M).

record_call_sentence_tr(_Fact,_,_M) :- !,  fail. % same as normal clause

record_call_goal_tr(Goal,NewGoal,M):-!,
    functor(Goal,Name,Arity),
    pred_record_call(M,Name,Arity),
    Goal =.. [Name|Args],
    atom_concat(Name,'$_auxiliar',Name1),
    NewGoal =.. [Name1|Args].


build_substitution(_,Name,Arity,NewHead,Cls,[(NewHead :- Cls)],M):-
    recorded(M,Name,Arity),!.
build_substitution(Head,Name,Arity,NewHead,Cls,[Clause, (NewHead :- Cls)],M):-
    assertz_fact(recorded(M,Name,Arity)),
    Clause = (Head :- record_call(NewHead,NewHead,M)).


use_record_calls([],_):-!.
use_record_calls([(P/A)|Preds],M):-
    assertz_fact(use_record_call(M,P,A)),
    use_record_calls(Preds,M).

clean_db(M):-
    retractall_fact(use_record_call(M,_,_)),
    retractall_fact(recorded(M,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Needed, but don't know why:

pred_record_call(Mod, F, A) :-
    ( defines_module(Base, Mod) -> true ; fail ),
    decl(Base, use_record_call(F/A)).