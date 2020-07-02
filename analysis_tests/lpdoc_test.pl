:- module(_,_,[]).

:- use_module(lpdoc_problem).
:- use_module(ciaopp(ciaopp)).
:- use_module(library(record_call/record_call_rt)).
:- use_module(engine(runtime_control)).
:- use_module(library(streams)).
:- use_module(library(numlists)).
:- use_module(typeslib(typeslib)).

/*This predicate calls the specified analysis domain, the specified amount of times
  and automatically dispolays the times of each stage of every  execution, the 
  average time taken and the standard deviation*/
main :- test(eterms,20).

t(Domain) :-
    cleanup_types,
    statistics('walltime',[T1,_]), module(lpdoc_problem), statistics('walltime',[T2,_]),
    T is T2 -T1, display(T), display(' ms.'), nl, nl, 
    
    nl, statistics('walltime',[T3,_]), display(Domain), nl, analyze(Domain),
    statistics('walltime',[T4,_]), TT is T4 -T3, display(TT), display(' ms.'),
    nl, nl, 
    
    statistics('walltime',[T5,_]), display('output'), nl, output,
    statistics('walltime',[T6,_]), TTT is T6 -T5, display(TTT), display(' ms.'),
    nl, nl.

t_stat(Domain,T,TT,TTT, Total) :-
    cleanup_types,
    statistics('walltime',[T1,_]), module(lpdoc_problem), statistics('walltime',[T2,_]),
    T is T2 -T1, display(T), display(' ms.'), nl, nl, 
    
    nl, statistics('walltime',[T3,_]), display(Domain), nl, analyze(Domain),
    statistics('walltime',[T4,_]), TT is T4 -T3, display(TT), display(' ms.'),
    nl, nl, 
    
    statistics('walltime',[T5,_]), display('output'), nl, output,
    statistics('walltime',[T6,_]), TTT is T6 -T5, display(TTT), display(' ms.'),
    nl, nl,

    Total is T + TT + TTT.

t_stats(_,0,[],[],[],[]).
t_stats(Domain,N,[T|Ts],[TT|TTs],[TTT|TTTs],[Total|Totals]) :-
    t_stat(Domain,T,TT,TTT,Total),
    N1 is N - 1,
    t_stats(Domain,N1,Ts,TTs,TTTs,Totals).

test(Domain,N):-
    t_stats(Domain,N,Ts,TTs,TTTs,Totals),
    std_dev(N,Ts,AT,ST),
    std_dev(N,TTs,ATT,STT),
    std_dev(N,TTTs,ATTT,STTT),
    std_dev(N,Totals,ATotal,STotal),
    nl,display('------------------------------------------------------------------------------------'), nl,
    display('Set Up: '), display(AT), display(' ms. in average +- '), display(ST), display(' std. deviation.'),nl,
    display(Ts),nl,nl,
    display('Analysis: '),display(ATT), display(' ms. in average +- '), display(STT), display(' std. deviation.'),nl,
    display(TTs),nl,nl,
    display('Output: '),display(ATTT), display(' ms. in average +- '), display(STTT), display(' std. deviation.'),nl,
    display(TTTs),nl,nl,
    display('Total: '),display(ATotal), display(' ms. in average +- '), display(STotal), display(' std. deviation.'),nl,
    display(Totals),nl,nl,
    display(AT), display(' '),display(ST), display(' '),
    display(ATT), display(' '),display(STT), display(' '),
    display(ATTT), display(' '),display(STTT), display(' '),
    display(ATotal), display(' '),display(STotal).
avg(N,List,AT):-
    sum_list(List,T),
    AT is T/N.

std_dev(N,List,Avg,Std):-
    avg(N,List,Avg),
    sq_dev(List,Avg,SqDevs),
    sum_list(SqDevs,S),
    Std is sqrt(S/N).

sq_dev([],_,[]).
sq_dev([N|Ns],Avg,[Sq|Sqs]):-
    D is N-Avg,
    Sq is D*D,
    sq_dev(Ns,Avg,Sqs).

/*typeanalysis(eterms).
typeanalysis(ptypes).
typeanalysis(svterms).
typeanalysis(terms).
typeanalysis(deftypes).*/


/*The following predicates use the record_call package to display detailed information
  on the cost of monitored predicates during each fase of the analysis.*/
eterms :-
    statistics('walltime',[T1,_]), module(lpdoc_problem), statistics('walltime',[T2,_]),
    T is T2 -T1, display(T), display(' ms.'), nl, nl, list_recorded_calls, clean_records,
    
    nl, statistics('walltime',[T3,_]), display('eterms'), nl, analyze(eterms),
    statistics('walltime',[T4,_]), TT is T4 -T3, display(TT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records, nl,
    
    statistics('walltime',[T5,_]), display('output'), nl, output,
    statistics('walltime',[T6,_]), TTT is T6 -T5, display(TTT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records.

ptypes :-
    statistics('walltime',[T1,_]), module(lpdoc_problem), statistics('walltime',[T2,_]),
    T is T2 -T1, display(T), display(' ms.'), nl, nl, list_recorded_calls, clean_records,
    
    nl, statistics('walltime',[T3,_]), display('ptypes'), nl, analyze(ptypes),
    statistics('walltime',[T4,_]), TT is T4 -T3, display(TT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records, nl,
    
    statistics('walltime',[T5,_]), display('output'), nl, output,
    statistics('walltime',[T6,_]), TTT is T6 -T5, display(TTT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records.

svterms :-
    statistics('walltime',[T1,_]), module(lpdoc_problem), statistics('walltime',[T2,_]),
    T is T2 -T1, display(T), display(' ms.'), nl, nl, list_recorded_calls, clean_records,
    
    nl, statistics('walltime',[T3,_]), display('svterms'), nl, analyze(svterms),
    statistics('walltime',[T4,_]), TT is T4 -T3, display(TT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records, nl,
    
    statistics('walltime',[T5,_]), display('output'), nl, output,
    statistics('walltime',[T6,_]), TTT is T6 -T5, display(TTT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records.

terms :-
    statistics('walltime',[T1,_]), module(lpdoc_problem), statistics('walltime',[T2,_]),
    T is T2 -T1, display(T), display(' ms.'), nl, nl, list_recorded_calls, clean_records,
    
    nl, statistics('walltime',[T3,_]), display('terms'), nl, analyze(terms),
    statistics('walltime',[T4,_]), TT is T4 -T3, display(TT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records, nl,
    
    statistics('walltime',[T5,_]), display('output'), nl, output,
    statistics('walltime',[T6,_]), TTT is T6 -T5, display(TTT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records.


deftypes :-
    statistics('walltime',[T1,_]),set_pp_flag(types, deftypes), module(lpdoc_problem), statistics('walltime',[T2,_]),
    T is T2 -T1, display(T), display(' ms.'), nl, nl, list_recorded_calls, clean_records,
    
    nl, statistics('walltime',[T3,_]), display('deftypes'), nl, analyze(deftypes),
    statistics('walltime',[T4,_]), TT is T4 -T3, display(TT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records, nl,
    
    statistics('walltime',[T5,_]), display('output'), nl, output,
    statistics('walltime',[T6,_]), TTT is T6 -T5, display(TTT), display(' ms.'),
    nl, nl, list_recorded_calls, clean_records.