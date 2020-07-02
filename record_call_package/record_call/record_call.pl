:- package(record_call).

:- use_module(library(streams)).

:- new_declaration(use_record_call/1,on).


:- load_compilation_module(library(record_call/record_call_tr)).
:- add_sentence_trans(record_call_tr:record_call_sentence_tr/3, 8310).
:- add_goal_trans(record_call_tr:record_call_goal_tr/3, 8310).
