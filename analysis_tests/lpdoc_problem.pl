:- module(_,[html_escape/2],[assertions,regtypes]).

%html_escape("``"||S0, "&ldquo;"||S) :- !, html_escape(S0, S). %7 + this was too slow
%html_escape("''"||S0, "&rdquo;"||S) :- !, html_escape(S0, S). %7 + this was too slow
html_escape([0'"|S0], "&quot;"||S) :- !, html_escape(S0, S). %7
html_escape([0''|S0], "&apos;"||S) :- !, html_escape(S0, S). %6,7
html_escape("&#"||S0, "&#"||S) :- !, html_escape(S0, S). %5,6,7
%html_escape([0'&|S0], "&amp;"||S) :- !, html_escape(S0, S). % 4 + this was too slow
html_escape([0'<|S0], "&lt;"||S) :- !, html_escape(S0, S). %4,5,6,7
html_escape([0'>|S0], "&gt;"||S) :- !, html_escape(S0, S). %3,4,5,6,7
html_escape([X|S0], [X|S]) :- !, html_escape(S0, S). %2,3,4,5,6,7
html_escape([], []). %1,2,3,4,5,6,7
