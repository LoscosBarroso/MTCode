:- module(printcolor, [underline/0, print_color/1, display_color/2, restore/0, test_colors/0], [assertions]).

%! \title Print text outputs, With color!

:- doc(module, "This module contains the operations required to print displays with colored text.
").
:- use_module(library(streams)).
:- use_module(library(stream_utils)).
:- use_module(library(lists)).
:- use_module(engine(io_basic)).

:- pred color(Color,Code)
   # "@var{Color} is a color name and @var{Code} is the ANSI escape code to print text in
      that color.".

color('black','\x1b\[30m').
color('red','\x1b\[31m').
color('green','\x1b\[32m').
color('yellow','\x1b\[33m').
color('blue','\x1b\[34m').
color('magenta','\x1b\[35m').
color('cyan','\x1b\[36m').
color('white','\x1b\[37m').

color('black_background','\x1b\[40m').
color('red_background','\x1b\[41m').
color('green_background','\x1b\[42m').
color('yellow_background','\x1b\[43m').
color('blue_background','\x1b\[44m').
color('magenta_background','\x1b\[45m').
color('cyan_background','\x1b\[46m').
color('white_background','\x1b\[47m').

color('bright_black','\x1b\[90m').
color('bright_red','\x1b\[91m').
color('bright_green','\x1b\[92m').
color('bright_yellow','\x1b\[93m').
color('bright_blue','\x1b\[94m').
color('bright_magenta','\x1b\[95m').
color('bright_cyan','\x1b\[96m').
color('bright_white','\x1b\[97m').

color('bright_black_background','\x1b\[100m').
color('bright_red_background','\x1b\[101m').
color('bright_green_background','\x1b\[102m').
color('bright_yellow_background','\x1b\[103m').
color('bright_blue_background','\x1b\[104m').
color('bright_magenta_background','\x1b\[105m').
color('bright_cyan_background','\x1b\[106m').
color('bright_white_background','\x1b\[107m').

:-pred restore
  # "Resets the display status to print in the regular Ciao format.".

restore :- display('\x1b\[0m').
bold :- display('\x1b\[1m').
faint :- display('\x1b\[2m').
italic :- display('\x1b\[3m').
underline :- display('\x1b\[4m').

:-pred print_color(Color)
  # "@Var{Color} is a color name understood by color/2. Every display after this predicate will print its text in the chosen color.".

print_color(Color):- color(Color,Code), display(Code).

:-pred display_color(Color,Text)
  # "@Var{Color} is a color name understood by color/2. @Var{Text} is the term to be 
    displayed in the color defined by @Var{Color}.".

display_color(Color,Text):- color(Color,Code), display(Code), display(Text), restore.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555
test_colors :- display_color(X,X),nl, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


