print_in_color(Color, Text) :-  color_code(Color, Code),
                                format('\e[~wm~w\e[0m', [Code, Text]).

color_code(blue, '34').    % Blue
color_code(red, '31').     % Red
color_code(green, '32').   % Green
color_code(cyan, '36').    % Cyan
color_code(yellow, '33').  % Yellow
color_code(magenta, '35'). % Magenta
color_code(white, '37').   % White
color_code(black, '90').   % Black
color_code(b_magenta, '95').
/*
 write('  '), format_color('-'), format_color('-'), format_color('-'), format_color('-'), format_color('-'),
    format_color('-'), format_color('-'), format_color('-'), format_color('-'), format_color('-'),
    format_color('-'), format_color('-'), format_color('-'), format_color('-'), format_color('-'), nl,
    write('  _______________  '), nl,
    print_board(Board, 1),
    nl. 
write('  '), format_color('_'), format_color('_'), format_color('_'), format_color('_'), format_color('_'),
    format_color('_'), format_color('_'), format_color('_'), format_color('_'), format_color('_'),
    format_color('_'), format_color('_'), format_color('_'), format_color('_'), format_color('_'), nl.
print_board(_, N) :- N > 16, !. */