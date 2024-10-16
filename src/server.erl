%% Team: AJ Valentino and Lauren Ferlito
-module(server).
-export([start/0, main_loop/3, parse_message/1, serv1/1, serv2/1, serv3/1]).

start() ->
    % Create in reverse order to pass servers as arguments
    Serv3 = spawn(fun() -> serv3(0) end),
    Serv2 = spawn(fun() -> serv2(Serv3) end),
    Serv1 = spawn(fun() -> serv1(Serv2) end),
    
    % Main loop: Send messages to serv1
    main_loop(Serv1, Serv2, Serv3).

main_loop(Serv1, Serv2, Serv3) ->
    io:format("Enter a message (or 'all_done' to exit):~n", []),
    Message = io:get_line("> "),
    case parse_message(string:trim(Message)) of
        all_done ->
            Serv1 ! halt, % Send the halt message to serv1, which will propagate down
            io:format("Shutting down...~n"),
            ok;
        update1 ->
            Serv1 ! update, % Send update message
            main_loop(Serv1, Serv2, Serv3);
        update2 ->
            Serv2 ! update, % Send update message
            main_loop(Serv1, Serv2, Serv3);
        update3 ->
            Serv3 ! update, % Send update message
            main_loop(Serv1, Serv2, Serv3);
        {ok, Msg} ->
            Serv1 ! Msg, % Sent valid messages
            main_loop(Serv1, Serv2, Serv3);
        {error, Msg} ->
            io:format("Invalid input or unsupported type: ~p~n", [Msg]),
            main_loop(Serv1, Serv2, Serv3)  % Loop back for invalid inputs
    end.

parse_message("all_done") -> 
    all_done;
parse_message("update1") -> 
    update1;
parse_message("update2") -> 
    update2;
parse_message("update3") -> 
    update3;
parse_message(Input) ->
    case erl_scan:string(Input++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, [Expr]} ->
                    {value, Term, _} = erl_eval:expr(Expr, []),
                    {ok, Term};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error, _} ->
            {error, Error}
    end.

serv1(Next) ->
    receive
        halt ->
            io:format("(serv1) Halting...~n", []),
            Next ! halt; % Forward halt to serv2
        update ->
            io:format("(serv1) Updating serv1...~n", []),
            server:serv1(Next);  % Call the updated version of serv1
        % Math Functions
        {add, A, B} when is_number(A), is_number(B) ->
            Result = A + B,
            io:format("(serv1) Adding ~p + ~p = ~p~n", [A, B, Result]),
            serv1(Next);

        {sub, A, B} when is_number(A), is_number(B) ->
            Result = A - B,
            io:format("(serv1) Subtracting ~p - ~p = ~p~n", [A, B, Result]),
            serv1(Next);

        {mult, A, B} when is_number(A), is_number(B) ->
            Result = A * B,
            io:format("(serv1) Multiplying ~p * ~p = ~p~n", [A, B, Result]),
            serv1(Next);

        % Used "divide" because "div" is a key word
        {divide, A, B} when is_number(A), is_number(B) ->
            Result = A / B,
            io:format("(serv1) Dividing ~p / ~p = ~p~n", [A, B, Result]),
            serv1(Next);

        {neg, A} when is_number(A) ->
            Result = -A,
            io:format("(serv1) Negating ~p = ~p~n", [A, Result]),
            serv1(Next);

        {sqrt, A} when is_number(A) ->
            Result = math:sqrt(A),
            io:format("(serv1) Square root of ~p = ~p~n", [A, Result]),
            serv1(Next);
            
        Msg ->
            io:format("(serv1) Received message: ~p~n", [Msg]),
            Next ! Msg,  % Forward all other messages to serv2
            serv1(Next)
    end.

serv2(Next) ->
    receive
        halt ->
            io:format("(serv2) Halting...~n", []),
            Next ! halt; % Forward halt to serv3
        update ->
            io:format("(serv2) Updating serv2...~n", []),
            server:serv2(Next);  % Call the updated version of serv2

        [Head | Tail] when is_integer(Head) ->
            Sum = lists:sum([X || X <- [Head | Tail], is_number(X)]), % Sum function sums all the items in a list
            io:format("(serv2) Sum of list with integer head: ~p = ~p~n", [[Head | Tail], Sum]),
            serv2(Next);

        [Head | Tail] when is_float(Head) ->
            Product = lists:foldl(fun(X, Prod) -> X * Prod end, 1, [X || X <- [Head | Tail]]), % Foldl calls fun on successive elements in the list
            io:format("(serv2) Product of list with float head: ~p = ~p~n", [[Head | Tail], Product]),
            serv2(Next);
            
        Msg ->
            io:format("(serv2) Forwarding message: ~p~n", [Msg]),
            Next ! Msg,  % Forward other messages to serv3
            serv2(Next)
    end.

serv3(UnhandledCount) ->
    receive
        halt ->
            io:format("(serv3) Halting...~n"),
            io:format("(serv3) Total unhandled messages: ~p~n", [UnhandledCount]);
        update ->
            io:format("(serv3) Updating serv3...~n", []),
            server:serv3(UnhandledCount);  % Call the updated version of serv3
        {error, Reason} ->
            io:format("(serv3) Error: ~p~n", [Reason]),
            serv3(UnhandledCount);

        Other ->
            io:format("(serv3) Not handled: ~p~n", [Other]),
            serv3(UnhandledCount + 1)  % Increment the count for unhandled messages
    end.
