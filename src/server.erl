-module(server).
-export([start/0, main_loop/1, parse_message/1, serv1/1, serv2/1, serv3/1]).

start() ->
    Serv3 = spawn(fun() -> serv3(0) end),
    Serv2 = spawn(fun() -> serv2(Serv3) end),
    Serv1 = spawn(fun() -> serv1(Serv2) end),
    
    % Main loop: Send messages to serv1
    main_loop(Serv1).

main_loop(Serv1) ->
    io:format("Enter a message (or 'all_done' to exit):~n", []),
    Message = io:get_line("> "),
    case parse_message(string:trim(Message)) of
        all_done ->
            Serv1 ! halt, % Send the halt message to serv1, which will propagate down
            io:format("Shutting down...~n");
        Msg when is_atom(Msg) -> % Check if Msg is an atom before forwarding
            Serv1 ! Msg,
            main_loop(Serv1);
        Msg ->
            io:format("Invalid input or unsupported type: ~p~n", [Msg]),
            main_loop(Serv1)  % Loop back for invalid inputs
    end.

parse_message("all_done") -> 
    all_done;
parse_message(Input) ->
    InputStr = string:trim(Input),  % Trim whitespace and newline
    case catch erl_eval:string_to_term(InputStr) of
        {'EXIT', _} -> 
            io:format("Invalid input: ~s~n", [InputStr]),
            all_done;  % Return to main_loop if invalid input
        Term -> 
            io:format("Converted input to atom: ~p~n", [Term]),  % Print the converted atom
            Term  % Successfully converted string to Erlang term
    end.

serv1(Next) ->
    receive
        halt ->
            io:format("(serv1) Halting...~n", []),
            Next ! halt; % Forward halt to serv2
            
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
            
        Other ->
            io:format("(serv3) Not handled: ~p~n", [Other]),
            serv3(UnhandledCount + 1)  % Increment the count for unhandled messages
    end.
