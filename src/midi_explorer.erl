-module(midi_explorer).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    [FileToParse | _] = Args,
    parser:display_midi_file(FileToParse),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
