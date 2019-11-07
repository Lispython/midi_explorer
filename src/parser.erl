-module(parser).

-export([open_file/1,
         parse_midi_header/1,
         parse_midi_file/1,
         bin_to_hex/1,
         display_header/1,
         display_chunk/1,
         display_chunks/1,
         display_midi_file/1,
         display_midi_data/1,
         hexstr_to_bin/1,
         hexstr_to_bin/2
]).


parse_midi_file(FilePath) ->
    case file:read_file(filename:absname(FilePath)) of
        {ok, BinaryData} ->
            {Header, Body} = parse_midi_header(BinaryData),
            %io:format("~p~n", [Body]),
            Chunks = parse_midi_chunks(Body),
            % {ok, {Header, ok}};
            {ok, {Header, Chunks}};
        {error, Reason} ->
            {error, Reason}
    end.


display_midi_file(FilePath)->
    case parse_midi_file(FilePath) of
        {error, Reason} ->
            io:format("File read ~s failed ~n", [Reason]);
        {ok, {Header, Chunks}} ->
            display_midi_data({Header, Chunks})
    end.


display_midi_data({Header, Tracks}) ->
    io:format("Header info ------~n"),
    display_header(Header),
    io:format("~n~nTracks info ------~n ~p~n~n", [Tracks])
    .

open_file(File) ->

    case file:open(filename:absname(File), read) of
        {ok, S} ->
            io:format("File ~s successfully opened ~n", [File]),
            {ok, S};
        {error, Why} ->
            io:format("~s ~n", [Why]),
            {error, Why}
    end.

parse_midi_header(<<BinaryData/binary>>) ->
    case BinaryData of

        % Bit 15 = 1 : timecode
        % Parse number of frames per second (fps)
        <<"MThd", Size:32/integer, Type:16/integer, NumTracks:16, 1:1, Tempo:15/integer, Rest/binary>> ->
            Header = {mthd, Size, Type, NumTracks, {fps, Tempo}};
        % Bit 15 = 0 : metrical timing
        % Parse ppqn format
        <<"MThd", Size:32/integer, Type:16/integer, NumTracks:16, 0:1, Tempo:15/integer, Rest/binary>> ->
            Header = {mthd, Size, Type, NumTracks, {ppqn, Tempo}}
    end,
    {Header, Rest}.


parse_midi_chunks(<<>>) ->
    {ok};


parse_midi_chunks(<<BinaryData/binary>>) ->
    <<"MTrk", Size:32/integer, Data:Size/binary, Rest/binary>> = BinaryData,
    case parse_midi_chunks(Rest) of
        {ok} -> [{mtrk, {size, Size}, parse_chunk_events(Data, unknown)}];
        ok -> [{mtrk, {size, Size}, parse_chunk_events(Data, unknown)}];
        ParsedData -> [{mtrk, {size, Size}, parse_chunk_events(Data, unknown)} | ParsedData]
    end.

% http://www.onicos.com/staff/iz/formats/midi-event.html
% http://midi.teragonaudio.com/tech/midispec/run.htm
% http://www.music-software-development.com/midi-tutorial.html
% https://stackoverflow.com/questions/46702181/how-differentiate-deltatime-vlq-from-running-status-midi-specification
% https://www.midi.org/specifications/item/table-3-control-change-messages-data-bytes-2
% https://www.noterepeat.com/articles/how-to/213-midi-basics-common-terms-explained
% http://www.personal.kent.edu/~sbirch/Music_Production/MP-II/MIDI/midi_control_change_messages.htm
% http://www.somascape.org/midi/basic/intro.html#midifiles
% http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html
% https://samesound.ru/p/midiwork/70572-midi-messages-explain
% http://www.songstuff.com/recording/article/midi_message_format/

parse_chunk_events(<<Chunk/binary>>, PrevStatus) ->
    % <MTrk event> = <delta-time> <event>
    % <event> = <MIDI event> | <sysex event> | <meta-event>
    % Meta events: FF <type> <length> <bytes>

    {{delta_time, DeltaTime}, Tail} = extract_delta_time(Chunk),

    {Status, StatusBodyAndRest} = events:parse_status_bytes(Tail),

    case Status of
        unknown ->
            % parse structure of prev data
            CurrentStatus = PrevStatus,

            NeedAddPrefix = lists:member(CurrentStatus, [note_off,
                                                         note_on,
                                                         program_change_event,
                                                         control_change_event,
                                                         note_aftertouch,
                                                         channel_aftertouch,
                                                         pitch_bend]),
            if
                NeedAddPrefix ->
                    {StatusData, Rest} = events:parse_status_data({CurrentStatus, <<0:1, 0:1, 0:1, 0:1, StatusBodyAndRest/bitstring>>});
                true ->
                    {StatusData, Rest} = events:parse_status_data({CurrentStatus, <<StatusBodyAndRest/bitstring>>})
            end,

            [{CurrentStatus, {delta_time, DeltaTime}, StatusData} | parse_chunk_events(Rest, CurrentStatus)];

        end_of_chunk ->
            {Status, {delta_time, DeltaTime}};
        _ ->
            % use current status structure
            CurrentStatus = Status,

            {StatusData, Rest} = events:parse_status_data({CurrentStatus, StatusBodyAndRest}),

            [{CurrentStatus, {delta_time, DeltaTime}, StatusData} | parse_chunk_events(Rest, CurrentStatus)]
    end.


% Match 1-4-bytes vlq
extract_delta_time(<<BinaryData/binary>>) ->
    % io:format("Extract delta time ~p ~n", [BinaryData]),

    case BinaryData of
        <<16#00, Rest/binary>> ->
            {{delta_time, 0}, Rest};
        _ ->
            {DeltaTime, Rest} = parse_delta_time(BinaryData),
            {{delta_time, DeltaTime}, Rest}
    end.


parse_delta_time(Bin) ->
    {ResBin, T} = parse_delta_time_acc(Bin, <<>>),
    {binary:decode_unsigned(ResBin), T}.

parse_delta_time_acc(<<0:1, Val:7, T/binary>>, Res) ->
    {<<0:1, Res/binary, Val:7>>, T};

parse_delta_time_acc(<<1:1, Val:7, T/binary>>, Res) ->
    parse_delta_time_acc(T, <<0:1, Res/binary, Val:7>>).


display_header({mthd, Size, Type, NumTracks, Tempo}) ->
    io:format("Size: ~p ~nType: ~p~nNumTracks: ~p~nTempo: ~p~n", [Size, Type, NumTracks, Tempo]).


display_chunk(Chunk) ->
    % format track chunk
    io:format("Format chunk ~p~n", [Chunk]).


display_chunks(Chunks) ->
    % format binary chunks
    case Chunks of
        [] -> {ok};
        [Head, Tail] ->
            display_chunk(Head),
            display_chunks(Tail)
    end.


hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).


bin_to_hex(BinaryData) ->
    io:format("<<~s>>~n", [[io_lib:format(" ~2.16.0B",[X]) || <<X:8>> <= BinaryData ]]).
