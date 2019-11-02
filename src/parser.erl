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
        ParsedData -> [{mtrk, {size, Size}, parse_chunk_events(Data, unknown)} | ParsedData]
    end.

% http://www.onicos.com/staff/iz/formats/midi-event.html
% http://midi.teragonaudio.com/tech/midispec/run.htm

parse_chunk_events(<<Chunk/binary>>, PrevStatus) ->
    % <MTrk event> = <delta-time> <event>
    % <event> = <MIDI event> | <sysex event> | <meta-event>
    % Meta events: FF <type> <length> <bytes>

    {{delta_time, DeltaTime}, Tail} = extract_delta_time(Chunk),

    case Tail of

        % FF 58 04 nn dd cc bb -> Time Signature
        <<16#ff, 16#58, 16#04, Nn:8, Dd:8, Cc:8, Bb:8, Rest/binary>> ->
            [{time_signature, {delta_time, DeltaTime}, Nn, Dd, Cc, Bb} | parse_chunk_events(Rest, time_signature)];

        % FF 59 02 sf mi -> Key Signature
        <<16#ff, 16#59, 16#02, Sf:8, Mi:8, Rest/binary>> ->
            [{key_signature, {delta_time, DeltaTime}, Sf, Mi} | parse_chunk_events(Rest, key_signature)];

        % FF 54 05 hr mn se fr ff ->SMPTE Offset
        <<16#ff, 16#54, 16#05, Hr:8, Mn:8, Se:8, Fr:8, Ff:8, Rest/binary>> ->
            [{smtpe_offset, {delta_time, DeltaTime}, Hr, Mn, Se, Fr, Ff} | parse_chunk_events(Rest, smtpe_offset)];

        % FF 51 03 tttttt ->  Set Tempo, in microseconds per MIDI quarter-note
        <<16#ff, 16#51, 16#03, Tt:24, Rest/binary>> ->
            [{tempo, {delta_time, DeltaTime}, Tt} | parse_chunk_events(Rest, tempo)];

        % FF 7F len data -> Sequencer-Specific Meta-Event
        <<16#ff, 16#7f, Size:8/integer, Data:Size/binary, Rest/binary>> ->
            [{sequencer_specific, {delta_time, DeltaTime}, Data} | parse_chunk_events(Rest, sequencer_specific)];

        % FF 2F 00 -> End of Track
        <<16#ff, 16#2f, 0>> ->
            {end_of_chunk, {delta_time, DeltaTime}};

        % FF 20 01 cc -> MIDI Channel Prefix
        <<16#ff, 16#20, 16#01, Cc:8, Rest/binary>> ->
            [{channel, {delta_time, DeltaTime}, Cc} | parse_chunk_events(Rest, channel)];

        % FF 00 02 ssss -> Sequence Number
        <<16#ff, 16#00, 16#02, Ss:8, Rest/binary>> ->
            [{sequence_number, {delta_time, DeltaTime}, Ss} | parse_chunk_events(Rest, sequence_number)];

        % FF 01 len text -> Text Event
        <<16#ff, 16#01, Size:8/integer, Text:Size/binary, Rest/binary>> ->
            [{text_event, {delta_time, DeltaTime}, Text} | parse_chunk_events(Rest, text_event)];

        % FF 02 len text -> Copyright Notice
        <<16#ff, 16#02, Size:8/integer, Text:Size/binary, Rest/binary>> ->
            [{copyright, {delta_time, DeltaTime}, Text} | parse_chunk_events(Rest, copyright)];

        % FF 03 len text -> Sequence/Track Name
        <<16#ff, 16#03, Size:8/integer, Text:Size/binary, Rest/binary>> ->
            [{sequence_or_track_name, {delta_time, DeltaTime}, Text} | parse_chunk_events(Rest, sequence_or_track_name)];

        % FF 04 len text -> Instrument Name
        <<16#ff, 16#04, Size:8/integer, Text:Size/binary, Rest/binary>> ->
            [{instrument_name, {delta_time, DeltaTime}, Text} | parse_chunk_events(Rest, instrument_name)];

        % FF 05 len text -> Lyric
        <<16#ff, 16#05, Size:8/integer, Text:Size/binary, Rest/binary>> ->
            [{lyric, {delta_time, DeltaTime}, Text} | parse_chunk_events(Rest, lyric)];

        % FF 06 len text -> Marker
        <<16#ff, 16#06, Size:8/integer, Text:Size/binary, Rest/binary>> ->
            [{marker, {delta_time, DeltaTime}, Text} | parse_chunk_events(Rest, marker)];

        % FF 07 len text -> cue point
        <<16#ff, 16#07, Size:8/integer, Text:Size/binary, Rest/binary>> ->
            [{cue_point, {delta_time, DeltaTime}, Text} | parse_chunk_events(Rest, cue_point)];

        % parse SysEx events
        % F0 <length> <bytes to be transmitted after F0>
        <<16#f0, Size:8/integer, Data:Size/integer, Rest/binary>> ->
            [{sys_ex, {delta_time, DeltaTime}, Data} | parse_chunk_events(Rest, sys_ex)];

        % F7 <length> <all bytes to be transmitted>
        % F7 <length> <all bytes to be transmitted F7>
        <<16#f7, Size:8/integer, Data:Size/integer, Rest/binary>> ->
            [{sys_ex, {delta_time, DeltaTime}, Data} | parse_chunk_events(Rest, sys_ex)];

        % Note Off Event
        % Note Off	MIDI Channel	Note Number	Velocity
        % 8 (0x8)	0-15	0-127	0-127
        % 1000nnnn
        <<1:1, 0:1, 0:1, 0:1, Channel:4/integer, NoteNumber:8/integer, Velocity:8/integer, Rest/binary>> ->
            [{note_off, {delta_time, DeltaTime}, {channel, Channel}, {note, NoteNumber}, {velocity, Velocity}} | parse_chunk_events(Rest, note_off)];

        % TODO: parse VLQ
        % Note On Event
        % Note On	MIDI Channel	Note Number	Velocity
        % 9 (0x9)	0-15	0-127	0-127
        % 1001nnnn
        % Note On (a velocity of 0 = Note Off)
        <<1:1, 0:1, 0:1, 1:1, Channel:4/integer, NoteNumber:8/integer, Velocity:8/integer, Rest/binary>> ->
            [{note_on, {delta_time, DeltaTime}, {channel, Channel}, {note, NoteNumber}, {velocity, Velocity}} | parse_chunk_events(Rest, note_on)];


        % Program Change Event
        % Program Change CnH
        % Program Change	MIDI Channel	Program Number
        % 12 (0xC)	0-15	0-127
        % 1100nnnn
        <<1:1, 1:1, 0:1, 0:1, Channel:4/integer, Program:8/integer, Rest/binary>> ->
            [{program_change_event, {delta_time, DeltaTime}, {channel, Channel}, Program} | parse_chunk_events(Rest, program_change_event)];

        %% Control Change BnH (0 - 119)
        % Controller	MIDI Channel	Controller Type	Value
        % 11 (0xB)	0-15	0-127	0-127
        % 1011nnnn
        <<1:1, 0:1, 1:1, 1:1, Channel:4/integer, ControllerType:8/integer, Value:8/integer, Rest/binary>> ->
            [{control_change_event, {delta_time, DeltaTime}, {channel, Channel}, ControllerType, Value} | parse_chunk_events(Rest, control_change_event)];

        % Note Aftertouch Event
        % Note Aftertouch	MIDI Channel	Note Number	Amount
        % 10 (0xA)	0-15	0-127	0-127
        % 1010nnnn
        <<1:1, 0:1, 1:1, 0:1, Channel:4/integer, NoteNumber:8/integer, Amount:8/integer, Rest/binary>> ->
            [{note_aftertouch, {delta_time, DeltaTime}, {channel, Channel}, NoteNumber, Amount} | parse_chunk_events(Rest, note_aftertouch)];

        % Channel Aftertouch Event
        % Channel Aftertouch	MIDI Channel	Amount
        % 13 (0xD)	0-15	0-127
        % 1101nnnn
        <<1:1, 1:1, 0:1, 1:1, Channel:4/integer, Amount:8/integer, Rest/binary>> ->
            [{channel_aftertouch, {delta_time, DeltaTime}, {channel, Channel}, Amount} | parse_chunk_events(Rest, channel_aftertouch)];

        % Pitch Bend Event
        % Pitch Bend	MIDI Channel
        % 14 (0xE)	0-15	0-127	0-127
        % 1110nnnn
        <<1:1, 1:1, 1:1, 0:1, Channel:4/integer, ValueLSP:8/integer, ValueMSB:8/integer, Rest/binary>> ->
            [{pitch_bend, {delta_time, DeltaTime}, {channel, Channel}, ValueLSP, ValueMSB} | parse_chunk_events(Rest, pitch_bend)];

        %% <<16#20, UnknownByte:8, Rest/binary>> ->
        %%     %<<B:128/binary, _/binary>> = Chunk,
        %%     %io:format("Unknown chunk format: ~p~n", [B]),
        %%     %{{delta_time, DeltaTime}, B};
        %%     io:format("Unknown bytes: ~p~n", [UnknownByte]),
        %%     [{{unknown, 16#20}, {delta_time, DeltaTime}, <<UnknownByte:8/integer>>} | parse_chunk_events(Rest)];
        %% <<16#32, UnknownByte:8, Rest/binary>> ->
        %%     io:format("Unknown bytes: ~p~n", [UnknownByte]),
        %%     [{{unknown, 16#32}, {delta_time, DeltaTime}, <<UnknownByte:8/integer>>} | parse_chunk_events(Rest)];

        % match running status messages
        % TODO: rewrite
        <<UnknownStatus:8, UnknownStatusData:8, Rest/binary>> ->
            io:format("Unknown bytes: ~p ~p ~n", [UnknownStatus, UnknownStatusData]),
            [{PrevStatus, {delta_time, DeltaTime}, UnknownStatus, UnknownStatusData} | parse_chunk_events(Rest, unknown)];

        <<B:128/binary, _/binary>> ->
            io:format("Unknown chunk format: ~p~n", [{{delta_time, DeltaTime}, B}]),
            {{delta_time, DeltaTime}, B};
        _ ->
            io:format("Unknown chunk format: ~p~n", [Chunk])
    end.


% Match 1-4-bytes vlq
extract_delta_time(<<BinaryData/binary>>) ->
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
