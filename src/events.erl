-module(events).

-export([parse_status_data/1,
         parse_status_bytes/1]).


parse_status_bytes(<<Chunk/bitstring>>) ->
    case Chunk of

        % FF 58 04 nn dd cc bb -> Time Signature
        <<16#ff, 16#58, 16#04, Rest/bitstring>> ->
            {time_signature, Rest};

        % FF 59 02 sf mi -> Key Signature
        <<16#ff, 16#59, 16#02, Rest/bitstring>> ->
            {key_signature, Rest};

        % FF 54 05 hr mn se fr ff ->SMPTE Offset
        <<16#ff, 16#54, 16#05, Rest/bitstring>> ->
            {smtpe_offset, Rest};

        % FF 51 03 tttttt ->  Set Tempo, in microseconds per MIDI quarter-note
        <<16#ff, 16#51, 16#03, Rest/bitstring>> ->
            {tempo, Rest};

        % FF 7F len data -> Sequencer-Specific Meta-Event
        <<16#ff, 16#7f, Rest/bitstring>> ->
            {sequencer_specific, Rest};

        % FF 2F 00 -> End of Track
        <<16#ff, 16#2f, 0>> ->
            {end_of_chunk, null};

        % FF 20 01 cc -> MIDI Channel Prefix
        <<16#ff, 16#20, 16#01, Rest/bitstring>> ->
            {channel, Rest};

        % FF 00 02 ssss -> Sequence Number
        <<16#ff, 16#00, 16#02, Rest/bitstring>> ->
            {sequence_number, Rest};

        % FF 01 len text -> Text Event
        <<16#ff, 16#01, Rest/bitstring>> ->
            {text_event, Rest};

        % FF 02 len text -> Copyright Notice
        <<16#ff, 16#02, Rest/bitstring>> ->
            {copyright, Rest};

        % FF 03 len text -> Sequence/Track Name
        <<16#ff, 16#03, Rest/bitstring>> ->
            {sequence_or_track_name, Rest};

        % FF 04 len text -> Instrument Name
        <<16#ff, 16#04, Rest/bitstring>> ->
            {instrument_name, Rest};

        % FF 05 len text -> Lyric
        <<16#ff, 16#05, Rest/bitstring>> ->
            {lyric, Rest};

        % FF 06 len text -> Marker
        <<16#ff, 16#06, Rest/bitstring>> ->
            {marker, Rest};

        % FF 07 len text -> cue point
        <<16#ff, 16#07, Rest/bitstring>> ->
            {cue_point, Rest};

        % parse SysEx events
        % F0 <length> <bytes to be transmitted after F0>
        <<16#f0, Rest/bitstring>> ->
            {sys_ex_f0, Rest};

        % F7 <length> <all bytes to be transmitted>
        % F7 <length> <all bytes to be transmitted F7>
        <<16#f7, Rest/bitstring>> ->
            {sys_ex_f7, Rest};

        % Note Off Event
        % Note Off	MIDI Channel	Note Number	Velocity
        % 8 (0x8)	0-15	0-127	0-127
        % 1000nnnn
        <<1:1, 0:1, 0:1, 0:1, Rest/bitstring>> ->
            {note_off, Rest};

        % TODO: parse VLQ
        % Note On Event
        % Note On	MIDI Channel	Note Number	Velocity
        % 9 (0x9)	0-15	0-127	0-127
        % 1001nnnn
        % Note On (a velocity of 0 = Note Off)
        <<1:1, 0:1, 0:1, 1:1, Rest/bitstring>> ->
            {note_on, Rest};


        % Program Change Event
        % Program Change CnH
        % Program Change	MIDI Channel	Program Number
        % 12 (0xC)	0-15	0-127
        % 1100nnnn
        <<1:1, 1:1, 0:1, 0:1, Rest/bitstring>> ->
            {program_change_event, Rest};

        %% Control Change BnH (0 - 119)
        % Controller	MIDI Channel	Controller Type	Value
        % 11 (0xB)	0-15	0-127	0-127
        % 1011nnnn
        <<1:1, 0:1, 1:1, 1:1, Rest/bitstring>> ->
            {control_change_event, Rest};

        % Note Aftertouch Event
        % Note Aftertouch	MIDI Channel	Note Number	Amount
        % 10 (0xA)	0-15	0-127	0-127
        % 1010nnnn
        <<1:1, 0:1, 1:1, 0:1, Rest/bitstring>> ->
            {note_aftertouch, Rest};

        % Channel Aftertouch Event
        % Channel Aftertouch	MIDI Channel	Amount
        % 13 (0xD)	0-15	0-127
        % 1101nnnn
        <<1:1, 1:1, 0:1, 1:1, Rest/bitstring>> ->
            {channel_aftertouch, Rest };

        % Pitch Bend Event
        % Pitch Bend	MIDI Channel
        % 14 (0xE)	0-15	0-127	0-127
        % 1110nnnn
        <<1:1, 1:1, 1:1, 0:1, Rest/bitstring>> ->
            {pitch_bend, Rest};

        _ ->
            {unknown, Chunk}
    end.


 % FF 58 04 nn dd cc bb -> Time Signature
parse_status_data({time_signature, <<Nn:8, Dd:8, Cc:8, Bb:8, Rest/binary>>}) ->
    {{Nn, Dd, Cc, Bb}, Rest};

% FF 59 02 sf mi -> Key Signature
parse_status_data({key_signature, <<Sf:8, Mi:8, Rest/binary>>}) ->
    {{Sf, Mi}, Rest};

% FF 54 05 hr mn se fr ff ->SMPTE Offset
parse_status_data({smtpe_offset, <<Hr:8, Mn:8, Se:8, Fr:8, Ff:8, Rest/binary>>}) ->
    {{Hr, Mn, Se, Fr, Ff}, Rest};

% FF 51 03 tttttt ->  Set Tempo, in microseconds per MIDI quarter-note
parse_status_data({tempo, <<Tt:24, Rest/binary>>}) ->
    {{Tt}, Rest};

% FF 7F len data -> Sequencer-Specific Meta-Event
parse_status_data({sequencer_specific, <<Size:8/integer, Data:Size/binary, Rest/binary>>}) ->
    {{Size, Data}, Rest};

% FF 2F 00 -> End of Track
parse_status_data({end_of_chunk, <<Rest/binary>>}) ->
    {{}, Rest};

% FF 20 01 cc -> MIDI Channel Prefix
parse_status_data({channel, <<Cc:8, Rest/binary>>}) ->
    {{Cc}, Rest};

% FF 00 02 ssss -> Sequence Number
parse_status_data({sequence_number, <<Ss:8, Rest/binary>>}) ->
    {{Ss}, Rest};

% FF 01 len text -> Text Event
parse_status_data({text_event, <<Size:8/integer, Text:Size/binary, Rest/binary>>}) ->
    {{Text}, Rest};

% FF 02 len text -> Copyright Notice
parse_status_data({copyright, <<Size:8/integer, Text:Size/binary, Rest/binary>>}) ->
    {{Text}, Rest};

% FF 03 len text -> Sequence/Track Name
parse_status_data({sequence_or_track_name, <<Size:8/integer, Text:Size/binary, Rest/binary>>}) ->
    {{Text}, Rest};

% FF 04 len text -> Instrument Name
parse_status_data({instrument_name, <<Size:8/integer, Text:Size/binary, Rest/binary>>}) ->
    {{Text}, Rest};

% FF 05 len text -> Lyric
parse_status_data({lyric, <<Size:8/integer, Text:Size/binary, Rest/binary>>}) ->
    {{Text}, Rest};

% FF 06 len text -> Marker
parse_status_data({marker, <<Size:8/integer, Text:Size/binary, Rest/binary>>}) ->
    {{Text}, Rest};

% FF 07 len text -> cue point
parse_status_data({cue_point, <<Size:8/integer, Text:Size/binary, Rest/binary>>}) ->
    {{Text}, Rest};

% parse SysEx events
% F0 <length> <bytes to be transmitted after F0>
parse_status_data({sys_ex_f0, <<Size:8/integer, Data:Size/integer, Rest/binary>>}) ->
    {{Data}, Rest};

% F7 <length> <all bytes to be transmitted>
% F7 <length> <all bytes to be transmitted F7>
parse_status_data({sys_ex_f7, <<Size:8/integer, Data:Size/integer, Rest/binary>>}) ->
    {{Data}, Rest};

% Note Off Event
% Note Off	MIDI Channel	Note Number	Velocity
% 8 (0x8)	0-15	0-127	0-127
% 1000nnnn
parse_status_data({note_off, <<Channel:4/integer, NoteNumber:8/integer, Velocity:8/integer, Rest/binary>>}) ->
    {{{channel, Channel}, {note, NoteNumber}, {velocity, Velocity}}, Rest};

% TODO: parse VLQ
% Note On Event
% Note On	MIDI Channel	Note Number	Velocity
% 9 (0x9)	0-15	0-127	0-127
% 1001nnnn
% Note On (a velocity of 0 = Note Off)
parse_status_data({note_on, <<Channel:4/integer, NoteNumber:8/integer, Velocity:8/integer, Rest/binary>>}) ->
    {{{channel, Channel}, {note, NoteNumber}, {velocity, Velocity}}, Rest};

% Program Change Event
% Program Change CnH
% Program Change	MIDI Channel	Program Number
% 12 (0xC)	0-15	0-127
% 1100nnnn
parse_status_data({program_change_event, <<Channel:4/integer, Program:8/integer, Rest/binary>>}) ->
    {{{channel, Channel}, Program}, Rest};

%% Control Change BnH (0 - 119)
% Controller	MIDI Channel	Controller Type	Value
% 11 (0xB)	0-15	0-127	0-127
% 1011nnnn
parse_status_data({control_change_event, <<Channel:4/integer, ControllerType:8/integer, Value:8/integer, Rest/binary>>}) ->
    {{{channel, Channel}, ControllerType, Value}, Rest};

% Note Aftertouch Event
% Note Aftertouch	MIDI Channel	Note Number	Amount
% 10 (0xA)	0-15	0-127	0-127
% 1010nnnn
parse_status_data({note_aftertouch, <<Channel:4/integer, NoteNumber:8/integer, Amount:8/integer, Rest/binary>>}) ->
    {{{channel, Channel}, NoteNumber, Amount}, Rest};

% Channel Aftertouch Event
% Channel Aftertouch	MIDI Channel	Amount
% 13 (0xD)	0-15	0-127
% 1101nnnn
parse_status_data({channel_aftertouch, <<Channel:4/integer, Amount:8/integer, Rest/binary>>}) ->
    {{{channel, Channel}, Amount}, Rest};

% Pitch Bend Event
% Pitch Bend	MIDI Channel
% 14 (0xE)	0-15	0-127	0-127
% 1110nnnn
parse_status_data({pitch_bend, <<Channel:4/integer, ValueLSP:8/integer, ValueMSB:8/integer, Rest/binary>>}) ->
    {{{channel, Channel}, ValueLSP, ValueMSB}, Rest};

parse_status_data({unknown, <<>>}) ->
    ok;

parse_status_data({unknown, <<UnknownStatus:8, UnknownStatusData:8, Rest/binary>>}) ->
    %io:format("Parse running status~n"),
    {{UnknownStatus, UnknownStatusData}, Rest};

parse_status_data({_Type, <<>>}) ->
    ok;

parse_status_data({_Type, <<0>>}) ->
    ok;

parse_status_data({Type, <<UnknownStatus:8, UnknownStatusData:8, Rest/binary>>}) ->
    io:format("Parse status data for type ~p~n", [Type]),
    {{UnknownStatus, UnknownStatusData}, Rest}.
