-module(mlp).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").

main([]) ->
    loop(standard_io).

loop(Handle) ->
    loop(Handle, <<>>).

loop(Handle, Buffer) ->
    case file:read_line(Handle) of
        {ok, Data} ->
            CompleteData = append(Buffer, Data),
            case process_line(CompleteData) of
                ok -> loop(Handle, []);
                {error, incomplete_log} ->
                    loop(Handle, CompleteData)
            end;
        eof -> ok;
        {error, Reason} -> io:format(standard_error, "error: ~p", [Reason])
    end.

%loop(Handle, _, Buffer) ->
%    case file:read_line(Handle) of
%        {ok, Data} ->
%            process_line(Data),
%            loop(Handle);
%        eof -> ok;
%        {error, Reason} -> io:format(standard_error, "error: ~p", [Reason])
%    end.

process_line(Data) -> dispatch(Data).

dispatch(Data) ->
    Actions = [ {"mod_bosh:info:208 Sending (binary) to", fun parse_bosh_sending/2},
                {"mod_bosh:info:204 Parsed body:", fun parse_bosh_parsed/2},
                {"ejabberd_c2s:send_text:1717 Send XML on stream = ", fun parse_c2s_send_xml/2},
                {"ejabberd_receiver:process_data:336 Received XML on stream = ", fun parse_c2s_received_xml/2} ],
    {Pattern, Action} = lists:foldl(pa:bind(fun match_line/3, Data),
                                    {"(no match)", fun no_action/2}, Actions),
    Action(Pattern, Data).

%% 2015-04-02 09:32:33.110 [debug] <0.15387.42>@mod_bosh:info:208 Sending (binary) to
%%  <<"8f171e2905fa08c9c2c0cea6d723f3cd99810680">>: <<"<body maxpause='120' inactivity='30'
%%  xmlns:stream='http://etherx.jabber.org/streams' xmlns:xmpp='urn:xmpp:xbosh' 
%%  xmlns='http://jabber.org/protocol/httpbind' xmpp:version='1.0' xmpp:restartlogic='true' 
%%  sid='8f171e2905fa08c9c2c0cea6d723f3cd99810680' accept='deflate,gzip' from='excellencemotors.com' 
%%  hold='1' requests='2' wait='60'><stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
%%  <session xmlns='urn:ietf:params:xml:ns:xmpp-session'/><sm xmlns='urn:xmpp:sm:3'/></stream:features></body>">>
parse_bosh_sending(Pattern, Data) ->
    LastIndex = string:rstr(Data, Pattern),
    PastPattern = LastIndex + length(Pattern),
    PacketIndex = PastPattern + string:str(string:substr(Data, PastPattern), ">>: ") + length(">>: ") - 1,
    PacketAsStringifiedBinary = string:substr(Data, PacketIndex),
    BPacket = eval(PacketAsStringifiedBinary ++ ".", []),
    case exml:parse(BPacket) of
        {ok, Element} ->
            io:format(">> bosh sent:~n~n~s~n", [exml:to_pretty_iolist(Element)]);
        _ ->
            io:format(">> bosh sent (unparseable):~n~n~p~n~n", [BPacket])
    end,
    ok.

%% 2015-04-02 09:32:33.300 [debug] <0.15387.42>@mod_bosh:info:204 Parsed body: 
%%  {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},
%% {<<"rid">>,<<"2796558209">>},{<<"sid">>,<<"8f171e2905fa08c9c2c0cea6d723f3cd99810680">>}],
%% [{xmlel,<<"iq">>,[{<<"xmlns">>,<<"jabber:client">>},{<<"type">>,<<"set">>},{<<"id">>,<<"_bind_auth_2">>}],
%% [{xmlel,<<"bind">>,[{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-bind">>}],
%% [{xmlel,<<"resource">>,[],[{xmlcdata,<<"FOEdge_i80ds0ls-1s3d">>}]}]}]}]}
parse_bosh_parsed(Pattern, Data) ->
    PacketIndex = string:rstr(Data, Pattern) + length(Pattern) + 1,
    StringifiedPacketTerm = string:substr(Data, PacketIndex),
    Element = eval(StringifiedPacketTerm ++ ".", []),
    io:format("<< bosh received:~n~n~s~n", [exml:to_pretty_iolist(Element)]),
    ok.

%% 2015-04-20 14:49:48.246 [debug] <0.645.0>@ejabberd_c2s:send_text:1717
%%  Send XML on stream = <<"<message from='asd@localhost/psi' to='zxc@somedomain'
%%  xml:lang='en' type='chat' id='aabf12312312312312a'>\n<body>a</body>\n
%% <archived by='zxc@somedomain' id='A517BLU0ANO1'/></message>">>
parse_c2s_send_xml(Pattern, Data) ->
    PacketIndex = string:rstr(Data, Pattern) + length(Pattern),
    StringifiedPacketTerm = string:substr(Data, PacketIndex),
    BPacket = iolist_to_binary(eval(StringifiedPacketTerm ++ ".", [])),
    case exml:parse(BPacket) of
        {ok, Element} ->
            io:format(">> c2s sent:~n~n~s~n", [exml:to_pretty_iolist(strip_whitespace_cdata(Element))]);
        _ ->
            io:format(">> c2s sent (unparseable):~n~n~p~n~n", [BPacket])
    end,
    ok.

%% 2015-04-17 11:33:57.073 [debug] <0.1569.0>@ejabberd_receiver:process_data:336
%%  Received XML on stream = "<iq id='d18339903e770db69a1ede841df48375' type='get'>
%% <query xmlns='jabber:iq:register'/></iq>"
parse_c2s_received_xml(Pattern, Data) ->
    PacketIndex = string:rstr(Data, Pattern) + length(Pattern) + 1,
    Len = length(Data) - PacketIndex - 1,
    case Len < 0 of
        %% Received keepalive:
        %% 2015-04-20 16:11:51.207 [debug] <0.640.0>@ejabberd_receiver:process_data:336 Received XML on stream = "
        %% "
        true -> ok;
        %% Any other "received" log message:
        false ->
            StringifiedPacketTerm = string:substr(Data, PacketIndex, Len),
            DoubleQuoteEscapedTerm = escape_double_quotes(StringifiedPacketTerm),
            BPacket = iolist_to_binary(eval("\"" ++ DoubleQuoteEscapedTerm ++ "\".", [])),
            case exml:parse(BPacket) of
                {ok, Element} ->
                    io:format("<< c2s received:~n~n~s~n", [exml:to_pretty_iolist(strip_whitespace_cdata(Element))]),
                    ok;
                _ ->
                    {error, incomplete_log}
            end
    end.

no_action(_Pattern, _Data) ->
    ok.
    %case string:strip(Data, both) of
    %    [] -> ok;
    %    %__ -> stderr("no action for: ~s~n", [string:substr(Data, 40) ++ "..."])
    %    __ -> stderr("no action for: ~p~n", [Data])
    %end.

match_line(Data, {Pattern, Action}, Acc) ->
    case 'contains?'(Data, Pattern) of
        true -> {Pattern, Action};
        false -> Acc
    end.

'contains?'(String, Pattern) ->
    string:str(String, Pattern) /= 0.

stderr(Format, Args) ->
    io:format(standard_io, Format, Args).

eval(S, Environ) ->
    {ok, Scanned, _} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Term, []} = erl_eval:exprs(Parsed, Environ),
    Term.

strip_whitespace_cdata({xmlcdata, CData} = El) ->
    case is_whitespace_only(CData) of
        true -> skip;
        false -> El
    end;
strip_whitespace_cdata(#xmlel{children = Children} = El) ->
    El#xmlel{children = [ C || C0 <- Children,
                               C <- [strip_whitespace_cdata(C0)],
                               C /= skip ]}.

is_whitespace_only(Data) ->
    re:run(Data, <<"\S">>) == nomatch.

append(Buffer, Data) ->
    binary_to_list(iolist_to_binary([Buffer, Data])).

escape_double_quotes(String) ->
    re:replace(String, "\"", "'", [global, {return, list}]).
