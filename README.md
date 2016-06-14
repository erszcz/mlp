# MongooseIM log processor

## Important notice

As of now the script is under a small rewrite to support a broader range
of server log messages - BOSH logs aren't supported yet.

## How to use

Clone and build:

```
git clone https://github.com/erszcz/mlp
cd mlp
./rebar get-deps
./rebar compile
```

To filter out an XMPP stream from MongooseIM log:

-   set the logging level to 5 - either in the config file or in the
    server shell (specifically for `ejabberd_c2s` and
    `ejabberd_receiver` modules) with:

        ejabberd_loglevel:set_custom(ejabberd_c2s, 5).
        ejabberd_loglevel:set_custom(ejabberd_receiver, 5).

-   gather some logs

-   for XMPP over TCP identify the process IDs of the c2s and receiver processes you're
    interested in (by inspecting the log or by some means in the server shell),
    for BOSH identify the BOSH session ID

-   use the `mlp` filter, but first grep using your process/session IDs from the
    previous point to filter out irrelevant lines (c2s and receiver
    process IDs used in this example):

        grep -E '<0.1180.0>|<0.1179.0>' ejabberd.log | mlp

## Example

This script takes this (BOSH session ID used in this example):

```
$ cat log/ejabberd.log | grep b851f0c040b750f263baa1437b4c04bb215c7762
2015-04-17 11:33:57.695 [debug] <0.1592.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns:xmpp">>,<<"urn:xmpp:xbosh">>},{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"to">>,<<"localhost">>},{<<"xml:lang">>,<<"en">>},{<<"wait">>,<<"60">>},{<<"hold">>,<<"1">>},{<<"ver">>,<<"1.6">>},{<<"xmpp:version">>,<<"1.0">>},{<<"content">>,<<"text/xml; charset=utf-8">>},{<<"rid">>,<<"1429263237694153">>}],[]}
2015-04-17 11:33:57.695 [debug] <0.1593.0>@mod_bosh_socket:init:183 mod_bosh_socket started
2015-04-17 11:33:57.695 [debug] <0.1592.0>@mod_bosh:start_session:361 Created new session <<"b851f0c040b750f263baa1437b4c04bb215c7762">>
2015-04-17 11:33:57.707 [debug] <0.1592.0>@mod_bosh:info:208 Sending (binary) to <<"b851f0c040b750f263baa1437b4c04bb215c7762">>: <<"<body maxpause='120' inactivity='30' xmlns:stream='http://etherx.jabber.org/streams' xmlns:xmpp='urn:xmpp:xbosh' xmlns='http://jabber.org/protocol/httpbind' xmpp:version='1.0' xmpp:restartlogic='true' sid='b851f0c040b750f263baa1437b4c04bb215c7762' accept='deflate,gzip' from='localhost' hold='1' requests='2' wait='60'><stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>DIGEST-MD5</mechanism><mechanism>SCRAM-SHA-1</mechanism></mechanisms><sm xmlns='urn:xmpp:sm:3'/></stream:features></body>">>
2015-04-17 11:33:57.708 [debug] <0.1592.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694154">>}],[{xmlel,<<"auth">>,[{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-sasl">>},{<<"mechanism">>,<<"PLAIN">>}],[{xmlcdata,<<"AGNhcm9sAHcwS3ZWbnFKR3ZyY3dSdiUyRmY1OWNteWdkeCUyQmtIeHZsUktGNmdTUEliblZQdGxSVGpMMDJtNWNQaEQ1V1ZGaFpH">>}]}]}
2015-04-17 11:33:57.708 [debug] <0.1592.0>@mod_bosh:info:208 Sending (binary) to <<"b851f0c040b750f263baa1437b4c04bb215c7762">>: <<"<body xmlns='http://jabber.org/protocol/httpbind' sid='b851f0c040b750f263baa1437b4c04bb215c7762'><success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/></body>">>
2015-04-17 11:33:57.709 [debug] <0.1592.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns:xmpp">>,<<"urn:xmpp:xbosh">>},{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"xmpp:restart">>,<<"true">>},{<<"to">>,<<"localhost">>},{<<"xml:lang">>,<<"en">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694155">>}],[]}
2015-04-17 11:33:57.720 [debug] <0.1592.0>@mod_bosh:info:208 Sending (binary) to <<"b851f0c040b750f263baa1437b4c04bb215c7762">>: <<"<body maxpause='120' inactivity='30' xmlns:stream='http://etherx.jabber.org/streams' xmlns:xmpp='urn:xmpp:xbosh' xmlns='http://jabber.org/protocol/httpbind' xmpp:version='1.0' xmpp:restartlogic='true' sid='b851f0c040b750f263baa1437b4c04bb215c7762' accept='deflate,gzip' from='localhost' hold='1' requests='2' wait='60'><stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/><session xmlns='urn:ietf:params:xml:ns:xmpp-session'/><sm xmlns='urn:xmpp:sm:3'/></stream:features></body>">>
2015-04-17 11:33:57.721 [debug] <0.1592.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694156">>}],[{xmlel,<<"iq">>,[{<<"id">>,<<"8948c17a5ffea7c31362f0c7f262c56f">>},{<<"type">>,<<"set">>}],[{xmlel,<<"bind">>,[{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-bind">>}],[{xmlel,<<"resource">>,[],[{xmlcdata,<<"bosh">>}]}]}]}]}
2015-04-17 11:33:57.721 [debug] <0.1592.0>@mod_bosh:info:208 Sending (binary) to <<"b851f0c040b750f263baa1437b4c04bb215c7762">>: <<"<body xmlns='http://jabber.org/protocol/httpbind' sid='b851f0c040b750f263baa1437b4c04bb215c7762'><iq type='result' id='8948c17a5ffea7c31362f0c7f262c56f' xmlns='jabber:client'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><jid>carol@localhost/bosh</jid></bind></iq></body>">>
2015-04-17 11:33:57.722 [debug] <0.1592.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694157">>}],[{xmlel,<<"iq">>,[{<<"id">>,<<"bbc549cdc013b6a2e99fada1a71d1bee">>},{<<"type">>,<<"set">>}],[{xmlel,<<"session">>,[{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-session">>}],[]}]}]}
2015-04-17 11:33:57.722 [debug] <0.1592.0>@mod_bosh:info:208 Sending (binary) to <<"b851f0c040b750f263baa1437b4c04bb215c7762">>: <<"<body xmlns='http://jabber.org/protocol/httpbind' sid='b851f0c040b750f263baa1437b4c04bb215c7762'><iq id='bbc549cdc013b6a2e99fada1a71d1bee' type='result' xmlns='jabber:client'><session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq></body>">>
2015-04-17 11:33:57.723 [debug] <0.1592.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694159">>}],[{xmlel,<<"message">>,[{<<"to">>,<<"geralt@localhost/res1">>},{<<"type">>,<<"chat">>}],[{xmlel,<<"body">>,[],[{xmlcdata,<<"2nd!">>}]}]}]}
2015-04-17 11:33:57.724 [info] <0.1593.0>@mod_bosh_socket:handle_stream_event:433 deferring (rid: 1429263237694159, expected: 1429263237694158): {normal,{xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694159">>}],[{xmlel,<<"message">>,[{<<"to">>,<<"geralt@localhost/res1">>},{<<"type">>,<<"chat">>}],[{xmlel,<<"body">>,[],[{xmlcdata,<<"2nd!">>}]}]}]}}
2015-04-17 11:33:57.724 [debug] <0.1597.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694158">>}],[{xmlel,<<"message">>,[{<<"to">>,<<"geralt@localhost/res1">>},{<<"type">>,<<"chat">>}],[{xmlel,<<"body">>,[],[{xmlcdata,<<"1st!">>}]}]}]}
2015-04-17 11:33:57.724 [debug] <0.1593.0>@mod_bosh_socket:process_deferred_events:615 processing deferred event: {normal,{xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694159">>}],[{xmlel,<<"message">>,[{<<"to">>,<<"geralt@localhost/res1">>},{<<"type">>,<<"chat">>}],[{xmlel,<<"body">>,[],[{xmlcdata,<<"2nd!">>}]}]}]},1429263237694159}
2015-04-17 11:33:57.725 [debug] <0.1597.0>@mod_bosh:info:208 Sending (binary) to <<"b851f0c040b750f263baa1437b4c04bb215c7762">>: <<"<body xmlns='http://jabber.org/protocol/httpbind' sid='b851f0c040b750f263baa1437b4c04bb215c7762'/>">>
2015-04-17 11:33:57.737 [debug] <0.1597.0>@mod_bosh:info:204 Parsed body: {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},{<<"type">>,<<"terminate">>},{<<"sid">>,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>},{<<"rid">>,<<"1429263237694160">>}],[{xmlel,<<"presence">>,[{<<"type">>,<<"unavailable">>}],[]}]}
2015-04-17 11:33:57.738 [debug] <0.1592.0>@mod_bosh:info:208 Sending (binary) to <<"b851f0c040b750f263baa1437b4c04bb215c7762">>: <<"<body xmlns='http://jabber.org/protocol/httpbind' sid='b851f0c040b750f263baa1437b4c04bb215c7762'/>">>
    {state,{bosh_socket,<<"b851f0c040b750f263baa1437b4c04bb215c7762">>,<0.1593.0>,{{127,0,0,1},61604}},mod_bosh_socket,#Ref<0.0.0.16706>,true,"1290741097",{sasl_state,<<"jabber">>,<<"localhost">>,<<>>,#Fun<ejabberd_c2s.2.53596914>,#Fun<ejabberd_c2s.3.53596914>,#Fun<ejabberd_c2s.4.53596914>,undefined,undefined},all,none,{false,0},false,false,false,[verify_none],true,{jid,<<"carol">>,<<"localhost">>,<<"bosh">>,<<"carol">>,<<"localhost">>,<<"bosh">>},<<"carol">>,<<"localhost">>,<<"bosh">>,{{1429,263237,723221},<0.1594.0>},{1,{{<<"carol">>,<<"localhost">>,<<>>},nil,nil}},{1,{{<<"carol">>,<<"localhost">>,<<>>},nil,nil}},{0,nil},{0,nil},[],undefined,undefined,undefined,false,{userlist,none,[],false},unknown,ejabberd_auth_internal,{{127,0,0,1},61604},[],"en",false,0,undefined,0,[],0,100,1,600,undefined,undefined})
2015-04-17 11:33:57.738 [debug] <0.1593.0>@mod_bosh_socket:terminate:402 Closing session <<"b851f0c040b750f263baa1437b4c04bb215c7762">> in 'normal' state. Handlers: [] Pending: []
```

And produces this:

```xml
$ cat log/ejabberd.log | grep b851f0c040b750f263baa1437b4c04bb215c7762 | mlp
cwd: {ok,"/Users/erszcz/work/esl/mongooseim/rel/mongooseim.local"}
base dir: "/Users/erszcz/work/lavrin/mlp"
deps dir: ["/Users/erszcz/work/lavrin/mlp/deps/exml/ebin",
           "/Users/erszcz/work/lavrin/mlp/deps/pa/ebin",
           "/Users/erszcz/work/lavrin/mlp/deps/proper/ebin"]
<< bosh sent:

<body wait='60' requests='2' hold='1' from='localhost' accept='deflate,gzip' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmpp:restartlogic='true' xmpp:version='1.0' inactivity='30' maxpause='120' xmlns='http://jabber.org/protocol/httpbind' xmlns:xmpp='urn:xmpp:xbosh' xmlns:stream='http://etherx.jabber.org/streams'>
  <stream:features>
    <mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
      <mechanism>PLAIN</mechanism>
      <mechanism>DIGEST-MD5</mechanism>
      <mechanism>SCRAM-SHA-1</mechanism>
    </mechanisms>
    <sm xmlns='urn:xmpp:sm:3'/>
  </stream:features>
</body>

>> bosh received:

<body rid='1429263237694154' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <auth mechanism='PLAIN' xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>AGNhcm9sAHcwS3ZWbnFKR3ZyY3dSdiUyRmY1OWNteWdkeCUyQmtIeHZsUktGNmdTUEliblZQdGxSVGpMMDJtNWNQaEQ1V1ZGaFpH</auth>
</body>

<< bosh sent:

<body sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>
</body>

>> bosh received:

<body rid='1429263237694155' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xml:lang='en' to='localhost' xmpp:restart='true' xmlns='http://jabber.org/protocol/httpbind' xmlns:xmpp='urn:xmpp:xbosh'/>

<< bosh sent:

<body wait='60' requests='2' hold='1' from='localhost' accept='deflate,gzip' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmpp:restartlogic='true' xmpp:version='1.0' inactivity='30' maxpause='120' xmlns='http://jabber.org/protocol/httpbind' xmlns:xmpp='urn:xmpp:xbosh' xmlns:stream='http://etherx.jabber.org/streams'>
  <stream:features>
    <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
    <session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>
    <sm xmlns='urn:xmpp:sm:3'/>
  </stream:features>
</body>

>> bosh received:

<body rid='1429263237694156' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <iq type='set' id='8948c17a5ffea7c31362f0c7f262c56f'>
    <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>
      <resource>bosh</resource>
    </bind>
  </iq>
</body>

<< bosh sent:

<body sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <iq id='8948c17a5ffea7c31362f0c7f262c56f' type='result' xmlns='jabber:client'>
    <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>
      <jid>carol@localhost/bosh</jid>
    </bind>
  </iq>
</body>

>> bosh received:

<body rid='1429263237694157' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <iq type='set' id='bbc549cdc013b6a2e99fada1a71d1bee'>
    <session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>
  </iq>
</body>

<< bosh sent:

<body sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <iq type='result' id='bbc549cdc013b6a2e99fada1a71d1bee' xmlns='jabber:client'>
    <session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>
  </iq>
</body>

>> bosh received:

<body rid='1429263237694159' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <message type='chat' to='geralt@localhost/res1'>
    <body>2nd!</body>
  </message>
</body>

>> bosh received:

<body rid='1429263237694158' sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'>
  <message type='chat' to='geralt@localhost/res1'>
    <body>1st!</body>
  </message>
</body>

<< bosh sent:

<body sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'/>

>> bosh received:

<body rid='1429263237694160' sid='b851f0c040b750f263baa1437b4c04bb215c7762' type='terminate' xmlns='http://jabber.org/protocol/httpbind'>
  <presence type='unavailable'/>
</body>

<< bosh sent:

<body sid='b851f0c040b750f263baa1437b4c04bb215c7762' xmlns='http://jabber.org/protocol/httpbind'/>
```

That's all, folks, nothing more to see here.
