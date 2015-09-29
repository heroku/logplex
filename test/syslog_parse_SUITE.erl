-module(syslog_parse_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../src/logplex.hrl").
-compile(export_all).

all() -> [logplex_utils, logplex_syslog_utils].

logplex_utils(_Config) ->
    [#msg{time= <<"2013-05-27T19:59:36.538297+00:00">>, ps= <<"router">>,
          content= <<"at=info method=POST path=/drain host=fakeapp.herokuapp.com fwd=\"108.21.18.132\" dyno=web.1 connect=2ms service=3ms status=200 bytes=0">>,
          source= <<"t.8ec095b4-8960-39fa-b948-e9e21f27f5de">>},
     #msg{time= <<"2013-05-27T20:10:25.840515+00:00">>, ps= <<"web.123">>,
          content= <<"Bid request was invalid. Problem field was [\"fake\", \"device\"]: {\"app\"=>{\"aid\"=>\"7777777777777777777777777777777777\", \"cat\"=>[\"IE.09\", \"AAAAA\", \"bbbbbbbbb\", \"CCCCCCCCCCCCCCCCC\"], \"global_uid\"=>\"com.javas.android\", \"name\"=>\"Javas Android\", \"paid\"=>0, \"pid\"=>\"aplub3B19i1pbmNyEAsSB0Fgu291rnQY7cCnEc=\", \"pub\"=>\"Javas\"}, \"at\"=>2, \"device\"=>{\"dpid\"=>\"c8e9183of3b6dbaaaaaaadf80d22ed2a555555ac\", \"ip\"=>\"11.72.3.20\", \"js\"=>1, \"loc\"=>\"-25.6259753934,-44.6876782128\", \"os\"=>\"Android\", \"osv\"=>\"4.1.1\", \"ua\"=>\"Mozilla/5.0 (Linux; U; Android 4.1.2; pt-br; GT-I9300 Build/JZO54K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30\"}, \"id\"=>\"cccccccc-4444-38de-0c1e-a5fe903e1f1f\", \"imp\"=>[{\"api\"=>3, \"battr\"=>[\"91\", \"1\", \"24\"], \"displaymanaserver\"=>\"4.910.78\", \"h\"=>50, \"impid\"=>\"eeeeeeee-aaaa-5555-9999-000000000000\", \"instl\"=>0, \"w\"=>320}], \"pf\"=>0.82, \"restrictions\"=>{\"badv\"=>[\"\\\".woo/wo/\\\"\", \"\\\"/webobjects/\\\"\", \"\\\"fbiclient\\\"\", \"\\\"ital\", \"\\\"msuserxp\\\"\", \"\\\"fbi.chi.metrics.senderaddr\\\":\", \"\\\"fbi.nyc.metrics.urla\\\",">>,
          source= <<"t.f872fbca-6673-e1a2-556e-6d1090ae13e4">>},
     #msg{time= <<"2013-05-27T20:10:25.840515+00:00">>, ps= <<"web.123">>,
          content= <<"Bid request was invalid. Problem field was [\"fake\", \"device\"]: {\"app\"=>{\"aid\"=>\"7777777777777777777777777777777777\", \"cat\"=>[\"IE.09\", \"AAAAA\", \"bbbbbbbbb\", \"CCCCCCCCCCCCCCCCC\"], \"global_uid\"=>\"com.javas.android\", \"name\"=>\"Javas Android\", \"paid\"=>0, \"pid\"=>\"aplub3B19i1pbmNyEAsSB0Fgu291rnQY7cCnEc=\", \"pub\"=>\"Javas\"}, \"at\"=>2, \"device\"=>{\"dpid\"=>\"c8e9183of3b6dbaaaaaaadf80d22ed2a555555ac\", \"ip\"=>\"11.72.3.20\", \"js\"=>1, \"loc\"=>\"-25.6259753934,-44.6876782128\", \"os\"=>\"Android\", \"osv\"=>\"4.1.1\", \"ua\"=>\"Mozilla/5.0 (Linux; U; Android 4.1.2; pt-br; GT-I9300 Build/JZO54K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30\"}, \"id\"=>\"cccccccc-4444-38de-0c1e-a5fe903e1f1f\", \"imp\"=>[{\"api\"=>3, \"battr\"=>[\"91\", \"1\", \"24\"], \"displaymanaserver\"=>\"4.910.78\", \"h\"=>50, \"impid\"=>\"eeeeeeee-aaaa-5555-9999-000000000000\", \"instl\"=>0, \"w\"=>320}], \"pf\"=>0.82, \"restrictions\"=>{\"badv\"=>[\"\\\".woo/wo/\\\"\", \"\\\"/webobjects/\\\"\", \"\\\"fbiclient\\\"\", \"\\\"ital\", \"\\\"msuserxp\\\"\", \"\\\"fbi.chi.metrics.senderaddr\\\":\", \"\\\"fbi.nyc.metrics.urla\\\",">>,
          source= <<"t.f872fbca-6673-e1a2-556e-6d1090ae13e4">>},
     #msg{time= <<"2013-05-27T20:33:11.944925+00:00">>, ps= <<"high.47">>,
          content= <<"push[foogle_cloud_messaging] from:\"Group:7189636\" to:\"AAAAAAArUM4dAqiulchv2222222222223UEmxzaAwFcw69c8VF7ZZZZZZZZZwsRRRRR-=====_NZ-hooooobrrbEHR4XiKc6CLoEEEEEr5cM8VeKriEcccccccz4FuneTXUy5k00000000000000dsthT???????Ow\" message_id:\"777777784c8c2acf\" text:\"FERDP (HABS): gg gomez we played tuff\"">>,
          source= <<"t.aaaaaaaa-ea9b-1c80-fedc-731e6b6045c0">>},
     #msg{time= <<"2013-05-24T06:48:51.740194+00:00">>, ps= <<"process">>,
          content= <<"this is the message">>,
          source= <<"token">>},
     #msg{time= <<"2013-05-24T06:48:51.740194+00:00">>, ps= <<"process">>,
          content= <<"this is the message">>,
          source= <<"token">>},
     #msg{time= <<"2013-05-24T06:48:51.740194+00:00">>, ps= <<"process">>,
          content= <<"this is the message">>,
          source= <<"token">>},
     #msg{time= <<"2013-05-24T06:48:51.740194+00:00">>, ps= <<"process">>,
          content= <<"this is the message">>,
          source= <<"token">>},
     #msg{time= <<"2013-05-24T06:48:51.740194+00:00">>, ps= <<"process">>,
          content= <<"this is the message">>,
          source= <<"token">>}]
    = [logplex_utils:parse_msg(Msg) || Msg <- logs()].

logplex_syslog_utils(_Config) ->
    %% facility, severity, time, source, ps, content
    [{19, 6, <<"2013-05-27T19:59:36.538297+00:00">>, <<"t.8ec095b4-8960-39fa-b948-e9e21f27f5de">>, <<"router">>,  <<"at=info method=POST path=/drain host=fakeapp.herokuapp.com fwd=\"108.21.18.132\" dyno=web.1 connect=2ms service=3ms status=200 bytes=0">>},
     { 1, 5, <<"2013-05-27T20:10:25.840515+00:00">>, <<"t.f872fbca-6673-e1a2-556e-6d1090ae13e4">>, <<"web.123">>, <<"Bid request was invalid. Problem field was [\"fake\", \"device\"]: {\"app\"=>{\"aid\"=>\"7777777777777777777777777777777777\", \"cat\"=>[\"IE.09\", \"AAAAA\", \"bbbbbbbbb\", \"CCCCCCCCCCCCCCCCC\"], \"global_uid\"=>\"com.javas.android\", \"name\"=>\"Javas Android\", \"paid\"=>0, \"pid\"=>\"aplub3B19i1pbmNyEAsSB0Fgu291rnQY7cCnEc=\", \"pub\"=>\"Javas\"}, \"at\"=>2, \"device\"=>{\"dpid\"=>\"c8e9183of3b6dbaaaaaaadf80d22ed2a555555ac\", \"ip\"=>\"11.72.3.20\", \"js\"=>1, \"loc\"=>\"-25.6259753934,-44.6876782128\", \"os\"=>\"Android\", \"osv\"=>\"4.1.1\", \"ua\"=>\"Mozilla/5.0 (Linux; U; Android 4.1.2; pt-br; GT-I9300 Build/JZO54K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30\"}, \"id\"=>\"cccccccc-4444-38de-0c1e-a5fe903e1f1f\", \"imp\"=>[{\"api\"=>3, \"battr\"=>[\"91\", \"1\", \"24\"], \"displaymanaserver\"=>\"4.910.78\", \"h\"=>50, \"impid\"=>\"eeeeeeee-aaaa-5555-9999-000000000000\", \"instl\"=>0, \"w\"=>320}], \"pf\"=>0.82, \"restrictions\"=>{\"badv\"=>[\"\\\".woo/wo/\\\"\", \"\\\"/webobjects/\\\"\", \"\\\"fbiclient\\\"\", \"\\\"ital\", \"\\\"msuserxp\\\"\", \"\\\"fbi.chi.metrics.senderaddr\\\":\", \"\\\"fbi.nyc.metrics.urla\\\",">>},
     { 1, 5, <<"2013-05-27T20:10:25.840515+00:00">>, <<"t.f872fbca-6673-e1a2-556e-6d1090ae13e4">>, <<"web.123">>, <<"Bid request was invalid. Problem field was [\"fake\", \"device\"]: {\"app\"=>{\"aid\"=>\"7777777777777777777777777777777777\", \"cat\"=>[\"IE.09\", \"AAAAA\", \"bbbbbbbbb\", \"CCCCCCCCCCCCCCCCC\"], \"global_uid\"=>\"com.javas.android\", \"name\"=>\"Javas Android\", \"paid\"=>0, \"pid\"=>\"aplub3B19i1pbmNyEAsSB0Fgu291rnQY7cCnEc=\", \"pub\"=>\"Javas\"}, \"at\"=>2, \"device\"=>{\"dpid\"=>\"c8e9183of3b6dbaaaaaaadf80d22ed2a555555ac\", \"ip\"=>\"11.72.3.20\", \"js\"=>1, \"loc\"=>\"-25.6259753934,-44.6876782128\", \"os\"=>\"Android\", \"osv\"=>\"4.1.1\", \"ua\"=>\"Mozilla/5.0 (Linux; U; Android 4.1.2; pt-br; GT-I9300 Build/JZO54K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30\"}, \"id\"=>\"cccccccc-4444-38de-0c1e-a5fe903e1f1f\", \"imp\"=>[{\"api\"=>3, \"battr\"=>[\"91\", \"1\", \"24\"], \"displaymanaserver\"=>\"4.910.78\", \"h\"=>50, \"impid\"=>\"eeeeeeee-aaaa-5555-9999-000000000000\", \"instl\"=>0, \"w\"=>320}], \"pf\"=>0.82, \"restrictions\"=>{\"badv\"=>[\"\\\".woo/wo/\\\"\", \"\\\"/webobjects/\\\"\", \"\\\"fbiclient\\\"\", \"\\\"ital\", \"\\\"msuserxp\\\"\", \"\\\"fbi.chi.metrics.senderaddr\\\":\", \"\\\"fbi.nyc.metrics.urla\\\",">>},
     { 1, 5, <<"2013-05-27T20:33:11.944925+00:00">>, <<"t.aaaaaaaa-ea9b-1c80-fedc-731e6b6045c0">>, <<"high.47">>, <<"push[foogle_cloud_messaging] from:\"Group:7189636\" to:\"AAAAAAArUM4dAqiulchv2222222222223UEmxzaAwFcw69c8VF7ZZZZZZZZZwsRRRRR-=====_NZ-hooooobrrbEHR4XiKc6CLoEEEEEr5cM8VeKriEcccccccz4FuneTXUy5k00000000000000dsthT???????Ow\" message_id:\"777777784c8c2acf\" text:\"FERDP (HABS): gg gomez we played tuff\"">>},
     {16, 5, <<"2013-05-24T06:48:51.740194+00:00">>, <<"token">>, <<"process">>, <<"this is the message">>},
     {16, 5, <<"2013-05-24T06:48:51.740194+00:00">>, <<"token">>, <<"process">>, <<"this is the message">>},
     {16, 5, <<"2013-05-24T06:48:51.740194+00:00">>, <<"token">>, <<"process">>, <<"this is the message">>},
     {16, 5, <<"2013-05-24T06:48:51.740194+00:00">>, <<"token">>, <<"process">>, <<"this is the message">>},
     {16, 5, <<"2013-05-24T06:48:51.740194+00:00">>, <<"token">>, <<"process">>, <<"this is the message">>}]
    = [logplex_syslog_utils:from_msg(Msg) || Msg <- logs()].

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%
logs() ->
    [<<"<158>1 2013-05-27T19:59:36.538297+00:00 node.78286@heroku.com t.8ec095b4-8960-39fa-b948-e9e21f27f5de router - - at=info method=POST path=/drain host=fakeapp.herokuapp.com fwd=\"108.21.18.132\" dyno=web.1 connect=2ms service=3ms status=200 bytes=0">>,
     <<"<13>1 2013-05-27T20:10:25.840515+00:00 runtime.12345@heroku.com t.f872fbca-6673-e1a2-556e-6d1090ae13e4 web.123 - - Bid request was invalid. Problem field was [\"fake\", \"device\"]: {\"app\"=>{\"aid\"=>\"7777777777777777777777777777777777\", \"cat\"=>[\"IE.09\", \"AAAAA\", \"bbbbbbbbb\", \"CCCCCCCCCCCCCCCCC\"], \"global_uid\"=>\"com.javas.android\", \"name\"=>\"Javas Android\", \"paid\"=>0, \"pid\"=>\"aplub3B19i1pbmNyEAsSB0Fgu291rnQY7cCnEc=\", \"pub\"=>\"Javas\"}, \"at\"=>2, \"device\"=>{\"dpid\"=>\"c8e9183of3b6dbaaaaaaadf80d22ed2a555555ac\", \"ip\"=>\"11.72.3.20\", \"js\"=>1, \"loc\"=>\"-25.6259753934,-44.6876782128\", \"os\"=>\"Android\", \"osv\"=>\"4.1.1\", \"ua\"=>\"Mozilla/5.0 (Linux; U; Android 4.1.2; pt-br; GT-I9300 Build/JZO54K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30\"}, \"id\"=>\"cccccccc-4444-38de-0c1e-a5fe903e1f1f\", \"imp\"=>[{\"api\"=>3, \"battr\"=>[\"91\", \"1\", \"24\"], \"displaymanaserver\"=>\"4.910.78\", \"h\"=>50, \"impid\"=>\"eeeeeeee-aaaa-5555-9999-000000000000\", \"instl\"=>0, \"w\"=>320}], \"pf\"=>0.82, \"restrictions\"=>{\"badv\"=>[\"\\\".woo/wo/\\\"\", \"\\\"/webobjects/\\\"\", \"\\\"fbiclient\\\"\", \"\\\"ital\", \"\\\"msuserxp\\\"\", \"\\\"fbi.chi.metrics.senderaddr\\\":\", \"\\\"fbi.nyc.metrics.urla\\\",">>,
     <<"<13>1 2013-05-27T20:10:25.840515+00:00 runtime.12345@heroku.com t.f872fbca-6673-e1a2-556e-6d1090ae13e4 web.123 - [meta testName=\"518\\\]\"] Bid request was invalid. Problem field was [\"fake\", \"device\"]: {\"app\"=>{\"aid\"=>\"7777777777777777777777777777777777\", \"cat\"=>[\"IE.09\", \"AAAAA\", \"bbbbbbbbb\", \"CCCCCCCCCCCCCCCCC\"], \"global_uid\"=>\"com.javas.android\", \"name\"=>\"Javas Android\", \"paid\"=>0, \"pid\"=>\"aplub3B19i1pbmNyEAsSB0Fgu291rnQY7cCnEc=\", \"pub\"=>\"Javas\"}, \"at\"=>2, \"device\"=>{\"dpid\"=>\"c8e9183of3b6dbaaaaaaadf80d22ed2a555555ac\", \"ip\"=>\"11.72.3.20\", \"js\"=>1, \"loc\"=>\"-25.6259753934,-44.6876782128\", \"os\"=>\"Android\", \"osv\"=>\"4.1.1\", \"ua\"=>\"Mozilla/5.0 (Linux; U; Android 4.1.2; pt-br; GT-I9300 Build/JZO54K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30\"}, \"id\"=>\"cccccccc-4444-38de-0c1e-a5fe903e1f1f\", \"imp\"=>[{\"api\"=>3, \"battr\"=>[\"91\", \"1\", \"24\"], \"displaymanaserver\"=>\"4.910.78\", \"h\"=>50, \"impid\"=>\"eeeeeeee-aaaa-5555-9999-000000000000\", \"instl\"=>0, \"w\"=>320}], \"pf\"=>0.82, \"restrictions\"=>{\"badv\"=>[\"\\\".woo/wo/\\\"\", \"\\\"/webobjects/\\\"\", \"\\\"fbiclient\\\"\", \"\\\"ital\", \"\\\"msuserxp\\\"\", \"\\\"fbi.chi.metrics.senderaddr\\\":\", \"\\\"fbi.nyc.metrics.urla\\\",">>,
     <<"<13>1 2013-05-27T20:33:11.944925+00:00 runtime.45678@heroku.com t.aaaaaaaa-ea9b-1c80-fedc-731e6b6045c0 high.47 - - push[foogle_cloud_messaging] from:\"Group:7189636\" to:\"AAAAAAArUM4dAqiulchv2222222222223UEmxzaAwFcw69c8VF7ZZZZZZZZZwsRRRRR-=====_NZ-hooooobrrbEHR4XiKc6CLoEEEEEr5cM8VeKriEcccccccz4FuneTXUy5k00000000000000dsthT???????Ow\" message_id:\"777777784c8c2acf\" text:\"FERDP (HABS): gg gomez we played tuff\"">>,
     <<"<133>1 2013-05-24T06:48:51.740194+00:00 host token process - - this is the message">>,
     <<"<133>1 2013-05-24T06:48:51.740194+00:00 host token process - [meta sequenceId=\"518\"] this is the message">>,
     <<"<133>1 2013-05-24T06:48:51.740194+00:00 host token process - [meta testName=\"bob\\\]\"] this is the message">>,
     <<"<133>1 2013-05-24T06:48:51.740194+00:00 host token process - [meta sequenceId=\"518\"][meta somethingElse=\"blah\"] this is the message">>,
     <<"<133>1 2013-05-24T06:48:51.740194+00:00 host token process - [meta sequenceId=\"518\"][meta somethingElse=\"blah\\\]\"] this is the message">>].
