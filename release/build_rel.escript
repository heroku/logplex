#!/usr/bin/env escript

main(["target", AppName, EbinDir]) ->
    code:add_patha(EbinDir),
    AppProps = app_props(AppName),
    AppDeps = proplists:get_value(applications, AppProps, []),
    AppVsn = proplists:get_value(vsn, AppProps),
    ok = write_rel_file(AppName, AppDeps, AppVsn),
    file:write_file("sys.config", <<"[].">>),
    file:make_dir("releases"),
    systools:make_script("release/" ++ AppName ++ "-" ++ AppVsn, [no_module_tests]),
    file:copy(filename:join([code:root_dir(), "bin", "start_clean.boot"]), "bin/start.boot"),
    file:write_file("releases/start_erl.data", iolist_to_binary([erlang:system_info(version), " ", AppVsn, "\n"])),
    systools:make_tar("release/" ++ AppName ++ "-" ++ AppVsn, [{erts, code:root_dir()}, {dirs, ['releases' | tar_dirs()]}]),
    file:delete("sys.config"),
    file:delete("bin/start.boot"),
    file:delete("releases/start_erl.data"),
    file:del_dir("releases"),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".boot"),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".rel"),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".script"),
    ok;
    
main(["release", AppName, EbinDir]) ->
    case filelib:is_regular("release/" ++ AppName ++ ".appup") of
        true ->
            ok;
        false ->
            io:format("*** missing release/~s.appup~n", [AppName]),
            halt(1)
    end,
    code:add_patha(EbinDir),
    AppProps = app_props(AppName),
    AppDeps = proplists:get_value(applications, AppProps, []),
    AppVsn = proplists:get_value(vsn, AppProps),
    ok = write_rel_file(AppName, AppDeps, AppVsn),
    systools:make_script("release/" ++ AppName ++ "-" ++ AppVsn, [{exref, AppDeps}, {outdir, "release"}]),
    systools:make_tar("release/" ++ AppName ++ "-" ++ AppVsn, [{dirs, tar_dirs()}, {outdir, "release"}]),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".boot"),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".rel"),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".script"),
    ok;
    
main(["boot", AppName, EbinDir]) ->
    code:add_patha(EbinDir),
    AppProps = app_props(AppName),
    AppDeps = proplists:get_value(applications, AppProps, []),
    AppVsn = proplists:get_value(vsn, AppProps),
    ok = write_rel_file(AppName, AppDeps, AppVsn),
    systools:make_script("release/" ++ AppName ++ "-" ++ AppVsn, [{exref, AppDeps}, {outdir, "release"}]),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".rel"),
    file:delete("release/" ++ AppName ++ "-" ++ AppVsn ++ ".script"),
    ok.
    
write_rel_file(AppName, AppDeps, AppVsn) ->
    {ok, FD} = file:open("release/" ++ AppName ++ "-" ++ AppVsn ++ ".rel", [write]),
    RelInfo = {release,
               {AppName, AppVsn},
               {erts, erts_vsn()}, 
               [{Pkg, lib_vsn(Pkg)} || Pkg <- AppDeps] ++
			   [{list_to_atom(AppName), AppVsn}]
              },
    io:format(FD, "~p.", [RelInfo]),
    file:close(FD),
    ok.
    
app_props(AppName) ->
    {ok, [{application,_,AppProps}]} = file:consult("ebin/" ++ AppName ++ ".app"),
    AppProps.
    
erts_vsn() ->
    erlang:system_info(version).
	
lib_vsn(App) ->
    load(App),
    {ok, Vsn} = application:get_key(App, vsn),
    Vsn.
    
load(App) ->
    case application:load(App) of
        ok -> 
            ok;             
        {error, {already_loaded, _}} -> 
            ok;
        E -> 
            io:format(standard_error, "Warning - can't load ~p (~p)~n", [App, E]),
            erlang:exit(E)
    end.

tar_dirs() ->
    {ok, Files} = file:list_dir("."),
    [list_to_atom(Dir) || Dir <- lists:filter(
        fun ("." ++ _) -> false;
            (File) ->
            filelib:is_dir(File) andalso not lists:member(File, ["bin"])
        end, Files)].
        
        
