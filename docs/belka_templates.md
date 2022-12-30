-*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
ex: ts=4 sw=4 et

# Overview

A simpble gen server to load and serve templates

```erlang

-module(belka_templates).

```

[Gen Server Behaviour](http://erlang.org/doc/design_principles/gen_server_concepts.html)

```erlang
-behaviour(gen_server).

-define(NEWLINE, [10]).
-define(CRLF,    [13, 10]).

```

## API Export Definition

```erlang
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

```

## Callback API Export Definition

```erlang
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


```

## Function API Export Definition

```erlang
-export([
            render/2,
            render/3
        ]).

```

## Admin/Debug API Export Definition

```erlang
-export([
            reload_templates/0,
            list_templates/0
        ]).

-record(state, {templates = []}).

```

## OTP API

```erlang

start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->
    start_link(#{}).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

```

## Functional API

```erlang

render(Template, Vals) ->
    gen_server:call(?MODULE, {render, {Template, Vals}}).

render(CallBackMod, Template, Vals) ->
    gen_server:call(?MODULE, {render, {CallBackMod, Template, Vals}}).

```

## Admin/debugging API

```erlang

reload_templates() ->
    gen_server:call(?MODULE, reload_templates).

list_templates() ->
    gen_server:call(?MODULE, list_templates).

```

## Normal Gen Server Callbacks

On starting the server `init` loads the templates

```erlang

init(_Args) ->
    true = register(?MODULE, self()),
    Templates = load_templates(),
    {ok, #state{templates = Templates}}.

```

The server handles two types of template calls that map to `dactyl:render/2` and `dactyl:render/3`

```erlang

handle_call({render, {Template, Vals}}, _From, #state{templates = Templates} = State) ->
    T = maps:get(Template, Templates),
    Binary = list_to_binary(dactyl:render(T, Vals)),
    Reply = re:replace(Binary, ?NEWLINE, ?CRLF, [global]),
    {reply, Reply, State};

handle_call({render, {CallBackMod, Template, Vals}}, _From, #state{templates = Templates} = State) ->
    T = maps:get(Template, Templates),
    Binary = list_to_binary(dactyl:render(CallBackMod, T, Vals)),
    Reply = re:replace(Binary, ?NEWLINE, ?CRLF, [global]),
    {reply, Reply, State};

```

There are a couple of functions for when you are developing. If you create a new template or tinker with an existing one you can ask the server to reload from disk.

```erlang

handle_call(reload_templates, _From, State) ->
    io:format("reloading templates~n"),
    Templates = load_templates(),
    print_templates(Templates),
    {reply, ok, State#state{templates = Templates}};

handle_call(list_templates, _From, State) ->
    #state{templates = Templates} = State,
    print_templates(Templates),
    {reply, ok, State};

```

We ignore any other Gen Server call.

```erlang

handle_call(Request, _From, State) ->
    io:format("got request ~p~n", [Request]),
    {reply, ignored, State}.

```

We ignore all cast and info messages.

```erlang

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

```

We do no clean up on terminate or code change.

```erlang

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

```

## Internal functions

```erlang

print_templates(Templates) ->
    io:format("The loaded templates are:~n"),
    [io:format("* ~s~n", [X]) || {X, _} <- maps:to_list(Templates)].

```

Templates are expected to be in ./priv/templates
***TODO***: this won't work in production babes need to specify a location

```erlang

load_templates() ->
    {ok, Templates} = file:list_dir("./priv/templates"),
    Compiled = [{filename:basename(X, ".txt"), compile(X)} || X <- Templates],
    maps:from_list(Compiled).

compile(File) ->
    {ok, Binary} = file:read_file(filename:join("./priv/templates", File)),
    CharList = binary_to_list(Binary),
    {ok, Compiled} = dactyl:compile(unicode:characters_to_list(CharList, utf8)),
    Compiled.
```
