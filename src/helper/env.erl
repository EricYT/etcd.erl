%% application envs

-module(env).

-export([get/2, get/3, put/3, put/4, del/2]).

-spec get(App, Key) -> Value | 'undefined' when
  App :: atom(),
  Key :: atom(),
  Value :: term().
get(App, Key) ->
  case application:get_env(App, Key) of
    {ok, Value} -> Value;
    undefined -> undefined
  end.

-spec get(App, Key, Defalut) -> Value when
  App :: atom(),
  Key :: atom(),
  Defalut :: term(),
  Value :: term().
get(App, Key, Defalut) ->
  case application:get_env(App, Key) of
    {ok, Value} -> Value;
    undefined -> Defalut
  end.

-spec put(App, Key, Value) -> ok when
  App :: atom(),
  Key :: atom(),
  Value :: term().
put(App, Key, Value) ->
  application:set_env(App, Key, Value).

-spec put(App, Key, Value, Options) -> ok when
  App :: atom(),
  Key :: atom(),
  Value :: term(),
  Options :: [any(), ...].
put(App, Key, Value, Options) ->
  application:set_env(App, Key, Value, Options).

-spec del(App, Key) -> ok when
  App :: atom(),
  Key :: atom().
del(App, Key) ->
  application:unset_env(App, Key).

