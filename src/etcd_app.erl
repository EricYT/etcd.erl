-module(etcd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  case etcd_sup:start_link() of
    {ok, Pid} ->
      reloader:start(),
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.
