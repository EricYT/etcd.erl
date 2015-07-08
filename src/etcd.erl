-module(etcd).

-export([start/0, stop/0]).
-export([set/2, set/3, set/4]).
-compile(export_all).

-include("include/etcd_types.hrl").

-define(DEFAULT_TIMEOUT, 50000).

%% @doc Start application with all depencies
-spec start() -> ok | {error, term()}.
start() ->
  case application:ensure_all_started(etcd) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Stop application
-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(etcd).

-spec set(key(), value()) -> result().
set(Key, Value) ->
  set(Key, Value, ?DEFAULT_TIMEOUT).

-spec set(key(), value(), pos_timeout()) -> result().
set(Key, Value, Timeout) ->
  Url = env:get(etcd, etcd_url, "http://192.168.59.103:4001"),
  set_(Url, Key, Value, Timeout).

%% @spec (Url, Key, Value, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   Value = binary() | string()
%%   Timeout = pos_integer() | 'infinity'
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec set_(url(), key(), value(), pos_timeout()) -> result().
set_(Url, Key, Value, Timeout) ->
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key),
  Result = put_request(FullUrl, #{value => Value}, Timeout),
  handle_request_result(Result).

-spec set(key(), value(), pos_timeout(), pos_timeout()) -> result().
set(Key, Value, TTL, Timeout) ->
  Url = env:get(etcd, etcd_url, "http://192.168.59.103:4001"),
  set_(Url, Key, Value, TTL, Timeout).

%% @spec (Url, Key, Value, TTL, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   Value = binary() | string()
%%   TTL = pos_integer()
%%   Timeout = pos_integer() | infinity
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec set_(url(), key(), value(), pos_integer(), pos_timeout()) -> result().
set_(Url, Key, Value, TTL, Timeout) ->
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key),
  Result = put_request(FullUrl, #{value => Value, ttl => TTL}, Timeout),
  handle_request_result(Result).


%% @spec (Url, Key, PrevValue, Value, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   PrevValue = binary() | string()
%%   Value = binary() | string()
%%   Timeout = pos_integer() | 'infinity'
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec test_and_set(key(), value(), value(), pos_timeout()) -> result().
test_and_set(Key, PrevValue, Value, Timeout) ->
  Url = env:get(etcd, etcd_url, "http://192.168.59.103:4001"),
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key),
  Result = put_request(FullUrl, #{value => Value, prevValue => PrevValue}, Timeout),
  handle_request_result(Result).

%% @spec (Url, Key, PrevValue, Value, TTL, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   PrevValue = binary() | string()
%%   Value = binary() | string()
%%   TTL = pos_integer()
%%   Timeout = pos_integer() | infinity
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec test_and_set(key(), value(), value(), pos_integer(), pos_timeout()) -> result().
test_and_set(Key, PrevValue, Value, TTL, Timeout) ->
  Url = env:get(etcd, etcd_url, "http://192.168.59.103:4001"),
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key),
  Result = put_request(FullUrl, #{value => Value, prevValue => PrevValue, ttl => TTL}, Timeout),
  handle_request_result(Result).

%% @spec (Url, Key, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   Timeout = pos_integer() | infinity
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec get(key(), pos_timeout()) -> result().
get(Key, Timeout) ->
  Url = env:get(etcd, etcd_url, "http://192.168.59.103:4001"),
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key),
  io:format("---------------> get url:~p~n", [FullUrl]),
  Result = lhttpc:request(FullUrl, get, [], Timeout),
  handle_request_result(Result).

%% @spec (Url, Key, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   Timeout = pos_integer() | infinity
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec delete(key(), pos_timeout()) -> result().
delete(Key, Timeout) ->
  Url = env:get(etcd, etcd_url, "http://192.168.59.103:4001"),
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key),
  Result = lhttpc:request(FullUrl, delete, [], Timeout),
  handle_request_result(Result).

-spec watch(key(), Recursive::boolean(), pos_timeout()) -> result().
watch(Key, Recursive, Timeout) ->
  Url = env:get(etcd, etcd_url, "http://192.168.59.103:4001"),
  watch_(Url, Key, Recursive, Timeout).

%% @spec (Url, Key, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   Timeout = pos_integer() | infinity
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec watch_(url(), key(), Recursive::boolean(), pos_timeout()) -> result().
watch_(Url, Key, Recursive, Timeout) ->
  BaseQuery = [{wait, true}],
  Query = case Recursive of
    true -> BaseQuery++[{recursive, true}];
    false-> BaseQuery
  end,
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key) ++ "?" ++ encode_params(Query),
  io:format("----------> watch_ url:~p~n", [FullUrl]),
  Result = lhttpc:request(FullUrl, get, [], Timeout),
  handle_request_result(Result).

%% @spec (Url, Key, Index, Timeout) -> Result
%%   Url = string()
%%   Key = binary() | string()
%%   Index = pos_integer()
%%   Timeout = pos_integer() | infinity
%%   Result = {ok, response() | [response()]} | {http_error, atom()}.
%% @end
-spec watch_(url(), key(), pos_integer(), Recursive::boolean(), pos_timeout()) -> result().
watch_(Url, Key, Index, Recursive, Timeout) ->
  BaseQuery = [{wait, true}, {waitIndex, Index}],
  Query = case Recursive of
    true -> BaseQuery++[{recursive, true}];
    false-> BaseQuery
  end,
  FullUrl = url_prefix(Url) ++ "/keys" ++ convert_to_string(Key) ++ "?" ++ encode_params(Query),
  io:format("----------> watch_ url:~p~n", [FullUrl]),
  Result = put_request(FullUrl, [{"index", Index}], Timeout),
  handle_request_result(Result).

-spec sadd(url(), key(), value(), pos_timeout()) -> ok | {error, any()}.
sadd(Url, Key, Value, Timeout) -> etcd_sets:add(Url, Key, Value, Timeout).

-spec sadd(url(), key(), value(), pos_integer(), pos_timeout()) -> ok | {error, any()}.
sadd(Url, Key, Value, TTL, Timeout) -> etcd_sets:add(Url, Key, Value, TTL, Timeout).

-spec sdel(url(), key(), value(), pos_timeout()) -> ok | {error, any()}.
sdel(Url, Key, Value, Timeout) -> etcd_sets:del(Url, Key, Value, Timeout).

-spec sismember(url(), key(), value(), pos_timeout()) -> {ok, boolean()} | {error, any()}.
sismember(Url, Key, Value, Timeout) -> etcd_sets:ismember(Url, Key, Value, Timeout).

-spec smembers(url(), key(), pos_timeout()) -> {ok, [binary()]} | {error, any()}.
smembers(Url, Key, Timeout) -> etcd_sets:members(Url, Key, Timeout).

%% @private
convert_to_string(Value) when is_integer(Value) ->
  integer_to_list(Value);
convert_to_string(Value) when is_binary(Value) ->
  binary_to_list(Value);
convert_to_string(Value) when is_atom(Value) ->
  atom_to_list(Value);
convert_to_string(Value) when is_list(Value) ->
  Value.

%% @private
encode_params(Pairs) ->
  List = [ http_uri:encode(convert_to_string(Key)) ++ "=" ++ http_uri:encode(convert_to_string(Value)) || {Key, Value} <- Pairs ],
  %%binary:list_to_bin(string:join(List, "&")).
  string:join(List, "&").

%% @private
url_prefix(Url) ->
  Url ++ "/v2".

%% @private
put_request(Url, Pairs, Timeout) ->
  Body = jiffy:encode(Pairs),
  Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
  lhttpc:request(Url, put, Headers, Body, Timeout).

%% @private
handle_request_result(Result) ->
  case Result of
    {ok, {{StatusCode, _ReasonPhrase}, _Hdrs, ResponseBody}} ->
      io:format("--------> code:~p~n", [Result]),
      case StatusCode div 100 of
        2 ->
         Decoded = jiffy:decode(ResponseBody),
         {ok, Decoded};
        _ ->
          Error = try
            jiffy:decode(ResponseBody)
          catch _:_ ->
            ResponseBody
          end,
          {error, Error}
      end;
    {error, Reason} ->
      {http_error, Reason}
  end.
