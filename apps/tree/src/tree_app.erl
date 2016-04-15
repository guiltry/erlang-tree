%%%-------------------------------------------------------------------
%% @doc tree public API
%% @end
%%%-------------------------------------------------------------------

-module(tree_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, empty/0, insert/2]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    tree_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
empty() ->
  {node, 'nil'}.

insert(Val, {node, 'nil'}) ->
  {node, {Val, {node, 'nil'}, {node, 'nil'}} };
insert(NewVal, {node, {Val, Smaller, Bigger}}) when NewVal < Val ->
  {node, {Val, insert(NewVal, Smaller), Bigger} };
insert(NewVal, {node, {Val, Smaller, Bigger}}) when NewVal > Val ->
  {node, {Val, Smaller, insert(NewVal, Bigger)} }.

%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_test() ->
  {node, 'nil'} = empty().

insert_empty_test() ->
  {node, {1, {node, 'nil'}, {node, 'nil'}}} = insert(1, empty()).

insert_twice_lower_test() ->
  Result = insert(10, empty()),
  FirstLevel = {node, {5, {node, 'nil'}, {node, 'nil'}}},
  Expected = {node, {10, FirstLevel, {node, 'nil'}}},

  Expected = insert(5, Result).

insert_twice_bigger_test() ->
  Result = insert(10, empty()),
  FirstLevel = {node, {15, {node, 'nil'}, {node, 'nil'}}},
  Expected = {node, {10, {node, 'nil'}, FirstLevel}},

  Expected = insert(15, Result).

-endif.
