%%%-------------------------------------------------------------------
%% @doc tree public API
%% @end
%%%-------------------------------------------------------------------

-module(tree_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, empty/0, insert/2, lookup/2]).

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

%%--------------------------------------------------------------------
insert(Val, {node, 'nil'}) ->
  {node, {Val, {node, 'nil'}, {node, 'nil'}} };
insert(NewVal, {node, {Val, Smaller, Bigger}}) when NewVal < Val ->
  {node, {Val, insert(NewVal, Smaller), Bigger} };
insert(NewVal, {node, {Val, Smaller, Bigger}}) when NewVal > Val ->
  {node, {Val, Smaller, insert(NewVal, Bigger)} }.

%%--------------------------------------------------------------------
lookup(Target, Node) ->
  lookup_process(Target, Node, 0).

lookup_process(_, {node, 'nil'}, Acc) ->
  {false, Acc};
lookup_process(Target, {node, {Value, _, _}}, Acc) when Target =:= Value ->
  {true, Acc};
lookup_process(Target, {node, {Value, SmallerNode, _}}, Acc) when Target < Value ->
  lookup_process(Target, SmallerNode, Acc+1);
lookup_process(Target, {node, {Value, _, BiggerNode}}, Acc) when Target > Value ->
  lookup_process(Target, BiggerNode, Acc+1).

%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Test
%%====================================================================
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

empty_test() ->
  {node, 'nil'} = empty().

%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
lookup_empty_test() ->
  {false, 0} = lookup(1, empty()).

lookup_found_test() ->
  Tree = insert(1, empty()),

  {true, 0} = lookup(1, Tree).

lookup_not_found_smaller_test() ->
  Tree = insert(10, empty()),

  {false, 1} = lookup(1, Tree).

lookup_not_found_bigger_test() ->
  Tree = insert(1, empty()),

  {false, 1} = lookup(10, Tree).

%%--------------------------------------------------------------------

-endif.
