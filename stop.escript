#!/usr/bin/env escript
%%! -sname stopper@localhost
main([Name]) ->
  start_distribution(),
  Node = list_to_atom(Name),
  case net_adm:ping(Node) of
    pang ->
      io:format("Failed to stop ~s node is not running~n",[Name]);
    pong ->
      rpc:call(Node, init, stop, []),
      io:format("Node ~s stopped~n", [Name])
  end;
main(_) ->
  io:format("Usage: stop.escript <node_name>").

start_distribution() ->
  net_kernel:start([node(), shortnames]).

