-module(groupy).
-export([start/2, startleader/2, startb/3, startc/3, startd/3, starte/3, stop/0, stopa/0, stopb/0]).

start(Module, Sleep) ->
	Leader = worker:start("1", Module, 1, Sleep),
	register(a, Leader),
	register(b, worker:start("2", Module, 2, Leader, Sleep)),
	register(c, worker:start("3", Module, 3, Leader, Sleep)),
	register(d, worker:start("4", Module, 4, Leader, Sleep)),
	register(e, worker:start("5", Module, 5, Leader, Sleep)).

startleader(Module, Sleep) ->
	register(a, worker:start("1", Module, 1, Sleep)).
	
startb(Module, Leader, Sleep) ->
	register(b, worker:start("2", Module, 2, Leader, Sleep)).
	
startc(Module, Leader, Sleep) ->
	register(c, worker:start("3", Module, 3, Leader, Sleep)).

startd(Module, Leader, Sleep) ->
	register(d, worker:start("4", Module, 4, Leader, Sleep)).

starte(Module, Leader, Sleep) ->
	register(e, worker:start("5", Module, 5, Leader, Sleep)).


stop() ->
	a ! stop,
	b ! stop,
	c ! stop,
	d ! stop,
	e ! stop.

stopa() ->
	a ! stop.
	
stopb() ->
	b ! stop.
	
