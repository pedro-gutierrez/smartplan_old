-module(app).
-export([create/0, drop/0, get/0, incr/3, url/2, url/3]).

drop() -> db:flushall().
	
create() -> 
  db:hmset([<<"db">>, <<"0">>], #{ created => kit:now() }, fun(_) ->
        DefaultAvatar = kit:cfg(assets, avatar),
        db:hmset([<<"asset">>, DefaultAvatar ], #{ 
            id => DefaultAvatar,
            name => <<"anonymous.png">>,
            size => 14143,
            type => <<"image/png">>
          }, fun(_) -> app:get() end)
  end).
	
get() -> db:hgetall([<<"db">>, <<"0">>]).

incr( C, A, N ) -> db:hincrby([<<"db">>, <<"0">>], C, A, N).
	
url(App, Ev, Arg)->
  R = maps:get( location, App),
  kit:fmt( "~s#e=~s&a=~s", [R, Ev, Arg]).

url(App, Ev)-> 
  R = maps:get( location, App),
  kit:fmt( "~s#e=~s", [R, Ev]).
