.PHONY: base lib engine rest release

quick: clean compile run

compile: base lib engine rest

run:
	@export SMARTPLAN_ENV=prod; erl +pc unicode -smp enable -sname smartplan -boot release/smartplan -config smartplan

noshell:
	@export SMARTPLAN_ENV=prod; erl -detached -noshell +pc unicode -smp enable -sname smartplan -boot release/smartplan -config smartplan


clean:
	@rm -rf *.dump
	@rm -rf *.log

start: stop clean
	@sudo service smartplan start

stop:
	@sudo service smartplan stop

status:
	@sudo service smartplan status

update:
	@git pull
    
release:
	@rm -rf release/*.boot
	@rm -rf release/*.script
	@cd release; erl -noshell -pa ../apps/*/ebin -pa ../deps/*/ebin -eval 'systools:make_script("smartplan", [local]),init:stop().'; cd ..


base:
	@rm -rf apps/base/ebin/*.beam
	@lfec -o apps/base/ebin apps/base/src/*.lfe

lib:
	@rm -rf apps/lib/ebin/*.beam
	@erlc -o apps/lib/ebin apps/lib/src/*.erl
	@lfec -o apps/lib/ebin apps/lib/src/*.lfe

engine:
	@rm -rf apps/engine/ebin/*.beam
	@lfec -o apps/engine/ebin apps/engine/src/*.lfe

rest:
	@rm -rf apps/rest/ebin/*.beam
	@lfec -o apps/rest/ebin apps/rest/src/*.lfe

docker:
	@docker build -t pedrogutierrez/smartplan:latest .


make_dep=@rm -rf _deps; mkdir -p _deps; cd _deps; git clone $2 $1; cd $1; git checkout $3; make; cd ../..; rm -rf deps/$1; mkdir -p deps/$1/ebin; cp -rf _deps/$1/ebin/* deps/$1/ebin; rm -rf _deps

rebar_dep=@rm -rf _deps; mkdir -p _deps; cd _deps; git clone $2 $1; cd $1; pwd; git checkout $3; rebar get-deps; rebar compile; cd ../..; rm -rf deps/$1; mkdir -p deps/$1/ebin; cp -rf _deps/$1/ebin/* deps/$1/ebin; rm -rf _deps

rebar_c_dep=@rm -rf _deps; mkdir -p _deps; cd _deps; git clone $2 $1; cd $1; git checkout $3; rebar get-deps; rebar compile; cd ../..; rm -rf deps/$1; mkdir -p deps/$1/ebin; cp -rf _deps/$1/ebin/* deps/$1/ebin; mkdir -p deps/$1/priv; cp -rf _deps/$1/priv/* deps/$1/priv; rm -rf _deps


deps: dep_eredis dep_poolboy dep_jiffy dep_cowlib dep_ranch dep_cowboy dep_uuid dep_pbkdf2 dep_erlbus

dep_eredis:
	$(call rebar_c_dep,"eredis","https://github.com/wooga/eredis.git","v1.0.8")

dep_poolboy:
	$(call rebar_dep,"poolboy","https://github.com/devinus/poolboy.git", "1.5.1")

dep_jiffy:
	$(call rebar_c_dep,"jiffy","https://github.com/davisp/jiffy.git","0.14.7")

dep_cowlib:
	$(call make_dep,"cowlib","https://github.com/ninenines/cowlib.git","1.3.0")
	
dep_ranch:
	$(call make_dep,"ranch","https://github.com/ninenines/ranch.git","1.2.1")
	
dep_cowboy:
	$(call rebar_dep,"cowboy","https://github.com/ninenines/cowboy.git","1.0.4")
	
dep_sockjs:
	$(call rebar_dep,"sockjs","https://github.com/ably-forks/sockjs-erlang","master")

dep_uuid:
	$(call make_dep,"uuid","https://github.com/avtobiff/erlang-uuid.git", "v0.4.7")
	
dep_pbkdf2:
	$(call rebar_dep,"pbkdf2","https://github.com/basho/erlang-pbkdf2.git","2.0.0")

dep_erlbus:
	$(call rebar_dep,"erlbus","https://github.com/cabol/erlbus.git", "master")
