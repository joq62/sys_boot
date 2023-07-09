all:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf *.crashdump;
	rm -rf _build;
	rm -rf logs;
	rm -rf ebin
	rm -rf rebar.lock;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	rm -rf ebin;
	git add  *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
build:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build;
	rm -rf ebin
	rm -rf rebar.lock;
	mkdir ebin;
	erlc -I include -I /home/joq62/erlang/include -o ebin ../etcd/src/*.erl;
	erlc -I include -I /home/joq62/erlang/include -o ebin ../control/src/*.erl; 
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin;
clean:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf *.crashdump;
	rm -rf _build;
	rm -rf ebin
	rm -rf logs;
	rm -rf rebar.lock

eunit:
#	Standard
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build;
	rm -rf ebin
	rm -rf logs;
	rm -rf rebar.lock
#	Application speciic
#	test
	mkdir test_ebin;
	cp test/*.app test_ebin;
	erlc -I include -I /home/joq62/erlang/include -o test_ebin test/*.erl;
#  	dependencies
#	Applications
	mkdir ebin;
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
#	Application specific
	erl -pa ebin -pa test_ebin\
	    -pa ../log/ebin\
	    -pa ../etcd/ebin\
	    -pa ../control/ebin\
	    -sname do_test\
	    -run $(m) start\
	    -setcookie test_cookie
#	    -hidden
