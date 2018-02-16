OS = ${shell uname -s}

include arch/${OS}/Makefile

build:
	gcc ${ARCHFLAGS} -I/usr/lib/erlang/usr/include -I${ERL_TOP}/usr/include -I${ERL_TOP}/lib/erlang/usr/include -o c_src/complex6_nif.so -fpic -shared c_src/complex.c c_src/complex6_nif.c
	rebar3 compile

# -s calls proxy:start()
# see http://erlang.org/doc/man/erl.html
start: build
	erl -boot start_sasl -pa _build/default/lib/*/ebin \
            -s proxy

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
	rm -rf rebar.lock
	rm -f c_src/complex6_nif.so
.PHONY: clean
