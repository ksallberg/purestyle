build:
	rebar3 compile

# -s calls proxy:start()
# see http://erlang.org/doc/man/erl.html
start:
	erl -boot start_sasl -pa _build/default/lib/*/ebin \
            -s proxy

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
	rm -rf rebar.lock
.PHONY: clean
