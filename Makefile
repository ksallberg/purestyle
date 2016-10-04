build:
	rebar3 compile

start: build
	erl -boot start_sasl -pa _build/default/lib/*/ebin \
            -pa priv -eval "application:start(musiklistan)"
clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
	rm -rf rebar.lock
.PHONY: clean
