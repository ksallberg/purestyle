build:
	rebar3 compile

start: build
	erl -name musiklistan -boot start_sasl -detached \
            -pa _build/default/lib/*/ebin \
            -pa priv -eval "application:start(musiklistan)"
clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
.PHONY: clean
