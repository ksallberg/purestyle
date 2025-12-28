# purestyle

personal web app for keeping track of play lists.

These play lists can contain tracks from different
sources (youtube, soundcloud.)

A running demo of this app is available [here](https://www.purestyle.se).

## Compile with:

* [Erlang/OTP](http://www.erlang.org) is required.

```
make build
```

## Start with:

```
make start
```

Inspect the development server (HTTP, not HTTPs)
[locally](http://localhost:8000).

### init.d script:

* put in /etc/init.d/
* sudo update-rc.d purestyle defaults
