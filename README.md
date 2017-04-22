#purestyle personal homepage

web app for keeping track of play lists.
These play lists can contain tracks from different
sources (youtube, spotify, etc.)

A running demo of this app is available [here](https://www.purestyle.se).

##Compile with:
LFE [lisp flavored erlang](http://lfe.io) is required.

```
make build
```

##Start with:
```
make start
```
Inspect the development server (HTTP, not HTTPs)
[locally](http://localhost:8000).

###init.d script:
* put in /etc/init.d/
* sudo update-rc.d purestyle defaults
