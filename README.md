<h1>musiklistan</h1>
web app (LYME stack, http://en.wikipedia.org/wiki/LYME_%28software_bundle%29) for keeping track of play lists.
These play lists can contain tracks from different sources (youtube, spotify, etc.)
<br/><br/>
Compile with:<br/>
rebar compile
<br/>
Append yaws.conf with: <br/>
include_dir = /home/xxx/yyy/musiklistan/include <br/>
<br/>
ebin_dir = /home/xxx/yyy/erlang/musiklistan/ebin <br/>
ebin_dir = /home/xxx/yyy/uuid/ebin <br/>
ebin_dir = /home/xxx/yyy/uuid/deps/quickrand/ebin <br/>
runmod = musiklistan <br/>
<br/>

```
<server localhost>
    port = 8000
    listen = 0.0.0.0
    docroot = /home/xxx/yyy/musiklistan/pages
    auth_log = true
    appmods = <cgi-bin, yaws_appmod_cgi>
</server>
```
