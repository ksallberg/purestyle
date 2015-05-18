{application, musiklistan,
 [{description,  "Music playlist YAWS app"},
  {vsn,          "1.0"},
  {id,           "musiklistan"},
  {modules,      [musiklistan,
                  test,
                  mldb
                 ]
  },
  {registered,   []},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {musiklistan, []}},
  {env, []}
 ]
}.
