%% run as: application:start(rest_server)
{application, musiklistan,
 [{description,  "Rest server"},
  {vsn,          "1.0"},
  {id,           "rest_server"},
  {modules,      [musiklistan,
                  test]
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
