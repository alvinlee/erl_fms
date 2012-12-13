{application, ria,
 [{description,  "ria"},
  {id,           "ria"},
  {vsn,          "1.0.0"},
  {modules,      [ tcp_acceptor,
                   ria_app,
                   ria_sup,
                   ria_proxy
                  ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  %% {env,          Env},
  {mod,          {ria_app,[]}}
  %% {start_phases, Phases}
 ]}.
