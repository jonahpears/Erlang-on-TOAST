{application, tpri_app,
 [{description, "Timed Protocol re-engineering application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {tpri_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[{dev_testing, true}]},
  {modules, []},

  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
