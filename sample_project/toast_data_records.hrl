
%% if mon=role_fsm then module used is "role_fsm_" ++ name (same as ID)
%% if mon=role_gen then only ID is "role_gen_" ++ name
-record(role_modules, { sup = role_sup,
                        mon = role_fsm, %% or role_tmp
                        imp = role_imp }).

-record(role_spec, { name,
                     modules = #role_modules{},
                     params = [] }).


-record(child_options, { restart = transient,
                         shutdown = 2000,
                         type = worker,
                         significant = false }).
