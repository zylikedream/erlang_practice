{application, chatserver,
    [
        {description, "chat server"},
        {vsn, "1.0"},
        {modules, []},
        {registered, []},
        {applications, [kernel, stdlib]},
        {mod, {chatserver_app, []}},
        {start_phases, []}
    ]
}.