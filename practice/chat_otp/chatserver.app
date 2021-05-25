{application, chatserver,
    [
        {description, "chat server"},
        {vsn, "1.0"},
        {modules, [chatserver_app, db, proto, common, server_sup, server_listener, service_chat, service_friend, socket_sup, socket_handler]},
        {registered, [server_sup, server_listener, service_chat, service_friend, socket_sup, socket_handler]},
        {applications, [kernel, stdlib]},
        {mod, {chatserver_app, []}},
        {start_phases, []}
    ]
}.