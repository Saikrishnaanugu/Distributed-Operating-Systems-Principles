{application, 'twitter_server', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['twitter_server_app','twitter_server_sup']},
	{registered, [twitter_server_sup]},
	{applications, [kernel,stdlib]},
	{mod, {twitter_server_app, []}},
	{env, []}
]}.