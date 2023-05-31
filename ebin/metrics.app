{application, 'metrics', [
	{description, "Metrics"},
	{vsn, "0.1.0"},
	{modules, ['metrics','metrics_app','metrics_config','metrics_exposition_h','metrics_observations','metrics_observations_sup','metrics_prom','metrics_statem','metrics_sup','metrics_util']},
	{registered, [metrics_sup]},
	{applications, [kernel,stdlib,cowboy,envy,recon]},
	{optional_applications, []},
	{mod, {metrics_app, []}},
	{env, []}
]}.