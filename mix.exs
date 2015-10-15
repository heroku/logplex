defmodule Logplex.Mixfile do
  use Mix.Project

  def project do
   [app: :logplex,
    deps_path: "../../deps",
    lockfile: "../../mix.lock",
    version: "1.0.0",
    compilers: [:erlang, :elixir, :app],
    erlc_paths: erlc_paths(Mix.env),
    deps: deps(Mix.env),
   ]
  end

  def erlc_paths(:test) do
    erlc_paths(:_) ++ ["ctest"]
  end

  def erlc_paths(_env) do
    ["src"]
  end


  def application do
    [applications: [:kernel ,:stdlib ,:sasl ,:inets ,:crypto ,:public_key ,:ssl ,:redis ,:mochiweb ,:pagerduty ,:redo ,:nsync ,:cowboy ,:quoted ,:gproc ,:ex_uri ,:ranch ,:lager ,:batchio ,:folsom ,:folsom_cowboy],
     included_applications: [:heroku_crashdumps, :redgrid, :backoff],
     mod: {:logplex_app, []},
     start_phases: [listen: []],
     env: env
    ]
  end

  defp env do
    [
    availability_zone: "not-aws",
    cloud_name: 'development',
    config_redis_url: "redis://localhost:6379/",
    default_redis_poll_ms: 2000, # #ms
    drain_buffer_size: 1024, # #messages
    ets_token_reindex_step_size: 10000, # num tokens per tab read, index write.
    http_body_checksum: :none, # none | md5
    http_drain_buffer_size: 1024, # messages
    http_drain_target_bytes: 102400, # bytes
    http_frame_retries: 1, # #extra attempts after first
    http_log_input_port: 8601, # syslog/http tcp listen port
    http_port: 8001,
    http_reconnect_time: 10, # ms
    http_send_loss_msg: :send, # send | dont_send
    log_history: 1500,
    log_unknown_tokens: false, # bool
    logplex_shard_urls: "redis://localhost:6379/",
    max_drains_per_channel: 5, # #channels
    metrics_namespace: "dev", # string, no spaces
    no_tail_warning: "Error L20 (Tail sessions forbidden) Tail sessions for this app are forbidden due to log volume.", # string
    no_redis_warning: "Warning: log history discarded due to excessive volume.", # string
    pagerduty: "0", # "0" = disabled
    queue_length: 2000,
    redgrid_redis_url: "redis://localhost:6379/",
    redis_buffer_expiry: <<"604800">>, # s -- one week
    redis_buffer_length: 2000,
    redis_writers: 10,
    session_expiry_s: 360, # #seconds
    session_lookup_timeout_s: 5, # #seconds
    syslog_port: 6001, # syslog tcp listen port
    tcp_drain_buffer_size: 1024, # #messages
    tcp_drain_target_bytes: 4096, # bytes
    tcp_syslog_backoff_max: 300, # seconds
    tcp_syslog_reconnect_min: 30, # seconds
    tcp_syslog_send_loss_msg: :send, # send | dont_send
    tcp_syslog_send_timeout_secs: 4, # seconds
    tcp_syslog_idle_timeout: 300000, # ms
    tcp_syslog_idle_fuzz: 15000, # ms
    tcp_syslog_max_ttl: 18000000, # ms
    tcp_syslog_shrink_after: 10, # retry attempts
    http_drain_idle_timeout: 300000, # ms
    http_drain_idle_fuzz: 15000, # ms
    http_drain_max_ttl: 18000000, # ms
    workers: 10,
    ]
  end

  defp deps(_) do
    [{:exrm, github: "bitwalker/exrm", tag: "0.19.6"},
     {:redis, git: "git://github.com/JacobVorreuter/redis_pool.git", branch: "master"},
     {:mochiweb, git: "git://github.com/heroku/mochiweb.git", tag: "R16B01"},
     {:pagerduty, git: "git://github.com/JacobVorreuter/pagerduty.git", branch: "master"},
     {:redgrid, git: "git://github.com/JacobVorreuter/redgrid.git", tag: "v1.0.3"},
     {:redo, git: "git://github.com/JacobVorreuter/redo.git", ref: "7c7eaef4cd65271e2fc4ea88587e848407cf0762", override: true},
     {:nsync, git: "git://github.com/heroku/nsync.git", branch: "OTP-17.4"},
     {:cowboy, git: "git://github.com/ninenines/cowboy.git", tag: "1.0.1"},
     {:quoted, git: "git://github.com/pivotree/quoted.erl.git", branch: "master"},
     {:gproc, git: "git://github.com/uwiger/gproc.git", branch: "master"},
     {:ex_uri, git: "git://github.com/heroku/ex_uri.git", branch: "master"},
     {:heroku_crashdumps, git: "git://github.com/heroku/heroku_crashdumps.git", tag: "0.1.0"},
     {:lager, git: "git://github.com/basho/lager.git", tag: "3.0.1"},
     {:batchio, git: "git://github.com/ferd/batchio.git", tag: "1.0.0"},
     {:recon, git: "git://github.com/ferd/recon.git", branch: "master"},
     {:folsom, git: "https://github.com/boundary/folsom.git", tag: "0.8.2", override: true},
     {:jsx, git: "https://github.com/talentdeficit/jsx.git", tag: "v1.3.1", override: true},
     {:folsom_cowboy, git: "https://github.com/evanmcc/folsom_cowboy.git", ref: "26f85d4b5658a264f328b15cce506ef7f2b484a1"},
     {:meck, "~> 0.8.2", override: true},
    ]
  end

end
