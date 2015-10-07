defmodule Logplex.Mixfile do
  use Mix.Project

  def project do
    [app: :logplex,
     version: "1.0.0",
     compilers: [:erlang, :app],
     deps: deps]
  end

  def application do
    [applications: [:kernel,:logplex]]
  end

  defp deps do
    [{:redis, git: "git://github.com/JacobVorreuter/redis_pool.git", branch: "master"},
     {:mochiweb, git: "git://github.com/heroku/mochiweb.git", tag: "R16B01"},
     {:pagerduty, git: "git://github.com/JacobVorreuter/pagerduty.git", branch: "master"},
     {:redgrid, git: "git://github.com/JacobVorreuter/redgrid.git", tag: "v1.0.3"},
     {:redo, git: "git://github.com/JacobVorreuter/redo.git", ref: "7c7eaef4cd65271e2fc4ea88587e848407cf0762"},
     {:nsync, git: "git://github.com/heroku/nsync.git", branch: "OTP-17.4"},
     {:cowboy, git: "git://github.com/ninenines/cowboy.git", tag: "1.0.1"},
     {:quoted, git: "git://github.com/pivotree/quoted.erl.git", branch: "master"},
     {:gproc, git: "git://github.com/uwiger/gproc.git", branch: "master"},
     {:ex_uri, git: "git://github.com/heroku/ex_uri.git", branch: "master"},
     {:heroku_crashdumps, git: "git://github.com/heroku/heroku_crashdumps.git", tag: "0.1.0"},
     {:lager, git: "git://github.com/basho/lager.git", tag: "3.0.1"},
     {:batchio, git: "git://github.com/ferd/batchio.git", tag: "1.0.0"},
     {:recon, git: "git://github.com/ferd/recon.git", branch: "master"},
     {:folsom, git: "https://github.com/boundary/folsom.git", tag: "0.8.2"},
     {:jsx, git: "https://github.com/talentdeficit/jsx.git", branch: "master"},
     {:folsom_cowboy, git: "https://github.com/evanmcc/folsom_cowboy.git", ref: "26f85d4b5658a264f328b15cce506ef7f2b484a1"},
    ]
  end
end
