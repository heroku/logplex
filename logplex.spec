{alias, logplex, "./test/"}.
{logdir, "./logs/"}.
{include, "./include/"}.

{suites, logplex, all}.
{skip_suites, logplex, [logplex_tcp_drain_bench_SUITE],
 "Needs two TCPsyslog drain versions, available at "
 "commit 1891262e02006e0f05b39e9c73a1b5a8218f4421 or a bit earlier. "
 "Eventually this suite can be dropped and this notice removed."}.
