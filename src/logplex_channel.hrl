-record(channel, {id :: logplex_channel:id(),
                  name = <<"">> :: logplex_channel:name(),
                  flags = [] :: logplex_channel:flags(),
                  tokens = [] :: [logplex_token:token()]
                 }).
