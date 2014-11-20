FROM unbalancedparentheses/erlang:R16B02

RUN mkdir -p $HOME/app
WORKDIR $HOME/app

ONBUILD COPY . $HOME/logplex

ONBUILD RUN ./rebar -C public.rebar.config get-deps compile

EXPOSE 8001
