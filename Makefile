PROJECT = damocles
include erlang.mk
DIALYZER_OPTS = -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs