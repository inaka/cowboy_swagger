PROJECT = cowboy_swagger

DEPS = trails

dep_trails = git https://github.com/inaka/cowboy-trails.git 23d84c2b1f30965d7fb9272d14a4355088433fe9

SHELL_DEPS = sync

dep_sync =  git https://github.com/inaka/sync.git 0.1.3

TEST_DEPS = xref_runner

dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2

PLT_APPS := cowboy
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions

include erlang.mk

SHELL_OPTS = -s sync

# Commont Test Config
CT_DEPS = xref_runner

CT_OPTS = -cover test/swagger.coverspec -erl_args
