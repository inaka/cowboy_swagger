PROJECT = cowboy_swagger

CONFIG ?= test/test.config

DEPS = jiffy trails

dep_jiffy  = git https://github.com/davisp/jiffy.git        0.14.3
dep_trails = git https://github.com/inaka/cowboy-trails.git 0.0.2

SHELL_DEPS = sync

dep_sync =  git https://github.com/inaka/sync.git 0.1.3

TEST_DEPS = xref_runner mixer shotgun

dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2
dep_mixer       = git https://github.com/inaka/mixer.git       0.1.4
dep_shotgun     = git https://github.com/inaka/shotgun.git     0.1.12

PLT_APPS := trails cowboy
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions

include erlang.mk

# Commont Test Config
CT_DEPS = xref_runner
TEST_ERLC_OPTS += +debug_info
CT_SUITES = cowboy_swagger cowboy_swagger_handler
CT_OPTS = -cover test/cowboy_swagger.coverspec -erl_args -config ${CONFIG}

SHELL_OPTS = -s sync
