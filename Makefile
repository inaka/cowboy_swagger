PROJECT = cowboy_swagger

CONFIG ?= test/test.config

DEPS = katana jiffy trails
SHELL_DEPS = sync
TEST_DEPS = xref_runner mixer shotgun

dep_sync =  git https://github.com/inaka/sync.git 0.1.3
dep_jiffy  = git https://github.com/davisp/jiffy.git        0.14.3
dep_trails = git https://github.com/inaka/cowboy-trails.git 0.0.2
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2
dep_mixer       = git https://github.com/inaka/mixer.git       0.1.4
dep_shotgun     = git https://github.com/inaka/shotgun.git     0.1.12
dep_katana      = git https://github.com/inaka/erlang-katana.git 07efe94

DIALYZER_DIRS := ebin/ test/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

include erlang.mk

LOCAL_DEPS := tools compiler syntax_tools common_test inets test_server dialyzer wx
TEST_ERLC_OPTS += +debug_info
CT_OPTS = -cover test/cowboy_swagger.coverspec -erl_args -config ${CONFIG}

SHELL_OPTS = -s sync

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze
