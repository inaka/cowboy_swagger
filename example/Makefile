PROJECT = example

DEPS = cowboy_swagger mixer

dep_mixer          = git https://github.com/inaka/mixer.git      0.1.3
dep_cowboy_swagger = git https://github.com/inaka/cowboy-swagger 3a376def6779a85ee8e5634ec64a5072d523e59b

include erlang.mk

ERLC_OPTS += +'{parse_transform}'

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config config/sys.config
