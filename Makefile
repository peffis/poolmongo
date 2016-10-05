PROJECT = poolmongo
PROJECT_DESCRIPTION = Just a fuse of mongodb-erlang driver and poolboy
PROJECT_VERSION = 0.0.1

DEPS = poolboy mongodb
dep_poolboy = git https://github.com/devinus/poolboy master
dep_mongodb = git https://github.com/comtihon/mongodb-erlang master


include erlang.mk
