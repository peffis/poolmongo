PROJECT = poolmongo
PROJECT_DESCRIPTION = Just a fuse of mongodb-erlang driver and poolboy
PROJECT_VERSION = 0.0.1

DEPS = poolboy mongodb
dep_poolboy = git https://github.com/devinus/poolboy master
dep_mongodb = git https://github.com/comtihon/mongodb-erlang 0dc587cf67e52bfd30b4f9e4ab08a9ff259bc5c5


include erlang.mk
