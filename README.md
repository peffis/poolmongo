# poolmongo
A thin wrapper that fuses [mongodb-erlang](https://github.com/comtihon/mongodb-erlang) with [poolboy](https://github.com/devinus/poolboy) (mostly for my own sake)

## usage
* Add poolmongo as dependency to your [erlang.mk](https://erlang.mk/) project 
```
DEPS = ... poolmongo ...
dep_poolmongo = git https://github.com/peffis/poolmongo master
```

* Add pool configuration to your sys.config
```
[
...
{poolmongo, [ 
               {pools, [
                        {pmongo, [
                                  {size, 10},
                                  {max_overflow, 20}
                                 ], [
                                     {host, "localhost"},
                                     {port, 27017},
                                     {database, <<"your mongodb database">>},
                                     {w_mode, safe},
                                     {r_mode, slave_ok}
                                    ]}
                       ]}
             ]}, 
...
].
```

* If you make use of .app.src you need to add poolmongo to the list of applications
```
{application, your_app,
 [
  {description, ""},
  {vsn, "0.9"},
  {registered, []},
  {applications, [
                  ...,
		              poolmongo
                 ]},
  {mod, { your_app, []}},
  {included_applications, []},
  {modules, []},
  {env, []}
 ]}.
 ```
 
 * Build your system
 ```
 make
 ```
 
 * Use the API in pmongo which is the same API as [mongodb-erlang's mc_worker_api](https://github.com/comtihon/mongodb-erlang) except that the first argument, the Connection, is removed
