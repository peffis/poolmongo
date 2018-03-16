%% @doc The poolmongo db api
%% This is a thin wrapper on top of the Erlang mongodb
%% that shows how one can use it together with poolboy
%% @end
%% @author Stefan Hellkvist.



-module(pmongo).
-export([
	 insert/2,
	 insert/3,
	 update/3,
	 update/5,
	 update/6,
	 delete/2,
	 delete_one/2,
	 delete_limit/3,
	 find_one/2,
	 find_one/3,
	 find/2,
	 find/3,
	 count/2,
	 count/3,
         command/1
	]).

-define(POOL_NAME, pmongo).


%% @doc insert a Doc into a collection.
insert(Collection, Doc) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:insert(Worker, Collection, Doc)
			end).

%% @doc insert a Doc into a collection.
insert(Collection, Doc, WC) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:insert(Worker,
						     Collection,
						     Doc,
						     WC)
			end).



%% @doc Replace the document matching criteria entirely with the new Document.
update(Coll, Selector, Doc) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:update(Worker,
						     Coll,
						     Selector,
						     Doc)
			end).


%% @doc Replace the document matching criteria entirely with the new Document.
update(Coll, Selector, Doc, Upsert, MultiUpdate) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:update(Worker,
						     Coll,
						     Selector,
						     Doc,
						     Upsert,
						     MultiUpdate)
			end).


%% @doc Replace the document matching criteria entirely with the new Document.
update(Coll, Selector, Doc, Upsert, MultiUpdate, WC) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:update(Worker,
						     Coll,
						     Selector,
						     Doc,
						     Upsert,
						     MultiUpdate,
						     WC)
			end).


% @doc deletes document from Collection matching Selector.
delete(Coll, Selector) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:delete(Worker,
						     Coll,
						     Selector)
			end).

% @doc deletes first selected document.
delete_one(Coll, Selector) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:delete_one(Worker,
							 Coll,
							 Selector)
			end).


% @doc deletes first N selected document.
delete_limit(Coll, Selector, N) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:delete_limit(Worker,
							   Coll,
							   Selector,
							   N)
			end).


%% @doc returns first selected document, if any
find_one(Coll, Selector) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:find_one(Worker,
						       Coll,
						       Selector)
			end).


%% @doc returns first selected document, if any
find_one(Coll, Selector, Args) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:find_one(Worker,
						       Coll,
						       Selector,
						       Args)
			end).


%% @doc Return selected documents.
find(Coll, Selector) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:find(Worker,
						   Coll,
						   Selector)
			end).


%% @doc Return selected documents.
find(Coll, Selector, Args) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:find(Worker,
						   Coll,
						   Selector,
						   Args)
			end).


%% @doc Count selected documents.
count(Coll, Selector) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:count(Worker,
						    Coll,
						    Selector)
			end).


%% @doc Count selected documents up to given max number, 0 means no max
count(Coll, Selector, Args) ->
    poolboy:transaction(?POOL_NAME,
			fun(Worker) ->
				mc_worker_api:count(Worker,
						    Coll,
						    Selector,
						    Args)
			end).


%% @doc Execute a generic command on the mongodb server
command(Command) ->
    poolboy:transaction(?POOL_NAME,
                        fun(Worker) ->
                                mc_worker_api:command(Worker, Command)
                        end).
