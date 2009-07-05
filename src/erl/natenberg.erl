% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(natenberg).
-behaviour(gen_server).
% callbacks
-export([start/0, stop/0, start_link/0, init/1, handle_call/3, 
		 handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  application:start(natenberg).

stop() ->
  application:stop(natenberg).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, self(), []).

init(_From) ->
  erlang:display(atom_to_list(?MODULE) ++ " starting ..."),
  process_flag(trap_exit,true),
  {ok, 0}.

handle_call(Function, _From, State) ->
  Json = erlang:apply(rest, Function, []),
  {reply, Json, State + 1}.

handle_cast(_Msg, State) ->
  {noreply, State}.
 
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  erlang:display(atom_to_list(?MODULE) ++ " terminating ..."),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.
