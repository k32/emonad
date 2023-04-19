%%--------------------------------------------------------------------
%% Copyright (c) 2023 k32. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc Maybe monad (for test purposes)
-module(emonad_maybe).

-behavior(emonad).

%% behavior callbacks:
-export([bind/2, return/1, nomatch/1]).

%%================================================================================
%% behavior callbacks
%%================================================================================

return(A) ->
  {just, A}.

bind(nothing, _Fun) ->
  nothing;
bind({just, A}, Fun) ->
  Fun(A).

nomatch(A) ->
  nothing.
