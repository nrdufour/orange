%% Copyright 2009-2016 Nicolas R Dufour.
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
%%
%% @author Nicolas R Dufour <nrdufour@gmail.com>
%% @copyright 2009-2016 Nicolas R Dufour.

-type adtType()  :: class_t | attribute_t | link_t | object_t.
-type adtNames() :: [string()].
-type adtState() :: alive | frozen | destroyed | none.

-record(meta, {
		type  = unknown :: adtType(),
		names = []      :: adtNames(),
		state = none    :: adtState()
	}).

-record(adt, {
		meta,
		data
	}).
