

%% @deprecated (4.x) 
-ifndef (NULL).
-define(NULL,  {}).  
-endif.

%%
%% 
-define(NONE,  undefined).

%%
%% either category pattern match
-define(EITHER_R(X), {ok,    X}).
-define(EITHER_L(X), {error, X}).

-define(XOR_R(X), {ok,    X}).
-define(XOR_L(X), {error, X}).
