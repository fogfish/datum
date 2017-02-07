
%%
%% empty data structure
-ifndef (NONE).
-define(NONE,  {}).  
-endif.

%% @deprecated (4.x) 
-ifndef (NULL).
-define(NULL,  {}).  
-endif.

%%
%% xor category pattern match
-define(XOR_R(X), {ok,    X}).
-define(XOR_L(X), {error, X}).
