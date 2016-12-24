
%%
%% empty data structure
-ifndef (NULL).
-define(NULL,  {}).  
-endif.

%%
%% xor category pattern match
-define(XOR_R(X), {ok,    X}).
-define(XOR_L(X), {error, X}).
