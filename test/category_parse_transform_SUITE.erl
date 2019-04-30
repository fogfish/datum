-module(category_parse_transform_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
   all/0,
   syntax_composition/1,
   syntax_composition_with_state/1,
   syntax_composition_with_pattern/1,
   syntax_composition_with_transformer/1,
   syntax_composition_partial/1,
   syntax_nested_list_comprehension/1,
   syntax_side_effect_operator/1,
   syntax_kleisli_with_list_ops/1,
   syntax_composition_parial/1
]).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%
%%
syntax_composition(_) ->
   ok = transform("[identity || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[$. || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[option || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[$? || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[undefined || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[either || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[$^ || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[reader || cats:unit(1), do:this(_), do:that(_)]."),
   ok = transform("[m_identity || cats:unit(1), do:this(_), do:that(_)].").

syntax_composition_with_state(_) ->
   ok = transform("[identity || A =< 1, B <- do:this(A), do:that(C)]."),
   ok = transform("[option || A =< 1, B <- do:this(A), do:that(C)]."),
   ok = transform("[undefined || A =< 1, B <- do:this(A), do:that(C)]."),
   ok = transform("[either || A =< 1, B <- do:this(A), do:that(C)]."),
   ok = transform("[reader || A =< 1, B <- do:this(A), do:that(C)]."),
   ok = transform("[m_identity || A =< 1, B <- do:this(A), do:that(C)].").

syntax_composition_with_pattern(_) ->
   % ok = transform("[identity || #a{a = A} =< 1, #b{b = B} <- do:this(A), do:that(C)]."),
   ok = transform("[option || #a{a = A} =< 1, #b{b = B} <- do:this(A), do:that(C)]."),
   % ok = transform("[undefined || #a{a = A} =< 1, #b{b = B} <- do:this(A), do:that(C)]."),
   ok = transform("[either || #a{a = A} =< 1, #b{b = B} <- do:this(A), do:that(C)]."),
   % ok = transform("[reader || #a{a = A} =< 1, #b{b = B} <- do:this(A), do:that(C)]."),
   % ok = transform("[m_identity || #a{a = A} =< 1, #b{b = B} <- do:this(A), do:that(C)]."),
   ok.

syntax_composition_with_transformer(_) ->
   ok = transform("[identity || cats:unit(1), _/= x(_), _/= cats:this(_), _/= do:that(_)]."),
   ok = transform("[option || cats:unit(1), _/= x(_), _/= cats:this(_), _/= do:that(_)]."),
   ok = transform("[undefined || cats:unit(1), _/= x(_), _/= cats:this(_), _/= do:that(_)]."),
   ok = transform("[either || cats:unit(1), _/= x(_), _/= cats:this(_), _/= do:that(_)]."),
   ok = transform("[reader || cats:unit(1), _/= x(_), _/= cats:this(_), _/= do:that(_)]."),
   ok = transform("[m_identity || cats:unit(1), _/= x(_), _/= cats:this(_), _/= do:that(_)].").

syntax_composition_partial(_) ->
   ok = transform("[identity || cats:unit(_), do:this(_), do:that(_)]."),
   ok = transform("[option || cats:unit(_), do:this(_), do:that(_)]."),
   ok = transform("[undefined || cats:unit(_), do:this(_), do:that(_)]."),
   ok = transform("[either || cats:unit(_), do:this(_), do:that(_)]."),
   ok = transform("[reader || cats:unit(_), do:this(_), do:that(_)]."),
   ok = transform("[m_identity || cats:unit(_), do:this(_), do:that(_)].").

syntax_nested_list_comprehension(_) ->
   ok = transform("[identity || do:this(1), cats:sequence([X || X <- _]), do:that(_)]."),
   ok = transform("[option || do:this(1), cats:sequence([X || X <- _]), do:that(_)]."),
   ok = transform("[either || do:this(1), cats:sequence([X || X <- _]), do:that(_)].").

syntax_side_effect_operator(_) ->
   ok = transform("[m_identity || _ > put, _ < get].").

syntax_kleisli_with_list_ops(_) ->
   ok = transform("[m_identity || _ > \"a\" ++ X, _ < get].").

syntax_composition_parial(_) ->
   ok = transform("[identity || fun(X) -> X end, do:that(_)]."),
   ok = transform("[identity || X, do:that(_)].").


%%%------------------------------------------------------------------
%%%
%%% helpers
%%%
%%%------------------------------------------------------------------   

transform(Code) ->
   {ok, Parsed, _} = erl_scan:string(Code),
   {ok, Forms} = erl_parse:parse_exprs(Parsed),
   Fun  = [{function, 1, a, 1, [{clause, 1, [], [], Forms}]}],
   [{function, _, _, _, _}] = category:parse_transform(Fun, []),
   ok.
