-module(category_parse_transform_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
   all/0,
   syntax_composition/1,
   syntax_composition_with_state/1,
   syntax_composition_with_transformer/1,
   syntax_composition_partial/1,
   syntax_nested_list_comprehension/1,
   syntax_side_effect_operator/1
]).

all() ->
   [
      syntax_composition,
      syntax_composition_with_state,
      syntax_composition_with_transformer,
      syntax_composition_partial,
      syntax_nested_list_comprehension,
      syntax_side_effect_operator
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
