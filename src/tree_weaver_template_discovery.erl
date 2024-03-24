-module(tree_weaver_template_discovery).
-export([discover_templates/2]).

discover_templates(Language, Criteria) ->
    case check_template_cache(Language) of
        {ok, CachedTemplates} ->
            CachedTemplates;
        not_found ->
            Templates = fetch_templates(Language, Criteria),
            RankedTemplates = rank_templates(Templates, Criteria),
            cache_templates(Language, RankedTemplates),
            RankedTemplates
    end.

check_template_cache(_) -> not_found.

fetch_templates(_, _) -> [].

rank_templates(Templates, _) -> Templates.

cache_templates(_, _) -> ok.
