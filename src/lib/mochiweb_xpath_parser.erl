%% @author Pablo Polvorin 
%% @doc Compile XPath expressions.
%% This module uses the xpath parser of xmerl.. that interface isn't documented
%% so could change between OTP versions.. its know to work with OTP R12B2 
%% created on 2008-05-07
-module(mochiweb_xpath_parser).

-export([compile_xpath/1]).

%% Return a compiled XPath expression
compile_xpath(XPathString) ->
    {ok,XPath} = xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens(XPathString)),
    simplify(XPath).



%% @doc Utility functions to convert between the *internal* representation of
%       xpath expressions used in xmerl(using lists and atoms), to a
%       representation using only binaries, to match the way in
%       which the mochiweb html parser represents data 
simplify({path, union, {Path1, Path2}})->
    %% "expr1 | expr2 | expr3"
    {path, union, {simplify(Path1), simplify(Path2)}};
simplify({path,Type,Path}) ->
    {path,Type,simplify_path(Path)};
simplify({comp,Comp,A,B}) ->
    {comp,Comp,simplify(A),simplify(B)};
simplify({literal,L}) ->
    {literal,list_to_binary(L)};
simplify({number,N}) ->
    {number,N};
simplify({negative, Smth}) ->
    {negative, simplify(Smth)};
simplify({bool, Comp, A, B}) ->
    {bool, Comp, simplify(A), simplify(B)};
simplify({function_call,Fun,Args}) ->
    {function_call,Fun,lists:map(fun simplify/1,Args)};
simplify({arith, Op, Arg1, Arg2}) ->
	{arith, Op, simplify(Arg1), simplify(Arg2)}.


simplify_path({step,{Axis,NodeTest,Predicates}}) ->
    {step,{Axis,
            simplify_node_test(NodeTest),
            simplify_predicates(Predicates)}};
simplify_path({refine,Path1,Path2}) ->
    {refine,simplify_path(Path1),simplify_path(Path2)}.


simplify_node_test({name,{Tag,Prefix,Local}}) ->
    {name,{to_binary(Tag),Prefix,Local}};

simplify_node_test(A={node_type, _Type}) ->
    A;
simplify_node_test({processing_instruction, Name}) ->
    {processing_instruction, list_to_binary(Name)};  % strictly, this must be node_type too!
simplify_node_test(A={wildcard,wildcard}) ->
    A;
simplify_node_test({prefix_test, Prefix}) ->
    %% [37] /prefix:*/ - namespace test
    {prefix_test, list_to_binary(Prefix)}.


simplify_predicates(X) -> lists:map(fun simplify_predicate/1,X).
simplify_predicate({pred,Pred}) ->
    {pred,simplify(Pred)}.

to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X)).




