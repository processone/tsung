%% xpath_functions.erl
%% @author Pablo Polvorin 
%% @doc Some core xpath functions that can be used in xpath expressions
%% created on 2008-05-07
-module(mochiweb_xpath_functions).

-export([lookup_function/1]).


%% Default functions.
%% The format is: {FunctionName, fun(), FunctionSignature}
%% WildCard argspec must be the last spec in list.
%%
%% @type FunctionName = atom()
%% @type FunctionSignature = [XPathArgSpec]
%% @type XPathArgSpec = XPathType | WildCardArgSpec
%% @type WildCardArgSpec = {'*', XPathType}
%% @type XPathType = node_set | string | number | boolean
%% 
%% The engine is responsable of calling the function with
%% the correct arguments, given the function signature. 
-spec lookup_function(atom()) -> mochiweb_xpath:xpath_fun_spec() | false.
lookup_function('last') ->
    {'last',fun last/2,[]};
lookup_function('position') ->
    {'position',fun position/2,[]};
lookup_function('count') ->
    {'count',fun count/2,[node_set]};
lookup_function('concat') ->
    {'concat',fun concat/2,[{'*', string}]};
lookup_function('name') ->
    {'name',fun 'name'/2,[node_set]};
lookup_function('starts-with') ->
    {'starts-with', fun 'starts-with'/2,[string,string]};
lookup_function('contains') ->
    {'contains', fun 'contains'/2,[string,string]};
lookup_function('substring') ->
    {'substring', fun substring/2,[string,number,number]};
lookup_function('sum') ->
    {'sum', fun sum/2,[node_set]};
lookup_function('string-length') ->
    {'string-length', fun 'string-length'/2,[string]};
lookup_function('not') ->
    {'not', fun x_not/2, [boolean]};
lookup_function('string') ->
    {'string', fun 'string'/2, [node_set]};
lookup_function(_) ->
    false.

%% @doc Function: boolean last() 
%%      The position function returns the position of the current node
last({ctx, _, _, _, Position, Size} = _Ctx, []) ->
    Position =:= Size.

%% @doc Function: number position() 
%%      The position function returns the position of the current node
position({ctx, _, _, _, Position, _} = _Ctx, []) ->
    Position.

%% @doc Function: number count(node-set) 
%%      The count function returns the number of nodes in the 
%%      argument node-set.
count(_Ctx,[NodeList]) ->
    length(NodeList).

%% @doc Function: concat(binary, binary, ...)
%%      Concatenate string arguments (variable length)
concat(_Ctx, BinariesList) ->
    %% list_to_binary()
    << <<Str/binary>> || Str <- BinariesList>>.

%% @doc Function: string name(node-set?)
'name'(_Ctx,[[{Tag,_,_,_}|_]]) ->
    Tag.

%% @doc Function: boolean starts-with(string, string) 
%%      The starts-with function returns true if the first argument string
%%      starts with the second argument string, and otherwise returns false.
'starts-with'(_Ctx,[Left,Right]) ->
    Size = size(Right),
    case Left of
        <<Right:Size/binary,_/binary>> -> true;
        _ -> false
    end.

%% @doc Function: checks that Where contains What
contains(_Ctx,[Where, What]) ->
    case binary:match(Where, [What]) of
        nomatch ->
            false;
        {_, _} ->
            true
    end.

%% @doc Function: string substring(string, number, number?) 
%%      The substring function returns the substring of the first argument 
%%      starting at the position specified in the second argument with length
%%      specified in the third argument
substring(_Ctx,[String,Start,Length]) when is_binary(String)->
    Before = Start -1,
    After = size(String) - Before - Length,
    case (Start + Length) =< size(String) of
        true ->
            <<_:Before/binary,R:Length/binary,_:After/binary>> = String,
            R;
        false ->
            <<>>
    end.

%% @doc Function: number sum(node-set) 
%%      The sum function returns the sum, for each node in the argument 
%%      node-set, of the result of converting the string-values of the node
%%      to a number.
sum(_Ctx,[Values]) ->
    lists:sum([mochiweb_xpath_utils:number_value(V) || V <- Values]).

%% @doc Function: number string-length(string?) 
%%      The string-length returns the number of characters in the string 
%%      TODO: this isn't true: currently it returns the number of bytes
%%            in the string, that isn't the same 
'string-length'(_Ctx,[String]) ->
    size(String).

%%  @doc Function: string string(node_set)
%%
%%       The sum function returns the string representation of the
%%       nodes in a node-set. This is different from text() in that
%%       it concatenates each bit of the text in the node along with the text in
%%       any children nodes along the way, in order.
%%       Note: this differs from normal xpath in that it returns a list of strings, one
%%       for each node in the node set, as opposed to just the first node.
'string'(_Ctx, [NodeList]) ->
    lists:map(fun({_Elem, _Attr, Children,_Pos}) -> concat_child_text(Children, []) end, NodeList).

concat_child_text([], Result) ->
    list_to_binary(lists:reverse(Result));
concat_child_text([{_,_,Children,_} | Rest], Result) ->
    concat_child_text(Rest, [concat_child_text(Children, []) | Result]);
concat_child_text([X | Rest], Result) ->
    concat_child_text(Rest, [X | Result]).

x_not(_Ctx, [Bool]) ->
    not Bool.
