-module(ts_test_mochi).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

xpath_parse_test() ->
    Data="\r\n\r\n<html><body><a href='/index.html?name=A&value=B&C'></a></body></html>",
    XPath = "//a/@href",
    Tree = mochiweb_html:parse(list_to_binary(Data)),
    ?assertEqual({<<"html">>,[],
		  [{<<"body">>,[],
		    [{<<"a">>, 
		      [{<<"href">>, <<"/index.html?name=A&value=B&C">>}], []
		     }]
		    }]
		  }, 
		 Tree).
