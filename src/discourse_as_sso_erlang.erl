-module(discourse_as_sso_erlang).

%% API exports
-export([
        as_formatted/3,
        get_sso_api_values/2,
        validate_sso_return_values/3
]).

%%====================================================================
%% API functions
%%====================================================================
  
validate_sso_return_values(Sso,Sig,Key) ->  
  URIDecoded = http_uri:decode(binary_to_list(Sso)),  
  QueryString = base64:decode(URIDecoded),  
  QPs = maps:from_list(cow_qs:parse_qs(QueryString)),  
  NonceFrom = maps:get(<<"nonce">>,QPs),  
  HexString = make_HMAC_SHA256_signature(URIDecoded,Key),  
  case HexString == Sig of   
    true ->  
      {valid, NonceFrom,QPs};  
    false ->  
      invalid  
  end.  
  
get_sso_api_values(ReturnURL,Key) ->  
  Nonce = new_nonce(),  
  {URLEncodedPayload,Base64EncodedPayload} = make_url_encoded_payload(Nonce,ReturnURL),   
  HexSignature = make_HMAC_SHA256_signature(Base64EncodedPayload,Key),  
  { URLEncodedPayload,HexSignature,Nonce }.

as_formatted(YourDiscourseForum,ReturnURL,Key) ->
  { URLEncodedPayload,HexSignature,Nonce } = get_sso_api_values(ReturnURL,Key),
  ForwardString = io_lib:format("~s/session/sso_provider?sso=~s&sig=~s",
                      [YourDiscourseForum,URLEncodedPayload,HexSignature ]),
  {lists:flatten(ForwardString),Nonce}.



%%====================================================================
%% Internal functions
%%====================================================================

make_HMAC_SHA256_signature(Base64EncodedPayload,Key) ->
  SHASig = crypto:hmac(sha256,Key,Base64EncodedPayload),
  hexlify(SHASig).

make_url_encoded_payload(Nonce,ReturnURL) ->
  Payload = [<<"nonce=">>,Nonce,<<"&return_sso_url=">>,ReturnURL], 
  Base64EncodedPayload = base64:encode(iolist_to_binary(Payload)),
  URLEncodedPayload = http_uri:encode(binary_to_list(Base64EncodedPayload)),
  {URLEncodedPayload,Base64EncodedPayload}.




new_nonce() ->
 id_generation_tools:genWordadoplicusUnique(). 

%% stolen from http://stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex
hexlify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

