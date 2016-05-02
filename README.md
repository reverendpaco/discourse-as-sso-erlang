# discourse-as-sso-erlang
Low-level erlang library to encode/decode payloads for using the forum software Discourse as an SSO endpoint.

Who is this For?
=====

You want to use this library if you are building an application in Erlang
and/or Elixir (or possibly LFE), and want to use the Discourse forum software
as your user management system.

The Discourse development team has published a specification for using their
software as an SSO endpoint in this post:

 [https://meta.discourse.org/t/using-discourse-as-a-sso-provider/32974](https://meta.discourse.org/t/using-discourse-as-a-sso-provider/32974).



This is **NOT** for the reverse case, where you have a Discoure forum and want
to use another system as your single-sign-on.

This is a low-level library for generating the payload according to the
specification in the above discussion (with hex encoded HMAC-Sha256
signatures), and has some helper methods for creating the URLs for redirecting.
It is still up to you to save the nonce in your Erlang/Elixir application and
to set your own cookies post validation. 

Usage
====

There are four main API methods, of which two will be mainly used:
```erlang
        as_formatted/3,
        get_sso_api_values/2,

        validate_sso_return_values/3,
        validate_query_string/2
```
The first two are for generating the payload to send **to** Discourse, and the last
two methods are for validating the payload sent back **from** Discourse (i.e. to the return
url that you specified).

```erlang
-type discourse_host() :: string().                                                                                
-type nonce() :: string().                                                                                
-type redirect_url() :: string().                                                                                
-type return_url() :: string().                                                                                
-type key() :: string().                                                                                
-type query_string() :: binary().
-type user_nvs() :: #{}. 
```

Mostly, you will use `as_formatted` and `validate_query_string`.  These methods
wrap the ohter two, but handle creating (or decoding) URLs. 

```erlang
-spec as_formatted(discourse_host(), return_url(), key()) -> 
      {redirect_url(), nonce()}.  
```
**It is up to you to re-direct.**  This library is not HTTP-aware.


```erlang
-spec validate_query_string(query_string(), key()) ->
  invalid | {valid, nonce(), user_nvs()}.  
```

Examples:
====

Example of Encoding payload to send to Discourse:
```erlang
2> discourse_as_sso_erlang:as_formatted("https://forum.wordadoplicus.com","https://wordadoplicus.com/return_url","mykey").

{"https://forum.wordadoplicus.com/session/sso_provider?sso=bm9uY2U9MWM1N2Fld3Y5NHQwYiZyZXR1cm5fc3NvX3VybD1odHRwczovL3dvcmRhZG9wbGljdXMuY29tL3JldHVybl91cmw%3D&sig=ee050c6e5e3a532a386c99d4c177c49254abd547f397e137a2bfd2fe116b5cac",
 "1c57aewv94t0b"}
```
Example of Decoding return values from Discourse:
```erlang
16> discourse_as_sso_erlang:validate_query_string(<<"sso=bm9uY2U9MWM1MzR0YmJxbTBnZyZuYW1lPURhbmllbG9wb2RvbiZ1c2VybmFt%0AZT13b3JkYWRvcGxpY3VzLWFkbWluJmVtYWlsPXNlcnZpY2UlNDB3b3JkYWRv%0AcGxpY3VzLmNvbSZleHRlcm5hbF9pZD0xJnJldHVybl9zc29fdXJsPWh0dHBz%0AJTNBJTJGJTJGd29yZGFkb3BsaWN1cy5jb20lMkZib29vJmFkbWluPXRydWUm%0AbW9kZXJhdG9yPWZhbHNl%0A&sig=0e59abaca0a6ec881d91d54ad8ce8feab65acebd467b28e7361a2c9dc822fc73">>,"wacopacotaco").
{valid,<<"1c534tbbqm0gg">>,
       #{<<"admin">> => <<"true">>,
         <<"email">> => <<"service@wordadoplicus.com">>,
         <<"external_id">> => <<"1">>,
         <<"moderator">> => <<"false">>,
         <<"name">> => <<"Danielopodon">>,
         <<"nonce">> => <<"1c534tbbqm0gg">>,
         <<"return_sso_url">> => <<"https://wordadoplicus.com/booo">>,
         <<"username">> => <<"wordadoplicus-admin">>}}
```

