:- module(llm, [llm/2]).

/** <module> Simple LLM client

This module exposes the predicate llm/2, which posts a user prompt to
an HTTP-based large language model (LLM) API and unifies the model's
response with the second argument.

Configuration is provided through environment variables.
The library can be reused across different APIs.

  * `LLM_API_URL`     – required LLM endpoint accepting POST requests.
  * `LLM_API_KEY`     – secret used to build a bearer token.
  * `LLM_MODEL`       – (optional) model identifier, defaults to "gpt-4o-mini".
  * `LLM_API_TIMEOUT` – (optional) request timeout in seconds, defaults to 60.

The library assumes an OpenAI-compatible payload/response. To target a
different API adjust llm_request_body/2 or llm_extract_text/2.
*/

:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_ssl_plugin)).

%!  llm(+Input, -Output) is det.
%
%   Send Input as a prompt to the configured LLM endpoint and unify
%   Output with the assistant's response text.

llm(Input, Output) :-
    ensure_prompt(Input, Prompt),
    llm_request_body(Prompt, Body),
    llm_post_json(Body, Response),
    llm_extract_text(Response, Output).

ensure_prompt(Input, Prompt) :-
    (   string(Input)
    ->  Prompt = Input
    ;   atom(Input)
    ->  atom_string(Input, Prompt)
    ;   is_list(Input)
    ->  string_codes(Prompt, Input)
    ;   throw(error(type_error(text, Input), _))
    ).

llm_request_body(Prompt, _{model:Model, messages:[_{role:'user', content:Prompt}]}) :-
    llm_model(Model).

llm_model(Model) :-
    (   getenv('LLM_MODEL', Raw), Raw \= ''
    ->  ensure_string(Raw, Model)
    ;   Model = "gpt-4o-mini"
    ).

llm_post_json(Body, Response) :-
    llm_endpoint(URL),
    llm_auth_header(Header),
    llm_timeout(Timeout),
    Options = [
        request_header('Authorization'=Header),
        accept(json),
        timeout(Timeout),
        json_object(dict),
        status_error(false),
        status_code(Status)
    ],
    catch(
        http_post(URL, json(Body), Data, Options),
        Error,
        throw(error(llm_request_failed(Error), _))
    ),
    handle_status(Status, Data, Response).

handle_status(Status, Data, Response) :-
    between(200, 299, Status),
    !,
    Response = Data.
handle_status(Status, Data, _) :-
    throw(error(llm_http_error(Status, Data), _)).

llm_endpoint(URL) :-
    (   getenv('LLM_API_URL', URL), URL \= ''
    ->  true
    ;   throw(error(existence_error(environment_variable, 'LLM_API_URL'), _))
    ).

llm_auth_header(Header) :-
    (   getenv('LLM_API_KEY', Key), Key \= ''
    ->  ensure_string(Key, KeyStr),
        format(string(Header), 'Bearer ~w', [KeyStr])
    ;   throw(error(existence_error(environment_variable, 'LLM_API_KEY'), _))
    ).

llm_timeout(Timeout) :-
    (   getenv('LLM_API_TIMEOUT', Raw), Raw \= '',
        catch(number_string(Timeout, Raw), _, fail)
    ->  true
    ;   Timeout = 60
    ).

llm_extract_text(Response, Output) :-
    (   _{choices:[First|_]} :< Response
    ->  extract_choice_text(First, Output0)
    ;   (   get_dict(output, Response, Output0)
        ;   get_dict(response, Response, Output0)
        )
    ),
    ensure_string(Output0, Output),
    !.
llm_extract_text(Response, _) :-
    throw(error(domain_error(llm_response, Response), _)).

extract_choice_text(Choice, Text) :-
    (   get_dict(message, Choice, Message),
        get_dict(content, Message, Content)
    ->  normalize_content(Content, Text)
    ;   get_dict(text, Choice, Text)
    ).

normalize_content(Content, Text) :-
    (   string(Content)
    ->  Text = Content
    ;   is_list(Content)
    ->  maplist(segment_text, Content, Segments),
        atomics_to_string(Segments, Text)
    ;   atom(Content)
    ->  atom_string(Content, Text)
    ;   throw(error(type_error(llm_content, Content), _))
    ).

segment_text(Dict, Text) :-
    (   is_dict(Dict),
        get_dict(type, Dict, 'text'),
        get_dict(text, Dict, Text0)
    ->  ensure_string(Text0, Text)
    ;   ensure_string(Dict, Text)
    ).

ensure_string(Value, Text) :-
    (   string(Value)
    ->  Text = Value
    ;   atom(Value)
    ->  atom_string(Value, Text)
    ;   is_list(Value)
    ->  string_codes(Text, Value)
    ;   throw(error(type_error(text, Value), _))
    ).
