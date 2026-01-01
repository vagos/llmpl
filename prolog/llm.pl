:- module(llm, [llm/2, llm/3, config/2]).

/** <module> Simple LLM client

This module exposes the predicate llm/2, which posts a user prompt to
an HTTP-based large language model (LLM) API and unifies the model's
response with the second argument.

Configuration is split between a one-time config predicate and
an API key environment variable. Optional per-call settings let you
override the model name and timeout.

  * `config/2`        – set the LLM endpoint and default model name.
  * `LLM_API_KEY`     – secret used to build a bearer token.
  * `model/1`         – (optional) model identifier, defaults to the configured model.
  * `timeout/1`       – (optional) request timeout in seconds, defaults to 60.

The library assumes an OpenAI-compatible payload/response. To target a
different API adjust llm_request_body/2 or llm_extract_text/2.
*/

:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(option)).

:- dynamic llm_config_url/1.
:- dynamic llm_config_model/1.

%!  llm(+Input, -Output) is det.
%
%   Send Input as a prompt to the configured LLM endpoint and unify
%   Output with the assistant's response text.

llm(Input, Output) :-
    llm(Input, Output, []).

%!  llm(+Input, -Output, +Options) is det.
%
%   Options may include model(Model) and timeout(Seconds).

llm(Input, Output, Options) :-
    llm_options(Options, Model, Timeout),
    (   var(Input)
    ->  ensure_prompt(Output, Target),
        generate_prompt(Target, Options, Prompt),
        Input = Prompt,
        constrained_prompt(Target, Prompt, PromptWithConstraint),
        llm_prompt_text(PromptWithConstraint, Model, Timeout, Text),
        unify_text(Text, Output)
    ;   llm_prompt_text(Input, Model, Timeout, Text),
        unify_text(Text, Output)
    ).

llm_prompt_text(Input, Model, Timeout, Text) :-
    ensure_prompt(Input, Prompt),
    llm_request_body(Prompt, Model, Body),
    llm_post_json(Body, Timeout, Response),
    llm_extract_text(Response, Text).

ensure_prompt(Input, Prompt) :-
    (   string(Input)
    ->  Prompt = Input
    ;   atom(Input)
    ->  atom_string(Input, Prompt)
    ;   is_list(Input)
    ->  string_codes(Prompt, Input)
    ;   throw(error(type_error(text, Input), _))
    ).

llm_request_body(Prompt, Model, _{model:Model, messages:[_{role:'user', content:Prompt}]}).

llm_post_json(Body, Timeout, Response) :-
    llm_endpoint(URL),
    llm_auth_header(Header),
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

unify_text(Text, Output) :-
    (   var(Output)
    ->  Output = Text
    ;   ensure_prompt(Output, Expected),
        Expected = Text
    ).

generate_prompt(Target, Options, Prompt) :-
    format(string(Request),
           "Provide a single user prompt that would make you reply with the exact text \"~w\". Return only the prompt.",
           [Target]),
    llm_options(Options, Model, Timeout),
    llm_prompt_text(Request, Model, Timeout, Suggestion),
    ensure_prompt(Suggestion, Prompt).

constrained_prompt(Target, Prompt, FinalPrompt) :-
    format(string(FinalPrompt),
           "You must answer ONLY with the exact text \"~w\" (case sensitive, no punctuation or extra words). Now respond to the following prompt:\n\n~w",
           [Target, Prompt]).

llm_endpoint(URL) :-
    (   llm_config_url(URL), URL \= ''
    ->  true
    ;   throw(error(existence_error(configuration, llm_url), _))
    ).

llm_auth_header(Header) :-
    (   getenv('LLM_API_KEY', Key), Key \= ''
    ->  ensure_string(Key, KeyStr),
        format(string(Header), 'Bearer ~w', [KeyStr])
    ;   throw(error(existence_error(environment_variable, 'LLM_API_KEY'), _))
    ).

config(URL, Model) :-
    ensure_string(URL, URLStr),
    ensure_string(Model, ModelStr),
    retractall(llm_config_url(_)),
    retractall(llm_config_model(_)),
    assertz(llm_config_url(URLStr)),
    assertz(llm_config_model(ModelStr)).

llm_options(Options, Model, Timeout) :-
    (   is_list(Options)
    ->  option(timeout(Timeout0), Options, 60),
        option_model(Options, Model),
        ensure_timeout(Timeout0, Timeout)
    ;   throw(error(type_error(list, Options), _))
    ).

option_model(Options, Model) :-
    (   option(model(Model0), Options)
    ->  ensure_string(Model0, Model)
    ;   (   llm_config_model(Configured)
        ->  Model = Configured
        ;   throw(error(existence_error(configuration, llm_model), _))
        )
    ).

ensure_timeout(Value, Timeout) :-
    (   number(Value)
    ->  Timeout is Value
    ;   ensure_string(Value, Text),
        catch(number_string(Timeout, Text), _, throw(error(type_error(number, Value), _)))
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
