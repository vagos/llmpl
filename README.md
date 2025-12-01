# llmpl

> Use LLMs inside Prolog!

`pllm` is a minimal SWI-Prolog helper that exposes `llm/2`.
The predicate posts a prompt to an HTTP LLM endpoint and unifies the model's
response text with the second argument.

The library currently supports any OpenAI-compatible chat/completions endpoint.

## Installation

```sh
?- pack_install(pllm).
```

## Configuration

Set the following environment variables: 

| Variable         | Description                                         |
|------------------|-----------------------------------------------------|
| `LLM_API_URL`   | the chat/completions endpoint that accepts POST requests. |
| `LLM_API_KEY`   | secret that will be sent as a bearer token.         |
| `LLM_MODEL`     | optional model name (defaults to `gpt-4o-mini`).    |
| `LLM_API_TIMEOUT` | optional request timeout in seconds (defaults to 60). |

## Usage

```sh
# Fill in .env with your settings
set -a && souce .env && set +a
swipl
```

```prolog
?- [prolog/llm].
?- llm("Say hello in French.", Output).
Output = "Bonjour !".

?- llm(Prompt, "Dog").
Prompt = "What animal is man's best friend?",
...
```

### Reverse prompts

If you call `llm/2` with an unbound first argument and a concrete response,
the library first asks the LLM to suggest a prompt that would (ideally)
produce that response, binds it to your variable, and then sends a *second*
request that wraps the suggested prompt in a hard constraint (`"answer only with ..."`). 
This costs two API calls and is still best-effort; the model may ignore the constraint, in which case the predicate simply fails.
