# pllm

> Use LLMs inside Prolog!

`pllm` is a minimal SWI-Prolog helper that exposes `llm/2`.
The predicate posts a prompt to an HTTP LLM endpoint and unifies the model's
response text with the second argument.

## Configuration

Set the following environment variables: 

- `LLM_API_URL` – the chat/completions endpoint that accepts POST requests.
- `LLM_API_KEY` – secret that will be sent as a bearer token.
- `LLM_MODEL` – optional model name (defaults to `gpt-4o-mini`).
- `LLM_API_TIMEOUT` – optional request timeout in seconds (defaults to 60).

## Usage

```bash
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

The request body is OpenAI-compatible (`{model: ..., messages: [...]}`) and
can be tweaked inside `llm_request_body/2`. Likewise, parsing logic can be
adjusted by editing `llm_extract_text/2` if you need to support a different
response format.

### Reverse prompts

If you call `llm/2` with an unbound first argument and a concrete response,
the library first asks the LLM to suggest a prompt that would (ideally)
produce that response, binds it to your variable, and then sends a *second*
request that wraps the suggested prompt in a hard constraint (`"answer only with ..."`). 
This costs two API calls and is still best-effort; the model may ignore the constraint, in which case the predicate simply fails.
