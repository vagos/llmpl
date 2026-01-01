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

Some services require an API key for authentication.
Set the `LLM_API_KEY` environment variable to your API key.
You can do the following in your shell before starting SWI-Prolog:
```
echo LLM_API_KEY="sk-..." >> .env
set -a && source .env && set +a
```

Configure the endpoint and default model before calling `llm/2` or `llm/3`:

```prolog
?- config("https://api.openai.com/v1/chat/completions", "gpt-4o-mini").
```

You can override the configured model per call with `llm/3` options.

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

?- llm("Say hello in French.", Output, [model("gpt-4o-mini"), timeout(30)]).
Output = "Bonjour !".

?- llm(Prompt, "Dog").
Prompt = "What animal is man's best friend?",
...
```

## Providers

This library expects an OpenAI-compatible chat/completions endpoint.
Below are common providers and endpoints you can try.

OpenAI
- Endpoint: `https://api.openai.com/v1/chat/completions`
- Example: `?- config("https://api.openai.com/v1/chat/completions", "gpt-4o-mini").`

Ollama (local)
- Endpoint: `http://localhost:11434/v1/chat/completions`
- Example: `?- config("http://localhost:11434/v1/chat/completions", "llama3.1").`

### Reverse prompts

If you call `llm/2` with an unbound first argument and a concrete response,
the library first asks the LLM to suggest a prompt that would (ideally)
produce that response, binds it to your variable, and then sends a *second*
request that wraps the suggested prompt in a hard constraint (`"answer only with ..."`). 
This costs two API calls and is still best-effort; the model may ignore the constraint, in which case the predicate simply fails.
