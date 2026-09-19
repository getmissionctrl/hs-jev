# hs-jev

A Haskell client for TypeSafe's **System One** (Jev) decision API: unstructured
state in, typed probabilistic decisions out. Choice / Score / Noul questions,
batched in one call via a free applicative, with an injected structured-event
logging callback.

## Quick start

```haskell
{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
import Jev

main :: IO ()
main = do
  client <- newClient                       -- reads TYPESAFE_API_KEY
  let st  = state ("...customer message..." :: String)
      ask = (,) <$> askChoice (plain "Which team?")
                     [("billing", descNone), ("returns", descNone)]
                <*> askNoul   (plain "Escalate to a human?")
  r <- callJev client st ask
  case r of
    Left err -> print err
    Right ((team, escalate), usage) ->
      print (team.choice, team.confidence, escalate, usage)
```

Questions compose with `<*>` and are sent in a **single** batched call
(TypeSafe's "speculative fan-out" pattern is intrinsic). Each answer's type is
known at the use site — no string keys, no `Maybe`.

### Closed-enum sugar

Map a Choice straight onto a closed Haskell enum; the returned key is
re-associated back to a constructor (or fails with `UnknownChoiceKey`):

```haskell
data Team = Billing | Returns | Shipping deriving (Eq, Ord, Show, Enum, Bounded)

askTeam :: Ask (ChoiceResult Team)
askTeam = askChoiceEnum (plain "Which team?") (const descNone)
-- ChoiceResult { chosen :: Team, dist :: Map Team Double, confidence :: Double }
```

### Routing on confidence

```haskell
import Jev.Patterns (band, Band(..))

route :: Double -> Band
route = band (0.5, 0.8)   -- < 0.5 Escalate, < 0.8 Confirm, else Act
```

### Logging

`callJev` runs in any `MonadIO`. It emits structured `JevEvent`s
(`Requested`/`Responded`/`Retried`/`Failed`) through `Config.onEvent` (default:
no-op). Wire it to katip — or anything — at your application layer:

```haskell
c <- newClientWith (defaultConfig myKey) { onEvent = \ev -> myLogger ev }
```

## Build & test

```bash
nix develop .#dev --command cabal build all
nix develop .#dev --command cabal test spec        # offline, hermetic (15 examples)
```

The opt-in live suite hits the real API. Put your key in `.env`
(`TYPESAFE_API_KEY=...`; git-ignored), then:

```bash
set -a && . ./.env && set +a
nix develop .#dev --command cabal test live         # skips cleanly if the key is unset
```

## Examples

`hs-jev-examples` recreates three examples from the TypeSafe docs, each issuing a
live `callJev` (so it needs `TYPESAFE_API_KEY`):

```bash
set -a && . ./.env && set +a
nix develop .#dev --command cabal run hs-jev-examples -- --list
nix develop .#dev --command cabal run hs-jev-examples -- support-triage
```

| example | primitives | what it shows |
|---|---|---|
| `support-triage` | Noul + Choice + Score | route a support ticket in one batched call (quickstart) |
| `spam-check` | six Nouls | speculative fan-out + three-band `band` verdict |
| `resume-screen` | four Scores | role-weighted `compositeScore` |

## Layout

| module | responsibility |
|---|---|
| `Jev.Types` | wire types, `Usage`, `JevError`, `JevEvent`, decode helpers |
| `Jev.Ask` | the `Ask` free applicative; `encodeQuestions` / `decodeAnswers`; enum sugar |
| `Jev.Client` | `Config`/`Client`, retry, `callJev` over `http-client` |
| `Jev.Patterns` | three-band routing, normalised composite scoring |
| `Jev` | umbrella re-export |
