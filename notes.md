## Course TODOs

* README files: `cabal run` instead of `cabal exec`

* Maybe add haddock doc as we go? Good to learn the syntax/practice

* HLint?

* level02:
    - need to say in the spec which content types to support.
    - mkTopic / mkCommentText: what are legal/illegal values?
        + empty not allowed

## spacemacs

* Set cabal target: `haskell-session-change-target`, then reload using `SPC m s b` (`haskell-process-load-file`).

## Followup

### Web API

* Servant: generate server, clients (Haskell, elm, JS)

### DB Access

* Selda: nice api. can work with existing tables
* beam: very new but promising
* Opaleye: safe, but uses arrow syntax which has fallen out of favour
* Persistent: heavy, Template Haskell, not good for existing schemas, tries to match SQL syntax which results in a non-ideal api
* Simple-sqlite/postgres fine to use

### Other

* EKG: monitoring
* optparse-applicative
