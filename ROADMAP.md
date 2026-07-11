# Roadmap

## Distribution

CRAN publication is a goal, but bayesgrove cannot be submitted while its
required `dagriculture` dependency is available only from GitHub. Publish
`dagriculture` to CRAN first, then remove bayesgrove's `Remotes` entry. The
cmdstanr r-universe repository can remain an additional repository.

## Protocol adapters

The next architectural experiment is a targets adapter that consumes targets
metadata and feeds typed summaries into bayesgrove's obligation and decision
protocol. It should remain a prototype until it demonstrates that the protocol
layer is useful with a second execution runtime.
