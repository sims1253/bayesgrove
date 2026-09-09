# Bayesian research search

Bayesgrove records a search through Bayesian model configurations, its evidence,
and the reasons for pursuing or closing each path.

## Language

**Investigation**: A research question, inferential goal, and the candidates
explored to address them.

**Reframing**: An explicit change to an investigation's question or inferential
goal that preserves the earlier framing and its associated decisions.

**Candidate**: A specified joint distribution (P), optionally combined with a
posterior approximator (A) and training data (D). Its specification stays fixed.
_Avoid_: Node, when referring to a model configuration.

**Revision**: A descendant candidate with an explicit change to P, A, or D.

**Lineage**: A candidate and the chain of revisions descending from it.

**Evidence**: A recorded check result and interpretation associated with an exact
candidate. Gathering evidence does not itself create a revision.

**Comparison**: An assessment of explicit candidates using stated criteria and
identified evidence. The candidates can belong to different lineages.

**Decision**: An authored choice or review with a rationale and references to the
evidence it addresses. A decision does not establish that a model is correct.

**Closed lineage**: A path that will not be extended unless explicitly reopened.
Its candidates and evidence remain available for inspection and comparison.
_Avoid_: Failed model, when the reason is only a decision to stop searching.

**Hold**: An unmet policy requirement preventing a particular research action.
Resolving a hold does not erase the evidence that prompted it.

**Utility**: A dimension used to assess a candidate in relation to an inferential
goal. Utility priorities and trade-offs depend on the investigation.

**Intervention mode**: The choice to record research operations, also provide
guidance, or also enforce the configured requirements.

**Operator**: The human or agent proposing a research action. The action has the
same meaning for either kind of operator.
