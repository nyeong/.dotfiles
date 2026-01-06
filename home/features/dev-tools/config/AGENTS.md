# Global Instructions

## nix

Use `nix run` when you need CLI tools which system doesn't have.

## Principles

### style

### Epistemlogy

Assumptions are the enemy. Never guess numerical values - benchmark instead of estimating.
When uncertain, measure. Say "this needs to be measured" rather than inventing statistics.

### Scaling

Validate at small scale before scaling up. Run a sub-minute version first to verify the full pipeline works. When scaling, only the scale parameter should change.

### Interaction

Clarify unclear requests, then proceed autonomosuly. Only ask for help when scripts timeout (>2min), sudo is needed, or genuine blockers arise.

### Ground truth Clarification

For non-trivial tasks, reach ground truth understanding before coding. Simple tasks execute immediately. Complex tasks (refactors, new features, ambiguous requirements) require clarification first: research codebase, ask targeted questions, confirm understanding, persist the plan, then execute autonomously.

### First principles reimplementation

Building from scratch can beat adapting legacy code when implementations are in wrong languages, carry historical baggage, or need architectural rewrites. Understand domain at spec level, choose optiomal stack, implement incrementally with human verification.

### Constraint Persistence

When user defines constraints ("never X", "always Y", "from now on"), immediately persist to project's local AGENT.md. Acknowledge, write, confirm.

# MCP

Use MCP servers for external info before guessing.

- exa: web search, code eamples.
- context7: library docs (resolve-library-id first, then query-docs).

## Machines

- `ssh nixbox` - homelab
- `ssh oc-eyes` - OCI cloud instance
