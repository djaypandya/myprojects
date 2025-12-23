# AGENT CODING FAILURE PREVENTION RULES
# Purpose: Prevent known paths to bad code. Success is defined as the absence of avoidable failure.

## 0. PRIME DIRECTIVE (NON-NEGOTIABLE)
The agent MUST prioritize eliminating known failure modes over adding features, abstractions, or optimizations.
If a choice exists between "clever" and "clear", ALWAYS choose clear.

---

## 1. DO NOT WRITE CODE YOU DO NOT UNDERSTAND
❌ Never copy, adapt, or hallucinate APIs, libraries, functions, or patterns without understanding:
- What problem they solve
- Their inputs and outputs
- Their failure modes

✅ If uncertain:
- Pause
- Ask for clarification
- Or explicitly mark assumptions before proceeding

---

## 2. DO NOT OPTIMIZE PREMATURELY
❌ Never:
- Optimize for performance before correctness
- Add caching, concurrency, async, or batching without evidence
- Abstract for “future scale” without a present need

✅ Always:
- Make the simplest correct version work first
- Measure before optimizing
- Treat optimization as a *separate, explicit step*

---

## 3. AVOID UNNECESSARY ABSTRACTION
❌ Never introduce:
- Generic base classes with one implementation
- Utility layers that hide simple logic
- Frameworks when functions suffice
- Config systems for values that won’t change

✅ Abstraction is allowed ONLY if:
- The same pattern appears ≥2 times
- The abstraction reduces total cognitive load
- The abstraction has a single, clear responsibility

---

## 4. NO SILENT FAILURE
❌ Never:
- Swallow exceptions
- Use bare `except`
- Return `None` or empty values without explanation
- Fail without logging or surfacing the error

✅ Always:
- Fail loudly by default
- Include actionable error messages
- Preserve original error context

---

## 5. DO NOT GUESS DATA SHAPES OR TYPES
❌ Never assume:
- JSON structure
- Column names
- Units
- Nullability
- Ordering

✅ Always:
- Validate inputs explicitly
- Assert assumptions
- Fail early if expectations are violated

---

## 6. NO MAGIC NUMBERS OR IMPLICIT BEHAVIOUR
❌ Never embed:
- Unexplained constants
- Hard-coded thresholds
- “Just works” logic without documentation

✅ Always:
- Name constants descriptively
- Explain why a value exists
- Make implicit behavior explicit

---

## 7. AVOID COUPLING AND HIDDEN DEPENDENCIES
❌ Never:
- Let functions depend on global state
- Hide IO (files, network, env vars) inside business logic
- Mix data access, logic, and presentation

✅ Always:
- Pass dependencies explicitly
- Separate concerns cleanly
- Make side effects visible at function boundaries

---

## 8. DO NOT WRITE CLEVER CODE
❌ Never:
- Use dense one-liners
- Use language tricks to impress
- Trade readability for brevity
- Assume the reader is “smart enough”

✅ Code must be readable by:
- A competent engineer unfamiliar with the project
- Your future self under time pressure

---

## 9. COMMENTS ARE FOR "WHY", NOT "WHAT"
❌ Never comment obvious code

✅ Always comment when:
- The logic is non-intuitive
- A trade-off was made
- A constraint exists
- A known limitation is accepted

---

## 10. TEST THE FAILURE PATHS
❌ Never write only “happy path” logic

✅ Always consider:
- Empty input
- Nulls
- Partial data
- Timeouts
- Invalid user behavior

If tests are not written, reasoning about failure cases MUST be explicit in the code or explanation.

---

## 11. PREFER BORING, WELL-KNOWN SOLUTIONS
❌ Never introduce:
- Novel patterns without necessity
- Experimental libraries without justification

✅ Prefer:
- Standard library
- Proven idioms
- Familiar architectures

Boring code is stable code.

---

## 12. STOP CONDITIONS
The agent MUST stop and ask for guidance if:
- Requirements are ambiguous
- Trade-offs are unclear
- A decision has irreversible consequences
- Multiple reasonable designs exist

Silence in ambiguity is failure.

---

## FINAL CHECK (MANDATORY)
Before finalizing code, ask:
1. What are the top 3 ways this could fail?
2. Have I explicitly guarded against them?
3. Is any complexity present that does not pay rent?

If unsure, simplify.