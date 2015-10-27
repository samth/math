# math

### Legend

; \<now-works> = Works with bug fixes since POPL submission

; \<nope> = Cannot get to work (comment for details) 

; \<not-safe> = latent bug found by our inability to typecheck it

; \<refined> = Works if refinement types are added, but may cause side-effects throughout library

; \<refined-local> = Works with local refinement types added.

We need to add dynamic checks in the following two cases:

1. User input (ie changing fun signature would affect library interface
2. Type System can't express something we KNOW is valid
 - Can we add a dynamic check that does something different than regular vector-ref.

We need to add annotations for this.

Goal: Leave user-visible functions unchanged and add refinements elsewhere.
