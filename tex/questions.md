The following are some questions / discussion points which I want to cover in this writeup.

0. Classical logic programming preliminaries
   - What the clause database is.

   - LP: classical logic programming

   - ILP: intuitionistic logic programming.  In the literature though,
     "ILP" usually stands for "inductive logic programming", which involves
     the automatic derivation of facts and rules which correspond to some
     given input data, which is completely unrelated to intuitionistic
     logic programming

1. What is intuitionistic logic programming?
   - Intuitive idea: allow for the dynamic addition of facts to the clause
     database, allowing for hypothetical reasoning.  For example,
     "if I take and pass CS101, will I be able to graduate?".  This sort
     of query is very unnatural in classical LP, but easy in ILP.

   - Looking at the different LPs as different restrictions and capabilities
     on the ruleset.

2. Why is does the literature seem so obtuse?
   - Focus is more on proofs of properties of the interpreter itself.
     Proofs are done in pencil-and-paper fashion, and so much of the
     paper tends to be on the proof itself.

   - In this community, people tend to use model theory for doing these
     proofs, which is based on the syntactic objects contained in classes.
     This is in contrast to proof theory, which focuses on semantic behaviors.
     Based on my understanding, in the PL community any sort of execution
     seems to be rooted in proof theory, so our PL perspective does not
     prepare us well for reading these sort of proofs.

   - Motivation is often vague or omitted entirely, resulting in papers which
     seem extremely abstract and disconnected from reality.  

   - Occassionally, something interesting happens - for example, three-valued
     logic seems to correspond to co-inductive big-step operational
     semantics, but the work on three-valued logic (in an LP setting)
     predates that work by around 20 years.

3. Doesn't the resolution operator no longer work?

4. What about negation-as-failure?

5. What about inconsistent databases?

6. Given its utility, why is ILP not more popular?
   - Dynamically adding facts makes common optimizations like indexing
     more challenging.  Similar problems come up with the related
     extralogical classical LP feature of assert.

   - In the general case, adding intuitionistic facts does NOT correspond
     to carefully using the extralogical assert and retract.  The reason
     why is that intuitionistic facts can capture uninstantiated variables,
     which may become instantiated later on.  If multiple facts capture
     the same variable, then as per the usual logical variable semantics
     both must become instantiated to the same value `v` if either becomes
     instantiated with `v`.  This cannot be done via `assert`, since essentially
     this is saying that two variables with the same name in different
     horn clauses actually refer to the same variable, which breaks scoping
     entirely.

   - Lambda Prolog is an ILP language, but it aims to be a metalanguage for
     writing theorem provers with.  Because it uses higher-order
     unification, it's not a very practical language for just doing ILP
     (performance will suffer).

7. What underlying logic does ILP correspond to?
   - The same logic as classical LP, along with intuitionistic implication
   - Need to be clear that classical LP does not correspond to classical logic;
     the only rule in classical LP is SLD-resolution, which is a combination
     of other logic rules.  This is why ILP is strictly more expressive than
     classical LP, while intuitionistic logic is strictly less expressive
     than classical logic.
