.. _Eta:

Eta FAQ
========

What is Eta?
------------
Eta is a dialect of Haskell that aims to bring the benefits of Haskell to the JVM, while supporting the vast majority of GHC extensions so that packages on Hackage can be used with little modification.

Where does the name "Eta" come from?
--------------------------------------
The name originates from eta reduction, the transformation `f x = g x => f = g` which makes lazy FP programs a bit simpler and more efficient. Moreover, *eta* in Greek happens to be the letter *H* in reference to Haskell.

Is Eta ready to use?
--------------------
Yes! Eta is hobby-ready. You can compile the vast majority of programs that GHC Haskell 7.10.3 can also compile, with the restriction that `TemplateHaskell` is not used and there is no equivalent of GHCi yet. You can call Java within Haskell and Haskell within Java with ease. The tooling is still immature but you can work on fun projects like `2048 <https://github.com/rahulmutt/eta-2048>`_.

When will Eta make its first official release?
----------------------------------------------
When Eta makes its first release, we want to make sure the minimum documentation and website are ready. Moreover, we have some pending changes that we want to finish before a release, such as:

- Finish support for the concurrent runtime
- Port all the core libraries
- Mature tooling for Java interoperation

Estimated time for the first release is January 2017.

Will Eta have an interpreter and support TemplateHaskell in the future?
-----------------------------------------------------------------------
Yes. The plan is to use the external interpreter implementation from GHC 8 in Eta. The timeline for this feature is not yet decided, but it is currently not a priority.

Will Eta be compatible with GHC 8?
----------------------------------
GHC 8 has made some fairy non-trivial changes to type system, so until that stabilizes and the bugs get ironed out, we want to avoid integrating those commits any time soon. On the other hand, there are quite a number of extensions that are orthogonal to the type system changes that we can integrate without drastic change, such as (but not limited to):

- Strict
- StrictData
- ApplicativeDo
- OverloadedRecordLabels

If a sufficient number of popular Hackage packages decide to update to GHC 8 without thinking about backwards compatibility, we will prioritize porting the extensions that are required to compile them. Until we get a pull from the packages side, we have no immediate plans of supporting GHC 8. We will probably implement a service to monitor popular packages on Hackage and automatically notify us when the ecosystem starts using newer extensions and adapt accordingly.

Why not make Eta a part of GHC? What will be different about the two?
---------------------------------------------------------------------
Eta will be a language with commercial support, backed by TypeLead. Therefore, our foremost priority is making the language stable, fast, and building a wide user base. This conflicts with the primary goal of GHC - to be a research platform for cutting-edge CS research. We believe that GHC being able to realize its goal and survive for decades is commendable and we have an amazing language as a result of its of labor.

But we are also big fans of Haskell and we are disappointed that it's used so little in industry. We feel that Haskell can solve many big problems in software development, but there's no concerted effort in solving the infrastructure/tooling problems that are hard requirements for large-scale, industrial use. We hope that we can solve these with Eta. We have met one of the biggest industry requirements with Eta: interoperability with the Java ecosystem.

We will continue to tackle more problems, optimistic that we can reach the wider programming community down the line if we solve the right problems at the right time. We will focus on the user experience and address the problems that are preventing Haskell-like languages from infiltrating industry including useful libraries, an industry-grade IDE, comprehensive documentation, and online training. Rather than forcing our priorities onto the GHC developers, we decided to take our own route to stay nimble and accommodate the needs of industry. We will open source most of our work so that the Haskell community can adopt it if they so choose.

How does Eta compare to GHC in performance?
---------------------------------------------
The GHC RTS does lots of wonderful optimizations in memory layout that are only possible at the native-level. Because of this, the speed of Eta will, in almost all cases, be slower than GHC. Therefore, in performance sensitive applications, GHC is probably a better option. We hope to reduce this gap as much as possible.

The current implementation aligns the Java call stack with the Eta runtime stack, allowing for a very efficient implementation of tail calls. This coupled with GHC's optimizations and a JVM-friendly encoding of the STG machine, yields decent performance. This strategy only works for lazy runtimes.

Performance is one of our highest priorities and we are actively looking for new ways to squeeze out a couple of percentage points when we can. The JVM is evolving to support functional programming constructs, so we are optimistic that Eta will benefit from these advances.

Why not work along with Haskell.org and help them improve their infrastructure and tooling?
-------------------------------------------------------------------------------------------
We are building a company around this, and we cannot rely on the speed of volunteer-based development. If we take matters into our own hands, we can achieve our targets at a much faster rate. This is not to say we will not collaborate with Haskell.org in attempts to keep the Eta ecosystem compatible with the Haskell ecosystem. These collaborations will not affect meeting our targets, so we are more than happy to do so. We will open source most of our work so that the Haskell community can adopt it if they so choose.

Why Eta and not Haskell?
------------------------

Haskell carries 26 years of psychology and perception that was built around the language. It's a language that many want to learn but give up because of peripheral reasons. Haskell has built a reputation over the years for being hard to learn, having brittle tooling, and missing useful libraries found in many other programming languages. We feel that this should be changed. We want Eta to be known for its focus on industrial use, comprehensive documentation, and extensive tooling.

Moreover, using the JVM as a platform will allow Eta to take advantage of a well-engineered and battle-tested garbage collector, a whole host of Just-In-Time compiler optimizations at runtime, and a vast ecosystem of libraries for almost any task.

Is Eta very different from GHC 7.10.3 in its initial state?
-----------------------------------------------------------
No. In fact, it's almost identical other than the Foreign Function Interface. The diverging changes will be those that can also be useful for GHC, but haven't been implemented yet due to lack of resources, time, and priority.

For example, we want to extend the type system to support row-type polymorphism and new syntax for anonymous record types as they have shown to be very successful in Elm and PureScript. We also want to focus on the necessary runtime changes to allow support for composable distributed systems. Cloud Haskell currently has limited runtime support with static pointers, but we want to go beyond, exploring Eden-style distribution.

When we do make such diverging changes, we will present a sketch of the implementation to the GHC devs and the relevant commits in Eta and anyone interested in integrating it to the GHC codebase is free to do so.

How is Eta different from Frege?
--------------------------------
Eta is strategically designed so that Hackage packages can be compiled with little modification, allowing reuse of existing infrastructure. This is done by supporting many of the GHC-specific extensions that are used heavily in popular libraries.

On the other hand, Frege, while it supports basic Haskell, lacks many of the key extensions required to compile Hackage, and hence cannot reuse the existing infrastructure. Moreover, because Eta uses a modified version of GHC's frontend, we have access to all the powerful and well-tuned optimizations that Frege does not.

Will Eta support CLR/.NET?
--------------------------
Because of the similarity of Java byteode and CLR bytecode, the code generator can be ported quite easily. The nontrivial part is porting the runtime. There are no plans for supporting it as of now, but in the distant future, it may be a possibility. It all depends on popular demand.
