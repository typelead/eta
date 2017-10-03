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
Yes! You can compile the vast majority of programs that GHC Haskell 7.10.3 can also compile, with the restriction that `TemplateHaskell` is not used and there is no equivalent of GHCi yet. You can call Java within Haskell and Haskell within Java with ease.

How does Eta handle TCO?
-------------------------
We don’t do anything to handle direct tail calls per se. At compile-time, we optimize tail recursive-calls into loops and we use the fact that lazy evaluation controls stack growth, unless you’re using too much of it. You can think of lazy evaluation as a hierachy of trampolines, but unlike general-purpose trampolines, each “trampoline” is specialized to a given type. And instead of executing a unknown virtual method call after getting a closure, it checks the tag of the closure which will help it select a branch to execute
next.

Will Eta have an interpreter and support TemplateHaskell in the future?
-----------------------------------------------------------------------
Yes. The plan is to use the external interpreter implementation from GHC 8 in Eta. The feature is due by September 15, 2017.

Will Eta be compatible with GHC 8?
----------------------------------
As GHC 8 is state-of-the-art and undoubtedly has some bugs, we will wait until the new extensions stabilize and start getting used in Hackage packages. If there are compelling,
practical use cases for a given extension, we will definitely backport it. A list of extensions we have plans to backport in the future are:

- Strict
- StrictData
- ApplicativeDo

If a sufficient number of popular Hackage packages decide to update to GHC 8 without thinking about backwards compatibility, we will prioritize porting the extensions that are required to compile them. Until we get a pull from the packages side, we have no immediate plans of supporting GHC 8. We will probably implement a service to monitor popular packages on Hackage and automatically notify us when the ecosystem starts using newer extensions and adapt accordingly.

Why not make Eta a part of GHC? What will be different about the two?
---------------------------------------------------------------------
Eta will be a language with commercial support, backed by TypeLead. Therefore, our foremost priority is making the language stable, fast, and building a wide user base. This may conflict with the primary goal of GHC - to be a research platform for cutting-edge CS research. We believe that GHC being able to realize its goal and survive for decades is commendable and we have an amazing language as a result of its of labor.

But we are also big fans of Haskell and we are disappointed that it's used so little in industry. We feel that Haskell can solve many big problems in software development, but there's no concerted effort in solving the infrastructure/tooling problems that are required for large-scale, industrial use. We hope that we can solve these with Eta. We have met one of the largest industry requirements with Eta: interoperability with the Java ecosystem.

We will continue to tackle more problems, optimistic that we can reach the wider programming community down the line if we solve the right problems at the right time. We will focus on the user experience and address the problems that are preventing Haskell-like languages from infiltrating industry including enterprise-grade libraries, an industry-grade IDE, comprehensive documentation, and online training.  We will open source most of our work so that the Haskell community can use it if it's helpful.

How does Eta compare to GHC in performance?
-------------------------------------------

Benchmarks show that Eta can be within the performance of GHC in a few cases and within 2x in many other cases after JIT optimizations. We've made several performance improvements since the project started and we will continue to make more. Performance is one of our highest priorities and we are actively looking for new ways to squeeze out a couple of percentage points when we can. The JVM is evolving to support functional programming constructs, so we are optimistic that Eta will benefit from these advances.

Why Eta and not Haskell?
------------------------

Haskell carries 26 years of psychology and perception that was built around the language. It's a language that many want to learn but give up because of peripheral reasons. Haskell has built a reputation over the years for being hard to learn, having brittle tooling, and missing useful libraries found in many other programming languages. We feel that this should be changed. We want Eta to be known for its focus on industrial use, comprehensive documentation, and extensive tooling.

Moreover, using the JVM as a platform will allow Eta to take advantage of a well-engineered and battle-tested garbage collector, a whole host of Just-In-Time compiler optimizations at runtime, and a vast ecosystem of libraries for almost any task.

How different is Eta from GHC?
------------------------------

It's very similar. In fact, it's almost identical other than the Foreign Function Interface. The diverging changes will be those that can also be useful for GHC, but haven't been implemented yet due to lack of resources, time, and priority.

For example, we want to extend the type system to support row-type polymorphism and new syntax for anonymous record types as they have shown to be very successful in Elm and PureScript. We also want to focus on the necessary runtime changes to allow support for composable distributed systems. Cloud Haskell currently has limited runtime support with static pointers, but we want to go beyond, exploring Eden-style distribution.

When we do make such diverging changes, we will present a sketch of the implementation to the GHC devs and the relevant commits in Eta and anyone interested in integrating it to the GHC codebase is free to do so.

How is Eta different from Frege?
--------------------------------

Eta is strategically designed so that Hackage packages can be compiled with little modification, allowing reuse of existing infrastructure. This is done by supporting many of the GHC-specific extensions that are used heavily in popular libraries.

On the other hand, Frege, while it supports basic Haskell, lacks many of the key extensions required to compile Hackage, and hence cannot reuse the existing infrastructure. Moreover, because Eta uses a modified version of GHC's frontend, we have access to all the powerful and well-tuned optimizations that Frege does not.

Will Eta support CLR/.NET?
--------------------------

Because of the similarity of Java bytecode and CLR bytecode, the code generator can be ported quite easily. The nontrivial part is porting the runtime. There are no plans for supporting it as of now, but in the distant future, it may be a possibility. It all depends on popular demand.


.. _opt-in-telemetry:

How do I opt-in to telemetry?
-----------------------------

1. Locate your Etlas config file.

   - On Unix-based systems like Linux and Mac, ``~/.etlas/config``
   - On Windows, ``%APPDATA%\Roaming\etlas\config``.
2. Open the file with a text editor.
3. Locate the ``send-metrics:`` field and change the value to ``True``.

.. _opt-out-telemetry:

How do I opt-out of telemetry?
------------------------------

1. Locate your Etlas config file.

   - On Unix-based systems like Linux and Mac, ``~/.etlas/config``
   - On Windows, ``%APPDATA%\Roaming\etlas\config``.
2. Open the file with a text editor.
3. Locate the ``send-metrics:`` field and change the value to ``False``.

How do I change character encoding of eta programs output?
----------------------------------------------------------

Most modern Linux/Unix systems has utf-8 as default character encoding and almost surely java will use it for output so you will not have many problems.
However on windows ones the story is a little bit more complex:

   - The default console character encoding usually is ``ibm850`` or similar.
   - The default system wide encoding varies depending on lang settings but it usually doesn't to match the console one (f.e. ``windows-1252``). Java (and eta) uses it as default char encoding.
   - Not all windows systems supports well ``utf-8`` (as code page ``65001``) or simply does not support it.

So you usually are going to have to change the default java character encoding to get a correct output of non-ascii chars.
Fortunately eta programs launcher scripts uses the environment variables ``$JAVA_ARGS`` and ``$ETA_JAVA_ARGS`` so you can do change java default charencoding with ``ETA_JAVA_ARGS="-Dfile.encoding=my_encoding"``.
You can determine ``my_encoding`` code in the `current java supported charsets page <https://docs.oracle.com/javase/8/docs/technotes/guides/intl/encoding.doc.html>`_.
For example, to get an ``utf-8`` output in a windows system you should:

    - Change the console font to one that supports unicode characters (f.e. ``Lucida``)
    - Change the encoding of console to ``utf-8`` with the command ``chcp 65001``. Without a suitable font the command will fail without notice any error.
    - Tell etlas you want to use it when executing eta programs with ``set ETA_JAVA_ARGS="-Dfile.encoding=UTF-8"``

Of course you can use ``$JAVA_ARGS`` or ``$ETA_JAVA_ARGS`` to change java file encoding in Linux/Unix systems if you have to do it.
The current behaviour of eta programs when you try to output chars that are not supported in the current file encoding is to throw an error (``<stdout>: commitBuffer: failed``), like ghc.
