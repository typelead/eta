-injars       eta-test.jar
-outjars      eta_out.jar
-libraryjars  <java.home>/lib/rt.jar
-overloadaggressively
-repackageclasses ''
-allowaccessmodification
-dontwarn

-keep public class eta.main {
  public static void main(java.lang.String[]);
}

-keep public class eta.runtime.stg.Closure
-keep public class eta.runtime.stg.StgContext

-keep public class ghc_prim.ghc.Types {
  eta.runtime.stg.Closure DFalse();
}

-keep public class base.ghc.TopHandler {
  eta.runtime.stg.Closure flushStdHandles();
}

-keep public class base.ghc.conc.Sync {
  eta.runtime.stg.Closure runSparks();
}

-keep public class base.control.exception.Base {
  eta.runtime.stg.Closure nonTermination();
  eta.runtime.stg.Closure nestedAtomically();
}

-keep public class base.ghc.io.Exception {
  eta.runtime.stg.Closure blockedIndefinitelyOnMVar();
}

-keep public class base.ghc.Weak {
  eta.runtime.stg.Closure runFinalizzerBatch();
}

-keep public class ghc_prim.ghc.types.datacons.Izh {* ;}

-keep public class base.java.exception.datacons.JException {* ;}

-keep public class base.ghc.exception.datacons.SomeException {* ;}

-keep public class base.java.Exception {
  eta.runtime.stg.Closure $fException_JException();
  eta.runtime.stg.Closure showException();
}
