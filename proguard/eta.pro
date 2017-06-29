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

-keep public class ghc_prim.ghc.Types {
  eta.runtime.stg.Closure False() ;
}

-keep public class base.ghc.TopHandler {
  eta.runtime.stg.Closure flushStdHandles();
}

-keep public class base.ghc.conc.Sync {
  eta.runtime.stg.Closure runSparks() ;
}

-keep public class base.control.exception.Base {
  eta.runtime.stg.Closure nonTermination();
  eta.runtime.stg.Closure nestedAtomically();
}

-keep public class base.ghc.Weak {
  eta.runtime.stg.Closure runFinalizerBatch();
}
