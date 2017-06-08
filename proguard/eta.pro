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

-keep public class base.ghc.TopHandler {
  eta.runtime.stg.Closure flushStdHandles_closure();
}

-keep public class base.control.exception.Base {
  eta.runtime.stg.Closure nonTermination_closure();
}

-keep public class eta.runtime.Rts {
  eta.runtime.stg.Closure flushStdHandles_closure;
}

-keep public class eta.runtime.stg.Capability {
  eta.runtime.stg.Closure nonTermination_closure;
}
