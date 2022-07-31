{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The @TyCon@ datatype
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Eta.Types.TyCon(
        -- * Main TyCon data types
        TyCon, FieldLabel,

        AlgTyConRhs(..), visibleDataCons,
        TyConParent(..), isNoParent,
        FamTyConFlav(..), Role(..),

        -- ** Constructing TyCons
        mkAlgTyCon,
        mkClassTyCon,
        mkFunTyCon,
        mkPrimTyCon,
        mkKindTyCon,
        mkLiftedPrimTyCon,
        mkTupleTyCon,
        mkSynonymTyCon,
        mkFamilyTyCon,
        mkPromotedDataCon,
        mkPromotedTyCon,

        -- ** Predicates on TyCons
        isAlgTyCon,
        isClassTyCon, isFamInstTyCon,
        isFunTyCon,
        isPrimTyCon,
        isTupleTyCon, isUnboxedTupleTyCon, isBoxedTupleTyCon,
        isTypeSynonymTyCon,
        isDecomposableTyCon,
        isPromotedDataCon, isPromotedTyCon, isPromotedTupleTyCon,
        isPromotedDataCon_maybe, isPromotedTyCon_maybe, isLiftedTypeKindTyConName,
        promotableTyCon_maybe, promoteTyCon,

        isDataTyCon, isProductTyCon, isDataProductTyCon_maybe,
        isEnumerationTyCon,
        isNewTyCon, isAbstractTyCon,
        isFamilyTyCon, isOpenFamilyTyCon,
        isTypeFamilyTyCon, isDataFamilyTyCon,
        isOpenTypeFamilyTyCon, isClosedSynFamilyTyCon_maybe,
        isBuiltInSynFamTyCon_maybe,
        isUnLiftedTyCon,
        isGadtSyntaxTyCon, isDistinctTyCon, isDistinctAlgRhs,
        isInjectiveTyCon, isGenerativeTyCon, isGenInjAlgRhs,
        isTyConAssoc, tyConAssoc_maybe,
        isRecursiveTyCon,
        isImplicitTyCon,
        -- ETA-specific
        isObjectTyCon,

        -- ** Extracting information out of TyCons
        tyConName,
        tyConKind,
        tyConUnique,
        tyConTyVars,
        tyConCType, tyConCType_maybe,
        tyConDataCons, tyConDataCons_maybe,
        tyConSingleDataCon_maybe, tyConSingleAlgDataCon_maybe,
        tyConFamilySize,
        tyConStupidTheta,
        tyConArity,
        tyConRoles,
        tyConParent,
        tyConTuple_maybe, tyConClass_maybe,
        tyConFamInst_maybe, tyConFamInstSig_maybe, tyConFamilyCoercion_maybe,
        synTyConDefn_maybe, synTyConRhs_maybe, famTyConFlav_maybe,
        algTyConRhs,
        newTyConRhs, newTyConEtadArity, newTyConEtadRhs,
        unwrapNewTyCon_maybe, unwrapNewTyConEtad_maybe,
        tupleTyConBoxity, tupleTyConSort, tupleTyConArity,

        -- ** Manipulating TyCons
        tcExpandTyCon_maybe, coreExpandTyCon_maybe,
        makeTyConAbstract,
        newTyConCo, newTyConCo_maybe,
        pprPromotionQuote,

        -- * Primitive representations of Types
        PrimRep(..), --PrimElemRep(..),
        tyConPrimRep, isVoidRep, isGcPtrRep, isObjectRep, getObjectClass,
        primRepSizeW, --primElemRepSizeB,

        -- * Recursion breaking
        RecTcChecker, initRecTc, checkRecTc

) where

#include "HsVersions.h"

import {-# SOURCE #-} Eta.Types.TypeRep ( Kind, Type, PredType )
import {-# SOURCE #-} Eta.BasicTypes.DataCon ( DataCon, isVanillaDataCon, dataConTyCon )

import Eta.BasicTypes.Var
import Eta.Types.Class
import Eta.BasicTypes.BasicTypes
import Eta.Main.DynFlags
import Eta.Prelude.ForeignCall
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameSet
import Eta.Types.CoAxiom
import Eta.Prelude.PrelNames
import Eta.Utils.Maybes
import Eta.Utils.Outputable
-- import Eta.Main.Constants
import Eta.Utils.Util

import qualified Data.Data as Data
import Data.Typeable (Typeable)
-- TODO: Refactor to FastString?
import Data.Text (Text)


{-
-----------------------------------------------
        Notes about type families
-----------------------------------------------

Note [Type synonym families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Type synonym families, also known as "type functions", map directly
  onto the type functions in FC:

        type family F a :: *
        type instance F Int = Bool
        ..etc...

* Reply "yes" to isTypeFamilyTyCon, and isFamilyTyCon

* From the user's point of view (F Int) and Bool are simply
  equivalent types.

* A Haskell 98 type synonym is a degenerate form of a type synonym
  family.

* Type functions can't appear in the LHS of a type function:
        type instance F (F Int) = ...   -- BAD!

* Translation of type family decl:
        type family F a :: *
  translates to
    a FamilyTyCon 'F', whose FamTyConFlav is OpenSynFamilyTyCon

        type family G a :: * where
          G Int = Bool
          G Bool = Char
          G a = ()
  translates to
    a FamilyTyCon 'G', whose FamTyConFlav is ClosedSynFamilyTyCon, with the
    appropriate CoAxiom representing the equations

* In the future we might want to support
    * injective type families (allow decomposition)
  but we don't at the moment [2013]

Note [Data type families]
~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Wrappers for data instance tycons] in MkId.lhs

* Data type families are declared thus
        data family T a :: *
        data instance T Int = T1 | T2 Bool

  Here T is the "family TyCon".

* Reply "yes" to isDataFamilyTyCon, and isFamilyTyCon

* The user does not see any "equivalent types" as he did with type
  synonym families.  He just sees constructors with types
        T1 :: T Int
        T2 :: Bool -> T Int

* Here's the FC version of the above declarations:

        data T a
        data R:TInt = T1 | T2 Bool
        axiom ax_ti : T Int ~ R:TInt

  The R:TInt is the "representation TyCons".
  It has an AlgTyConParent of
        FamInstTyCon T [Int] ax_ti

* The axiom ax_ti may be eta-reduced; see
  Note [Eta reduction for data family axioms] in TcInstDcls

* The data constructor T2 has a wrapper (which is what the
  source-level "T2" invokes):

        $WT2 :: Bool -> T Int
        $WT2 b = T2 b `cast` sym ax_ti

* A data instance can declare a fully-fledged GADT:

        data instance T (a,b) where
          X1 :: T (Int,Bool)
          X2 :: a -> b -> T (a,b)

  Here's the FC version of the above declaration:

        data R:TPair a where
          X1 :: R:TPair Int Bool
          X2 :: a -> b -> R:TPair a b
        axiom ax_pr :: T (a,b) ~ R:TPair a b

        $WX1 :: forall a b. a -> b -> T (a,b)
        $WX1 a b (x::a) (y::b) = X2 a b x y `cast` sym (ax_pr a b)

  The R:TPair are the "representation TyCons".
  We have a bit of work to do, to unpick the result types of the
  data instance declaration for T (a,b), to get the result type in the
  representation; e.g.  T (a,b) --> R:TPair a b

  The representation TyCon R:TList, has an AlgTyConParent of

        FamInstTyCon T [(a,b)] ax_pr

* Notice that T is NOT translated to a FC type function; it just
  becomes a "data type" with no constructors, which can be coerced inot
  into R:TInt, R:TPair by the axioms.  These axioms
  axioms come into play when (and *only* when) you
        - use a data constructor
        - do pattern matching
  Rather like newtype, in fact

  As a result

  - T behaves just like a data type so far as decomposition is concerned

  - (T Int) is not implicitly converted to R:TInt during type inference.
    Indeed the latter type is unknown to the programmer.

  - There *is* an instance for (T Int) in the type-family instance
    environment, but it is only used for overlap checking

  - It's fine to have T in the LHS of a type function:
    type instance F (T a) = [a]

  It was this last point that confused me!  The big thing is that you
  should not think of a data family T as a *type function* at all, not
  even an injective one!  We can't allow even injective type functions
  on the LHS of a type function:
        type family injective G a :: *
        type instance F (G Int) = Bool
  is no good, even if G is injective, because consider
        type instance G Int = Bool
        type instance F Bool = Char

  So a data type family is not an injective type function. It's just a
  data type with some axioms that connect it to other data types.

Note [Associated families and their parent class]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Associated* families are just like *non-associated* families, except
that they have a TyConParent of AssocFamilyTyCon, which identifies the
parent class.

However there is an important sharing relationship between
  * the tyConTyVars of the parent Class
  * the tyConTyvars of the associated TyCon

   class C a b where
     data T p a
     type F a q b

Here the 'a' and 'b' are shared with the 'Class'; that is, they have
the same Unique.

This is important. In an instance declaration we expect
  * all the shared variables to be instantiated the same way
  * the non-shared variables of the associated type should not
    be instantiated at all

  instance C [x] (Tree y) where
     data T p [x] = T1 x | T2 p
     type F [x] q (Tree y) = (x,y,q)

Note [TyCon Role signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every tycon has a role signature, assigning a role to each of the tyConTyVars
(or of equal length to the tyConArity, if there are no tyConTyVars). An
example demonstrates these best: say we have a tycon T, with parameters a at
nominal, b at representational, and c at phantom. Then, to prove
representational equality between T a1 b1 c1 and T a2 b2 c2, we need to have
nominal equality between a1 and a2, representational equality between b1 and
b2, and nothing in particular (i.e., phantom equality) between c1 and c2. This
might happen, say, with the following declaration:

  data T a b c where
    MkT :: b -> T Int b c

Data and class tycons have their roles inferred (see inferRoles in TcTyDecls),
as do vanilla synonym tycons. Family tycons have all parameters at role N,
though it is conceivable that we could relax this restriction. (->)'s and
tuples' parameters are at role R. Each primitive tycon declares its roles;
it's worth noting that (~#)'s parameters are at role N. Promoted data
constructors' type arguments are at role R. All kind arguments are at role
N.

************************************************************************
*                                                                      *
\subsection{The data type}
*                                                                      *
************************************************************************
-}

-- | TyCons represent type constructors. Type constructors are introduced by
-- things such as:
--
-- 1) Data declarations: @data Foo = ...@ creates the @Foo@ type constructor of
--    kind @*@
--
-- 2) Type synonyms: @type Foo = ...@ creates the @Foo@ type constructor
--
-- 3) Newtypes: @newtype Foo a = MkFoo ...@ creates the @Foo@ type constructor
--    of kind @* -> *@
--
-- 4) Class declarations: @class Foo where@ creates the @Foo@ type constructor
--    of kind @*@
--
-- This data type also encodes a number of primitive, built in type constructors
-- such as those for function and tuple types.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data TyCon
  = -- | The function type constructor, @(->)@
    FunTyCon {
        tyConUnique :: Unique,   -- ^ A Unique of this TyCon. Invariant:
                                 -- identical to Unique of Name stored in
                                 -- tyConName field.

        tyConName   :: Name,     -- ^ Name of the constructor

        tyConKind   :: Kind,     -- ^ Kind of this TyCon (full kind, not just
                                 -- the return kind)

        tyConArity  :: Arity     -- ^ Number of arguments this TyCon must
                                 -- receive to be considered saturated
                                 -- (including implicit kind variables)
    }

  -- | Algebraic type constructors, which are defined to be those
  -- arising @data@ type and @newtype@ declarations.  All these
  -- constructors are lifted and boxed. See 'AlgTyConRhs' for more
  -- information.
  | AlgTyCon {
        tyConUnique  :: Unique,  -- ^ A Unique of this TyCon. Invariant:
                                 -- identical to Unique of Name stored in
                                 -- tyConName field.

        tyConName    :: Name,    -- ^ Name of the constructor

        tyConKind    :: Kind,    -- ^ Kind of this TyCon (full kind, not just
                                 -- the return kind)

        tyConArity   :: Arity,   -- ^ Number of arguments this TyCon must
                                 -- receive to be considered saturated
                                 -- (including implicit kind variables)

        tyConTyVars  :: [TyVar], -- ^ The kind and type variables used in the
                                 -- type constructor.
                                 -- Invariant: length tyvars = arity
                                 -- Precisely, this list scopes over:
                                 --
                                 -- 1. The 'algTcStupidTheta'
                                 -- 2. The cached types in algTyConRhs.NewTyCon
                                 -- 3. The family instance types if present
                                 --
                                 -- Note that it does /not/ scope over the data
                                 -- constructors.

        tcRoles      :: [Role],  -- ^ The role for each type variable
                                 -- This list has the same length as tyConTyVars
                                 -- See also Note [TyCon Role signatures]

        tyConCType   :: Maybe CType,-- ^ The C type that should be used
                                    -- for this type when using the FFI
                                    -- and CAPI

        algTcGadtSyntax  :: Bool,   -- ^ Was the data type declared with GADT
                                    -- syntax?  If so, that doesn't mean it's a
                                    -- true GADT; only that the "where" form
                                    -- was used.  This field is used only to
                                    -- guide pretty-printing

        algTcStupidTheta :: [PredType], -- ^ The \"stupid theta\" for the data
                                        -- type (always empty for GADTs).  A
                                        -- \"stupid theta\" is the context to
                                        -- the left of an algebraic type
                                        -- declaration, e.g. @Eq a@ in the
                                        -- declaration @data Eq a => T a ...@.

        algTcRhs    :: AlgTyConRhs, -- ^ Contains information about the
                                    -- data constructors of the algebraic type

        algTcRec    :: RecFlag,     -- ^ Tells us whether the data type is part
                                    -- of a mutually-recursive group or not

        algTcParent :: TyConParent, -- ^ Gives the class or family declaration
                                    -- 'TyCon' for derived 'TyCon's representing
                                    -- class or family instances, respectively.
                                    -- See also 'synTcParent'

        tcPromoted  :: Maybe TyCon  -- ^ Promoted TyCon, if any
    }

  -- | Represents the infinite family of tuple type constructors,
  --   @()@, @(a,b)@, @(# a, b #)@ etc.
  | TupleTyCon {
        tyConUnique    :: Unique,   -- ^ A Unique of this TyCon. Invariant:
                                    -- identical to Unique of Name stored in
                                    -- tyConName field.

        tyConName      :: Name,     -- ^ Name of the constructor

        tyConKind      :: Kind,     -- ^ Kind of this TyCon (full kind, not just
                                    -- the return kind)

        tyConArity     :: Arity,    -- ^ Number of arguments this TyCon must
                                    -- receive to be considered saturated
                                    -- (including implicit kind variables)

        tyConTupleSort :: TupleSort,-- ^ Is this a boxed, unboxed or constraint
                                    -- tuple?

        tyConTyVars    :: [TyVar],  -- ^ List of type and kind variables in this
                                    -- TyCon. Includes implicit kind variables.
                                    -- Invariant:
                                    -- length tyConTyVars = tyConArity

        dataCon        :: DataCon,  -- ^ Corresponding tuple data constructor

        tcPromoted     :: Maybe TyCon
                                    -- ^ Nothing for unboxed tuples
    }

  -- | Represents type synonyms
  | SynonymTyCon {
        tyConUnique  :: Unique,  -- ^ A Unique of this TyCon. Invariant:
                                 -- identical to Unique of Name stored in
                                 -- tyConName field.

        tyConName    :: Name,    -- ^ Name of the constructor

        tyConKind    :: Kind,    -- ^ Kind of this TyCon (full kind, not just
                                 -- the return kind)

        tyConArity   :: Arity,   -- ^ Number of arguments this TyCon must
                                 -- receive to be considered saturated
                                 -- (including implicit kind variables)

        tyConTyVars  :: [TyVar], -- ^ List of type and kind variables in this
                                 -- TyCon. Includes implicit kind variables.
                                 -- Invariant: length tyConTyVars = tyConArity

        tcRoles      :: [Role],  -- ^ The role for each type variable
                                 -- This list has the same length as tyConTyVars
                                 -- See also Note [TyCon Role signatures]

        synTcRhs     :: Type     -- ^ Contains information about the expansion
                                 -- of the synonym
    }

  -- | Represents type families
  | FamilyTyCon {
        tyConUnique  :: Unique,  -- ^ A Unique of this TyCon. Invariant:
                                 -- identical to Unique of Name stored in
                                 -- tyConName field.

        tyConName    :: Name,    -- ^ Name of the constructor

        tyConKind    :: Kind,    -- ^ Kind of this TyCon (full kind, not just
                                 -- the return kind)

        tyConArity   :: Arity,   -- ^ Number of arguments this TyCon must
                                 -- receive to be considered saturated
                                 -- (including implicit kind variables)

        tyConTyVars  :: [TyVar], -- ^ The kind and type variables used in the
                                 -- type constructor.
                                 -- Invariant: length tyvars = arity
                                 -- Precisely, this list scopes over:
                                 --
                                 -- 1. The 'algTcStupidTheta'
                                 -- 2. The cached types in 'algTyConRhs.NewTyCon'
                                 -- 3. The family instance types if present
                                 --
                                 -- Note that it does /not/ scope over the data
                                 -- constructors.

        famTcFlav    :: FamTyConFlav, -- ^ Type family flavour: open, closed,
                                      -- abstract, built-in. See comments for
                                      -- FamTyConFlav

        famTcParent  :: TyConParent   -- ^ TyCon of enclosing class for
                                      -- associated type families

    }

  -- | Primitive types; cannot be defined in Haskell. This includes
  -- the usual suspects (such as @Int#@) as well as foreign-imported
  -- types and kinds
  | PrimTyCon {
        tyConUnique   :: Unique, -- ^ A Unique of this TyCon. Invariant:
                                 -- identical to Unique of Name stored in
                                 -- tyConName field.

        tyConName     :: Name,   -- ^ Name of the constructor

        tyConKind     :: Kind,   -- ^ Kind of this TyCon (full kind, not just
                                 -- the return kind)

        tyConArity    :: Arity,  -- ^ Number of arguments this TyCon must
                                 -- receive to be considered saturated
                                 -- (including implicit kind variables)

        tcRoles       :: [Role], -- ^ The role for each type variable
                                 -- This list has the same length as tyConTyVars
                                 -- See also Note [TyCon Role signatures]

        primTyConRep  :: PrimRep,-- ^ Many primitive tycons are unboxed, but
                                 -- some are boxed (represented by
                                 -- pointers). This 'PrimRep' holds that
                                 -- information.  Only relevant if tyConKind = *

        isUnLifted   :: Bool     -- ^ Most primitive tycons are unlifted (may
                                 -- not contain bottom) but other are lifted,
                                 -- e.g. @RealWorld@
    }

  -- | Represents promoted data constructor.
  | PromotedDataCon {          -- See Note [Promoted data constructors]
        tyConUnique :: Unique, -- ^ Same Unique as the data constructor
        tyConName   :: Name,   -- ^ Same Name as the data constructor
        tyConArity  :: Arity,
        tyConKind   :: Kind,   -- ^ Translated type of the data constructor
        tcRoles     :: [Role], -- ^ Roles: N for kind vars, R for type vars
        dataCon     :: DataCon -- ^ Corresponding data constructor
    }

  -- | Represents promoted type constructor.
  | PromotedTyCon {
        tyConUnique :: Unique, -- ^ Same Unique as the type constructor
        tyConName   :: Name,   -- ^ Same Name as the type constructor
        tyConArity  :: Arity,  -- ^ n if ty_con :: * -> ... -> *  n times
        tyConKind   :: Kind,   -- ^ Always TysPrim.superKind
        ty_con      :: TyCon   -- ^ Corresponding type constructor
    }

  deriving Typeable

-- | Names of the fields in an algebraic record type
type FieldLabel = Name

-- | Represents right-hand-sides of 'TyCon's for algebraic types
data AlgTyConRhs

    -- | Says that we know nothing about this data type, except that
    -- it's represented by a pointer.  Used when we export a data type
    -- abstractly into an .hi file.
  = AbstractTyCon
      Bool      -- True  <=> It's definitely a distinct data type,
                --           equal only to itself; ie not a newtype
                -- False <=> Not sure
                -- See Note [AbstractTyCon and type equality]

    -- | Represents an open type family without a fixed right hand
    -- side.  Additional instances can appear at any time.
    --
    -- These are introduced by either a top level declaration:
    --
    -- > data T a :: *
    --
    -- Or an associated data type declaration, within a class declaration:
    --
    -- > class C a b where
    -- >   data T b :: *
  | DataFamilyTyCon

    -- | Information about those 'TyCon's derived from a @data@
    -- declaration. This includes data types with no constructors at
    -- all.
  | DataTyCon {
        data_cons :: [DataCon],
                          -- ^ The data type constructors; can be empty if the
                          --   user declares the type to have no constructors
                          --
                          -- INVARIANT: Kept in order of increasing 'DataCon'
                          -- tag (see the tag assignment in DataCon.mkDataCon)

        is_enum :: Bool   -- ^ Cached value: is this an enumeration type?
                          --   See Note [Enumeration types]
    }

  -- | Information about those 'TyCon's derived from a @newtype@ declaration
  | NewTyCon {
        data_con :: DataCon,    -- ^ The unique constructor for the @newtype@.
                                --   It has no existentials

        nt_rhs :: Type,         -- ^ Cached value: the argument type of the
                                -- constructor, which is just the representation
                                -- type of the 'TyCon' (remember that @newtype@s
                                -- do not exist at runtime so need a different
                                -- representation type).
                                --
                                -- The free 'TyVar's of this type are the
                                -- 'tyConTyVars' from the corresponding 'TyCon'

        nt_etad_rhs :: ([TyVar], Type),
                        -- ^ Same as the 'nt_rhs', but this time eta-reduced.
                        -- Hence the list of 'TyVar's in this field may be
                        -- shorter than the declared arity of the 'TyCon'.

                        -- See Note [Newtype eta]
        nt_co :: CoAxiom Unbranched
                             -- The axiom coercion that creates the @newtype@
                             -- from the representation 'Type'.

                             -- See Note [Newtype coercions]
                             -- Invariant: arity = #tvs in nt_etad_rhs;
                             -- See Note [Newtype eta]
                             -- Watch out!  If any newtypes become transparent
                             -- again check Trac #1072.
    }

{-
Note [AbstractTyCon and type equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO
-}

-- | Extract those 'DataCon's that we are able to learn about.  Note
-- that visibility in this sense does not correspond to visibility in
-- the context of any particular user program!
visibleDataCons :: AlgTyConRhs -> [DataCon]
visibleDataCons (AbstractTyCon {})            = []
visibleDataCons DataFamilyTyCon {}            = []
visibleDataCons (DataTyCon{ data_cons = cs }) = cs
visibleDataCons (NewTyCon{ data_con = c })    = [c]

-- ^ Both type classes as well as family instances imply implicit
-- type constructors.  These implicit type constructors refer to their parent
-- structure (ie, the class or family from which they derive) using a type of
-- the following form.  We use 'TyConParent' for both algebraic and synonym
-- types, but the variant 'ClassTyCon' will only be used by algebraic 'TyCon's.
data TyConParent
  = -- | An ordinary type constructor has no parent.
    NoParentTyCon

  -- | Type constructors representing a class dictionary.
  -- See Note [ATyCon for classes] in TypeRep
  | ClassTyCon
        Class           -- INVARIANT: the classTyCon of this Class is the
                        -- current tycon

  -- | An *associated* type of a class.
  | AssocFamilyTyCon
        Class           -- The class in whose declaration the family is declared
                        -- See Note [Associated families and their parent class]

  -- | Type constructors representing an instance of a *data* family.
  -- Parameters:
  --
  --  1) The type family in question
  --
  --  2) Instance types; free variables are the 'tyConTyVars'
  --  of the current 'TyCon' (not the family one). INVARIANT:
  --  the number of types matches the arity of the family 'TyCon'
  --
  --  3) A 'CoTyCon' identifying the representation
  --  type with the type instance family
  | FamInstTyCon          -- See Note [Data type families]
        (CoAxiom Unbranched)  -- The coercion axiom.
               -- Generally of kind   T ty1 ty2 ~ R:T a b c
               -- where T is the family TyCon,
               -- and R:T is the representation TyCon (ie this one)
               -- and a,b,c are the tyConTyVars of this TyCon
               --
               -- BUT may be eta-reduced; see TcInstDcls
               --     Note [Eta reduction for data family axioms]

          -- Cached fields of the CoAxiom, but adjusted to
          -- use the tyConTyVars of this TyCon
        TyCon   -- The family TyCon
        [Type]  -- Argument types (mentions the tyConTyVars of this TyCon)
                -- Match in length the tyConTyVars of the family TyCon

        -- E.g.  data instance T [a] = ...
        -- gives a representation tycon:
        --      data R:TList a = ...
        --      axiom co a :: T [a] ~ R:TList a
        -- with R:TList's algTcParent = FamInstTyCon T [a] co

instance Outputable TyConParent where
    ppr NoParentTyCon           = text "No parent"
    ppr (ClassTyCon cls)        = text "Class parent" <+> ppr cls
    ppr (AssocFamilyTyCon cls)  =
        text "Class parent (assoc. family)" <+> ppr cls
    ppr (FamInstTyCon _ tc tys) =
        text "Family parent (family instance)" <+> ppr tc <+> sep (map ppr tys)

-- | Checks the invariants of a 'TyConParent' given the appropriate type class
-- name, if any
okParent :: Name -> TyConParent -> Bool
okParent _       NoParentTyCon               = True
okParent tc_name (AssocFamilyTyCon cls)      = tc_name `elem` map tyConName (classATs cls)
okParent tc_name (ClassTyCon cls)            = tc_name == tyConName (classTyCon cls)
okParent _       (FamInstTyCon _ fam_tc tys) = tyConArity fam_tc == length tys

isNoParent :: TyConParent -> Bool
isNoParent NoParentTyCon = True
isNoParent _             = False

--------------------

-- | Information pertaining to the expansion of a type synonym (@type@)
data FamTyConFlav
  = -- | An open type synonym family  e.g. @type family F x y :: * -> *@
     OpenSynFamilyTyCon

   -- | A closed type synonym family  e.g.
   -- @type family F x where { F Int = Bool }@
   | ClosedSynFamilyTyCon
       (CoAxiom Branched) -- The one axiom for this family

   -- | A closed type synonym family declared in an hs-boot file with
   -- type family F a where ..
   | AbstractClosedSynFamilyTyCon

   -- | Built-in type family used by the TypeNats solver
   | BuiltInSynFamTyCon BuiltInSynFamily

{-
Note [Closed type families]
~~~~~~~~~~~~~~~~~~~~~~~~~
* In an open type family you can add new instances later.  This is the
  usual case.

* In a closed type family you can only put equations where the family
  is defined.


Note [Promoted data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A data constructor can be promoted to become a type constructor,
via the PromotedTyCon alternative in TyCon.

* Only data constructors with
     (a) no kind polymorphism
     (b) no constraints in its type (eg GADTs)
  are promoted.  Existentials are ok; see Trac #7347.

* The TyCon promoted from a DataCon has the *same* Name and Unique as
  the DataCon.  Eg. If the data constructor Data.Maybe.Just(unique 78,
  say) is promoted to a TyCon whose name is Data.Maybe.Just(unique 78)

* The *kind* of a promoted DataCon may be polymorphic.  Example:
    type of DataCon           Just :: forall (a:*). a -> Maybe a
    kind of (promoted) tycon  Just :: forall (a:box). a -> Maybe a
  The kind is not identical to the type, because of the */box
  kind signature on the forall'd variable; so the tyConKind field of
  PromotedTyCon is not identical to the dataConUserType of the
  DataCon.  But it's the same modulo changing the variable kinds,
  done by DataCon.promoteType.

* Small note: We promote the *user* type of the DataCon.  Eg
     data T = MkT {-# UNPACK #-} !(Bool, Bool)
  The promoted kind is
     MkT :: (Bool,Bool) -> T
  *not*
     MkT :: Bool -> Bool -> T

Note [Enumeration types]
~~~~~~~~~~~~~~~~~~~~~~~~
We define datatypes with no constructors to *not* be
enumerations; this fixes trac #2578,  Otherwise we
end up generating an empty table for
  <mod>_<type>_closure_tbl
which is used by tagToEnum# to map Int# to constructors
in an enumeration. The empty table apparently upset
the linker.

Moreover, all the data constructor must be enumerations, meaning
they have type  (forall abc. T a b c).  GADTs are not enumerations.
For example consider
    data T a where
      T1 :: T Int
      T2 :: T Bool
      T3 :: T a
What would [T1 ..] be?  [T1,T3] :: T Int? Easiest thing is to exclude them.
See Trac #4528.

Note [Newtype coercions]
~~~~~~~~~~~~~~~~~~~~~~~~
The NewTyCon field nt_co is a CoAxiom which is used for coercing from
the representation type of the newtype, to the newtype itself. For
example,

   newtype T a = MkT (a -> a)

the NewTyCon for T will contain nt_co = CoT where CoT t : T t ~ t -> t.

In the case that the right hand side is a type application
ending with the same type variables as the left hand side, we
"eta-contract" the coercion.  So if we had

   newtype S a = MkT [a]

then we would generate the arity 0 axiom CoS : S ~ [].  The
primary reason we do this is to make newtype deriving cleaner.

In the paper we'd write
        axiom CoT : (forall t. T t) ~ (forall t. [t])
and then when we used CoT at a particular type, s, we'd say
        CoT @ s
which encodes as (TyConApp instCoercionTyCon [TyConApp CoT [], s])

Note [Newtype eta]
~~~~~~~~~~~~~~~~~~
Consider
        newtype Parser a = MkParser (IO a) deriving Monad
Are these two types equal (to Core)?
        Monad Parser
        Monad IO
which we need to make the derived instance for Monad Parser.

Well, yes.  But to see that easily we eta-reduce the RHS type of
Parser, in this case to ([], Froogle), so that even unsaturated applications
of Parser will work right.  This eta reduction is done when the type
constructor is built, and cached in NewTyCon.  The cached field is
only used in coreExpandTyCon_maybe.

Here's an example that I think showed up in practice
Source code:
        newtype T a = MkT [a]
        newtype Foo m = MkFoo (forall a. m a -> Int)

        w1 :: Foo []
        w1 = ...

        w2 :: Foo T
        w2 = MkFoo (\(MkT x) -> case w1 of MkFoo f -> f x)

After desugaring, and discarding the data constructors for the newtypes,
we get:
        w2 :: Foo T
        w2 = w1
And now Lint complains unless Foo T == Foo [], and that requires T==[]

This point carries over to the newtype coercion, because we need to
say
        w2 = w1 `cast` Foo CoT

so the coercion tycon CoT must have
        kind:    T ~ []
 and    arity:   0

************************************************************************
*                                                                      *
\subsection{PrimRep}
*                                                                      *
************************************************************************

Note [rep swamp]

GHC has a rich selection of types that represent "primitive types" of
one kind or another.  Each of them makes a different set of
distinctions, and mostly the differences are for good reasons,
although it's probably true that we could merge some of these.

Roughly in order of "includes more information":

 - A Width (cmm/CmmType) is simply a binary value with the specified
   number of bits.  It may represent a signed or unsigned integer, a
   floating-point value, or an address.

    data Width = W8 | W16 | W32 | W64 | W80 | W128

 - Size, which is used in the native code generator, is Width +
   floating point information.

   data Size = II8 | II16 | II32 | II64 | FF32 | FF64 | FF80

   it is necessary because e.g. the instruction to move a 64-bit float
   on x86 (movsd) is different from the instruction to move a 64-bit
   integer (movq), so the mov instruction is parameterised by Size.

 - CmmType wraps Width with more information: GC ptr, float, or
   other value.

    data CmmType = CmmType CmmCat Width

    data CmmCat     -- "Category" (not exported)
       = GcPtrCat   -- GC pointer
       | BitsCat    -- Non-pointer
       | FloatCat   -- Float

   It is important to have GcPtr information in Cmm, since we generate
   info tables containing pointerhood for the GC from this.  As for
   why we have float (and not signed/unsigned) here, see Note [Signed
   vs unsigned].

 - ArgRep makes only the distinctions necessary for the call and
   return conventions of the STG machine.  It is essentially CmmType
   + void.

 - PrimRep makes a few more distinctions than ArgRep: it divides
   non-GC-pointers into signed/unsigned and addresses, information
   that is necessary for passing these values to foreign functions.

There's another tension here: whether the type encodes its size in
bytes, or whether its size depends on the machine word size.  Width
and CmmType have the size built-in, whereas ArgRep and PrimRep do not.

This means to turn an ArgRep/PrimRep into a CmmType requires DynFlags.

On the other hand, CmmType includes some "nonsense" values, such as
CmmType GcPtrCat W32 on a 64-bit machine.
-}

-- | A 'PrimRep' is an abstraction of a type.  It contains information that
-- the code generator needs in order to pass arguments, return results,
-- and store values of this type.
data PrimRep
  = VoidRep
  | PtrRep
  | IntRep        -- ^ Signed, word-sized value
  | WordRep       -- ^ Unsigned, word-sized value
  | Int64Rep      -- ^ Signed, 64 bit value (with 32-bit words only)
  | Word64Rep     -- ^ Unsigned, 64 bit value (with 32-bit words only)
  | AddrRep       -- ^ A pointer, but /not/ to a Haskell value (use 'PtrRep')
  | FloatRep
  | DoubleRep
  -- ETA-specific
  | BoolRep
  | CharRep
  | ByteRep
  | ShortRep
  | ObjectRep Text
  | ArrayRep  PrimRep
--  | VecRep Int PrimElemRep  -- ^ A vector
  deriving( Eq, Show )

-- data PrimElemRep
--   = Int8ElemRep
--   | Int16ElemRep
--   | Int32ElemRep
--   | Int64ElemRep
--   | Word8ElemRep
--   | Word16ElemRep
--   | Word32ElemRep
--   | Word64ElemRep
--   | FloatElemRep
--   | DoubleElemRep
--    deriving( Eq, Show )

instance Outputable PrimRep where
  ppr r = text (show r)

-- instance Outputable PrimElemRep where
--   ppr r = text (show r)

isVoidRep :: PrimRep -> Bool
isVoidRep VoidRep = True
isVoidRep _       = False

isGcPtrRep :: PrimRep -> Bool
isGcPtrRep PtrRep = True
isGcPtrRep _      = False

isObjectRep :: PrimRep -> Bool
isObjectRep (ObjectRep _) = True
isObjectRep (ArrayRep  _) = True
isObjectRep _             = False

getObjectClass :: PrimRep -> Text
getObjectClass (ObjectRep t)   = t
getObjectClass (ArrayRep  _) = error $ "getObjectClass: Array type"
  -- arrayWrap . objectWrap $ getObjectClass rep
getObjectClass rep             = error $ "getObjectClass: " ++ show rep

-- | Find the size of a 'PrimRep', in words
primRepSizeW :: DynFlags -> PrimRep -> Int
primRepSizeW _      IntRep        = 1
primRepSizeW _      WordRep       = 1
primRepSizeW _      Int64Rep      = 2
primRepSizeW _      Word64Rep     = 2
primRepSizeW _      FloatRep      = 1    -- NB. might not take a full word
primRepSizeW _      DoubleRep     = 2
primRepSizeW _      AddrRep       = 1
primRepSizeW _      PtrRep        = 1
primRepSizeW _      VoidRep       = 0
primRepSizeW _      BoolRep       = 1
primRepSizeW _      CharRep       = 1
primRepSizeW _      ByteRep       = 1
primRepSizeW _      ShortRep      = 1
primRepSizeW _      (ObjectRep _) = 1
primRepSizeW _ _ = error $ "primRepSizeW: bad primRepSizeW"
-- primRepSizeW dflags (VecRep len rep) = len * primElemRepSizeB rep `quot` 4

-- primElemRepSizeB :: PrimElemRep -> Int
-- primElemRepSizeB Int8ElemRep   = 1
-- primElemRepSizeB Int16ElemRep  = 2
-- primElemRepSizeB Int32ElemRep  = 4
-- primElemRepSizeB Int64ElemRep  = 8
-- primElemRepSizeB Word8ElemRep  = 1
-- primElemRepSizeB Word16ElemRep = 2
-- primElemRepSizeB Word32ElemRep = 4
-- primElemRepSizeB Word64ElemRep = 8
-- primElemRepSizeB FloatElemRep  = 4
-- primElemRepSizeB DoubleElemRep = 8

{-
************************************************************************
*                                                                      *
\subsection{TyCon Construction}
*                                                                      *
************************************************************************

Note: the TyCon constructors all take a Kind as one argument, even though
they could, in principle, work out their Kind from their other arguments.
But to do so they need functions from Types, and that makes a nasty
module mutual-recursion.  And they aren't called from many places.
So we compromise, and move their Kind calculation to the call site.
-}

-- | Given the name of the function type constructor and it's kind, create the
-- corresponding 'TyCon'. It is recommended to use 'TypeRep.funTyCon' if you want
-- this functionality
mkFunTyCon :: Name -> Kind -> TyCon
mkFunTyCon name kind
  = FunTyCon {
        tyConUnique = nameUnique name,
        tyConName   = name,
        tyConKind   = kind,
        tyConArity  = 2
    }

-- | This is the making of an algebraic 'TyCon'. Notably, you have to
-- pass in the generic (in the -XGenerics sense) information about the
-- type constructor - you can get hold of it easily (see Generics
-- module)
mkAlgTyCon :: Name
           -> Kind              -- ^ Kind of the resulting 'TyCon'
           -> [TyVar]           -- ^ 'TyVar's scoped over: see 'tyConTyVars'.
                                --   Arity is inferred from the length of this
                                --   list
           -> [Role]            -- ^ The roles for each TyVar
           -> Maybe CType       -- ^ The C type this type corresponds to
                                --   when using the CAPI FFI
           -> [PredType]        -- ^ Stupid theta: see 'algTcStupidTheta'
           -> AlgTyConRhs       -- ^ Information about dat aconstructors
           -> TyConParent
           -> RecFlag           -- ^ Is the 'TyCon' recursive?
           -> Bool              -- ^ Was the 'TyCon' declared with GADT syntax?
           -> Maybe TyCon       -- ^ Promoted version
           -> TyCon
mkAlgTyCon name kind tyvars roles cType stupid rhs parent is_rec gadt_syn prom_tc
  = AlgTyCon {
        tyConName        = name,
        tyConUnique      = nameUnique name,
        tyConKind        = kind,
        tyConArity       = length tyvars,
        tyConTyVars      = tyvars,
        tcRoles          = roles,
        tyConCType       = cType,
        algTcStupidTheta = stupid,
        algTcRhs         = rhs,
        algTcParent      = ASSERT2( okParent name parent, ppr name $$ ppr parent ) parent,
        algTcRec         = is_rec,
        algTcGadtSyntax  = gadt_syn,
        tcPromoted       = prom_tc
    }

-- | Simpler specialization of 'mkAlgTyCon' for classes
mkClassTyCon :: Name -> Kind -> [TyVar] -> [Role] -> AlgTyConRhs -> Class
             -> RecFlag -> TyCon
mkClassTyCon name kind tyvars roles rhs clas is_rec
  = mkAlgTyCon name kind tyvars roles Nothing [] rhs (ClassTyCon clas)
               is_rec False
               Nothing    -- Class TyCons are not promoted

mkTupleTyCon :: Name
             -> Kind    -- ^ Kind of the resulting 'TyCon'
             -> Arity   -- ^ Arity of the tuple
             -> [TyVar] -- ^ 'TyVar's scoped over: see 'tyConTyVars'
             -> DataCon
             -> TupleSort    -- ^ Whether the tuple is boxed or unboxed
             -> Maybe TyCon  -- ^ Promoted version
             -> TyCon
mkTupleTyCon name kind arity tyvars con sort prom_tc
  = TupleTyCon {
        tyConUnique = nameUnique name,
        tyConName = name,
        tyConKind = kind,
        tyConArity = arity,
        tyConTupleSort = sort,
        tyConTyVars = tyvars,
        dataCon = con,
        tcPromoted = prom_tc
    }

-- | Create an unlifted primitive 'TyCon', such as @Int#@
mkPrimTyCon :: Name  -> Kind -> [Role] -> PrimRep -> TyCon
mkPrimTyCon name kind roles rep
  = mkPrimTyCon' name kind roles rep True

-- | Kind constructors
mkKindTyCon :: Name -> Kind -> TyCon
mkKindTyCon name kind
  = mkPrimTyCon' name kind [] VoidRep True

-- | Create a lifted primitive 'TyCon' such as @RealWorld@
mkLiftedPrimTyCon :: Name  -> Kind -> [Role] -> PrimRep -> TyCon
mkLiftedPrimTyCon name kind roles rep
  = mkPrimTyCon' name kind roles rep False

mkPrimTyCon' :: Name  -> Kind -> [Role] -> PrimRep -> Bool -> TyCon
mkPrimTyCon' name kind roles rep is_unlifted
  = PrimTyCon {
        tyConName    = name,
        tyConUnique  = nameUnique name,
        tyConKind    = kind,
        tyConArity   = length roles,
        tcRoles      = roles,
        primTyConRep = rep,
        isUnLifted   = is_unlifted
    }

-- | Create a type synonym 'TyCon'
mkSynonymTyCon :: Name -> Kind -> [TyVar] -> [Role] -> Type -> TyCon
mkSynonymTyCon name kind tyvars roles rhs
  = SynonymTyCon {
        tyConName   = name,
        tyConUnique = nameUnique name,
        tyConKind   = kind,
        tyConArity  = length tyvars,
        tyConTyVars = tyvars,
        tcRoles     = roles,
        synTcRhs    = rhs
    }

-- | Create a type family 'TyCon'
mkFamilyTyCon:: Name -> Kind -> [TyVar] -> FamTyConFlav -> TyConParent
             -> TyCon
mkFamilyTyCon name kind tyvars flav parent
  = FamilyTyCon
      { tyConUnique = nameUnique name
      , tyConName   = name
      , tyConKind   = kind
      , tyConArity  = length tyvars
      , tyConTyVars = tyvars
      , famTcFlav   = flav
      , famTcParent = parent
      }


-- | Create a promoted data constructor 'TyCon'
-- Somewhat dodgily, we give it the same Name
-- as the data constructor itself; when we pretty-print
-- the TyCon we add a quote; see the Outputable TyCon instance
mkPromotedDataCon :: DataCon -> Name -> Unique -> Kind -> [Role] -> TyCon
mkPromotedDataCon con name unique kind roles
  = PromotedDataCon {
        tyConName   = name,
        tyConUnique = unique,
        tyConArity  = arity,
        tcRoles     = roles,
        tyConKind   = kind,
        dataCon     = con
  }
  where
    arity = length roles

-- | Create a promoted type constructor 'TyCon'
-- Somewhat dodgily, we give it the same Name
-- as the type constructor itself
mkPromotedTyCon :: TyCon -> Kind -> TyCon
mkPromotedTyCon tc kind
  = PromotedTyCon {
        tyConName   = getName tc,
        tyConUnique = getUnique tc,
        tyConArity  = tyConArity tc,
        tyConKind   = kind,
        ty_con      = tc
  }

isFunTyCon :: TyCon -> Bool
isFunTyCon (FunTyCon {}) = True
isFunTyCon _             = False

-- | Test if the 'TyCon' is algebraic but abstract (invisible data constructors)
isAbstractTyCon :: TyCon -> Bool
isAbstractTyCon (AlgTyCon { algTcRhs = AbstractTyCon {} }) = True
isAbstractTyCon _ = False

-- | Make an algebraic 'TyCon' abstract. Panics if the supplied 'TyCon' is not
-- algebraic
makeTyConAbstract :: TyCon -> TyCon
makeTyConAbstract tc@(AlgTyCon { algTcRhs = rhs })
  = tc { algTcRhs = AbstractTyCon (isDistinctAlgRhs rhs) }
makeTyConAbstract tc = pprPanic "makeTyConAbstract" (ppr tc)

-- | Does this 'TyCon' represent something that cannot be defined in Haskell?
isPrimTyCon :: TyCon -> Bool
isPrimTyCon (PrimTyCon {}) = True
isPrimTyCon _              = False

-- | Is this 'TyCon' unlifted (i.e. cannot contain bottom)? Note that this can
-- only be true for primitive and unboxed-tuple 'TyCon's
isUnLiftedTyCon :: TyCon -> Bool
isUnLiftedTyCon (PrimTyCon  {isUnLifted = is_unlifted}) = is_unlifted
isUnLiftedTyCon (TupleTyCon {tyConTupleSort = sort})
    = not (isBoxed (tupleSortBoxity sort))
isUnLiftedTyCon _                                       = False

-- | Returns @True@ if the supplied 'TyCon' resulted from either a
-- @data@ or @newtype@ declaration
isAlgTyCon :: TyCon -> Bool
isAlgTyCon (AlgTyCon {})   = True
isAlgTyCon (TupleTyCon {}) = True
isAlgTyCon _               = False

isDataTyCon :: TyCon -> Bool
-- ^ Returns @True@ for data types that are /definitely/ represented by
-- heap-allocated constructors.  These are scrutinised by Core-level
-- @case@ expressions, and they get info tables allocated for them.
--
-- Generally, the function will be true for all @data@ types and false
-- for @newtype@s, unboxed tuples and type family 'TyCon's. But it is
-- not guaranteed to return @True@ in all cases that it could.
--
-- NB: for a data type family, only the /instance/ 'TyCon's
--     get an info table.  The family declaration 'TyCon' does not
isDataTyCon (AlgTyCon {algTcRhs = rhs})
  = case rhs of
        DataTyCon {}       -> True
        NewTyCon {}        -> False
        DataFamilyTyCon {} -> False
        AbstractTyCon {}   -> False      -- We don't know, so return False
isDataTyCon (TupleTyCon {tyConTupleSort = sort}) = isBoxed (tupleSortBoxity sort)
isDataTyCon _ = False

-- | 'isInjectiveTyCon' is true of 'TyCon's for which this property holds
-- (where X is the role passed in):
--   If (T a1 b1 c1) ~X (T a2 b2 c2), then (a1 ~X1 a2), (b1 ~X2 b2), and (c1 ~X3 c2)
-- (where X1, X2, and X3, are the roles given by tyConRolesX tc X)
-- See also Note [Decomposing equalities] in TcCanonical
isInjectiveTyCon :: TyCon -> Role -> Bool
isInjectiveTyCon _                             Phantom          = False
isInjectiveTyCon (FunTyCon {})                 _                = True
isInjectiveTyCon (AlgTyCon {})                 Nominal          = True
isInjectiveTyCon (AlgTyCon {algTcRhs = rhs})   Representational
  = isGenInjAlgRhs rhs
isInjectiveTyCon (TupleTyCon {})               _                = True
isInjectiveTyCon (SynonymTyCon {})             _                = False
isInjectiveTyCon (FamilyTyCon {})              _                = False
isInjectiveTyCon (PrimTyCon {})                _                = True
isInjectiveTyCon (PromotedDataCon {})          _                = True
isInjectiveTyCon (PromotedTyCon {ty_con = tc}) r
  = isInjectiveTyCon tc r

-- | 'isGenerativeTyCon' is true of 'TyCon's for which this property holds
-- (where X is the role passed in):
--   If (T tys ~X t), then (t's head ~X T).
-- See also Note [Decomposing equalities] in TcCanonical
isGenerativeTyCon :: TyCon -> Role -> Bool
isGenerativeTyCon = isInjectiveTyCon
  -- as it happens, generativity and injectivity coincide, but there's
  -- no a priori reason this must be the case

-- | Is this an 'AlgTyConRhs' of a 'TyCon' that is generative and injective
-- with respect to representational equality?
isGenInjAlgRhs :: AlgTyConRhs -> Bool
isGenInjAlgRhs (DataTyCon {})           = True
isGenInjAlgRhs (DataFamilyTyCon {})     = False
isGenInjAlgRhs (AbstractTyCon distinct) = distinct
isGenInjAlgRhs (NewTyCon {})            = False

-- | 'isDistinctTyCon' is true of 'TyCon's that are equal only to
-- themselves, even via coercions (except for unsafeCoerce).
-- This excludes newtypes, type functions, type synonyms.
-- It relates directly to the FC consistency story:
--     If the axioms are consistent,
--     and  co : S tys ~ T tys, and S,T are "distinct" TyCons,
--     then S=T.
-- Cf Note [Pruning dead case alternatives] in Unify
isDistinctTyCon :: TyCon -> Bool
isDistinctTyCon (AlgTyCon {algTcRhs = rhs}) = isDistinctAlgRhs rhs
isDistinctTyCon (FunTyCon {})               = True
isDistinctTyCon (TupleTyCon {})             = True
isDistinctTyCon (PrimTyCon {})              = True
isDistinctTyCon (PromotedDataCon {})        = True
isDistinctTyCon _                           = False

isDistinctAlgRhs :: AlgTyConRhs -> Bool
isDistinctAlgRhs (DataTyCon {})           = True
isDistinctAlgRhs (DataFamilyTyCon {})     = False
isDistinctAlgRhs (AbstractTyCon distinct) = distinct
isDistinctAlgRhs (NewTyCon {})            = False

-- | Is this 'TyCon' that for a @newtype@
isNewTyCon :: TyCon -> Bool
isNewTyCon (AlgTyCon {algTcRhs = NewTyCon {}}) = True
isNewTyCon _                                   = False

-- | Take a 'TyCon' apart into the 'TyVar's it scopes over, the 'Type' it expands
-- into, and (possibly) a coercion from the representation type to the @newtype@.
-- Returns @Nothing@ if this is not possible.
unwrapNewTyCon_maybe :: TyCon -> Maybe ([TyVar], Type, CoAxiom Unbranched)
unwrapNewTyCon_maybe (AlgTyCon { tyConTyVars = tvs,
                                 algTcRhs = NewTyCon { nt_co = co,
                                                       nt_rhs = rhs }})
                           = Just (tvs, rhs, co)
unwrapNewTyCon_maybe _     = Nothing

unwrapNewTyConEtad_maybe :: TyCon -> Maybe ([TyVar], Type, CoAxiom Unbranched)
unwrapNewTyConEtad_maybe (AlgTyCon { algTcRhs = NewTyCon { nt_co = co,
                                                           nt_etad_rhs = (tvs,rhs) }})
                           = Just (tvs, rhs, co)
unwrapNewTyConEtad_maybe _ = Nothing

isProductTyCon :: TyCon -> Bool
-- True of datatypes or newtypes that have
--   one, vanilla, data constructor
isProductTyCon tc@(AlgTyCon {}) = case algTcRhs tc of
                                    DataTyCon{ data_cons = [data_con] }
                                                -> isVanillaDataCon data_con
                                    NewTyCon {} -> True
                                    _           -> False
isProductTyCon (TupleTyCon {})  = True
isProductTyCon _                = False


isDataProductTyCon_maybe :: TyCon -> Maybe DataCon
-- True of datatypes (not newtypes) with
--   one, vanilla, data constructor
isDataProductTyCon_maybe (AlgTyCon { algTcRhs = DataTyCon { data_cons = cons } })
  | [con] <- cons         -- Singleton
  , isVanillaDataCon con  -- Vanilla
  = Just con
isDataProductTyCon_maybe (TupleTyCon { dataCon = con })
  = Just con
isDataProductTyCon_maybe _ = Nothing

-- | Is this a 'TyCon' representing a regular H98 type synonym (@type@)?
isTypeSynonymTyCon :: TyCon -> Bool
isTypeSynonymTyCon (SynonymTyCon {}) = True
isTypeSynonymTyCon _                 = False


-- As for newtypes, it is in some contexts important to distinguish between
-- closed synonyms and synonym families, as synonym families have no unique
-- right hand side to which a synonym family application can expand.
--

isDecomposableTyCon :: TyCon -> Bool
-- True iff we can decompose (T a b c) into ((T a b) c)
--   I.e. is it injective?
-- Specifically NOT true of synonyms (open and otherwise)
-- Ultimately we may have injective associated types
-- in which case this test will become more interesting
--
-- It'd be unusual to call isDecomposableTyCon on a regular H98
-- type synonym, because you should probably have expanded it first
-- But regardless, it's not decomposable
isDecomposableTyCon (SynonymTyCon {}) = False
isDecomposableTyCon (FamilyTyCon  {}) = False
isDecomposableTyCon _other            = True

-- | Is this an algebraic 'TyCon' declared with the GADT syntax?
isGadtSyntaxTyCon :: TyCon -> Bool
isGadtSyntaxTyCon (AlgTyCon { algTcGadtSyntax = res }) = res
isGadtSyntaxTyCon _                                    = False

-- | Is this an algebraic 'TyCon' which is just an enumeration of values?
isEnumerationTyCon :: TyCon -> Bool
-- See Note [Enumeration types] in TyCon
isEnumerationTyCon (AlgTyCon {algTcRhs = DataTyCon { is_enum = res }}) = res
isEnumerationTyCon (TupleTyCon {tyConArity = arity}) = arity == 0
isEnumerationTyCon _                                                   = False

-- | Is this a 'TyCon', synonym or otherwise, that defines a family?
isFamilyTyCon :: TyCon -> Bool
isFamilyTyCon (FamilyTyCon {})                           = True
isFamilyTyCon (AlgTyCon {algTcRhs = DataFamilyTyCon {}}) = True
isFamilyTyCon _                                          = False

-- | Is this a 'TyCon', synonym or otherwise, that defines a family with
-- instances?
isOpenFamilyTyCon :: TyCon -> Bool
isOpenFamilyTyCon (FamilyTyCon {famTcFlav = OpenSynFamilyTyCon }) = True
isOpenFamilyTyCon (AlgTyCon    {algTcRhs  = DataFamilyTyCon    }) = True
isOpenFamilyTyCon _                                               = False

-- | Is this a synonym 'TyCon' that can have may have further instances appear?
isTypeFamilyTyCon :: TyCon -> Bool
isTypeFamilyTyCon (FamilyTyCon {}) = True
isTypeFamilyTyCon _                = False

isOpenTypeFamilyTyCon :: TyCon -> Bool
isOpenTypeFamilyTyCon (FamilyTyCon {famTcFlav = OpenSynFamilyTyCon }) = True
isOpenTypeFamilyTyCon _                                               = False

-- leave out abstract closed families here
isClosedSynFamilyTyCon_maybe :: TyCon -> Maybe (CoAxiom Branched)
isClosedSynFamilyTyCon_maybe
  (FamilyTyCon {famTcFlav = ClosedSynFamilyTyCon ax}) = Just ax
isClosedSynFamilyTyCon_maybe _                        = Nothing

isBuiltInSynFamTyCon_maybe :: TyCon -> Maybe BuiltInSynFamily
isBuiltInSynFamTyCon_maybe
  (FamilyTyCon {famTcFlav = BuiltInSynFamTyCon ops }) = Just ops
isBuiltInSynFamTyCon_maybe _                          = Nothing

-- | Is this a synonym 'TyCon' that can have may have further instances appear?
isDataFamilyTyCon :: TyCon -> Bool
isDataFamilyTyCon (AlgTyCon {algTcRhs = DataFamilyTyCon {}}) = True
isDataFamilyTyCon _ = False

-- | Are we able to extract information 'TyVar' to class argument list
-- mapping from a given 'TyCon'?
isTyConAssoc :: TyCon -> Bool
isTyConAssoc tc = isJust (tyConAssoc_maybe tc)

tyConAssoc_maybe :: TyCon -> Maybe Class
tyConAssoc_maybe tc = case tyConParent tc of
                        AssocFamilyTyCon cls -> Just cls
                        _                    -> Nothing

-- The unit tycon didn't used to be classed as a tuple tycon
-- but I thought that was silly so I've undone it
-- If it can't be for some reason, it should be a AlgTyCon
isTupleTyCon :: TyCon -> Bool
-- ^ Does this 'TyCon' represent a tuple?
--
-- NB: when compiling @Data.Tuple@, the tycons won't reply @True@ to
-- 'isTupleTyCon', because they are built as 'AlgTyCons'.  However they
-- get spat into the interface file as tuple tycons, so I don't think
-- it matters.
isTupleTyCon (TupleTyCon {}) = True
isTupleTyCon _               = False

-- | Is this the 'TyCon' for an unboxed tuple?
isUnboxedTupleTyCon :: TyCon -> Bool
isUnboxedTupleTyCon (TupleTyCon {tyConTupleSort = sort}) =
    not (isBoxed (tupleSortBoxity sort))
isUnboxedTupleTyCon _ = False

-- | Is this the 'TyCon' for a /promoted/ tuple?
isPromotedTupleTyCon :: TyCon -> Bool
isPromotedTupleTyCon tyCon
  | Just dataCon <- isPromotedDataCon_maybe tyCon
  , isTupleTyCon (dataConTyCon dataCon) = True
  | otherwise = False

-- | Is this the 'TyCon' for a boxed tuple?
isBoxedTupleTyCon :: TyCon -> Bool
isBoxedTupleTyCon (TupleTyCon {tyConTupleSort = sort}) = isBoxed (tupleSortBoxity sort)
isBoxedTupleTyCon _  = False

-- | Extract the boxity of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConBoxity :: TyCon -> Boxity
tupleTyConBoxity tc = tupleSortBoxity (tyConTupleSort tc)

-- | Extract the 'TupleSort' of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConSort :: TyCon -> TupleSort
tupleTyConSort tc = tyConTupleSort tc

-- | Extract the arity of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConArity :: TyCon -> Arity
tupleTyConArity tc = tyConArity tc

-- | Is this a recursive 'TyCon'?
isRecursiveTyCon :: TyCon -> Bool
isRecursiveTyCon (AlgTyCon {algTcRec = Recursive}) = True
isRecursiveTyCon _                                 = False

promotableTyCon_maybe :: TyCon -> Maybe TyCon
promotableTyCon_maybe (AlgTyCon { tcPromoted = prom })   = prom
promotableTyCon_maybe (TupleTyCon { tcPromoted = prom }) = prom
promotableTyCon_maybe _                                  = Nothing

promoteTyCon :: TyCon -> TyCon
promoteTyCon tc = case promotableTyCon_maybe tc of
                    Just prom_tc -> prom_tc
                    Nothing      -> pprPanic "promoteTyCon" (ppr tc)

-- | Is this a PromotedTyCon?
isPromotedTyCon :: TyCon -> Bool
isPromotedTyCon (PromotedTyCon {}) = True
isPromotedTyCon _                  = False

-- | Retrieves the promoted TyCon if this is a PromotedTyCon;
isPromotedTyCon_maybe :: TyCon -> Maybe TyCon
isPromotedTyCon_maybe (PromotedTyCon { ty_con = tc }) = Just tc
isPromotedTyCon_maybe _ = Nothing

-- | Is this a PromotedDataCon?
isPromotedDataCon :: TyCon -> Bool
isPromotedDataCon (PromotedDataCon {}) = True
isPromotedDataCon _                    = False

-- | Retrieves the promoted DataCon if this is a PromotedDataCon;
isPromotedDataCon_maybe :: TyCon -> Maybe DataCon
isPromotedDataCon_maybe (PromotedDataCon { dataCon = dc }) = Just dc
isPromotedDataCon_maybe _ = Nothing

isLiftedTypeKindTyConName :: Name -> Bool
isLiftedTypeKindTyConName n = n `hasKey` liftedTypeKindTyConKey

-- | Identifies implicit tycons that, in particular, do not go into interface
-- files (because they are implicitly reconstructed when the interface is
-- read).
--
-- Note that:
--
-- * Associated families are implicit, as they are re-constructed from
--   the class declaration in which they reside, and
--
-- * Family instances are /not/ implicit as they represent the instance body
--   (similar to a @dfun@ does that for a class instance).
isImplicitTyCon :: TyCon -> Bool
isImplicitTyCon (FunTyCon {})        = True
isImplicitTyCon (TupleTyCon {})      = True
isImplicitTyCon (PrimTyCon {})       = True
isImplicitTyCon (PromotedDataCon {}) = True
isImplicitTyCon (PromotedTyCon {})   = True
isImplicitTyCon (AlgTyCon { algTcParent = AssocFamilyTyCon {} })    = True
isImplicitTyCon (AlgTyCon {})                                       = False
isImplicitTyCon (FamilyTyCon { famTcParent = AssocFamilyTyCon {} }) = True
isImplicitTyCon (FamilyTyCon {})                                    = False
isImplicitTyCon (SynonymTyCon {})                                   = False

tyConCType_maybe :: TyCon -> Maybe CType
tyConCType_maybe tc@(AlgTyCon {}) = tyConCType tc
tyConCType_maybe _ = Nothing

isObjectTyCon :: TyCon -> Bool
isObjectTyCon = (`hasKey` jobjectPrimTyConKey)

{-
-----------------------------------------------
--      Expand type-constructor applications
-----------------------------------------------
-}

tcExpandTyCon_maybe, coreExpandTyCon_maybe
        :: TyCon
        -> [tyco]                 -- ^ Arguments to 'TyCon'
        -> Maybe ([(TyVar,tyco)],
                  Type,
                  [tyco])         -- ^ Returns a 'TyVar' substitution, the body
                                  -- type of the synonym (not yet substituted)
                                  -- and any arguments remaining from the
                                  -- application

-- ^ Used to create the view the /typechecker/ has on 'TyCon's.
-- We expand (closed) synonyms only, cf. 'coreExpandTyCon_maybe'
tcExpandTyCon_maybe (SynonymTyCon { tyConTyVars = tvs
                                  , synTcRhs    = rhs }) tys
   = expand tvs rhs tys
tcExpandTyCon_maybe _ _ = Nothing

---------------

-- ^ Used to create the view /Core/ has on 'TyCon's. We expand
-- not only closed synonyms like 'tcExpandTyCon_maybe',
-- but also non-recursive @newtype@s
coreExpandTyCon_maybe tycon tys = tcExpandTyCon_maybe tycon tys


----------------
expand  :: [TyVar] -> Type                 -- Template
        -> [a]                             -- Args
        -> Maybe ([(TyVar,a)], Type, [a])  -- Expansion
expand tvs rhs tys
  = case n_tvs `compare` length tys of
        LT -> Just (tvs `zip` tys, rhs, drop n_tvs tys)
        EQ -> Just (tvs `zip` tys, rhs, [])
        GT -> Nothing
   where
     n_tvs = length tvs

-- | As 'tyConDataCons_maybe', but returns the empty list of constructors if no
-- constructors could be found
tyConDataCons :: TyCon -> [DataCon]
-- It's convenient for tyConDataCons to return the
-- empty list for type synonyms etc
tyConDataCons tycon = tyConDataCons_maybe tycon `orElse` []

-- | Determine the 'DataCon's originating from the given 'TyCon', if the 'TyCon'
-- is the sort that can have any constructors (note: this does not include
-- abstract algebraic types)
tyConDataCons_maybe :: TyCon -> Maybe [DataCon]
tyConDataCons_maybe (AlgTyCon {algTcRhs = DataTyCon { data_cons = cons }})
    = Just cons
tyConDataCons_maybe (AlgTyCon {algTcRhs = NewTyCon { data_con = con }})
    = Just [con]
tyConDataCons_maybe (TupleTyCon {dataCon = con})
    = Just [con]
tyConDataCons_maybe _
    = Nothing

-- | Determine the number of value constructors a 'TyCon' has. Panics if the
-- 'TyCon' is not algebraic or a tuple
tyConFamilySize  :: TyCon -> Int
tyConFamilySize (AlgTyCon   {algTcRhs = DataTyCon {data_cons = cons}}) =
  length cons
tyConFamilySize (AlgTyCon   {algTcRhs = NewTyCon {}})        = 1
tyConFamilySize (AlgTyCon   {algTcRhs = DataFamilyTyCon {}}) = 0
tyConFamilySize (TupleTyCon {})                              = 1
tyConFamilySize other = pprPanic "tyConFamilySize:" (ppr other)

-- | Extract an 'AlgTyConRhs' with information about data constructors from an
-- algebraic or tuple 'TyCon'. Panics for any other sort of 'TyCon'
algTyConRhs :: TyCon -> AlgTyConRhs
algTyConRhs (AlgTyCon {algTcRhs = rhs}) = rhs
algTyConRhs (TupleTyCon {dataCon = con, tyConArity = arity})
    = DataTyCon { data_cons = [con], is_enum = arity == 0 }
algTyConRhs other = pprPanic "algTyConRhs" (ppr other)

-- | Get the list of roles for the type parameters of a TyCon
tyConRoles :: TyCon -> [Role]
-- See also Note [TyCon Role signatures]
tyConRoles tc
  = case tc of
    { FunTyCon {}                         -> const_role Representational
    ; AlgTyCon { tcRoles = roles }        -> roles
    ; TupleTyCon {}                       -> const_role Representational
    ; SynonymTyCon { tcRoles = roles }    -> roles
    ; FamilyTyCon {}                      -> const_role Nominal
    ; PrimTyCon { tcRoles = roles }       -> roles
    ; PromotedDataCon { tcRoles = roles } -> roles
    ; PromotedTyCon {}                    -> const_role Nominal
    }
  where
    const_role r = replicate (tyConArity tc) r

-- | Extract the bound type variables and type expansion of a type synonym
-- 'TyCon'. Panics if the 'TyCon' is not a synonym
newTyConRhs :: TyCon -> ([TyVar], Type)
newTyConRhs (AlgTyCon {tyConTyVars = tvs, algTcRhs = NewTyCon { nt_rhs = rhs }})
    = (tvs, rhs)
newTyConRhs tycon = pprPanic "newTyConRhs" (ppr tycon)

-- | The number of type parameters that need to be passed to a newtype to
-- resolve it. May be less than in the definition if it can be eta-contracted.
newTyConEtadArity :: TyCon -> Int
newTyConEtadArity (AlgTyCon {algTcRhs = NewTyCon { nt_etad_rhs = tvs_rhs }})
        = length (fst tvs_rhs)
newTyConEtadArity tycon = pprPanic "newTyConEtadArity" (ppr tycon)

-- | Extract the bound type variables and type expansion of an eta-contracted
-- type synonym 'TyCon'.  Panics if the 'TyCon' is not a synonym
newTyConEtadRhs :: TyCon -> ([TyVar], Type)
newTyConEtadRhs (AlgTyCon {algTcRhs = NewTyCon { nt_etad_rhs = tvs_rhs }}) = tvs_rhs
newTyConEtadRhs tycon = pprPanic "newTyConEtadRhs" (ppr tycon)

-- | Extracts the @newtype@ coercion from such a 'TyCon', which can be used to
-- construct something with the @newtype@s type from its representation type
-- (right hand side). If the supplied 'TyCon' is not a @newtype@, returns
-- @Nothing@
newTyConCo_maybe :: TyCon -> Maybe (CoAxiom Unbranched)
newTyConCo_maybe (AlgTyCon {algTcRhs = NewTyCon { nt_co = co }}) = Just co
newTyConCo_maybe _                                               = Nothing

newTyConCo :: TyCon -> CoAxiom Unbranched
newTyConCo tc = case newTyConCo_maybe tc of
                 Just co -> co
                 Nothing -> pprPanic "newTyConCo" (ppr tc)

-- | Find the primitive representation of a 'TyCon'
tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep (PrimTyCon {primTyConRep = rep}) = rep
tyConPrimRep tc = ASSERT(not (isUnboxedTupleTyCon tc)) PtrRep

-- | Find the \"stupid theta\" of the 'TyCon'. A \"stupid theta\" is the context
-- to the left of an algebraic type declaration, e.g. @Eq a@ in the declaration
-- @data Eq a => T a ...@
tyConStupidTheta :: TyCon -> [PredType]
tyConStupidTheta (AlgTyCon {algTcStupidTheta = stupid}) = stupid
tyConStupidTheta (TupleTyCon {})                        = []
tyConStupidTheta tycon = pprPanic "tyConStupidTheta" (ppr tycon)

-- | Extract the 'TyVar's bound by a vanilla type synonym
-- and the corresponding (unsubstituted) right hand side.
synTyConDefn_maybe :: TyCon -> Maybe ([TyVar], Type)
synTyConDefn_maybe (SynonymTyCon {tyConTyVars = tyvars, synTcRhs = ty})
  = Just (tyvars, ty)
synTyConDefn_maybe _ = Nothing

-- | Extract the information pertaining to the right hand side of a type synonym
-- (@type@) declaration.
synTyConRhs_maybe :: TyCon -> Maybe Type
synTyConRhs_maybe (SynonymTyCon {synTcRhs = rhs}) = Just rhs
synTyConRhs_maybe _                               = Nothing

-- | Extract the flavour of a type family (with all the extra information that
-- it carries)
famTyConFlav_maybe :: TyCon -> Maybe FamTyConFlav
famTyConFlav_maybe (FamilyTyCon {famTcFlav = flav}) = Just flav
famTyConFlav_maybe _                                = Nothing

-- | If the given 'TyCon' has a /single/ data constructor, i.e. it is a @data@
-- type with one alternative, a tuple type or a @newtype@ then that constructor
-- is returned. If the 'TyCon' has more than one constructor, or represents a
-- primitive or function type constructor then @Nothing@ is returned. In any
-- other case, the function panics
tyConSingleDataCon_maybe :: TyCon -> Maybe DataCon
tyConSingleDataCon_maybe (TupleTyCon {dataCon = c})
    = Just c
tyConSingleDataCon_maybe (AlgTyCon {algTcRhs = DataTyCon { data_cons = [c] }})
    = Just c
tyConSingleDataCon_maybe (AlgTyCon {algTcRhs = NewTyCon { data_con = c }})
    = Just c
tyConSingleDataCon_maybe _
    = Nothing

tyConSingleAlgDataCon_maybe :: TyCon -> Maybe DataCon
-- Returns (Just con) for single-constructor *algebraic* data types
-- *not* newtypes
tyConSingleAlgDataCon_maybe (TupleTyCon {dataCon = c})
    = Just c
tyConSingleAlgDataCon_maybe (AlgTyCon {algTcRhs = DataTyCon { data_cons= [c] }})
    = Just c
tyConSingleAlgDataCon_maybe _
    = Nothing

-- | Is this 'TyCon' that for a class instance?
isClassTyCon :: TyCon -> Bool
isClassTyCon (AlgTyCon {algTcParent = ClassTyCon _}) = True
isClassTyCon _                                       = False

-- | If this 'TyCon' is that for a class instance, return the class it is for.
-- Otherwise returns @Nothing@
tyConClass_maybe :: TyCon -> Maybe Class
tyConClass_maybe (AlgTyCon {algTcParent = ClassTyCon clas}) = Just clas
tyConClass_maybe _                                          = Nothing

tyConTuple_maybe :: TyCon -> Maybe TupleSort
tyConTuple_maybe (TupleTyCon {tyConTupleSort = sort}) = Just sort
tyConTuple_maybe _                                    = Nothing

----------------------------------------------------------------------------
tyConParent :: TyCon -> TyConParent
tyConParent (AlgTyCon    {algTcParent = parent}) = parent
tyConParent (FamilyTyCon {famTcParent = parent}) = parent
tyConParent _                                    = NoParentTyCon

----------------------------------------------------------------------------
-- | Is this 'TyCon' that for a data family instance?
isFamInstTyCon :: TyCon -> Bool
isFamInstTyCon tc = case tyConParent tc of
                      FamInstTyCon {} -> True
                      _               -> False

tyConFamInstSig_maybe :: TyCon -> Maybe (TyCon, [Type], CoAxiom Unbranched)
tyConFamInstSig_maybe tc
  = case tyConParent tc of
      FamInstTyCon ax f ts -> Just (f, ts, ax)
      _                    -> Nothing

-- | If this 'TyCon' is that of a family instance, return the family in question
-- and the instance types. Otherwise, return @Nothing@
tyConFamInst_maybe :: TyCon -> Maybe (TyCon, [Type])
tyConFamInst_maybe tc
  = case tyConParent tc of
      FamInstTyCon _ f ts -> Just (f, ts)
      _                   -> Nothing

-- | If this 'TyCon' is that of a family instance, return a 'TyCon' which
-- represents a coercion identifying the representation type with the type
-- instance family.  Otherwise, return @Nothing@
tyConFamilyCoercion_maybe :: TyCon -> Maybe (CoAxiom Unbranched)
tyConFamilyCoercion_maybe tc
  = case tyConParent tc of
      FamInstTyCon co _ _ -> Just co
      _                   -> Nothing

{-
************************************************************************
*                                                                      *
\subsection[TyCon-instances]{Instance declarations for @TyCon@}
*                                                                      *
************************************************************************

@TyCon@s are compared by comparing their @Unique@s.

The strictness analyser needs @Ord@. It is a lexicographic order with
the property @(a<=b) || (b<=a)@.
-}

instance Eq TyCon where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord TyCon where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = getUnique a `compare` getUnique b

instance Uniquable TyCon where
    getUnique tc = tyConUnique tc

instance Outputable TyCon where
  -- At the moment a promoted TyCon has the same Name as its
  -- corresponding TyCon, so we add the quote to distinguish it here
  ppr tc = pprPromotionQuote tc <> ppr (tyConName tc)

pprPromotionQuote :: TyCon -> SDoc
pprPromotionQuote (PromotedDataCon {}) = char '\''   -- Quote promoted DataCons
                                                     -- in types
pprPromotionQuote (PromotedTyCon {})   = ifPprDebug (char '\'')
pprPromotionQuote _                    = empty -- However, we don't quote TyCons
                                               -- in kinds e.g.
                                               -- type family T a :: Bool -> *
                                               -- cf Trac #5952.
                                               -- Except with -dppr-debug

instance NamedThing TyCon where
    getName = tyConName

instance Data.Data TyCon where
    -- don't traverse?
    toConstr _   = abstractConstr "TyCon"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "TyCon"

{-
************************************************************************
*                                                                      *
           Walking over recursive TyCons
*                                                                      *
************************************************************************

Note [Expanding newtypes and products]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When expanding a type to expose a data-type constructor, we need to be
careful about newtypes, lest we fall into an infinite loop. Here are
the key examples:

  newtype Id  x = MkId x
  newtype Fix f = MkFix (f (Fix f))
  newtype T     = MkT (T -> T)

  Type           Expansion
 --------------------------
  T              T -> T
  Fix Maybe      Maybe (Fix Maybe)
  Id (Id Int)    Int
  Fix Id         NO NO NO

Notice that we can expand T, even though it's recursive.
And we can expand Id (Id Int), even though the Id shows up
twice at the outer level.

So, when expanding, we keep track of when we've seen a recursive
newtype at outermost level; and bale out if we see it again.

We sometimes want to do the same for product types, so that the
strictness analyser doesn't unbox infinitely deeply.

The function that manages this is checkRecTc.
-}

newtype RecTcChecker = RC NameSet

initRecTc :: RecTcChecker
initRecTc = RC emptyNameSet

checkRecTc :: RecTcChecker -> TyCon -> Maybe RecTcChecker
-- Nothing      => Recursion detected
-- Just rec_tcs => Keep going
checkRecTc (RC rec_nts) tc
  | not (isRecursiveTyCon tc)     = Just (RC rec_nts)
  | tc_name `elemNameSet` rec_nts = Nothing
  | otherwise                     = Just (RC (extendNameSet rec_nts tc_name))
  where
    tc_name = tyConName tc
