//! DCLabel
//!
//! Main DCLabel type.  @DCLabel@s use 'CNF' boolean formulas over
//! principals to express authority exercised by a combination of
//! principals.  A @DCLabel@ contains two 'CNF's.  One, 'dcSecrecy',
//! specifies the minimum authority required to make data with the
//! label completely public.  The second, 'dcIntegrity', expresses the
//! minimum authority that was used to endorse data with the label, or,
//! for mutable objects, the minimum authority required to modify the
//! object.
//!
//! @DCLabel@s are more conveniently expressed using the '%%' operator,
//! with 'dcSecrecy' on the left and 'dcIntegrity' on the right, i.e.:
//! @(@/dcSecrecyValue/ '%%' /dcIntegrityValue/@)@.
//!
//! @DCLabel@s enforce the following relations:
//!
//!   * If @cnf1@ and @cnf2@ are 'CNF's describing authority, then
//!   @cnf1 ``speaksFor`` cnf2@ if and only if @cnf1@ logically implies
//!   @cnf2@ (often written @cnf1 &#x27f9; cnf2@).  For example,
//!   @(\"A\" '/\' \"B\") ``speaksFor`` 'toCNF' \"A\"@, while @'toCNF'
//!   \"A\" ``speaksFor`` (\"A\" '\/' \"C\")@.
//!
//!   * Given two @DCLabel@s @dc1 = (s1 '%%' i1)@ and @dc2 = (s2 '%%'
//!   i2)@, @dc1 ``canFlowTo`` dc2@ (often written @dc1@ &#8849; @dc2@)
//!   if and only if @s2 ``speaksFor`` s1 && i1 ``speaksFor`` i2@.  In
//!   other words, data can flow in the direction of requiring more
//!   authority to make it public or removing integrity endorsements.
//!
//!   * Given two @DCLabel@s @dc1 = (s1 '%%' i1)@ and @dc2 = (s2 '%%'
//!   i2)@, and a @p::'CNF'@ representing privileges, @'canFlowToP' p
//!   dc1 dc2@ (often written @dc1@ &#8849;&#8346; @dc2@) if and only
//!   if @(p '/\' s2) ``speaksFor`` s2 && (p '/\' i1) ``speaksFor``
//!   i2@.
use std::fmt;
use std::ops::{BitOr, BitAnd, Rem};
use crate::logic::*;

pub type Priv<'a> = CNF<'a>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct DCLabel<'a> {
    /// Describes the authority required to make
    /// the data public.
    dc_secrecy: CNF<'a>,
    /// Describes the authority with which
    /// immutable data was endorsed, or the
    /// authority required to modify mutable data.
    dc_integrity: CNF<'a>,
}

impl<'a> fmt::Display for DCLabel<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // instance Show DCLabel where
        //   showsPrec d (DCLabel sec int) =
        //     showParen (d > 5) $ shows sec . (" %% " ++) . shows int
        unimplemented!()
    }
}

impl<'a> fmt::Debug for DCLabel<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // instance Read DCLabel where
        //   readPrec = parens $ prec 5 $ do
        //     sec <- readPrec
        //     Symbol "%%" <- lexP
        //     int <- readPrec
        //     return $ DCLabel sec int
        unimplemented!()
    }
}

impl<'a> BitAnd for CNF<'a> {
    type Output = Self;

    /// Compute a conjunction of two 'CNF's or 'ToCNF' instances.
    fn bitand(self, rhs: Self) -> Self::Output {
        self.union(rhs)
    }
}

impl<'a> BitOr for CNF<'a> {
    type Output = Self;

    /// Compute a disjunction of two 'CNF's or 'ToCNF' instances.  Note
    /// that this can be an expensive operation if the inputs have many
    /// conjunctions.
    fn bitor(self, rhs: Self) -> Self::Output {
        self.or(rhs)
    }
}

impl<'a> DCLabel<'a> {
    /// dcPublic = True %% True
    ///
    /// This label corresponds to public data with no integrity guarantees.
    /// For instance, an unrestricted Internet socket should be labeled
    /// @dcPublic@.  The significance of @dcPublic@ is that given data
    /// labeled @(s %% i)@, @s@ is the exact minimum authority such that
    /// @(s %% i) &#x2291;&#x209b; dcPublic@, while @i@ is the exact
    /// minimum authority such that @dcPublic &#x2291;&#x1d62; (s %% i)@.
    pub fn public() -> Self {
        // dcPublic = True %% True
        unimplemented!()
    }

    /// The primary way of creating a 'DCLabel'.  The secrecy component
    /// goes on the left, while the integrity component goes on the right,
    /// e.g.:
    ///
    /// > label = secrecyCNF %% integrityCNF
    ///
    /// Unlike the 'DCLabel' constructor, the arguments can be any instance
    /// of 'ToCNF'.  @%%@ has fixity:
    ///
    /// > infix 6 %%
    pub fn new<T: Into<CNF<'a>>, U: Into<CNF<'a>>>(secrecy: T, integrity: U) -> Self {
        // (%%) :: (ToCNF a, ToCNF b) => a -> b -> DCLabel
        // a %% b = toCNF a `DCLabel` toCNF b
        // infix 6 %%
        unimplemented!()
    }

    pub fn label(&self) {
        // instance Label DCLabel where
        //   lub (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cUnion s1 s2) (cOr i1 i2)
        //   glb (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cOr s1 s2) (cUnion i1 i2)
        //   canFlowTo (DCLabel s1 i1) (DCLabel s2 i2) = cImplies s2 s1 && cImplies i1 i2
        unimplemented!()
    }
}

// instance SpeaksFor CNF where
//   {-# INLINE speaksFor #-}
//   speaksFor = cImplies

impl<'a> CNF<'a> {
    pub fn dc_max_downgrade(label: &DCLabel<'a>) -> DCLabel<'a> {
        // dcMaxDowngrade p (DCLabel (CNF ds) int) = DCLabel sec (cUnion p int)
        //   where sec = CNF $ Set.filter (not . cImplies1 p) ds
        unimplemented!()
    }
}

// instance PrivDesc DCLabel CNF where
//   downgradeP = dcMaxDowngrade
//   canFlowToP p (DCLabel s1 i1) (DCLabel s2 i2) =
//     cImplies (cUnion p s2) s1 && cImplies (cUnion p i1) i2

// ///
// /// Type aliases
// ///

// /// | A common default starting state, where @'lioLabel' = 'dcPublic'@
// /// and @'lioClearance' = False '%%' True@ (i.e., the highest
// /// possible clearance).
// dcDefaultState :: LIOState DCLabel
// dcDefaultState = LIOState { lioLabel = dcPublic
//                           , lioClearance = False %% True }

// /// | The main monad type alias to use for 'LIO' computations that are
// /// specific to 'DCLabel's.
// type DC = LIO DCLabel

// /// | 'DCLabel' privileges are expressed as a 'CNF' of the principals
// /// whose authority is being exercised.
// type DCPriv = Priv CNF

// /// | An alias for 'Labeled' values labeled with a 'DCLabel'.
// type DCLabeled = Labeled DCLabel

// /// | Wrapper function for running @'LIO' 'DCLabel'@ computations.
// ///
// /// @
// /// evalDC dc = 'evalLIO' dc 'dcDefaultState'
// /// @
// evalDC :: DC a -> IO a
// evalDC dc = evalLIO dc dcDefaultState

// /// | 'DCLabel' wrapper for 'tryLIO':
// ///
// /// @
// /// tryDC dc = 'tryLIO' dc 'dcDefaultState'
// /// @
// tryDC :: DC a -> IO (Either SomeException a, LIOState DCLabel)
// tryDC dc = tryLIO dc dcDefaultState

#[cfg(test)]
mod test {
    use super::*;
}
