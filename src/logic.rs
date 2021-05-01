use std::fmt;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub type SetTag = u64;
pub type ByteString = [u8];

/// A @Principal@ is a primitive source of authority, represented as
/// a string.  The interpretation of principal strings is up to the
/// application.  Reasonable schemes include encoding user names,
/// domain names, and/or URLs in the 'Principal' type.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Principal<'a> {
    name: &'a [u8],
    tag: SetTag,
}

impl<'a> Into<Principal<'a>> for &'a str {
    /// Create a principal from a 'String'.  The 'String' is packed into
    /// a 'S.ByteString' using 'fromString', which will almost certainly
    /// give unexpected results for non-ASCII unicode code points.
    fn into(self) -> Principal<'a> {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hv = hasher.finish();
        // TODO: verify bloom filter calculation
        let bloom = (hv & 0x3f) | ((hv >> 6) & 0x3f) | ((hv >> 12) & 0x3f);
        // let bloom = bit (hv .&. 0x3f)
        //         .|. (bit $ shiftR hv 6 .&. 0x3f)
        //         .|. (bit $ shiftR hv 12 .&. 0x3f);
        Principal {
            name: self.as_bytes(),
            tag: bloom,
        }
    }
}

impl<'a> Into<&'a ByteString> for Principal<'a> {
    /// Extract the name of a principal as a strict 'S.ByteString'.
    /// (Use 'show' to get it as a regular 'String'.)
    fn into(self) -> &'a ByteString {
        unimplemented!()
    }
}

impl<'a> Into<Principal<'a>> for &'a ByteString {
    /// Create a principal from a strict 'S.ByteString'.
    fn into(self) -> Principal<'a> {
        unimplemented!()
    }
}

impl<'a> fmt::Display for Principal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(f, "{}", self.name)
        unimplemented!()
    }
}

impl<'a> fmt::Debug for Principal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // readsPrec d s = do
        //   (name, rest) <- readsPrec d s
        //   return (principalBS name, rest)
        // write!(f, "{}", self.name)
        unimplemented!()
    }
}

/// Represents a disjunction of 'Principal's, or one clause of a
/// 'CNF'.  There is generally not much need to work directly with
/// @Disjunction@s unless you need to serialize and de-serialize them
/// (by means of 'dToSet' and 'dFromList').
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Disjunction<'a> {
    ps: Vec<Principal<'a>>,
    tag: SetTag,
}

impl<'a> Disjunction<'a> {
    pub fn as_false() -> Self {
        // dFalse = Disjunction Set.empty 0
        unimplemented!()
    }

    pub fn singleton(p: Principal) -> Self {
        // dSingleton p@(Principal _ t) = Disjunction (Set.singleton p) t
        unimplemented!()
    }

    pub fn union(self, other: Disjunction<'a>) -> Disjunction<'a> {
        // dUnion (Disjunction ps1 t1) (Disjunction ps2 t2) =
        //   Disjunction (Set.union ps1 ps2) (t1 .|. t2)
        unimplemented!()
    }

    pub fn implies(&self, other: &Disjunction<'a>) -> bool {
        // dImplies (Disjunction ps1 t1) (Disjunction ps2 t2)
        //   | t1 .&. t2 /= t1 = False
        //   | otherwise       = ps1 `Set.isSubsetOf` ps2
        unimplemented!()
    }

    /// Expose the set of 'Principal's being ORed together in a
    /// 'Disjunction'.
    pub fn to_set(&self) -> &Vec<Principal<'a>> {
        &self.ps
    }

    pub fn insert(&self, cnf: CNF<'a>) -> CNF<'a> {
        // cInsert :: Disjunction -> CNF -> CNF
        // cInsert dnew c@(CNF ds)
        //   | setAny (`dImplies` dnew) ds = c
        //   | otherwise = CNF $ Set.insert dnew $ Set.filter (not . (dnew `dImplies`)) ds
        unimplemented!()
    }
}

impl<'a> fmt::Display for Disjunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // showsPrec _ (Disjunction ps _)
        //   | Set.size ps == 0 = ("False" ++)
        //   | Set.size ps == 1 = shows $ Set.findMin ps
        //   | otherwise = showParen True $
        //       foldr1 (\l r -> l . (" \\/ " ++) . r) $ map shows $ Set.toList ps
        unimplemented!()
    }
}

impl<'a> fmt::Debug for Disjunction<'a> {
    /// Note that a disjunction containing more than one element /must/
    /// be surrounded by parentheses to parse correctly.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // instance Read Disjunction where
        //   readPrec = false <++ clause <++ single
        //     where false = do False <- readPrec; return dFalse
        //           single = dSingleton <$> readPrec
        //           clause = parens $ prec minPrec $ do
        //             let next = do Symbol "\\/" <- lexP
        //                           next'
        //                        <++ return []
        //                 next' = ((:) <$> readPrec) `ap` next
        //             dFromList <$> next'
        unimplemented!()
    }

}

impl<'a> Into<Disjunction<'a>> for Vec<Principal<'a>> {
    /// Convert a list of 'Principal's into a 'Disjunction'.
    fn into(self) -> Disjunction<'a> {
        unimplemented!()
    }
}

/// A boolean formula in Conjunctive Normal Form.  @CNF@ is used to
/// describe 'DCLabel' privileges, as well to provide each of the two
/// halves of a 'DCLabel'.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CNF<'a> {
    ds: Vec<Disjunction<'a>>,
}

impl<'a> CNF<'a> {
    pub fn new(ds: Vec<Disjunction<'a>>) -> Self {
        Self { ds }
    }

    /// A 'CNF' that is always @True@--i.e., trivially satisfiable.  When
    /// @'dcSecrecy' = cTrue@, it means data is public.  When
    /// @'dcIntegrity' = cTrue@, it means data carries no integrity
    /// guarantees.  As a description of privileges, @cTrue@ conveys no
    /// privileges; @'canFlowToP' cTrue l1 l2@ is equivalent to
    /// @'canFlowTo' l1 l2@.
    ///
    /// Note that @'toCNF' 'True' = cTrue@.  Hence @'dcPublic' = 'DCLabel'
    /// cTrue cTrue@.
    pub fn as_true() -> Self {
        Self::new(vec![])
    }

    /// A 'CNF' that is always @False@.  If @'dcSecrecy' = cFalse@, then
    /// no combination of principals is powerful enough to make the data
    /// public.  For that reason, @cFalse@ generally shouldn't appear in a
    /// data label.  However, it is convenient to include as the
    /// 'dcSecrecy' component of 'lioClearance' to indicate a thread may
    /// arbitrarily raise its label.
    ///
    /// @'dcIntegrity' = cFalse@ indicates impossibly much integrity--i.e.,
    /// data that no combination of principals is powerful enough to modify
    /// or have created.  Generally this is not a useful concept.
    ///
    /// As a privilege description, @cFalse@ indicates impossibly high
    /// privileges (i.e., higher than could be achieved through any
    /// combination of 'Principal's).  @cFalse ``speaksFor`` p@ for any
    /// 'CNF' @p@.  This can be a useful concept for bootstrapping
    /// privileges within the 'DC' monad itself.  For instance, the result
    /// of @'privInit' cFalse@ can be passed to fully-trusted 'DC' code,
    /// which can in turn use 'delegate' to create arbitrary finite
    /// privileges to pass to less privileged code.
    pub fn as_false() -> Self {
        // cFalse = CNF $ Set.singleton dFalse
        unimplemented!()
    }

    pub fn singleton(d: Disjunction<'a>) -> Self {
        // cSingleton = CNF . Set.singleton
        unimplemented!()
    }

    pub fn set_any() {
        // setAny :: (a -> Bool) -> Set a -> Bool
        // setAny prd = Set.foldr' (\a -> (prd a ||)) False
        unimplemented!()
    }

    pub fn set_all() {
        // setAll :: (a -> Bool) -> Set a -> Bool
        // setAll prd = Set.foldr' (\a -> (prd a &&)) True
        unimplemented!()
    }

    pub fn union(self, other: CNF<'a>) -> CNF<'a> {
        // cUnion c (CNF ds) = Set.foldr cInsert c ds
        unimplemented!()
    }

    pub fn or(self, other: CNF<'a>) -> CNF<'a> {
        // cOr (CNF ds1) (CNF ds2) =
        //   cFromList $ [dUnion d1 d2 | d1 <- Set.toList ds1, d2 <- Set.toList ds2]
        unimplemented!()
    }

    pub fn implies(&self, other: &Disjunction<'a>) -> bool {
        // cImplies1 (CNF ds) d = setAny (`dImplies` d) ds
        unimplemented!()
    }

    pub fn implies_cnf(&self, other: &CNF<'a>) -> bool {
        // cImplies c (CNF ds) = setAll (c `cImplies1`) ds
        unimplemented!()
    }

    /// Convert a 'CNF' to a 'Set' of 'Disjunction's.  Mostly useful if
    /// you wish to serialize a 'DCLabel'.
    pub fn to_set(&self) -> &Vec<Disjunction> {
        &self.ds
    }
}

impl<'a> fmt::Display for CNF<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // showsPrec d (CNF ds)
        //   | Set.size ds == 0 = ("True" ++)
        //   | Set.size ds == 1 = shows $ Set.findMin ds
        //   | otherwise = showParen (d > 7) $
        //       foldr1 (\l r -> l . (" /\\ " ++) . r) $ map shows $ Set.toList ds
        unimplemented!()
    }
}

impl<'a> fmt::Debug for CNF<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // readPrec = true <++ formula <++ single
        //   where true = do True <- readPrec; return cTrue
        //         single = cSingleton <$> readPrec
        //         formula = parens $ prec 7 $ do
        //           let next = do Symbol "/\\" <- lexP
        //                         next'
        //                      <++ return []
        //               next' = ((:) <$> readPrec) `ap` next
        //           cFromList <$> next'
        unimplemented!()
    }
}

// As a type, a 'CNF' is always a conjunction of 'Disjunction's of
// 'Principal's.  However, mathematically speaking, a single
// 'Principal' or single 'Disjunction' is also a degenerate example of
// conjunctive normal form.  Class 'ToCNF' abstracts over the
// differences between these types, promoting them all to 'CNF'.

impl<'a> Into<CNF<'a>> for Vec<Disjunction<'a>> {
    /// Convert a list of 'Disjunction's into a 'CNF'.  Mostly useful if
    /// you wish to de-serialize a 'CNF'.
    fn into(self) -> CNF<'a> {
        // cFromList = Set.foldr cInsert cTrue . Set.fromList
        unimplemented!()
    }
}

impl<'a> Into<CNF<'a>> for Disjunction<'a> {
    fn into(self) -> CNF<'a> {
        // instance ToCNF Disjunction where toCNF = cSingleton
        unimplemented!()
    }
}

impl<'a> Into<CNF<'a>> for Principal<'a> {
    fn into(self) -> CNF<'a> {
        // instance ToCNF Principal where toCNF = toCNF . dSingleton
        unimplemented!()
    }
}

impl<'a> Into<CNF<'a>> for &'a str {
    fn into(self) -> CNF<'a> {
        // instance ToCNF [Char] where toCNF = toCNF . principal
        unimplemented!()
    }
}

impl<'a> Into<CNF<'a>> for bool {
    fn into(self) -> CNF<'a> {
        // instance ToCNF Bool where
        //   toCNF True = cTrue
        //   toCNF False = cFalse
        unimplemented!()
    }
}

// instance ToCNF (Priv CNF) where toCNF = privDesc

#[cfg(test)]
mod test {
    use super::*;
}
