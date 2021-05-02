use std::cmp;
use std::fmt;
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};

pub type SetTag = u64;
pub type ByteString = [u8];

/// A @Principal@ is a primitive source of authority, represented as
/// a string.  The interpretation of principal strings is up to the
/// application.  Reasonable schemes include encoding user names,
/// domain names, and/or URLs in the 'Principal' type.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Principal<'a> {
    name: &'a [u8],
    tag: SetTag,
}

impl<'a> Into<Principal<'a>> for &'a str {
    /// Create a principal from a 'String'.  The 'String' is packed into
    /// a 'S.ByteString' using 'fromString', which will almost certainly
    /// give unexpected results for non-ASCII unicode code points.
    fn into(self) -> Principal<'a> {
        self.as_bytes().into()
    }
}

impl<'a> Into<&'a ByteString> for Principal<'a> {
    /// Extract the name of a principal as a strict 'S.ByteString'.
    /// (Use 'show' to get it as a regular 'String'.)
    fn into(self) -> &'a ByteString {
        self.name
    }
}

impl<'a> Into<Principal<'a>> for &'a ByteString {
    /// Create a principal from a strict 'S.ByteString'.
    fn into(self) -> Principal<'a> {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hv = hasher.finish();
        let bloom = (hv & 0x3f) | ((hv >> 6) & 0x3f) | ((hv >> 12) & 0x3f);
        Principal {
            name: self,
            tag: bloom,
        }
    }
}

impl<'a> fmt::Display for Principal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", std::str::from_utf8(self.name).unwrap())
    }
}

/// Represents a disjunction of 'Principal's, or one clause of a
/// 'CNF'.  There is generally not much need to work directly with
/// @Disjunction@s unless you need to serialize and de-serialize them
/// (by means of 'dToSet' and 'dFromList').
#[derive(PartialEq, Clone)]
pub struct Disjunction<'a> {
    ps: BTreeSet<Principal<'a>>,
    tag: SetTag,
}

impl<'a> Disjunction<'a> {
    /// Expose the set of 'Principal's being ORed together in a
    /// 'Disjunction'.
    pub fn to_set(&self) -> &BTreeSet<Principal<'a>> {
        &self.ps
    }

    pub fn as_false() -> Self {
        Disjunction {
            tag: 0,
            ps: BTreeSet::new(),
        }
    }

    pub fn singleton(p: Principal<'a>) -> Self {
        Disjunction {
            tag: p.tag,
            ps: vec![p].into_iter().collect(),
        }
    }

    pub fn union(mut self, other: Disjunction<'a>) -> Disjunction<'a> {
        self.ps.extend(other.ps);
        Disjunction {
            tag: self.tag | other.tag,
            ps: self.ps,
        }
    }

    /// Returns 'True' iff the first disjunction is a subset of the second.
    pub fn implies(&self, other: &Disjunction<'a>) -> bool {
        if self.tag & other.tag != self.tag {
            false
        } else {
            self.ps.is_subset(&other.ps)
        }
    }
}

impl<'a> cmp::Eq for Disjunction<'a> {}

impl<'a> cmp::PartialOrd for Disjunction<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> cmp::Ord for Disjunction<'a> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match self.ps.len().cmp(&other.ps.len()) {
            cmp::Ordering::Equal => {
                let ps1: Vec<_> = self.ps.iter().collect();
                let ps2: Vec<_> = other.ps.iter().collect();
                ps1.cmp(&ps2)
            }
            ordering => ordering,
        }
    }
}

impl<'a> fmt::Display for Disjunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ps.len() {
            0 => write!(f, "False"),
            1 => write!(f, "{}", self.ps.iter().next().unwrap()),
            len => {
                let mut ps: Vec<_> = self.ps.iter().collect();
                ps.sort();
                write!(f, "({}", ps[0])?;
                for i in 1..len {
                    write!(f, " \\/ {}", ps[i])?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

impl<'a> fmt::Debug for Disjunction<'a> {
    /// Note that a disjunction containing more than one element /must/
    /// be surrounded by parentheses to parse correctly.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ps.len() {
            0 => write!(f, "False"),
            1 => write!(f, "{:?}", self.ps.iter().next().unwrap()),
            len => {
                let mut ps: Vec<_> = self.ps.iter().collect();
                ps.sort();
                write!(f, "({:?}", ps[0])?;
                for i in 1..len {
                    write!(f, " \\/ {:?}", ps[i])?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

impl<'a> Into<Disjunction<'a>> for Vec<Principal<'a>> {
    /// Convert a list of 'Principal's into a 'Disjunction'.
    fn into(self) -> Disjunction<'a> {
        Disjunction {
            tag: self.iter().fold(0, |tag, p| tag | p.tag),
            ps: self.into_iter().collect(),
        }
    }
}

/// A boolean formula in Conjunctive Normal Form.  @CNF@ is used to
/// describe 'DCLabel' privileges, as well to provide each of the two
/// halves of a 'DCLabel'.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct CNF<'a> {
    ds: BTreeSet<Disjunction<'a>>,
}


pub fn set_any<T>(prd: fn(T) -> bool, set: BTreeSet<T>) -> bool {
    // set.into_iter().fold(false, |res, elem| res || prd(elem))
    for elem in set {
        if prd(elem) {
            return true;
        }
    }
    false
}

pub fn set_all<T>(prd: fn(T) -> bool, set: BTreeSet<T>) -> bool {
    // set.into_iter().fold(true, |res, elem| res && prd(elem))
    for elem in set {
        if !prd(elem) {
            return false;
        }
    }
    true
}

impl<'a> CNF<'a> {
    pub fn new(ds: BTreeSet<Disjunction<'a>>) -> Self {
        Self { ds }
    }

    /// Convert a 'CNF' to a 'Set' of 'Disjunction's.  Mostly useful if
    /// you wish to serialize a 'DCLabel'.
    pub fn to_set(&self) -> &BTreeSet<Disjunction> {
        &self.ds
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
        Self::new(BTreeSet::new())
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
        CNF { ds: vec![Disjunction::as_false()].into_iter().collect() }
    }

    pub fn singleton(d: Disjunction<'a>) -> Self {
        CNF { ds: vec![d].into_iter().collect() }
    }

    pub fn insert(mut self, dnew: Disjunction<'a>) -> CNF<'a> {
        for d in &self.ds {
            if d.implies(&dnew) {
                return self;
            }
        }
        self.ds = self.ds.into_iter().filter(|d| !dnew.implies(&d)).collect();
        self.ds.insert(dnew);
        self
    }

    pub fn union(self, other: CNF<'a>) -> CNF<'a> {
        other.ds.into_iter().fold(self, |cnf, d| cnf.insert(d))
    }

    pub fn or(self, other: CNF<'a>) -> CNF<'a> {
        // TODO: unnecessary clones
        self.ds.into_iter().flat_map(|d1| {
            other.ds.iter().map(move |d2| d1.clone().union(d2.clone()))
        })
        .collect::<Vec<_>>()
        .into()
    }

    pub fn implies1(&self, d2: &Disjunction<'a>) -> bool {
        // cImplies1 (CNF ds) d = setAny (`dImplies` d) ds
        for d1 in &self.ds {
            if d1.implies(d2) {
                return true;
            }
        }
        false
    }

    pub fn implies(&self, cnf: &CNF<'a>) -> bool {
        // cImplies c (CNF ds) = setAll (c `cImplies1`) ds
        for d in &cnf.ds {
            if !self.implies1(d) {
                return false;
            }
        }
        true
    }
}

impl<'a> fmt::Display for CNF<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ds.len() {
            0 => write!(f, "True"),
            1 => write!(f, "{}", self.ds.iter().next().unwrap()),
            len => {
                let mut ds: Vec<_> = self.ds.iter().collect();
                ds.sort();
                write!(f, "({}", ds[0])?;
                for i in 1..len {
                    write!(f, " /\\ {}", ds[i])?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

impl<'a> fmt::Debug for CNF<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ds.len() {
            0 => write!(f, "True"),
            1 => write!(f, "{:?}", self.ds.iter().next().unwrap()),
            len => {
                let mut ds: Vec<_> = self.ds.iter().collect();
                ds.sort();
                write!(f, "({:?}", ds[0])?;
                for i in 1..len {
                    write!(f, " /\\ {:?}", ds[i])?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
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
        self.into_iter().fold(CNF::as_true(), |cnf, d| cnf.insert(d))
    }
}

impl<'a> Into<CNF<'a>> for Disjunction<'a> {
    fn into(self) -> CNF<'a> {
        CNF::singleton(self)
    }
}

impl<'a> Into<CNF<'a>> for Principal<'a> {
    fn into(self) -> CNF<'a> {
        Disjunction::singleton(self).into()
    }
}

impl<'a> Into<CNF<'a>> for &'a str {
    fn into(self) -> CNF<'a> {
        let p: Principal = self.into();
        p.into()
    }
}

impl<'a> Into<CNF<'a>> for bool {
    fn into(self) -> CNF<'a> {
        if self {
            CNF::as_true()
        } else {
            CNF::as_false()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
