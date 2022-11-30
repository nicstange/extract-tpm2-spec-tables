use core::cmp::{PartialEq, Eq, PartialOrd, Ord, Ordering};
use crate::avl_tree::{AugmentedAVLTree, AugmentedAVLTreeIterator, AugmentedAVLTreeMutIterator};


#[derive(PartialEq, Eq, Debug)]
pub enum IntervalBound<Q> {
    Inclusive(Q),
    Exclusive(Q),
}

impl<Q> IntervalBound<Q> {
    pub fn unwrap(&self) -> &Q {
        match self {
            Self::Inclusive(b) => b,
            Self::Exclusive(b) => b,
        }
    }
}

impl<Q: Clone> Clone for IntervalBound<Q> {
    fn clone(&self) -> Self {
        match self {
            Self::Inclusive(b) => Self::Inclusive(b.clone()),
            Self::Exclusive(b) => Self::Exclusive(b.clone()),
        }
    }
}

impl<Q: Clone + Copy> Copy for IntervalBound<Q> {}

#[derive(PartialEq, Eq, Debug)]
pub struct Interval <B: Ord> {
    lb: IntervalBound<B>,
    ub: IntervalBound<B>,
}

impl<B: Ord> Interval<B> {
    pub fn new(b0: IntervalBound<B>, b1: IntervalBound<B>) -> Self {
        if b0.unwrap() <= b1.unwrap() {
            Self{lb: b0, ub: b1}
        } else {
            Self{lb: b1, ub: b0}
        }
    }
}

struct LowerBoundQuery<'a, B: Ord> {
    lb: &'a IntervalBound<B>,
}

impl<'a, B: Ord> LowerBoundQuery<'a, B> {
    fn new(lb: &'a IntervalBound<B>) -> Self {
        Self{lb}
    }
}

impl<'a, B: Ord> PartialEq for LowerBoundQuery<'a, B> {
    fn eq(&self, other: &Self) -> bool {
        *self.lb == *other.lb
    }
}

impl<'a, B: Ord> PartialOrd for LowerBoundQuery<'a, B> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.lb {
            IntervalBound::Inclusive(lhs_lb) => {
                match &other.lb {
                    IntervalBound::Inclusive(rhs_lb) => {
                        lhs_lb.partial_cmp(rhs_lb)
                    },
                    IntervalBound::Exclusive(rhs_lb) => {
                        if *lhs_lb == *rhs_lb {
                            Some(Ordering::Less)
                        } else {
                            lhs_lb.partial_cmp(rhs_lb)
                        }
                    },
                }
            },
            IntervalBound::Exclusive(lhs_lb) => {
                match &other.lb {
                    IntervalBound::Inclusive(rhs_lb) => {
                        if *lhs_lb == *rhs_lb {
                            Some(Ordering::Greater)
                        } else {
                            lhs_lb.partial_cmp(rhs_lb)
                        }
                    },
                    IntervalBound::Exclusive(rhs_lb) => {
                        lhs_lb.partial_cmp(rhs_lb)
                    },
                }
            },
        }
    }
}

struct UpperBoundQuery<'a, B: Ord> {
    ub: &'a IntervalBound<B>,
}

impl<'a, B: Ord> UpperBoundQuery<'a, B> {
    fn new(ub: &'a IntervalBound<B>) -> Self {
        Self{ub}
    }
}

impl<'a, B: Ord> PartialEq for UpperBoundQuery<'a, B> {
    fn eq(&self, other: &Self) -> bool {
        *self.ub == *other.ub
    }
}

impl<'a, B: Ord> PartialOrd for UpperBoundQuery<'a, B> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.ub {
            IntervalBound::Inclusive(lhs_ub) => {
                match &other.ub {
                    IntervalBound::Inclusive(rhs_ub) => {
                        lhs_ub.partial_cmp(rhs_ub)
                    },
                    IntervalBound::Exclusive(rhs_ub) => {
                        if *lhs_ub == *rhs_ub {
                            Some(Ordering::Greater)
                        } else {
                            lhs_ub.partial_cmp(rhs_ub)
                        }
                    },
                }
            },
            IntervalBound::Exclusive(lhs_ub) => {
                match &other.ub {
                    IntervalBound::Inclusive(rhs_ub) => {
                        if *lhs_ub == *rhs_ub {
                            Some(Ordering::Less)
                        } else {
                            lhs_ub.partial_cmp(rhs_ub)
                        }
                    },
                    IntervalBound::Exclusive(rhs_ub) => {
                        lhs_ub.partial_cmp(rhs_ub)
                    },
                }
            },
        }
    }
}

impl<'a, B: Ord> PartialEq<UpperBoundQuery<'a, B>> for LowerBoundQuery<'a, B> {
    fn eq(&self, _other: &UpperBoundQuery<'a, B>) -> bool {
        false
    }
}

impl<'a, B: Ord> PartialOrd<UpperBoundQuery<'a, B>> for LowerBoundQuery<'a, B> {
    fn partial_cmp(&self, other: &UpperBoundQuery<'a, B>) -> Option<Ordering> {
        match self.lb {
            IntervalBound::Inclusive(lhs_lb) => {
                match &other.ub {
                    IntervalBound::Inclusive(rhs_ub) => {
                        if *lhs_lb == *rhs_ub {
                            Some(Ordering::Less)
                        } else {
                            lhs_lb.partial_cmp(rhs_ub)
                        }
                    },
                    IntervalBound::Exclusive(rhs_ub) => {
                        if *lhs_lb == *rhs_ub {
                            Some(Ordering::Greater)
                        } else {
                            lhs_lb.partial_cmp(rhs_ub)
                        }
                    },
                }
            },
            IntervalBound::Exclusive(lhs_lb) => {
                match &other.ub {
                    IntervalBound::Inclusive(rhs_ub) => {
                        if *lhs_lb == *rhs_ub {
                            Some(Ordering::Greater)
                        } else {
                            lhs_lb.partial_cmp(rhs_ub)
                        }
                    },
                    IntervalBound::Exclusive(rhs_ub) => {
                        if *lhs_lb == *rhs_ub {
                            Some(Ordering::Greater)
                        } else {
                            lhs_lb.partial_cmp(rhs_ub)
                        }
                    },
                }
            },
        }
    }
}

impl<'a, B: Ord> PartialEq<LowerBoundQuery<'a, B>> for UpperBoundQuery<'a, B> {
    fn eq(&self, other: &LowerBoundQuery<'a, B>) -> bool {
        other.eq(self)
    }
}


impl<'a, B: Ord> PartialOrd<LowerBoundQuery<'a, B>> for UpperBoundQuery<'a, B> {
    fn partial_cmp(&self, other: &LowerBoundQuery<'a, B>) -> Option<Ordering> {
        other.partial_cmp(self)
            .map(|o| match o {
                Ordering::Less => Ordering::Greater,
                Ordering::Equal => Ordering::Equal,
                Ordering::Greater => Ordering::Less,
            })
    }
}

struct LowerBoundKey<B: Ord> {
    interval: Interval<B>,
}

impl<B: Ord> LowerBoundKey<B> {
    fn new(interval: Interval<B>) -> Self {
        Self{interval}
    }
}

impl<B: Ord> PartialEq for LowerBoundKey<B> {
    fn eq(&self, other: &Self) -> bool {
        LowerBoundQuery::new(&self.interval.lb).eq(other)
    }
}

impl<B: Ord> Eq for LowerBoundKey<B> {}

impl<B: Ord> PartialOrd for LowerBoundKey<B> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        LowerBoundQuery::new(&self.interval.lb).partial_cmp(other)
    }
}

impl<B: Ord> Ord for LowerBoundKey<B> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<'a, B: Ord> PartialEq<LowerBoundKey<B>> for LowerBoundQuery<'a, B> {
    fn eq(&self, other: &LowerBoundKey<B>) -> bool {
        self.eq(&LowerBoundQuery::new(&other.interval.lb))
    }
}

impl<'a, B: Ord> PartialOrd<LowerBoundKey<B>> for LowerBoundQuery<'a, B> {
    fn partial_cmp(&self, other: &LowerBoundKey<B>) -> Option<Ordering> {
        self.partial_cmp(&LowerBoundQuery::new(&other.interval.lb))
    }
}

impl<'a, B: Ord> PartialEq<LowerBoundKey<B>> for UpperBoundQuery<'a, B> {
    fn eq(&self, other: &LowerBoundKey<B>) -> bool {
        self.eq(&LowerBoundQuery::new(&other.interval.lb))
    }
}

impl<'a, B: Ord> PartialOrd<LowerBoundKey<B>> for UpperBoundQuery<'a, B> {
    fn partial_cmp(&self, other: &LowerBoundKey<B>) -> Option<Ordering> {
        self.partial_cmp(&LowerBoundQuery::new(&other.interval.lb))
    }
}

#[derive(Clone, PartialEq, Eq)]
struct UpperBoundAux<B: Clone + Ord> {
    ub: IntervalBound<B>,
}

impl<B: Clone + Ord + Copy> Copy for UpperBoundAux<B> {}

impl<B: Clone + Ord> UpperBoundAux<B> {
    fn new(ub: IntervalBound<B>) -> Self {
        Self{ub}
    }
}

impl<B: Clone + Ord> PartialOrd for UpperBoundAux<B> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        UpperBoundQuery::new(&self.ub).partial_cmp(&UpperBoundQuery::new(&other.ub))
    }
}

impl<B: Clone + Ord> Ord for UpperBoundAux<B> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub struct IntervalTree<B: Clone + Ord + 'static, T> {
    tree: AugmentedAVLTree<LowerBoundKey<B>, T, UpperBoundAux<B>>,
}

impl<B: Clone + Ord + 'static, T> IntervalTree<B, T> {
    pub fn new() -> Self {
        Self{tree: AugmentedAVLTree::new()}
    }

    fn shall_descend(query_lb: Option<&IntervalBound<B>>, subtree_ub: &UpperBoundAux<B>) -> bool {
        match query_lb {
            None => true,
            Some(query_lb) => LowerBoundQuery::new(query_lb) < UpperBoundQuery::new(&subtree_ub.ub)
        }
    }

    fn update_upper_bound_aux(subtree_ub: &mut UpperBoundAux<B>,
                              key: &LowerBoundKey<B>,
                              left_subtree_ub: Option<&UpperBoundAux<B>>,
                              right_subtree_ub: Option<&UpperBoundAux<B>>) {
        *subtree_ub = UpperBoundAux::new(key.interval.ub.clone());
        if let Some(left_subtree_ub) = left_subtree_ub {
            if *left_subtree_ub > *subtree_ub {
                *subtree_ub = left_subtree_ub.clone();
            }
        }
        if let Some(right_subtree_ub) = right_subtree_ub {
            if *right_subtree_ub > *subtree_ub {
                *subtree_ub = right_subtree_ub.clone();
            }
        }
    }

    pub fn insert(&mut self, key: Interval<B>, val: T) {
        let upper_bound_aux = UpperBoundAux::new(key.ub.clone());
        self.tree.insert(LowerBoundKey::new(key), val, upper_bound_aux,
                         &Self::update_upper_bound_aux)
    }

    pub fn delete<IsMatch>(&mut self, key: &Interval<B>, is_match: &IsMatch) -> Option<(Interval<B>, T)>
    where for<'a> IsMatch: Fn(&'a Interval<B>, &'a T) -> bool
    {
        let is_empty = LowerBoundQuery::new(&key.lb) > UpperBoundQuery::new(&key.ub);
        self.tree.delete(&LowerBoundQuery::new(&key.lb),
                         &|node_key, node_value| {
                             if node_key.interval == *key {
                                 is_match(&node_key.interval, &node_value)
                             } else {
                                 false
                             }
                         },
                         &|upper_bound_aux| {
                             if !is_empty {
                                 Self::shall_descend(Some(&key.lb), upper_bound_aux)
                             } else {
                                 // Be careful to not miss empty intervals, don't used Self::shall_descend() here.
                                 key.lb.unwrap() <= upper_bound_aux.ub.unwrap()
                             }
                         },
                         &Self::update_upper_bound_aux)
            .map(|(key, value)| (key.interval, value))
    }

    pub fn iter<'a>(&'a self, lb: Option<IntervalBound<B>>, ub: Option<IntervalBound<B>>)
                    -> IntervalTreeIterator<'a, B, T> {
        IntervalTreeIterator::new(self, lb, ub)
    }

    pub fn iter_mut<'a>(&'a mut self, lb: Option<IntervalBound<B>>, ub: Option<IntervalBound<B>>)
                        -> IntervalTreeMutIterator<'a, B, T> {
        IntervalTreeMutIterator::new(self, lb, ub)
    }
}

struct IteratorLowerBoundQuery<> {}

impl<B: Ord> PartialEq<LowerBoundKey<B>> for IteratorLowerBoundQuery {
    fn eq(&self, _other: &LowerBoundKey<B>) -> bool {
        false
    }
}

impl<B: Ord> PartialOrd<LowerBoundKey<B>> for IteratorLowerBoundQuery {
    fn partial_cmp(&self, _other: &LowerBoundKey<B>) -> Option<Ordering> {
        None
    }
}

struct IteratorUpperBoundQuery<B: Ord> {
    ub: IntervalBound<B>,
}

impl<B: Ord> IteratorUpperBoundQuery<B> {
    fn new(ub: IntervalBound<B>) -> Self {
        Self{ub}
    }
}

impl<B: Ord> PartialEq<LowerBoundKey<B>> for IteratorUpperBoundQuery<B> {
    fn eq(&self, other: &LowerBoundKey<B>) -> bool {
        UpperBoundQuery::new(&self.ub).eq(other)
    }
}

impl<B: Ord> PartialOrd<LowerBoundKey<B>> for IteratorUpperBoundQuery<B> {
    fn partial_cmp(&self, other: &LowerBoundKey<B>) -> Option<Ordering> {
        UpperBoundQuery::new(&self.ub).partial_cmp(other)
    }
}

pub struct IntervalTreeIterator<'a, B: Clone + Ord, T> {
    lb: Option<IntervalBound<B>>,
    it: AugmentedAVLTreeIterator<'a, IteratorLowerBoundQuery, IteratorUpperBoundQuery<B>,
                                 LowerBoundKey<B>, T, UpperBoundAux<B>,
                                 Box<dyn Fn(&UpperBoundAux<B>) -> bool>>,
}

impl<'a, B: Clone + Ord + 'static, T> IntervalTreeIterator<'a, B, T> {
    fn new(tree: &'a IntervalTree<B, T>,
           lb: Option<IntervalBound<B>>,
           ub: Option<IntervalBound<B>>) -> Self {
        let lb_for_aux_cmp = lb.clone();
        Self{
            lb,
            it: tree.tree.iter(None, ub.map(|ub| IteratorUpperBoundQuery::new(ub)),
                               Box::new(move |upper_bound_aux| {
                                   IntervalTree::<B, T>::shall_descend(lb_for_aux_cmp.as_ref(), upper_bound_aux)
                               })),
        }
    }
}

impl<'a, B: Clone + Ord, T> Iterator for IntervalTreeIterator<'a, B, T> {
    type Item = (&'a Interval<B>, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let item = self.it.next();
            if let Some(query_lb) = &self.lb {
                if let Some(item) = &item {
                    if !(LowerBoundQuery::new(query_lb) < UpperBoundQuery::new(&item.0.interval.ub)) {
                        continue;
                    }
                }
            }

            break item;
        }.map(|item| (&item.0.interval, item.1))
    }
}

pub struct IntervalTreeMutIterator<'a, B: Clone + Ord, T> {
    lb: Option<IntervalBound<B>>,
    it: AugmentedAVLTreeMutIterator<'a, IteratorLowerBoundQuery, IteratorUpperBoundQuery<B>,
                                    LowerBoundKey<B>, T, UpperBoundAux<B>,
                                    Box<dyn Fn(&UpperBoundAux<B>) -> bool>>,
}

impl<'a, B: Clone + Ord + 'static, T> IntervalTreeMutIterator<'a, B, T> {
    fn new(tree: &'a mut IntervalTree<B, T>,
           lb: Option<IntervalBound<B>>,
           ub: Option<IntervalBound<B>>) -> Self {
        let lb_for_aux_cmp = lb.clone();
        Self{
            lb,
            it: tree.tree.iter_mut(None, ub.map(|ub| IteratorUpperBoundQuery::new(ub)),
                                   Box::new(move |upper_bound_aux| {
                                       IntervalTree::<B, T>::shall_descend(lb_for_aux_cmp.as_ref(), upper_bound_aux)
                                   })),
        }
    }
}

impl<'a, B: Clone + Ord, T> Iterator for IntervalTreeMutIterator<'a, B, T> {
    type Item = (&'a Interval<B>, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let item = self.it.next();
            if let Some(query_lb) = &self.lb {
                if let Some(item) = &item {
                    if !(LowerBoundQuery::new(query_lb) < UpperBoundQuery::new(&item.0.interval.ub)) {
                        continue;
                    }
                }
            }

            break item;
        }.map(|item| (&item.0.interval, item.1))
    }
}

#[test]
fn test_insert_delete_same() {
    let mut t: IntervalTree<u32, u32> = IntervalTree::new();

    t.insert(Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)), 0);
    t.insert(Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)), 1);
    t.insert(Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)), 2);
    assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), 3);
    assert_eq!(t.iter(Some(IntervalBound::Inclusive(0)), Some(IntervalBound::Inclusive(1))).count(), 3);
    assert_eq!(t.iter(Some(IntervalBound::Exclusive(0)), Some(IntervalBound::Inclusive(1))).count(), 3);
    assert_eq!(t.iter(Some(IntervalBound::Inclusive(0)), Some(IntervalBound::Exclusive(1))).count(), 3);
    assert_eq!(t.iter(Some(IntervalBound::Exclusive(0)), Some(IntervalBound::Exclusive(1))).count(), 3);
    assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), 3);
    assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(0)), Some(IntervalBound::Inclusive(1))).count(), 3);
    assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(0)), Some(IntervalBound::Inclusive(1))).count(), 3);
    assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(0)), Some(IntervalBound::Exclusive(1))).count(), 3);
    assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(0)), Some(IntervalBound::Exclusive(1))).count(), 3);
    assert_eq!(t.iter(Some(IntervalBound::Exclusive(0)), Some(IntervalBound::Inclusive(0))).count(), 3);
    assert_eq!(t.iter(Some(IntervalBound::Inclusive(1)), Some(IntervalBound::Exclusive(1))).count(), 3);
    assert_eq!(t.iter(Some(IntervalBound::Inclusive(0)), Some(IntervalBound::Exclusive(0))).count(), 0);
    assert_eq!(t.iter(Some(IntervalBound::Exclusive(1)), Some(IntervalBound::Inclusive(1))).count(), 0);
    assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(0)), Some(IntervalBound::Inclusive(0))).count(), 3);
    assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(1)), Some(IntervalBound::Exclusive(1))).count(), 3);
    assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(0)), Some(IntervalBound::Exclusive(0))).count(), 0);
    assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(1)), Some(IntervalBound::Inclusive(1))).count(), 0);


    assert_eq!(t.delete(&Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)),
                        &|_i, v| *v == 0),
               Some((Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)), 0)));
    assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), 2);
    assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), 2);

    assert_eq!(t.delete(&Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)),
                        &|_i, v| *v == 2),
               Some((Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)), 2)));
    assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), 1);
    assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), 1);

    assert_eq!(t.delete(&Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)),
                        &|_i, v| *v == 1),
               Some((Interval::new(IntervalBound::Inclusive(0), IntervalBound::Inclusive(1)), 1)));
    assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), 0);
    assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), 0);
}

#[test]
fn test_insert_delete_many_inclusive() {
    let mut t: IntervalTree<u32, u32> = IntervalTree::new();

    for i in 0..256 {
        t.insert(Interval::new(IntervalBound::Inclusive(i), IntervalBound::Inclusive(i + 2)), i);
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), i as usize + 1);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), i as usize + 1);
    }

    for i in 0..255 {
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, Some(IntervalBound::Inclusive(i + 1))).count(),
                   i as usize + 2);
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, Some(IntervalBound::Exclusive(i + 1))).count(),
                   i as usize + 1);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, Some(IntervalBound::Inclusive(i + 1))).count(),
                   i as usize + 2);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, Some(IntervalBound::Exclusive(i + 1))).count(),
                   i as usize + 1);
    }

    for i in 0..255 {
        assert_eq!(t.iter(Some(IntervalBound::Inclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 2);
        assert_eq!(t.iter(Some(IntervalBound::Exclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 1);
        assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 2);
        assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 1);
    }

    for i in 2..254 {
        assert_eq!(t.iter(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 5);
        assert_eq!(t.iter(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 4);
        assert_eq!(t.iter(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 4);
        assert_eq!(t.iter(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 3);
        assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 5);
        assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 4);
        assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 4);
        assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 3);
    }

    for i in 0..256 {
        assert_eq!(t.delete(&Interval::new(IntervalBound::Inclusive(i), IntervalBound::Inclusive(i + 2)),
                            &|_i, _v| true),
                   Some((Interval::new(IntervalBound::Inclusive(i), IntervalBound::Inclusive(i + 2)), i)));
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), (255 - i) as usize);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), (255 - i) as usize);
    }
    assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), 0);
    assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), 0);
}

#[test]
fn test_insert_delete_many_excusive() {
    let mut t: IntervalTree<u32, u32> = IntervalTree::new();

    for i in 0..256 {
        t.insert(Interval::new(IntervalBound::Exclusive(i), IntervalBound::Exclusive(i + 2)), i);
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), i as usize + 1);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), i as usize + 1);
    }

    for i in 0..255 {
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, Some(IntervalBound::Inclusive(i + 1))).count(),
                   i as usize + 1);
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, Some(IntervalBound::Exclusive(i + 1))).count(),
                   i as usize + 1);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, Some(IntervalBound::Inclusive(i + 1))).count(),
                   i as usize + 1);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, Some(IntervalBound::Exclusive(i + 1))).count(),
                   i as usize + 1);
    }

    for i in 0..255 {
        assert_eq!(t.iter(Some(IntervalBound::Inclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 1);
        assert_eq!(t.iter(Some(IntervalBound::Exclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 1);
        assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 1);
        assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(256 - i)), None::<IntervalBound::<u32>>).count(),
                   i as usize + 1);
    }

    for i in 2..254 {
        assert_eq!(t.iter(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 3);
        assert_eq!(t.iter(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 3);
        assert_eq!(t.iter(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 3);
        assert_eq!(t.iter(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 3);
        assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 3);
        assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Inclusive(i + 2))).count(), 3);
        assert_eq!(t.iter_mut(Some(IntervalBound::Inclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 3);
        assert_eq!(t.iter_mut(Some(IntervalBound::Exclusive(i)), Some(IntervalBound::Exclusive(i + 2))).count(), 3);
    }

    for i in 0..256 {
        assert_eq!(t.delete(&Interval::new(IntervalBound::Exclusive(i), IntervalBound::Exclusive(i + 2)),
                            &|_i, _v| true),
                   Some((Interval::new(IntervalBound::Exclusive(i), IntervalBound::Exclusive(i + 2)), i)));
        assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), (255 - i) as usize);
        assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), (255 - i) as usize);
    }
    assert_eq!(t.iter(None::<IntervalBound::<u32>>, None).count(), 0);
    assert_eq!(t.iter_mut(None::<IntervalBound::<u32>>, None).count(), 0);
}
