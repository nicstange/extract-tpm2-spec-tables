extern crate alloc;

use core::mem;
use core::cmp::Ord;

struct NodeRef<K: Ord, T, A> {
    node: Option<Box<Node<K, T, A>>>,
}

struct Node<K: Ord, T, A> {
    key: K,
    val: T,
    aux: A,
    bf: i8,

    left: NodeRef<K, T, A>,
    right: NodeRef<K, T, A>,
}

enum NodeIterVal<'a, K: Ord, T, A> {
    Value((&'a K, &'a T)),
    Child(&'a Node<K, T, A>),
}

enum NodeIterMutVal<'a, K: Ord, T, A> {
    Value((&'a K, &'a mut T)),
    Child(&'a mut Node<K, T, A>),
}

struct NodeIter<'a, K: Ord, T, A> {
    key_val: Option<(&'a K, &'a T)>,
    left: Option<&'a Node<K, T, A>>,
    right: Option<&'a Node<K, T, A>>,
}

struct NodeIterMut<'a, K: Ord, T, A> {
    key_val: Option<(&'a K, &'a mut T)>,
    left: Option<&'a mut Node<K, T, A>>,
    right: Option<&'a mut Node<K, T, A>>,
}

#[derive(PartialEq, Eq)]
pub enum QueryBound<Q> {
    Inclusive(Q),
    Exclusive(Q),
}

pub struct AugmentedAVLTreeIterator<'a, Q: PartialOrd<K>, K: Ord, T, A, ShallDescend>
where for<'b> ShallDescend: Fn(&'b A) -> bool
{
    lb: Option<QueryBound<Q>>,
    ub: Option<QueryBound<Q>>,
    shall_descend: &'a ShallDescend,
    stack: Vec<NodeIter<'a, K, T, A>>,
}

pub struct AugmentedAVLTreeMutIterator<'a, Q: PartialOrd<K>, K: Ord, T, A, ShallDescend>
where for<'b> ShallDescend: Fn(&'b A) -> bool
{
    lb: Option<QueryBound<Q>>,
    ub: Option<QueryBound<Q>>,
    shall_descend: &'a ShallDescend,
    stack: Vec<NodeIterMut<'a, K, T, A>>,
}

pub struct AugmentedAVLTree<K: Ord, T, A> {
    root: NodeRef<K, T, A>,
}

impl<K: Ord, T, A> Node<K, T, A> {
    fn new(key: K, val: T, aux: A) -> Self {
        Node{key, val, aux, bf: 0, left: NodeRef::new(), right: NodeRef::new()}
    }

    fn do_update_aux<UpdateAux>(&mut self, update_aux: &UpdateAux)
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        let left_aux = match &self.left.node {
            None => None,
            Some(n) => Some(&n.aux),
        };
        let right_aux = match &self.right.node {
            None => None,
            Some(n) => Some(&n.aux),
        };

        update_aux(&mut self.aux, &self.key, left_aux, right_aux);
    }

    fn iter<'a, Q: PartialOrd<K>, ShallDescend>(&'a self,
                                                lb: &Option<QueryBound<Q>>, ub: &Option<QueryBound<Q>>,
                                                shall_descend: &ShallDescend) -> Option<NodeIter<'a, K, T, A>>
    where for<'b> ShallDescend: Fn(&'b A) -> bool
    {
        if !shall_descend(&self.aux) {
            return None;
        }

        let mut in_lb = true;
        if let Some(lb) = lb {
            match lb {
                QueryBound::Inclusive(lb) => {
                    if *lb > self.key {
                        in_lb = false;
                    }
                },
                QueryBound::Exclusive(lb) => {
                    if *lb >= self.key {
                        in_lb = false;
                    }
                }
            }
        }

        let mut in_ub = true;
        if let Some(ub) = ub {
            match ub {
                QueryBound::Inclusive(ub) => {
                    if *ub < self.key {
                        in_ub = false;
                    }
                },
                QueryBound::Exclusive(ub) => {
                    if *ub <= self.key {
                        in_ub = false;
                    }
                }
            }
        }

        if !in_lb && !in_ub {
            return None;
        }

        let mut i = NodeIter::new();
        if in_lb && in_ub {
            i.key_val = Some((&self.key, &self.val));
        }
        if in_lb {
            if let Some(ln) = &self.left.node {
                i.left = Some(&*ln);
            }
        }
        if in_ub {
            if let Some(rn) = &self.right.node {
                i.right = Some(&*rn);
            }
        }

        Some(i)
    }

    fn iter_mut<'a, Q: PartialOrd<K>, ShallDescend>(&'a mut self,
                                                    lb: &Option<QueryBound<Q>>, ub: &Option<QueryBound<Q>>,
                                                    shall_descend: &ShallDescend)
                                                    -> Option<NodeIterMut<'a, K, T, A>>
    where for<'b> ShallDescend: Fn(&'b A) -> bool
    {
        if !shall_descend(&self.aux) {
            return None;
        }

        let mut in_lb = true;
        if let Some(lb) = lb {
            match lb {
                QueryBound::Inclusive(lb) => {
                    if *lb > self.key {
                        in_lb = false;
                    }
                },
                QueryBound::Exclusive(lb) => {
                    if *lb >= self.key {
                        in_lb = false;
                    }
                }
            }
        }

        let mut in_ub = true;
        if let Some(ub) = ub {
            match ub {
                QueryBound::Inclusive(ub) => {
                    if *ub < self.key {
                        in_ub = false;
                    }
                },
                QueryBound::Exclusive(ub) => {
                    if *ub <= self.key {
                        in_ub = false;
                    }
                }
            }
        }

        if !in_lb && !in_ub {
            return None;
        }

        let mut i = NodeIterMut::new();
        if in_lb && in_ub {
            i.key_val = Some((&self.key, &mut self.val));
        }
        if in_lb {
            if let Some(ln) = &mut self.left.node {
                i.left = Some(&mut *ln);
            }
        }
        if in_ub {
            if let Some(rn) = &mut self.right.node {
                i.right = Some(&mut *rn);
            }
        }

        Some(i)
    }
}

impl<K: Ord, T, A> NodeRef<K, T, A> {
    fn new() -> Self {
        NodeRef{node: None}
    }

    fn new_node(key: K, val: T, aux: A) -> Self {
        NodeRef{node: Some(Box::new(Node::new(key, val, aux)))}
    }

    fn rotate_left<UpdateAux>(&mut self, update_aux: &UpdateAux)
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        match &mut self.node {
            None => (),

            Some(sn) => {
                if sn.bf <= 0 {
                    return ();
                }
                assert!(sn.bf <= 2);

                let mut r = mem::replace(&mut sn.right.node, None);
                match &mut r {
                    None => (),

                    Some(rn) => {
                        assert!(-1 <= rn.bf && rn.bf <= 2);
                        let orig_s_bf = sn.bf;
                        if rn.bf >= 0 {
                            sn.bf = orig_s_bf - rn.bf - 1;
                        } else {
                            sn.bf = orig_s_bf - 1;
                        }
                        assert!(-1 <= sn.bf && sn.bf <= 1);
                        if sn.bf >= 0 {
                            rn.bf = rn.bf - 1;
                        } else {
                            if rn.bf >= 0 {
                                rn.bf = orig_s_bf - 2;
                            } else {
                                rn.bf = orig_s_bf + rn.bf - 2;
                            }
                        }
                        assert!(-2 <= rn.bf && rn.bf <= 1);

                        let rl = mem::replace(&mut rn.left.node, None);
                        sn.right.node = rl;
                        sn.do_update_aux(update_aux);
                        rn.left.node = mem::replace(&mut self.node, None);
                        rn.do_update_aux(update_aux);
                        self.node = r;
                    },
                }
            }
        }
    }

    fn rotate_right<UpdateAux>(&mut self, update_aux: &UpdateAux)
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        match &mut self.node {
            None => (),

            Some(sn) => {
                if sn.bf >= 0 {
                    return ();
                }
                assert!(sn.bf >= -2);

                let mut l = mem::replace(&mut sn.left.node, None);
                match &mut l {
                    None => (),

                    Some(ln) => {
                        assert!(-2 <= ln.bf && ln.bf <= 1);
                        let orig_s_bf = sn.bf;
                        if ln.bf <= 0 {
                            sn.bf = orig_s_bf - ln.bf + 1;
                        } else {
                            sn.bf = orig_s_bf + 1
                        }
                        assert!(-1 <= sn.bf && sn.bf <= 1);
                        if sn.bf <= 0 {
                            ln.bf = ln.bf + 1;
                        } else {
                            if ln.bf <= 0 {
                                ln.bf = orig_s_bf + 2;
                            } else {
                                ln.bf = orig_s_bf + ln.bf + 2;
                            }
                        }

                        let lr = mem::replace(&mut ln.right.node, None);
                        sn.left.node = lr;
                        sn.do_update_aux(update_aux);
                        ln.right.node = mem::replace(&mut self.node, None);
                        ln.do_update_aux(update_aux);
                        self.node = l;
                    },
                }
            }
        }
    }

    fn rotate_right_left<UpdateAux>(&mut self, update_aux: &UpdateAux)
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        match &mut self.node {
            None => (),

            Some(sn) => {
                sn.right.rotate_right(update_aux);
                self.rotate_left(update_aux);
            },
        }
    }

    fn rotate_left_right<UpdateAux>(&mut self, update_aux: &UpdateAux)
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        match &mut self.node {
            None => (),

            Some(sn) => {
                sn.left.rotate_left(update_aux);
                self.rotate_right(update_aux);
            },
        }
    }

    fn insert<UpdateAux>(&mut self, key: K, val: T, aux: A, update_aux: &UpdateAux) -> bool
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        match &mut self.node {
            None => {
                *self = Self::new_node(key, val, aux);
                self.node.as_mut().unwrap().do_update_aux(update_aux);
                true
            },
            Some(n) => {
                if key <= n.key {
                    if !n.left.insert(key, val, aux, update_aux) {
                        n.do_update_aux(update_aux);
                        return false;
                    }

                    assert!(-1 <= n.bf && n.bf <= 1);
                    n.bf = n.bf - 1;
                    if n.bf < -1 {
                        let nl = n.left.node.as_ref().unwrap();
                        if nl.bf > 0 {
                            self.rotate_left_right(update_aux);
                        } else {
                            self.rotate_right(update_aux);
                        }
                        assert_eq!(self.node.as_ref().unwrap().bf, 0);
                        false
                    } else {
                        n.do_update_aux(update_aux);
                        n.bf != 0
                    }

                } else {
                    if !n.right.insert(key, val, aux, update_aux) {
                        n.do_update_aux(update_aux);
                        return false;
                    }

                    assert!(-1 <= n.bf && n.bf <= 1);
                    n.bf = n.bf + 1;
                    if n.bf > 1 {
                        let nr = n.right.node.as_ref().unwrap();
                        if nr.bf < 0 {
                            self.rotate_right_left(update_aux);
                        } else {
                            self.rotate_left(update_aux);
                        }
                        assert_eq!(self.node.as_ref().unwrap().bf, 0);
                        false
                    } else {
                        n.do_update_aux(update_aux);
                        n.bf != 0
                    }
                }
            }
        }
    }

    fn account_deletion_left<UpdateAux>(&mut self, update_aux: &UpdateAux) -> bool
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        let n = self.node.as_deref_mut().unwrap();
        n.bf = n.bf + 1;
        if n.bf > 1 {
            let nr = n.right.node.as_ref().unwrap();
            let nr_bf = nr.bf;
            if nr_bf < 0 {
                self.rotate_right_left(update_aux);
                true
            } else {
                self.rotate_left(update_aux);
                nr_bf != 0
            }
        } else {
            n.do_update_aux(update_aux);
            n.bf == 0
        }
    }

    fn account_deletion_right<UpdateAux>(&mut self, update_aux: &UpdateAux) -> bool
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        let n = self.node.as_deref_mut().unwrap();
        n.bf = n.bf - 1;
        if n.bf < -1 {
            let nl = n.left.node.as_ref().unwrap();
            let nl_bf = nl.bf;
            if nl_bf > 0 {
                self.rotate_left_right(update_aux);
                true
            } else {
                self.rotate_right(update_aux);
                nl_bf != 0
            }
        } else {
            n.do_update_aux(update_aux);
            n.bf == 0
        }
    }

    fn isolate_first_node<UpdateAux>(&mut self, update_aux: &UpdateAux) -> (Self, bool)
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        match &mut self.node {
            None => (Self::new(), false),
            Some(n) => {
                let (l, height_decreased) = n.left.isolate_first_node(update_aux);
                match l.node {
                    None => {
                        let r = mem::replace(&mut n.right.node, None);
                        n.bf = 0;
                        let s = mem::replace(&mut self.node, r);
                        (NodeRef{node: s}, true)
                    },
                    Some(_) => {
                        if height_decreased {
                            let height_decreased = self.account_deletion_left(update_aux);
                            (l, height_decreased)
                        } else {
                            n.do_update_aux(update_aux);
                            (l, false)
                        }
                    }
                }
            }
        }
    }

    pub fn delete<Q: PartialOrd<K>, IsMatch, ShallDescend, UpdateAux>(&mut self, key: &Q,
                                                                      is_match: &IsMatch, shall_descend: &ShallDescend,
                                                                      update_aux: &UpdateAux) -> Option<((K, T), bool)>
    where
        for<'a> IsMatch: Fn(&'a K, &'a T) -> bool,
        for<'a> ShallDescend: Fn(&'a A) -> bool,
        for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        match &mut self.node {
            None => None,
            Some(n) => {
                if !shall_descend(&n.aux) {
                    return None;
                }

                if *key == n.key && is_match(&n.key, &n.val) {
                    let l = mem::replace(&mut n.left.node, None);
                    match l {
                        None => {
                            let r = mem::replace(&mut n.right.node, None);
                            let deleted = mem::replace(&mut self.node, r).unwrap();
                            let deleted = (deleted.key, deleted.val);
                            return Some((deleted, true));
                        },
                        Some(_) => {
                            let (mut s, height_decreased) = n.right.isolate_first_node(update_aux);
                            match &mut s.node {
                                None => {
                                    let deleted = mem::replace(&mut self.node, l).unwrap();
                                    let deleted = (deleted.key, deleted.val);
                                    return Some((deleted, true));
                                },
                                Some(sn) => {
                                    sn.bf = n.bf;
                                    sn.left.node = l;
                                    sn.right.node = mem::replace(&mut n.right.node, None);
                                    sn.do_update_aux(update_aux);
                                    let deleted = mem::replace(&mut self.node, s.node).unwrap();
                                    let deleted = (deleted.key, deleted.val);
                                    if height_decreased {
                                        let height_decreased = self.account_deletion_right(update_aux);
                                        return Some((deleted, height_decreased));
                                    } else {
                                        return Some((deleted, false));
                                    }
                                }
                            }
                        }
                    }
                }

                if *key <= n.key {
                    let deleted = n.left.delete(key, is_match, shall_descend, update_aux);
                    if let Some((deleted, height_decreased)) = deleted {
                        if height_decreased {
                        let height_decreased = self.account_deletion_left(update_aux);
                            return Some((deleted, height_decreased));
                        } else {
                            n.do_update_aux(update_aux);
                            return Some((deleted, false));
                        }
                    }
                }

                if *key >= n.key {
                    let deleted = n.right.delete(key, is_match, shall_descend, update_aux);
                    if let Some((deleted, height_decreased)) = deleted {
                        if height_decreased {
                            let height_decreased = self.account_deletion_right(update_aux);
                            return Some((deleted, height_decreased));
                        } else {
                            n.do_update_aux(update_aux);
                            return Some((deleted, false));
                        }
                    }
                }

                None
            }
        }
    }
}

impl<'a, K: Ord, T, A> NodeIter<'a, K, T, A> {
    fn new() -> Self {
        Self{key_val: None, left: None, right: None}
    }

    fn next(&mut self) -> Option<NodeIterVal<'a, K, T, A>> {
        match self.left.take() {
            Some(ln) => Some(NodeIterVal::Child(ln)),
            None => {
                match self.key_val.take() {
                    Some(v) => Some(NodeIterVal::Value(v)),
                    None => {
                        match self.right.take() {
                            Some(rn) => Some(NodeIterVal::Child(rn)),
                            None => None
                        }
                    }
                }
            }
        }
    }
}

impl<'a, K: Ord, T, A> NodeIterMut<'a, K, T, A> {
    fn new() -> Self {
        Self{key_val: None, left: None, right: None}
    }

    fn next(&mut self) -> Option<NodeIterMutVal<'a, K, T, A>> {
        match self.left.take() {
            Some(ln) => Some(NodeIterMutVal::Child(ln)),
            None => {
                match self.key_val.take() {
                    Some(v) => Some(NodeIterMutVal::Value(v)),
                    None => {
                        match self.right.take() {
                            Some(rn) => Some(NodeIterMutVal::Child(rn)),
                            None => None
                        }
                    }
                }
            }
        }
    }
}

impl<Q> QueryBound<Q> {
    pub fn unwrap(&self) -> &Q {
        match self {
            Self::Inclusive(b) => b,
            Self::Exclusive(b) => b,
        }
    }
}

impl<Q: Clone> Clone for QueryBound<Q> {
    fn clone(&self) -> Self {
        match self {
            Self::Inclusive(b) => Self::Inclusive(b.clone()),
            Self::Exclusive(b) => Self::Exclusive(b.clone()),
        }
    }
}

impl<Q: Clone + Copy> Copy for QueryBound<Q> {}

impl<'a, Q: PartialOrd<K>, K: Ord, T, A, ShallDescend> AugmentedAVLTreeIterator<'a, Q, K, T, A, ShallDescend>
where for<'b> ShallDescend: Fn(&'b A) -> bool
{
    fn new(root: &'a NodeRef<K, T, A>,
           lb: Option<QueryBound<Q>>,
           ub: Option<QueryBound<Q>>,
           shall_descend: &'a ShallDescend) -> Self {
        let mut stack = Vec::new();
        if let Some(rn) = &root.node {
            let ni = rn.iter(&lb, &ub, shall_descend);
            if let Some(ni) = ni {
                stack.push(ni);
            }
        }

        Self{lb, ub, shall_descend, stack}
    }
}

impl<'a, Q: PartialOrd<K>, K: Ord, T, A, ShallDescend>
    Iterator for AugmentedAVLTreeIterator<'a, Q, K, T, A, ShallDescend>
where for<'b> ShallDescend: Fn(&'b A) -> bool
{
    type Item = (&'a K, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(top) = self.stack.last_mut() {
            match top.next() {
                Some(NodeIterVal::Child(cn)) => {
                    if let Some(ni) = cn.iter(&self.lb, &self.ub, self.shall_descend) {
                        self.stack.push(ni);
                    }
                },
                Some(NodeIterVal::Value(v)) => {
                    return Some(v);
                },
                None => {
                    self.stack.pop();
                }
            }
        }
        None
    }
}

impl<'a, Q: PartialOrd<K>, K: Ord, T, A, ShallDescend> AugmentedAVLTreeMutIterator<'a, Q, K, T, A, ShallDescend>
where for<'b> ShallDescend: Fn(&'b A) -> bool
{
    fn new(root: &'a mut NodeRef<K, T, A>,
           lb: Option<QueryBound<Q>>,
           ub: Option<QueryBound<Q>>,
           shall_descend: &'a ShallDescend) -> Self {
        let mut stack = Vec::new();
        if let Some(rn) = &mut root.node {
            let ni = rn.iter_mut(&lb, &ub, shall_descend);
            if let Some(ni) = ni {
                stack.push(ni);
            }
        }

        Self{lb, ub, shall_descend, stack}
    }
}

impl<'a, Q: PartialOrd<K>, K: Ord, T, A, ShallDescend>
    Iterator for AugmentedAVLTreeMutIterator<'a, Q, K, T, A, ShallDescend>
where for<'b> ShallDescend: Fn(&'b A) -> bool
{
    type Item = (&'a K, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(top) = self.stack.last_mut() {
            match top.next() {
                Some(NodeIterMutVal::Child(cn)) => {
                    if let Some(ni) = cn.iter_mut(&self.lb, &self.ub, self.shall_descend) {
                        self.stack.push(ni);
                    }
                },
                Some(NodeIterMutVal::Value(v)) => {
                    return Some(v);
                },
                None => {
                    self.stack.pop();
                }
            }
        }
        None
    }
}

impl<K: Ord, T, A> AugmentedAVLTree<K, T, A> {
    pub fn new() -> Self {
        Self{root: NodeRef::new()}
    }

    pub fn insert<UpdateAux>(&mut self, key: K, val: T, aux: A, update_aux: &UpdateAux)
    where for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        self.root.insert(key, val, aux, update_aux);
    }

    pub fn delete<Q: PartialOrd<K>, IsMatch, ShallDescend, UpdateAux>(&mut self, key: &Q,
                                                                      is_match: &IsMatch, shall_descend: &ShallDescend,
                                                                      update_aux: &UpdateAux) -> Option<(K, T)>
    where
        for<'a> IsMatch: Fn(&'a K, &'a T) -> bool,
        for<'a> ShallDescend: Fn(&'a A) -> bool,
        for<'a> UpdateAux: Fn(&'a mut A, &'a K, Option<&'a A>, Option<&'a A>)
    {
        self.root.delete(key, is_match, shall_descend, update_aux).map(|deleted| deleted.0)
    }

    pub fn iter<'a, Q: PartialOrd<K>, ShallDescend>
        (&'a self,
         lb: Option<QueryBound<Q>>, ub: Option<QueryBound<Q>>,
         shall_descend: &'a ShallDescend)
         -> AugmentedAVLTreeIterator<'a, Q, K, T, A, ShallDescend>
    where for<'b> ShallDescend: Fn(&'b A) -> bool {
        AugmentedAVLTreeIterator::new(&self.root, lb, ub, shall_descend)
    }

    pub fn iter_mut<'a, Q: PartialOrd<K>, ShallDescend>
        (&'a mut self,
         lb: Option<QueryBound<Q>>, ub: Option<QueryBound<Q>>,
         shall_descend: &'a ShallDescend)
         -> AugmentedAVLTreeMutIterator<'a, Q, K, T, A, ShallDescend>
    where for<'b> ShallDescend: Fn(&'b A) -> bool {
        AugmentedAVLTreeMutIterator::new(&mut self.root, lb, ub, shall_descend)
    }
}


#[cfg(test)]
fn test_update_aux<T>(height: &mut u32, _val: &T, lheight: Option<&u32>, rheight: Option<&u32>) {
    match lheight {
        None => {
            match rheight {
                None => *height = 1,
                Some(rheight) => *height = rheight + 1,
            }
        },
        Some(lheight) => {
            match  rheight {
                None => *height = lheight + 1,
                Some(rheight) => *height = core::cmp::max(lheight, rheight) + 1,
            }
        }
    }
}

#[cfg(test)]
fn test_check_node<K: Ord, T>(n: &NodeRef<K, T, u32>) -> u32 {
    match &n.node {
        None => return 0,
        Some(n) => {
            let lheight = test_check_node(&n.left);
            let rheight = test_check_node(&n.right);
            let height = core::cmp::max(lheight, rheight) + 1;
            assert_eq!(n.aux, height);
            if lheight < rheight {
                assert_eq!(lheight + 1, rheight);
                assert_eq!(n.bf, 1);
            } else if lheight > rheight {
                assert_eq!(lheight, rheight + 1);
                assert_eq!(n.bf, -1);
            } else {
                assert_eq!(n.bf, 0);
            }

            let mut lneigh = &n.left.node;
            while let Some(lneighn) = &lneigh{
                assert!(lneighn.key <= n.key);
                lneigh = &lneighn.right.node;
            }

            let mut rneigh = &n.right.node;
            while let Some(rneighn) = &rneigh{
                assert!(n.key <= rneighn.key);
                rneigh = &rneighn.left.node;
            }

            height
        }
    }
}

#[test]
fn test_insert_delete_same() {
    let mut t: AugmentedAVLTree<u32, u32, u32> = AugmentedAVLTree::new();

    for i in 0..128 {
        t.insert(0, i, 0, &test_update_aux);
        test_check_node(&t.root);
        assert_eq!(t.iter(Some(QueryBound::Inclusive(0)),
                          Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   (i + 1) as usize);
        assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(0)),
                              Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   (i + 1) as usize);
    }
    assert_eq!(t.iter(None::<QueryBound<u32>>, None, &|_a| true).count(), 128);
    assert_eq!(t.iter_mut(None::<QueryBound<u32>>, None, &|_a| true).count(), 128);

    for i in 0..128 {
        assert_eq!(t.delete(&0, &|_k, v| *v == i, &|_a| true, &test_update_aux), Some((0, i)));
        test_check_node(&t.root);
        assert_eq!(t.iter(Some(QueryBound::Inclusive(0)),
                          Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   (127 - i) as usize);
        assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(0)),
                              Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   (127 - i) as usize);
    }
    assert_eq!(t.iter(None::<QueryBound<u32>>, None, &|_a| true).count(), 0);
    assert_eq!(t.iter_mut(None::<QueryBound<u32>>, None, &|_a| true).count(), 0);
    assert_eq!(t.root.node.is_none(), true);
}

#[test]
fn test_insert_delete_same_rev() {
    let mut t: AugmentedAVLTree<u32, u32, u32> = AugmentedAVLTree::new();

    for i in 0..128 {
        t.insert(0, i, 0, &test_update_aux);
        test_check_node(&t.root);
        assert_eq!(t.iter(Some(QueryBound::Inclusive(0)),
                          Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   (i + 1) as usize);
        assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(0)),
                              Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   (i + 1) as usize);
    }
    assert_eq!(t.iter(None::<QueryBound<u32>>, None, &|_a| true).count(), 128);
    assert_eq!(t.iter_mut(None::<QueryBound<u32>>, None, &|_a| true).count(), 128);

    for i in (0..128).rev() {
        assert_eq!(t.delete(&0, &|_k, v| *v == i, &|_a| true, &test_update_aux), Some((0, i)));
        test_check_node(&t.root);
        assert_eq!(t.iter(Some(QueryBound::Inclusive(0)),
                          Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   i as usize);
        assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(0)),
                              Some(QueryBound::Exclusive(1)), &|_a| true).count(),
                   i as usize);
    }
    assert_eq!(t.iter(None::<QueryBound<u32>>, None, &|_a| true).count(), 0);
    assert_eq!(t.iter_mut(None::<QueryBound<u32>>, None, &|_a| true).count(), 0);
    assert_eq!(t.root.node.is_none(), true);
}

#[test]
fn test_insert_delete_strided() {
    let mut t: AugmentedAVLTree<u32, u32, u32> = AugmentedAVLTree::new();

    for i in (0..256).step_by(4) {
        for j in [0, 2, 3, 1] {
            t.insert(i + j, i + j, 0, &test_update_aux);
            test_check_node(&t.root);
            assert_eq!(t.iter(Some(QueryBound::Inclusive(i + j)),
                              Some(QueryBound::Exclusive(i + j + 1)), &|_a| true).count(),
                       1);
            assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(i + j)),
                                  Some(QueryBound::Exclusive(i + j + 1)), &|_a| true).count(),
                       1);

            t.insert(i + 256 + j, i + 256 + j, 0, &test_update_aux);
            test_check_node(&t.root);
            assert_eq!(t.iter(Some(QueryBound::Inclusive(i + 256 + j)),
                              Some(QueryBound::Exclusive(i + 256 + j + 1)), &|_a| true).count(),
                       1);
            assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(i + 256 + j)),
                                  Some(QueryBound::Exclusive(i + 256 + j + 1)), &|_a| true).count(),
                       1);
        }
    }
    assert_eq!(t.iter(None::<QueryBound<u32>>, None, &|_a| true).count(), 512);
    assert_eq!(t.iter(Some(QueryBound::Exclusive(0)), Some(QueryBound::Exclusive(511)), &|_a| true).count(), 510);
    assert_eq!(t.iter(Some(QueryBound::Inclusive(0)), Some(QueryBound::Inclusive(511)), &|_a| true).count(), 512);
    assert_eq!(t.iter(Some(QueryBound::Exclusive(192)), Some(QueryBound::Exclusive(320)), &|_a| true).count(), 127);
    assert_eq!(t.iter(Some(QueryBound::Inclusive(192)), Some(QueryBound::Inclusive(320)), &|_a| true).count(), 129);
    assert_eq!(t.iter_mut(None::<QueryBound<u32>>, None, &|_a| true).count(), 512);
    assert_eq!(t.iter_mut(Some(QueryBound::Exclusive(0)), Some(QueryBound::Exclusive(511)), &|_a| true).count(), 510);
    assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(0)), Some(QueryBound::Inclusive(511)), &|_a| true).count(), 512);
    assert_eq!(t.iter_mut(Some(QueryBound::Exclusive(192)), Some(QueryBound::Exclusive(320)), &|_a| true).count(), 127);
    assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(192)), Some(QueryBound::Inclusive(320)), &|_a| true).count(), 129);

    for i in (0..256).step_by(4) {
        for j in [0, 2, 3, 1] {
            assert_eq!(t.delete(&(i + j), &|_k, v| *v == i + j, &|_a| true, &test_update_aux), Some((i + j, i + j)));
            test_check_node(&t.root);
            assert_eq!(t.iter(Some(QueryBound::Inclusive(i + j)),
                              Some(QueryBound::Exclusive(i + j + 1)), &|_a| true).count(),
                       0);
            assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(i + j)),
                                  Some(QueryBound::Exclusive(i + j + 1)), &|_a| true).count(),
                       0);
        }
    }
    assert_eq!(t.iter(None::<QueryBound<u32>>, None, &|_a| true).count(), 256);
    assert_eq!(t.iter(Some(QueryBound::Exclusive(0)), Some(QueryBound::Exclusive(511)), &|_a| true).count(), 255);
    assert_eq!(t.iter(Some(QueryBound::Inclusive(0)), Some(QueryBound::Inclusive(511)), &|_a| true).count(), 256);
    assert_eq!(t.iter(Some(QueryBound::Exclusive(128)), Some(QueryBound::Exclusive(320)), &|_a| true).count(), 64);
    assert_eq!(t.iter(Some(QueryBound::Inclusive(128)), Some(QueryBound::Inclusive(320)), &|_a| true).count(), 65);
    assert_eq!(t.iter_mut(None::<QueryBound<u32>>, None, &|_a| true).count(), 256);
    assert_eq!(t.iter_mut(Some(QueryBound::Exclusive(0)), Some(QueryBound::Exclusive(511)), &|_a| true).count(), 255);
    assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(0)), Some(QueryBound::Inclusive(511)), &|_a| true).count(), 256);
    assert_eq!(t.iter_mut(Some(QueryBound::Exclusive(128)), Some(QueryBound::Exclusive(320)), &|_a| true).count(), 64);
    assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(128)), Some(QueryBound::Inclusive(320)), &|_a| true).count(), 65);

    for i in (0..256).step_by(4).rev() {
        for j in [0, 2, 3, 1].iter().rev() {
            assert_eq!(t.delete(&(i + 256 + j), &|_k, v| *v == i + 256 + j, &|_a| true, &test_update_aux),
                       Some((i + 256 + j, i + 256 + j)));
            test_check_node(&t.root);
            assert_eq!(t.iter(Some(QueryBound::Inclusive(i + 256 + j)),
                              Some(QueryBound::Exclusive(i + 256 + j + 1)), &|_a| true).count(),
                       0);
            assert_eq!(t.iter_mut(Some(QueryBound::Inclusive(i + 256 + j)),
                                  Some(QueryBound::Exclusive(i + 256 + j + 1)), &|_a| true).count(),
                       0);
        }
    }
    assert_eq!(t.iter(None::<QueryBound<u32>>, None, &|_a| true).count(), 0);
    assert_eq!(t.iter_mut(None::<QueryBound<u32>>, None, &|_a| true).count(), 0);
    assert_eq!(t.root.node.is_none(), true);
}
