// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
//
// https://github.com/contain-rs/linked-list/

//! An alternative implementation of `std::collections::LinkedList`, featuring experimental
//! Cursor-based APIs.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter;
use std::marker::PhantomData;
use std::{mem, ptr};

// FIXME(Gankro): Although the internal interface we have here is *safer* than std's LinkedList,
// it's still by no means safe. Any claims we make here about safety in the internal APIs
// are complete hand-waving. For now I'm leaving it like this while we work on better solutions.

/// A LinkedList node.
struct Node<T> {
    prev: Raw<T>,
    next: Link<T>,
    elem: T,
}

impl<T> Node<T> {
    /// Makes a node with the given element.
    #[inline]
    fn new(elem: T) -> Self {
        Node {
            prev: Raw::none(),
            next: None,
            elem,
        }
    }

    /// Joins two lists.
    #[inline]
    fn link(&mut self, mut next: Box<Self>) {
        next.prev = Raw::some(self);
        self.next = Some(next);
    }

    /// Makes the given node come after this one, appropriately setting all other links.
    /// Assuming that self has a `next`.
    #[inline]
    fn splice_next(&mut self, mut next: Box<Self>) {
        let mut old_next = self.next.take();
        if let Some(node) = old_next.as_mut() {
            node.prev = Raw::some(&mut *next);
        }
        /*
        old_next
            .as_mut()
            .map(|node| node.prev = Raw::some(&mut *next));
        */
        next.prev = Raw::some(self);
        next.next = old_next;
        self.next = Some(next);
    }

    /// Takes the next node from this one, breaking the list into two correctly linked lists.
    #[inline]
    fn take_next(&mut self) -> Option<Box<Self>> {
        let mut next = self.next.take();
        if let Some(node) = next.as_mut() {
            node.prev = Raw::none();
        }
        // next.as_mut().map(|node| node.prev = Raw::none());
        next
    }
}

/// An owning link.
type Link<T> = Option<Box<Node<T>>>;

/// A non-owning link, based on a raw ptr.
struct Raw<T> {
    ptr: *const Node<T>,
}

impl<T> Raw<T> {
    /// Makes a null reference.
    #[inline]
    fn none() -> Self {
        Raw {
            ptr: ptr::null_mut(),
        }
    }

    /// Makes a reference to the given node.
    #[inline]
    fn some(ptr: &mut Node<T>) -> Self {
        Raw { ptr }
    }

    /// Converts the ref to an Option containing a reference.
    #[inline]
    fn as_ref(&self) -> Option<&Node<T>> {
        unsafe {
            if self.ptr.is_null() {
                None
            } else {
                // 100% legit (no it's not)
                Some(&*self.ptr)
            }
        }
    }

    /// Converts the ref to an Option containing a mutable reference.
    #[inline]
    fn as_mut(&mut self) -> Option<&mut Node<T>> {
        unsafe {
            if self.ptr.is_null() {
                None
            } else {
                // 100% legit (no it's not)
                Some(&mut *(self.ptr as *mut Node<T>))
            }
        }
    }

    /// Takes the reference out and nulls out this one.
    #[inline]
    fn take(&mut self) -> Self {
        mem::replace(self, Raw::none())
    }

    /// Clones this reference. Note that mutability differs from standard clone.
    /// We don't want these to be cloned in the immutable case.
    #[inline]
    fn clone(&mut self) -> Self {
        Raw { ptr: self.ptr }
    }
}

/// An experimental rewrite of LinkedList to provide a more cursor-oriented API.
pub struct LinkedList<T> {
    len: usize,
    head: Link<T>,
    tail: Raw<T>,
}

impl<T> LinkedList<T> {
    /// Returns an empty `LinkedList`.
    #[inline]
    pub fn new() -> Self {
        LinkedList {
            head: None,
            tail: Raw::none(),
            len: 0,
        }
    }

    /// Appends the given element to the back of the list.
    pub fn push_back(&mut self, elem: T) {
        self.len += 1;
        let mut node = Box::new(Node::new(elem));
        // unconditionally make the new node the new tail
        let mut old_tail = mem::replace(&mut self.tail, Raw::some(&mut *node));
        match old_tail.as_mut() {
            // List was empty, so the new node is the new head
            None => self.head = Some(node),
            // List wasn't empty, just need to append this to the old tail
            Some(tail) => tail.link(node),
        }
    }

    /// Appends the given element to the front of the list.
    pub fn push_front(&mut self, elem: T) {
        self.len += 1;
        let mut node = Box::new(Node::new(elem));
        match self.head.take() {
            // List was empty, so the new node is the new tail
            None => self.tail = Raw::some(&mut *node),
            // List wasn't empty, append the old head to the new node
            Some(head) => node.link(head),
        }
        // unconditionally make the new node the new head
        self.head = Some(node);
    }

    /// Removes the element at the back of the list and returns it.
    ///
    /// Returns `None` if the list was empty.
    pub fn pop_back(&mut self) -> Option<T> {
        // null out the list's tail pointer unconditionally
        self.tail.take().as_mut().and_then(|tail| {
            // tail pointer wasn't null, so decrease the len
            self.len -= 1;
            match tail.prev.take().as_mut() {
                // tail had no previous value, so the list only contained this node.
                // So we have to take this node out by removing the head itself
                None => self.head.take().map(|node| node.elem),
                // tail had a previous value, so we need to make that the new tail
                // and take the node out of its next field
                Some(prev) => {
                    self.tail = Raw::some(prev);
                    prev.next.take().map(|node| node.elem)
                }
            }
        })
    }

    /// Removes the element at the front of the list and returns it.
    ///
    /// Returns `None` if the list was empty.
    pub fn pop_front(&mut self) -> Option<T> {
        // null out the list's head pointer unconditionally
        self.head.take().map(|mut head| {
            // head wasn't null, so decrease the len
            self.len -= 1;
            match head.take_next() {
                // head had no next value, so just null out the tail
                None => self.tail = Raw::none(),
                // head had a next value, which should be the new head
                Some(next) => self.head = Some(next),
            }
            head.elem
        })
    }

    /// Returns a reference to the element at the front of the list.
    ///
    /// Returns `None` if the list is empty.
    #[inline]
    pub fn front(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }

    /// Returns a reference to the element at the back of the list.
    ///
    /// Returns `None` if the list is empty.
    #[inline]
    pub fn back(&self) -> Option<&T> {
        self.tail.as_ref().map(|node| &node.elem)
    }

    /// Returns a mutable reference to the element at the front of the list.
    ///
    /// Returns `None` if the list is empty.
    #[inline]
    pub fn front_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.elem)
    }

    /// Returns a mutable reference to the element at the back of the list.
    ///
    /// Returns `None` if the list is empty.
    #[inline]
    pub fn back_mut(&mut self) -> Option<&mut T> {
        self.tail.as_mut().map(|node| &mut node.elem)
    }

    /// Inserts the given element into the list at the given index.
    ///
    /// # Panics
    ///
    /// Panics if the index is greater than the length of the list.
    #[inline]
    pub fn insert(&mut self, index: usize, elem: T) {
        assert!(index <= self.len(), "index out of bounds");
        let mut cursor = self.cursor();
        cursor.seek_forward(index);
        cursor.insert(elem);
    }

    /// Removes the element at the given index and returns it.
    ///
    /// Returns `None` if the index is greater than or equal to the length of the list.
    #[inline]
    pub fn remove(&mut self, index: usize) -> Option<T> {
        if index >= self.len() {
            None
        } else {
            let mut cursor = self.cursor();
            cursor.seek_forward(index);
            cursor.remove()
        }
    }

    /// Splits the list into two lists at the given index. Returns the right side of the split.
    /// Returns an empty list if index is out of bounds.
    ///
    /// This method is deprecated in favor of [`split_off`](#method.split_off) and will be removed
    /// in a future release.
    pub fn split_at(&mut self, index: usize) -> Self {
        if index >= self.len() {
            Self::new()
        } else {
            let mut cursor = self.cursor();
            cursor.seek_forward(index);
            cursor.split()
        }
    }

    /// Splits the list in two at the given index.
    ///
    /// After this method returns, `self` contains the elements that previously lay in the range
    /// `[0, index)`, and the returned list contains the elements that previously lay in the range
    /// `[index, len)`.
    ///
    /// # Panics
    ///
    /// Panics if the given index is greater than the length of the list.
    pub fn split_off(&mut self, index: usize) -> Self {
        assert!(
            index <= self.len(),
            "Cannot split off at a nonexistent index"
        );
        let mut cursor = self.cursor();
        cursor.seek_forward(index);
        cursor.split()
    }

    /// Appends the given list to the end of this one. The old list will be empty afterwards.
    pub fn append(&mut self, other: &mut Self) {
        let mut cursor = self.cursor();
        cursor.prev();
        cursor.splice(other);
    }

    /// Inserts the given list at the given index. The old list will be empty afterwards.
    pub fn splice(&mut self, index: usize, other: &mut Self) {
        let mut cursor = self.cursor();
        cursor.seek_forward(index);
        cursor.splice(other);
    }

    /// Returns the number of elements in the list.
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Checks if the list is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Removes all elements from the list.
    #[inline]
    pub fn clear(&mut self) {
        while !self.is_empty() {
            self.pop_front();
        }
    }

    /// Returns a cursor over the list.
    #[inline]
    pub fn cursor(&mut self) -> Cursor<T> {
        Cursor {
            list: self,
            prev: Raw::none(),
            index: 0,
        }
    }

    /// Returns a forward iterator that yields references to the list's elements.
    #[inline]
    pub fn iter(&self) -> Iter<T> {
        Iter {
            nelem: self.len(),
            head: &self.head,
            tail: &self.tail,
        }
    }

    /// Returns a forward iterator that yields mutable references to the list's elements.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<T> {
        let head_raw = match self.head.as_mut() {
            Some(head) => Raw::some(&mut **head),
            None => Raw::none(),
        };
        IterMut {
            nelem: self.len(),
            head: head_raw,
            tail: self.tail.clone(),
            phantom: PhantomData,
        }
    }
}

/// A cursor over a `LinkedList`.
///
/// A `Cursor` is like an iterator, except that it can freely seek back-and-forth, and can
/// safely mutate the list during iteration. This is because the lifetime of its yielded
/// references is tied to its own lifetime, instead of just the underlying list. This means
/// cursors cannot yield multiple elements at once.
///
/// Cursors always rest between two elements in the list, and index in a logically circular way.
/// To accommodate this, there is a "ghost" non-element that yields `None` between the head and
/// tail of the list.
///
/// When created, cursors start between the ghost and the front of the list. That is, `next` will
/// yield the front of the list, and `prev` will yield `None`. Calling `prev` again will yield
/// the tail.
pub struct Cursor<'a, T: 'a> {
    list: &'a mut LinkedList<T>,
    prev: Raw<T>,
    // index of `next`, where the ghost is at `len`.
    index: usize,
}

// Note, the Cursor's ops and repr are specifically designed so that the cursor's reference
// into the list never needs to update after an operation. It always mutates in front of
// itself. Also to gain ownership of a node, we generally need a ref to the previous Node.
// This is why we hold `prev` rather than `next`.

impl<'a, T> Cursor<'a, T> {
    /// Resets the cursor to lie between the first and last element in the list.
    #[inline]
    pub fn reset(&mut self) {
        self.prev = Raw::none();
        self.index = 0;
    }

    /// Gets the next element in the list.
    pub fn next(&mut self) -> Option<&mut T> {
        self.index += 1;
        match self.prev.take().as_mut() {
            // We had no previous element; the cursor was sitting at the start position
            // Next element should be the head of the list
            None => match self.list.head {
                // No head. No elements.
                None => {
                    self.index = 0;
                    None
                }
                // Got the head. Set it as prev and yield its element
                Some(ref mut head) => {
                    self.prev = Raw::some(&mut **head);
                    Some(&mut head.elem)
                }
            },
            // We had a previous element, so let's go to its next
            Some(prev) => match prev.next {
                // No next. We're back at the start point, null the prev and yield None
                None => {
                    self.index = 0;
                    self.prev = Raw::none();
                    None
                }
                // Got a next. Set it as prev and yield its element
                Some(ref mut next) => {
                    self.prev = Raw::some(&mut **next);
                    unsafe {
                        // upgrade the lifetime
                        Some(mem::transmute(&mut next.elem))
                    }
                }
            },
        }
    }

    /// Gets the previous element in the list.
    pub fn prev(&mut self) -> Option<&mut T> {
        match self.prev.take().as_mut() {
            // No prev. We're at the start of the list. Yield None and jump to the end.
            None => {
                self.prev = self.list.tail.clone();
                self.index = self.list.len();
                None
            }
            // Have a prev. Yield it and go to the previous element.
            Some(prev) => {
                self.index -= 1;
                self.prev = prev.prev.clone();
                unsafe {
                    // upgrade the lifetime
                    Some(mem::transmute(&mut prev.elem))
                }
            }
        }
    }

    /// Gets the next element in the list, without moving the cursor head.
    pub fn peek_next(&mut self) -> Option<&mut T> {
        let Cursor {
            ref mut list,
            ref mut prev,
            ..
        } = *self;
        match prev.as_mut() {
            None => list.front_mut(),
            Some(prev) => prev.next.as_mut().map(|next| &mut next.elem),
        }
    }

    /// Gets the previous element in the list, without moving the cursor head.
    pub fn peek_prev(&mut self) -> Option<&mut T> {
        self.prev.as_mut().map(|prev| &mut prev.elem)
    }

    /// Inserts an element at the cursor's location in the list, and moves the cursor head to
    /// lie before it. Therefore, the new element will be yielded by the next call to `next`.
    pub fn insert(&mut self, elem: T) {
        // destructure so that we can mutate list while working with prev
        let Cursor {
            ref mut list,
            ref mut prev,
            ..
        } = *self;
        match prev.as_mut() {
            // No prev, we're at the start of the list
            // Also covers empty list
            None => list.push_front(elem),
            Some(node) => {
                if node.next.as_mut().is_none() {
                    // No prev.next, we're at the end of the list
                    list.push_back(elem);
                } else {
                    // We're somewhere in the middle, splice in the new node
                    list.len += 1;
                    node.splice_next(Box::new(Node::new(elem)));
                }
            }
        }
    }

    /// Removes the next element in the list, without moving the cursor. Returns None if the list
    /// is empty, or if `next` is the ghost element
    pub fn remove(&mut self) -> Option<T> {
        let Cursor {
            ref mut list,
            ref mut prev,
            ..
        } = *self;
        match prev.as_mut() {
            // No prev, we're at the start of the list
            // Also covers empty list
            None => list.pop_front(),
            Some(prev) => match prev.take_next() {
                // No prev.next, we're at the ghost, yield None
                None => None,
                // We're somewhere in the middle, rip out prev.next
                Some(mut next) => {
                    list.len -= 1;
                    match next.next.take() {
                        // We were actually at the end of the list, so fix the list's tail
                        None => list.tail = Raw::some(prev),
                        // Really in the middle, link the results of removing next
                        Some(next_next) => prev.link(next_next),
                    }
                    Some(next.elem)
                }
            },
        }
    }

    /// Splits the list into two at the cursor's current position. This will return a new list
    /// consisting of everything after the cursor, with the original list retaining everything
    /// before. The cursor will then lie between the tail and the ghost.
    pub fn split(&mut self) -> LinkedList<T> {
        let Cursor {
            ref mut list,
            ref mut prev,
            index,
        } = *self;
        let new_tail = prev.clone();
        let len = list.len();
        match prev.as_mut() {
            // We're at index 0. The new list is the whole list, so just swap
            None => mem::replace(*list, LinkedList::new()),
            // We're not at index 0. This means we don't have to worry about fixing
            // the old list's head.
            Some(prev) => {
                let next_tail = list.tail.clone();
                list.len = index;
                list.tail = new_tail; // == prev
                let next_head = prev.take_next();

                LinkedList {
                    head: next_head,
                    tail: next_tail,
                    len: len - index,
                }
            }
        }
    }

    /// Inserts the entire list's contents right after the cursor.
    pub fn splice(&mut self, other: &mut LinkedList<T>) {
        if other.is_empty() {
            return;
        }
        let len = other.len;
        other.len = 0;
        let mut head = other.head.take();
        let mut tail = other.tail.take();
        let Cursor {
            ref mut list,
            ref mut prev,
            ..
        } = *self;

        list.len += len;
        match prev.as_mut() {
            // We're at the head of the list
            None => match list.head.take() {
                // self list is empty, should just be the other list
                None => {
                    list.head = head;
                    list.tail = tail;
                }
                // self list isn't empty
                Some(self_head) => {
                    list.head = head;
                    tail.as_mut().unwrap().link(self_head);
                }
            },
            // Middle or end
            Some(prev) => match prev.take_next() {
                // We're at the end of the list
                None => {
                    prev.link(head.take().unwrap());
                    list.tail = tail;
                }
                // We're strictly in the middle. Self's head and tail won't change
                Some(next) => {
                    prev.link(head.take().unwrap());
                    tail.as_mut().unwrap().link(next);
                }
            },
        }
    }

    /// Calls `next` the specified number of times.
    pub fn seek_forward(&mut self, by: usize) {
        for _ in 0..by {
            self.next();
        }
    }

    /// Calls `prev` the specified number of times.
    pub fn seek_backward(&mut self, by: usize) {
        for _ in 0..by {
            self.prev();
        }
    }
}

/// An iterator over references to the items of a `LinkedList`.
pub struct Iter<'a, T: 'a> {
    head: &'a Link<T>,
    tail: &'a Raw<T>,
    nelem: usize,
}

/// An iterator over mutable references to the items of a `LinkedList`.
pub struct IterMut<'a, T: 'a> {
    head: Raw<T>,
    tail: Raw<T>,
    nelem: usize,
    phantom: PhantomData<&'a mut T>,
}

/// An iterator over the items of a `LinkedList`.
#[derive(Clone)]
pub struct IntoIter<T> {
    list: LinkedList<T>,
}

impl<'a, T> Clone for Iter<'a, T> {
    fn clone(&self) -> Self {
        Iter { ..*self }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        if self.nelem == 0 {
            return None;
        }
        self.head.as_ref().map(|head| {
            self.nelem -= 1;
            self.head = &head.next;
            &head.elem
        })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.nelem, Some(self.nelem))
    }
}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a T> {
        if self.nelem == 0 {
            return None;
        }
        self.tail.as_ref().map(|tail| {
            self.nelem -= 1;
            self.tail = &tail.prev;
            &tail.elem
        })
    }
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<&'a mut T> {
        if self.nelem == 0 {
            return None;
        }
        self.head.take().as_mut().map(|next| {
            self.nelem -= 1;
            self.head = match next.next {
                Some(ref mut node) => Raw::some(&mut **node),
                None => Raw::none(),
            };
            unsafe {
                //upgrade ref to the necessary lifetime
                &mut *((&mut next.elem) as *mut _)
            }
        })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.nelem, Some(self.nelem))
    }
}

impl<'a, T> DoubleEndedIterator for IterMut<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a mut T> {
        if self.nelem == 0 {
            return None;
        }
        self.tail.take().as_mut().map(|prev| {
            self.nelem -= 1;
            self.tail = prev.prev.clone();
            unsafe {
                //upgrade ref to the necessary lifetime
                &mut *((&mut prev.elem) as *mut _)
            }
        })
    }
}

impl<'a, T> ExactSizeIterator for IterMut<'a, T> {}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        self.list.pop_front()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.list.len(), Some(self.list.len()))
    }
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
        self.list.pop_back()
    }
}

impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        self.clear();
    }
}

impl<T> Default for LinkedList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> iter::FromIterator<T> for LinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut ret = LinkedList::new();
        ret.extend(iter);
        ret
    }
}

impl<T> Extend<T> for LinkedList<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for elt in iter {
            self.push_back(elt);
        }
    }
}

impl<'a, T: 'a + Copy> Extend<&'a T> for LinkedList<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        self.extend(iter.into_iter().cloned());
    }
}

impl<T: PartialEq> PartialEq for LinkedList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().eq(other)
    }
}

impl<T: Eq> Eq for LinkedList<T> {}

impl<T: PartialOrd> PartialOrd for LinkedList<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iter().partial_cmp(other)
    }
}

impl<T: Ord> Ord for LinkedList<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.iter().cmp(other)
    }
}

impl<T: fmt::Debug> fmt::Debug for LinkedList<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self).finish()
    }
}

impl<T: Hash> Hash for LinkedList<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.len().hash(state);
        for elt in self.iter() {
            elt.hash(state);
        }
    }
}

impl<T: Clone> Clone for LinkedList<T> {
    fn clone(&self) -> Self {
        self.iter().cloned().collect()
    }
}

impl<'a, T> IntoIterator for &'a LinkedList<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut LinkedList<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;
    fn into_iter(self) -> IterMut<'a, T> {
        self.iter_mut()
    }
}

impl<T> IntoIterator for LinkedList<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> IntoIter<T> {
        IntoIter { list: self }
    }
}

unsafe impl<T: Send> Send for LinkedList<T> {}
unsafe impl<T: Sync> Sync for LinkedList<T> {}

unsafe impl<'a, T: Send> Send for Iter<'a, T> {}
unsafe impl<'a, T: Sync> Sync for Iter<'a, T> {}

unsafe impl<'a, T: Send> Send for IterMut<'a, T> {}
unsafe impl<'a, T: Sync> Sync for IterMut<'a, T> {}

unsafe impl<'a, T: Send> Send for Cursor<'a, T> {}
unsafe impl<'a, T: Sync> Sync for Cursor<'a, T> {}
