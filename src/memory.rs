use crate::object::StrObj;
use fnv::FnvHashSet;
use std::fmt;
use std::ops::Deref;
use std::ptr::NonNull;

pub trait Trace {
    fn trace(&mut self);
}

pub struct Gc<T: Trace + 'static> {
    ptr: NonNull<GcBox<T>>,
}

impl<T: Trace> Trace for Gc<T> {
    fn trace(&mut self) {
        unsafe {
            match (*self.ptr.as_ptr()).header.color {
                Color::White => {
                    (*self.ptr.as_ptr()).header.color = Color::Gray;
                    // trace the objects reachable via this value
                    (*self.ptr.as_ptr()).value.trace();
                    (*self.ptr.as_ptr()).header.color = Color::Black;
                }
                Color::Black | Color::Gray => {}
            }
        }
    }
}

impl<T: Trace> Gc<T> {
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T: Trace> Copy for Gc<T> {}

impl<T: Trace> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Trace> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { (*self.ptr.as_ptr()).value() }
    }
}

impl<T: Trace + fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.deref()))
    }
}

impl<T: Trace + fmt::Display> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.deref()))
    }
}

enum Color {
    White, // undiscovered
    Gray,  // discovered
    Black, // processed
}

struct GcBoxHeader {
    next: Option<NonNull<GcBox<dyn Trace>>>,
    color: Color,
}

struct GcBox<T: Trace + ?Sized + 'static> {
    header: GcBoxHeader,
    value: T,
}

impl<T: Trace> GcBox<T> {
    fn new(value: T) -> NonNull<Self> {
        let gc_box = Box::into_raw(Box::new(GcBox {
            header: GcBoxHeader {
                next: None,
                color: Color::White,
            },
            value: value,
        }));
        unsafe { NonNull::new_unchecked(gc_box) }
    }
}

impl<T: Trace + ?Sized> GcBox<T> {
    fn value(&self) -> &T {
        &self.value
    }
}

// unit-like struct to be used as a dummy head of the linked list of
// objects.
struct DummyHead;
impl Trace for DummyHead {
    fn trace(&mut self) {}
}

pub struct Heap {
    head: NonNull<GcBox<dyn Trace>>,
    interned_str: FnvHashSet<Gc<StrObj>>,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            head: GcBox::new(DummyHead),
            interned_str: FnvHashSet::default(),
        }
    }

    pub fn allocate<T: Trace>(&mut self, val: T) -> Gc<T> {
        // allocate the box
        let gc_box = GcBox::new(val);
        unsafe {
            (*gc_box.as_ptr()).header.next = (*self.head.as_ptr()).header.next;
            (*self.head.as_ptr()).header.next = Some(gc_box);
        }
        Gc { ptr: gc_box }
    }

    pub fn allocate_string(&mut self, val: String) -> Gc<StrObj> {
        if let Some(gc_str) = self.interned_str.get(&val) {
            return gc_str.clone();
        }
        let new_str = self.allocate(StrObj(val));
        self.interned_str.insert(new_str);
        new_str
    }

    pub fn sweep(&mut self) {
        let mut next_box;
        let mut tail_ptr = self.head;
        unsafe { next_box = &(*tail_ptr.as_ptr()).header.next }

        while let Some(gc_box) = next_box {
            unsafe {
                next_box = &(*gc_box.as_ptr()).header.next;
                match (*gc_box.as_ptr()).header.color {
                    Color::White => {
                        // unmarked, so let it drop by giving ownership to a Box
                        let _owning_box = Box::from_raw(gc_box.as_ptr());
                        (*tail_ptr.as_ptr()).header.next = *next_box;
                    }
                    Color::Gray | Color::Black => {
                        // reset the mark
                        (*gc_box.as_ptr()).header.color = Color::White;
                        tail_ptr = *gc_box;
                    }
                }
            }
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        self.sweep();
    }
}
