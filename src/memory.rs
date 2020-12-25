use std::fmt;
use std::ops::Deref;
use std::ptr::NonNull;

pub trait Object {}

pub struct Heap {
    head: Option<NonNull<GcBox<dyn Object>>>,
}

impl Heap {
    pub fn new() -> Self {
        Heap { head: None }
    }

    pub fn allocate<T: Object>(&mut self, val: T) -> Gc<T> {
        // allocate the box
        let gc_box = GcBox::new(val);
        match self.head {
            Some(curr_head) => {
                unsafe {
                    (*gc_box.as_ptr()).header.next = Some(curr_head);
                }
                self.head = Some(gc_box);
            }
            None => {
                self.head = Some(gc_box);
            }
        }
        Gc { ptr: gc_box }
    }

    pub fn sweep(&mut self) {
        let mut next_box;
        let mut tail_ptr;
        match self.head {
            Some(curr_head) => {
                tail_ptr = curr_head;
                unsafe { next_box = &(*curr_head.as_ptr()).header.next }
            }
            None => {
                return;
            } // empty list
        }
        let head_p = tail_ptr;

        while let Some(gc_box) = next_box {
            unsafe {
                next_box = &(*gc_box.as_ptr()).header.next;
                if !(*gc_box.as_ptr()).header.marked {
                    // unmarked, so let it drop by giving ownership to a Box
                    let _owning_box = Box::from_raw(gc_box.as_ptr());
                    (*tail_ptr.as_ptr()).header.next = *next_box;
                } else {
                    // reset the mark
                    (*gc_box.as_ptr()).header.marked = false;
                    tail_ptr = *gc_box;
                }
            }
        }

        unsafe {
            if !(*head_p.as_ptr()).header.marked {
                // head is unmarked too, so drop it the same way
                let owning_box = Box::from_raw(head_p.as_ptr());
                self.head = owning_box.header.next;
            } else {
                // reset the mark
                (*head_p.as_ptr()).header.marked = false;
            }
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        println!("destroying the heap");
        self.sweep();
    }
}

pub struct Gc<T: Object + 'static> {
    ptr: NonNull<GcBox<T>>,
}

impl<T: Object> Copy for Gc<T> {}

impl<T: Object> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Object> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { (*self.ptr.as_ptr()).value() }
    }
}

impl<T: Object + fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.deref()))
    }
}

struct GcBoxHeader {
    next: Option<NonNull<GcBox<dyn Object>>>,
    marked: bool,
}

struct GcBox<T: Object + ?Sized + 'static> {
    header: GcBoxHeader,
    value: T,
}

impl<T: Object> GcBox<T> {
    fn new(value: T) -> NonNull<Self> {
        let gc_box = Box::into_raw(Box::new(GcBox {
            header: GcBoxHeader {
                next: None,
                marked: false,
            },
            value: value,
        }));
        unsafe { NonNull::new_unchecked(gc_box) }
    }
}

impl<T: Object + ?Sized> GcBox<T> {
    fn value(&self) -> &T {
        &self.value
    }
}

#[derive(Debug)]
pub struct StrObj(pub String);

impl Object for StrObj {}
