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
}

#[derive(Debug)]
pub struct Gc<T: Object + 'static> {
    ptr: NonNull<GcBox<T>>,
}

impl<T: Object> Copy for Gc<T> {}

impl<T: Object> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
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
pub struct StrObj(String);

impl Object for StrObj {}
