
![lang module="core"]
mod core {
  # Language-fundamental things live here.

  ![lang module="core::iter"]
  mod iter {
    pub interface Iter<T> {
      fn next(self ref) -> Option<T>;
    }
  }

  ![lang module="core::num"]
  mod num {
    # Provides all numeric-primitive types.
  }

  ![lang module="core::str"]
  pub str {
    # Provides `str` type.
  }

  ![lang module="core::slice"]
  mod slice {
    # Provides `Slice<T>` type.
    
    ![lang struct="num::core::slice::Slice"]
    pub struct Slice<T> {
      begin: T ptr;
      end: T ptr;
    }
  }

  ![lang module="core::range"]
  mod range {
    # Provides `Range<T>` type.

    ![lang struct="num::core::range::Range"]
    pub struct Range<T> {
      begin: T;
      end: T;
    }
  }

  ![lang module="core::func"]
  mod func {
    # Provides `Fn<P, R>` type.
  }
}
