# zero-cost-structures

This is yet another proof-of-concept library.

The goal here is to enable the use of data abstraction with zero runtime cost in Common Lisp / SBCL. We rely on `values multiple-value-bind` from ANSI CL and `define-declaration variable-information` from CLTL2 environments API, and provide the following functionality:

- define-zero-cost-struct
- with-zero-cost-struct
- with-zero-cost-struct

See [tests.lisp](./tests.lisp) or [the below section](#using-zero-cost-structures-to-eliminate-runtime-cost-of-data-abstraction) for examples.

I have already reached a dead-end with this though; composing zero cost structures aka making a triplet of pairs of ints does not make sense with this approach. See [the section on composing](#composing) after understanding the implementation of `with-zero-cost-struct`.

## A C example of zero runtime cost data abstraction

Consider the below code:

```c
struct twin_float{double a; double b;};

double do_tf(int n){
  double sum = 0;
  for(int i=0; i<n; i++){
    struct twin_float tf = {(double)i, (double)i};
    sum += tf.a;
    sum += tf.b;
  }
  return sum;
}

double do_raw(int n){
  double sum = 0;
  for(int i=0; i<n; i++){
    double a = (double)i;
    double b = (double)i;
    sum += a;
    sum += b;
  }
  return sum;
}
```

Above, the disassembly of `do_tf` and `do_raw` is identical when the code is compiled using `gcc` on a x86-64 intel CPU. Thus, the struct `twin_float` only exists at compile time, and not at runtime - the use of the struct abstraction has no effect on the cpu cycles or memory consumption at runtime.

## A Common Lisp example of non-zero runtime cost data abstraction

As opposed to the above C code, when the below Common Lisp code is compiled, their disassembly (SBCL, 64-bit intel) actually differs and the `do-tf` version contains code that indicates the creation of the structure at runtime.

```lisp
(declaim (inline make-twin-float))
(defstruct (twin-float (:constructor make-twin-float (a b)))
  (a 0.0f0 :type double-float)
  (b 0.0f0 :type double-float))

(defun do-tf (n)
  (declare (optimize speed)
           (type double-float n))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (loop :for i :from 0.0d0 :below n :by 1.0d0
          :do (let ((tf (make-twin-float i i)))
                (declare (dynamic-extent tf))
                (incf sum (twin-float-a tf))
                (incf sum (twin-float-b tf))))
    sum))

(defun do-raw (n)
  (declare (optimize speed)
           (type double-float n))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (loop :for i :from 0.0d0 :below n :by 1.0d0
          :do (let ((a i)
                    (b i))
                (incf sum a)
                (incf sum b)))
    sum))
```

## Using `zero-cost-structures` to eliminate runtime cost of data abstraction

However, the disassembly of `do-zc-tf` below is actually identical to the disassembly of the `do-raw` above!

```lisp
(define-zero-cost-struct zc-twin-float
  (a 0.0d0 :type double-float)
  (b 0.0d0 :type double-float))

(defun do-zc-tf (n)
  (declare (optimize speed)
           (type double-float n))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (loop :for i :from 0.0d0 :below n :by 1.0d0
          :do (with-zero-cost-struct
                  (tf zc-twin-float)
                  (make-zc-twin-float :a i :b i)
                (incf sum (zc-twin-float-a tf))
                (incf sum (zc-twin-float-b tf))))
    sum))
```

## Debugging

The main issue with zero runtime cost abstractions is that because the structure object does not exist at runtime, there is no way to debug the relevant code at run time.

## Composing

To return a composite zero cost structure `(pair (pair a b) (pair c d))` requires that a `(values (values 1 2) (values 3 4))` actually return the... values `1 2 3 4`... which does not make sense.

Further, to create a `(pair (pair a b) (pair c d))` requires that the multiple values of the argument forms to `make-pair` be used rather than ignored. In Common Lisp, only the first value is used; the rest of the values are ignored in a function call: `(make-pair (values 1 2) (values 3 4))` only passes the value 1 and 3 to `make-pair`; the values 3 and 4 are ignored.

Indeed, if you ignore functions altogether, and instead rely heavily on macros, then everything is possible; but at that point, your debug life will probably be hell, and you might rather use rust, C++, or other languages that can allow you to squeeze out more performance. 
