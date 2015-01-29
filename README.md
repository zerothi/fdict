fvar
====

Supplying a variable module which does not restrict the type used.

Using this module one gains access to a generic type variable which
can contain *any* data format.

Basically it is used like this:
```
integer :: a(3)
type(var) :: v
a = 2
call assign(v,a)
a = 3
call assign(a,v)
```

Also the variable contains an abbreviation for assigning pointers to 
not copy data, but retain data locality.
```
integer, target :: a(3)
type(var) :: v
a = 2
call associate(v,a)
a = 3
! Now v contains a = 3
```

To delete a variable one simply does:
```
type(var) :: v
call delete(v)
```
However, when the variable is using pointers, instead the user can do
```
type(var) :: v
call delete(v,dealloc=.false.)
```
which merely destroys the variable object and thus retains the data
where it is. As with any other pointer arithmetic it is up to the programmer
to ensure no memory leaks.


dictionary
==========

Using the `type(var)` it becomes easy to create dictionaries in fortran.

Using this module we implement a dictionary which can contain *any* data
format using a `key:val` based formalism. The underlying data structure is a
linked list sorted according to hash-values of the keys. Hence searching 
for specific elements in the dictionary is *extremely* fast. Concatenating 
dictionaries is also very fast.

Creating a dictionary is almost as easy as the Python equivalent:
```
type(dict) :: dic
dic = ('KEY'.kv.1)
```
To extend a dictionary one uses the concatenating format:
```
dic = dic // ('Hello'.kv.'world') // ('No'.kv.'world')
```
Again as is used by the `type(var)` one can with benefit use `.kvp.` to create
the dictionary value by pointers instead of copying the content.  
Hence doing:
```
real :: r(4)
dic = dic // ('reals'.kvp.r)
r = 4
```
will change the value in the dictionary.

Note that the dictionary can also contain *any* data type.
However, if it needs to do custom data-types the programmer needs to
extend the code by supplying a few custom routines.

Intrinsically the dictionary can contain dictionaries by this:
```
type(dict) :: d1, d2
d1 = ('hello'.kv.'world')
d2 = ('hello'.kv.'world')
d1 = d1 // ('dict'.kv.d2)
```
But it will be up to the user to *know* the key for data types other than
integers, reals, complex numbers and characters.