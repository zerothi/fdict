# fdict #

A variable and dictionary in pure fortran for retaining any data-type
and a fast hash-table dictionary.

## Usage ##

This module consists of two separate modules which co-exist for
maintenance and usage reasons.

First, the variable module which is a type-free variable that can contain
any variable type, and any dimension as well.

Second, the dictionary module which contains a hash-table of variables
that can contain _any_ data-type allowed by the variable module.

## Downloading and installation ##

Installing fdict requires a download of the library 
hosted at [github](https://github.com/) at [fdict@git].

Extract and create an `arch.make` file for compilation, a minimal
`arch.make` file can look like this

	FC=gfortran
	FFLAGS = -g

	.F90.o:
	    $(FC) -c $(INC) $(FFLAGS) $<

	.f90.o:
	    $(FC) -c $(INC) $(FFLAGS) $<

Type `make` and a library called `libvardict.a` is created.

To use the dictionary you need to add include statements for the
modules as well as linking to the program.

To link fdict to your program the following can be used in a `Makefile`

    FDICT_PATH  = /path/to/fdict/parent
    FDICT_LIBS  = -L$(FDICT_PATH) -lvardict
    FDICT_INC   = -I$(FDICT_PATH)


### variable ###

Using this module one gains access to a generic type variable which
can contain _any_ data format.

Basically it is used like this:
```
use variable
integer :: a(3)
type(var) :: v
a = 2
call assign(v,a)
a = 3
call assign(a,v)
```

Also the variable contains an abbreviation for assigning pointers to 
not copy data, but retain data locality:
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
use variable
type(var) :: v
call delete(v)
```
However, when the variable is using pointers, instead the user can do
```
use variable
type(var) :: v
call delete(v,dealloc=.false.)
! or
call nullify(v)
```
which merely destroys the variable object and thus retains the data
where it is. As with any other pointer arithmetic it is up to the programmer
to ensure no memory leaks.


### dictionary ###

Using `type(var)` it becomes easy to create dictionaries in fortran.

Using this module we implement a dictionary which can contain _any_ data
format using a `key:val` based formalism. The underlying data structure is a
linked list sorted according to hash-values of the keys. Hence searching 
for specific elements in the dictionary is _extremely_ fast. Concatenating 
dictionaries is also very fast.

Creating a dictionary is almost as easy as the Python equivalent:
```
use dictionary
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

Note that the dictionary can also contain _any_ data type.

However, if it needs to do custom data-types the programmer needs to
extend the code by supplying a few custom routines.

Intrinsically the dictionary can contain dictionaries by this:
```
use dictionary
type(dict) :: d1, d2
d1 = ('hello'.kv.'world')
d2 = ('hello'.kv.'world')
d1 = d1 // ('dict'.kvp.d2)
```
But it will be up to the user to _know_ the key for data types other than
integers, reals, complex numbers and characters.

Note that the dictionary contained is passed by reference, and thus
if you delete `d2`, you will have a dangling pointer in `d1`.


## Contributions, issues and bugs ##

I would advice any users to contribute as much feedback and/or PRs to further
maintain and expand this library.

Please do not hesitate to contribute!

If you find any bugs please form a [bug report/issue][issue].

If you have a fix please consider adding a [pull request][pr].


## License ##

The fdict license is [lgpl], see the LICENSE file.

## Thanks ##

A big thanks goes to Alberto Garcia for contributing ideas and given
me bug-reports.

<!---
Links to external and internal sites.
-->
[fdict@git]: https://github.com/zerothi/fdict
<!-- [fdict-doc]: https://github.com/zerothi/fdict/wiki -->
[issue]: https://github.com/zerothi/fdict/issues
[pr]: https://github.com/zerothi/fdict/pulls
[lgpl]: http://www.gnu.org/licenses/lgpl.html
