# fdict #

[![Build Status](https://travis-ci.org/zerothi/fdict.svg?branch=master)](https://travis-ci.org/zerothi/fdict)
[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=NGNU2AA3JXX94&lc=DK&item_name=Papior%2dCodes&item_number=codes&currency_code=EUR&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted)

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

Extract and create an `setup.make` file for compilation, a minimal
`setup.make` file can look like this

	FC=gfortran
	FFLAGS = -g

Type `make` and a library called `libfdict.a` is created.  
Subsequently the installation may be performed by:

    make PREFIX=/papth/to/fdict install

which installs the required files (modules and libraries) to the folder.

To use the dictionary you need to add include statements for the
modules as well as linking to the program.

To link fdict to your program the following can be used in a `Makefile`

    FDICT_PATH  = /path/to/fdict/parent
    FDICT_LIBS  = -L$(FDICT_PATH) -lfdict
    FDICT_INC   = -I$(FDICT_PATH)

The file `fdict.inc` may be included in projects which exposes the following
definitions:

    _FDICT_MAJOR_ 0
    _FDICT_MINOR_ 7
    _FDICT_MICRO_ 0
    _FDICT_VERSION_ 0.7.0

which may be used in functional codes to utilize the correct interfaces.
This is mainly meant as a feature usable when the fdict interface and
e.g. modules change names.


#### Controlling interface parameters ####

__Typically not needed__: allows for customization of different interfaces.

By default the number of dimensions allowed by the library is 3, i.e.
there is no interface created for `real a(:,:,:,:)`, etc. However,
to accomodate arbitrary dimensions you can call a _setup_ script
which initializes a different number of dimensions, which can
be controlled individually.

Run `./setup.sh` to get options regarding the setup.

For instance, if you require interfaces for `real` and `real(kind(0.d0))`
up to 4 dimensions and all others up to 3 dimensions you can do this

    # -A == all data-types, s = single, d = double
    ./setup.sh -A 3 -s 4 -d 4
    # -R is a shorthand for both -s and -d
    ./setup.sh -A 3 -R 4


### variable ###

Using this module one gains access to a generic type variable which
can contain _any_ data format.  
It currently supports the following data-types:

| Type             | Precision format           | C-type               |
|------------------|----------------------------|----------------------|
| `integer`        | `selected_int_kind(4)`     | `short`              |
| `integer`        | `selected_int_kind(9)`     | `int`                |
| `integer`        | `selected_int_kind(18)`    | `long`               |
| `real`           | `selected_real_kind(p=6)`  | `float`              |
| `real`           | `selected_real_kind(p=15)` | `double`             |
| `complex`        | `selected_real_kind(p=6)`  | `float complex`      |
| `complex`        | `selected_real_kind(p=15)` | `double complex`     |
| `type(c_ptr)`    |                            | `void *`             |
| `type(c_funptr)` |                            | (procedure) `void *` |


Basically it is used like this:

    use variable
	integer :: a(3)
	type(variable_t) :: v
	a = 2
	call assign(v,a)
	a = 3
	call assign(a,v)

Also the variable contains an abbreviation for assigning pointers to 
not copy data, but retain data locality:

	integer, target :: a(3)
	type(variable_t) :: v
	a = 2
	call associate(v,a)
	a = 3
	! Now v contains a = 3

To delete a variable one simply does:

	use variable
	type(variable_t) :: v
	call delete(v)

However, when the variable is using pointers, instead the user can do

	use variable
	type(variable_t) :: v
	call delete(v,dealloc=.false.)
	! or
	call nullify(v)

which merely destroys the variable object and thus retains the data
where it is. As with any other pointer arithmetic it is up to the programmer
to ensure there is no memory leaks.


### dictionary ###

Using `type(variable_t)` it becomes easy to create dictionaries in fortran.

Using this module we implement a dictionary which can contain _any_ data
format using a `key:val` based formalism. The underlying data structure is a
linked list sorted according to hash-values of the keys. Hence searching 
for specific elements in the dictionary is _extremely_ fast. Searching a
dictionary with 100 keys 300000 times takes less than 0.04 seconds on
a Haswell laptop.
Concatenating dictionaries is also very fast.

Creating a dictionary is almost as easy as the Python equivalent:

	use dictionary
	type(dictionary_t) :: dict
	dict = ('KEY'.kv.1)

To extend a dictionary one uses the concatenating format:

	dict = dict // ('Hello'.kv.'world') // ('No'.kv.'world')

Again as is used by the `type(variable_t)` one can with benefit use `.kvp.` to create
the dictionary value by pointers instead of copying the content.  
Hence doing:

	real :: r(4)
	dict = dict // ('reals'.kvp.r)
	r = 4

will change the value in the dictionary.  
Note that one can easily create memory leaks with dictionaries:

	use dictionary
	type(dictionary_t) :: dict
	dict = ('KEY'.kv.1)
	dict = dict // ('KEY'.kv.2)
	dict = ('KEY'.kv.3)

The 1st assignement is valid since the dictionary is empty.
The 2nd assignment concatenates and does not produce any memory leaks.
In that case the old key `KEY` is deleted and the new value `2` is inserted.
The 3rd assignment produces a memory leak since the pointer to the original
dictionary gets lost. Be sure to call `call delete(dict)` prior to single
assignments.


Note that the dictionary can also contain _any_ data type.

However, if it needs to do custom data-types the programmer needs to
extend the code by supplying a few custom routines.

Intrinsically the dictionary can contain dictionaries by this:

	use dictionary
	type(dictionary_t) :: d1, d2
	d1 = ('hello'.kv.'world')
	d2 = ('hello'.kv.'world')
	d1 = d1 // ('dict'.kvp.d2)

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

The fdict license is [MPL-2.0][mpl-2], see the LICENSE file.

## Thanks ##

A big thanks goes to Alberto Garcia for contributing ideas and giving
me bug reports. Without him the interface would have been much more
complex!

<!---
Links to external and internal sites.
-->
[fdict@git]: https://github.com/zerothi/fdict
<!-- [fdict-doc]: https://github.com/zerothi/fdict/wiki -->
[issue]: https://github.com/zerothi/fdict/issues
[pr]: https://github.com/zerothi/fdict/pulls
[mpl-2]: https://opensource.org/licenses/MPL-2.0
