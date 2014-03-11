! When dealing with preprocessors that
! are passed by defines we need it to be
! passed further down.
! Hence we use one extra level of expansion

! Concatenation routine
#define _cc2_1(x,y) _cc2_2(x,y)
#define _cc2_2(x,y) x ## y
#define _cc3_1(x,y,z) _cc3_2(x,y,z)
#define _cc3_2(x,y,z) x ## y ## z

! String routine
#define _str1(x) _str2(x)
#define _str2(x) #x


! Common macros to create names etc
#define ROUTINE(base,var) _cc3_1(base,_,var)
#define STR(x) _str1(x)
#define THIS(x) this ## % ## x


! Allocate routine
#define ALLOC(v,ref) \
#if DIM == 0 NEWLINE\
    allocate(THIS(v)) NEWLINE\
#elif DIM == 1 NEWLINE\
    allocate(THIS(v)(size(ref))) NEWLINE\
#elif DIM == 2 NEWLINE\
    allocate(THIS(v)(size(ref,1),size(ref,2))) NEWLINE\
#elif DIM == 3 NEWLINE\
    allocate(THIS(v)(size(ref,1),size(ref,2),size(ref,3))) NEWLINE\
#elif DIM == 4 NEWLINE\
    allocate(THIS(v)(size(ref,1),size(ref,2),size(ref,3),size(ref,4))) NEWLINE\
#elif DIM == 5 NEWLINE\
    allocate(THIS(v)(size(ref,1),size(ref,2),size(ref,3),size(ref,4),size(ref,5))) NEWLINE\
#elif DIM == 6 NEWLINE\
    allocate(THIS(v)(size(ref,1),size(ref,2),size(ref,3),size(ref,4),size(ref,5),size(ref,6))) NEWLINE\
#endif

#define CHECK_SIZE_S(v,var,ref,dim) v = size(THIS(var),dim) == size(ref,dim)
#define CHECK_SIZE_A(v,var,ref,dim) v = v .and. size(THIS(var),dim) == size(ref,dim)
#define CHECK_SIZE(v,var,ref,dim) \
#if dim >= 1 NEWLINE\
    CHECK_SIZE_S(v,var,ref,1) NEWLINE\
#endif NEWLINE\
#if dim >= 2 NEWLINE\
    CHECK_SIZE_A(v,var,ref,2) NEWLINE\
#endif NEWLINE\
#if dim >= 3 NEWLINE\
    CHECK_SIZE_A(v,var,ref,3) NEWLINE\
#endif NEWLINE\
#if dim >= 4 NEWLINE\
    CHECK_SIZE_A(v,var,ref,4) NEWLINE\
#endif NEWLINE\
#if dim >= 5 NEWLINE\
    CHECK_SIZE_A(v,var,ref,5) NEWLINE\
#endif NEWLINE\
#if dim >= 6 NEWLINE\
    CHECK_SIZE_A(v,var,ref,6) NEWLINE\
#endif
