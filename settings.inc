#define _cc2_1(x,y) _cc2_2(x,y)
#define _cc2_2(x,y) x ## y
#define _cc3_1(x,y,z) _cc3_2(x,y,z)
#define _cc3_2(x,y,z) x ## y ## z

#define _str1(x) _str2(x)
#define _str2(x) #x


#define ROUTINE(base,var) _cc3_1(base,_,var)
#define STR(x) _str1(x)
#define THIS(x) _cc2_1(this%,x)

#undef ALLOC

#if DIM == 0 
#  define ALLOC(v,ref) allocate(v)
#elif DIM == 1 
#  define ALLOC(v,ref) allocate(v(size(ref)))
#elif DIM == 2 
#  define ALLOC(v,ref) allocate(v(size(ref,1),size(ref,2)))
#elif DIM == 3
#  define ALLOC(v,ref) allocate(v(size(ref,1),size(ref,2),size(ref,3)))
#elif DIM == 4
#  define ALLOC(v,ref) allocate(v(size(ref,1),size(ref,2),size(ref,3),size(ref,4)))
#elif DIM == 5
#  define ALLOC(v,ref) allocate(v(size(ref,1),size(ref,2),size(ref,3),size(ref,4),size(ref,5)))
#elif DIM == 6
#  define ALLOC(v,ref) allocate(v(size(ref,1),size(ref,2),size(ref,3),size(ref,4),size(ref,5),size(ref,6)))
#endif


#undef CHECK_SIZE

#define CHECK_SIZE_S(v,var,ref) v = size(var,1) == size(ref,1)
#define CHECK_SIZE_A(v,var,ref,dim) .and. size(var,dim) == size(ref,dim)

#if DIM == 1
#  define CHECK_SIZE(v,vv,r,d) CHECK_SIZE_S(v,vv,r)
#elif DIM == 2
#  define CHECK_SIZE(v,vv,r,d) CHECK_SIZE_S(v,vv,r) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,2)
#elif DIM == 3
#  define CHECK_SIZE(v,vv,r,d) CHECK_SIZE_S(v,vv,r) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,2) CHECK_SIZE_A(v,vv,r,3)
#elif DIM == 4
#  define CHECK_SIZE(v,vv,r,d) CHECK_SIZE_S(v,vv,r) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,2) CHECK_SIZE_A(v,vv,r,3) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,4)
#elif DIM == 5
#  define CHECK_SIZE(v,vv,r,d) CHECK_SIZE_S(v,vv,r) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,2) CHECK_SIZE_A(v,vv,r,3) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,4) CHECK_SIZE_A(v,vv,r,5)
#elif DIM == 6
#  define CHECK_SIZE(v,vv,r,d) CHECK_SIZE_S(v,vv,r) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,2) CHECK_SIZE_A(v,vv,r,3) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,4) CHECK_SIZE_A(v,vv,r,5) & NEWLINE\
    CHECK_SIZE_A(v,vv,r,6)
#endif

