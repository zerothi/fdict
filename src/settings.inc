#ifdef COMMENTS
! @LICENSE@, see README.md
#endif
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
#elif DIM == 7
#  define ALLOC(v,ref) allocate(v(size(ref,1),size(ref,2),size(ref,3),size(ref,4),size(ref,5),size(ref,6),size(ref,7)))
#endif
