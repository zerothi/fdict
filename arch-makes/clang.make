FC=clang
FC_SERIAL=clang

FFLAGS=-O3 -m64 -fPIC -funroll-loops -freroll-loops 

#FFLAGS += -Wunused

CPP = fpp -P
# Or clang -E
