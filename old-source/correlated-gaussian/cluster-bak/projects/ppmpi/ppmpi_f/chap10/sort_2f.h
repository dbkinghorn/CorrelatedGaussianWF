C  sort_2f.h -- header file for sort_2.f
C 
      integer KEY_MIN
      integer KEY_MAX
      integer KEY_MOD
      parameter (KEY_MIN = 0, KEY_MAX = 32767,
     +           KEY_MOD = 32769)
C
      integer list_size, allocated_size, keys
      parameter (list_size = 1, allocated_size = 0,
     +           keys = 2)
C
C functions
       integer Get_list_size 
       integer Allocate_list
C