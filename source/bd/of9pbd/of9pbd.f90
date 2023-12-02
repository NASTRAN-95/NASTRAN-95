!*==of9pbd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA of9pbd
   USE c_ofpb9
   IMPLICIT NONE
!OF9PBD
!
!     BLOCK DATA FOR ALL NON-STRESS AND NON-FORCE C ARRAYS
!
! DISPLACEMENT VECTOR REAL SORT 1
! DISPLACEMENT VECTOR REAL SORT 2
! DISPLACEMENT VECTOR COMPLEX SORT 1
! DISPLACEMENT VECTOR COMPLEX SORT 2
! LOAD VECTOR REAL SORT 1
! LOAD VECTOR REAL SORT 2
! LOAD VECTOR COMPLEX SORT 1
! LOAD VECTOR COMPLEX SORT 2
! SPCF VECTOR REAL SORT 1
! SPCF VECTOR REAL SORT 2
! SPCF VECTOR COMPLEX SORT 1
! SPCF VECTOR COMPLEX SORT 2
! VELOCITY VECTOR REAL SORT 1     ACCELERATION VECTOR REAL SORT 1
! VELOCITY REAL SORT 2(LEFT)     ACCELERATION REAL SORT 2 (RIGHT)
! NON-LINEAR FORCE REAL SORT 1     NON-LINEAR FORCE REAL SORT 2
! VELOCITY COMPLEX SORT 1
! VELOCITY COMPLEX SORT 2
! ACCELERATION COMPLEX SORT 1
! ACCELERATION COMPLEX SORT 2
! EIGENVALUE SUMMARY REAL SORT 1       EIGENVALUE SUMMARY COMPLEX SORT 1
   DATA c1/1 , 0 , 1 , -1 , 0 , 2 , 0 , 0 , 0 , -1 , 0 , 0 , 311 , 107 , 1 , -1 , 0 , 315 , 354 , 107 , 1 , -1 , 0 , 112 , 374 ,    &
      & 104 , 119 , 125 , 0 , 2 , 411 , 104 , 119 , 126 , 0 , 2 , 392 , 107 , 119 , 125 , 0 , 111 , 429 , 107 , 119 , 126 , 0 ,     &
      & 111 , 1 , 0 , 33 , -1 , 0 , 2 , 0 , 0 , 0 , -1 , 0 , 0 , 311 , 107 , 33 , -1 , 0 , 315 , 354 , 107 , 33 , -1 , 0 , 112 ,    &
      & 374 , 104 , 123 , 125 , 0 , 2 , 411 , 104 , 123 , 126 , 0 , 2 , 392 , 107 , 123 , 125 , 0 , 111 , 429 , 107 , 123 , 126 ,   &
      & 0 , 111 , 1 , 0 , 45 , -1 , 0 , 2 , 0 , 0 , 0 , -1 , 0 , 0 , 311 , 107 , 45 , -1 , 0 , 315 , 354 , 107 , 45 , -1 , 0 , 112 ,&
      & 374 , 104 , 122 , 125 , 0 , 2 , 411 , 104 , 122 , 126 , 0 , 2 , 392 , 107 , 122 , 125 , 0 , 111 , 429 , 107 , 122 , 126 ,   &
      & 0 , 111 , 1 , 106 , 113 , -1 , 0 , 2 , 1 , 106 , 114 , -1 , 0 , 2 , 354 , 107 , 113 , -1 , 0 , 112 , 354 , 107 , 114 , -1 , &
      & 0 , 112 , 1 , 106 , 115 , -1 , 0 , 2 , 354 , 107 , 115 , -1 , 0 , 112 , 374 , 104 , 120 , 125 , 0 , 2 , 411 , 104 , 120 ,   &
      & 126 , 0 , 2 , 392 , 107 , 120 , 125 , 0 , 111 , 429 , 107 , 120 , 126 , 0 , 111 , 374 , 104 , 121 , 125 , 0 , 2 , 411 ,     &
      & 104 , 121 , 126 , 0 , 2 , 392 , 107 , 121 , 125 , 0 , 111 , 429 , 107 , 121 , 126 , 0 , 111 , 298 , 0 , 3 , -1 , 4 , 5 ,    &
      & 365 , 0 , 116 , -1 , 117 , 118/
!
! EIGENVECTOR COMPLEX SORT 1
! VDR-DISPLACEMENT REAL SORT 1     VDR-DISPLACEMENT REAL SORT 2
! VDR-DISPLACEMENT VECTOR COMPLEX SORT 1
! VDR-DISPLACEMENT VECTOR COMPLEX SORT 2
! VDR-VELOCITY REAL SORT 1     VDR-VELOCITY REAL SORT 2
! VDR-VELOCITY VECTOR COMPLEX SORT 1
! VDR-VELOCITY VECTOR COMPLEX SORT 2
! VDR-ACCELERATION REAL SORT 1     VDR-ACCELERATION REAL SORT 2
! VDR-ACCELERATION VECTOR COMPLEX SORT 1
! VDR-ACCELERATION VECTOR COMPLEX SORT 2
! VDR-EIGENVECTOR COMPLEX SORT 1
! VDR-EIGENVECTOR COMPLEX SORT 2
! EIGENVALUE ANALYSIS SUMMARY  (4 TYPES)      REAL SORT 1
! EIGENVALUE ANYLYSIS SUMMARY COMPLEX SORT 1  (4 TYPES)
! EIGENVECTOR REAL SORT 1     GPST REAL SORT 1
! ELEMENT STRAIN ENERGY
! GRID POINT FORCE BALANCE
! MPCFORCE VECTOR REAL SORT 1
   DATA c41/374 , 0 , 124 , 125 , 0 , 2 , 411 , 0 , 124 , 126 , 0 , 2 , 1 , 0 , 212 , -1 , 0 , 2 , 354 , 107 , 212 , -1 , 0 , 112 , &
      & 374 , 104 , 208 , 125 , 0 , 2 , 411 , 104 , 208 , 126 , 0 , 2 , 392 , 107 , 208 , 125 , 0 , 111 , 429 , 107 , 208 , 126 ,   &
      & 0 , 111 , 1 , 0 , 211 , -1 , 0 , 2 , 354 , 107 , 211 , -1 , 0 , 112 , 374 , 104 , 209 , 125 , 0 , 2 , 411 , 104 , 209 ,     &
      & 126 , 0 , 2 , 392 , 107 , 209 , 125 , 0 , 111 , 429 , 107 , 209 , 126 , 0 , 111 , 1 , 0 , 213 , -1 , 0 , 2 , 354 , 107 ,    &
      & 213 , -1 , 0 , 112 , 374 , 104 , 210 , 125 , 0 , 2 , 411 , 104 , 210 , 126 , 0 , 2 , 392 , 107 , 210 , 125 , 0 , 111 , 429 ,&
      & 107 , 210 , 126 , 0 , 111 , 374 , 104 , 214 , 125 , 0 , 2 , 411 , 104 , 214 , 126 , 0 , 2 , 392 , 107 , 214 , 125 , 0 ,     &
      & 111 , 429 , 107 , 214 , 126 , 0 , 111 , 1 , 0 , 0 , -1 , 92 , 93 , 1 , 0 , 0 , -1 , 90 , 91 , 336 , 0 , 92 , -1 , 95 , 94 , &
      & 1 , 0 , 0 , -1 , 215 , 216 , 1 , 0 , 0 , -1 , 96 , 98 , 1 , 0 , 0 , -1 , 100 , 99 , 345 , 0 , 96 , -1 , 95 , 97 , 1 , 0 ,   &
      & 0 , -1 , 100 , 99 , 1 , 0 , 6 , -1 , 0 , 2 , 321 , 0 , 30 , -1 , 31 , 32 , 2258 , 0 , 353 , -1 , 0 , 354 , 0 , 0 , 0 , 0 ,  &
      & 0 , 0 , 2266 , 0 , 355 , -1 , 0 , 356 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 375 , -1 , 0 , 2 , 0 , 0 , 0 , -1 , 0 , 0/
!
END BLOCKDATA of9pbd
