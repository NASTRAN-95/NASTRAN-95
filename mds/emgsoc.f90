
SUBROUTINE emgsoc(Icore,Ncore,Heat)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ixxx , Mach
   COMMON /machin/ Mach
   COMMON /zzemgx/ Ixxx
!
! Dummy argument declarations
!
   REAL Heat
   INTEGER Icore , Ncore
!
! Local variable declarations
!
   INTEGER korsz
!
! End of declarations
!
!
!     THIS .MDS VERSION IS USED ONLY IN THE VIRTUAL MACHINES (IBM, VAX,
!     AND UNIX)
!     CDC & UNIVAC, NON-VIRTUAL MACHINES, SHOULD USE THE EMGSOC.MIS
!     VERSION
!
!     ICORE = RELATIVE ADDRESS OF FIRST WORD OF OPEN CORE.
!     NCORE = RELATIVE ADDRESS OF FINAL WORD OF OPEN CORE.
!
!     IFILE = GINO FILE WHOSE TRAILER BITS INDICATE ACTIVE COMMON GROUPS
!
!     BOUNDARY ALIGNMENT IS ASSURED BY THE FACT THAT ALL COMMON BLOCKS
!     START AT AN ODD ADDRESS.
!
!
   Ncore = korsz(Ixxx)
   Icore = 3
   IF ( Mach==3 .OR. Mach==4 ) STOP ' EMGSOC'
END SUBROUTINE emgsoc