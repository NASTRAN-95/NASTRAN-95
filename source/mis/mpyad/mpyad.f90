!*==mpyad.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpyad(Zz,Z,Zd)
   USE c_mpyadx
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Zz
   REAL :: Z
   REAL :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: l49 , method
   EXTERNAL mma , mpyado , sswtch
!
! End of declarations rewritten by SPAG
!
!
!     THE FOLLOWING DEFINES THE VARIOUS I/O METHODS AND STORAGE METHODS USED
!  BY THE DIFFERENT MULTIPLY-ADD METHODS.
!
!  IN REGARDS TO THE NEW METHODS BELOW, WHEN MULTIPLE COLUMNS OF A MATRIX
!  ARE STORED AND READ BY GETSTR, THEN THE MATRIX IS STORED IN MEMORY IN
!  COMPACT FORM.  SEE SUBROUTINES 'MMARM1,2,3,4' FOR A DESCRIPTION OF
!  THIS COMPACT FORM.  WHEN ONLY A SINGLE COLUMN OF A MATRIX IS STORED
!  AND IT IS BEING READ BY GETSTR, IT IS STORED IN COMPACT FORM IN MEMORY.
!  SEE SUBROUTINES 'MMARC1,2,3,4' FOR A DESCRIPTION OF THIS FORM.
!
!   METHOD     METHOD OF READING MATRIX    MULTIPLE COLUMNS OF MATRIX STORED
!                 A        B       C           A         B        D
!  OLD METHODS
!     1         INTPK   UNPACK   UNPACK       NO         YES      YES
!     2T        GETSTR  UNPACK   INTPK        YES        NO       NO
!     2NT       GETSTR  INTPK    INTPK        YES        NO       NO
!     3T        UNPACK  GETSTR   INTPK        YES        NO       NO
!  NEW METHODS
!     10        UNPACK  UNPACK   UNPACK       YES        NO       NO
!     11        UNPACK  GETSTR   UNPACK       YES        NO       NO
!     20        UNPACK  UNPACK   UNPACK       NO         YES      YES
!     21        GETSTR  UNPACK   UNPACK       NO         YES      YES
!     30        GETSTR  UNPACK   UNPACK       YES        NO       NO
!     31        GETSTR  GETSTR   UNPACK       YES        NO       NO
!     40        UNPACK  GETSTR   UNPACK       NO         YES      YES
!     41        GETSTR  GETSTR   UNPACK       NO         YES      YES
!
   !>>>>EQUIVALENCE (Ksystm(58),Method)
   CALL sswtch(49,l49)
   IF ( method>=1 .AND. method<=3 ) l49 = 1
   IF ( l49/=0 ) CALL mpyado(Zz,Z,Zd)
   IF ( l49==0 ) CALL mma(Zz,Z,Zd)
END SUBROUTINE mpyad
