
SUBROUTINE mpyad(Zz,Z,Zd)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Prec1 , Scrtch , Signab , Signc , T , Time
   INTEGER Ksystm(152) , Method , Nz
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /system/ Ksystm
!
! Dummy argument declarations
!
   REAL Z , Zd , Zz
!
! Local variable declarations
!
   INTEGER l49
!
! End of declarations
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
   EQUIVALENCE (Ksystm(58),Method)
   CALL sswtch(49,l49)
   IF ( Method>=1 .AND. Method<=3 ) l49 = 1
   IF ( l49/=0 ) CALL mpyado(Zz,Z,Zd)
   IF ( l49==0 ) CALL mma(Zz,Z,Zd)
END SUBROUTINE mpyad
