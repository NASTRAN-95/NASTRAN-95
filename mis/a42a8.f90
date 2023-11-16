
SUBROUTINE a42a8(A,B,C)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dummy(40)
   INTEGER Ncpw
   COMMON /system/ Dummy , Ncpw
!
! Dummy argument declarations
!
   REAL A , B
   CHARACTER*4 Ka , Kb
   CHARACTER*8 Kc
   INTEGER Notuse
   REAL C(2)
!
! Local variable declarations
!
   CHARACTER*8 d
!
! End of declarations
!
!
!     MERGES TWO A4 BCD WORDS (A AND B) TO ONE A8 BCD WORD (C)
!
!
   WRITE (d,99001) A , B
   IF ( Ncpw<8 ) READ (d,99001) C(1) , C(2)
   IF ( Ncpw>=8 ) READ (d,99002) C(1)
   RETURN
!
!
   ENTRY a42k8(A,B,Kc)
!     ======================
!
!     MERGES TWO A4 BCD WORDS (A AND B) TO ONE A8 CHARACTER WORD (KC)
!
   WRITE (Kc,99001) A , B
   RETURN
!
!
   ENTRY a42k4(A,Ka,Notuse)
!     ===========================
!
!     CONVERTS ONE A4 BCD WORD (A) TO ONE A4 CHARACTER WORD (KA)
!
   WRITE (Ka,99003) A
   RETURN
!
!
   ENTRY a82k8(C,Kc,Notuse)
!     ===========================
!
!     CONVERTS ONE A8 BCD WORD (C) TO ONE A4 CHARACTER WORD (KC)
!
   IF ( Ncpw<8 ) WRITE (Kc,99001) C(1) , C(2)
   IF ( Ncpw>=8 ) WRITE (Kc,99002) C(1)
   RETURN
!
!
   ENTRY k42k8(Ka,Kb,Kc)
!     ========================
!
!     MERGES TWO A4 CHARACTER WORDS (KA AND KB) TO ONE A8 CHARACTER
!     WORD (KC)
!
!     NOTE - SOME MACHINES, SUCH AS UNIVAC, HANDLE BCD WORD AND
!            CHARACTER WORD QUIT DIFFERENTLY
!
   WRITE (Kc,99001) Ka , Kb
   RETURN
!
!
   ENTRY k42a8(Ka,Kb,C)
!     =======================
!
!     MERGES TWO A4 CHARACTER WORDS (KA AND KB) TO ONE A8 BCD WORD (C)
!
   WRITE (d,99001) Ka , Kb
   IF ( Ncpw<8 ) READ (d,99001) C(1) , C(2)
   IF ( Ncpw>=8 ) READ (d,99002) C(1)
   RETURN
!
!
   ENTRY k42a4(Ka,A,Notuse)
!     ===========================
!
!     CONVERTS ONE A4 CHARACTER WORD (KA) TO ONE A4 BCD WORD (A)
!
   READ (Ka,99003) A
   RETURN
!
!
   ENTRY k82a8(Kc,C,Notuse)
!     ===========================
!
!     CONVERTS ONE A8 CHARACTER WORD (KC) TO ONE A8 BCD WORD (C)
!
   IF ( Ncpw<8 ) READ (Kc,99001) C(1) , C(2)
   IF ( Ncpw>=8 ) READ (Kc,99002) C(1)
99001 FORMAT (2A4)
99002 FORMAT (A8)
99003 FORMAT (A4)
!
END SUBROUTINE a42a8