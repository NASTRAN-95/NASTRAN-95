!*==a42a8.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE a42a8(A,B,C)
   USE c_system
   USE C_SYSTEM
   IMPLICIT NONE
   REAL Dummy(40)
   INTEGER Ncpw
   COMMON /system/ Dummy , Ncpw
   REAL A , B
   CHARACTER*4 Ka , Kb
   CHARACTER*8 Kc
   INTEGER Notuse
   REAL C(2)
   CHARACTER*8 d
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
