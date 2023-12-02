!*==fornum.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fornum(Form,Ichar,Imult)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   CHARACTER(1) , DIMENSION(200) :: Form
   INTEGER :: Ichar
   INTEGER :: Imult
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(1) , SAVE :: blank
   INTEGER :: ii
   CHARACTER(1) , DIMENSION(2) , SAVE :: number
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
! THIS SUBROUTINE CONVERTS ALL NUMBERS TO INTEGER FORMAT
!
   DATA blank/' '/
   DATA number/'0' , '9'/
   Imult = 0
   DO WHILE ( Form(Ichar)==blank )
      Ichar = Ichar + 1
   ENDDO
   DO WHILE ( Form(Ichar)>=number(1) .AND. Form(Ichar)<=number(2) )
      READ (Form(Ichar),99001) ii
99001 FORMAT (I1)
      Imult = Imult*10 + ii
      Ichar = Ichar + 1
   ENDDO
END SUBROUTINE fornum
