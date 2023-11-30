
SUBROUTINE fornum(Form,Ichar,Imult)
   IMPLICIT NONE
   INTEGER Ichar , Imult
   CHARACTER*1 Form(200)
   CHARACTER*1 blank , number(2)
   INTEGER ii
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
