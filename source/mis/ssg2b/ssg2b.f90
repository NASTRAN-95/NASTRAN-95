!*==ssg2b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg2b(Kfs,Cdt,Pabar,Sr1,T1,Iprec1,Ia1,Sr2)
!
   USE c_mpyadx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kfs
   INTEGER :: Cdt
   INTEGER :: Pabar
   INTEGER :: Sr1
   INTEGER :: T1
   INTEGER :: Iprec1
   INTEGER :: Ia1
   INTEGER :: Sr2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: diag , ident , rect , square , symm
   INTEGER :: i , ioutpt , irc , j , k , kprec1 , prec1 , sysbuf
   EXTERNAL korsz , mpyad , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Ksystm(55),Kprec1) , (Ksystm(1),Sysbuf) , (Ksystm(2),Ioutpt)
   DATA square , rect , diag , symm , ident/1 , 2 , 3 , 6 , 8/
!
   prec1 = min0(kprec1,Iprec1)
   IF ( prec1<=0 ) prec1 = kprec1
   nz = korsz(core)
   DO i = 1 , 21
      filea(i) = 0
   ENDDO
   filea(1) = Kfs
   scr2 = Sr2
   IF ( iabs(Ia1)<1 ) THEN
      i1 = -1
      i2 = 1
   ELSEIF ( iabs(Ia1)==1 ) THEN
      i2 = Ia1
      i1 = Ia1
   ELSE
      i2 = -1
      i1 = 1
   ENDIF
   CALL rdtrl(filea)
   fileb(1) = Cdt
   CALL rdtrl(fileb)
   IF ( fileb(1)<=0 ) fileb(4) = symm
   filec(1) = Pabar
   CALL rdtrl(filec)
   IF ( filec(1)<=0 ) THEN
      filec(1) = 0
      filec(4) = diag
   ELSEIF ( filec(2)/=fileb(2) .AND. fileb(1)>0 ) THEN
      WRITE (ioutpt,99001) swm , fileb(1) , fileb(3) , fileb(2) , fileb(3) , filec(2)
99001 FORMAT (A27,' 2363, SSG2B FORCED MPYAD COMPATIBILITY OF MATRIX ON',I5,8H, FROM (,I5,1H,,I5,7H), TO (,I5,1H,,I5,1H))
      fileb(2) = filec(2)
   ENDIF
   filed(4) = rect
   filed(1) = Sr1
!
!     COMPUTE TYPE OF OUTPUT
!
   irc = 0
   IF ( filea(5)>2 .OR. fileb(5)>2 .OR. (filec(5)>2 .AND. filec(1)/=0) ) irc = 2
   filed(5) = prec1 + irc
   t = T1
   prec = prec1
   filed(3) = filea(3)
   IF ( t/=0 ) filed(3) = filea(2)
   IF ( filea(1)<=0 .OR. fileb(1)<=0 ) filed(3) = filec(3)
   CALL mpyad(core,core,core)
   IF ( filed(2)==filed(3) .AND. filed(4)/=symm ) filed(4) = square
   IF ( filed(4)/=symm .AND. filed(4)==square ) THEN
!
!     IF END RESULT IS A SYMMETRIC MATRIX, MAKE SURE THE FORM IS SET TO
!     6 (SYMM). IT COULD SAVE CPU TIME LATER AND WORTH ONE FINAL CHECK.
!
      k = 0
      DO i = 1 , 21 , 7
         IF ( filea(i)>0 ) THEN
            j = filea(i+3)
            IF ( j/=diag .OR. i/=15 ) THEN
               IF ( j/=symm .AND. j/=ident ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
               IF ( j==symm ) k = k + 10
               IF ( j==ident ) k = k + 1
            ENDIF
         ENDIF
      ENDDO
      IF ( k>0 ) filed(4) = ident
      IF ( k>=10 ) filed(4) = symm
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      CALL wrttrl(filed)
   END SUBROUTINE spag_block_1
END SUBROUTINE ssg2b
