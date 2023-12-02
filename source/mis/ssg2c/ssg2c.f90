!*==ssg2c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg2c(A,B,C,Op,Block)
USE C_SADDX
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   INTEGER :: B
   INTEGER :: C
   INTEGER :: Op
   INTEGER , DIMENSION(11) :: Block
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dit
   REAL(REAL64) :: dit1
   INTEGER , DIMENSION(2) :: dt1 , na , nb
   INTEGER :: i , ia5 , ib5 , ipr1 , iprec , irc , nomix , nout
   INTEGER , DIMENSION(5) :: ia , ib , ic
   INTEGER , DIMENSION(1) :: it , it1
   EXTERNAL fname , korsz , rdtrl , sadd , wrttrl
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (dt1,dit1)
   !>>>>EQUIVALENCE (Ksystm(55),Ipr1) , (Mcbs(1),Ia(1)) , (Mcbs(8),It(1),Dit) , (Mcbs(13),Ib(1)) , (Mcbs(20),It1(1)) , (Mcbs(61),Ic(1)) ,&
!>>>>    & (Ksystm(2),Nout) , (Ia5,Ia(5)) , (Ib5,Ib(5))
!
!     BLOCK(6) WAS NOT USED IN ORIGINAL NASTRAN. IT IS NOW USED TO FLAG
!     THE CHECKING OF THE INPUT MATRICES COMPATABILITY IF THE CALLER
!     PRESETS BLOCK(6) TO -1
!
   ia(1) = A
   CALL rdtrl(ia)
   IF ( ia(1)<0 ) ia(1) = 0
   ib(1) = B
   CALL rdtrl(ib)
   IF ( ib(1)>0 ) THEN
      DO i = 2 , 4
         ic(i) = ib(i)
      ENDDO
   ELSE
      ib(1) = 0
      IF ( ia(1)<=0 ) RETURN
      DO i = 2 , 4
         ic(i) = ia(i)
      ENDDO
   ENDIF
!
   nomix = 0
   IF ( Block(6)==-1 ) THEN
      IF ( ia5/=0 .AND. ib5/=0 ) THEN
         IF ( .NOT.((ia5<=2 .AND. ib5<=2) .OR. (ia5>=3 .AND. ib5>=3)) ) THEN
            IF ( max0(ia5,Block(1))/=max0(ib5,Block(7)) ) THEN
               nomix = 1
               CALL fname(A,na)
               CALL fname(B,nb)
               WRITE (nout,99001) Uwm , na , ia(2) , ia(3) , ia5 , ia(4) , nb , ib(2) , ib(3) , ib5 , ib(4)
99001          FORMAT (A25,', SSG2C RECEIVES TWO MIXED FILE TYPES FOR ADDING.',/,2(5X,'FILE ',2A4,'(',I6,' X',I6,') TYPE =',I3,     &
                      &', FORM =',I3))
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     UNSY + SYM = UNSY
!
   IF ( ic(4)==6 ) THEN
      IF ( ia(1)/=0 .AND. ia(4)/=6 ) ic(4) = 1
      IF ( ib(1)/=0 .AND. ib(4)/=6 ) ic(4) = 1
   ENDIF
   IF ( Op<0 ) ia(2) = -ic(2)
   DO i = 1 , 5
      it(i) = Block(i)
      it1(i) = Block(i+6)
   ENDDO
   dt1(1) = Mcbs(20)
   dt1(2) = Mcbs(21)
   IF ( nomix/=0 ) WRITE (nout,99002,ERR=100) it(1) , dit , it1(1) , dit1
99002 FORMAT ('  MULTIPLIERS =',I3,D12.3,I8,D12.3)
 100  ic(1) = C
   Lcore = korsz(Core)
!
!     DETERMINE TYPE OF OUTPUT
!
   irc = 0
   IF ( ia(1)/=0 ) THEN
      IF ( ia5>2 .OR. it(1)>2 ) irc = 2
   ENDIF
   IF ( ib(1)/=0 ) THEN
      IF ( ib5>2 .OR. it1(1)>2 ) irc = 2
   ENDIF
   iprec = ipr1
   ic(5) = irc + iprec
   Nomat = 2
   IF ( nomix/=0 ) THEN
      CALL fname(ic(1),na)
      WRITE (nout,99003) na , ic(2) , ic(3) , ic(5) , ic(4)
99003 FORMAT (5X,'FILE ',2A4,'(',I6,' X',I6,') TYPE =',I3,', FORM =',I3,5X,'(RESULTANT)')
   ENDIF
   CALL sadd(Core,Core)
   CALL wrttrl(ic)
END SUBROUTINE ssg2c
