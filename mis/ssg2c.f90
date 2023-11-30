
SUBROUTINE ssg2c(A,B,C,Op,Block)
   IMPLICIT NONE
   REAL Core(1) , Dit
   INTEGER Ia(5) , Ia5 , Ib(5) , Ib5 , Ic(5) , Ipr1 , It(1) , It1(1) , Ksystm(65) , Lcore , Mcbs(67) , Nomat , Nout
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /saddx / Nomat , Lcore , Mcbs
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Core
   INTEGER A , B , C , Op
   INTEGER Block(11)
   DOUBLE PRECISION dit1
   INTEGER dt1(2) , i , iprec , irc , na(2) , nb(2) , nomix
   INTEGER korsz
!
   EQUIVALENCE (dt1,dit1)
   EQUIVALENCE (Ksystm(55),Ipr1) , (Mcbs(1),Ia(1)) , (Mcbs(8),It(1),Dit) , (Mcbs(13),Ib(1)) , (Mcbs(20),It1(1)) , (Mcbs(61),Ic(1)) ,&
    & (Ksystm(2),Nout) , (Ia5,Ia(5)) , (Ib5,Ib(5))
!
!     BLOCK(6) WAS NOT USED IN ORIGINAL NASTRAN. IT IS NOW USED TO FLAG
!     THE CHECKING OF THE INPUT MATRICES COMPATABILITY IF THE CALLER
!     PRESETS BLOCK(6) TO -1
!
   Ia(1) = A
   CALL rdtrl(Ia)
   IF ( Ia(1)<0 ) Ia(1) = 0
   Ib(1) = B
   CALL rdtrl(Ib)
   IF ( Ib(1)>0 ) THEN
      DO i = 2 , 4
         Ic(i) = Ib(i)
      ENDDO
   ELSE
      Ib(1) = 0
      IF ( Ia(1)<=0 ) GOTO 99999
      DO i = 2 , 4
         Ic(i) = Ia(i)
      ENDDO
   ENDIF
!
   nomix = 0
   IF ( Block(6)==-1 ) THEN
      IF ( Ia5/=0 .AND. Ib5/=0 ) THEN
         IF ( .NOT.((Ia5<=2 .AND. Ib5<=2) .OR. (Ia5>=3 .AND. Ib5>=3)) ) THEN
            IF ( max0(Ia5,Block(1))/=max0(Ib5,Block(7)) ) THEN
               nomix = 1
               CALL fname(A,na)
               CALL fname(B,nb)
               WRITE (Nout,99001) Uwm , na , Ia(2) , Ia(3) , Ia5 , Ia(4) , nb , Ib(2) , Ib(3) , Ib5 , Ib(4)
99001          FORMAT (A25,', SSG2C RECEIVES TWO MIXED FILE TYPES FOR ADDING.',/,2(5X,'FILE ',2A4,'(',I6,' X',I6,') TYPE =',I3,     &
                      &', FORM =',I3))
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     UNSY + SYM = UNSY
!
   IF ( Ic(4)==6 ) THEN
      IF ( Ia(1)/=0 .AND. Ia(4)/=6 ) Ic(4) = 1
      IF ( Ib(1)/=0 .AND. Ib(4)/=6 ) Ic(4) = 1
   ENDIF
   IF ( Op<0 ) Ia(2) = -Ic(2)
   DO i = 1 , 5
      It(i) = Block(i)
      It1(i) = Block(i+6)
   ENDDO
   dt1(1) = Mcbs(20)
   dt1(2) = Mcbs(21)
   IF ( nomix/=0 ) WRITE (Nout,99002,ERR=100) It(1) , Dit , It1(1) , dit1
99002 FORMAT ('  MULTIPLIERS =',I3,D12.3,I8,D12.3)
 100  Ic(1) = C
   Lcore = korsz(Core)
!
!     DETERMINE TYPE OF OUTPUT
!
   irc = 0
   IF ( Ia(1)/=0 ) THEN
      IF ( Ia5>2 .OR. It(1)>2 ) irc = 2
   ENDIF
   IF ( Ib(1)/=0 ) THEN
      IF ( Ib5>2 .OR. It1(1)>2 ) irc = 2
   ENDIF
   iprec = Ipr1
   Ic(5) = irc + iprec
   Nomat = 2
   IF ( nomix/=0 ) THEN
      CALL fname(Ic(1),na)
      WRITE (Nout,99003) na , Ic(2) , Ic(3) , Ic(5) , Ic(4)
99003 FORMAT (5X,'FILE ',2A4,'(',I6,' X',I6,') TYPE =',I3,', FORM =',I3,5X,'(RESULTANT)')
   ENDIF
   CALL sadd(Core,Core)
   CALL wrttrl(Ic)
99999 RETURN
END SUBROUTINE ssg2c
