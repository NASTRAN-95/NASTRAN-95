
SUBROUTINE ssg2b(Kfs,Cdt,Pabar,Sr1,T1,Iprec1,Ia1,Sr2)
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Core(1) , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , I1 , I2 , Ioutpt , Kprec1 , Ksystm(55) , Nz , Prec , Scr2 ,        &
         & Sysbuf , T
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , I1 , I2 , Prec , Scr2
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Cdt , Ia1 , Iprec1 , Kfs , Pabar , Sr1 , Sr2 , T1
!
! Local variable declarations
!
   INTEGER diag , i , ident , irc , j , k , prec1 , rect , square , symm
   INTEGER korsz
!
! End of declarations
!
   EQUIVALENCE (Ksystm(55),Kprec1) , (Ksystm(1),Sysbuf) , (Ksystm(2),Ioutpt)
   DATA square , rect , diag , symm , ident/1 , 2 , 3 , 6 , 8/
!
   prec1 = min0(Kprec1,Iprec1)
   IF ( prec1<=0 ) prec1 = Kprec1
   Nz = korsz(Core)
   DO i = 1 , 21
      Filea(i) = 0
   ENDDO
   Filea(1) = Kfs
   Scr2 = Sr2
   IF ( iabs(Ia1)<1 ) THEN
      I1 = -1
      I2 = 1
   ELSEIF ( iabs(Ia1)==1 ) THEN
      I2 = Ia1
      I1 = Ia1
   ELSE
      I2 = -1
      I1 = 1
   ENDIF
   CALL rdtrl(Filea)
   Fileb(1) = Cdt
   CALL rdtrl(Fileb)
   IF ( Fileb(1)<=0 ) Fileb(4) = symm
   Filec(1) = Pabar
   CALL rdtrl(Filec)
   IF ( Filec(1)<=0 ) THEN
      Filec(1) = 0
      Filec(4) = diag
   ELSEIF ( Filec(2)/=Fileb(2) .AND. Fileb(1)>0 ) THEN
      WRITE (Ioutpt,99001) Swm , Fileb(1) , Fileb(3) , Fileb(2) , Fileb(3) , Filec(2)
99001 FORMAT (A27,' 2363, SSG2B FORCED MPYAD COMPATIBILITY OF MATRIX ON',I5,8H, FROM (,I5,1H,,I5,7H), TO (,I5,1H,,I5,1H))
      Fileb(2) = Filec(2)
   ENDIF
   Filed(4) = rect
   Filed(1) = Sr1
!
!     COMPUTE TYPE OF OUTPUT
!
   irc = 0
   IF ( Filea(5)>2 .OR. Fileb(5)>2 .OR. (Filec(5)>2 .AND. Filec(1)/=0) ) irc = 2
   Filed(5) = prec1 + irc
   T = T1
   Prec = prec1
   Filed(3) = Filea(3)
   IF ( T/=0 ) Filed(3) = Filea(2)
   IF ( Filea(1)<=0 .OR. Fileb(1)<=0 ) Filed(3) = Filec(3)
   CALL mpyad(Core,Core,Core)
   IF ( Filed(2)==Filed(3) .AND. Filed(4)/=symm ) Filed(4) = square
   IF ( Filed(4)/=symm .AND. Filed(4)==square ) THEN
!
!     IF END RESULT IS A SYMMETRIC MATRIX, MAKE SURE THE FORM IS SET TO
!     6 (SYMM). IT COULD SAVE CPU TIME LATER AND WORTH ONE FINAL CHECK.
!
      k = 0
      DO i = 1 , 21 , 7
         IF ( Filea(i)>0 ) THEN
            j = Filea(i+3)
            IF ( j/=diag .OR. i/=15 ) THEN
               IF ( j/=symm .AND. j/=ident ) GOTO 100
               IF ( j==symm ) k = k + 10
               IF ( j==ident ) k = k + 1
            ENDIF
         ENDIF
      ENDDO
      IF ( k>0 ) Filed(4) = ident
      IF ( k>=10 ) Filed(4) = symm
   ENDIF
 100  CALL wrttrl(Filed)
END SUBROUTINE ssg2b
