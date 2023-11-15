
SUBROUTINE fndpnt(Iary,Id)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bgpdt , Edt , I1(2) , I2 , Ibuf , Icore(1) , Ied , Igptt , Impt , Isil , Lcore , Mpt , Nout , Nrow1 , Old , Sil
   REAL Core(1) , Cstm , Dum(3) , Gptt
   COMMON /fpt   / Dum , Nrow1 , Lcore
   COMMON /loadx / I1 , Bgpdt , Old , Cstm , Sil , Isil , I2 , Mpt , Gptt , Edt , Impt , Igptt , Ied
   COMMON /system/ Ibuf , Nout
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   REAL Delta
   INTEGER Id , Idef , Ied1 , Ip
   INTEGER Iary(4)
!
! Local variable declarations
!
   REAL arry(3) , flag
   INTEGER i , icp , iedt(2) , if , ifed(2) , ifound , ipm , iry(3) , isave(4) , k , name(2) , ns
!
! End of declarations
!
!
   EQUIVALENCE (iry(1),arry(1)) , (Core(1),Icore(1))
   DATA name/4HFNDP , 4HNT  /
   DATA iedt/4HEDT  , 4HFEDT/ , ifed/4HFEDT , 4HST  /
!
!     FIND POINT ON BGPDT
!
   IF ( Id<0 ) GOTO 300
   IF ( Id>=268435455 .OR. Old<0 ) THEN
!               268435455 = 2**28 - 1
      WRITE (Nout,99001) Id , Old
99001 FORMAT (//,' BAD DATA PASSED TO FNDPNT, ID,OLD =',2I14)
      CALL mesage(-37,0,name)
   ENDIF
   DO
      ns = 4*(Id-Old)
      IF ( ns<4 ) THEN
!
         IF ( ns==0 ) GOTO 100
         CALL bckrec(Bgpdt)
         Old = 0
      ELSEIF ( ns==4 ) THEN
         EXIT
      ELSE
         CALL read(*300,*300,Bgpdt,isave(1),-ns+4,0,flag)
         EXIT
      ENDIF
   ENDDO
   CALL read(*300,*300,Bgpdt,isave(1),4,0,flag)
   Old = Id
 100  DO i = 1 , 4
      Iary(i) = isave(i)
   ENDDO
 200  RETURN
!
 300  ipm = Bgpdt
 400  CALL mesage(-2,ipm,name)
 500  ipm = Sil
   GOTO 400
 600  ipm = Edt
   GOTO 400
!
!
   ENTRY fndsil(Ip)
   DO
!     =================
!
!     FIND SIL VALUE
!
      ns = Ip - Isil
      IF ( ns<1 ) THEN
         IF ( ns==0 ) GOTO 700
         CALL bckrec(Sil)
         Isil = 0
      ELSEIF ( ns==1 ) THEN
         EXIT
      ELSE
         CALL read(*500,*500,Sil,i,-ns+1,0,flag)
         EXIT
      ENDIF
   ENDDO
   CALL read(*500,*500,Sil,if,1,0,flag)
   Isil = Ip
 700  Ip = if
   GOTO 200
!
!
   ENTRY fedtst(Idef)
!     ===================
!
!     FIND ENFORCED DISPLACEMENT
!
!     PUT DEFORM EID S AND VALUES INTO CORE FOR THIS SET
!
   icp = Nrow1 + 1
   k = 0
   CALL read(*600,*600,Edt,arry(1),-3,0,flag)
   DO
      CALL read(*600,*800,Edt,arry(1),3,0,flag)
      IF ( Idef==iry(1) .OR. k/=0 ) THEN
         IF ( Idef/=iry(1) ) EXIT
         k = k + 2
         Core(icp+k) = arry(3)
         Icore(icp+k-1) = iry(2)
         IF ( Lcore-Nrow1+k<=0 ) CALL mesage(-8,ipm,ifed)
      ENDIF
   ENDDO
 800  IF ( k==0 ) CALL mesage(-32,Idef,iedt)
   CALL bckrec(Edt)
   GOTO 200
!
!
   ENTRY fedt(Ied1,Delta,Idef)
!     ============================
!
!     FIND VALUE FOR EID IF IT EXISTS
!
   DO i = 1 , k , 2
      IF ( Ied1==Icore(icp+i) ) THEN
         Icore(icp+i) = -Icore(icp+i)
         Delta = Core(icp+i+1)
         GOTO 200
      ENDIF
   ENDDO
   Delta = 0.0
   GOTO 200
!
!
   ENTRY fedted(Idef)
!     ===================
!
!     CHECK TO SEE IF ALL ELEMENTS IN THE SET WERE USED
!
   ifound = 0
   DO i = 1 , k , 2
      IF ( Icore(icp+i)>=0 ) THEN
         iedt(1) = Icore(icp+i)
         iedt(2) = Idef
         CALL mesage(30,139,iedt)
         ifound = 1
      ENDIF
   ENDDO
   IF ( ifound==1 ) CALL mesage(-61,0,0)
   GOTO 200
END SUBROUTINE fndpnt
