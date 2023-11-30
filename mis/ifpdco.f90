
LOGICAL FUNCTION ifpdco(Ic)
   IMPLICIT NONE
   REAL Dum(521)
   INTEGER Gc(7) , Idummy(55) , Ithrml , Ll(6)
   COMMON /ifpdta/ Dum , Gc , Ll
   COMMON /system/ Idummy , Ithrml
   INTEGER Ic
   INTEGER dg , lc
!
!     DECODE D.O.F. INTO LL SPACE.
!     RETURN WITH IFPDCO=.TRUE. IF ERROR ENCOUNTERED
!     FOR EXAMPLE - GIVEN IC=124, THEN
!                   LL(1)=1,   LL(2)=2,  LL(4)=4, LL(3)=LL(5)=LL(6)=0
!                   GC(1)=124, GC(2)=12, GC(3)=1, GC(4)=GC(5),GC(6)=0
!                   IFPDCO=.FALSE.
!
!
   Gc(1) = Ic
   DO lc = 1 , 6
      Ll(lc) = 0
   ENDDO
   IF ( Ic<0 ) GOTO 300
   IF ( Ic/=0 ) THEN
      DO lc = 1 , 6
         Gc(lc+1) = Gc(lc)/10
         dg = Gc(lc) - 10*Gc(lc+1)
         IF ( Ithrml/=1 .AND. dg>6 ) GOTO 300
         IF ( Ithrml==1 .AND. dg>1 ) GOTO 300
         IF ( dg==0 ) GOTO 200
         IF ( Ll(dg)/=0 ) GOTO 300
         Ll(dg) = dg
      ENDDO
      IF ( Gc(7)/=0 ) GOTO 300
   ENDIF
 100  ifpdco = .FALSE.
   RETURN
 200  IF ( Gc(lc)==0 ) GOTO 100
 300  ifpdco = .TRUE.
END FUNCTION ifpdco
