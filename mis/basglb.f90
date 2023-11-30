
SUBROUTINE basglb(Vin1,Vout1,Pont,Icstm)
   IMPLICIT NONE
   INTEGER Cstm , Ibuf , Icm , Idum(3) , Lc(4) , Lc1(10) , Nout , Nsys , Tysys
   REAL Ro(3) , To(3,3) , Tz(3,3)
   COMMON /loadx / Lc , Cstm , Lc1 , Idum , Icm
   COMMON /system/ Ibuf , Nout
   COMMON /tranx / Nsys , Tysys , Ro , To
   COMMON /xcstm / Tz
   INTEGER Icstm
   REAL Pont(3) , T(9) , Vin1(3) , Vout1(3)
   INTEGER check , i , iexit , ioth , iparm(2) , iparm1 , j , n1
   REAL flag , pont1(3) , r , ti(3,3) , tl(3,3) , vin(3) , xl , xr , yr , zl
   LOGICAL tonly
!
!     THIS ROUTINE CONTAINS FOUR ENTRY POINTS
!
!     1- BASGLB TRANSFORMS A VECTOR FROM BASIC TO GLOBAL
!     2- GLBBAS TRANSFORMS A VECTOR FROM GLOBAL TO BASIC
!     3- FDCSTM FINDS THE LOGICAL RECORD ON THE CSTM FOR A PARTICULAR ID
!     4- GBTRAN FINDS A PARTICULAR GLOBAL TO BASIC TRANSFORMATION AND
!        RETURNS IT AS A 3 X 3 STORED BY ROWS.
!
!
   DATA iparm/4HBASG , 2HLB/
!
!     NSYS  IS SYSTEM NUMBER
!     TYSYS IS SYSTEM TYPE
!     RO IS LOCATION OF ORIGIN
!     TO IS ROTATION MATRIX
!
   tonly = .FALSE.
   check = 123456789
   ASSIGN 600 TO iexit
   GOTO 100
!
!
   ENTRY gbtran(Icstm,Pont,T)
!     ===========================
!
   IF ( Icstm==0 ) THEN
!
!     COORDINATE SYSTEM 0
!
      DO i = 2 , 8
         T(i) = 0.
      ENDDO
      T(1) = 1.
      T(5) = 1.
      T(9) = 1.
      GOTO 200
   ELSE
      IF ( Tysys>=2 .AND. check/=123456789 ) WRITE (Nout,99001)
99001 FORMAT ('0*** SYSTEM POTENTIAL ERROR, GBTRAN WAS CALLED WITHOUT',' FIRST CALLING BASGLB')
      check = 123456789
      tonly = .TRUE.
      GOTO 800
   ENDIF
!
!
   ENTRY fdcstm(Icstm)
!     ====================
!
   tonly = .FALSE.
   ASSIGN 200 TO iexit
!
!     FDCSTM WILL FIND REQUESTED SYSTEM (ICSTM)
!
 100  IF ( Icstm==0 ) THEN
!
!     REQUEST FOR BASIC COORDINATE SYSTEM
!
      Tysys = 1
      Nsys = 0
      Ro(1) = 0.0
      Ro(2) = 0.0
      Ro(3) = 0.0
      DO i = 1 , 3
         DO j = 1 , 3
            To(j,i) = 0.0
         ENDDO
      ENDDO
      To(1,1) = 1.0
      To(2,2) = 1.0
      To(3,3) = 1.0
   ELSE
      IF ( Icm/=0 ) GOTO 500
      IF ( Icstm/=Nsys ) THEN
         DO
            CALL read(*300,*500,Cstm,Nsys,14,0,flag)
            IF ( Icstm==Nsys ) THEN
               CALL bckrec(Cstm)
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDIF
   GOTO iexit
 200  RETURN
!
 300  n1 = -2
   iparm1 = Cstm
 400  CALL mesage(n1,iparm1,iparm)
!
!     UNABLE TO FIND REQUESTED COORDINATE SYSTEM
!
 500  n1 = -30
   iparm1 = 25
   iparm(1) = Icstm
   GOTO 400
!
!     CONVERTS BASIC TO GLOBAL
!
 600  ioth = 0
!
!     RECTANGULAR
!
 700  DO i = 1 , 3
      DO j = 1 , 3
         Tz(i,j) = To(j,i)
         ti(i,j) = To(j,i)
      ENDDO
      vin(i) = Vin1(i)
   ENDDO
   IF ( Tysys<2 ) THEN
      CALL mpyl(ti(1,1),vin(1),3,3,1,Vout1(1))
   ELSE
!
!     CYLINDRICAL
!
      DO i = 1 , 3
         pont1(i) = Pont(i) - Ro(i)
      ENDDO
      CALL mpyl(ti(1,1),pont1(1),3,3,1,vin(1))
      DO i = 1 , 3
         DO j = 1 , 3
            tl(i,j) = 0.0
         ENDDO
      ENDDO
      r = sqrt(vin(1)*vin(1)+vin(2)*vin(2))
      IF ( r==0.0 ) THEN
!
!     ORIENTATION ARBITARY   TL = I   I.E. TZ = TI
!
         DO i = 1 , 3
            DO j = 1 , 3
               Tz(i,j) = ti(i,j)
            ENDDO
         ENDDO
      ELSE
         IF ( Tysys>2 ) THEN
!
!     SPHERICAL
!
            xl = sqrt(vin(1)*vin(1)+vin(2)*vin(2)+vin(3)*vin(3))
            xr = vin(1)/r
            yr = vin(2)/r
            zl = vin(3)/xl
!
!     BUILD TL TRANSPOSE
!
            tl(1,1) = vin(1)/xl
            tl(1,2) = xr*zl
            tl(1,3) = -yr
            tl(2,1) = vin(2)/xl
            tl(2,2) = yr*zl
            tl(2,3) = xr
            tl(3,1) = zl
            tl(3,2) = -r/xl
         ELSE
            tl(3,3) = 1.0
            tl(1,1) = vin(1)/r
            tl(2,2) = tl(1,1)
            tl(2,1) = vin(2)/r
            tl(1,2) = -tl(2,1)
         ENDIF
         CALL mpyl(tl(1,1),ti(1,1),3,3,3,Tz(1,1))
      ENDIF
      IF ( tonly ) THEN
!
!     RETURN THE TRANSFORMATION ONLY
!
         T(1) = Tz(1,1)
         T(2) = Tz(1,2)
         T(3) = Tz(1,3)
         T(4) = Tz(2,1)
         T(5) = Tz(2,2)
         T(6) = Tz(2,3)
         T(7) = Tz(3,1)
         T(8) = Tz(3,2)
         T(9) = Tz(3,3)
      ELSEIF ( ioth/=0 ) THEN
!
!     COMPUTE TL TRANSPOSE
!
!     TRANSPOSE ROTATION PRODUCT
!
         DO i = 1 , 3
            vin(i) = Vin1(i)
            DO j = 1 , 3
               ti(i,j) = Tz(j,i)
            ENDDO
         ENDDO
         CALL mpyl(ti(1,1),vin(1),3,3,1,Vout1(1))
      ELSE
         DO i = 1 , 3
            vin(i) = Vin1(i)
         ENDDO
         CALL mpyl(Tz(1,1),vin(1),3,3,1,Vout1(1))
      ENDIF
   ENDIF
   GOTO 200
!
!
   ENTRY glbbas(Vin1,Vout1,Pont,Icstm)
!     ====================================
!
   tonly = .FALSE.
 800  ASSIGN 900 TO iexit
   ioth = 1
   GOTO 100
!
!     CONVERTS FROM GLOBAL TO BASIC
!
 900  IF ( Tysys>=2 ) GOTO 700
   IF ( tonly ) THEN
!
!     RETURN THE TRANSFORMATION ONLY.
!
      T(1) = To(1,1)
      T(2) = To(2,1)
      T(3) = To(3,1)
      T(4) = To(1,2)
      T(5) = To(2,2)
      T(6) = To(3,2)
      T(7) = To(1,3)
      T(8) = To(2,3)
      T(9) = To(3,3)
   ELSE
      DO i = 1 , 3
         vin(i) = Vin1(i)
      ENDDO
      CALL mpyl(To(1,1),vin(1),3,3,1,Vout1(1))
   ENDIF
   GOTO 200
END SUBROUTINE basglb
