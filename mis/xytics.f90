
SUBROUTINE xytics(Iout,Out,Ndevis,R1,R2,Iskip,Log,Iflag)
   IMPLICIT NONE
   INTEGER Iflag , Iskip , Log , Ndevis
   REAL R1 , R2
   INTEGER Iout(8)
   REAL Out(8)
   REAL div , endv , finc , first , length , temp
   INTEGER iinc , ipower , itics , last , maxdig , nfirst , ntemp
!
!     THIS SUBROUTINE PERFORMS ONLY TIC COMPUTATIONS FOR XYDUMP.
!
!
   IF ( Log/=0 ) THEN
!
!     LOG SCALE - INITIAL LABELING CALCULATED
!
      first = R1
      itics = Log
      endv = R2
      finc = 10.0
      maxdig = 1
      ipower = 0
   ELSE
      IF ( R1==R2 ) R2 = R1 + 1.0
      div = Ndevis
      IF ( div<=0.0 ) div = 5.0
      length = R2 - R1
      IF ( Iflag/=0 ) div = length
      IF ( length<=0.0 ) THEN
!
!     LENGTH = 0
!
         itics = 0
      ELSE
         finc = 1.0001*length/div
!
!     CONVERT FINC TO SCIENTIFIC AND ROUND OFF TO 1 DIGIT (1 TO 10)
!
         ipower = 0
         IF ( finc<1.0 ) THEN
            DO
               ipower = ipower - 1
               finc = finc*10.0
               IF ( finc>=1.0 ) EXIT
            ENDDO
         ELSE
            DO WHILE ( finc>=10.0 )
               ipower = ipower + 1
               finc = finc/10.0
            ENDDO
         ENDIF
         iinc = 10
         IF ( finc<7.5 ) iinc = 5
         IF ( finc<3.5 ) iinc = 2
         IF ( finc<1.5 ) iinc = 1
!
!     ACTUAL INCREMENT
!
         finc = float(iinc)*10.0**ipower
!
!     COMPUTE FIRST DIVISION POINT
!
         nfirst = R1*10.0**(-ipower) + sign(0.555,R1)
!
!     GUARANTEE THAT TICKS WILL STEP THROUGH ZERO
!
         ntemp = nfirst/iinc
         nfirst = ntemp*iinc
         first = float(nfirst)*10.0**(ipower)
!
!     GET LOWEST VALUE OF FRAME
!
         IF ( first>R1 ) THEN
!
!     CHECK ABAINST EPSILON DIFFERENCE.  SENSITIVE TO TRUNCATION
!
            length = finc*1.0E-4
            IF ( first-R1<length ) first = first - length
            IF ( first-R1>=length ) first = first - finc
            nfirst = first*10.0**(-ipower) + sign(0.5,R1)
         ENDIF
         itics = (R2-first)/finc + 1.5
         temp = float(itics-1)*finc + first
         endv = temp
         IF ( endv<R2 ) THEN
            length = finc*2.0E-4
            IF ( endv+length>=R2 ) endv = endv + length
            IF ( endv+length<R2 ) endv = endv + finc
            itics = (endv-first)/finc + 0.5
            temp = float(itics-1)*finc + first
         ENDIF
         IF ( endv-temp<finc/4.0 ) itics = itics - 1
!
!     FIND MAXIMUM NUMBER OF DIGITS
!
         last = nfirst + iinc*itics
         last = max0(iabs(last),iabs(nfirst))
         maxdig = 1
         DO WHILE ( last>=10 )
            maxdig = maxdig + 1
            last = last/10
         ENDDO
      ENDIF
   ENDIF
   Out(1) = first
   Out(2) = finc
   Out(3) = endv
   Iout(4) = maxdig
   Iout(5) = ipower
   Iout(6) = itics
   Iout(7) = Iskip
   RETURN
!
END SUBROUTINE xytics
