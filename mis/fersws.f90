
SUBROUTINE fersws(V1,V3,Vb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dcore(1) , Smapos(7) , Xl(1)
   INTEGER Ibflt , Ibforv , Ibfsma , Icore(1) , Io , Ksystm(65) , Ltpos(7) , Mcblt(7) , Mcbsma(7) , Nidlt , Nidorv , Nidsma ,       &
         & Nltli , Nsmali
   COMMON /feerim/ Nidsma , Nidlt , Nidorv , Nltli , Nsmali , Ibfsma , Ibflt , Ibforv , Smapos , Ltpos
   COMMON /opinv / Mcblt , Mcbsma
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Icore
!
! Dummy argument declarations
!
   REAL V1(1) , V3(1) , Vb(1)
!
! Local variable declarations
!
   INTEGER iblk(20) , icol , icrow , ii , ik , ilrow , j , ji , mem , nrow , ntms , ntmsnx , ntmss
   REAL su , sum , v3j , xljj , zero
!
! End of declarations
!
!
!  The original to this subroutine was FRSW.  It has been modified
!  to read the matrix data from memory and after this data is exhausted
!  then to read the remaining data from the file.
!
   EQUIVALENCE (Ksystm(02),Io)
   EQUIVALENCE (Dcore(1),Icore(1),Xl(1))
   DATA zero/0.0/
!
   nrow = Mcblt(2)
   CALL ferlts(Mcbsma(1),V1(1),V3(1),Vb(1))
!   FORWARD SWEEP DIRECTLY ON V3
   icrow = 1
   IF ( Nidlt==0 ) THEN
      CALL rewind(Mcblt)
      CALL skprec(Mcblt,1)
      GOTO 200
   ELSE
      ilrow = Ltpos(1)
      mem = Nidlt
      DO j = 1 , nrow
         icrow = j
         IF ( icrow>ilrow ) GOTO 100
         DO
            icol = Icore(mem)
            IF ( icol/=j ) THEN
               V3(j) = V3(j)/xljj
               EXIT
            ELSE
               ji = mem + 2
               ntms = Icore(mem+1)
               ntmss = ntms
               ik = Icore(mem+2+ntms)
               IF ( ik==j ) THEN
                  ntms = ntms - 1
                  xljj = Dcore(ji)
                  ji = ji + 1
                  ik = ik + 1
               ENDIF
               IF ( ntms/=0 ) THEN
                  v3j = V3(j)
                  DO ii = 1 , ntms
                     V3(ik) = V3(ik) + Dcore(ji)*v3j
                     ik = ik + 1
                     ji = ji + 1
                  ENDDO
               ENDIF
               mem = mem + ntmss + 4
            ENDIF
         ENDDO
      ENDDO
      GOTO 300
   ENDIF
! POSITION FILE TO APPROPRIATE COLUMN
 100  CALL dsspos(Mcblt,Ltpos(2),Ltpos(3),Ltpos(4))
 200  iblk(1) = Mcblt(1)
!
! CONTINUE WITH FORWARD SWEEP
!
   DO j = icrow , nrow
      iblk(8) = -1
      DO
         CALL getstr(*250,iblk)
         ik = iblk(4)
         ji = iblk(5)
         ntms = iblk(6)
         IF ( ik==j ) THEN
            ntms = ntms - 1
            xljj = Xl(ji)
            ji = ji + 1
            ik = ik + 1
         ENDIF
         IF ( ntms/=0 ) THEN
            v3j = V3(j)
            IF ( v3j/=zero ) THEN
               DO ii = 1 , ntms
                  V3(ik) = V3(ik) + Xl(ji)*v3j
                  ik = ik + 1
                  ji = ji + 1
               ENDDO
            ENDIF
         ENDIF
         CALL endget(iblk)
      ENDDO
 250  V3(j) = V3(j)/xljj
   ENDDO
!
!     BACKWARD SUBSTITUTION OMIT DIAGONAL
!
 300  icrow = nrow
   IF ( j==1 ) RETURN
   IF ( ilrow==nrow .AND. Nidlt/=0 ) THEN
!  CONTINUE BACKWARD SUBSTITUTION USING DATA FROM MEMORY
      mem = mem - ntmss - 4
      GOTO 600
   ELSE
      j = nrow
      iblk(8) = -1
   ENDIF
 400  DO
      CALL getstb(*500,iblk)
      ik = iblk(4)
      ji = iblk(5)
      ntms = iblk(6)
      IF ( ik-ntms+1==j ) ntms = ntms - 1
      IF ( ntms/=0 ) THEN
         su = zero
         DO ii = 1 , ntms
            sum = sum + Xl(ji)*V3(ik)
            ji = ji - 1
            ik = ik - 1
         ENDDO
         V3(j) = V3(j) + sum
      ENDIF
      CALL endgtb(iblk)
   ENDDO
 500  IF ( j==1 ) GOTO 99999
   j = j - 1
   IF ( j<=ilrow ) THEN
      mem = mem - ntmss - 4
   ELSE
      iblk(8) = -1
      GOTO 400
   ENDIF
 600  DO
      icol = Icore(mem)
      IF ( icol/=j ) THEN
         IF ( j==1 ) EXIT
         j = j - 1
      ELSE
         ntms = Icore(mem+1)
         ntmss = ntms
         ji = mem + 1 + ntms
         ik = Icore(mem+2+ntms) + ntms - 1
         IF ( ik-ntms+1==j ) ntms = ntms - 1
         IF ( ntms/=0 ) THEN
            v3j = V3(j)
            DO ii = 1 , ntms
               v3j = v3j + Dcore(ji)*V3(ik)
               ji = ji - 1
               ik = ik - 1
            ENDDO
            V3(j) = v3j
         ENDIF
         IF ( mem==Nidlt ) EXIT
         ntmsnx = Icore(mem-1)
         mem = mem - ntmsnx - 4
      ENDIF
   ENDDO
99999 END SUBROUTINE fersws
