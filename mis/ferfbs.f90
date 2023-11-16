
SUBROUTINE ferfbs(V1,V2,V3,Vb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dcore(1) , Xl(1)
   INTEGER Ibflt , Ibforv , Ibfsma , Icore(1) , Ksystm(65) , Ltpos(7) , Mcblt(7) , Mcbsma(7) , Nidlt , Nidorv , Nidsma , Nltli ,    &
         & Nout , Nsmali , Smapos(7)
   COMMON /feerim/ Nidsma , Nidlt , Nidorv , Nltli , Nsmali , Ibfsma , Ibflt , Ibforv , Smapos , Ltpos
   COMMON /opinv / Mcblt , Mcbsma
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Icore
!
! Dummy argument declarations
!
   REAL V1(1) , V2(1) , V3(1) , Vb(1)
!
! Local variable declarations
!
   INTEGER i , iblk(20) , icol , icrow , ii , ik , ilrow , j , ji , mem , nrow , ntms , ntmsnx , ntmss
   REAL v2j , v3j , xljj
!
! End of declarations
!
!
!  FERFBS is a modification of the old FRBK subroutine.  It has been
!  modified to read matrix data from memory until that data is exhausted
!  and then to read the remaining data from the file.
!
   EQUIVALENCE (Ksystm(02),Nout)
   EQUIVALENCE (Dcore(1),Icore(1),Xl)
!
   nrow = Mcblt(2)
   DO i = 1 , nrow
      V2(i) = V1(i)
   ENDDO
   ilrow = Ltpos(1)
   icrow = nrow
   IF ( ilrow==0 .AND. Nidlt/=0 ) GOTO 500
!
!     BACKWARD SUBSTITUTION
!
!     POSITION FILE TO LAST COLUMN
!
   IF ( Nidlt==0 ) THEN
      CALL rewind(Mcblt)
      CALL skprec(Mcblt,nrow+1)
   ELSE
      CALL dsspos(Mcblt,Ltpos(5),Ltpos(6),Ltpos(7))
   ENDIF
   iblk(1) = Mcblt(1)
   j = nrow
 100  iblk(8) = -1
   icrow = j
   IF ( j<=ilrow ) GOTO 500
 200  CALL getstb(*400,iblk(1))
   ntms = iblk(6)
   ji = iblk(5)
   ik = iblk(4)
   IF ( ik-ntms+1==j ) THEN
      ntms = ntms - 1
      xljj = Xl(ji-ntms)
      IF ( ntms==0 ) GOTO 300
   ENDIF
   v2j = V2(j)
   DO ii = 1 , ntms
      v2j = v2j + Xl(ji)*V2(ik)
      ji = ji - 1
      ik = ik - 1
   ENDDO
   V2(j) = v2j
 300  CALL endgtb(iblk(1))
   GOTO 200
 400  V2(j) = V2(j)/xljj
   IF ( j==1 ) GOTO 900
   j = j - 1
   GOTO 100
!
!     CONTINUE BACKWARD SUBSTITUTION WITH DATA IN MEMORY
!
 500  mem = Nltli
   ntms = Icore(mem)
   mem = mem - ntms - 3
   j = icrow
 600  icol = Icore(mem)
   IF ( icol/=j ) GOTO 800
   ntms = Icore(mem+1)
   ntmss = ntms
   ji = mem + 1 + ntms
   ik = Icore(mem+2+ntms) + ntms - 1
   IF ( ik-ntms+1==j ) THEN
      ntms = ntms - 1
      xljj = Dcore(ji-ntms)
      IF ( ntms==0 ) GOTO 700
   ENDIF
   v2j = V2(j)
   DO ii = 1 , ntms
      v2j = v2j + Dcore(ji)*V2(ik)
      ji = ji - 1
      ik = ik - 1
   ENDDO
   V2(j) = v2j
 700  IF ( mem/=Nidlt ) THEN
      ntmsnx = Icore(mem-1)
      mem = mem - ntmsnx - 4
      GOTO 100
   ENDIF
 800  V2(j) = V2(j)/xljj
   IF ( j/=1 ) THEN
      j = j - 1
      GOTO 600
   ENDIF
 900  CALL ferlts(Mcbsma(1),V2(1),V3(1),Vb(1))
!
! BEGIN FORWARD SWEEP DIRECTLY ON V3
!
   icrow = 1
   IF ( Nidlt==0 ) THEN
      CALL rewind(Mcblt)
      CALL skprec(Mcblt,1)
      GOTO 1100
   ELSE
      mem = Nidlt
      DO j = 1 , nrow
         icrow = j
         IF ( j>ilrow ) GOTO 1000
         DO
            icol = Icore(mem)
            IF ( icol/=j ) EXIT
            ji = mem + 2
            ntms = Icore(mem+1)
            ntmss = ntms
            ik = Icore(mem+2+ntms)
            IF ( ik==j ) THEN
               ntms = ntms - 1
               V3(j) = V3(j)/Dcore(ji)
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
         ENDDO
      ENDDO
      GOTO 99999
   ENDIF
!
!     CONTINUE FORWARD SWEEP DIRECTLY ON V3
!
!     POSITION FILE TO CONTINUE READING COLUMN DATA NOT IN MEMORY
!
 1000 CALL dsspos(Mcblt,Ltpos(2),Ltpos(3),Ltpos(4))
 1100 DO j = icrow , nrow
      iblk(8) = -1
      DO
         CALL getstr(*1200,iblk)
         ik = iblk(4)
         ji = iblk(5)
         ntms = iblk(6)
         IF ( ik==j ) THEN
            ntms = ntms - 1
            V3(j) = V3(j)/Xl(ji)
            ji = ji + 1
            ik = ik + 1
         ENDIF
         IF ( ntms/=0 ) THEN
            v3j = V3(j)
            DO ii = 1 , ntms
               V3(ik) = V3(ik) + Xl(ji)*v3j
               ik = ik + 1
               ji = ji + 1
            ENDDO
         ENDIF
         CALL endget(iblk(1))
      ENDDO
 1200 ENDDO
99999 RETURN
END SUBROUTINE ferfbs
