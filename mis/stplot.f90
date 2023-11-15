
SUBROUTINE stplot(Pltnum)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bframs , Camera , Char(60) , Date(3) , Eof , Ksystm(65) , Model , Ploter , Pltape , Pltype
   REAL Chrscl , Cntx , Pbufsz , Reg(2,2) , Skpa1(3) , Skpa2(5) , Skpa3 , Xymax(13)
   COMMON /char94/ Char
   COMMON /pltdat/ Model , Ploter , Reg , Xymax , Chrscl , Skpa1 , Cntx , Skpa2 , Pltype , Pltape , Skpa3 , Eof
   COMMON /system/ Ksystm
   COMMON /xxparm/ Pbufsz , Camera , Bframs
!
! Dummy argument declarations
!
   INTEGER Pltnum
!
! Local variable declarations
!
   INTEGER i , id , idte(8) , j , lstplt , m , n
   REAL save(2,2)
!
! End of declarations
!
!
   EQUIVALENCE (Ksystm(15),Date(1))
   DATA idte/2*1H  , 1H/ , 2*1H  , 1H/ , 2*1H / , lstplt , m/0 , 0/
!
   IF ( Pltnum<0 ) THEN
!
!     TERMINATE A PLOT
!
      CALL skpfrm(1)
      CALL typint(0,0,0,0,0,1)
      IF ( Eof==0 ) CALL seof(Pltape)
      CALL sclose(Pltape)
   ELSE
!
!     SELECT THE PROPER CAMERA
!
      CALL selcam(Camera,Pltnum,0)
!
!     GENERATE THE ID PLOT
!
      IF ( Ploter/=lstplt ) CALL skpfrm(1)
      lstplt = Ploter
      CALL idplot(id)
      IF ( id/=0 ) THEN
         CALL selcam(Camera,Pltnum,0)
         CALL skpfrm(1)
      ENDIF
!
!     INSERT THE BLANK FRAMES ON FILM ONLY
!
      IF ( Camera/=2 .AND. iabs(Pltype)==1 ) THEN
         IF ( Bframs/=0 ) THEN
            CALL selcam(1,0,1)
            CALL skpfrm(max0(Bframs,1))
         ENDIF
      ENDIF
      CALL selcam(Camera,0,1)
!
!     TYPE THE PLOT NUMBER IN UPPER LEFT AND RIGHT CORNERS OF THE PLOT
!
      IF ( Pltnum/=0 ) THEN
         DO i = 1 , 2
            save(i,1) = Reg(i,1)
            Reg(i,1) = 0.
            save(i,2) = Reg(i,2)
            Reg(i,2) = Xymax(i)
         ENDDO
         CALL typint(0,0,0,0,0,-1)
         CALL typint(Reg(1,1)+Chrscl,Reg(2,2)-Chrscl,+1,Pltnum,0,0)
!
!     PRINT THE DATE
!
         IF ( m==0 ) THEN
            DO n = 1 , 7 , 3
               m = m + 1
               i = Date(m)/10 + 1
               j = Date(m) - (i-1)*10 + 1
               IF ( i==1 ) i = 48
               idte(n) = Char(i)
               idte(n+1) = Char(j)
            ENDDO
         ENDIF
!
         CALL tipe(8.*Cntx,Reg(2,2)-Chrscl,1,idte(1),8,0)
!
         CALL typint(Reg(1,2)-Chrscl,Reg(2,2)-Chrscl,-1,Pltnum,0,0)
         DO i = 1 , 2
            Reg(i,1) = save(i,1)
            Reg(i,2) = save(i,2)
         ENDDO
      ENDIF
      CALL typint(0,0,0,0,0,1)
   ENDIF
!
END SUBROUTINE stplot
