!*==stplot.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stplot(Pltnum)
   IMPLICIT NONE
   USE C_CHAR94
   USE C_PLTDAT
   USE C_SYSTEM
   USE C_XXPARM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Pltnum
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: date
   INTEGER :: i , id , j , n
   INTEGER , DIMENSION(8) , SAVE :: idte
   INTEGER , SAVE :: lstplt , m
   REAL , DIMENSION(2,2) :: save
   EXTERNAL idplot , sclose , selcam , seof , skpfrm , tipe , typint
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Ksystm(15),Date(1))
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
               i = date(m)/10 + 1
               j = date(m) - (i-1)*10 + 1
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
