!*==stplot.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stplot(Pltnum)
   USE c_char94
   USE c_pltdat
   USE c_system
   USE c_xxparm
   IMPLICIT NONE
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
      IF ( eof==0 ) CALL seof(pltape)
      CALL sclose(pltape)
   ELSE
!
!     SELECT THE PROPER CAMERA
!
      CALL selcam(camera,Pltnum,0)
!
!     GENERATE THE ID PLOT
!
      IF ( ploter/=lstplt ) CALL skpfrm(1)
      lstplt = ploter
      CALL idplot(id)
      IF ( id/=0 ) THEN
         CALL selcam(camera,Pltnum,0)
         CALL skpfrm(1)
      ENDIF
!
!     INSERT THE BLANK FRAMES ON FILM ONLY
!
      IF ( camera/=2 .AND. iabs(pltype)==1 ) THEN
         IF ( bframs/=0 ) THEN
            CALL selcam(1,0,1)
            CALL skpfrm(max0(bframs,1))
         ENDIF
      ENDIF
      CALL selcam(camera,0,1)
!
!     TYPE THE PLOT NUMBER IN UPPER LEFT AND RIGHT CORNERS OF THE PLOT
!
      IF ( Pltnum/=0 ) THEN
         DO i = 1 , 2
            save(i,1) = reg(i,1)
            reg(i,1) = 0.
            save(i,2) = reg(i,2)
            reg(i,2) = xymax(i)
         ENDDO
         CALL typint(0,0,0,0,0,-1)
         CALL typint(reg(1,1)+chrscl,reg(2,2)-chrscl,+1,Pltnum,0,0)
!
!     PRINT THE DATE
!
         IF ( m==0 ) THEN
            DO n = 1 , 7 , 3
               m = m + 1
               i = date(m)/10 + 1
               j = date(m) - (i-1)*10 + 1
               IF ( i==1 ) i = 48
               idte(n) = char(i)
               idte(n+1) = char(j)
            ENDDO
         ENDIF
!
         CALL tipe(8.*cntx,reg(2,2)-chrscl,1,idte(1),8,0)
!
         CALL typint(reg(1,2)-chrscl,reg(2,2)-chrscl,-1,Pltnum,0,0)
         DO i = 1 , 2
            reg(i,1) = save(i,1)
            reg(i,2) = save(i,2)
         ENDDO
      ENDIF
      CALL typint(0,0,0,0,0,1)
   ENDIF
!
END SUBROUTINE stplot
