!*==gptlbl.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gptlbl(Gplst,X,U,Deform,Buf)
   IMPLICIT NONE
   USE C_BLANK
   USE C_PLTDAT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER :: Deform
   INTEGER :: Buf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: gp , gpt , gpx
   INTEGER , SAVE :: inprew , rew
   REAL :: xx , yy
   EXTERNAL close , fread , gopen , typint
!
! End of declarations rewritten by SPAG
!
!
   DATA inprew , rew/0 , 1/
!
   CALL gopen(Exgpid,Gplst(Buf),inprew)
   CALL typint(0,0,0,0,0,-1)
   DO gp = 1 , Ngp
      CALL fread(Exgpid,gpt,1,0)
      CALL fread(Exgpid,gpx,1,0)
      gpx = Gplst(gpx)
!
!     IF THE GRID POINT INDEX IS 0 (NOT IN SET) OR NEGATIVE (EXCLUDED),
!     NEVER PUT A LABEL AT THAT GRID POINT.
!
      IF ( gpx>0 ) THEN
!
!     TYPE THE GRID POINT ID
!
         IF ( Deform/=0 ) THEN
            xx = U(1,gpx)
            yy = U(2,gpx)
         ELSE
            xx = X(2,gpx)
            yy = X(3,gpx)
         ENDIF
         CALL typint(xx+Cntx,yy,1,gpt,0,0)
      ENDIF
   ENDDO
!
   CALL close(Exgpid,rew)
   CALL typint(0,0,0,0,0,1)
END SUBROUTINE gptlbl
