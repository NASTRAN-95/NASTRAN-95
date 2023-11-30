
SUBROUTINE gptlbl(Gplst,X,U,Deform,Buf)
   IMPLICIT NONE
   REAL Cntx , Skp1(9) , Skp2(5) , Skpa(3) , Skpplt(20)
   INTEGER Exgpid , Ngp
   COMMON /blank / Ngp , Skp1 , Skp2 , Exgpid
   COMMON /pltdat/ Skpplt , Skpa , Cntx
   INTEGER Buf , Deform
   INTEGER Gplst(1)
   REAL U(2,1) , X(3,1)
   INTEGER gp , gpt , gpx , inprew , rew
   REAL xx , yy
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
