
SUBROUTINE magbdy
   IMPLICIT NONE
   INTEGER Iout , Ipg , Iz(1) , Sysbuf
   CHARACTER*23 Ufm
   REAL Z(1)
   COMMON /blank / Ipg
   COMMON /system/ Sysbuf , Iout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER buf1 , eqexin , file , geom1 , i , idx , ieqex , jloc , lcore , mcb(7) , n , nam(2) , neq , ngrids , npts , permbd ,     &
         & permby(2)
   INTEGER korsz
!
!     THIS ROUTINE PICKS UP THE GRIDS ON THE AIR/IRON INTERFACES
!     FROM A PERMBDY CARD,CONVERTS EXTERNAL TO INTERNAL SILS, AND
!     STORES RESULTS ON PERMBD WHICH IS READ IN SSG1. SSG1 WILL NEED TO
!     COMPUTE MAGNETIC LOADS ONLY AT THESE POINTS.
!
!     MAGBDY   GEOM1,HEQEXIN/PERMBD/V,N,IPG $
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA nam/4HMAGB , 4HDY  /
   DATA geom1 , eqexin , permbd/101 , 102 , 201/
   DATA permby/4201 , 42/
!
   lcore = korsz(Z)
   buf1 = lcore - Sysbuf
   lcore = buf1 - 1
   IF ( lcore>0 ) THEN
!
!     SEE IF A PERMBDY CARD EXISTS
!
      Ipg = -1
      file = geom1
      CALL preloc(*600,Z(buf1),geom1)
      CALL locate(*100,Z(buf1),permby,idx)
      Ipg = 1
!
!     READ PERMBDY INTO CORE
!
      CALL read(*700,*200,geom1,Z,lcore,0,npts)
   ENDIF
   GOTO 800
!
!     NO PERMBDY CARD - RETURN
!
 100  CALL close(geom1,1)
   RETURN
 200  CALL close(geom1,1)
!
!     READ IN 1ST RECORD OF EQEXIN
!
   lcore = lcore - npts
   ieqex = npts
   CALL gopen(eqexin,Z(buf1),0)
   file = eqexin
   CALL read(*700,*300,eqexin,Z(ieqex+1),lcore,0,neq)
   GOTO 800
 300  CALL close(eqexin,1)
   ngrids = neq/2
   lcore = lcore - neq
!
!     GET THE INTERNAL NUMBER (=SIL NUMBER FOR HEAT TRAMSFER)FOR EACH
!     POINT ON PERMBDY AND STORE IT BACK ONTO EXTERNAL NUMBER,SINCE THE
!     EXTERNAL IS NO LONGER NEEDED
!
   DO i = 1 , npts
      CALL bisloc(*400,Iz(i),Iz(ieqex+1),2,ngrids,jloc)
      Iz(i) = Iz(ieqex+jloc+1)
   ENDDO
   GOTO 500
!
 400  WRITE (Iout,99001) Ufm , Iz(i)
99001 FORMAT (A23,', GRID',I9,' ON PERMBDY CARD DOES NOT EXIST')
   CALL mesage(-61,0,0)
!
!     WRITE THESE INTERNAL ID-S ONTO PERMBD
!
 500  CALL gopen(permbd,Z(buf1),1)
   CALL write(permbd,Iz(1),npts,1)
   CALL close(permbd,1)
   mcb(1) = permbd
   mcb(2) = npts
   DO i = 3 , 7
      mcb(i) = 0
   ENDDO
   CALL wrttrl(mcb)
!
   RETURN
!
 600  n = -1
   GOTO 900
 700  n = -2
   GOTO 900
 800  n = -8
   file = 0
 900  CALL mesage(n,file,nam)
END SUBROUTINE magbdy