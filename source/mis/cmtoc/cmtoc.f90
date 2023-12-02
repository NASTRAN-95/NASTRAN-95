!*==cmtoc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmtoc
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_output
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER :: i , ia , ib , itest , itot , kdh , nt , nwds
   INTEGER , DIMENSION(96) , SAVE :: ihed
   INTEGER , DIMENSION(2) :: name
   INTEGER , SAVE :: nheqss
   LOGICAL :: print
   EXTERNAL andf , close , mesage , open , page , page2 , rshift , sfetch , suread , write
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE GENERATES A TABLE OF CONTENTS FOR A COMBINE
!     OPERATION. FOR EACH PSEUDO-STRUCTURE IT LISTS THE NAME, NUMBER
!     OF COMPONENTS, AND EACH COMPONENT BASIC SUBSTRUCTURE NAME.
!     THIS DATA IS THEN WRITTEN ON SCRATCH FILE SCTOC.
!
   DATA ihed/7*4H     , 4HP S  , 4HE U  , 4HD O  , 4HS T  , 4HR U  , 4HC T  , 4HU R  , 4HE    , 4HT A  , 4HB L  , 4HE    , 4HO F  , &
       &4H  C  , 4HO N  , 4HT E  , 4HN T  , 4HS    , 15*4H     , 4H PSE , 4HUDO- , 4H     , 4H   N , 4HO. O , 4HF    , 26*2H   ,    &
       &4HSTRU , 4HCTUR , 4HE    , 4H COM , 4HPONE , 4HNTS  , 4H   - , 4H---- , 4H---- , 4H- CO , 4HMPON , 4HENT  , 4HNAME ,        &
      & 4HS -- , 4H---- , 4H---- , 4H-    , 8*4H    /
   DATA aaa/4HCMTO , 4HC   /
   DATA nheqss/4HEQSS/
!
   print = .FALSE.
   IF ( andf(rshift(iprint,1),1)==1 ) print = .TRUE.
   tocopn = .TRUE.
   itot = 0
   DO i = 1 , 96
      ihdr(i) = ihed(i)
   ENDDO
   IF ( print ) CALL page
   CALL open(*100,sctoc,z(buf5),1)
   DO i = 1 , npsub
      name(1) = combo(i,1)
      name(2) = combo(i,2)
      CALL sfetch(name,nheqss,1,itest)
      CALL suread(z(score),-1,nwds,itest)
      z(score) = name(1)
      z(score+1) = name(2)
      CALL write(sctoc,z(score),3,0)
      itot = itot + 3
      ia = score
      ib = score + 2
      IF ( print ) WRITE (outt,99001) (z(kdh),kdh=ia,ib)
99001 FORMAT (34X,2A4,6X,I4)
      combo(i,5) = z(score+2)
      nwds = nwds - 4
      ia = score + 4
      ib = ia + nwds - 1
      nt = (ib-ia+1)/8
      IF ( nt==0 ) nt = 1
      IF ( print ) CALL page2(nt)
      IF ( print ) WRITE (outt,99002) (z(kdh),kdh=ia,ib)
99002 FORMAT (1H+,57X,2X,2A4,2X,2A4,2X,2A4,2X,2A4,/(58X,2X,2A4,2X,2A4,2X,2A4,2X,2A4))
      itot = itot + nwds
      CALL write(sctoc,z(score+4),nwds,1)
   ENDDO
   CALL close(sctoc,1)
   CALL open(*100,sctoc,z(buf5),0)
!
!     DETERMINE WHETHER TO CLOSE FILE
!
   IF ( itot<=xxx ) RETURN
   tocopn = .FALSE.
   CALL close(sctoc,1)
   RETURN
!
 100  CALL mesage(-1,sctoc,aaa)
END SUBROUTINE cmtoc
