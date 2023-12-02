!*==cmtoc.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmtoc
   IMPLICIT NONE
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_ZZZZZZ
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
   IF ( andf(rshift(Iprint,1),1)==1 ) print = .TRUE.
   Tocopn = .TRUE.
   itot = 0
   DO i = 1 , 96
      Ihdr(i) = ihed(i)
   ENDDO
   IF ( print ) CALL page
   CALL open(*100,Sctoc,Z(Buf5),1)
   DO i = 1 , Npsub
      name(1) = Combo(i,1)
      name(2) = Combo(i,2)
      CALL sfetch(name,nheqss,1,itest)
      CALL suread(Z(Score),-1,nwds,itest)
      Z(Score) = name(1)
      Z(Score+1) = name(2)
      CALL write(Sctoc,Z(Score),3,0)
      itot = itot + 3
      ia = Score
      ib = Score + 2
      IF ( print ) WRITE (Outt,99001) (Z(kdh),kdh=ia,ib)
99001 FORMAT (34X,2A4,6X,I4)
      Combo(i,5) = Z(Score+2)
      nwds = nwds - 4
      ia = Score + 4
      ib = ia + nwds - 1
      nt = (ib-ia+1)/8
      IF ( nt==0 ) nt = 1
      IF ( print ) CALL page2(nt)
      IF ( print ) WRITE (Outt,99002) (Z(kdh),kdh=ia,ib)
99002 FORMAT (1H+,57X,2X,2A4,2X,2A4,2X,2A4,2X,2A4,/(58X,2X,2A4,2X,2A4,2X,2A4,2X,2A4))
      itot = itot + nwds
      CALL write(Sctoc,Z(Score+4),nwds,1)
   ENDDO
   CALL close(Sctoc,1)
   CALL open(*100,Sctoc,Z(Buf5),0)
!
!     DETERMINE WHETHER TO CLOSE FILE
!
   IF ( itot<=Xxx ) RETURN
   Tocopn = .FALSE.
   CALL close(Sctoc,1)
   RETURN
!
 100  CALL mesage(-1,Sctoc,aaa)
END SUBROUTINE cmtoc
