!*==ascm12.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ascm12(Name,Iphase,Isol,Nogo)
   USE c_asdbd
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Name
   INTEGER :: Iphase
   INTEGER :: Isol
   INTEGER :: Nogo
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6,1) , SAVE :: comnd
   INTEGER :: i , icomnd , j , k
   INTEGER , DIMENSION(7,15) , SAVE :: ptbs
   INTEGER , DIMENSION(18,6) , SAVE :: rdmap
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
!
!
!     PLOT COMMAND DMAP DATA
!
   DATA comnd/4HPLOT , 6 , 0 , 0 , 15 , 0/
   DATA rdmap/4HPLTM , 4HRG   , 4H  CA , 4HSECC , 4H,PCD , 4HB/PL , 4HTSTP , 4H,GPS , 4HTP,E , 4HLSTP , 4H,BGS , 4HTP,C , 4HASST ,  &
       &4HP,EQ , 4HSTP/ , 4H*NAM , 4HE    , 4H */  , 4H     , 4H     , 4H  S, , 4HN,NG , 4HP/S, , 4HN,LS , 4HIL/S , 4H,N,N ,        &
      & 4HPSET , 4H $   , 8*4H     , 4HSETV , 4HAL   , 4H  // , 4HS,N, , 4HPLTF , 4HLG/1 , 4H/S,N , 4H,PFI , 4HL/0  , 4H$    ,      &
       &8*4H     , 4HPLOT , 4H     , 4H  PL , 4HTSTP , 4H,GPS , 4HTP,E , 4HLSTP , 4H,CAS , 4HSTP, , 4HBGST , 4HP,EQ , 4HSTP, ,      &
       &4H,,,, , 4H,,/P , 4HMSTP , 4H/NGP , 4H/LSI , 4HL/   , 4H     , 4H     , 4H  S, , 4HN,NP , 4HSET/ , 4HS,N, , 4HPLTF ,        &
      & 4HLG/S , 4H,N,P , 4HFIL  , 4H$    , 7*4H     , 4HPRTM , 4HSG   , 4H  PM , 4HSTP/ , 4H/ $  , 13*4H    /
   DATA ptbs/1 , 26 , 26 , 3 , 4HSTEP , 0 , 0 , 1 , 32 , 32 , 3 , 4HSTEP , 0 , 0 , 1 , 38 , 38 , 3 , 4HSTEP , 0 , 0 , 1 , 44 , 44 , &
      & 3 , 4HSTEP , 0 , 0 , 1 , 51 , 51 , 3 , 4HSTEP , 0 , 0 , 1 , 57 , 57 , 3 , 4HSTEP , 0 , 0 , 1 , 62 , 62 , 8 , 4HNAME , 0 ,   &
      & 0 , 4 , 14 , 14 , 3 , 4HSTEP , 0 , 0 , 4 , 20 , 20 , 3 , 4HSTEP , 0 , 0 , 4 , 26 , 26 , 3 , 4HSTEP , 0 , 0 , 4 , 33 , 33 ,  &
      & 3 , 4HSTEP , 0 , 0 , 4 , 39 , 39 , 3 , 4HSTEP , 0 , 0 , 4 , 45 , 45 , 3 , 4HSTEP , 0 , 0 , 4 , 58 , 58 , 3 , 4HSTEP , 0 ,   &
      & 0 , 6 , 13 , 13 , 3 , 4HSTEP , 0 , 0/
   DATA subnam/4HASCM , 2H12/
!
!     VALIDATE COMMAND AND SET POINTERS
!
   IF ( Name/=comnd(1,1) ) THEN
!
!     INPUT ERROR
!
      CALL mesage(7,0,subnam)
      Nogo = 1
      RETURN
   ELSE
      icomnd = 1
      irdm = 1
      nrdm = comnd(2,icomnd)
      ixtra = irdm + 18*nrdm
      nxtra = comnd(3,icomnd)
      ioct = ixtra + nxtra
      noct = comnd(4,icomnd)
      iptbs = ioct + 3*noct
      nptbs = comnd(5,icomnd)
      iph = iptbs + 7*nptbs
      nph = comnd(6,icomnd)
!
!     MOVE RDMAP DATA
!
      k = 0
      IF ( nrdm/=0 ) THEN
         DO j = 1 , nrdm
            DO i = 1 , 18
               k = k + 1
               idat(k) = rdmap(i,j)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE PTBS DATA
!
      IF ( nptbs/=0 ) THEN
         DO j = 1 , nptbs
            DO i = 1 , 7
               k = k + 1
               idat(k) = ptbs(i,j)
            ENDDO
         ENDDO
      ENDIF
   ENDIF
!
!
END SUBROUTINE ascm12
