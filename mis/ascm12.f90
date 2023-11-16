
SUBROUTINE ascm12(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idat(213) , Ioct , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
!
! Dummy argument declarations
!
   INTEGER Iphase , Isol , Name , Nogo
!
! Local variable declarations
!
   INTEGER comnd(6,1) , i , icomnd , j , k , ptbs(7,15) , rdmap(18,6) , subnam(2)
!
! End of declarations
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
      GOTO 99999
   ELSE
      icomnd = 1
      Irdm = 1
      Nrdm = comnd(2,icomnd)
      Ixtra = Irdm + 18*Nrdm
      Nxtra = comnd(3,icomnd)
      Ioct = Ixtra + Nxtra
      Noct = comnd(4,icomnd)
      Iptbs = Ioct + 3*Noct
      Nptbs = comnd(5,icomnd)
      Iph = Iptbs + 7*Nptbs
      Nph = comnd(6,icomnd)
!
!     MOVE RDMAP DATA
!
      k = 0
      IF ( Nrdm/=0 ) THEN
         DO j = 1 , Nrdm
            DO i = 1 , 18
               k = k + 1
               Idat(k) = rdmap(i,j)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE PTBS DATA
!
      IF ( Nptbs/=0 ) THEN
         DO j = 1 , Nptbs
            DO i = 1 , 7
               k = k + 1
               Idat(k) = ptbs(i,j)
            ENDDO
         ENDDO
      ENDIF
   ENDIF
!
   RETURN
!
99999 RETURN
END SUBROUTINE ascm12
