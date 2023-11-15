
SUBROUTINE ascm02(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idat(117) , Ioct , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
!
! Dummy argument declarations
!
   INTEGER Iphase , Isol , Name , Nogo
!
! Local variable declarations
!
   INTEGER comnd(6,2) , i , icomnd , j , k , ptbs(7,1) , rdmap(18,6) , subnam(2)
!
! End of declarations
!
!
!     RUN COMMAND DATA
!
   DATA comnd/4HRUN  , 1 , 0 , 0 , 1 , 0 , 4HENDD , 6 , 0 , 0 , 0 , 0/
   DATA rdmap/4HPARA , 4HM    , 4H  // , 4H*ADD , 4H*/DR , 4HY/-1 , 4H /0  , 4H$    , 4H     , 4H     , 8*4H     , 4HLABE , 4HL    ,&
       &4H  LB , 4HSEND , 4H $   , 13*4H     , 4HPARA , 4HM    , 4H  // , 4H*ADD , 4H*/DR , 4HY/DR , 4HY/1  , 4H$    , 4H     ,     &
       &4H     , 8*4H     , 4HCOND , 4H     , 4H  FI , 4HNIS, , 4HDRY  , 4H$    , 12*4H     , 4HREPT , 4H     , 4H  LB , 4HSBEG ,   &
       &4H,1 $ , 13*4H     , 4HJUMP , 4H     , 4H  FI , 4HNIS  , 4H$    , 13*4H    /
   DATA ptbs/1 , 22 , 23 , 3 , 4HRUN  , 0 , 0/
!
   DATA subnam/4HASCM , 2H02/
!
!     VALIDATE COMMAND AND SET POINTERS
!
   DO i = 1 , 2
      IF ( Name==comnd(1,i) ) GOTO 100
   ENDDO
!
!     INPUT ERROR
!
   CALL mesage(7,0,subnam)
   Nogo = 1
   GOTO 99999
 100  icomnd = i
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
!
   RETURN
!
99999 END SUBROUTINE ascm02
