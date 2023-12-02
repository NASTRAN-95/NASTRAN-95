!*==ascm02.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ascm02(Name,Iphase,Isol,Nogo)
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
   INTEGER , DIMENSION(6,2) , SAVE :: comnd
   INTEGER :: i , icomnd , j , k
   INTEGER , DIMENSION(7,1) , SAVE :: ptbs
   INTEGER , DIMENSION(18,6) , SAVE :: rdmap
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
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
      IF ( Name==comnd(1,i) ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     INPUT ERROR
!
   CALL mesage(7,0,subnam)
   Nogo = 1
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Icomnd = I
      irdm = 1
      nrdm = Comnd(2,Icomnd)
      ixtra = irdm + 18*nrdm
      nxtra = Comnd(3,Icomnd)
      ioct = ixtra + nxtra
      noct = Comnd(4,Icomnd)
      iptbs = ioct + 3*noct
      nptbs = Comnd(5,Icomnd)
      iph = iptbs + 7*nptbs
      nph = Comnd(6,Icomnd)
!
!     MOVE RDMAP DATA
!
      K = 0
      IF ( nrdm/=0 ) THEN
         DO J = 1 , nrdm
            DO I = 1 , 18
               K = K + 1
               idat(K) = Rdmap(I,J)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE PTBS DATA
!
      IF ( nptbs/=0 ) THEN
         DO J = 1 , nptbs
            DO I = 1 , 7
               K = K + 1
               idat(K) = Ptbs(I,J)
            ENDDO
         ENDDO
      ENDIF
!
   END SUBROUTINE spag_block_1
!
END SUBROUTINE ascm02
