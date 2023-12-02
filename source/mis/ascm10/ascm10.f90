!*==ascm10.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ascm10(Name,Iphase,Isol,Nogo)
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
   INTEGER , DIMENSION(6,6) , SAVE :: comnd
   INTEGER :: i , icomnd , j , k
   INTEGER , DIMENSION(7,10) , SAVE :: ptbs
   INTEGER , DIMENSION(18,2) , SAVE :: rdmap
   REAL , SAVE :: slash
   INTEGER , DIMENSION(2) , SAVE :: subnam
   INTEGER , DIMENSION(1) , SAVE :: xtra
   EXTERNAL khrfn1 , mesage
!
! End of declarations rewritten by SPAG
!
!
!     SUBSTRUCTURE UTILITY COMMANDS DMAP DATA
!
   DATA comnd/4HDEST , 2 , 0 , 0 , 3 , 0 , 4HEDIT , 2 , 0 , 0 , 3 , 0 , 4HEQUI , 2 , 1 , 0 , 5 , 0 , 4HSOFP , 2 , 0 , 0 , 10 , 0 ,  &
       &4HDELE , 2 , 0 , 0 , 10 , 0 , 4HRENA , 2 , 0 , 0 , 5 , 0/
   DATA slash/1H//
   DATA rdmap/4HSOFU , 4HT    , 4H  // , 4HDRY/ , 4H*NAM , 4HE    , 4H *!* , 4HOPER , 4H*/OP , 4HT!*N , 4HAME0 , 4H002* , 4H!*PR ,  &
       &4HEF*/ , 4H*ITM , 4H1*!* , 4HITM2 , 4H*/   , 4H     , 4H     , 4H  *I , 4HTM3* , 4H!*IT , 4HM4*/ , 4H*ITM , 4H5* $ ,        &
      & 4H     , 4H     , 8*4H    /
   DATA xtra/4HPREF/
   DATA ptbs/1 , 16 , 18 , 8 , 4HNAME , 0 , 0 , 1 , 27 , 29 , 4 , 4HOPER , 0 , 0 , 1 , 34 , 35 , 3 , 4HOPTI , 0 , 0 , 1 , 38 , 40 , &
      & 8 , 4HNEW  , 0 , 0 , 1 , 49 , 51 , 4 , 4HPREF , 0 , 0 , 1 , 56 , 58 , 4 , 4HITM1 , 0 , 0 , 1 , 63 , 65 , 4 , 4HITM2 , 0 ,   &
      & 0 , 2 , 11 , 12 , 4 , 4HITM3 , 0 , 0 , 2 , 17 , 19 , 4 , 4HITM4 , 0 , 0 , 2 , 24 , 26 , 4 , 4HITM5 , 0 , 0/
   DATA subnam/4HASCM , 2H10/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   rdmap(7,1) = khrfn1(rdmap(7,1),3,slash,1)
   rdmap(10,1) = khrfn1(rdmap(10,1),2,slash,1)
   rdmap(13,1) = khrfn1(rdmap(13,1),1,slash,1)
   rdmap(16,1) = khrfn1(rdmap(16,1),3,slash,1)
   rdmap(5,2) = khrfn1(rdmap(5,2),1,slash,1)
!
!     VALIDATE COMMAND AND SET POINTERS
!
   DO i = 1 , 6
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
!     MOVE XTRA DATA
!
      IF ( nxtra/=0 ) THEN
         DO I = 1 , nxtra
            K = K + 1
            idat(K) = Xtra(I)
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
END SUBROUTINE ascm10
