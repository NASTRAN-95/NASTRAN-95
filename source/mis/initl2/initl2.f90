!*==initl2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE initl2(Offset,Deltt)
   USE c_dcompx
   USE c_names
   USE c_saddx
   USE c_sfact
   USE c_trdxx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Offset
   REAL :: Deltt
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: alpha , beta
   INTEGER :: file , i , ip1 , iprec , itypal , itypbt
   INTEGER , DIMENSION(7) :: ifila , ifilb , ifilc
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL decomp , korsz , mesage , sadd , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     INITL2 WILL COMPUTE THE STARTING VALUES FOR THE INTEGRATION
!     ROUTINE
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!
   !>>>>EQUIVALENCE (Mcbs(1),Ifila(1)) , (Mcbs(8),Itypal) , (Mcbs(9),Alpha(1)) , (Mcbs(13),Ifilb(1)) , (Mcbs(20),Itypbt) ,               &
!>>>>    & (Mcbs(21),Beta(1)) , (Mcbs(61),Ifilc(1))
   DATA name/4HINIT , 4HL2  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nomat = 2
         iprec = rdp
         alpha(2) = 0.0D0
         beta(2) = 0.0D0
         nx = korsz(z) - Offset
         nz = nx
!
!     FORM AND DECOMPOSE THE LEFT HAND MATRIX
!
         itypal = rdp
         itypbt = rdp
         alpha(1) = 1.0D0/Deltt**2
         beta(1) = 0.5D0/Deltt
         ifilc(4) = 6
         DO i = 1 , 7
            ifila(i) = filem(i)
            ifilb(i) = fileb(i)
         ENDDO
         ifilc(2) = filek(2)
         ifilc(1) = iscr2
         IF ( filek(1)<=0 ) ifilc(1) = iscr1
         ifilc(3) = filek(2)
         IF ( ifila(1)/=0 .AND. ifila(4)/=6 ) ifilc(4) = sqr
         IF ( ifilb(1)/=0 .AND. ifilb(4)/=6 ) ifilc(4) = sqr
         ifilc(5) = iprec
         IF ( filem(1)<=0 .AND. fileb(1)<=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sadd(z,z)
         IF ( filek(1)<=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO i = 1 , 7
            ifila(i) = ifilc(i)
            ifilb(i) = filek(i)
         ENDDO
         IF ( ifilb(4)/=6 ) ifilc(4) = sqr
         ifilc(1) = iscr1
         alpha(1) = 1.0D0
         beta(1) = 1.0D0/3.0D0
         CALL sadd(z,z)
         spag_nextblock_1 = 3
      CASE (3)
         CALL wrttrl(ifilc)
         IF ( ifilc(4)/=6 ) THEN
!
!     SET UP FOR UNSYMMETRIC DECOMPOSITION
!
            isym = 1
            DO i = 1 , 7
               ia(i) = ifilc(i)
            ENDDO
            il(1) = iscr2
            iu(1) = iscr3
            iscr10 = iscr4
            iscr20 = iscr5
            iscr30 = iscr6
            il(5) = iprec
            file = ia(1)
            CALL decomp(*20,z(1),z(1),z(1))
            CALL wrttrl(il)
            CALL wrttrl(iu)
         ELSE
!
!     SET UP FOR SYMMETRIC DECOMPOSITION
!
            DO i = 1 , 7
               ifa(i) = ifilc(i)
            ENDDO
            ifl(1) = iscr2
            ifu(1) = iscr3
            isc1 = iscr4
            isc2 = iscr5
            isc3 = iscr6
            ifl(5) = iprec
            ichl = 0
            nxx = nx
            file = ifa(1)
            CALL sdcomp(*20,z,z,z)
            CALL wrttrl(ifl)
            isym = 0
         ENDIF
!
!     FORM FIRST RIGHT HAND MATRIX
!
         DO i = 1 , 7
            ifila(i) = filem(i)
         ENDDO
         alpha(1) = 2.0D0/Deltt**2
         beta(1) = -1.0D0/3.0D0
         ifilc(1) = iscr1
         CALL sadd(z,z)
!
!     FORM SECOND RIGHT HAND MATRIX
!
         alpha(1) = -1.0D0/Deltt**2
         ifilc(1) = iscr5
         CALL sadd(z,z)
         DO i = 1 , 7
            ifila(i) = ifilc(i)
            ifilb(i) = fileb(i)
         ENDDO
         alpha(1) = 1.0D0
         beta(1) = 0.5D0/Deltt
         ifilc(1) = iscr4
         CALL sadd(z,z)
         RETURN
!
!     ERRORS
!
 20      ip1 = -5
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ip1,file,name(1))
         spag_nextblock_1 = 5
      CASE (5)
!
!     NO BDD OR MDD
!
         IF ( filek(1)<=0 ) THEN
!
!     ILLEGAL INPUT.   NO MATRICES
!
            ip1 = -7
            spag_nextblock_1 = 4
         ELSE
            ifilc(1) = 0
            spag_nextblock_1 = 2
         ENDIF
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE initl2
