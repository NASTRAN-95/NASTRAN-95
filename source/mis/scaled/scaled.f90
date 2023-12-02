!*==scaled.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scaled(Type,Emord)
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
   INTEGER :: Emord
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: code , eid , gsube , i , ioutpt , ip , ipg , ncol , ngrids , nterms
   INTEGER , DIMENSION(2) , SAVE :: cpt
   INTEGER , DIMENSION(7) :: dict
   REAL(REAL64) , DIMENSION(16) :: dz
   INTEGER , DIMENSION(4) , SAVE :: gpt , gspt , kpt
   INTEGER , DIMENSION(2) :: icomp , isil
   INTEGER , DIMENSION(1) :: iest
   REAL , DIMENSION(16) :: z
   EXTERNAL emgout
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE PROCESSES CELAS, CDAMP, AND CMASS ELEMENTS.
!
!     TYPE  - DENOTES FORM OF EST DATA. IE CELAS1,CELAS2,ETC.
!     EMORD - DENOTES MATRIX  1 = CELAS = STIFFNESS MATRIX,
!                             2 = CMASS = MASS MATRIX,
!                             3 = CDAMP = DAMPING MATRIX
!
!     EST FOR ELAS ELEMENTS
!
!                     TYPE           TYPE           TYPE           TYPE
!             CELAS1         CELAS2         CELAS3         CELAS4
!             ------  ----   ------  ----   ------  ----   ------  ----
!     ECPT(1) IELID     I    IELID     I    IELID     I    IELID     I
!     ECPT(2) IGP1      I    K         R    IS1       I    K         R
!     ECPT(3) IGP2      I    IGP1      I    IS2       I    IS1       I
!     ECPT(4) IC1       I    IGP2      I    K         R    IS2       I
!     ECPT(5) IC2       I    IC1       I    GSUBE     R
!     ECPT(6) K         R    IC2       I    S         R
!     ECPT(7) GSUBE     R    GSUBE     R
!     ECPT(8) S         R    S         R
!
   !>>>>EQUIVALENCE (Ksystm(2),Ioutpt) , (z(1),dz(1)) , (Iest(1),Est(1))
   DATA gpt/2 , 3 , 2 , 3/ , cpt/4 , 5/ , kpt/6 , 2 , 4 , 2/
   DATA gspt/7 , 7 , 5 , 0/
!
!     TEST IF MATRIX TO BE PRODUCED IS REQUESTED
!
   IF ( imat(Emord)==0 ) RETURN
!
!     MOVE EST DATA TO LOCAL ARRAYS.  LOCATIONS ARE GIVEN BY DATA //
!
   eid = iest(1)
   ip = kpt(Type)
   z(1) = est(ip)
   gsube = 0
   icomp(1) = 0
   icomp(2) = 0
   dict(2) = 1
   ngrids = 2
   ip = gpt(Type)
   isil(1) = iest(ip)
   isil(2) = iest(ip+1)
   IF ( Type<3 ) THEN
      ip = cpt(Type)
      IF ( iest(ip)/=0 ) icomp(1) = iest(ip) - 1
      IF ( iest(ip+1)/=0 ) icomp(2) = iest(ip+1) - 1
   ENDIF
!
!     IF ONE SIL IS ZERO INSURE THAT IT IS THE SECOND.
!     IF BOTH SILS ARE NON-ZERO MAKE SURE HIGHER OF TWO IS SECOND.
!
   IF ( isil(2)/=0 ) THEN
      IF ( isil(1)/=0 ) THEN
         IF ( isil(1)<=isil(2) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
!
!     SWITCH SILS AND COMPS
!
      ip = isil(2)
      isil(2) = isil(1)
      isil(1) = ip
      ip = icomp(2)
      icomp(2) = icomp(1)
      icomp(1) = ip
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      IF ( Isil(2)>0 ) THEN
!
         IF ( Isil(2)/=Isil(1) ) THEN
            IF ( Icomp(1)==Icomp(2) ) THEN
!
!     COMPONENTS ARE THE SAME FOR BOTH POINTS
!
               Nterms = 4
               Ncol = 2
               Code = 2**Icomp(1)
               Z(2) = -Z(1)
               Z(3) = -Z(1)
               Z(4) = Z(1)
               CALL spag_block_2
               RETURN
            ENDIF
!
!     IF THE ELEMENT CONNECTS TWO COMPONENTS OF THE SAME POINT IT
!     MUST HAVE SPECIAL TREATMENT
!
         ELSEIF ( Icomp(2)==Icomp(1) ) THEN
!
            WRITE (Ioutpt,99001) uwm , Eid
99001       FORMAT (A25,' 3120, IMPROPER CONNECTION ON CELAS ELEMENT',I9)
            RETURN
!
!     IN THE GENERAL CASE, THE CONNECTED COMPONENTS MAY BE THE SAME
!     AND THE MATRIX IS A 2 BY 2.  IF THE COMPONENTS ARE DIFFERENT
!     THE MATRIX WILL BE A 4 BY 4 WITH ADDITIONAL ZEROS.
!
         ENDIF
!
         Nterms = 16
         Code = 2**Icomp(1) + 2**Icomp(2)
         Ncol = 4
         DO I = 2 , 16
            Z(I) = 0.0
         ENDDO
         IF ( Icomp(2)<Icomp(1) ) THEN
            Z(6) = Z(1)
            Z(7) = -Z(1)
            Z(10) = -Z(1)
            Z(11) = Z(1)
            Z(1) = 0.0
            IF ( Isil(1)==Isil(2) ) THEN
               Z(1) = Z(11)
               Z(2) = Z(10)
               Z(5) = Z(7)
               Z(7) = 0.0
               Z(10) = 0.0
               Z(11) = 0.0
            ENDIF
         ELSE
            Z(4) = -Z(1)
            Z(13) = -Z(1)
            Z(16) = Z(1)
            IF ( Isil(1)==Isil(2) ) THEN
               Z(2) = Z(4)
               Z(5) = Z(13)
               Z(6) = Z(16)
               Z(4) = 0.0
               Z(13) = 0.0
               Z(16) = 0.0
            ENDIF
         ENDIF
      ELSE
!
!     IF THE SECOND SIL EQUALS ZERO THE ELEMENT IS GROUNDED
!     ONLY A SINGLE MATRIX TERM IS PRODUCED
!
         Ngrids = 1
         Dict(2) = 1
         Nterms = 1
         Code = 2**Icomp(1)
         Ncol = 1
      ENDIF
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     OUTPUT THE MATRIX HERE
!
      Dict(1) = estid
      Dict(3) = Ncol
      Dict(4) = Code
      Dict(5) = 0
      Ipg = Gspt(Type)
!
!     STRUCTURAL DAMPING FOR  STIIFNESS MATRICES IS INSERTED IN DICT
!
      IF ( Emord==1 .AND. Type<=3 ) Dict(5) = Iest(Ipg)
      IF ( iprec/=1 ) THEN
         I = Nterms
         SPAG_Loop_1_1: DO
            Dz(I) = Z(I)
            I = I - 1
            IF ( I<=0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
      ENDIF
      CALL emgout(Z,Dz,Nterms,1,Dict,Emord,iprec)
   END SUBROUTINE spag_block_2
END SUBROUTINE scaled
