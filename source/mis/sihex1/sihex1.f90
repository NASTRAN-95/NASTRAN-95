!*==sihex1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sihex1(Type,Strspt,Nip)
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATISO
   USE C_MATOUT
   USE C_SDR2X5
   USE C_SDR2X6
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
   INTEGER :: Strspt
   INTEGER :: Nip
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: anis , rect , tdep
   REAL , DIMENSION(3) :: bxyz
   REAL , DIMENSION(3,32) :: dshpb
   REAL , DIMENSION(8) , SAVE :: gauss
   REAL , DIMENSION(36) :: gmat
   INTEGER :: i , idxyz , ijk , is , isub , j , k , l , nipm1 , nwdiso , nwdnow
   INTEGER , DIMENSION(46) :: ib
   INTEGER , DIMENSION(1) :: iest , iphio
   INTEGER , DIMENSION(3,64) :: itab
   REAL , DIMENSION(4) :: s
   REAL , DIMENSION(18) :: store
   REAL :: x , y , z
   EXTERNAL gmmats , ihexss , mat , transs
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PHASE 1 STRESS ROUTINE FOR IHEX1, IHEX2, AND IHEX3 ELEMENTS
!
!     TYPE = 1    IHEX1
!     TYPE = 2    IHEX2
!     TYPE = 3    IHEX3
!
!     THE EST ENTRIES ARE
!
!     NAME  ---------INDEX---------   DESCRIPTION
!            IHEX1   IHEX2   IHEX3
!
!     EID        1       1       1    ELEMENT ID NO.
!     SIL      2-9    2-21    2-33    SCALAR INDEX LIST
!     MID       10      22      34    MATERIAL ID NO.
!     CID       11      23      35    MATERIAL COORD. SYSTEM ID NO.
!     NIP       12      24      36    NO. INTEGRATION POINTS PER EDGE
!     MAXAR     13      25      37    MAX ASPECT RATIO
!     ALFA      14      26      38    MAX ANGLE FOR NORMALS
!     BETA      15      27      39    MAX ANGLE FOR MIDSIDE POINTS
!     BGPDT  16-47  28-107  40-167    BASIC GRID POINT DATA
!     GPT    48-55 108-127 168-199    GRID POINT TEMPERATURES
!
!     PHIOUT (ESTA) CONTAINS THE FOLLOWING WHERE NGP IS THE NUMBER
!     OF GRID POINTS
!
!     ELEMENT ID
!     NGP SIL NUMBERS
!     NGP VALUES OF THE SHAPE FUNCTIONS AT THIS STRESS POINT
!     REFERENCE TEMPERATURE
!     6 THERMAL STRESS COEFFICIENTS
!     NGP, 6 BY 3 MATRICES, RELATING STRESS TO DISPLACEMENTS AT THIS
!          STRESS POINT (STORED ROW-WISE)
!
   !>>>>EQUIVALENCE (Est(1),Iest(1),Dshpb(1,1)) , (Phiout(1),Iphio(1)) , (Est(97),Idxyz) , (Est(98),Bxyz(1)) , (Ib(1),Bufm6(1))
   DATA gauss/.57735027 , .55555556 , .77459667 , .88888889 , .34785485 , .86113631 , .65214515 , .33998104/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Strspt==0 ) Strspt = Strspt + 1
         IF ( Strspt<=1 ) THEN
!
!     MOVE EST DATA INTO /SDR2X6/, /MATIN/, AND PHIOUT
!
            Eid = iest(1)
            Ngp = 12*Type - 4
            Mid = iest(Ngp+2)
            Cid = iest(Ngp+3)
            Nip = iest(Ngp+4)
            IF ( Nip==0 ) Nip = Type/2 + 2
!
!     FOR STRESS COMPUTATION, SET NUMBER OF STRESS POINTS TO 2
!     NUMBER OF GAUSS POINTS) TO CUT DOWN ON AMOUNT OF INFO ON ESTA
!
            Nip = 2
            l = 0
            DO i = 1 , Nip
               DO j = 1 , Nip
                  DO k = 1 , Nip
                     l = l + 1
                     itab(1,l) = i
                     itab(2,l) = j
                     itab(3,l) = k
                  ENDDO
               ENDDO
            ENDDO
            DO i = 1 , Ngp
               Gpt(i) = Est(5*Ngp+7+i)
               Bgpid(i) = iest(Ngp+4+4*i)
               DO j = 1 , 3
                  Bgpdt(j,i) = Est(Ngp+4+4*i+j)
               ENDDO
            ENDDO
            Phiout(1) = Est(1)
            DO i = 1 , Ngp
               Phiout(i+1) = Est(i+1)
            ENDDO
!
!     FETCH MATERIAL PROPERTIES
!
!     CHANGE FOR GENERAL ANISOTROPIC MATERIAL
!
!     TEST FOR ANISOTROPIC MATERIAL
!
            anis = .FALSE.
            Inflag = 10
!
!     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH ANISOTROPIC
!     MATERIAL IS DEFINED
!
            rect = .TRUE.
            tdep = .TRUE.
!
            DO i = 2 , Ngp
               IF ( Gpt(i)/=Gpt(1) ) GOTO 10
            ENDDO
            tdep = .FALSE.
 10         Temp = Gpt(1)
            CALL mat(Eid)
            IF ( ib(46)==6 ) anis = .TRUE.
            Tref = Bufm6(44)
            IF ( .NOT.Mtdep ) tdep = .FALSE.
!
!     IF ISOTROPIC, TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
!
            IF ( .NOT.(tdep) ) THEN
               IF ( .NOT.(anis) ) THEN
                  IF ( ib(46)/=0 ) THEN
                     E1 = Bufm6(1)
                     E2 = Bufm6(2)
                     E3 = Bufm6(22)
                     Alpha = Bufm6(38)
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     WRITE (Iprnt,99001) Uwm , Mid , Eid
                     E1 = 1.5*E
                     E2 = 0.75*E
                     E3 = 0.375*E
                  ENDIF
               ENDIF
!
!     IF MATERIAL IS ANISOTROPIC, DEFINED IN A RECTANGULAR
!     COORDINATE SYSTEM, AND NOT TEMPERATURE DEPENDENT, TRANSFORM
!     IT TO THE BASIC SYSTEM.
!
               IF ( rect ) THEN
!
!     ADD CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL
!     TO BASIC COORDINATE SYSTEM HERE.
!
                  DO ijk = 1 , 36
                     gmat(ijk) = Bufm6(ijk)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     INITIALIZATION TO FIND GAUSS POINT COORDINATES
!
         nipm1 = Nip - 1
         IF ( nipm1==2 ) THEN
            s(1) = gauss(3)
            s(2) = 0.
            s(3) = -gauss(3)
         ELSEIF ( nipm1==3 ) THEN
            s(1) = gauss(6)
            s(2) = gauss(8)
            s(3) = -gauss(8)
            s(4) = -gauss(6)
         ELSE
            s(1) = gauss(1)
            s(2) = -gauss(1)
         ENDIF
         IF ( Strspt==Nip**3+1 ) THEN
            x = 0.
            y = 0.
            z = 0.
         ELSE
            l = itab(1,Strspt)
            x = s(l)
            l = itab(2,Strspt)
            y = s(l)
            l = itab(3,Strspt)
            z = s(l)
         ENDIF
!
!     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
!
         CALL ihexss(Type,Phiout(Ngp+2),Dshp,Jacob,Detj,Eid,x,y,z,Bgpdt)
         IF ( Detj/=0.0 ) THEN
!
!     COMPUTE STRAIN-DISPLACEMENT RELATIONS
!
!     REVERSE CALLING SEQUENCE SINCE MATRICES ARE COLUMN STORED
!
            CALL gmmats(Dshp,Ngp,3,0,Jacob,3,3,0,dshpb)
!
!     IF MATERIAL IS TEMPERATURE DEPENDENT, MUST COMPUTE TEMPERATURE
!     AT THIS STRESS POINT AND FETCH MATERIAL PROPERTIES AGAIN
!
            IF ( tdep ) THEN
               Temp = 0.0
               DO j = 1 , Ngp
                  Temp = Temp + Gpt(j)*Phiout(Ngp+1+j)
               ENDDO
               CALL mat(Eid)
               IF ( .NOT.(anis) ) THEN
                  IF ( ib(46)/=0 ) THEN
                     E1 = Bufm6(1)
                     E2 = Bufm6(2)
                     E3 = Bufm6(22)
                     Alpha = Bufm6(38)
                  ELSE
                     WRITE (Iprnt,99001) Uwm , Mid , Eid
                     E1 = 1.5*E
                     E2 = 0.75*E
                     E3 = 0.375*E
                  ENDIF
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGJLAR
!     COORDINATE SYSTEM, TRANSFORM IT TO BASIC COORDINATE SYSTEM AT
!     THIS STRESS POINT.
!
!
!     IN THIS VERSION, ANISOTROPIC PROPERTIES MUST BE RECTANGULAR
!     JUST STORE G MATRIX
!     ===========================================================
!
!     THIS CODE MUST BE COMPLETED WHEN GENERAL ANISOTROPIC MATERIAL IS
!     ADDED.
!
            ELSEIF ( .NOT.anis ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO ijk = 1 , 36
               gmat(ijk) = Bufm6(ijk)
            ENDDO
         ELSE
!
!     FALL HERE IF JACOBIAN MATRIX SINGULAR (BAD ELEMENT)
!
            j = Ngp*19 + 7
            DO i = 1 , j
               Phiout(Ngp+1+i) = 0.0
            ENDDO
            RETURN
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
!     ANISOTROPIC MATERIAL.
!
!     MATERIAL HAS BEEN EVALUATED AT THIS STRESS POINT WHEN GET TO HERE
!
!     TEMPERATURE TO STRESS VECTOR
!
         Phiout(2*Ngp+2) = Tref
         IF ( anis ) THEN
!
!     ANISOTROPIC CASE
!
!     ADD CODE WHEN ANISOTROPIC MATERIAL BECOMES AVAILABLE
!
            CALL gmmats(gmat,6,6,0,Bufm6(38),6,1,0,Phiout(2*Ngp+3))
            DO ijk = 1 , 6
               is = 2*Ngp + 2 + ijk
               Phiout(is) = -Phiout(is)
            ENDDO
         ELSE
!
!     ISOTROPIC CASE
!
            DO j = 1 , 3
               Phiout(2*Ngp+2+j) = -Alpha*(E1+2.0*E2)
               Phiout(2*Ngp+5+j) = 0.0
            ENDDO
         ENDIF
!
!     DISPLACEMENT TO STRESS MATRICES
!
         DO i = 1 , Ngp
            is = 2*Ngp + 8 + 18*(i-1)
!
!     ROW-STORED
!
            IF ( anis ) THEN
!
!     ANISOTROPIC CASE
!
!     ADD CODE WHEN GENERAL ANISOTROPIC MATERIAL BECOMES AVAILABLE
!
               DO ijk = 1 , 18
                  store(ijk) = 0.
               ENDDO
               store(1) = dshpb(1,i)
               store(5) = dshpb(2,i)
               store(9) = dshpb(3,i)
               store(10) = dshpb(2,i)
               store(11) = dshpb(1,i)
               store(14) = dshpb(3,i)
               store(15) = dshpb(2,i)
               store(16) = dshpb(3,i)
               store(18) = dshpb(1,i)
!
               CALL gmmats(gmat(1),6,6,0,store(1),6,3,0,Phiout(is+1))
            ELSE
!
!     ISOTROPIC CASE
!
               Phiout(is+1) = E1*dshpb(1,i)
               Phiout(is+2) = E2*dshpb(2,i)
               Phiout(is+3) = E2*dshpb(3,i)
               Phiout(is+4) = E2*dshpb(1,i)
               Phiout(is+5) = E1*dshpb(2,i)
               Phiout(is+6) = E2*dshpb(3,i)
               Phiout(is+7) = E2*dshpb(1,i)
               Phiout(is+8) = E2*dshpb(2,i)
               Phiout(is+9) = E1*dshpb(3,i)
               Phiout(is+10) = E3*dshpb(2,i)
               Phiout(is+11) = E3*dshpb(1,i)
               Phiout(is+14) = E3*dshpb(3,i)
               Phiout(is+15) = E3*dshpb(2,i)
               Phiout(is+16) = E3*dshpb(3,i)
               Phiout(is+18) = E3*dshpb(1,i)
               Phiout(is+12) = 0.0
               Phiout(is+13) = 0.0
               Phiout(is+17) = 0.0
            ENDIF
!
!     POST-MULTIPLY BY GLOBAL TO BASIC TRANSFORMATION MATRIX,
!     IF NECESSARY
!
            IF ( Bgpid(i)/=0 ) THEN
               idxyz = Bgpid(i)
               DO k = 1 , 3
                  bxyz(k) = Bgpdt(k,i)
               ENDDO
!
!     FETCH TRANSFORMATION AND USE IT
!
               CALL transs(idxyz,T)
               CALL gmmats(Phiout(is+1),6,3,0,T,3,3,0,Sglob)
               DO j = 1 , 18
                  Phiout(is+j) = Sglob(j)
               ENDDO
            ENDIF
         ENDDO
         iphio(20*Ngp+9) = Nip
         nwdnow = 20*Ngp + 9
         nwdiso = 649 - nwdnow
         IF ( nwdiso==0 ) RETURN
         DO i = 1 , nwdiso
            isub = nwdnow + i
            Phiout(isub) = 0.
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT (A25,' 4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED',' UNDER MATERIAL ID =',I10,' FOR ELEMENT ID =',I10,/32X,     &
             &'NU = 0.333 ASSUMED FOR STRESS COMPUTATION')
END SUBROUTINE sihex1
