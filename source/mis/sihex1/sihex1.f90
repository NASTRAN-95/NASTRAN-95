!*==sihex1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sihex1(Type,Strspt,Nip)
   USE c_matin
   USE c_matiso
   USE c_matout
   USE c_sdr2x5
   USE c_sdr2x6
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
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
            eid = iest(1)
            ngp = 12*Type - 4
            mid = iest(ngp+2)
            cid = iest(ngp+3)
            Nip = iest(ngp+4)
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
            DO i = 1 , ngp
               gpt(i) = est(5*ngp+7+i)
               bgpid(i) = iest(ngp+4+4*i)
               DO j = 1 , 3
                  bgpdt(j,i) = est(ngp+4+4*i+j)
               ENDDO
            ENDDO
            phiout(1) = est(1)
            DO i = 1 , ngp
               phiout(i+1) = est(i+1)
            ENDDO
!
!     FETCH MATERIAL PROPERTIES
!
!     CHANGE FOR GENERAL ANISOTROPIC MATERIAL
!
!     TEST FOR ANISOTROPIC MATERIAL
!
            anis = .FALSE.
            inflag = 10
!
!     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH ANISOTROPIC
!     MATERIAL IS DEFINED
!
            rect = .TRUE.
            tdep = .TRUE.
!
            DO i = 2 , ngp
               IF ( gpt(i)/=gpt(1) ) GOTO 10
            ENDDO
            tdep = .FALSE.
 10         temp = gpt(1)
            CALL mat(eid)
            IF ( ib(46)==6 ) anis = .TRUE.
            tref = bufm6(44)
            IF ( .NOT.mtdep ) tdep = .FALSE.
!
!     IF ISOTROPIC, TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
!
            IF ( .NOT.(tdep) ) THEN
               IF ( .NOT.(anis) ) THEN
                  IF ( ib(46)/=0 ) THEN
                     e1 = bufm6(1)
                     e2 = bufm6(2)
                     e3 = bufm6(22)
                     alpha = bufm6(38)
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     WRITE (iprnt,99001) uwm , mid , eid
                     e1 = 1.5*e
                     e2 = 0.75*e
                     e3 = 0.375*e
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
                     gmat(ijk) = bufm6(ijk)
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
         CALL ihexss(Type,phiout(ngp+2),dshp,jacob,detj,eid,x,y,z,bgpdt)
         IF ( detj/=0.0 ) THEN
!
!     COMPUTE STRAIN-DISPLACEMENT RELATIONS
!
!     REVERSE CALLING SEQUENCE SINCE MATRICES ARE COLUMN STORED
!
            CALL gmmats(dshp,ngp,3,0,jacob,3,3,0,dshpb)
!
!     IF MATERIAL IS TEMPERATURE DEPENDENT, MUST COMPUTE TEMPERATURE
!     AT THIS STRESS POINT AND FETCH MATERIAL PROPERTIES AGAIN
!
            IF ( tdep ) THEN
               temp = 0.0
               DO j = 1 , ngp
                  temp = temp + gpt(j)*phiout(ngp+1+j)
               ENDDO
               CALL mat(eid)
               IF ( .NOT.(anis) ) THEN
                  IF ( ib(46)/=0 ) THEN
                     e1 = bufm6(1)
                     e2 = bufm6(2)
                     e3 = bufm6(22)
                     alpha = bufm6(38)
                  ELSE
                     WRITE (iprnt,99001) uwm , mid , eid
                     e1 = 1.5*e
                     e2 = 0.75*e
                     e3 = 0.375*e
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
               gmat(ijk) = bufm6(ijk)
            ENDDO
         ELSE
!
!     FALL HERE IF JACOBIAN MATRIX SINGULAR (BAD ELEMENT)
!
            j = ngp*19 + 7
            DO i = 1 , j
               phiout(ngp+1+i) = 0.0
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
         phiout(2*ngp+2) = tref
         IF ( anis ) THEN
!
!     ANISOTROPIC CASE
!
!     ADD CODE WHEN ANISOTROPIC MATERIAL BECOMES AVAILABLE
!
            CALL gmmats(gmat,6,6,0,bufm6(38),6,1,0,phiout(2*ngp+3))
            DO ijk = 1 , 6
               is = 2*ngp + 2 + ijk
               phiout(is) = -phiout(is)
            ENDDO
         ELSE
!
!     ISOTROPIC CASE
!
            DO j = 1 , 3
               phiout(2*ngp+2+j) = -alpha*(e1+2.0*e2)
               phiout(2*ngp+5+j) = 0.0
            ENDDO
         ENDIF
!
!     DISPLACEMENT TO STRESS MATRICES
!
         DO i = 1 , ngp
            is = 2*ngp + 8 + 18*(i-1)
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
               CALL gmmats(gmat(1),6,6,0,store(1),6,3,0,phiout(is+1))
            ELSE
!
!     ISOTROPIC CASE
!
               phiout(is+1) = e1*dshpb(1,i)
               phiout(is+2) = e2*dshpb(2,i)
               phiout(is+3) = e2*dshpb(3,i)
               phiout(is+4) = e2*dshpb(1,i)
               phiout(is+5) = e1*dshpb(2,i)
               phiout(is+6) = e2*dshpb(3,i)
               phiout(is+7) = e2*dshpb(1,i)
               phiout(is+8) = e2*dshpb(2,i)
               phiout(is+9) = e1*dshpb(3,i)
               phiout(is+10) = e3*dshpb(2,i)
               phiout(is+11) = e3*dshpb(1,i)
               phiout(is+14) = e3*dshpb(3,i)
               phiout(is+15) = e3*dshpb(2,i)
               phiout(is+16) = e3*dshpb(3,i)
               phiout(is+18) = e3*dshpb(1,i)
               phiout(is+12) = 0.0
               phiout(is+13) = 0.0
               phiout(is+17) = 0.0
            ENDIF
!
!     POST-MULTIPLY BY GLOBAL TO BASIC TRANSFORMATION MATRIX,
!     IF NECESSARY
!
            IF ( bgpid(i)/=0 ) THEN
               idxyz = bgpid(i)
               DO k = 1 , 3
                  bxyz(k) = bgpdt(k,i)
               ENDDO
!
!     FETCH TRANSFORMATION AND USE IT
!
               CALL transs(idxyz,t)
               CALL gmmats(phiout(is+1),6,3,0,t,3,3,0,sglob)
               DO j = 1 , 18
                  phiout(is+j) = sglob(j)
               ENDDO
            ENDIF
         ENDDO
         iphio(20*ngp+9) = Nip
         nwdnow = 20*ngp + 9
         nwdiso = 649 - nwdnow
         IF ( nwdiso==0 ) RETURN
         DO i = 1 , nwdiso
            isub = nwdnow + i
            phiout(isub) = 0.
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT (A25,' 4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED',' UNDER MATERIAL ID =',I10,' FOR ELEMENT ID =',I10,/32X,     &
             &'NU = 0.333 ASSUMED FOR STRESS COMPUTATION')
END SUBROUTINE sihex1
