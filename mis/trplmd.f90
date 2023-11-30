
SUBROUTINE trplmd(Gmat,Dmat,Bmat,Bmat1,Bmat2,Mattyp,Jcor,Wtk)
   IMPLICIT NONE
   DOUBLE PRECISION Akgg(1)
   LOGICAL Bendng , Mbcoup , Membrn , Norpth , Shrflx
   INTEGER Ibot , Iptx1 , Iptx2 , Ipty1 , Ipty2 , Ndof
   COMMON /terms / Membrn , Bendng , Shrflx , Mbcoup , Norpth
   COMMON /trplm / Ndof , Ibot , Iptx1 , Iptx2 , Ipty1 , Ipty2
   COMMON /zzzzzz/ Akgg
   INTEGER Jcor , Mattyp
   DOUBLE PRECISION Wtk
   DOUBLE PRECISION Bmat(240) , Bmat1(1) , Bmat2(1) , Dmat(7,7) , Gmat(10,10)
   DOUBLE PRECISION dbm(240) , dmat1(3,3) , dmat2(4,4)
   INTEGER i , j , nd1 , nd2 , nd3 , nd4 , nd5 , nd6 , nd7 , nd8 , nd9 , nda
!
!     ROUTINE TO PERFORM THE TRIPLE MULTIPLY AT EACH INTEGRATION
!     POINT FOR THE QUAD4 ELEMENT.
!     DIFFERENT PATHS ARE TAKEN BASED ON THE FOLLOWING CRITERIA -
!      1- ELEMENT BEING A MEMBRANE ONLY, OR BENDING ONLY, OR BOTH
!         MEMBRANE AND BENDING ELEMENT.
!      2- THE MATERIAL PROPERTIES BEING ISOTROPIC OR NOT.
!      3- THE MACHINE THIS CODE IS RUNNING ON. (TENTATIVE)
!
!
!
!
!*****
!     INITIALIZE
!*****
   nd1 = Ndof
   nd2 = nd1*2
   nd3 = nd1*3
   nd4 = nd1*4
   nd5 = nd1*5
   nd6 = nd1*6
   nd7 = nd1*7
   nd8 = nd1*8
   nd9 = nd1*9
   nda = nd1*10
   IF ( Norpth ) THEN
!*****
!    ALL MIDS ARE THE SAME AND THERE IS NO COUPLING.
!    IF THE MATERIAL IS ISOTROPIC, PERFORM THE 1ST MUTIPLY EXPLICITLY.
!    IF NOT, USE GMMATD. IN EITHER CASE, THE 2ND MULTIPLY USES GMMATD.
!*****
      DO i = 1 , nd1
         Bmat(i+nd2) = Bmat2(i+Ibot)
         Bmat(i+nd3) = Bmat1(i+Ipty1)
         Bmat(i+nd4) = Bmat1(i+Ipty2)
         Bmat(i+nd5) = Bmat1(i+Iptx1+nd1)
         Bmat(i+nd6) = Bmat1(i+Iptx2+nd1)
      ENDDO
!
      IF ( Mattyp/=1 ) THEN
!
         CALL gmmatd(Dmat,7,7,0,Bmat,7,nd1,0,dbm)
      ELSE
         DO i = 1 , nd1
            dbm(i) = Dmat(1,1)*Bmat(i) + Dmat(1,2)*Bmat(i+nd1)
            dbm(i+nd1) = Dmat(2,1)*Bmat(i) + Dmat(2,2)*Bmat(i+nd1)
            dbm(i+nd2) = Dmat(3,3)*Bmat(i+nd2)
            dbm(i+nd3) = Dmat(4,4)*Bmat(i+nd3) + Dmat(4,5)*Bmat(i+nd4)
            dbm(i+nd4) = Dmat(5,4)*Bmat(i+nd3) + Dmat(5,5)*Bmat(i+nd4)
            dbm(i+nd5) = Dmat(6,6)*Bmat(i+nd5) + Dmat(6,7)*Bmat(i+nd6)
            dbm(i+nd6) = Dmat(7,6)*Bmat(i+nd5) + Dmat(7,7)*Bmat(i+nd6)
         ENDDO
      ENDIF
!
      DO i = 1 , nd7
         Bmat(i) = Bmat(i)*Wtk
      ENDDO
      CALL gmmatd(Bmat,7,nd1,-1,dbm,7,nd1,0,Akgg(Jcor))
      RETURN
!*****
!     MIDS ARE NOT THE SAME. CHECK FOR MEMBRANE ONLY AND BENDING ONLY
!     CASES AND BRANCH APPROPRIATELY. IF BOTH ARE THERE, CONTINUE.
!*****
   ELSEIF ( .NOT.Bendng ) THEN
!*****
!     MEMBRANE ONLY ELEMENT. ONLY THE FIRST 3X3 OF GMAT AND THE FIRST
!     3 ROWS OF BMAT ARE MULTIPLIED.
!*****
      DO i = 1 , nd1
         Bmat(i+nd2) = Bmat2(i+Ibot)
      ENDDO
!
      IF ( Mattyp/=1 ) THEN
!
         DO i = 1 , 3
            DO j = 1 , 3
               dmat1(i,j) = Gmat(i,j)
            ENDDO
         ENDDO
         CALL gmmatd(dmat1,3,3,0,Bmat(1),3,nd1,0,dbm(1))
      ELSE
         DO i = 1 , nd1
            dbm(i) = Gmat(1,1)*Bmat(i) + Gmat(1,2)*Bmat(i+nd1)
            dbm(i+nd1) = Gmat(2,1)*Bmat(i) + Gmat(2,2)*Bmat(i+nd1)
            dbm(i+nd2) = Gmat(3,3)*Bmat(i+nd2)
         ENDDO
      ENDIF
   ELSEIF ( .NOT.Membrn ) THEN
!*****
!     BENDING ONLY ELEMENT. THE FIRST 3 ROWS AND COLUMNS OF GMAT AND
!     THE FIRST 3 ROWS OF BMAT WILL BE EXCLUDED FROM MULTIPLICATIONS.
!*****
      DO i = 1 , nd1
         Bmat(i+nd6) = Bmat1(i+Ipty1)
         Bmat(i+nd7) = Bmat1(i+Ipty2)
         Bmat(i+nd8) = Bmat1(i+Iptx1+nd1)
         Bmat(i+nd9) = Bmat1(i+Iptx2+nd1)
      ENDDO
!
      DO i = 1 , 3
         DO j = 1 , 3
            dmat1(i,j) = Gmat(i+3,j+3)
         ENDDO
      ENDDO
      DO i = 1 , 4
         DO j = 1 , 4
            dmat2(i,j) = Gmat(i+6,j+6)
         ENDDO
      ENDDO
!
      CALL gmmatd(dmat1,3,3,0,Bmat(nd3+1),3,nd1,0,dbm(1))
      CALL gmmatd(dmat2,4,4,0,Bmat(nd6+1),4,nd1,0,dbm(nd3+1))
!
      DO i = nd3 + 1 , nda
         Bmat(i) = Bmat(i)*Wtk
      ENDDO
      CALL gmmatd(Bmat(nd3+1),7,nd1,-1,dbm,7,nd1,0,Akgg(Jcor))
      GOTO 99999
   ELSE
      DO i = 1 , nd1
         Bmat(i+nd2) = Bmat2(i+Ibot)
         Bmat(i+nd5) = Bmat2(i+Ibot+nd1)
         Bmat(i+nd6) = Bmat1(i+Ipty1)
         Bmat(i+nd7) = Bmat1(i+Ipty2)
         Bmat(i+nd8) = Bmat1(i+Iptx1+nd1)
         Bmat(i+nd9) = Bmat1(i+Iptx2+nd1)
      ENDDO
!
      CALL gmmatd(Gmat,10,10,0,Bmat,10,nd1,0,dbm)
!
      DO i = 1 , nda
         Bmat(i) = Bmat(i)*Wtk
      ENDDO
      CALL gmmatd(Bmat,10,nd1,-1,dbm,10,nd1,0,Akgg(Jcor))
      RETURN
   ENDIF
!
   DO i = 1 , nd3
      Bmat(i) = Bmat(i)*Wtk
   ENDDO
   CALL gmmatd(Bmat,3,nd1,-1,dbm,3,nd1,0,Akgg(Jcor))
   RETURN
!
99999 RETURN
END SUBROUTINE trplmd