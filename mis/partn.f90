
SUBROUTINE partn(Irp,Icp,Core)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A11(4) , Rule
   INTEGER Ia(2) , Ia11(7) , Ia12(7) , Ia21(7) , Ia22(7) , Ias(7,4) , Ieol , Ieor , Iforma , Ii , Itypa , Lcare , Namea , Ncola ,   &
         & Nrowa , Sysbuf , Two1(32)
   COMMON /parmeg/ Namea , Ncola , Nrowa , Iforma , Itypa , Ia , Ia11 , Ia21 , Ia12 , Ia22 , Lcare , Rule
   COMMON /system/ Sysbuf
   COMMON /two   / Two1
   COMMON /zntpkx/ A11 , Ii , Ieol , Ieor
!
! Dummy argument declarations
!
   INTEGER Core(1) , Icp(1) , Irp(1)
!
! Local variable declarations
!
   INTEGER andf , rshift
   REAL block1(40) , head(2) , ocpct , orpct , zcpct , zrpct
   INTEGER i , ibuf , ibufcp , ibufrp , iln , inorp , iopen , iotp , ipm1 , ipm2 , ipos , itemp , j , k , km , l , l1 , lcore ,     &
         & loop , m , m1 , name(2)
   EXTERNAL andf , rshift
!
! End of declarations
!
!
   EQUIVALENCE (Ias(1,1),Ia11(1))
   DATA iln/20/ , name/4HPART , 4HN   /
!
!     ZERO 6 AND 7 OF OUTPUT BLOCKS
!
   iotp = Itypa
   iopen = 0
   DO i = 1 , 4
      DO j = 6 , 7
         Ias(j,i) = 0
      ENDDO
      IF ( Ias(1,i)/=0 ) THEN
         IF ( Ias(5,i)/=Itypa ) iotp = 4
         iopen = iopen + 1
         DO j = 2 , 5
            IF ( Ias(j,i)<=0 ) GOTO 100
         ENDDO
         Ias(2,i) = 0
      ENDIF
   ENDDO
   lcore = Lcare
   ibuf = lcore - Sysbuf + 1
   ibufcp = ibuf - Nrowa
   ibufrp = ibufcp - (Ncola+31)/32
   IF ( ibufrp<=0 ) THEN
!
      ipm1 = -8
      CALL mesage(ipm1,ipm2,name)
   ELSE
      lcore = ibufrp - 1
      inorp = 0
      CALL ruler(Rule,Icp,zcpct,ocpct,Core(ibufcp),Nrowa,Core(ibuf),1)
      IF ( Irp(1)==Icp(1) .AND. Irp(1)/=0 .AND. Nrowa==Ncola ) THEN
         inorp = 1
         lcore = ibufcp - 1
      ELSE
         CALL ruler(Rule,Irp,zrpct,orpct,Core(ibufrp),Ncola,Core(ibuf),0)
      ENDIF
!
!     OPEN OUTPUT MATRICES
!
      IF ( iopen*Sysbuf>lcore ) THEN
         ipm1 = -8
         CALL mesage(ipm1,ipm2,name)
      ELSE
         DO i = 1 , 4
            IF ( Ias(1,i)/=0 ) THEN
               lcore = lcore - Sysbuf
               CALL open(*10,Ias(1,i),Core(lcore+1),1)
               CALL fname(Ias(1,i),head)
               CALL write(Ias(1,i),head,2,1)
            ENDIF
            CYCLE
 10         Ias(1,i) = 0
         ENDDO
!
!     OPEN INPUT MATRIX
!
         CALL gopen(Namea,Core(ibuf),0)
!
!     LOOP FOR EACH COLUMN
!
         km = 0
         DO loop = 1 , Ncola
            IF ( inorp/=0 ) THEN
               l = ibufcp + loop - 1
               IF ( Core(l)<0 ) THEN
                  l1 = 0
               ELSE
                  l1 = 2
               ENDIF
            ELSE
!
!     COLUMN PARTITION A SEQ. OF ZEROS AND ONES
!
               km = km + 1
               IF ( km>32 ) km = 1
               l = ibufrp + (loop-1)/32
               itemp = andf(Core(l),Two1(km))
               IF ( km==1 ) itemp = rshift(andf(Core(l),Two1(km)),1)
               IF ( itemp/=0 ) THEN
                  l1 = 2
               ELSE
                  l1 = 0
               ENDIF
            ENDIF
!
!     BEGIN BLDPK ON TWO SUBS
!
            j = 0
            DO l = 1 , 2
               k = l1 + l
               m = iln*(l-1) + 1
               IF ( Ias(1,k)/=0 ) THEN
                  CALL bldpk(iotp,Ias(5,k),Ias(1,k),block1(m),1)
                  j = j + 1
               ENDIF
            ENDDO
            IF ( j/=0 ) THEN
!
!     SEARCH COLUMN FOR NON-ZERO ELEMENTS
!
               CALL intpk(*20,Namea,0,iotp,0)
!
!     LOOP FOR ROWS WITHIN COLUMN
!
               DO WHILE ( Ieol==0 )
                  CALL zntpki
!
!     COMPUTE ROW POSITION AND OUTPUT MATRIX
!
                  l = ibufcp + Ii - 1
                  ipos = iabs(Core(l))
                  IF ( Core(l)<0 ) THEN
                     m1 = l1 + 1
                     m = 1
                  ELSE
                     m1 = l1 + 2
                     m = iln + 1
                  ENDIF
                  IF ( Ias(1,m1)/=0 ) CALL bldpki(A11(1),ipos,Ias(1,m1),block1(m))
               ENDDO
            ELSE
               CALL skprec(Namea,1)
               CYCLE
            ENDIF
 20         DO l = 1 , 2
               k = l + l1
               m = iln*(l-1) + 1
               IF ( Ias(1,k)/=0 ) CALL bldpkn(Ias(1,k),block1(m),Ias(1,k))
            ENDDO
         ENDDO
!
!     ALL DONE - CLOSE OPEN MATRICES
!
         CALL close(Namea,1)
         DO i = 1 , 4
            IF ( Ias(1,i)/=0 ) CALL close(Ias(1,i),1)
         ENDDO
         RETURN
      ENDIF
   ENDIF
 100  DO
      ipm1 = -7
      CALL mesage(ipm1,ipm2,name)
   ENDDO
END SUBROUTINE partn
