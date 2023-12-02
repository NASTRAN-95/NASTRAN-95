!*==rotat.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rotat(Ect2,B1,Gplst,X)
   IMPLICIT NONE
   USE C_BLANK
   USE C_XXPARM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ect2
   INTEGER :: B1
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3,3) :: a
   INTEGER , SAVE :: bar , esym
   REAL , DIMENSION(3) :: cross , normal , shear
   REAL :: eigen , figen , sum , time , twopi
   INTEGER :: elid , i , iel , ielmt , ig1 , ig2 , ig3 , irdect , irec , ishear , isub , it , itype , j , k , m , more , n , ngppe ,&
            & norm , nwds , offset
   INTEGER , DIMENSION(12) :: gpts
   INTEGER , DIMENSION(13) , SAVE :: isym , types
   REAL , DIMENSION(2) :: magtud
   REAL , DIMENSION(146) :: rec1
   REAL , DIMENSION(17) :: rec2
   REAL , DIMENSION(2,2) :: t
   REAL , DIMENSION(2,3) :: v
   EXTERNAL bckrec , close , fread , fwdrec , open , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (rec1(3),itype) , (rec1(4),isub) , (rec1(5),time) , (rec1(6),eigen) , (rec1(10),nwds)
   DATA types/6 , 7 , 8 , 9 , 15 , 16 , 17 , 18 , 19 , 62 , 63 , 64 , 83/
   DATA isym/2HT1 , 2HTB , 2HTP , 2HTM , 2HQP , 2HQM , 2HT2 , 2HQ2 , 2HQ1 , 2HM1 , 2HM2 , 2HQ4 , 2HT3/ , esym/2H  / , bar/2HBR/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         twopi = 8.0*atan(1.0)
         irdect = 0
         sum = 0.0
         CALL open(*60,Newoes,Gplst(B1),1)
         irec = 0
         elid = 0
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*60,*60,Oes1,rec1,146,1,m)
         IF ( isub==Icase ) THEN
            IF ( Flag==0.0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Flag==1.0 .AND. time==Data ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            figen = sqrt(abs(eigen))/twopi
            IF ( Flag==2.0 .AND. abs(figen-Data)>1.0E-5 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( irec==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 60
      CASE (3)
!
!     CHECK ELEMENT TYPE
!
         irec = irec + 2
         DO it = 1 , 13
            IF ( itype==types(it) ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
!
!     SKIP SUBCASE
!
         CALL fwdrec(*60,Oes1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         SPAG_Loop_1_4: DO
!
!     CHECK ELEMENT TYPE
!
            IF ( elid==0 ) THEN
               CALL read(*20,*20,Ect2,esym,1,0,n)
               CALL fread(Ect2,ngppe,1,0)
               irdect = 1
               offset = 0
               IF ( esym==bar ) offset = 6
               IF ( esym==isym(12) .OR. esym==isym(13) ) offset = 1
               IF ( esym==isym(it) ) THEN
!
!     PROCESS SUBCASE
!
                  CALL write(Newoes,rec1,146,1)
                  nwds = nwds - 1
                  SPAG_Loop_2_3: DO
                     CALL read(*60,*40,Oes1,ielmt,1,0,m)
                     CALL fread(Oes1,rec2,nwds,0)
                     SPAG_Loop_3_1: DO
                        CALL fread(Ect2,elid,1,0)
                        IF ( elid==0 ) THEN
!
!     CLOSE RECORD
!
                           CALL fread(Oes1,0,0,1)
                           EXIT SPAG_Loop_3_1
                        ELSE
                           CALL fread(Ect2,0,-1,0)
                           CALL fread(Ect2,gpts,ngppe,0)
                           IF ( offset/=0 ) CALL fread(Ect2,0,-offset,0)
                           DO WHILE ( elid/=ielmt/10 )
                              IF ( elid<=ielmt/10 ) CYCLE SPAG_Loop_3_1
!
!     SKIP ELEMENT
!
                              CALL read(*60,*40,Oes1,ielmt,1,0,m)
                              CALL fread(Oes1,rec2,nwds,0)
                           ENDDO
                           ig1 = gpts(1)
                           ig2 = gpts(2)
                           ig1 = iabs(Gplst(ig1))
                           ig2 = iabs(Gplst(ig2))
                           ig3 = gpts(3)
                           ig3 = iabs(Gplst(ig3))
                           DO i = 1 , 3
                              v(1,i) = X(i,ig1) - X(i,ig2)
                              v(2,i) = X(i,ig1) - X(i,ig3)
                           ENDDO
                           magtud(1) = sqrt(v(1,1)**2+v(1,2)**2+v(1,3)**2)
                           magtud(2) = sqrt(v(2,1)**2+v(2,2)**2+v(2,3)**2)
                           DO i = 1 , 3
                              v(1,i) = v(1,i)/magtud(1)
                              v(2,i) = v(2,i)/magtud(2)
                              a(1,i) = v(1,i)
                           ENDDO
                           a(2,1) = a(1,2)
                           a(3,1) = a(1,3)
                           a(3,3) = v(1,1)*v(2,2) - v(2,1)*v(1,2)
                           cross(1) = v(1,2)*v(2,3) - v(2,2)*v(1,3)
                           cross(2) = v(2,1)*v(1,3) - v(1,1)*v(2,3)
                           cross(3) = a(3,3)
                           a(2,2) = cross(1)*v(1,3) - v(1,1)*cross(3)
                           a(2,3) = v(1,1)*cross(2) - cross(1)*v(1,2)
                           a(3,2) = a(2,3)
                           iel = 0
                           SPAG_Loop_4_2: DO more = 1 , 2
                              IF ( itype==9 .OR. itype==16 ) THEN
                                 norm = iel + 1
                                 ishear = iel + 3
                              ELSE
                                 norm = iel + 2
                                 ishear = iel + 4
                              ENDIF
                              t(1,1) = rec2(norm)
                              t(2,2) = rec2(norm+1)
                              t(1,2) = rec2(ishear)
                              t(2,1) = t(1,2)
                              DO i = 1 , 3
                                 sum = 0.0
                                 DO j = 1 , 2
                                    DO k = 1 , 2
                                       sum = sum + a(i,j)*a(i,k)*t(j,k)
                                    ENDDO
                                 ENDDO
                                 normal(i) = sum
                              ENDDO
                              shear(1) = a(2,1)*a(1,1)*t(1,1) + a(2,1)*a(1,2)*t(1,2) + a(2,2)*a(1,2)*t(2,1) + a(2,2)*a(1,2)*t(2,2)
                              shear(2) = a(3,1)*a(1,1)*t(1,1) + a(3,1)*a(1,2)*t(1,2) + a(3,2)*a(1,2)*t(2,1) + a(3,2)*a(1,2)*t(2,2)
                              shear(3) = a(3,1)*a(2,1)*t(1,1) + a(3,1)*a(2,2)*t(1,2) + a(3,2)*a(2,1)*t(2,1) + a(3,2)*a(2,2)*t(2,2)
                              DO i = 1 , 3
                                 ishear = ishear + 1
                                 rec2(norm) = normal(i)
                                 rec2(ishear) = shear(i)
                                 norm = norm + 1
                              ENDDO
                              iel = iel + 8
                              IF ( itype==9 .OR. itype==16 ) EXIT SPAG_Loop_4_2
                           ENDDO SPAG_Loop_4_2
                           CALL write(Newoes,ielmt,1,0)
                           CALL write(Newoes,rec2,nwds,0)
                           CYCLE SPAG_Loop_2_3
                        ENDIF
                     ENDDO SPAG_Loop_3_1
                     GOTO 40
                  ENDDO SPAG_Loop_2_3
               ENDIF
            ENDIF
            DO
               CALL fread(Ect2,elid,1,0)
               IF ( elid==0 ) CYCLE SPAG_Loop_1_4
               j = 1 + ngppe + offset
               CALL fread(Ect2,0,-j,0)
            ENDDO
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
 20      CALL bckrec(Ect2)
         irdect = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      CALL write(Newoes,0,0,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      IF ( irdect>0 ) CALL bckrec(Ect2)
         CALL bckrec(Oes1)
         CALL close(Newoes,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rotat
