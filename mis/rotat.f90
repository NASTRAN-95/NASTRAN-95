
SUBROUTINE rotat(Ect2,B1,Gplst,X)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Data , Flag , Oes1 , Scr1 , Scr2 , Skip(23) , Skparm , Skppar(211)
   INTEGER Icase , Newoes
   COMMON /blank / Skip , Oes1 , Scr1 , Scr2 , Newoes
   COMMON /xxparm/ Skppar , Icase , Flag , Data , Skparm
!
! Dummy argument declarations
!
   INTEGER B1 , Ect2
   INTEGER Gplst(1)
   REAL X(3,1)
!
! Local variable declarations
!
   REAL a(3,3) , cross(3) , eigen , figen , magtud(2) , normal(3) , rec1(146) , rec2(17) , shear(3) , sum , t(2,2) , time , twopi , &
      & v(2,3)
   INTEGER bar , elid , esym , gpts(12) , i , iel , ielmt , ig1 , ig2 , ig3 , irdect , irec , ishear , isub , isym(13) , it ,       &
         & itype , j , k , m , more , n , ngppe , norm , nwds , offset , types(13)
!
! End of declarations
!
!
   EQUIVALENCE (rec1(3),itype) , (rec1(4),isub) , (rec1(5),time) , (rec1(6),eigen) , (rec1(10),nwds)
   DATA types/6 , 7 , 8 , 9 , 15 , 16 , 17 , 18 , 19 , 62 , 63 , 64 , 83/
   DATA isym/2HT1 , 2HTB , 2HTP , 2HTM , 2HQP , 2HQM , 2HT2 , 2HQ2 , 2HQ1 , 2HM1 , 2HM2 , 2HQ4 , 2HT3/ , esym/2H  / , bar/2HBR/
!
   twopi = 8.0*atan(1.0)
   irdect = 0
   sum = 0.0
   CALL open(*900,Newoes,Gplst(B1),1)
   irec = 0
   elid = 0
 100  CALL read(*900,*900,Oes1,rec1,146,1,m)
   IF ( isub==Icase ) THEN
      IF ( Flag==0.0 ) GOTO 200
      IF ( Flag==1.0 .AND. time==Data ) GOTO 200
      figen = sqrt(abs(eigen))/twopi
      IF ( Flag==2.0 .AND. abs(figen-Data)>1.0E-5 ) GOTO 200
   ENDIF
   IF ( irec/=0 ) GOTO 900
   GOTO 300
!
!     CHECK ELEMENT TYPE
!
 200  irec = irec + 2
   DO it = 1 , 13
      IF ( itype==types(it) ) GOTO 400
   ENDDO
!
!     SKIP SUBCASE
!
 300  CALL fwdrec(*900,Oes1)
   GOTO 100
!
!     CHECK ELEMENT TYPE
!
 400  IF ( elid==0 ) THEN
      CALL read(*500,*500,Ect2,esym,1,0,n)
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
         GOTO 600
      ENDIF
   ENDIF
   DO
      CALL fread(Ect2,elid,1,0)
      IF ( elid==0 ) GOTO 400
      j = 1 + ngppe + offset
      CALL fread(Ect2,0,-j,0)
   ENDDO
 500  CALL bckrec(Ect2)
   irdect = 0
   GOTO 300
 600  CALL read(*900,*800,Oes1,ielmt,1,0,m)
   CALL fread(Oes1,rec2,nwds,0)
   DO
      CALL fread(Ect2,elid,1,0)
      IF ( elid==0 ) THEN
!
!     CLOSE RECORD
!
         CALL fread(Oes1,0,0,1)
         EXIT
      ELSE
         CALL fread(Ect2,0,-1,0)
         CALL fread(Ect2,gpts,ngppe,0)
         IF ( offset/=0 ) CALL fread(Ect2,0,-offset,0)
         DO WHILE ( elid/=ielmt/10 )
            IF ( elid<=ielmt/10 ) GOTO 700
!
!     SKIP ELEMENT
!
            CALL read(*900,*800,Oes1,ielmt,1,0,m)
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
         DO more = 1 , 2
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
            IF ( itype==9 .OR. itype==16 ) EXIT
         ENDDO
         CALL write(Newoes,ielmt,1,0)
         CALL write(Newoes,rec2,nwds,0)
         GOTO 600
      ENDIF
 700  ENDDO
 800  CALL write(Newoes,0,0,1)
   GOTO 100
 900  IF ( irdect>0 ) CALL bckrec(Ect2)
   CALL bckrec(Oes1)
   CALL close(Newoes,1)
END SUBROUTINE rotat
