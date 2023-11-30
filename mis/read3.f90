
SUBROUTINE read3(Novect,Ncol,Sr1fil,Sr2fil,Filc,Kdblm)
   IMPLICIT NONE
   REAL Eofnrw , Rd , Rew , Shftpt , Systm(52) , Wrt , Wrtrew , Z(1)
   INTEGER Incr , Incru , Ipak , Iprec , Itypa , Itypb , Itypu , Iunp , Iz(1) , Jpak , Junp , Norew , Nout , Option , Optn2 , Rdp , &
         & Rdrew , Rsp , Sturm , Sysbuf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp
   COMMON /packx / Itypa , Itypb , Ipak , Jpak , Incr
   COMMON /reigkr/ Option , Optn2
   COMMON /sturmx/ Sturm , Shftpt
   COMMON /system/ Sysbuf , Nout , Systm , Iprec
   COMMON /unpakx/ Itypu , Iunp , Junp , Incru
   COMMON /zzzzzz/ Z
   INTEGER Filc , Kdblm , Ncol , Novect , Sr1fil , Sr2fil
   INTEGER dashz , feer , filelm(7) , filevc(7) , i , i2 , ibuf1 , ibuf2 , ifile , ii , ik , ipos , j , k , nam(2) , ncol2 , no ,   &
         & nocl , nz
   DOUBLE PRECISION dxx(2)
   REAL file , zz
   INTEGER korsz
!
!     READ3 PACKS THE EIGENVECTORS AND EIGENVALUES AND PUTS THEM OUT IN
!     ASCENDING ORDER
!
!     LAST REVISED  1/92, BY G.CHAN/UNISYS
!     ZERO OUT RIGID BODY FREQUENCIES IF METHOD IS 'FEER' (NOT 'FEER-X'
!     NOR 'FEER-Q')
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA feer , dashz/4HFEER , 4H-X  /
   DATA nam/4HREAD , 4H3   / , i2/2/
!
!     FILELM (=KDBLM=LAMA=201) WILL HOLD THE EIGENVALUES  UPON RETURN
!     FILEVC (=FILC =PHIA=202) WILL HOLD THE EIGENVECTORS UPON RETURN
!
   filelm(1) = Kdblm
   filevc(1) = Filc
   Itypa = Rsp
   Itypb = Rsp
   Incr = 1
   Ipak = 1
   Jpak = Ncol
   ncol2 = Iprec*Ncol
   Itypu = Rsp
   Incru = 1
   nocl = 2*Ncol + 2
   nz = korsz(Z)
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
!
!     READ IN ALL EIGENVALUES
!
   ifile = Sr1fil
   CALL gopen(Sr1fil,Z(ibuf1),Rdrew)
   i = 1
   DO
      CALL fread(Sr1fil,dxx,Iprec,1)
      Z(i+1) = dxx(1)
      i = i + 1
      IF ( i>Novect ) THEN
         CALL close(Sr1fil,Rew)
!
!     SET UP AN INDEX VECTOR AND SORT THE EIGENVALUES
!
         j = Ncol + 2
         k = j + Ncol - 1
         ii = 1
         DO i = j , k
            Iz(i) = ii
            ii = ii + 1
         ENDDO
         Z(1) = Z(i2)
         j = 2
         k = j + Novect - 1
         DO i = j , k
            IF ( Z(i)<Z(1) ) Z(1) = Z(i)
         ENDDO
         DO i = 1 , Novect
            k = i
            DO WHILE ( Z(k+1)<Z(k) )
               zz = Z(k)
               Z(k) = Z(k+1)
               Z(k+1) = zz
               j = k + Ncol
               ii = Iz(j)
               Iz(j) = Iz(j+1)
               Iz(j+1) = ii
               k = k - 1
            ENDDO
         ENDDO
!
!     ZERO OUT RIGID BODY EIGENVALUES IF THEY ARE PRESENT AND METHOD IS
!     'FEER-Z'
!     I.E. ZERO FREQUENCIES BELOW PTSHFT AND KEEP, AS CHECKED BY STURM
!     SEQUENCE
!
         IF ( Sturm>=0 ) THEN
            DO i = 2 , Novect
               ik = i + Sturm
               IF ( Z(ik)>=Shftpt .OR. ik>Novect ) EXIT
               IF ( Z(i)<0. .AND. Option==feer .AND. Optn2==dashz ) Z(i) = 0.
            ENDDO
         ENDIF
!
!     READ THE EIGENVECTORS AND PACK THEM IN ASCENDING ORDER
!
         CALL gopen(filevc,Z(ibuf1),1)
         ifile = Sr2fil
         CALL gopen(Sr2fil,Z(ibuf2),Rdrew)
         ipos = 1
         CALL makmcb(filevc(1),Filc,Ncol,2,Rsp)
!
         DO i = 1 , Novect
            k = i + Ncol + 1
            no = Iz(k)
            IF ( no<ipos ) THEN
               CALL rewind(Sr2fil)
               ipos = no
               IF ( no<=0 ) GOTO 100
               CALL skprec(Sr2fil,no)
            ELSEIF ( no/=ipos ) THEN
               no = no - ipos
               ipos = ipos + no
               CALL skprec(Sr2fil,no)
            ENDIF
            Iunp = 0
            CALL unpack(*10,Sr2fil,Z(nocl))
            ipos = ipos + 1
            Ipak = Iunp
            Jpak = Junp
            GOTO 20
 10         Ipak = 1
            Jpak = 1
            Z(nocl) = 0.0
 20         CALL pack(Z(nocl),filevc,filevc)
         ENDDO
!
         CALL close(filevc(1),Rew)
         CALL close(Sr2fil,Rew)
         CALL wrttrl(filevc)
!
!     OUTPUT THE EIGENVALUES, 1ST DATA RECORD
!
         CALL gopen(filelm,Z(ibuf1),1)
         CALL write(filelm,Z(i2),Novect,1)
!
!     SAVE ORDER FOUND IN 2ND DATA RECORD
!
         CALL write(filelm,Iz(Ncol+2),Novect,1)
         CALL close(filelm(1),Rew)
         filelm(2) = Novect
         CALL wrttrl(filelm)
         RETURN
      ENDIF
   ENDDO
!
 100  CALL mesage(-7,file,nam)
!
END SUBROUTINE read3