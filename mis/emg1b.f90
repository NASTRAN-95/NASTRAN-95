
SUBROUTINE emg1b(Buf,Sil,Ii,File,Dampc)
   IMPLICIT NONE
   LOGICAL Anycon , Error , Heat , Last
   INTEGER Dict(15) , Elid , Eltype , Estid , Filtyp , Flags(3) , Ibloc , Icall , Icmbar , Icong , Icore , Icstm , Idit , If4gg ,   &
         & Ihmat , Ilast , Imat , Irows , Jcore , Ksystm(65) , Lcong , Lcstm , Ldict , Lhmat , Lmat , Nbloc , Ncong , Ncore ,       &
         & Ncstm , Ndit , Nhmat , Nlocs , Nmat , Nsils , Outpt , Posvec(10) , Precis , Sils(10) , Z(1)
   DOUBLE PRECISION Dz(1)
   REAL Rz(1) , Smaio(36)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /emg1bx/ Nsils , Posvec , Ibloc , Nbloc , Irows , Dict , Filtyp , Sils , Last
   COMMON /emgdic/ Eltype , Ldict , Nlocs , Elid , Estid
   COMMON /emgprm/ Icore , Jcore , Ncore , Icstm , Ncstm , Imat , Nmat , Ihmat , Nhmat , Idit , Ndit , Icong , Ncong , Lcong ,      &
                 & Anycon , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat
   COMMON /iemg1b/ Icall , Ilast
   COMMON /sma1io/ Smaio
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
   DOUBLE PRECISION Dampc
   INTEGER File , Ii , Sil
   DOUBLE PRECISION Buf(1)
   REAL c
   LOGICAL double
   INTEGER i , ic , idbloc , j , j1 , j2 , kmat , ndbloc , rowsiz , subr(2) , zbase
!
!     THIS ROUTINE REPLACES SMA1B AND GROUPS TOGETHER THE
!     SUB-PARTITIONS OF A PIVOT-PARTITION.
!
!     THE SUB-PARTIONS ARE ARRANGED IN CORE BY ASCENDING SILS OF THE
!     ELEMENT INVOLVED.
!
!
   EQUIVALENCE (Ksystm(2),Outpt)
   EQUIVALENCE (Z(1),Dz(1),Rz(1)) , (c,ic)
   EQUIVALENCE (Smaio(13),If4gg)
!
   DATA subr/4HEMG1 , 4HB   /
!
   IF ( Error ) RETURN
   IF ( Sil==-1111111 ) THEN
!
!     OUTPUT PIVOT-ROWS-PARTITION
!
      IF ( Icall>0 ) THEN
         IF ( Last ) Ilast = 1
         CALL emgout(Z(Ibloc),Z(Ibloc),(Nbloc-Ibloc+1)/Precis,Ilast,Dict,Filtyp,Precis)
      ENDIF
      Ilast = 0
      Icall = 0
      GOTO 99999
   ELSE
      double = .FALSE.
      IF ( Precis==2 ) double = .TRUE.
      Icall = Icall + 1
!
!     IF -FILE- EQUALS IF4GG FOR THE OLD ELEMENTS, THEN THE ELEMENT
!     DAMPING CONSTANT SENT IS PLACED IN THE DICTIONARY AND A SIMPLE
!     RETURN IS MADE.
!
      IF ( .NOT.(Heat) ) THEN
         IF ( File==If4gg ) THEN
            c = Dampc
            Dict(5) = ic
            Icall = Icall - 1
            RETURN
         ELSEIF ( Ii<=0 ) THEN
            Irows = 6
            Dict(4) = 63
            GOTO 100
         ENDIF
      ENDIF
!
      Irows = 1
      Dict(4) = 1
   ENDIF
 100  IF ( Icall<=1 ) THEN
      rowsiz = Nsils*Irows
      Dict(3) = rowsiz
      Ibloc = Jcore + mod(Jcore+1,2)
      IF ( Dict(2)==2 ) THEN
         Nbloc = Ibloc + Irows*Precis - 1
      ELSE
         Nbloc = Ibloc + rowsiz*Irows*Precis - 1
      ENDIF
      IF ( Nbloc>Ncore ) CALL mesage(-8,Nbloc-Ncore,subr)
      IF ( double ) THEN
!
         idbloc = Ibloc/2 + 1
         ndbloc = Nbloc/2
         DO i = idbloc , ndbloc
            Dz(i) = 0.0D0
         ENDDO
      ELSE
         DO i = Ibloc , Nbloc
            Rz(i) = 0.0E0
         ENDDO
      ENDIF
   ENDIF
!
!     INSERT SUB-PARTITION OF PARTITION IN POSITION OF SIL ORDER.
!
!     BUF IS ASSUMED DOUBLE PRECISION.
!
   DO i = 1 , Nsils
      IF ( Sil==Sils(i) ) GOTO 200
   ENDDO
   WRITE (Outpt,99001) Sfm , Elid
99001 FORMAT (A25,' 3116, ELEMENT ID',I10,' SENDS BAD SIL TO ROUTINE ','EMG1B.')
   CALL mesage(-37,0,subr)
!
 200  IF ( Dict(2)==2 ) THEN
!
!     SIMPLE DIAGONAL MATRIX INSERTION
!
      kmat = 1
      IF ( double ) THEN
!
         j1 = idbloc
         DO i = 1 , Irows
            Dz(j1) = Dz(j1) + Buf(kmat)
            j1 = j1 + 1
            kmat = kmat + 14
         ENDDO
      ELSE
         j1 = Ibloc
         DO i = 1 , Irows
            Rz(j1) = Rz(j1) + sngl(Buf(kmat))
            j1 = j1 + 1
            kmat = kmat + 14
         ENDDO
      ENDIF
   ELSE
      zbase = Irows*(i-1)
      kmat = 1
      IF ( double ) THEN
!
!     DOUBLE PRECISION ADDITION OF MATRIX DATA.
!
         j1 = idbloc + zbase
         j2 = j1 + Irows - 1
         DO i = 1 , Irows
            DO j = j1 , j2
               Dz(j) = Dz(j) + Buf(kmat)
               kmat = kmat + 1
            ENDDO
            j1 = j1 + rowsiz
            j2 = j2 + rowsiz
         ENDDO
      ELSE
!
!     SINGLE PRECISION ADDITION OF DATA
!
         j1 = Ibloc + zbase
         j2 = j1 + Irows - 1
         DO i = 1 , Irows
            DO j = j1 , j2
               Rz(j) = Rz(j) + sngl(Buf(kmat))
               kmat = kmat + 1
            ENDDO
            j1 = j1 + rowsiz
            j2 = j2 + rowsiz
         ENDDO
      ENDIF
   ENDIF
!
   RETURN
99999 RETURN
END SUBROUTINE emg1b
