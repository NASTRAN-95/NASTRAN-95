
SUBROUTINE inptt3
!
!     THIS ROUTINE READS MATRIX DATA FROM AN INPUT TAPE, WRITTEN IN
!     ROCKWELL INTERNATIONAL COMPANY'S CUSTOMARY FORMAT, INTO NASTRAN
!     GINO MATRIX BLOCK.
!     (THE RI DATA IS IN A COMPACT FORTRAN-FORMATTED CODED FORM, DOUBLE
!     PRECISION, WHCIH APPEARS TO HAVE QUITE WIDESPREAD ACCEPTANCE IN
!     THE AEROSPACE FIELD, AND PARTICULARY IN MARSHALL SPACE FLIEGHT
!     CENTER (MSFC) AREA)
!
!     WRITTEN ORIGINALLY BY MEL MARTENS, ROCKWELL INTERNATIONAL, SPACE
!     DIVISION (213) 922-2316, AND MODIFIED UP TO NASTRAN STANDARD BY
!     G.CHAN/UNISYS, 2/1987
!
!     INPTT3  /O1,O2,O3,O4,O5/V,N,UNIT/V,N,ERRFLG/V,N,TEST  $
!
!             UNIT  = FORTRAN INPUT TAPE UNIT NO.
!                     TAPE IS REWOUND BEFORE READ IF UNIT IS NEGATIVE
!                     FORTRAN UNIT 11 (INPT) IS USED IF UNIT= 0 OR -1.
!             ERRFLG= 1, JOB TERMINATED IF DATA BLOCK ON TAPE NO FOUND
!                     0, NO TERMINATION IF DATA BLOCK NO FOUND ON TAPE
!             TEST  = 0, NO CHECK ON FILE NAMES ON TAPE AND DMAP NAMES
!                   = 1, NAMES CHECK, WILL SEARCH TAPE FOR MATCH.
!
   IMPLICIT NONE
   DOUBLE PRECISION Dz(1)
   INTEGER Errflg , Ibuf , Ii , Incr , Jj , Nout , Rd , Rdrew , Rew , Test , Typin , Typout , Unit , Wrt , Wrtrew , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Unit , Errflg , Test
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /packx / Typin , Typout , Ii , Jj , Incr
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER buf1 , core , end , file , head , i , irew , iu , j , k , mcb(7) , name(2) , namx(2) , nc , nr , subnam(2) , type
   INTEGER korsz
   EQUIVALENCE (Z(1),Dz(1))
   DATA end , head , subnam/ - 999 , -111 , 4HINPT , 4HT3  /
!
   core = korsz(Z(1))
   buf1 = core - Ibuf + 1
   core = buf1 - 1
   Typin = 2
   Typout = 2
   Incr = 1
!
   iu = Unit
   IF ( Unit==0 .OR. Unit==-1 ) iu = -11
   IF ( iu<=0 ) THEN
      iu = -iu
      irew = 0
      REWIND iu
   ENDIF
!
   DO k = 1 , 5
      file = 200 + k
      mcb(1) = file
      CALL rdtrl(mcb)
      IF ( mcb(1)>0 ) THEN
         CALL gopen(file,Z(buf1),Wrtrew)
         CALL fname(file,name)
         DO
            READ (iu,99001,ERR=100,END=200) i , namx
99001       FORMAT (I6,2A4)
            IF ( i<=0 ) THEN
               IF ( i==end ) THEN
!
                  IF ( irew/=0 ) THEN
                     REWIND iu
                     irew = 0
                     CYCLE
                  ENDIF
               ELSEIF ( i==head ) THEN
                  IF ( namx(1)==name(1) .AND. namx(2)==name(2) ) THEN
!
!     FOUND
!
                     WRITE (Nout,99002) Uim , name
99002                FORMAT (A29,', DATA BLOCK ',2A4,' FOUND')
                  ELSE
                     WRITE (Nout,99003) Uim , namx , name
99003                FORMAT (A29,', DATA BLOCK ',2A4,' FOUND WHILE SEARCHING FOR ',2A4)
                     IF ( Test/=0 ) CYCLE
                  ENDIF
                  READ (iu,99004) nr , nc , type
99004             FORMAT (3I6)
                  WRITE (Nout,99005) name , nc , nr , type
99005             FORMAT (/5X,'MATRIX BLOCK ',2A4,' IS OF SIZE ',I6,'(COL) BY',I5,'(ROW),  AND TYPE =',I6)
                  IF ( nr>core ) CALL mesage(-8,nr-core,subnam)
                  irew = 1
                  Ii = 1
                  Jj = nr
                  CALL makmcb(mcb,file,nr,type,2)
                  DO i = 1 , nc
                     READ (iu,99006,ERR=100,END=200) (Dz(j),j=1,nr)
99006                FORMAT (12X,1P,5D24.16)
                     CALL pack(Z,file,mcb)
                  ENDDO
                  CALL close(file,Rew)
                  CALL wrttrl(mcb)
                  EXIT
               ENDIF
               WRITE (Nout,99007) Uwm , name
99007          FORMAT (A25,', INPTT3 FAILED TO LOCATE DATA BLOCK ',2A4,' ON ','TAPE')
               IF ( Errflg/=0 ) CALL mesage(-61,0,subnam)
               REWIND iu
               irew = 0
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDDO
   RETURN
!
 100  WRITE (Nout,99008) iu
99008 FORMAT ('0*** ERROR DUING READ.  TAPE UNIT',I5)
   CALL close(file,Rew)
   CALL mesage(-61,0,subnam)
 200  WRITE (Nout,99009) Uwm , iu
99009 FORMAT (A25,' FROM INPTT3, EOF ENCOUNTERED ON INPUT TAPE',I4)
   CALL close(file,Rew)
   CALL wrttrl(mcb)
END SUBROUTINE inptt3
