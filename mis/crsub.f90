
SUBROUTINE crsub(Name,I)
   IMPLICIT NONE
   INTEGER Buf(1) , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Iodum(8) , Mdidum(4) , Nxtdum(15)
   LOGICAL Ditup
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdidum , Nxtdum , Ditup
   COMMON /zzzzzz/ Buf
   INTEGER I
   INTEGER Name(2)
   INTEGER iempty(2) , indsbr , jdit , nmsbr(2)
!
!     THE SUBROUTINE CREATES AN ENTRY FOR THE SUBSTRUCTURE NAME IN THE
!     DIT THE OUTPUT PARAMETER I INDICATES THAT THE SUBSTRUCTURE NAME
!     IS THE ITH SUBSTRUCTURE IN THE DIT.
!
   DATA iempty/2*4H    /
   DATA indsbr/1/ , nmsbr/4HCRSU , 4HB   /
!
   CALL chkopn(nmsbr(1))
   IF ( Ditsiz==Ditnsb*2 ) THEN
!
!     NO INTERNAL EMPTY SPACE IN THE MDI.  DIRECTORY FOR THE NEW
!     SUBSTRUCTURE
!
      Ditsiz = Ditsiz + 2
      I = Ditsiz/2
   ELSE
!
!     THERE IS AN EMPTY INTERNAL DIRECTORY SPACE IN THE MDI.
!
      CALL fdsub(iempty(1),I)
      IF ( I==-1 ) THEN
!
!     ERROR MESSAGES.
!
         CALL errmkn(indsbr,5)
         GOTO 99999
      ENDIF
   ENDIF
!
!     UPDATE DIT.
!
   Ditnsb = Ditnsb + 1
   CALL fdit(I,jdit)
   Buf(jdit) = Name(1)
   Buf(jdit+1) = Name(2)
   Ditup = .TRUE.
   RETURN
99999 RETURN
END SUBROUTINE crsub