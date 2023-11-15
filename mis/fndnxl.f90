
SUBROUTINE fndnxl(Name,Newnm)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(1) , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Iodum(8) , Mdi , Mdibl , Mdilbn , Mdipbn , Nxtdum(15)
   LOGICAL Ditup , Mdiup
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdi , Mdipbn , Mdilbn , Mdibl , Nxtdum , Ditup , Mdiup
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Name(2) , Newnm(2)
!
! Local variable declarations
!
   INTEGER andf
   INTEGER hl , i , iempty , imdi , jdit , k , nmsbr(2)
   EXTERNAL andf
!
! End of declarations
!
!
!     THE SUBROUTINE LOOKS FOR A HIGHER LEVEL SUBSTRUCTURE TO THE
!     SUBSTRUCTURE NAME.  IF NAME DOES HAVE A HIGHER LEVEL SUBSTRUCTURE,
!     THE NAME OF THE HIGHER LEVEL SUBSTRUCTURE WILL BE RETURNED IN
!     NEWNM.  IF NAME DOES NOT HAVE A HIGHER LEVEL SUBSTRUCTURE, NAME
!     WILL BE RETURNED IN NEWNM.  IF NAME IS NOT KNOWN TO THE SYSTEM,
!     BLANKS WILL BE RETURNED IN NEWNM.
!
   DATA hl/2/
   DATA iempty/4H    / , nmsbr/4HFNDN , 4HXL  /
!
   CALL chkopn(nmsbr(1))
   CALL fdsub(Name(1),k)
   IF ( k/=-1 ) THEN
!
!     FIND THE HIGHER LEVEL SUBSTRUCTURE TO NAME.
!
      CALL fmdi(k,imdi)
      i = andf(Buf(imdi+hl),1023)
      IF ( i==0 ) THEN
!
!     NAME DOES NOT HAVE A HIGHER LEVEL SUBSTRUCTURE.
!
         Newnm(1) = Name(1)
         Newnm(2) = Name(2)
         GOTO 99999
      ENDIF
   ELSE
      Newnm(1) = iempty
      Newnm(2) = iempty
      RETURN
   ENDIF
!
!     NAME DOES HAVE A HIGHER LEVEL SUBSTRUCTURE.
!
   CALL fdit(i,jdit)
   Newnm(1) = Buf(jdit)
   Newnm(2) = Buf(jdit+1)
   RETURN
99999 END SUBROUTINE fndnxl
