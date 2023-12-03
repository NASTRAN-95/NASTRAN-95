!*==/home/marcusmae/nasa/nastran/SPAGged/C_SDR2X1.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_SDR2X1
   INTEGER :: Iacc, Idispl, Idload, Ieigen, Ieldef, Ielf, Ifrout, Iloads, Ilsym, Isload, Isorc, Ispcf, Istr, Isymfl,  &
            & Itload, Ittl, Ivel
!
!*****
!     DATA DEFINING POSITIONS OF PARAMETERS IN A CASE CONTROL RECORD.
!*****
   DATA ieigen/5/ , ieldef/6/ , itload/7/ , isymfl/16/ , iloads/17/ , idispl/20/ , istr/23/ , ielf/26/ , iacc/29/ , ivel/32/ ,      &
      & ispcf/35/ , ittl/39/ , ilsym/200/ , ifrout/145/ , isload/4/ , idload/13/ , isorc/136/

END MODULE C_SDR2X1
