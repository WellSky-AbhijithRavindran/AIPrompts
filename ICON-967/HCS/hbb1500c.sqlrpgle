     H SrtSeq(*LangIDShr) AltSeq(*Ext)
     hCOPYRIGHT('(C) Copyright WellSky 2025')
      *****************************************************************
      **                                                             **
      **                     1500 BILL PROGRAM                       **
      **                                                             **
      **     ***** NOTE ***** NOTE ***** NOTE ***** NOTE *****     **
      **     CHANGES TO THIS PROGRAM MAY ALSO NEED TO BE MADE IN     **
      **              HBR1500C - PRINTING BILL FROM HISTORY          **
      **                                                             **
      **        COPYRIGHT - 2025 - WellSky                           **
      *****************************************************************
      **                                                             **
      **   DO NOT REMOVE BILL FORM EXCEPTIONS FROM THIS PROGRAM!!  **
      **   THEY ARE USER MAINTAINABLE, IF YOUR CLIENT DOES NOT     **
      **   NEED THEM,THEY SHOULDNT SET THEM UP! IF YOU HAVE NEW    **
      **   ONES ADD THEM AS THE NEXT EXCEPTION NUMBER. DO NOT USE  **
      **   EXISTING EXCEPTION NUMBER FOR A NEW ONE!!!!!            **
      **   YEAH!!!!!?!?!1!!?1?!1one!                                 **
      *****************************************************************
      **                           ABSTRACT                          **
      **                                                             **
      **  The purpose of this program is to generate the 1500 Bills. **
      **                                                             **
      **-------------------------------------------------------------**
      **                                                             **
      **  INDICATORS USED WITHIN THE PROGRAM:                        **
      **                                                             **
      **  23 - PATIENT ZIP CODE PRINT                                **
      **  24 - SECONDARY INSURANCE CARRIER                           **
      **  27 - BILL EXCEPTION 112                                    **
      **                                                             **
      **  38 - EXCEPTION CODE 131                                    **
      **  39 - ATTACHMENT (DOCUMENTATION FLAG = 'Y')                 **
      **                                                             **
      **  53 - LETTER OF MED WITH BILL                               **
      **  54 - PROGRESS NOTES WITH BILL                              **
      **  55 - RX REQUIRED TO BILL                                   **
      **  56 - PHYSICIAN REFERRAL REQUIRED TO BILL                   **
      **  57 - LEVEL 1 & LEVEL 6 NAMES TO PRINT UNDER FORM           **
      **                                                             **
      **  61 - DEMO/GUARANTOR CHAIN                                  **
      **                                                             **
      **  70 - CHARGE CHAIN                                          **
      **                                                             **
      **  73 - Bill Exception 317                                    **
      **                                                             **
      **  75 - USE BILL FROM DATE FOR DETAIL FROM DATE               **
      **  77 - CHAIN INDICATOR                                       **
      **  78 - CHAIN INDICATOR                                       **
      **  79 - CHAIN INDICATOR                                       **
      **                                                             **
      *****************************************************************
      **                    MODIFICATION SUMMARY                     **
      **                                                             **
      **  CHANGE DATE   PROGRAMMER/COMMENTS           INSTALL DATE   **
      **                                                             **
      **  10/29/2013  Eric Bjorge - HCS1500 (02-12)   11/11/2013     **
      **             -Created program based on HBB1500N for the new  **
      **              1500 form (02-12)                              **
      **              - Installed by Michael Kundla                  **
      **                                                             **
      **  12/03/2013    Justin Sarnak                                **
      **               - removed DSM4 code                           **
      **                                                             **
      **  01/21/2014  Michael Kundla                  01/21/2014     **
      **              - Don't pass period into electronic bill form  **
      **                                                             **
      **  01/23/2014  Justin Sarnak                                  **
      **              - Changes for ICD-10                           **
      **                                                             **
      **  02/13/2014  Justin Sarnak                                  **
      **              - Changed parm for XFXDXPER                    **
      **                                                             **
      **  02/28/2014    Brian Shore                                  **
      **               -Increased the size of RQVDX1 and RQVMO1      **
      **                                                             **
      **  04/11/2014    Casey Antczak                                **
      **                - Report will display either med rec # or    **
      **                  account # depending on mrnRollup           **
      **                - Installed by Steve Ferguson                **
      **                                                             **
      **  05/20/2014    Alex Hansen                                  **
      **                - Move address and city into print fields    **
      **  05/21/2014    - Changed to use individual fields for group **
      **                  information instead of bcvsvc              **
      **                                                             **
      **  12/17/2013    Matt McClellan - AHS                         **
      **               -BGNAME no longer in HBPMAST and HBPMAST no   **
      **                longer used but BGNAME used here. Set BGNAME **
      **                equal to MMNAME.                             **
      **               -pulled from AHS by cpierce on 5/30/2014      **
      **                                                             **
      ** 06/03/2014  Steve Ferguson                                  **
      **             - use financial class from payor/plan file      **
      **                                                             **
      ** 06/04/2014  Chris Shull                      06/04/2014     **
      **             - ebillptr data structure not defined correctly **
      **             causing all diags to not pull to bill           **
      **                                                             **
      ** 06/20/2014  Casey Antczak                                   **
      **             - Edited call to XFXPPD to use data structure   **
      **                                                             **
      ** 10/24/2014  Brett Miller                                    **
      **             - bbbsts = 'V' means must be bbtorb = '8'       **
      **                                                             **
      **  10/24/2014 Brett Miller                                    **
      **             Added ICN#                                      **
      **                                                             **
      ** 11/06/2014  Alex Price                                      **
      **             - Split exception for alternate name and        **
      **              address                                        **
      **                                                             **
      ** 11/14/2014  Casey Antczak                                   **
      **             - added parm to XFXTRNSF for rate type          **
      **             Installed by Steve Ferguson                     **
      **                                                             **
      ** 11/18/2014  Steve Ferguson                                  **
      **             - alternate name should print in box32 and 33   **
      **             - alternate address should print in box33       **
      **                                                             **
      **  1/30/2015  Brett Miller                                    **
      **             - exception 681 - Print Attending Physician and **
      **               NPI in box 17 and 17B                         **
      **             - exception 117 - box 14 = blank                **
      **             - exception 682 - service dates in box 18       **
      **                                                             **
      **  2/02/2015  Brett Miller                                    **
      **             - fixed exceptions 186 and 665                  **
      **                                                             **
      **  2/03/2015  Brett Miller                                    **
      **             - do not send 0 amount charges electonically    **
      **               or count them for exception 682               **
      **                                                             **
      **  5/04/2015  Chris Shull                                     **
      **             - exception 714 - remove decimal points in      **
      **               diagnosis codes                               **
      **             - exception 715 - print "G2" and then a space   **
      **               then the facility tax ID in box32b            **
      **                                                             **
      **  5/20/2015  Brett Miller                                    **
      **             - when the single print option is taken, never  **
      **               use the overlay                               **
      **                                                             **
      ** 06/16/2015  Brett Miller                                    **
      **             - changes for exceptions by rule                **
      **                                                             **
      ** 06/09/2015  Alex Price                       06/19/2015     **
      **             -Added logic for bill aproval for rebills       **
      **              only flag                                      **
      **                                                             **
      ** 07/15/2015  Alex Hansen                                     **
      **             -Policy number was being replaced incorrectly   **
      **                                                             **
      ** 07/31/2015  Alex Price       GOL-618                        **
      **             - exception 741 - Never show modifier 59        **
      **             - Installed by Michael Kundla                   **
      **                                                             **
      ** 08/24/2015  Chris Shull      GOL-721                        **
      **             - issues with page #'s that are printed on bills**
      **             - Installed by Michael Kundla                   **
      **                                                             **
      ** 08/27/2015  Kevin Pillsbury  GOL-731                        **
      **             - exception 752 - print LU and provider number  **
      **                               in box 32b                    **
      **             - exception 753 - print total charges for bill  **
      **                               box 30                        **
      **             - print level6 and MR# at top of bill           **
      **             - Installed by Michael Kundla                   **
      **                                                             **
      ** 09/10/2015  Kevin Pillsbury  GOL-790                        **
      **             - exception 755 - alternate payor name in 11c   **
      **             - Installed by Michael Kundla                   **
      **                                                             **
      ** 10/07/2015  Chris Shull GOL-855                             **
      **             - exception 763 - pull diagnosis codes from     **
      **                   care dates.                               **
      **             - Installed by Michael Kundla                   **
      **                                                             **
      ** 12/22/2015  Kevin Pillsbury  GOL-1112                       **
      **             - fix for exceptions by rule, rule was being    **
      **               cleared after checking first exception and    **
      **               never populated again                         **
      **             - Installed by Michael Kundla                   **
      **                                                             **
      ** 01/19/2016  Alex Hansen  GOL-1171                           **
      **             - bill exception 789 to print level 6 name      **
      **               in box 31                                     **
      **             - Installed by Michael Kundla                   **
      **                                                             **
      ** 02/16/2016  Brian Shore                                     **
      **             - pulled entire program from GOL and merged     **
      **               HCS changes                                   **
      **             - reinstated code that pulled payor address     **
      **               from ICOV when other address not present      **
      **                                                             **
      ** BEGIN HCS MERGE                                             **
      **                                                             **
      ** 03/10/2015  Eric Bjorge - SHS1676                           **
      **            -Perform similar logic for the IN1AD* fields as  **
      **             is done for the PRIAD* fields if the address    **
      **             is blank                                        **
      **            -Bug fix, should be checking %eof not %found on  **
      **             the reads to HAPICOV                            **
      **             Installed by Brian Shore from Solaris           **
      **                                                             **
      ** 03/27/2015  Eric Bjorge - SHS1723                           **
      **            -SRE179 was using the wrong physician field for  **
      **             the license, which in turn updated BGDTTH.  We  **
      **             were sending wrong rendering physician assoc'd  **
      **             with the charge                                 **
      **             Installed by Brian Shore from Solaris           **
      **                                                             **
      ** 04/09/2015  Eric Bjorge - SHS1759                           **
      **            -Fixed issues where it was pulling information   **
      **             from an encounter, but the account didn't have  **
      **             an encounter.                                   **
      **             Installed by Brian Shore from Solaris           **
      **                                                             **
      ** END HCS MERGE                                               **
      **                                                             **
      ** 04/11/2016  Alex Hansen  GOL-1396                           **
      **             -Added parm to HBXICN                           **
      **            -Pulled from GOL to HCS by Brian Shore 5/31/16   **
      **                                                             **
      ** 05/05/2016  Alex Hansen  GOL-1478                           **
      **             - save box 22B for electronic billing           **
      **            -Pulled from GOL to HCS by Brian Shore 5/31/16   **
      **                                                             **
      ** BEGIN AHS PULLS (HDEV-13479)                                **
      **                                                             **
      **  02/25/2014    Matt McClellan - AHS                         **
      **               -Bug fix for ex code 6. It was putting hx6a21 **
      **                into box321 which should be the facility     **
      **                name.                                        **
      **                                                             **
      **  02/05/2015    Matt McClellan - AHS                         **
      **               -Since it is possible to have bills that are  **
      **                on different hbppdiem records, check to see  **
      **                if HBR1500C needs to be opened.              **
      **                                                             **
      **  03/04/2015    Jon Steinbronn                               **
      **               -Clear action flag when retrieving charge     **
      **                info                                         **
      **                                                             **
      **  04/02/2015    Jon Steinbronn                               **
      **               -For the "other" insurance fields, use the    **
      **                highest ranked insurance that is not the     **
      **                insurance being billed                       **
      **                                                             **
      ** END AHS PULLS (HDEV-13479) - Matthew Viola 12/20/2016       **
      **                                                             **
      **  05/02/2014    Matt McClellan - AHS            05/02/2014   **
      **               -IN1NPI, IN2NPI and IN3NPI to use PAYID first **
      **                and then try NPI if no PAYID                 **
      **               -pulled from AHS by Matthew Viola 12/21/2016  **
      **                HDEV-13480                                   **
      **               -this is conditioned by bill exception 834    **
      **                                                             **
      **  04/21/2014    Matt McClellan - AHS            01/27/2017   **
      **               -Collect box 18 information.                  **
      **               -Pulled from AHS by Dan Reeve HDEV-13482      **
      **                                                             **
      **  06/14/2012    cbailey                        03/24/2017    **
      **               - Added parms to HAXCOVG                      **
      **               -Pulled from CAR by Alex Hansen  HDEV-14498   **
      **                                                             **
      **  03/17/2017  Kevin Pillsbury  HDEV-14723                    **
      **             -move call to HBXGBILL from ARUP to HBBPRDT and **
      **              HBB1500C so it can be conditioned by bill exc  **
      **             - Pulled from AHS                               **
      **                                                             **
      **  03/20/2015    Jon Steinbronn                 05/16/2017    **
      **               -Switched printouts to use a printer file     **
      **                instead of o-specs                           **
      **               -Pulled from AHS by ZHEUSCHKEL HDEV-13485     **
      **                                                             **
      **  06/21/2017  Dan Reeve - HDEV17325                          **
      **              -Position 3 of BREL hmap has relation mapping. **
      **              -Program was searching for BIR5 hmap values to **
      **               determine relation instead of BREL hmap values**
      **              -Pulled Bill Exception 445 from RES to print   **
      **               Level 6 Street Address in Box 32.             **
      **              -Created Bill Exc 854 to print the name of the **
      **               Physician on the charge in Box 31.            **
      **                                                             **
      **  07/26/2017  Kevin Pillsbury  HDEV-17687                    **
      **              -Look up PNPI using Cat1/Cat2                  **
      **                                                             **
      **  04/10/2018    Matt McClellan - HDEV-22012       /  /       **
      **               -Added bill exception 869 to print the        **
      **                attending physician in box 24J when there is **
      **                no physician defined on the charge.          **
      **                                                             **
      **  08/31/2017  Nick Ela  - HDEV-18475                         **
      **              -Created bill exception 859: print taxonomy    **
      **              code in box 33B                                **
      **              - Pulled from CCC by CDOUGHERTY HDEV-18474     **
      **                                                             ***********
      **  06/13/2018  bgates - HDEV24158                                      **
      **             -Use HBXBREV to determine Room and Board Revenue code    **
      **                                                                      **
      **  06/14/2018    bgates - HDEV-24076                                   **
      **               -Use HXXTITLE call in place of direct file reference   **
      **                                                                      **
      **  03/16/2018  Zach Heuschkel HDEV-22001                               **
      **             -Add in bill exception 196 to print                      **
      **              authorization based on service.                         **
      **             -Pulled from FWS by Cecile Cornelus HDEV-22000           **
      **                                                                      **
      ** 11/09/2018  bgates - HDEV-26865                                      **
      **             -Bill exceptions by Category 1 and Category 2            **
      **                                                                      **
      **  05/31/2019    Kevin Pillsbury  HDEV-30509                           **
      **                - Changed HAXCOVG parms                               **
      **                                                                      **
      **  07/29/2019  Alex Price          HDEV-32557                          **
      **              - Always purge and ZB payor on hold claims              **
      **                                                                      **
      **  10/25/2019  Kevin Pillsbury  HDEV-33972                             **
      **              - Use plan name and plan ID number from                 **
      **                insurance verification if entered                     **
      **                                                                      **
      **  11/15/2019  Alex Price       HDEV-27742                             **
      **              - Added parms to xfxedits                               **
      **                                                                      **
      **  12/05/2019  Michelle Palmieri  HDEV-34067                           **
      **              - Added request value to XFXPRTCD                       **
      **                                                                      **
      **  01/06/2020  Alex Hansen  HDEV-27905                                 **
      **              -Added bill exception 907 to print box 18 using 6       **
      **               digit date (MM DD YY)                                  **
      **                                                                      **
      **  02/06/2020  Michelle Palmieri  HDEV-35290                           **
      **              - Added bill exception 910 to print 1 penny for G codes **
      **                (logic copied from HBBPRDT)                           **
      **                                                                      **
      **  06/01/2020  Catherine Dougherty HDEV-37397                          **
      **              - add exception code 417, box33a will print ALTNP prv.  **
      **                variable                                              **
      **                                                                      **
      **  06/01/2020  Catherine Dougherty HDEV-37347                          **
      **              - add exception code 918, box33 will not be affected by **
      **                code 665                                              **
      **                                                                      **
      **  06/01/2020  Catherine Dougherty HDEV-37396                          **
      **              - add exception code 919, print alternate address 2 in  **
      **                box 32                                                **
      **                                                                      **
      **  06/02/2020  Alex Hansen  HDEV-24278                                 **
      **             - Qualified RevDS data structure from CBXBREV            **
      **               copysource                                             **
      **                                                                      **
      **  09/16/2020  Alex Hansen  HDEV-39063                                 **
      **             - Added logic for bill approval for system rebills       **
      **                                                                      **
      **  01/12/2021  Michelle Palmieri  HDEV-40528                           **
      **             - Populate provider extended zip                         **
      **                                                                      **
      **  03/01/2021  Nick Ela           HDEV-41679                           **
      **             - Pull back exception 552 from CHCSOURCE                 **
      ** 11/22/2010-  Alex Hansen                                             **
      **              -Added bill exception 552 to print attending            **
      **               physician NPI in box 24J                               **
      **                                                                      **
      **  08/03/2021  Alex Hansen  IBILL-159                                  **
      **             - Retire old versions of 1500 bill form                  **
      **                                                                      **
      **  09/16/2021  George Fernett   IBILL-744                              **
      **             - Add check for provider variable ALTTX                  **
      **                                                                      **
      **  07/25/2022  Alex Hansen  IBILL-989                                  **
      **             - Need to check for exception 214 when exception 60      **
      **               is on                                                  **
      **                                                                      **
      **  10/28/2022  Matt Viola IBILL-1136                                   **
      **             - pulled bill exception 117 from RES                     **
      **             - created bill exception 527 to print patient liability  **
      **               amount in box 29                                       **
      **                                                                      **
      **  02/13/2023  Luis Mota  IBILL-1233                                   **
      **             - created bill exception 953 to blank out hospitalization**
      **               dates in box 18                                        **
      **                                                                      **
      **  04/12/2023  Alex Hansen  IBILL-1211                                 **
      **             - moved logic for bill exception 60 to HBBPR15           **
      **             - change bill exception 214 to update quantity in        **
      **               E5PCHR15                                               **
      **             - fix counter for E5PCHR15                               **
      **                                                                      **
      **  07/12/2023  Alex Hansen  IBILL-1456                                 **
      **             - Created bill exception 959 to replace diagnosis codes  **
      **               with authorization diagnosis code                      **
      **                                                                      **
      **  08/25/2023  Alex Hansen  IBILL-1438                                 **
      **             - Created bill exception 960 to replace missing          **
      **               diagnosis codes with account diagnosis codes           **
      **                                                                      **
      **  03/06/2024  Abhijith Ravindran IBILL-1932                           **
      **             - Created new save fields for moving values to the field **
      **               exCat1 & exCat2 since these variables are reset in the **
      **               bilexc subroutine while running in loop.               **
      **                                                                      **
      **  03/26/2024  Abhijith Ravindran ICON-88                              **
      **             - Added bill exception 73 for Brightspring to print      **
      **               account number in box 26                               **
      **                                                                      **
      **  03/26/2024  Sarath Kosuri  ICON-89                                  **
      **             - Added bill exception 75 for Brightspring to print      **
      **               Bill from Dates in Box 24A                             **
      **                                                                      **
      **  04/03/2024  Sarath Kosuri  ICON-90                                  **
      **             - Added bill exception 126 for Brightspring to not       **
      **               print Box 10D                                          **
      **                                                                      **
      **  03/28/2024  Abhijith Ravindran ICON-83                              **
      **             - Modify bill exception 17 for Brightspring to print     **
      **               Medicaid License number in box 24J                     **
      **                                                                      **
      **  04/03/2024  Saikiran Parupalli ICON-91                              **
      **             - Added bill exception 964 for Brightspring to print     **
      **               Alternate Facility Information in Box 32               **
      **                                                                      **
      **  04/08/2024  Sarath Kosuri  ICON-101                                 **
      **             - Added bill exception 349 for Brightspring to not       **
      **               print "Signature on File" in box 31                    **
      **                                                                      **
      **  04/08/2024  Sarath Kosuri  ICON-98                                  **
      **             - Added bill exception 325 for Brightspring to           **
      **               always check Medicaid in Box 1                         **
      **                                                                      **
      **  04/10/2024  Sarath Kosuri  ICON-143                                 **
      **             - Added logic to populate QUAL 17A in exception          **
      **               399 along with UPIN                                    **
      **                                                                      **
      **  04/11/2024  Abhijith Ravindran ICON-92                              **
      **             - Added bill exception 189 to Brightspring for printing  **
      **               ALTBN provider variable in Box 33                      **
      **                                                                      **
      **  04/10/2024  Saikiran Parupalli ICON-96                              **
      **             - Added bill exception 224 for Brightspring to print     **
      **               the date in Onset date from Billing master in Box 14   **
      **                                                                      **
      **  04/12/2024  Abhijith Ravindran ICON-100                             **
      **             - Modify bill exception 348 to Brightspring for          **
      **               excluding payor#1, payor#2 & payor#3                   **
      **                                                                      **
      **  04/12/2024  Sarath Kosuri  ICON-106                                 **
      **             - Added bill exception 470 to print TAXON variable       **
      **               in Box 24I and 24J                                     **
      **                                                                      **
      **  04/15/2024  Sarath Kosuri  ICON-114                                 **
      **             - Added bill exception 965 to print rendering provider   **
      **               taxonomy code in 24J                                   **
      **                                                                      **
      **  04/16/2024  Aakash/Abhijith ICON-102                                **
      **             - Added bill exception 425 for BrightSprint to           **
      **               clear Box32A                                           **
      **                                                                      **
      **  04/17/2024  Saikiran Parupalli ICON-108                             **
      **             - Added bill exception 549 for Brightspring to print     **
      **               the Taxonomy Code in Box 32B                           **
      **                                                                      **
      **  04/19/2024  Sarath Kosuri ICON-119                                  **
      **             - Added bill exception 935 for Brightspring to print     **
      **               PAYTO in Box 33 and send BN837 electronically          **
      **                                                                      **
      **  04/24/2024  Sarath Kosuri ICON-113                                  **
      **             - Added bill exception 591 for Brightspring to print     **
      **               Procedure Code Description above Date of Service       **
      **                                                                      **
      **  04/23/2024  Abhijith Ravindran ICON-105                             **
      **             - Added bill exception 466 for BrightSprint to           **
      **               print the taxonamy code in box 33B                     **
      **                                                                      **
      **  04/24/2024  Sarath Kosuri ICON-121                                  **
      **             - Added bill exception 948 for Brightspring to print     **
      **               2ndry Procedure Code Description above Date of Service **
      **                                                                      **
      **  04/25/2024  Abhijith Ravindran ICON-116                             **
      **             - Added bill exception 885 for BrightSprint to           **
      **               print the taxonamy code in box 33B without its         **
      **               qualifier                                              **
      **                                                                      **
      **  04/25/2024  Sarath Kosuri ICON-110                                  **
      **             - Added bill exception 584 for Brightspring to print     **
      **               Bill-To Date for Detail-To Date                        **
      **                                                                      **
      **  04/25/2024  Abhijith Ravindran ICON-118                             **
      **             - Added bill exception 966 for BrightSprint to           **
      **               print the FALOC code in box 32B without qualifier      **
      **                                                                      **
      **  04/26/2024  Sarath Kosuri ICON-120                                  **
      **             - Added bill exception 947 for Brightspring to print     **
      **               Therapist Name on charge record in Box 31              **
      **                                                                      **
      **  05/06/2024  Abhijith Ravindran ICON-141                             **
      **             - Modified bill exception 4 for BrightSprint to          **
      **               print the To date same as From date in procedure       **
      **               details.                                               **
      **                                                                      **
      **  05/08/2024  Abhijith Ravindran ICON-117                             **
      **             - Added bill exception 888 for Brightspring to clear out **
      **               other insurance details                                **
      **                                                                      **
      **  05/11/2024  Abhijith Ravindran ICON-109                             **
      **             - Added bill exception 559 for Brightspring to print the **
      **               FOM & LOM for line item service dates (Do not exceed   **
      **               Admit & Discharge date)                                **
      **                                                                      **
      **  05/20/2024  SKosuri ICON-201                                        **
      **             - Added include zero gross charges on bill?(Y/N)         **
      **                                                                      **
      **  05/22/2024  Abhijith Ravindran ICON-112                             **
      **             - Added bill exception 586 for Brightspring to print the **
      **               policy number of the bill in box 11                    **
      **                                                                      **
      **  05/23/2024  SKosuri ICON-103                                        **
      **             - Added bill exception 452 to Print qty in hours based   **
      **               on Time of Procedure and Hourly reimbursement Type     **
      **                                                                      **
      **  05/28/2024  Abhijith Ravindran ICON-111                             **
      **             - Added bill exception 585 for Brightspring to remove    **
      **               all marks in box 1                                     **
      **                                                                      **
      **  06/05/2024  SKosuri ICON-206                                        **
      **             - Charges in box 24F not showing 0.00 on all pages except**
      **               the last page, where there is no charge on that line   **
      **                                                                      **
      **  06/07/2024  SKosuri ICON-104                                        **
      **             - Added bill exception 463 to Print round qty in hours   **
      **               based on time of procedure and hourly reimbursement typ**
      **                                                                      **
      **  06/18/2024  Saikiran Parupalli  ICON-169                            **
      **             - Added new bill exception 968 to Print Attending        **
      **               physician number in Box 31                             **
      **                                                                      **
      **  06/19/2024  Saikiran Parupalli  ICON-167                            **
      **             - Added bill exceptions 970, 971 and 972 to mark the     **
      **               insurance type Tricare, Champva and Group Health Plan  **
      **               respectively in Box 1                                  **
      **                                                                      **
      **  06/24/2024  Saikiran Parupalli  ICON-163                            **
      **             - Added bill exceptions 973 to print Y4 Qualifier and    **
      **               insured ID in Box 11B                                  **
      **                                                                      **
      **  07/02/2024  SKosuri ICON-158                                        **
      **             - Added bill exceptions 974 to print additional claim    **
      **               information                                            **
      **                                                                      **
      **  09/17/2024  Saikiran Parupalli  ICON-290                            **
      **             - Added Parameters Visit type, Category 3 and Modifiers  **
      **               1 to 4 to the program XFXPRTCD                         **
      **             - Returns CPT code when RQSVAL is 'C' or 'R' and         **
      **               returns Revenue code when RQSVAL is 'V' along with the **
      **               respective modifiers                                   **
      **                                                                      **
      **  10/21/2024  SKosuri ICON-472                                        **
      **             - Fixed bug to check for BE 4 when BE 50, 59 are On.     **
      **                                                                      **
      **  11/12/2024  SKosuri ICON-502                                        **
      **             - Added bill exception 978 for Brightspring to print     **
      **               accident/onset date in Box 15                          **
      **                                                                      **
      **  12/09/2024  Saikiran Parupalli  ICON-562                            **
      **             - Fixed issue of BOX11DN not populated when there is     **
      **               only one insurance                                     **
      **                                                                      **
      **  12/12/2024  Kunal Prashnani ICON-515                                **
      **             - Added Provider Variable code ALTGP/VASID               **
      **             - Added new Bill Exception 420/423/649 for ALTGP         **
      **             - Added new Bill Exception 837/909 for VASID             **
      **                                                                      **
      **  12/12/2024  Kunal Prashnani ICON-164                                **
      **             - Allow Accident/Onset Date when workers Comp is checked **
      **               Print Date in Box 14                                   **
      **                                                                      **
      **  12/10/2024  SKosuri ICON-532                                        **
      **             - Added bill exception 979 for Brightspring to print     **
      **               accident/onset date in Box 15                          **
      **                                                                      **
      **  12/17/2024  Kunal Prashnani   ICON-166                              **
      **             - Added bill exception 190 and 195 for Brightspring to   **
      **               print ALTTX.                                           **
      **                                                                      **
      **  12/26/2024  Kunal Prashnani   ICON-170                              **
      **             - Added bill exception 981 to print account diagnosis    **
      **               codes regardless of the existence of charge diagnosis  **
      **               codes.                                                 **
      **                                                                      **
      **  01/10/2025  SKosuri ICON-586                                        **
      **             - Added bill exception 983 for Brightspring to populate  **
      **               Box 22A with resubmission code                         **
      **                                                                      **
      **  01/14/2025  SKosuri ICON-619                                        **
      **             - Change BE 964 to pull provider variable ALTFN along    **
      **               with ALTFA into Box 32                                 **
      **                                                                      **
      **  7/20/2021   Charles Nagy - INC001613057 / HDEV-42034                **
      **             -Created new bill exception 935, based on                **
      **              bill exception 189, to pull the PAYTO                   **
      **              provider variable information                           **
      ** 01/31/2025  -pulled from RES by Matt Viola ICON-627                  **
      **                                                                      **
      ** 02/25/2025   Abhijith Ravindran ICON-694                             **
      **             - Added bill exception 989 to print the converted        **
      **               quantity based on minutes per unit in column 24G.      **
      **                                                                      **
      ** 02/27/2025  Abhijith Ravindran ICON-714                              **
      **             -Added new exception 990 for rolling up charges on       **
      **              same service date.                                      **
      **                                                                      **
      ** 03/10/2025  Alex Price         ICON-722                              **
      **             -During type of rebill 7 reassignment also check         **
      **              if the original claim was transmitted                   **
      **                                                                      **
      ** 03/18/2025  Kunal Prashnani   ICON-762                               **
      **             - Added BE 189 to pull Alternate billing name            **
      **              by using APVNM provider variable                        **
      **                                                                      **
      ** 03/26/2025  Kunal Prashnani   ICON-782                               **
      **             - BE 935 not working as expected.                        **
      **                                                                      **
      ** 04/01/2025  Saikiran Parupalli  ICON-808                             **
      **             - Added B515Q field to save the Box 15 Qualifier for the **
      **               electronic claims                                      **
      **                                                                      **
      ** 04/17/2025  SKosuri ICON-695                                         **
      **             -Added BE 992 to not print the claim when there are no   **
      **              detail lines with charges                               **
      **                                                                      **
      ** 04/30/2025   Craig Schulte  ICON-832                                 **
      **             -Added code to populate the qualifier in box             **
      **              24I, the PIN in box 24J, and the NPI in box             **
      **              24J bottom in E5PCHR15 for use in the 837               **
      **             - Added bill exception 995 to print rendering provider   **
      **               taxonomy code in 24J and qualifier in 24i              **
      **                                                                      **
      ** 05/01/2025   Craig Schulte  ICON-849                                 **
      **             -Added bill exception 994 for Brightspring to populate   **
      **              1 in BBTORB when BBTORB is not 6,7 or 8                 **
      **                                                                      **
      ** 05/02/2025  Saikiran Parupalli  ICON-839                             **
      **             - Fixed the charge modifiers overriding the modifiers    **
      **               from override maintenance on paper and electronic      **
      **               claims                                                 **
      **                                                                      **
      ** 05/09/2025  Alex Price   ICON-884                                    **
      **             - Fixed issue where BE 196 is turning off the processing **
      **               of other detail BEs                                    **
      **             - Make BE 196 work with revenue code and visit related   **
      **               verification by service records                        **
      **                                                                      **
      ** 05/14/2025   Matt Viola ICON-862                                     **
      **             -added logic for BE 992 to print fractional units        **
      **                                                                      **
      ** 05/22/2025  SKosuri ICON-869                                         **
      **             - Added BE 996 to print treatment diagnosis codes when   **
      **               BE 992 is on                                           **
      **                                                                      **
      ** 05/22/2025  Alex Price    ICON-880                                   **
      **             - Account for Info Only IVSVC Records                    **
      **                                                                      **
      ** 06/18/2025  SKosuri ICON-916                                         **
      **             - Added BE 113 to not print Patient's address in Box 5   **
      **                                                                      **
      ** 06/19/2025  SKosuri ICON-917                                         **
      **             - Added BE 114 to not print Patient's relatonship to the **
      **             Insured in Box 6                                         **
      **                                                                      **
      ** 06/24/2025  Kunal Prashnani  ICON-1011                               **
      **             - Added BE 227 to print Overriding Provider Numbers in   **
      **               Box 24k.                                               **
      **             - Added BE 419 to print Overriding Provider Numbers in   **
      **               Box 24j-a.                                             **
      **                                                                      **
      ** 06/24/2025  SKosuri ICON-1012                                        **
      **             - Added BE 999 to print Level 6 address in Box 32        **
      **                                                                      **
      ** 06/30/2025  SKosuri ICON-1013                                        **
      **             - Added BE 1000 to print Attending physician NPI         **
      **               in Box 24J                                             **
      **                                                                      **
      ** 06/30/2025  SKosuri ICON-1016                                        **
      **             - Added BE 1001 to print Policy in Box 9A with qualifier **
      **                                                                      **
      ** 07/01/2025  Alex Price ICON-934                                      **
      **             - Added BE 319 Print Representative Name Level 5         **
      **                                                                      **
      ** 07/02/2025  SKosuri ICON-1017                                        **
      **             - Added BE 1002 to print Provider variable in Box 32B    **
      **               along with dynamic qualifier                           **
      **                                                                      **
      ** 07/09/2025  SKosuri ICON-938                                         **
      **             - Added BE 328 to print Patient Name and Address in Box32**
      **                                                                      **
      ** 07/09/2025  Alex Price ICON-953                                      **
      **             - Added BE 543 to Print Provider Number in Boxes 32A     **
      **               and 33A                                                **
      **                                                                      **
      ** 07/09/2025  SKosuri ICON-966                                         **
      **             - Added BE 833 to print 'ATYPICAL' in Box 33A.           **
      **                                                                      **
      ** 07/10/2025  SKosuri ICON-919                                         **
      **             - Added BE 116 to not print Box 10.                      **
      **                                                                      **
      ** 07/14/2025  SKosuri ICON-913                                         **
      **             - Added BE 78 to not print Box 25E.                      **
      **                                                                      **
      ** 07/15/2025  Saikiran Parupalli  ICON-912                             **
      **             - Added Bill Exception 74 to not print Box 12.           **
      **                                                                      **
      ** 07/16/2025  SKosuri ICON-914                                         **
      **             - Added Bill Exception 79 to print Procedure Provider    **
      **               Number in PIN# Box 33B.                                **
      **                                                                      **
      ** 07/17/2025  SKosuri ICON-921                                         **
      **             - Added Bill Exception 118 to not print Box 23           **
      **                                                                      **
      ** 07/29/2025  Saikiran Parupalli  ICON-932                             **
      **             - Added Bill Exception 287 to not use time conversion    **
      **               when printing units in Box 24G                         **
      **                                                                      **
      ** 07/29/2025  Abhijith Ravindran ICON-924                              **
      **             - Added Bill Exception 152 to not print Box 11 to 11D    **
      **                                                                      **
      ** 08/04/2025  SKosuri ICON-910                                         **
      **             - Added BE 71 to deduct patient share from billed amt    **
      **                                                                      **
      ** 08/13/2025  Saikiran Parupalli ICON-967                             **
      **             - Added Bill Exception 861                              **
      **************************************************************************
     fhblbilfa  up   e           k disk
     fhblbiltr  if   e           k disk    rename(hbfbill:hbfbiltr)
     f                                     prefix(t:1)
     fhblbillt  if   e           k disk    rename(hbfbill:hbfbillt)
     f                                     prefix(t:1)
     fhbptobf   if   e           k disk
     fhblchg15  if   e           k disk
     fhbpchrg   if   e           k disk
     fhbpchgbl  if   e           k disk
     fhblchgmm  if   e           k disk     rename(hbfchr15:hbfchgmm)
     fhbltscdd  if   e           k disk
     fhblmmod   if   e           k disk
     fhbpmodwf  uf a e           k disk
     fhblwkmod  uf a e           k disk
     fhblalwsp  if   e           k disk
     fhblalwsm  if   e           k disk
     f                                      rename(hbfalws:hbfalwsm)
     fhapdemo   if   e           k disk
     fhapguar   if   e           k disk
     fhmpmams   if   e           k disk
     f******hmpdiag   if   e           k disk
     fhmpmdiag  if   e           k disk
     fhmpenct   if   e           k disk
     fhapdstn   if   e           k disk
     f****hxpinsd   if   e           k disk
     fhxpinst   if   e           k disk
     fhxplvl1   if   e           k disk
     fhxplvl5   if   e           k disk
     fhxplvl6   if   e           k disk
     fhmpmast   if   e           k disk
     fhxpbnfit  if   e           k disk
     fhxpprocc  if   e           k disk
     fhxpprocm  if   e           k disk
     fhbpcpt4   if   e           k disk
     fhxpctg1   if   e           k disk
     fhmlplnp3  if   e           k disk
     fhbpbfex   if   e           k disk
     fhblbexct  if   e           k disk
     fhbpbexc   if   e           k disk    rename(hbfbexc:hbfbexc1)
     fhbpbfxo   if   e           k disk
     fhbpcdsc   if   e           k disk
     fhbpnobil  if   e           k disk
     fhapdemoi  if   e           k disk
     fhapicov   if   e           k disk
     fhbpdiem   if   e           k disk
     fhbppdiem  if   e           k disk
     fhmpasgn   if   e           k disk
     fhmllsndt  if   e           k disk
     fhmlpdiat  if   e           k disk
     fhxpcdfle  if   e           k disk
     fhxppomp   if   e           k disk
     f****hmpdsm4   if   e           k disk
     fhupappt   if   e           k disk
     f****hxltitlb  if   e           k disk
     fhbpcdgn   if   e           k disk
     fhbpcompc  if   e           k disk
     fhxprmad   if   e           k disk
     fhappntrk  if   e           k disk
     fhmlurautb if   e           k disk
     fhxpprrc   if   e           k disk
     fhxlprrcd  if   e           k disk
     f                                     rename(hxfprrc:hxfprrcd)
     fe5pchr15  uf a e           k disk
     fe5lchr15  uf a e           k disk    rename(e5fchr15:e5fchr15l)
     f                                     prefix(z:1)
     fe5p1500   uf a e           k disk
     Fhalivsvc  IF   E           K DISK
     fhblbilot  if   e           k disk    rename(hbfbill:hbfbilot)
     f                                     prefix(f:1)
     fhbpgtob   o    e           k disk
     fhbpbillh  o    e             disk
     f                                     rename(hbfbill:hbfbillh)
     fhbsbill   o    e             disk
     f                                     rename(hbfbill:hbfbills)
     fprintr2   o    f  198        printer oflind(*inoa) usropn
     f****printr3   o    f  132        printer oflind(*inof) usropn
     fhbr1500c  o    e             printer               usropn
     fhbr1500cn o    e             printer               usropn
     f                                     rename(bill15:BILLNOOVL)
      *****************************************************************
      /EJECT
      *****************************************************************
      **                       Prototype Definition                  **
      *****************************************************************
     d XFXDXPER        pr                  extpgm('XFXDXPER')
     d  inputdx                       7    const options(*varsize)
     d  inputver                      2    const options(*varsize)
     d  returndx                      8    options(*varsize)

     d HBXSYSRBL       pr                  extpgm('HBXSYSRBL')
     d  level6                        6  0 const
     d  account                      12  0 const
     d  billtrak                     14    const
     d  sysrebil                       n

     d XFXSNAM         pr                  extpgm('XFXSNAM')
     d  nameIn_                      26    const
     d  nameOut_                     35

     d XFXBQ15         pr                  extpgm('XFXBQ15')
     d  payor_                        6  0 const
     d  code_                         5    const
     d  map_                          2

     d XFXCVTIM        pr                  extpgm('XFXCVTIM')
     d  quantity                      7  2
     d  convertCode                   2    const
     d  convertType                   2    const
     d  rtncod                        1

      /copy copysrc,cbxbrev
      /copy copysrc,cxxtitle
      /copy copysrc,hxxmngc
      /copy copysrc,cfxproc
      *****************************************************************
      **                       ARRAY DEFINITION                      **
      *****************************************************************
     d dx1             s              6    dim(4)
     d dxc             s              6    dim(4)
     d dxd             s             18    dim(4)
     d pbf             s              8  0 dim(12)
     d pbt             s              8  0 dim(12)
     d tot             s              9  2 dim(3)
     d inc             s              5  3 dim(999)
     d qty5            s              5  0 dim(6)
      *****************************************************************
     d**** bcvsvc          ds
     d****  bcvga1                 1     25
     d****  bcvga2                26     50
     d****  bcvgct                51     68
     d****  bcvgst                69     70
     d****  bcvgz1                71     75
     d****  bcvgz2                76     79
     d****  bcvgmt                80     80
     d****  bcvgph                81     90
     d****  bcvgfx                91    100
     d                 ds
     d eps                            2    dim(12)
     d  eps01                  1      2
     d  eps02                  3      4
     d  eps03                  5      6
     d  eps04                  7      8
     d  eps05                  9     10
     d  eps06                 11     12

     d                 ds
     d  ediag                  1     84    dim(12)
     d  bdia01                 1      7
     d  bdia02                 8     14
     d  bdia03                15     21
     d  bdia04                22     28
     d  bdia05                29     35
     d  bdia06                36     42
     d  bdia07                43     49
     d  bdia08                50     56
     d  bdia09                57     63
     d  bdia10                64     70
     d  bdia11                71     77
     d  bdia12                78     84

     d                 ds
     d dtf                            8    dim(12)
     d  dtf01                  1      8
     d  dtf02                  9     16
     d  dtf03                 17     24
     d  dtf04                 25     32
     d  dtf05                 33     40
     d  dtf06                 41     48
     d                 ds
     d dtt                            8    dim(12)
     d  dtt01                  1      8
     d  dtt02                  9     16
     d  dtt03                 17     24
     d  dtt04                 25     32
     d  dtt05                 33     40
     d  dtt06                 41     48
     d                 ds
     d dtf6                           6    dim(12)
     d  dtf601                 1      6
     d  dtf602                 7     12
     d  dtf603                13     18
     d  dtf604                19     24
     d  dtf605                25     30
     d  dtf606                31     36
     d                 ds
     d dtt6                           6    dim(12)
     d  dtt601                 1      6
     d  dtt602                 7     12
     d  dtt603                13     18
     d  dtt604                19     24
     d  dtt605                25     30
     d  dtt606                31     36
     d                 ds
     d dtfm                           2    dim(12)
     d  dtfm01                 1      2
     d  dtfm02                 3      4
     d  dtfm03                 5      6
     d  dtfm04                 7      8
     d  dtfm05                 9     10
     d  dtfm06                11     12
     d                 ds
     d dttm                           2    dim(12)
     d  dttm01                 1      2
     d  dttm02                 3      4
     d  dttm03                 5      6
     d  dttm04                 7      8
     d  dttm05                 9     10
     d  dttm06                11     12
     d                 ds
     d dtfd                           2    dim(12)
     d  dtfd01                 1      2
     d  dtfd02                 3      4
     d  dtfd03                 5      6
     d  dtfd04                 7      8
     d  dtfd05                 9     10
     d  dtfd06                11     12
     d                 ds
     d dttd                           2    dim(12)
     d  dttd01                 1      2
     d  dttd02                 3      4
     d  dttd03                 5      6
     d  dttd04                 7      8
     d  dttd05                 9     10
     d  dttd06                11     12
     d                 ds
     d dtfy                           2    dim(12)
     d  dtfy01                 1      2
     d  dtfy02                 3      4
     d  dtfy03                 5      6
     d  dtfy04                 7      8
     d  dtfy05                 9     10
     d  dtfy06                11     12
     d                 ds
     d dtty                           2    dim(12)
     d  dtty01                 1      2
     d  dtty02                 3      4
     d  dtty03                 5      6
     d  dtty04                 7      8
     d  dtty05                 9     10
     d  dtty06                11     12
     d                 ds
     d pos                            2    dim(12)
     d  pos01                  1      2
     d  pos02                  3      4
     d  pos03                  5      6
     d  pos04                  7      8
     d  pos05                  9     10
     d  pos06                 11     12
     d                 ds
     d cpt                            8    dim(12)
     d  cpt01                  1      8
     d  cpt02                  9     16
     d  cpt03                 17     24
     d  cpt04                 25     32
     d  cpt05                 33     40
     d  cpt06                 41     48
     d                 ds
     d mod                           12    dim(18)
     d  mod01                  1     12
     d  mod011                 1      3
     d  mod012                 4      6
     d  mod013                 7      9
     d  mod014                10     12
     d  mod02                 13     24
     d  mod021                13     15
     d  mod022                16     18
     d  mod023                19     21
     d  mod024                22     24
     d  mod03                 25     36
     d  mod031                25     27
     d  mod032                28     30
     d  mod033                31     33
     d  mod034                34     36
     d  mod04                 37     48
     d  mod041                37     39
     d  mod042                40     42
     d  mod043                43     45
     d  mod044                46     48
     d  mod05                 49     60
     d  mod051                49     51
     d  mod052                52     54
     d  mod053                55     57
     d  mod054                58     60
     d  mdo06                 61     72
     d  mod061                61     63
     d  mod062                64     66
     d  mod063                67     69
     d  mod064                70     72

     d                 ds
     d cds                           27    dim(12)
     d  cds01                  1     27
     d  cds02                 28     54
     d  cds03                 55     81
     d  cds04                 82    108
     d  cds05                109    135
     d  cds06                136    162

     d                 ds
     d pdsc                          30    dim(12)
     d  pdsc01                 1     30
     d  pdsc02                31     60
     d  pdsc03                61     90
     d  pdsc04                91    120
     d  pdsc05               121    150
     d  pdsc06               151    180

     d                 ds
     d amt                            9  2 dim(12)
     d  amt01                  1      9  2
     d  amt02                 10     18  2
     d  amt03                 19     27  2
     d  amt04                 28     36  2
     d  amt05                 37     45  2
     d  amt06                 46     54  2
     d                 ds
     d amta                           9  2 dim(12)
     d  amt01a                 1      9  2
     d  amt02a                10     18  2
     d  amt03a                19     27  2
     d  amt04a                28     36  2
     d  amt05a                37     45  2
     d  amt06a                46     54  2
     d                 ds
     d qty                            3  0 dim(12)
     d  qty01                  1      3  0
     d  qty02                  4      6  0
     d  qty03                  7      9  0
     d  qty04                 10     12  0
     d  qty05                 13     15  0
     d  qty06                 16     18  0
     d                 ds
     d qty4                           4  0 dim(12)
     d  qty41                  1      4  0
     d  qty42                  5      8  0
     d  qty43                  9     12  0
     d  qty44                 13     16  0
     d  qty45                 17     20  0
     d  qty46                 21     24  0
     d                 ds
     d qty2                           6  2 dim(12)
     d  qty21                  1      6  2
     d  qty22                  7     12  2
     d  qty23                 13     18  2
     d  qty24                 19     24  2
     d  qty25                 25     30  2
     d  qty26                 31     36  2
     d                 ds
     d dix                            7    dim(12)
     d  dix01                  1      7
     d  dix02                  8     14
     d  dix03                 15     21
     d  dix04                 22     28
     d  dix05                 29     35
     d  dix06                 36     42
     d                 ds
     d pin                           10    dim(12)
     d  pin01                  1     10
     d  pin02                 11     20
     d  pin03                 21     30
     d  pin04                 31     40
     d  pin05                 41     50
     d  pin06                 51     60
     d                 ds
     d bx24i                          3    dim(12)
     d  bx24i1                 1      3
     d  bx24i2                 4      6
     d  bx24i3                 7      9
     d  bx24i4                10     12
     d  bx24i5                13     15
     d  bx24i6                16     18
     d                 ds
     d npi                           10    dim(12)
     d  npi01                  1     10
     d  npi02                 11     20
     d  npi03                 21     30
     d  npi04                 31     40
     d  npi05                 41     50
     d  npi06                 51     60
     d                 ds
     d ddx                            7    dim(12)
     d  ddx01                  1      7
     d  ddx02                  8     14
     d  ddx03                 15     21
     d  ddx04                 22     28
     d  ddx05                 29     35
     d  ddx06                 36     42
     d                 ds
     d dxv                            8    dim(99)
     d  dxv1                   1      8
     d  dxv2                   9     16
     d  dxv3                  17     24
     d  dxv4                  25     32
     d  dxv5                  33     40
     d  dxv6                  41     48
     d  dxv7                  49     56
     d  dxv8                  57     64
     d  dxv9                  65     72
     d  dxv10                 73     80
     d  dxv11                 81     88
     d  dxv12                 89     96
     d
     d                 ds
     d prbirt                         8  0
     d  prbdmm                        2  0 overlay(prbirt)
     d  prbddd                        2  0 overlay(prbirt:3)
     d  prbdyy                        4  0 overlay(prbirt:5)
     d   prbdyr                       2  0 overlay(prbdyy:3)
     d                 ds
     d sbbdte                         8  0
     d  prs1yy                        4  0 overlay(sbbdte)
     d   prs1yr                       2  0 overlay(prs1yy:3)
     d  prs1mm                        2  0 overlay(sbbdte:5)
     d  prs1dd                        2  0 overlay(sbbdte:7)
     d                 ds
     d injdte                         8  0
     d  indtyy                        4  0 overlay(injdte)
     d   indtyr                       2  0 overlay(indtyy:3)
     d  indtmm                        2  0 overlay(injdte:5)
     d  indtdd                        2  0 overlay(injdte:7)

     d                 ds
     d box15dte                       8  0
     d  bx15yy                        4  0 overlay(box15dte)
     d   bx15yr                       2  0 overlay(bx15yy:3)
     d  bx15mm                        2  0 overlay(box15dte:5)
     d  bx15dd                        2  0 overlay(box15dte:7)

     d nameout         ds
     d  namf                   1     15
     d  naml                  16     35
     d                 ds
     d dxdesc                        18
     d  dxcode                        6    overlay(dxdesc)
     d  dxname                       11    overlay(dxdesc:8)
     d                 ds
     d wkdat6                         6  0
     d  mmdd6                         4  0 overlay(wkdat6)
     d   mm6                          2  0 overlay(mmdd6)
     d   dd6                          2  0 overlay(mmdd6:3)
     d  yy6                           2  0 overlay(wkdat6:5)
     D                 ds
     D  wrkymd                 1      8  0
     D  wrkyy                  1      4  0
     D  wrkmm                  5      6  0
     D  wrkdd                  7      8  0
     d                 ds
     d adds                          48
     d  adcty                        15    overlay(adds)
     d  adsta                         2    overlay(adds:17)
     d  adzip                         5    overlay(adds:20)
     d  addash                        1    overlay(adds:25)
     d  adzp2                         4    overlay(adds:26)
     d                 ds
     d tlds                          10
     d  tlp1                          3    overlay(tlds)
     d  tlp2                          3    overlay(tlds:4)
     d  tlp3                          4    overlay(tlds:7)
     d                 ds
     d  b21                    1     80
     d                                     dim(4)
     d  pb211a                 1     20
     d  pb212a                21     40
     d  pb213a                41     60
     d  pb214a                61     80
     d binchk          ds
     d binary                         9b 0
     d  valueh                        1    overlay(binary:3)
     d  valuel                        1    overlay(binary:4)
     d rqfcl           ds
     d  afcx                   1    196
     d                                     dim(98)
     d rqubc           ds
     d  aubx                   1    588  0
     d                                     dim(98)
     d rqplc           ds
     d  aplx                   1    490  0
     d                                     dim(98)
     d rqpol           ds
     d  apox                   1   1960
     d                                     dim(98)
     d rqisq           ds
     d  isqx                   1    392  0
     d                                     dim(98)                              PRORATE REMAINING
     d wkafc           ds
     d  afc                    1      6
     d                                     dim(3)
     d wkaub           ds
     d  aub                    1     18  0
     d                                     dim(3)
     d wkplc           ds
     d  apl                    1     15  0
     d                                     dim(3)
     d wkpol           ds
     d  apo                    1     60
     d                                     dim(3)
     d wkisq           ds
     d  isq                    1     12  0
     d                                     dim(3)                               PRORATE REMAINING

     d level           ds           297
     d lv6abr                196    198
     d                 ds
     d rundt                         14  0
     d  runtme                        6  0 overlay(rundt)
     d  rundte                        8  0 overlay(rundt:7)
     d                 ds
     d box19                         47
     d  lstdte                        8    overlay(box19)
     d  lic#                         20    overlay(box19:10)

     d rtnaddress      ds            80
     d  rtnadr                 1     25
     d  rtnad2                26     50
     d  rtncty                51     65
     d  rtnst                 66     67
     d  rtnzp1                68     72
     d  rtnzp2                73     76

     d                 ds
     d****  ebillptr                      8    dim(4)
     d  ebillptr               1      8
     d                                     dim(4)
     d   bgdptr                1      2
     d   bgdpt2                3      4
     d   bgdpt3                5      6
     d   bgdpt4                7      8

     d                sds
     d  pgmnam           *proc

     d                 ds
     d xfbfut
     d  xfbpartb                      1    overlay(xfbfut:1)
     d  xfbuad                        1    overlay(xfbfut:2)
     d  xfbcmp                        1    overlay(xfbfut:3)
     d  xfbnfl                        1    overlay(xfbfut:4)
     d  xfbmdp                        1    overlay(xfbfut:5)
     d  xfbber                        5    overlay(xfbfut:6)
     d  xfbfer                        5    overlay(xfbfut:11)

     d ds763           ds                  likeds(chgDS) dim(9999)
     d chgDS           ds                  qualified
     d  chgSeq                        7  0
     d  chgdg1                        7
     d  chgdg2                        7
     d  chgdg3                        7
     d  chgdg4                        7
     d  chgdg5                        7
     d  chgdg6                        7
     d  chgdg7                        7
     d  chgdg8                        7
     d  chgdg9                        7
     d  chgdg10                       7
     d  chgdg11                       7
     d  chgdg12                       7
     d  chgdg13                       7
     d  chgdg14                       7

     d ds763d          ds                  likeds(chgDSd) dim(9999)
     d chgDSd          ds                  qualified
     d  seq                           7  0
     d  dg1                           7
     d  dg2                           7
     d  dg3                           7
     d  dg4                           7

     d                 ds
     d tscdDiag                1     98
     d                                     dim(14)
     d  btsdia                 1      7
     d  btsdx2                 8     14
     d  btsdx3                15     21
     d  btsdx4                22     28
     d  btsdx5                29     35
     d  btsdx6                36     42
     d  btsdx7                43     49
     d  btsdx8                50     56
     d  btsdx9                57     63
     d  btsdx0                64     70
     d  btsmx1                71     77
     d  btsmx2                78     84
     d  btsmx3                85     91
     d  btsmx4                92     98

     d                 ds
     d relcode                       10
     d  relfill                       2    overlay(relcode)
     d  relchar                       1    overlay(relcode:3)

     d                 ds
     d wrktim                         4  0
     d  xfphr                         2  0 overlay(wrktim)
     d  xfpmn                         2  0 overlay(wrktim:3)
      *****************************************************************
      /copy copysrc,hxxcovg
      /copy copysrc,hxxlda
      *******/copy hcssource/copysrc,hxxdiag
      /copy copysrc,hxxdiag10
      /copy copysrc,hxxrow
      /copy copysrc,hxxcol
      /copy copysrc,hxxppd
      *****************************************************************
     d attach          c                   const('ATTACHMENT')
     d continue        c                   const('CONTINUED ON NEXT PAGE')
     d lmn             c                   const('LMN')
     d phr             c                   const('PHR')
     d prg             c                   const('PRG')
     d prm             c                   const('PRIMARY')
     d rxr             c                   const('RXR')
     d sec             c                   const('SECONDARY')
     d ter             c                   const('TERTIARY')
     d sign            c                   const('SIGNATURE ON FILE')
     d slash1          c                   const('/')
      *****************************************************************
     d accchg          s              9  2
     d accdue          s              9  2
     d accpgs          s              5  0
     d accpmt          s              9  2
     d acct#           s             12  0
     d action          s              1
     d acttmp          s             12  0
     d addedCareDG     s               n
     d arcmbr          s              9
     d arctyp          s              1
     d asgflg          s              1
     d asglv6          s              6  0
     d baldue          s              8  2
     d belong          s              1
     d billtype        s              1
     d bilchg          s              9  2
     d bildte          s              8  0
     d bildue          s              9  2
     d bilpgs          s              5  0
     d bilpmt          s              9  2
     d biltrk          s             14
     d bodyp           s              3
     d box18a          s              8  0
     d box18b          s              8  0
     d box325e         s              4
     d cat2            s              3
     d chgseq          s              7  0
     d chkcnt          s              2  0
     d chkprv          s              1
     d chPRsp          s              9  2 inz
     d chnenc          s               n
     d chr15new        s              7  0
     d chr15old        s              7  0
     d chrgdxs         s                   like(bgdicd) dim(4)
     d chrlv6          s              6
     d cklv6           s              6  0
     d cktype          s              1
     d cmoyr           s              6  0
     d cntins          s              8  0
     d code            s              2  0
     D colO            s                   like(cl) dim(%elem(cl))
     D colN            s                   like(cl) dim(%elem(cl))
     d conalw          s              8  2
     d** count           s              2  0
     d count           s              3  0
     d counter         s              5  0
     d cpayor          s              6  0
     d cplan           s              5  0
     d crtspl          s              1
     d ct              s              3  0
     d ct1flg          s              1
     d ct2flg          s              1
     d cursvd          s              8  0
     d d               s              2  0
     d dattmp          s              8  0
     d dc              s              2  0
     d dd              s              2  0
     d decimal2        s              2  0
     d dftflg          s              1
     d dgcnt           s              1  0
     d dtline          s              2  0
     d doc#            s              9  0
     d dupDetail       s               n
     d dx              s              1  0
     d dz              s              1  0
     d ecode           s             10
     d edate           s              8  0
     d eight           s              8
     d epayor          s              6  0
     d eplan           s              5  0
     d epol            s             20
     d elecwrite       s               n
     d exc04           s               n
     d exc05           s               n
     d exc10           s              1
     d exc12           s               n
     d exc38           s               n
     d exc77           s              1
     d exc152          s               n
     d exc176          s               n
     d exc179          s              1
     d exc186          s               n
     d exc189          s               n
     d exc196          s               n
     d exc212          s              1
     d exc224          s               n
     d exc343          s              1
     d exc401          s               n
     d exc417          s               n
     d exc425          s               n
     d exc443          s              1
     d exc452          s               n
     d exc527          s               n
     d exc532          s               n
     d exc543          s               n
     d exc549          s               n
     d exc665          s               n
     d exc682          s               n
     d exc714          s               n
     d exc753          s               n
     d exc759          s               n
     d exc763          s               n
     d exc833          s               n
     d exc834          s               n
     d exc837          s               n
     d exc885          s               n
     d exc859          s               n
     d exc861          s               n
     d exc910          s               n
     d exc918          s               n
     d exc919          s               n
     d exc935          s               n
     d exc953          s               n
     d exc959          s               n
     d exc960          s               n
     d exc964          s               n
     d exc978          s               n
     d exc979          s               n
     d exc981          s               n
     d exc983          s               n
     d exc989          s               n
     d exc992          s               n
     d exc994          s               n
     d exc996          s               n
     d ex763Ct         s              4  0
     d exCat1          s              3
     d exCat2          s              3
     d ex452q          s              5  2
     d ex989q          s              5  2
     d savCat1         s              3
     d savCat2         s              3
     d exfc            s              2
     d exlvl6          s              6  0
     d expayr          s              6  0
     d explan          s              5  0
     d exrevc          s              4  0
     d exRule          s              5
     d exstat          s              2
     d factor          s              5  2
     d fincls          s              2
     d first           s              1
     d first992        s               n
     d firstDate       s              8  0
     d flag            s              1  0
     d form            s              2
     d formtp          s              1
     d found           s              1
     d foundURDiag     s               n
     d foundPrcrt      s               n
     d frmdte          s              8  0
     d frstdt          s              8  0
     d garflg          s              1
     d grsnet          s              1
     d hcsfc           s              2
     d hld24k          s              9  0
     d hbadat          s              8  0
     d hmap            s             10
     d icndcn          s             23
     d i               s              4  0
     d incO            s              5  3 DIM(%elem(rw))
     d incN            s              5  3 DIM(%elem(rw))
     d ins#            s              1  0
     d insbil          s              9
     d initTrtDt       s              8  0
     d include         s               n
     d jobnam          s             10
     d jobnum          s              6
     d jobusr          s             10
     d keylv6          s              6  0
     d labchrg         s              1
     d lastdt          s              8  0
     d lastDate        s              8  0
     d lcycdt          s              8  0
     d ldesc           s             50
     d level6          s              6  0
     d levl6p          s              6  0
     d levl6#          s              6  0
     d**** levl#           s              1  0
     d licnum          s             20
     d lsdate          s              8
     d lvl2ad          s             26
     d lvl2ct          s             15
     d lvl2nm          s             30
     d lvl2st          s              2
     d lvl2tl          s             10  0
     d lvl2zp          s              5
     d lvl2z2          s              4
     d lvl6nm          s             26
     d lv6chg          s              9  2
     d lv6due          s              9  2
     d lv6pgs          s              5  0
     d lv6pmt          s              9  2
     d lv6#            s              6  0
     d lv6tmp          s              6  0
     d l1name          s             40
     d l6name          s             40
     d maxdate         s              8  0 inz(99999999)
     d mdate           s              8  0
     d newrev          s              4  0
     d nobill          s              1
     d minsPerUnit     s              5  0 inz(0)
     d mmnct1          s              3
     d mmnct2          s              3
     d mmncpt          s              5
     d mmndag          s              7
     d mmnprc          s              7
     d mmncde          s              8
     d mmnvtp          s              2
     d mmnrev          s              4  0
     d mmndrg          s              4  0
     d mmnrmc          s              3
     d mmnct3          s              3
     d mmnqty          s              5  0
     d mmnamtp         s              9  2
     d mmnubc          s              6  0
     d mmnpln          s              5  0
     d mmnpol          s             20
     d mmnisq          s              4  0
     d mmnsvdp         s              8  0
     d mmnbdt          s              8  0
     d mmnmod          s              2
     d mmnct#          s              7  0
     d mmndoc          s              9  0
     d mmnpos          s              2
     d modct1          s              3
     d modct2          s              3
     d modlty          s              1
     d modpln          s              5  0
     d modprc          s              8
     d modsdt          s              8  0
     d modubn          s              6  0
     d modVerCount     s              2  0
     d mrnRollup       s              1
     d omtype          s              1
     d onetrk          s              1
     d open            s              1
     d otherInsNum     s             15  0
     d pageno          s              4  0
     d pageno2         s              4  0
     d paychg          s              9  2
     d paydue          s              9  2
     d payor           s              6  0
     d paypgs          s              5  0
     d paypmt          s              9  2
     d physicianName   s             26
     d plan            s              5  0
     d plnchg          s              9  2
     d plndue          s              9  2
     d plnpgs          s              5  0
     d plnpmt          s              9  2
     d policy          s             20
     d posmap          s              2
     d prauth          s             17
     d prcct1          s              3
     d prcct2          s              3
     d prgrp#          s             17
     d prgrpx          s             17
     d prtf            s             10
     d prtplcy         s             20
     d prtr            s             10
     d priadr          s             20
     d priad2          s             20
     d priatn          s             26
     d pricty          s             15
     d prinam          s             28
     d prinm2          s             28
     d prinn2          s             14
     d prisd2          s             26
     d prista          s              2
     d prizip          s              5
     d prizp2          s              4
     d prmacc          s             12  0
     d prmApp          s              1
     d prmbyr          s              2  0
     d prmcod          s              1
     d prmgdt          s              8  0
     d prmisq          s              4  0
     d prmlv6          s              6  0
     d prmnam          s             10
     d prmmsg          s             70
     d prmpyr          s              6  0
     d prmpln          s              5  0
     d prmpol          s             20
     d prmval          s             25
     d prmct1          s              3
     d prmct2          s              3
     d prmfle          s             10
     d prmmbr          s             10
     d prmubc          s              6  0
     d prmprc          s              8
     d prmdte          s              8  0
     d prmrev          s              4  0
     d prmrel          s                   like(in1rel)
     d prmspl          s             10
     d prmResp         s              1
     d prmInu          s              7  2
     d prtpmt          s              1
     d prtpol          s             20
     d prvid           s              5
     d prvvar          s             80
     d prprov          s             15
     d prresc          s              9
     d prrg#           s             15
     d prrnpi          s             10
     d prrphy          s             26
     d prrupn          s             12
     d prtcod          s              9
     d prthnm          s             21
     d physname        s             21
     d prtmd1          s              2
     d prtmd2          s              2
     d prtmd3          s              2
     d prtmd4          s              2
     d prtnum          s             12  0
     d prtnumh         s              9
     d prtnums         s              4
     d prttyp          s              2
     d pstsvc          s              1
     d qua32b          s              2
     d qua33b          s              2
     d reqcod          s              9  0
     d rdate           s              8  0
     d reqdat          s              8  0
     d reqexp          s              1
     d reqitp          s              1
     d reqlv6          s              6  0
     d reqnm2          s             12  0
     d reqnum          s             12  0
     d reqprc          s              8
     d reqsrc          s              2
     d reqsys          s              1
     d reqtra          s             18
     d reqtyp          s              2
     d resad1          s             20
     d rescty          s             15
     d returndx        s              8
     d rk              s              2  0
     d rlcode          s              2  0
     d rptbls          s              5  0
     d rptchg          s              9  2
     d rptdue          s              9  2
     d rptpgs          s              5  0
     d rptpmt          s              9  2
     D rowO            s                   like(rw) dim(%elem(rw))
     D rowN            s                   like(rw) dim(%elem(rw))
     d rqabl           s             98
     d rqaccount       s             12  0
     d rqacct          s             12  0
     d rqcat1          s              3
     d rqcat2          s              3
     d rqcat3          s              3
     d rqcat4          s              3
     d rqchng          s              1
     d rqdate          s              8  0
     d rqdoctor#       s              9  0
     d rqefd           s            784
     d rqetd           s            784
     d rqfrdt          s              8  0
     d rqgrp           s           1666
     d rqiseq          s              4  0
     d rqisq1          s              4  0
     d rqisq2          s              4  0
     d rqisq3          s              4  0
     d rqlevel6        s              6  0
     d rqlictype       s              5
     d rqlvl6          s              6  0
     d rqmrno          s              9  0
     d rqoamt          s              7  2
     d rqpayr          s              6  0
     d rqplan          s              5  0
     d rqport          s              1
     d rqpro           s             98
     d rqproc          s              8
     d rqrcls          s              2
     d rqroom          s             10
     d rqrtyp          s              1
     d rqsacc          s             12  0
     d rqsct1          s              3
     d rqsct2          s              3
     d rqsct3          s              3
     d rqsdte          s              8  0
     d rqslv6          s              6  0
     d rqspln          s              5  0
     d rqsprc          s              8
     d rqspyr          s              6  0
     d rqssvd          s              8  0
     d rqstate         s              2
     d rqstatechk      s              1
     d rqsval          s              1
     d rqsvtp          s              2
     d rqtodt          s              8  0
     d rqtrak          s             14
     d rqtype          s              1
     d rqvis           s            294
     d rtncat2         s              3
     d rtncod          s              1
     d rtnexpire       s              8  0
     d rtnxtn          s              6
     d rtnflg          s              1
     d rtnlicense      s             20
     d rtnlv1          s              2  0
     d rtnlv2          s              2  0
     d rtnlv3          s              2  0
     d rtnlv4          s              4  0
     d rtnlv5          s              6  0
     d rtnphn          s             10  0
     d rtntherapist    s              9
     d snpi            s             10
     d rtplac          s              2
     d rtnprn          s             20
     d rtnReimType     s              5
     d rttran          s              1
     d rttype          s              2
     d runidx          s              1
     d savdte          s              8  0
     d savprc          s              8
     d svproc          s              8
     d savrev          s              4  0
     d sbsex           s              1
     d sb2bdt          s              8  0
     d sb2sex          s              1
     d sdesc           s             20
     d secind          s              1  0
     d secind2         s              1  0
     d sep             s              1    inz(' ')
     d singlePrint     s              1    inz('N')
     d spayor          s              6  0
     d splan           s              5  0
     d splitUB15       s               n
     d spol            s             20
     d srlv6           s              6  0
     d sract           s             12  0
     d status          s              2
     d svbild          s              8  0
     d svCat1          s              3
     d svCat2          s              3
     d svdate          s              8  0
     d svdiag          s              7
     d svfc            s              2
     d svlvl6          s              6  0
     d svpayr          s              6  0
     d svplan          s              5  0
     d svrevc          s              4  0
     d svRule          s              5
     d svstat          s              2
     d svtrak          s             14
     d sysrbl          s               n
     d svdtqt          s              5  0
     d taxid           s             15
     d taxlevel        s              1
     d taxlevelid      s              6  0
     d tcode           s              4
     d therap          s              9  0
     d theronfile      s              9  0
     d thrct2          s              3
     d tind            s              1
     d**** title           s              3
     d tmpfc           s              2
     d tmptim          s              6  0
     d tmpnam          s             13
     d tmp5char        s              5
     d tobill          s              7
     d todate          s              8  0
     d totchg          s              8  2
     d totChgBill      s              8  2
     d totpmt          s              8  2
     d trkact          s             14
     d ubcode          s              6  0
     d ubplan          s              5  0
     d ubisq           s              4  0
     d usenub          s               n
     d vasid           s             75
     d wkcat1          s              3
     d wkcat2          s              3
     d wkcat3          s              3
     d wklvl6          s              6  0
     d wkpayr          s              6  0
     d wkplan          s              5  0
     d workfreq        s              2
     d wrkatn          s             26
     d wrkct1          s              3
     d wrkct2          s              3
     d wrkct3          s              3
     d wrkdiag         s              7
     d wrkfcl          s              2
     d wrkfld          s              9  2
     d wrklv6          s              6  0
     d wrkpln          s              5  0
     d wrkpyr          s              6  0
     d wrkrnk          s              2  0
     d wxFfA1          s             30
     d wxFfA2          s             30
     d wxFfCt          s             20
     d wxFfNm          s             30
     d wxFfSt          s              2
     d wxFfTl          s             10
     d wxFfZp          s             10
     d wx33tl          s             10
     d wx33nm          s             30
     d wx33a1          s             30
     d wx33a2          s             30
     d wx33ct          s             20
     d wx33st          s              2
     d wx33z5          s              5
     d wx33z4          s              4
     d x               s              3  0
     d xfptme          s              5  0
     d xx              s              3  0
     d xy              s              3  0
     d y               s              3  0
     d ydate           s              8  0
     d ydate1          s              8  0
     d ydate2          s              8  0
     d ydays           s              5  0
     d xxy             s              2  0
     d z               s              2  0
     d zz              s              2  0

      ***************************************************************
      ** Constants
      ***************************************************************
     d ALPHABET        c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      *****************************************************************
     ihbfbill
     i                                          bbaccn        l1
     i                                          bbname        l2
     i                                          bbplan        l3
     i                                          bbpayr        l4
     i                                          bbplv6        l5
     i                                          bbplv5        l6
      *****************************************************************
     c     *entry        plist
     c                   parm                    reqlv6
     c                   parm                    crtspl
     c                   parm                    prmmbr
     c                   parm                    prmfle
     c                   parm                    prmspl
     c                   parm                    runidx
      *****************************************************************
      ** initialization subroutine
      *****************************************************************
     c     once          ifne      'D'
     c                   move      'D'           once              1
     c                   eval      open = *blanks
      **
     c                   if        crtspl = 'P'
     c                   eval      singlePrint = 'Y'
     c                   eval      crtspl = 'N'
     c                   else
     c                   eval      singlePrint = 'N'
     c                   endif
      **
     c     *dtaara       define    *lda          ldads
     c     *dtaara       define    hxalevel      level
     c     *dtaara       define    hbadate       hbadat
     c                   in        ldads
 002 c                   in        level
 002 c                   in        hbadat
      **
     c                   eval      reqdat = bbtodt
     c                   exsr      srppd
     c                   if        (sndf10 = 'Y'
     c                             or sndf10 = 'A')
     c                             and singlePrint <> 'Y'
     c                   eval      prmspl = 'HBR1500C'
     c                   else
     c****               eval      prmspl = 'PRINTR3'
     c                   eval      prmspl = 'HBR1500CN'
E003 c                   endif
      **
 002  **
     c****               if        bbaafl <> *blanks

     c                   callp     HBXSYSRBL(bbplv6 : bbaccn : bbtrak : sysrbl)

     c                   if        bbaafl = 'X'
     c                             or (bbaafl ='R' and bbbsts <> 'R')
     c                             or (bbaafl ='S' and sysrbl = *off)
     c                             or bbproc <> *blanks
     c                             and *inu7 = *off
     c                   move      'P'           arctyp
     c                   else
     c                   move      'T'           arctyp
     c                   endif
     c**
     c                   call      'HXXGETARC'
     c                   parm                    prmfle
     c                   parm                    arcmbr
     c**
     c                   eval      prmmbr = arctyp + arcmbr
 002  **
      **
     c                   move      'N'           crtspl
     c                   call      'XFXMRASG'
     c                   parm                    asgflg
      **
     c                   call      'XFXGUASG'
     c                   parm                    garflg
      **
     c                   call      'XFXRTVJA'
     c                   parm      *blanks       jobnam
     c                   parm      *blanks       jobusr
     c                   parm      *blanks       jobnum
      **
     c     dmoiky        klist
     c                   kfld                    asglv6
     c                   kfld                    bbmrno
     c                   kfld                    ubcode
     c                   kfld                    ubplan
     c                   kfld                    policy
      **
      **
     c**
     c     physky        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    status
     c                   kfld                    bbtodt
      **
     c     physk2        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    status
     c**
     c****     repkey        klist
     c****                   kfld                    levl#
     c****                   kfld                    bbplv6
     c****                   kfld                    title
      **
     C     IVSKEY        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    bbiseq
     C                   KFLD                    bbpayr
     C                   KFLD                    bbplan
     C                   KFLD                    bbplcy
     **
     c     prmkey        klist
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
      **
     c     scdkey        klist
     c                   kfld                    ubcode
     c                   kfld                    ubplan
      **
     c     chgkey        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
      **
     c     enckey        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bgdapp
      **
     c     chgkeysq      klist
     c                   kfld                    bgcbl6
     c                   kfld                    bgcbac
     c                   kfld                    bgcbsq
      **
     c     alwkey        klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtac
     c                   kfld                    bgdtpr
     c                   kfld                    bgdseq
      **
     c     thprky        klist
     c                   kfld                    msgdr#
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    level6
      **
     c     thprk2        klist
     c                   kfld                    therap
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    level6
      **
      **
      **
     c     billtp        klist
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    wkcat1
     c                   kfld                    wkcat2
     c                   kfld                    wkcat3
     c                   kfld                    workfreq
      **
     c     mastky        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
      **
     c     demoky        klist
     c                   kfld                    asglv6
     c                   kfld                    mmmrno

     c     pntrKy2       klist
     c                   kfld                    asglv6
     c                   kfld                    mmmrno
     c                   kfld                    bbtodt
      **
     c     guarky        klist
     c                   kfld                    keylv6
     c                   kfld                    mmgacc
      **
     c     excpky        klist
     c                   kfld                    exstat
     c                   kfld                    exlvl6
     c                   kfld                    exCat1
     c                   kfld                    exCat2
     c                   kfld                    expayr
     c                   kfld                    explan
     c                   kfld                    exfc
     c                   kfld                    exrevc
     c                   kfld                    exRule
     c                   kfld                    excode
      **
     c     exccod        klist
     c                   kfld                    form
     c                   kfld                    formtp
      **
     c     exccod2       klist
     c                   kfld                    form
     c                   kfld                    formtp
     c                   kfld                    excode
      **
     c     exokey        klist
     c                   kfld                    hestat
     c                   kfld                    helvl6
     c                   kfld                    hecat1
     c                   kfld                    hecat2
     c                   kfld                    hefc
     c                   kfld                    hepayr
     c                   kfld                    heplan
     c                   kfld                    herevc
     c                   kfld                    herule
     c                   kfld                    heexcd
     c                   kfld                    omtype
     c                   kfld                    bgdtpr
      **
     c     empkey        klist
     c                   kfld                    mmpesi
     c                   kfld                    mmempc
      **
     c     diagky        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    todate
      **
     c     cpyrkey       klist
     c                   kfld                    cpayor
     c                   kfld                    cplan
      **
     c     procky        klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtpr
      **
     c     cdsckey       klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtac
     c                   kfld                    bgdseq
      **
     c     rmadky        klist
     c                   kfld                    lv6tmp
     c                   kfld                    rqroom
      **
     c     mmodky        klist
     c                   kfld                    modubn
     c                   kfld                    modpln
     c                   kfld                    modct1
     c                   kfld                    modct2
     c                   kfld                    modsdt
      **
     c     mmodk2        klist
     c                   kfld                    modubn
     c                   kfld                    modpln
     c                   kfld                    modct1
     c                   kfld                    modct2
      **
     c     modwky        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    modsdt
      **
     c     modwk2        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
      **
     c     mmchky        klist
     c                   kfld                    wrklv6
     c                   kfld                    wrkacn
      **
     c     alwmky        klist
     c                   kfld                    wrklv6
     c                   kfld                    wrkacn
     c                   kfld                    modprc
     c                   kfld                    wrkcpr
     c                   kfld                    wrkcps
     c                   kfld                    wrksdt
      **
     c     wkmdky        klist
     c                   kfld                    wrklv6
     c                   kfld                    wrkacn
     c                   kfld                    wrksdt
     c                   kfld                    wrkcpr
     c                   kfld                    wrkcps
      **
     c     instky        klist
     c                   kfld                    wrkubn
     c                   kfld                    code
      **
     c     apptky        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbmrno
     c**
     c     poskey        klist
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    wrkct1
     c                   kfld                    wrkct2
     c                   kfld                    wrkct3
     c                   kfld                    bgdpos
      **
     c     prmicovkey1   klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bbiseq
     c                   kfld                    bbfrdt
      **
     c     prmicovkey2   klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bbiseq
      **
     c     scdicovkey1   klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    ubisq
     c                   kfld                    bbfrdt
      **
     c     scdicovkey2   klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    ubisq
      **
     c     tscdkey       klist                                                  =Start of Care
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    btsthr
     c                   kfld                    maxdate
      **
     c     tscdky2       klist                                                  =Start of Care
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    btsthr
      **
     c     mdiagky       klist
     c                   kfld                    icdVer
     c                   kfld                    tscdDiag(x)
      **
     c     noBilKey      klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    bbplcy
     c                   kfld                    bbiseq
      **
     c     origClaim     klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bbtrko
      **
      /free
       // Get alignment for printer file with overlay
       prtf = 'HBR1500C';
       exsr setptr;
       rowO = rw;
       colO = cl;
       incO = inc;

       // Get alignment for printer file without overlay
       prtf = 'HBR1500CN';
       exsr setptr;
       rowN = rw;
       colN = cl;
       incN = inc;
      /end-free
      **
     c                   move      '15'          form
     c                   eval      cntins = 0
      **
     c                   eval      pageno = 0
     c                   eval      pageno2 = 0
     c                   eval      gtbstp = 0
      **
     c                   if        bbtran <> 'D'
     c                   if        reqlv6 <> 0
     c                   call      'HBXNXTK'
     c                   parm      reqlv6        levl6#
     c                   parm      *blanks       trkact
     c                   else
     c                   call      'HBXNXTK'
     c                   parm      999999        levl6#
     c                   parm      *blanks       trkact
     c                   endif
     c                   movel(p)  trkact        gtbgtn
      **
      **
     c                   endif
      **
     c                   eval      page = 0
      **
      **
     c                   call      'XFXMRNROL'
     c                   parm                    mrnRollup
      **
     c                   if        mrnRollup = 'Y'
     c                   eval      prtnumh = 'MED REC #'
     c                   eval      prtnums = 'M/R '
     c                   else
     c                   eval      prtnumh = 'ACCOUNT #'
     c                   eval      prtnums = 'ACCT'
     c                   endif
      **
      **
     c                   time                    rundt
      **
     c                   call      'XFXUBMAP'
     c                   parm      '6'           cktype
     c                   parm      bbplv6        cklv6
     c                   parm                    rtnlv1
     c                   parm                    rtnlv2
     c                   parm                    rtnlv3
     c                   parm                    rtnlv4
     c                   parm                    rtnlv5
      **
     c                   eval      l1name = *blanks
     c     rtnlv1        chain     hxflvl1                            79
     c                   if        *in79 = *off
     c                   call      'XFXCNTR'
     c                   parm      hx1nam        l1name
     c                   endif
      **
     c                   eval      rptchg = 0
     c                   eval      rptpmt = 0
     c                   eval      rptdue = 0
     c                   eval      rptpgs = 0
     c                   eval      rptbls = 0
      **
     c                   endif
      *****************************************************************
      ** l5 break - new level 6
      *****************************************************************
     c                   if        *inl5
      **
     c                   eval      l6name = *blanks
     c     bbplv6        chain     hxflvl6                            79
     c                   if        *in79 = *off
     c                   move      hx6num        chrlv6
     c     hx6nam        cat(p)    chrlv6:1      l6name
     c                   call      'XFXCNTR'
     c                   parm                    l6name
     c                   endif
      **
     c                   if        open = 'Y'
     c                   move      *on           *inoa
     C                   endif
      **
      ** Check for provider variable Alternate Tax ID
     c                   eval      prmlv6 = bbplv6                              prmlv6
     c                   eval      payor = *zeros                               payor
     c                   eval      plan = *zeros                                plan
     c                   eval      prmct1 = bbcat1                              prmct1
     c                   eval      prmct2 = *blanks                             prmct2
     c                   eval      prvid = 'ALTTX'                              prvid
     c                   eval      prmprc = *blanks                             prmprc
     c                   eval      prmrev = *zeros                              prmrev
     c                   eval      prmdte = bbfrdt                              prmdte
     c                   eval      prvvar = *blanks                             prvvar
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      taxid = prvvar
     c                   else
     c                   call      'XFXTAXID'
     c                   parm                    bbplv6
     c                   parm                    taxid
     c                   parm      '6'           taxlevel
     c                   parm                    taxlevelid
     c                   endif
      **
     c                   endif
      **
      *****************************************************************
      ** check to see if 1500 new of 1500 old form
      *****************************************************************
     c                   if        mrnRollup = 'Y'
     c                   eval      prtnum = bbmrno
     c                   else
     c                   eval      prtnum = bbaccn
     c                   endif
     **
     c                   eval      pageno = 0
      **
     c****               eval      payor = bbpayr
     c****               eval      plan = bbplan
     c****               eval      prmrev = 0
     c****               eval      prmprc = *blanks
     c****               eval      prmct1 = ' '
     c****               eval      prmct2 = ' '
     c****               eval      prvid = '15FRM'
     c****               eval      prmlv6 = bbplv6
     c****               exsr      srprv
     c****               if        prvvar <> '02-12'
     c****               goto      skip
     c****               endif
     c                   if        open <> 'Y'
     c                   if        prmspl = 'HBR1500C'
     c                   open      hbr1500c
     c****               open      printr3
     c                   open      hbr1500cn
     c*******************except    alignPage
     c                   else
     c****               open      printr3
     c                   open      hbr1500cn
     c*******************except    alignPage
     c                   endif
     c                   open      printr2
     c                   move      *on           *inoa
     c                   eval      open = 'Y'
     c                   elseif    not%open(hbr1500c)
     c                   eval      reqdat = bbtodt
     c                   exsr      srppd
     c                   if        sndf10 = 'Y'
     c                             or sndf10 = 'A'
     c                   open      hbr1500c
     c                   endif
     c                   endif
      *****************************************************************
      ** clear prior detail
      *****************************************************************
     c     bbtrak        setll     e5fchr15
     c                   dou       *in79 = *on
     c     bbtrak        reade     e5fchr15                               79
     c                   if        *in79 = *off
     c                   delete    e5fchr15
     c                   endif
     c                   enddo
     c     bbtrak        chain     e5f1500                            79
     c                   if        *in79 = *off
     c                   delete    e5f1500
     c                   endif
     c                   clear                   e5f1500
     c                   eval      elecwrite = *off
     c                   eval      ediag = *blanks
      *****************************************************************
      ** Check for bill exception 532 and 834
      *****************************************************************
      **
     c                   eval      newrev = 0
     c     bbtrak        setll     hbfchgbl
     c                   dou       %eof
     c     bbtrak        reade     hbfchgbl
     c                   if        not %eof
     c     chgkeysq      chain     hbfchrg
     c                   if        %found
     c                             and bgctqt <> 0
     c                   eval      bgdlv6 = bgclv6
     c                   eval      bgdtpr = bgctpr
     c     procky        chain     xffprocc
     c                   eval      newrev = xfpubc
     c                   leave
     c                   endif
     c                   endif
     c                   enddo
      **
     c     mastky        chain     hmfmast
     c                   if        %found(hmpmast)
     c                   eval      exCat1 = mmpct1
     c                   eval      exCat2 = mmpct2
     c                   eval      savCat1 = exCat1
     c                   eval      savCat2 = exCat2
     c                   endIf
      **
     c                   eval      exc532 = *off
     c                   eval      excode = 532
     c                   move      'F'           formtp
     c     exccod2       chain     hbfbexc
     c                   if        %found
     c                             and exdelt <> 'X'
     c                   movel     hx6st1        exstat
     c                   eval      exlvl6 = bbplv6
     c                   eval      expayr = bbpayr
     c                   eval      explan = bbplan
     c                   movel     tmpfc         exfc
     c                   eval      exrevc = newrev
     c                   eval      exCat1 = savCat1
     c                   eval      exCat2 = savCat2
     c                   exsr      bilexc
     c                   endif
      **
     c                   eval      exc834 = *off
     c                   eval      excode = 834
     c     exccod2       chain     hbfbexc
     c                   if        %found
     c                             and exdelt <> 'X'
     c                   movel     hx6st1        exstat
     c                   eval      exlvl6 = bbplv6
     c                   eval      expayr = bbpayr
     c                   eval      explan = bbplan
     c                   movel     tmpfc         exfc
     c                   eval      exrevc = newrev
     c                   eval      exCat1 = savCat1
     c                   eval      exCat2 = savCat2
     c                   exsr      bilexc
     c                   endif

      ** Check for payor hold
     c     noBilKey      setll     hbfnobil
     c                   dou       %eof(hbpnobil)
     c     noBilKey      reade     hbfnobil
     c                   if        not %eof(hbpnobil)
     c                             and nbrsnm = 'PAYORHLD'
     c                             and bbfrdt = nbfrdt
     c                             and bbtodt = nbtodt
     c                   eval      bbtran = 'G'
     c                   eval      bbform = 'ZB'
     c                   eval      nobill = 'X'
     c                   goto      enddtl
     c                   endif
     c                   enddo

     c                   if        not exc532
     c                   call      'HBXGBILL'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm                    bbtrak
     c                   parm      *blanks       rttran
     c                   if        rttran = 'G'
     c                   eval      bbtran = 'G'
     c                   endif
     c                   endif

      *****************************************************************
      ** see if bill has any charges - if not get out
      *****************************************************************
     c                   exsr      chkchg
     c*****************************************************************
     c** See if zero balance bills should be produced
     c*****************************************************************
     c                   if        rtnflg <> 'Y'
     c                             and exc532 = *off                            =Print all charges
      **
     c                   eval      rqdate = bbfrdt
     c                   eval      rqcat1 = mmpct1
     c                   eval      rqcat2 = mmpct2
      **
     c                   call      'XFXEDITS'
     c                   parm      bbplv6        prmlv6
     c                   parm      bbaccn        prmacc
     c                   parm      ' '           prmval
     c                   parm      'ZEROBAL'     prmnam
     c                   parm      bbpayr        prmpyr
     c                   parm      bbplan        prmpln
     c                   parm      bbplcy        prmpol
     c                   parm      bbiseq        prmisq
     c                   parm      rqcat1        prmct1
     c                   parm      rqcat2        prmct2
     c                   parm      bbfrdt        prmdte
     c                   parm      0             prmbyr                         =Birth Year
     c                   parm      *blanks       prmResp
     c                   parm      *blanks       prmApp
     c                   parm      0             prmgdt
     c                   parm                    prmmsg
     c                   parm      ' '           prmcod
      **
     c                   if        prmcod = ' '                                 =No zero bills
     c                   if        bbproc = ' '
     c                   eval      bbform = 'ZB'
     c                   endif
     c                   eval      nobill = 'X'
     c                   goto      enddtl
     c                   endif
      **
     c                   endif
     c**
     c                   eval      nobill = ' '
     c****               eval      gtbstp = 1 + pageno
     c                   eval      gtbstp = 1 + pageno2

     c                   eval      exRule = *blanks
     c     prmkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   eval      exRule = xfbber
     c                   endif
      *****************************************************************
      ** check cat 1 and cat 2 once per bill here
      *****************************************************************
     c     prmkey        setll     hbfdiem
     c                   dou       *in70 = *on
     c     prmkey        reade     hbfdiem                                70
     c                   if        *in70 = *off
     c                   if        bbfrdt >= xffdt
     c                             and bbfrdt <= xftdt
     c                   if        xfct1 = 'X'
     c                   eval      ct1flg = 'X'
     c                   else
     c                   eval      ct1flg = *blanks
     c                   endif
     c                   if        xfct2 = 'X'
     c                   eval      ct2flg = 'X'
     c                   else
     c                   eval      ct2flg = *blanks
     c                   endif
     c                   leave
     c                   endif
     c                   endif
     c                   enddo
      *****************************************************************
      ** clear fields
      *****************************************************************
     c                   eval      box10d = *blanks
     c                   eval      exc179 = ' '
      **
     c                   if        bbtran = 'D'
     c                   if        reqlv6 <> 0
     c                   call      'HBXNXTK'
     c                   parm      reqlv6        levl6#
     c                   parm      *blanks       trkact
     c                   else
     c                   call      'HBXNXTK'
     c                   parm      999999        levl6#
     c                   parm      *blanks       trkact
     c                   endif
     c                   movel(p)  trkact        gtbgtn
      **
      **
     c                   movel     trkact        bbtrak
     c                   endif
      **
     c                   eval      bilchg = 0
     c                   eval      bilpmt = 0
     c                   eval      bildue = 0
     c                   eval      bilpgs = 0
      **
      **
      ****  ATTACHMENT
     c                   if        bbdcfl = 'Y'
     c                   move      *on           *in39
     c                   else
     c                   move      *off          *in39
     c                   endif
      **
     c                   call      'XFXDTAAR'
     c                   parm                    bbplv6
     c                   parm                    bildte
     c                   parm                    flag
     c                   parm                    lcycdt
      **
     c                   eval      ydate = bildte
     c                   exsr      srcymd
     c                   eval      rptdte = mdate
     c                   eval      rptdt6= %int(%char(%date(rptdte:*usa):*mdy0))
      **
      ****  BOX 32 - FACILITIY WHERE SERVICES WERE RENDERED
      ****  BOX 33 - SUPPLIER'S BILLING NAME AND ADDRESS
      **
     c                   eval      lvl2nm = *blanks
     c                   eval      lvl2ad = *blanks
     c                   eval      lvl2ct = *blanks
     c                   eval      lvl2st = *blanks
     c                   eval      lvl2zp = *blanks
     c                   eval      lvl2z2 = *blanks
     c                   eval      lvl2tl = 0
     c                   eval      lvl6nm = *blanks
     c                   eval      box321 = *blanks
     c                   eval      box322 = *blanks
     c                   eval      box323 = *blanks
     c                   eval      box324 = *blanks
     c                   eval      box325 = *blanks
     c                   eval      box325e = *blanks
     c                   eval      box32a = *blanks
     c                   eval      box32b = *blanks
     c                   eval      wx33tl = *blanks
     c                   eval      wx33nm = *blanks
     c                   eval      wx33a1 = *blanks
     c                   eval      wx33a2 = *blanks
     c                   eval      wx33ct = *blanks
     c                   eval      wx33st = *blanks
     c                   eval      wx33z5 = *blanks
     c                   eval      wx33z4 = *blanks
     c                   eval      wxFfNm = ' '
     c                   eval      wxFfA1 = ' '
     c                   eval      wxFfA2 = ' '
     c                   eval      wxFfCt = ' '
     c                   eval      wxFfSt = ' '
     c                   eval      wxFfZp = ' '
     c                   eval      wxFfTl = ' '

     c                   eval      physicianName = *blanks
     c                   eval      initTrtDt = *zeros
      **
      **
     c                   eval      chnenc = *off
      **
     c     apptky        setll     hufappt
     c                   dou       *in78 = *on
     c     noappt        tag
     c     apptky        reade     hufappt                                78
     c                   if        *in78 = *off
     c     huadte        cabgt     bbtodt        noappt
     c     huadte        cablt     bbfrdt        noappt
     c                   endif
     c                   enddo
      **
     c                   clear                   hmfenct
     c     chgkey        setll     hbfchr15                                     =get appointment nbr
     c     nextcharge2   tag                                                     from a charge that
     c     chgkey        reade     hbfchr15                                      will be on the bill
     c                   if        not %eof(hblchg15)
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c                   eval      action = ' '
     c                   exsr      srcgbl
     c     action        cabeq     'N'           nextcharge2
     c                   endif
     c     bgdapp        cabeq     0             nextcharge2
     c                   eval      chnenc = *on
     c     enckey        chain     hmfenct                                      =get encounter rcd
     c                   endif
      **
     c                   if        %found(hmpenct) and enaafm <> 0              =encounter found
     c                             and chnenc = *on
     c     enaafm        chain     xffdstn
     c                   if        %found(hapdstn)
     c                   movel     xfddsc        lvl6nm
     c                   movel     xfddsc        box321
     c                   if        xfdad2 <> *blanks
     c                   movel     xfdad2        box322
     c                   movel     xfdcty        box323
     c                   movel     xfdsta        box324
     c                   movel     xfdzp1        box325
     c                   movel     xfdzp2        box325e
     c                   else
     c                   movel     xfdadr        box322
     c                   movel     xfdcty        box323
     c                   movel     xfdsta        box324
     c                   movel     xfdzp1        box325
     c                   movel     xfdzp2        box325e
     c                   endif
     c                   goto      home
     c                   endif
     c                   endif
      **
     c     bbplv6        chain     hxflvl6                            79
     c     huafac        cabeq     'HM'          home
     c                   if        *in79 = *off
     c                   movel     hx6nam        lvl6nm
     c                   movel     hx6nam        box321
     c                   if        hx6a21 <> *blanks
     c                   movel     hx6a21        box322
     c                   movel     hx6ct2        box323
     c                   movel     hx6st2        box324
     c                   movel     hx6z21        box325
     c                   eval      box325e = hx6z22
     c                   else
     c                   movel     hx6a11        box322
     c                   movel     hx6ct1        box323
     c                   movel     hx6st1        box324
     c                   movel     hx6z11        box325
     c                   eval      box325e = hx6z12
     c                   endif
      **
     c                   eval      chkprv = sndprv
     c                   eval      chkcnt = 0
      **
     c                   exsr      srctab
     c                   movel     'BDBS'        tcode
     c                   movel     hx6st1        ecode
     c                   exsr      srtabl
     c                   if        tind <> ' '
     c                   move      *on           *in57
     c                   endif
     c                   else
     c                   move      *on           *in57
     c                   endif
      **
     c     home          tag
      **
      **
      *****************************************************************
      /EJECT
     c                   movea     '00000000'    *in(01)
     c                   movea     '000'         *in(12)
     c                   movea     '00000000'    *in(21)
     c                   move      *off          *in29
     c                   move      *off          *in30
     c                   movea     '00000000'    *in(31)
     c                   move      *off          *in40
     c                   movea     '000000000'   *in(41)
     c                   movea     '00000000'    *in(51)
     c                   movea     '00'          *in(59)
     c                   movea     '00000000'    *in(62)
     c                   move      *off          *in73
     c                   movea     '0000'        *in(82)
     c                   move      *off          *in72
     c                   move      *off          *in75
      **
     c                   move      'N'           exc10                          =exception 10
     c                   move      'N'           exc212                         =exception 212
     c                   move      'N'           exc343
     c                   move      'N'           exc443
     c                   move      'N'           prtpmt
     c                   eval      exc38 = *off
     c                   eval      exc152 = *off
     c                   eval      exc401 = *off
     c                   eval      exc417 = *off
     c                   eval      exc425 = *off
     c                   eval      exc543 = *off
     c                   eval      exc682 = *off
     c                   eval      exc753 = *off
     c                   eval      exc759 = *off
     c                   eval      exc763 = *off
     c                   eval      exc833 = *off
     c                   eval      exc859 = *off
     c                   eval      exc861 = *off
     c                   eval      exc885 = *off
     c                   eval      exc910 = *off
     c                   eval      exc918 = *off
     c                   eval      exc919 = *off
     c                   eval      exc953 = *off
     c                   eval      exc959 = *off
     c                   eval      exc960 = *off
     c                   eval      exc981 = *off
     c                   eval      exc452 = *off
     c                   eval      exc983 = *off
     c                   eval      exc989 = *off
     c                   eval      exc992 = *off
     c                   eval      exc994 = *off
     c                   eval      exc996 = *off
     c                   eval      first992 = *off

      **
     c                   eval      mmmrno = 0
     c                   eval      mmgacc = 0
     c                   eval      ins# = 0
     c                   eval      rlcode = 0
     c                   eval      sbbdte = 0
     c                   eval      sb2bdt = 0
     c                   eval      injdte = 0
     c                   eval      sbsex = *blanks
     c                   eval      box01a = *blanks
     c                   eval      prgrpx = *blanks
     c                   eval      prauth = *blanks
     c                   eval      tmpfc = *blanks
     c                   eval      box19 = *blanks
     c                   eval      wrkfcl = *blanks
     c                   eval      qua33b = *blanks
      **
     c                   if        bbtype <> ' '
     c                   if        bbtran = 'A'
     c                             or bbtran = 'T'
     c                             or bbtran = 'R'
     c                   move      *on           *in82
     c                   endif
     c                   endif
      **
     c                   if        bbtype = ' '
     c                   if        bbtran = 'A'
     c                             or bbtran = 'T'
     c                             or bbtran = 'R'
     c                   move      *on           *in83
     c                   endif
     c                   endif
      **
     c                   if        bbtype = ' '
     c                             and bbtran = 'M'
     c                   move      *on           *in84
     c                   endif
      **
     c                   if        bbtype <> ' '
     c                             and bbtran = 'M'
     c                   move      *on           *in85
     c                   endif
      **
     c                   if        asgflg = 'X'
     c                   eval      asglv6 = 0
     c                   else
     c                   eval      asglv6 = bbplv6
     c                   endif
      **
      **** TYPE OF BILL
      **
     c                   eval      tobill = *blanks
     c                   if        bbbind = 9
     c                   movel     'REBILL'      tobill
     c                   endif
      **
     c     mastky        chain     hmfmast                            79
     c     *in79         cabeq     *on           enddtl
     c                   movel(p)  mmname        bgname
     c                   eval      exCat1 = mmpct1
     c                   eval      exCat2 = mmpct2
     c                   eval      savCat1 = exCat1
     c                   eval      savCat2 = exCat2
      **
      **
      ** get billing type
      **
     c                   call      'HBXBTYP'
     c                   parm                    mmplv6
     c                   parm                    mmacct
     c                   parm                    billtype
      **
     c                   if        tobill = *blanks
     c                   if        bbfrdt = mmaddt
     c                   movel     'INITIAL'     tobill
     c                   else
     c                   if        bbtodt = mmdsdt
     c                   movel     'FINAL  '     tobill
     c                   else
     c                   movel     'INTERIM'     tobill
     c                   endif
     c                   endif
     c                   endif
      **
     c                   if        billtype <> 'A'
     c                   move      bbtrak        box26
     c                   else
     c                   move      mmacct        box26
     c                   endif
      **
     c     mmplv6        chain     hxflvl6                            79
     c                   if        *in79 = *off
     c                   eval      lvl2nm = *blanks
     c                   movel     hx6nam        lvl2nm
     c                   movel     hx6nam        tmpnam
     c                   if        hx6ra1 <> *blanks
     c                   movel     hx6ra1        lvl2ad
     c                   movel     hx6rct        lvl2ct
     c                   movel     hx6rst        lvl2st
     c                   movel     hx6rzp        lvl2zp
     c                   movel     hx6rz2        lvl2z2
     c                   else
     c                   movel     hx6a11        lvl2ad
     c                   movel     hx6ct1        lvl2ct
     c                   movel     hx6st1        lvl2st
     c                   movel     hx6z11        lvl2zp
     c                   movel     hx6z12        lvl2z2
     c                   endif
     c                   eval      lvl2tl = hx6phn
     c                   endif

      ** Move below because we have not check exception codes yet
     c**                 if        exc186 = *on
     c**                 eval      payor = bbpayr
     c**                 eval      plan = bbplan
     c**                 eval      prmrev = 0
     c**                 eval      prmprc = *blanks
     c**                 eval      prmct1 = mmpct1
     c**                 eval      prmct2 = mmpct2
     c**                 eval      prvid = 'ALTAD'
     c**                 eval      prmlv6 = bbplv6
     c**                 exsr      srprv
     c**                 if        prvvar <> *blanks
     c**                 movel     prvvar        rtnaddress
     c**                 movel     rtnadr        lvl2ad
     c**                 movel     rtncty        lvl2ct
     c**                 movel     rtnst         lvl2st
     c**                 movel     rtnzp1        lvl2zp
     c**                 endif
     c**                 endif

     c**                 if        exc665 = *on
     c**                 eval      payor = bbpayr
     c**                 eval      plan = bbplan
     c**                 eval      prmrev = 0
     c**                 eval      prmprc = *blanks
     c**                 eval      prmct1 = mmpct1
     c**                 eval      prmct2 = mmpct2
     c**                 eval      prvid = 'ALTNM'
     c**                 eval      prmlv6 = bbplv6
     c**                 exsr      srprv
     c**                 if        prvvar <> *blanks
     c**                 eval      lvl2nm = prvvar
     c**                 endif
     c**                 endif
      **
     c                   movel     lvl2tl        tlds
      **
      ****  PRINT WHICH INSURANCE IS BEING BILLED AT THE TOP OF THE BILL
      **
     c                   eval      insbil = *blanks
     c                   eval      wrkrnk = 0
     c                   exsr      sractv
      **
     c                   if        bbpayr = aub(1)
     c                             and bbplan = apl(1)
     c                             and bbplcy = apo(1)
     c                   movel     prm           insbil
     c                   eval      ins# = 1
     c                   eval      ubcode = aub(1)
     c                   eval      ubplan = apl(1)
     c                   eval      ubisq  = isq(1)
     c                   movel     apo(1)        policy
     c                   else
     c                   if        bbpayr = aub(2)
     c                             and bbplan = apl(2)
     c                             and bbplcy = apo(2)
     c                   if        afc(1) = '  '
     c                   movel     prm           insbil
     c                   else
     c                   movel     sec           insbil
     c                   endif
     c                   eval      ins# = 2
     c                   eval      ubcode = aub(2)
     c                   eval      ubplan = apl(2)
     c                   eval      ubisq  = isq(2)
     c                   movel     apo(2)        policy
     c                   else
     c                   if        bbpayr = aub(3)
     c                             and bbplan = apl(3)
     c                             and bbplcy = apo(3)
     c                   if        afc(1) = '  '
     c                             and afc(2) = '  '
     c                   movel     prm           insbil
     c                   else
     c                   if        afc(1) <> '  '
     c                             and afc(2) = '  '
     c                   movel     sec           insbil
     c                   else
     c                   movel     ter           insbil
     c                   endif
     c                   endif
     c                   eval      ins# = 3
     c                   eval      ubcode = aub(3)
     c                   eval      ubplan = apl(3)
     c                   eval      ubisq  = isq(3)
     c                   movel     apo(3)        policy
     c                   endif
     c                   endif
     c                   endif
      **
     c                   eval      belong = 'N'
     c                   if        ins# = 0
     c                   eval      dattmp = bbfrdt
     c                   eval      lv6tmp = bbplv6
     c                   eval      acttmp = bbaccn
     c                   exsr      srtrfr
     c                   endif

     c                   exsr      get3ins
      **
      ****  BOX 10A - PATIENTS CONDITION RELATED TO EMPLOYMENT
      **
     c                   if        mmpcmp <> *blanks
     c                             and mmpcmp <> 'N'
     c                   eval      box10ay = 'X'
     c                   eval      box10an = ' '
     c                   else
     c                   eval      box10ay = ' '
     c                   eval      box10an = 'X'
     c                   endif

     c                   if        %found(hmpenct) and chnenc = *on
     c                   if        encomp <> *blanks
     c                             and encomp <> 'N'
     c                   eval      box10ay = 'X'
     c                   eval      box10an = ' '
     c                   else
     c                   eval      box10ay = ' '
     c                   eval      box10an = 'X'
     c                   endif
     c                   endif
      **
      **  BOX 1A - INSURED'S ID NUMBER
      **  BOX 11 - INSURED'S POLICY GROUP OR FECA NUMBER
      **  BOX 11A - INSURED'S DATE OF BIRTH AND SEX
      **  BOX 23  - PRIOR AUTHORIZATION NUMBER
      **
     c                   clear                   hafdemoi
     c     dmoiky        chain     hafdemoi                           79
     c                   if        *in79 = *off
     c                   move      bdirlc        rlcode
     c                   movel     bdigrp        prgrpx
     c                   movel     bdirsc        prresc
      **
     c                   movel     bdiply        box01a
     c                   movel     bdissx        sbsex
     c                   eval      sbbdte = bdisbd
     c                   movel     bdifcl        tmpfc
      **
     c                   if        bdirlc = '01'
     c                   if        bdibnm = *blanks
     c                   movel(p)  bdisnm        box04
     c                   else
     c                   movel(p)  bdibnm        box04
     c                   endif
      **
     c                   eval      box07a = *blanks
     c                   eval      box07b = *blanks
     c                   eval      box07c = *blanks
     c                   eval      box07d = *blanks
     c                   eval      box07e = 0
      **
     c                   if        garflg = 'X'
     c                   eval      keylv6 = 0
     c                   else
     c                   eval      keylv6 = bbplv6
     c                   endif
      **
     c     guarky        chain     hafguar                            61
     c                   if        *in61 = *off
     c                   if        bdisnm = bgname
     c                   movel(p)  'SAME'        box07a
     c                   else
     c                   movel     brgadr        box07a
     c                   movel     brgcty        box07b
     c                   movel     brgst         box07c
     c                   movel     brgzp1        box07d
     c                   eval      levl6p = keylv6
     c                   eval      reqnum = brgacc
     c                   eval      reqnm2 = 0
     c                   eval      reqsrc = 'AG'
     c                   eval      reqtyp = 'H '
     C                   exsr      srphnl
     c                   eval      box07e = rtnphn
     c                   endif
     c                   else
     c                   eval      brgenm = *blanks
     c                   eval      brgea1 = *blanks
     c                   eval      brgect = *blanks
     c                   eval      brgest = *blanks
     c                   eval      brgez1 = *blanks
     c                   endif
      **
     c                   else
     c                   if        bdibnm = *blanks
     c                   movel     bdisnm        box04
     c                   else
     c                   movel     bdibnm        box04
     c                   endif
     c                   if        bdisnm = bgname
     c                   movel(p)  'SAME'        box07a
     c                   else
     c                   movel     bdiad1        box07a
     c                   movel     bdicty        box07b
     c                   movel     bdista        box07c
     c                   movel     bdizp1        box07d
     c                   endif
     c                   endif
      **
     c                   else
     c                   eval      rlcode = 0
     c                   eval      prgrpx = *blanks
     c                   eval      prresc = *blanks
     c                   eval      box01a = *blanks
     c                   eval      sbsex = *blanks
     c                   eval      sbbdte = 0
     c                   eval      tmpfc = *blanks
     c                   eval      box04 = *blanks
     c                   eval      box07a = *blanks
     c                   eval      box07b = *blanks
     c                   eval      box07c = *blanks
     c                   eval      box07d = *blanks
     c                   eval      box07e = 0
     c                   endif
      **
     c                   eval      rqiseq = bbiseq
     c                   exsr      srcovg
     c                   if        rqtype <> 'N'
     c                             and rqeff1 <= bbtodt
     c                   movel     rqvre1        prauth
     c                   movel     rqvre1        bbauth
     c                   endif
      **
      ****  BOX 11A - SUBSCRIBER SEX
      **
     c                   eval      box11am = ' '
     c                   eval      box11af = ' '
      **
     c                   if        sbsex = 'M'
     c                   eval      box11am = 'X'
     c                   eval      box11af = ' '
     c                   endif
     c                   if        sbsex = 'F'
     c                   eval      box11am = ' '
     c                   eval      box11af = 'X'
     c                   endif

      ***** Box 11B - Other Claim ID
     c                   eval      box11b = *blanks
     c                   eval      box11bq = *blanks

      ****  BOX 14 - DATE OF CURRENT INJURY
     c                   move      'TH'          status
     c     physky        setgt     hmfasgn
     c     readagn       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c                             and msgdlt = 'D'
     c                   clear                   hmfasgn
     c                   goto      readagn
     c                   endif
      **
     c                   if        ins# = 0
     c                   eval      cntins = cntins + 1
     c                   endif
      **
     c                   if        ins# <> 0
     c                   eval      box11c = *blanks
     c                   eval      prinam = *blanks
     c                   eval      priadr = *blanks
     c                   eval      priad2 = *blanks
     c                   eval      pricty = *blanks
     c                   eval      prista = *blanks
     c                   eval      prizip = *blanks
     c                   eval      prizp2 = *blanks
     c                   eval      prprov = *blanks
     c                   eval      priatn = *blanks
      **
      ****  BOX 11C - PROVIDER NAME
      **
     c                   move      'G'           grsnet
     c     prmkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   eval      reqdat = bbtodt
     c                   exsr      srppd
     c****               eval      policy = bbplcy
     c                   eval      prtplcy = bbplcy
     c                   eval      rqiseq = bbiseq
     c                   exsr      pyradr

       setgt (bbplv6:bbaccn:rqiseq:bbtodt) haficov;
       readpe (bbplv6:bbaccn:rqiseq) haficov;
       if (not%eof(hapicov) and bcvpnm <> ' ');
         xfbnam = bcvpnm;
       endif;
     c                   movel     xfbnam        box11c
     c                   movel     xfbnam        prinam
     c                   movel     xfbadr        priadr
     c                   movel     xfbad2        priad2
     c                   movel     xfbcty        pricty
     c                   movel     xfbsta        prista
     c                   movel     xfbzip        prizip
     c                   movel     xfbzp2        prizp2
     c                   move      sndbng        grsnet
     c                   move      xfbfcl        wrkfcl
     c                   movel(p)  xfbatn        wrkatn
      **
     c                   if        priadr = *blanks
     c     prmicovkey1   setgt     haficov
     c     prmicovkey2   readpe    haficov
     c*****              if        %found(hapicov)
     c                   if        not %eof(hapicov)
     c                   movel     bcvga1        priadr
     c                   movel     bcvga2        priad2
     c                   movel     bcvgct        pricty
     c                   movel     bcvgst        prista
     c                   movel     bcvgz1        prizip
     c                   movel     bcvgz2        prizp2
     c                   endif
     c                   endif
      **
      ****  BOX BELOW FORM - NECESSARY ATTACHMENTS/REQUIREMENTS
      **
     c                   if        sndlmn = 'Y'
     c                   move      *on           *in53
     c                   endif
      **
     c                   if        sndbpn = 'Y'
     c                   move      *on           *in54
     c                   endif
      **
     c                   if        sndprb = 'Y'
     c                   move      *on           *in55
     c                   endif
      **
     c                   if        sndprr = 'Y'
     c                   move      *on           *in56
     c                   endif
      **
     c                   eval      prcct1 = ' '
     c                   eval      prcct2 = ' '
     c                   eval      theronfile = 0
     c     chgkey        setll     hbfchr15                                     =get cat 1 and 2
     c     nextcharge    tag                                                     from a charge that
     c     chgkey        reade     hbfchr15                                      will be on the bill
     c                   if        not %eof(hblchg15)
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c                   eval      action = ' '
     c                   exsr      srcgbl
     c     action        cabeq     'N'           nextcharge
     c                   endif
     c                   eval      theronfile = bgdtth
     c                   eval      prcct1 = bgdct1
     c                   eval      prcct2 = bgdct2
     c                   endif
      **
      ****  BOX 33A - NPI and BOX33B - PROVIDER NUMBER
      **
     c                   eval      box33a = *blanks
     c                   eval      prprov = *blanks
     c                   eval      chkprv = sndprv
     c                   eval      chkcnt = 0
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      qua33b = *blanks
     c**
     c     findprovid    tag
      **
     c                   select
     c*****************************************************************
     c** code D - facility NPI
     c*****************************************************************
     c                   when      chkprv = 'D'
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   if        prprov = *blanks
     c                   movel     prvvar        prprov
     c                   endif
     c                   movel     prvvar        box33a
     c                   endif
     c*****************************************************************
     c** code C - facility provider ID
     c*****************************************************************
     c                   when      chkprv = 'C'
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'PRVDR'       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   if        prprov = *blanks
     c                   movel     prvvar        prprov
     c                   endif
     c                   endif
       setgt (bbplv6:bbaccn:bbiseq:bbtodt) haficov;
       readpe (bbplv6:bbaccn:bbiseq) haficov;
       if (not%eof(hapicov) and bcvhp# <> ' ');
         prprov = bcvhp#;
       endif;

     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c*****************************************************************
     c** code U - therapist NPI
     c*****************************************************************
     c                   when      chkprv = 'U'
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        prprov = *blanks
     c                   movel     rtnlicense    prprov
     c                   endif
     c                   movel     rtnlicense    box33a
     c*****************************************************************
     c** code T - therapist provider ID
     c*****************************************************************
     c                   when      chkprv = 'T'
     c                   eval      level6 = bbplv6                              =check by
     c     thprky        chain     hmfplnpr                           79         level 6, payor/plan
     c                   if        *in79 = *off
     c                   if        prprov = *blanks
     c                   movel     hmtpr#        prprov
     c                   endif
     c                   else
     c                   eval      level6 = 0                                   =check by
     c     thprky        chain     hmfplnpr                           79         payor/plan alone
     c                   if        *in79 = *off
     c                   if        prprov = *blanks
     c                   movel     hmtpr#        prprov
     c                   endif
     c                   endif
     c                   endif
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        prprov = *blanks
     c                   movel     rtnlicense    prprov
     c                   endif
     c                   movel     rtnlicense    box33a
     c*****************************************************************
     c** code X - use tax id
     c*****************************************************************
     c                   when      chkprv = 'X'
     c                   if        prprov = *blanks
     c                   movel     taxid         prprov
     c                   endif
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c                   endsl
     c*****************************************************************
     c** if no provider variable found for method, try another
     c*****************************************************************
     c                   if        prprov = *blanks and chkcnt < 6
     c                   eval      chkcnt = chkcnt + 1
     c                   select
     c                   when      chkprv = 'D'
     c                   eval      chkprv = 'C'
     c                   goto      findprovid
     c                   when      chkprv = 'C'
     c                   eval      chkprv = 'U'
     c                   goto      findprovid
     c                   when      chkprv = 'U'
     c                   eval      chkprv = 'T'
     c                   goto      findprovid
     c                   when      chkprv = 'T'
     c                   eval      chkprv = 'X'
     c                   goto      findprovid
     c                   when      chkprv = 'X'
     c                   eval      chkprv = 'D'
     c                   goto      findprovid
     c                   endsl
     c                   endif
      **
     c                   if        prprov <> *blanks
     c     '1C'          cat       prprov:1      prprov
     c                   endif
      **
     c                   endif

        // If exception code is on and value exists for Alternate NPI, ignore
        // the contract set up and use provider variable.
     c                   if        exc417
     c                   movel     bgdtpr        prmprc
     c                   movel     'ALTNP'       prvid
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prmrev = 0
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c                   endif
      **
     c                   if        exc543 = *on
     c                   eval      prmlv6 = bbplv6
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prmrev = *zeros
     c                   eval      prmdte = bbtodt
     c                   eval      prvvar = *blanks
     c                   eval      prvid = 'PRVDR'
     c                   eval      prmprc = bgdtpr
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      box32a = prvvar
     c                   eval      box33a = prvvar
     c                   endif
     c                   endif

     c                   if        exc833 and box33a = *blanks
     c                   eval      box33a = 'ATYPICAL'
     c                   endif
      **
     c*>>                   eval      prmct1 = ' '
     c*>>                   eval      prmct2 = ' '
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
 001 c                   movel     'PRVNM'       prvid
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      lvl2nm = prvvar
     c                   endif
      **
      ****  BOX 19 - DATE LAST SEEN
      **
     c                   eval      ydate1 = bbtodt
     c                   eval      ydays = 60
     c                   exsr      srmind
     c                   eval      todate = bbtodt
     c                   eval      licnum = *blanks
     c                   eval      lic# = *blanks
     c                   eval      lstdte = *blanks
     c                   eval      lsdate = *blanks
     c     diagky        setgt     hmflsndt
     c     mastky        readpe    hmflsndt                               79
     c                   if        *in79 = *off
     c                   if        mlsdat < ydate2
     c                   else
     c                   eval      ydate = mlsdat
     c                   exsr      srcymd
     c                   movel     mdate         lsdate
     c                   endif
     c                   endif
     c                   move      'AT'          status
     c     physky        setgt     hmfasgn
     c     readag2       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c                   if        msgdlt = 'D'
     c                   clear                   hmfasgn
     c                   goto      readag2
     c                   else
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   movel     rtnlicense    licnum
     c                   else
     c                   eval      rqlictype = 'UPIN'
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   movel     rtnlicense    licnum
     c                   else
     c                   eval      licnum = *blanks
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      **
      *****  BOX 1 - GET FINANCIAL CLASS
      **
     c                   eval      box01X1 = ' '
     c                   eval      box01X2 = ' '
     c                   eval      box01X3 = ' '
     c                   eval      box01X4 = ' '
     c                   eval      box01X5 = ' '
     c                   eval      box01X6 = ' '
      **
     c**** bbpayr        chain     xffinsd                            79
     c     prmkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c****               eval      fincls = xfifcl
     c                   eval      fincls = xfbfcl
     c                   exsr      srctab
     c                   movel     'BFCL'        tcode
     c****               movel     xfifcl        ecode
     c                   movel     xfbfcl        ecode
     c                   exsr      srtabl
     c                   move      hmap          hcsfc
     c                   if        hcsfc = 'MC'
     c                   eval      box01X1 = 'X'
     c                   else
     c****               if        xfifcl = 'MD'
     c                   if        xfbfcl = 'MD'
     c                   eval      box01X2 = 'X'
     c                   else
     c                   eval      box01X3 = 'X'
     c                   endif
     c                   endif
     c                   endif
      **
     c     demoky        chain     hafdemo                            61
     c                   if        *in61 = *on
     c                   eval      bdpbdt = 0
     c                   eval      bdpsex = *blanks
     c                   eval      bdpmst = *blanks
     c                   eval      bdpad1 = *blanks
     c                   eval      bdpcty = *blanks
     c                   eval      bdpsta = *blanks
     c                   eval      bdpzp1 = *blanks
     c                   eval      bdphtl = 0
     c                   eval      bdpss# = 0
     c                   endif
     c                   movel     bdpad1        resad1
     c                   movel     bdpcty        rescty
      **
      ****  BOX 7 - INSURED'S ADDRESS INFORMATION
      **
      **
      ****  BOX 3A - PATIENT BIRTH DATE
      **
     c                   eval      ydate = bdpbdt
     c                   exsr      srcymd
     c                   eval      prbirt = mdate
      **
      ****  BOX 3B - PATIENT SEX
      **
     c                   if        bdpsex = 'M'
     c                   eval      box03m = 'X'
     c                   eval      box03f = ' '
     c                   else
     c                   if        bdpsex = 'F'
     c                   eval      box03m = ' '
     c                   eval      box03f = 'X'
     c                   endif
     c                   endif
      **
      ****  BOX 6 - PATIENT RELATIONSHIP TO INSURED
      **
     c                   eval      box06a = ' '
     c                   eval      box06b = ' '
     c                   eval      box06c = ' '
     c                   eval      box06d = ' '
      **
     c                   exsr      srctab
     c                   movel     'BREL'        tcode
     c                   movel     rlcode        ecode
     c                   exsr      srtabl
     c                   eval      relcode = hmap
     c                   eval      rlcode = 0
     c                   move      relchar       rlcode
     c***                movel     hmap          rlcode
      **
     c                   if        rlcode = 01                                  =self
     c***                if        rlcode = 18                                  =self
     c                   eval      box06a = 'X'
     c                   else
     c                   if        rlcode = 02                                  =spouse
     c***                if        rlcode = 01                                  =spouse
     c                   eval      box06b = 'X'
     c                   else
     c                   if        rlcode = 03                                  =child
     c***                if        rlcode = 32 or rlcode = 33                   =parent
     c                   eval      box06c = 'X'
     c                   else
     c                   eval      box06d = 'X'                                 =other
     c                   endif
     c                   endif
     c                   endif
      **
      ****  BOX 5C - PRINT ZIP CODE
      **
     c                   if        bdpzp1 <> *blanks
     c                   move      *on           *in23
     c                   endif
      **
      ****  BOX 9 - OTHER INSURED'S NAME
      ****  BOX 9A - OTHER INSURED'S POLICY OR GROUP NUMBER
      ****  BOX 9B - OTHER INSURED'S DATE OF BIRTH
      ****  BOX 11D - IS THERE ANOTHER HEALTH BENEFIT PLAN
      **
     c****               eval      prinm2 = *blanks
     c****               eval      prinn2 = *blanks
     c****               eval      prtpol = *blanks
      ****
     c****               select
     c****               when      ins# = 1
     c**
     c****               if        afc(2) <> '  '
     c****                         and afc(2) <> 'SP'
     c****               eval      ubcode = aub(2)
     c****               eval      ubplan = apl(2)
     c****               eval      ubisq  = isq(2)
     c**** scdkey        chain     xffbnfit                           79
     c****               if        %found(hxpbnfit)
 001 c****               movel(p)  xfbnam        prinm2
     c****               move      *on           *in24
     c****               eval      box11dn = ' '
     c****               eval      box11dy = 'X'
     c****               movel     apo(2)        prinn2
     c****               movel     apo(2)        policy
     c****               clear                   hafdemoi
     c****               eval      ubcode = aub(2)
     c****               eval      ubplan = apl(2)
     c****               eval      ubisq  = isq(2)
     c**** dmoiky        chain     hafdemoi                           79
     c****               if        bdibnm = *blanks
     c****               movel     bdisnm        prisd2
     c****               else
     c****               movel     bdibnm        prisd2
     c****               endif
     c****               eval      sb2bdt = bdisbd
     c****               movel     bdissx        sb2sex
     c****               endif
     c****               endif
     c****               if        afc(3) <> '  '
     c****                         and afc(3) <> 'SP'
     c****                         and *in24 = *off
     c****               eval      ubcode = aub(3)
     c****               eval      ubplan = apl(3)
     c****               eval      ubisq  = isq(3)
     c**** scdkey        chain     xffbnfit                           79
     c****               if        %found(hxpbnfit)
 001 c****               movel(p)  xfbnam        prinm2
     c****               move      *on           *in24
     c****               eval      box11dn = ' '
     c****               eval      box11dy = 'X'
     c****               movel     apo(3)        prinn2
     c****               movel     apo(3)        policy
     c****               clear                   hafdemoi
     c****               eval      ubcode = aub(3)
     c****               eval      ubplan = apl(3)
     c****               eval      ubisq  = isq(3)
     c**** dmoiky        chain     hafdemoi                           79
     c****               if        bdibnm = *blanks
     c****               movel     bdisnm        prisd2
     c****               else
     c****               movel     bdibnm        prisd2
     c****               endif
     c****               eval      sb2bdt = bdisbd
     c****               movel     bdissx        sb2sex
     c****               endif
     c****               endif
      **
     c****               when      ins# = 2
      **
     c****               if        afc(3) <> '  '
     c****                         and afc(3) <> 'SP'
     c****               eval      ubcode = aub(3)
     c****               eval      ubplan = apl(3)
     c****               eval      ubisq  = isq(3)
     c**** scdkey        chain     xffbnfit                           79
     c****               if        %found(hxpbnfit)
 001 c****               movel(p)  xfbnam        prinm2
     c****               move      *on           *in24
     c****               eval      box11dn = ' '
     c****               eval      box11dy = 'X'
     c****               movel     apo(3)        prinn2
     c****               movel     apo(3)        policy
     c****               clear                   hafdemoi
     c****               eval      ubcode = aub(3)
     c****               eval      ubplan = apl(3)
     c****               eval      ubisq  = isq(3)
     c**** dmoiky        chain     hafdemoi                           79
     c****               if        bdibnm = *blanks
     c****               movel     bdisnm        prisd2
     c****               else
     c****               movel     bdibnm        prisd2
     c****               endif
     c****               eval      sb2bdt = bdisbd
     c****               movel     bdissx        sb2sex
     c****               endif
     c****               endif
     c**
     c****               endsl
      /free
       // Get the proper "other" insurance info
       box11dn = 'X';
       box11dy = ' ';
       prinm2 = '';
       prinn2 = '';
       prtpol = '';
       prisd2 = '';
       otherInsNum = 0;
       if     afc(1) <> ''
          and afc(1) <> 'SP'
          and ins# <> 1;
          otherInsNum = 1;
       elseif afc(2) <> ''
          and afc(2) <> 'SP'
          and ins# <> 2;
          otherInsNum = 2;
       elseif afc(3) <> ''
          and afc(3) <> 'SP'
          and ins# <> 3;
          otherInsNum = 3;
       endif;
       // If an "other" insurance was found, retrieve its info
       if otherInsNum <> 0;
         ubcode = aub(otherInsNum);
         ubplan = apl(otherInsNum);
         ubisq  = isq(otherInsNum);
         prinn2 = apo(otherInsNum);
         policy = apo(otherInsNum);
         chain scdkey xffbnfit;
         if %found(hxpbnfit);
           prinm2 = xfbnam;
           *in24 = *on;
           box11dn = ' ';
           box11dy = 'X';
           clear hafdemoi;
           chain dmoiky hafdemoi;
           if bdibnm = *blanks;
             prisd2 = bdisnm;
           else;
             prisd2 = bdibnm;
           endif;
           sb2bdt = bdisbd;
           sb2sex = bdissx;
         endif;
       endif;
      /end-free
      **
      ****  BOX 10B - PATIENTS CONDITION RELATED TO AUTO ACCIDENT
      **
     c                   if        mmpmva <> *blanks
     c                             and mmpmva <> 'N'
     c                   eval      box10bs = mmpmvs
     c                   eval      box10by = 'X'
     c                   eval      box10bn = ' '
     c                   else
     c                   eval      box10bs = '  '
     c                   eval      box10by = ' '
     c                   eval      box10bn = 'X'
     c                   endif

     c                   if        %found(hmpenct) and chnenc = *on
     c                   if        enmvaf <> *blanks
     c                             and enmvaf <> 'N'
     c                   eval      box10bs = enmvas
     c                   eval      box10by = 'X'
     c                   eval      box10bn = ' '
     c                   else
     c                   eval      box10bs = '  '
     c                   eval      box10by = ' '
     c                   eval      box10bn = 'X'
     c                   endif
     c                   endif
      **
      ****  BOX 10C - PATIENTS CONDITION RELATED TO OTHER ACCIDENT
      **
     c                   if        mmpoac <> *blanks
     c                             and mmpoac <> 'N'
     c                   eval      box10cy = 'X'
     c                   eval      box10cn = ' '
     c                   else
     c                   eval      box10cy = ' '
     c                   eval      box10cn = 'X'
     c                   endif

     c                   if        %found(hmpenct) and chnenc = *on
     c                   if        enoacf <> *blanks
     c                             and enoacf <> 'N'
     c                   eval      box10cy = 'X'
     c                   eval      box10cn = ' '
     c                   else
     c                   eval      box10cy = ' '
     c                   eval      box10cn = 'X'
     c                   endif
     c                   endif
      **
      ****  BOX 17 - NAME OF REFERRING PHYSICIAN
      ****  BOX 17A - ID NUMBER OF REFERRING PHYSICIAN
      **
 004 c                   move      *blanks       prrupn
 004 c                   move      *blanks       prrnpi
 003 c                   movel     *blanks       prrphy
     c                   clear                   hmfasgn
     c                   move      'RF'          status
     c     physky        setgt     hmfasgn
     c     tryagn        tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c     msgdlt        cabeq     'D'           tryagn
     c     msgdr#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      box17q = 'DN'
     c                   movel     hmdnam        prrphy
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   movel     rtnlicense    prrnpi
     c                   else
     c                   eval      rqlictype = 'UPIN'
     c                   exsr      srlicense
     c                   movel     rtnlicense    prrupn
     c                   endif
     c                   endif
     c                   endif

     c                   if        %found(hmpenct) and enrefp <> 0
     c                             and chnenc = *on
 004 c                   move      *blanks       prrupn
 004 c                   move      *blanks       prrnpi
 003 c                   movel     *blanks       prrphy
     c     enrefp        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      box17q = 'DN'
     c                   movel     hmdnam        prrphy
     c                   eval      rqdoctor# = enrefp
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   movel     rtnlicense    prrnpi
     c                   else
     c                   eval      rqlictype = 'UPIN'
     c                   exsr      srlicense
     c                   movel     rtnlicense    prrupn
     c                   endif
     c                   endif
     c                   endif
      **
      ****  BOX 18A and 18B - hospitalization dates
      **


     c                   eval      dtf18 = *blanks
     c                   eval      dtf618 = *blanks
     c                   eval      dtfm18 = *blanks
     c                   eval      dtfd18 = *blanks
     c                   eval      dtfy18 = *blanks
     c                   eval      dtt18 = *blanks
     c                   eval      dtt618 = *blanks
     c                   eval      dttm18 = *blanks
     c                   eval      dttd18 = *blanks
     c                   eval      dtty18 = *blanks
      **
     c                   eval      ydate = mmaddt
     c                   exsr      srcymd
     c                   eval      box18a = mdate
      **
     c                   if        mdate <> 0
     c                   move      mdate         dtf18
     c                   z-add     mmaddt        mmdd6
     c                   move      dtf18         yy6
     c                   move      wkdat6        dtf618
     c                   move      mm6           dtfm18
     c                   move      dd6           dtfd18
     c                   move      yy6           dtfy18
     c                   endif
      **
     c                   eval      ydate = mmdsdt
     c                   exsr      srcymd
     c                   eval      box18b = mdate
      **
     c                   if        mdate <> 0
     c                   move      mdate         dtt18
     c                   z-add     mmdsdt        mmdd6
     c                   move      dtt18         yy6
     c                   move      wkdat6        dtt618
     c                   move      mm6           dttm18
     c                   move      dd6           dttd18
     c                   move      yy6           dtty18
     c                   endif
      **
     c                   if        %found(hmpenct) and chnenc = *on
     c                   eval      ydate = enpadd
     c                   exsr      srcymd
     c                   eval      box18a = mdate
      **
     c                   if        mdate <> 0
     c                   move      mdate         dtf18
     c                   z-add     enpadd        mmdd6
     c                   move      dtf18         yy6
     c                   move      wkdat6        dtf618
     c                   move      mm6           dtfm18
     c                   move      dd6           dtfd18
     c                   move      yy6           dtfy18
     c                   endif
      **
     c                   eval      ydate = enpdsd
     c                   exsr      srcymd
     c                   eval      box18b = mdate
      **
     c                   if        mdate <> 0
     c                   move      mdate         dtt18
     c                   z-add     enpdsd        mmdd6
     c                   move      dtt18         yy6
     c                   move      wkdat6        dtt618
     c                   move      mm6           dttm18
     c                   move      dd6           dttd18
     c                   move      yy6           dtty18
     c                   endif
     c                   endif

      **
      ****  BOX 12A - PATIENT SIGNATURE
      **
     c                   eval      box12 = sign
      **
      ****  BOX 12B - SIGNATURE DATE
      **
     c                   eval      ydate = mmaddt
     c                   exsr      srcymd
     c                   eval      praddt = mdate
     c                   eval      pradt6= %int(%char(%date(praddt:*usa):*mdy0))
      **
      ****  BOX 13 - INSURED SIGNATURE
      **
     c                   eval      box13 = sign

      *****  Box 15 - Other date of current injury/LMP - Currently on IN28, will always print blanks
      *                until it is needed (which will probably be never)
     c                   eval      *in28 = *off
     c                   eval      box15dte = 0
     c                   eval      box15q = *blanks

      ****  BOX 22 - Resubmission code and Original ref No. (Set to blanks for now)
     c                   eval      box22a = *blanks
     c                   eval      box22b = *blanks
      ** Fill in Original Reference Number if we have it
      ** Box 22
 001 c                   eval      icndcn = *blanks
     c                   if        bbtorb = '6' or bbtorb = '7' or bbtorb = '8'

 001 c                   call      'HBXICN'
 001 c                   parm                    bbplv6
 001 c                   parm                    bbaccn
 001 c                   parm                    bbfrdt
 001 c                   parm                    bbtodt
 001 c                   parm                    bbpayr
 001 c                   parm                    bbplan
 001 c                   parm                    bbplcy
 001 c                   parm                    bbiseq
 001 c                   parm                    bbtrak
 001 c                   parm                    icndcn

B002 c                   if        icndcn = *blanks
     c                             and bbtcn <> *blanks
     c                   movel     bbtcn         icndcn
     c                   endif
     c                   if        icndcn <> *blanks
     c                   movel     icndcn        box22b
     c                   endif
     c                   endif
      **
      ** BOX 25E - EIN FLAG
      **
     c                   eval      box25e = 'X'
      **
      ** BOX 27 - ACCEPT ASSIGNMENT
      **
     c                   eval      box27y = 'X'
     c                   eval      box27n = ' '
      **
      ** BOX 31 - PRIMARY THERAPIST NAME
      **
     c                   eval      prthnm = 'SIGNATURE ON FILE'
      **
     c                   clear                   hmfasgn
     c                   move      'TH'          status
     c     physky        setgt     hmfasgn
     c     tryagn2       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c     msgdlt        cabeq     'D'           tryagn2
     c     msgdr#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   movel     hmdnam        prthnm
     c                   movel     hmdtyp        prttyp
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'STATE'
     c                   eval      rqstate = hx6st1
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    prrg#
     c                   movel     rtntherapist  prtcod
     c                   movel     rtncat2       thrct2
     c                   endif
     c                   endif
      **
     c                   eval      wrkpyr = bbpayr
     c                   eval      wrkpln = bbplan
     c                   eval      wklvl6 = bbplv6
      **
     c                   eval      prmlv6 = bbplv6
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = '1510D'
     c                   eval      prmprc = *blanks
     c                   eval      prmrev = *zeros
     c                   eval      prvvar = *blanks
     c                   eval      prmprc = *blanks
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      wklvl6 = 0
     c                   eval      box10d = prvvar
     c                   endif
      *
     c                   eval      dtline = 6
      **
     c                   eval      exc04 = *off
     c                   eval      exc05 = *off
     c                   eval      exc12 = *off
     c                   eval      exc77 = *off
     c                   eval      exc176 = *off
     c                   eval      exc186 = *off
     c                   eval      exc189 = *off
     c                   eval      exc196 = *off
     c                   eval      exc224 = *off
     c                   eval      exc549 = *off
     c                   eval      exc665 = *off
     c                   eval      exc714 = *off
     c                   eval      exc837 = *off
     c                   eval      exc964 = *off
     c                   eval      exc935 = *off
     c                   eval      exc978 = *off
     c                   eval      exc979 = *off
     c                   eval      exc992 = *off
      **
     c                   eval      newrev = 0
     c     bbtrak        setll     hbfchgbl
     c                   dou       %eof
     c     bbtrak        reade     hbfchgbl
     c                   if        not %eof
     c     chgkeysq      chain     hbfchrg
     c                   if        %found
     c                             and bgctqt <> 0
     c                   eval      bgdlv6 = bgclv6
     c                   eval      bgdtpr = bgctpr
     c     procky        chain     xffprocc
     c                   eval      newrev = xfpubc
     c                   leave
     c                   endif
     c                   endif
     c                   enddo
      **
     c                   eval      hld24k = 0
     c                   move      'F'           formtp
     c     exccod        setll     hbfbexc
     c                   dou       *in71 = *on
     c     exccod        reade     hbfbexc                                71
     c                   if        *in71 = *off
     c                             and exdelt <> 'X'
      **
     c                   movel     hx6st1        exstat
     c                   eval      exlvl6 = bbplv6
     c                   eval      expayr = bbpayr
     c                   eval      explan = bbplan
     c                   movel     tmpfc         exfc
     c                   eval      exrevc = newrev
     c                   eval      exCat1 = savCat1
     c                   eval      exCat2 = savCat2
     c                   exsr      bilexc
      **
     c                   endif
     c                   enddo
      **
     c                   if        exc196 = *off
     c                   exsr      src2aut
     c                   if        reqtra <> *blanks
     c                   eval      prauth = *blanks
     c                   movel     reqtra        prauth
     c                   eval      bbauth = *blanks
     c                   movel     reqtra        bbauth
     c                   endif
     c                   else
     c                   movel     prauth        bbauth
     c                   endif
      **
     c                   if        exc682 = *on
     c                             or exc753 = *on
     c                             or exc953 = *on
     c                   exsr      getdates                                     =it gets more than
     c                   endif                                                   just dates
      **
     **********
     ** box32a
     **********
     c                   if        exc77 = *off  and exc425 = *off
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box32a
     c                   endif
     c                   endif

        // Box 32 - Address
     c                   if        exc919 = *on
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'ALTA2'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      rtnaddress = *blanks
     c                   movel     prvvar        rtnaddress
     c                   movel     rtnadr        box322
     c                   movel     rtncty        box323
     c                   movel     rtnst         box324
     c                   movel     rtnzp1        box325
     c                   movel     rtnzp2        box325e
     c                   endif
     c                   endif

        // Box 33 - Address
     c                   if        exc186 = *on
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'ALTAD'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   movel     prvvar        rtnaddress
     c                   movel     rtnadr        lvl2ad
     c                   movel     rtncty        lvl2ct
     c                   movel     rtnst         lvl2st
     c                   movel     rtnzp1        lvl2zp
     c                   movel     rtnzp2        lvl2z2
     c                   endif
     c                   endif

        // Box 33 - Billing Address
     c                   if        exc189 = *on
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prmlv6 = bbplv6
     c                   eval      prvid = 'ALTBN'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   movel     prvvar        rtnaddress
     c*                  movel     rtnadr        lvl2ad
                         lvl2ad = %trim(rtnadr) + ' ' + %trim(rtnad2);
     c                   movel     rtncty        lvl2ct
     c                   movel     rtnst         lvl2st
     c                   movel     rtnzp1        lvl2zp
     c                   movel     rtnzp2        lvl2z2
     c                   endif
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prmlv6 = bbplv6
     c                   eval      prvid = 'ALTBP'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   movel     prvvar        lvl2tl
     c                   endif

     c                   eval      prmlv6 = bbplv6
     c                   eval      prmpyr = bbpayr
     c                   eval      prmpln = bbplan
     c                   eval      prvid  = 'APVNM'
     c                   eval      prmdte = bbtodt
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   movel     prvvar        lvl2nm
     c                   endif

     c                   endif

     c                   if        exc543 = *on
     c                   eval      prmlv6 = bbplv6
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prmrev = *zeros
     c                   eval      prmdte = bbtodt
     c                   eval      prvvar = *blanks
     c                   eval      prvid = 'PRVDR'
     c                   eval      prmprc = bgdtpr
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      box32a = prvvar
     c                   eval      box33a = prvvar
     c                   endif
     c                   endif

     c                   if        exc833 and box33a = *blanks
     c                   eval      box33a = 'ATYPICAL'
     c                   endif

        // Box 32 - Facility Address
     c                   if        exc964 = *on
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'ALTFA'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   movel     prvvar        rtnaddress
     c                   movel     rtnadr        box322
     c                   movel     rtncty        box323
     c                   movel     rtnst         box324
     c                   movel     rtnzp1        box325
     c                   movel     rtnzp2        box325e
     c                   endif
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'ALTFN'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      box321 = prvvar
     c                   endif
     c                   endif

        // Box 32B - Taxonomy Code
     c                   if        exc549 = *on
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = bgdtpr
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'TAXON'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm                    prvid
     c                   parm                    qua32b
     c                   eval      box32b = qua32b + ' ' + prvvar
     c                   else
     c                   eval      box32b = *blanks
     c                   eval      qua32b = *blanks
     c                   endif
     c                   endif

     c                   if        exc665 = *on
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'ALTNM'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
       //-- Code 918 - Do not affect facility name in box 33
     c                   if        not exc918
     c                   eval      lvl2nm = prvvar
     c                   endif
     c                   eval      box321 = prvvar
     c                   endif
     c                   endif
      **
      ** Box 33B
      **
     c                   if        exc859 = *on
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   eval      prvid = 'TAXON'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   movel     prvvar        prtprv
     c                   endif

      *****************************************************************
      /EJECT
      *****************************************************************
      **                    SET UP 1500 BILL DETAIL                  **
      *****************************************************************
     c                   eval      dtf = *blanks
     c                   eval      dtt = *blanks
     c                   eval      dtf6 = *blanks
     c                   eval      dtt6 = *blanks
     c                   eval      dttm = *blanks
     c                   eval      dtfm = *blanks
     c                   eval      dttd = *blanks
     c                   eval      dtfd = *blanks
     c                   eval      dtty = *blanks
     c                   eval      dtfy = *blanks
     c                   eval      pbf = 0
     c                   eval      pbt = 0
     c                   eval      pos = *blanks
     c                   eval      eps = *blanks
     c                   eval      cpt = *blanks
     c                   eval      ddx = *blanks
     c                   eval      mod = *blanks
     c                   eval      cds = *blanks
     c                   eval      pdsc = *blanks
     c                   eval      amt = 0
     c                   eval      amta = 0
     c                   eval      qty = 0
     c                   eval      qty4 = 0
     c                   eval      qty2 = 0
     c                   eval      qty5 = 0
     c                   eval      dix = *blanks
     c                   eval      dxv = *blanks
     c                   eval      pin = *blanks
     c                   eval      bx24i = *blanks
     c                   eval      npi = *blanks
     c                   eval      tot = 0
     c                   eval      svdiag = *blanks
     c                   eval      savdte = 0
     c                   eval      savrev = 0
     c                   eval      savprc = *blanks
     c                   eval      svproc = *blanks
     c                   eval      z = 0
     c                   eval      totchg = 0
     c                   eval      totpmt = 0
     c                   eval      baldue = 0
     c                   eval      conalw = 0
     c                   move      '0'           onetrk

     c*****              exsr      srsetupbox21
      **
      ** check bill exception 992 before hand
     c                   move      'D'           formtp
     c                   eval      excode = 992
     c     exccod2       chain     hbfbexc
     c                   if        %found(hblbexct) and exdelt <> 'X'
     c                   movel     hx6st1        exstat
     c                   eval      exlvl6 = bbplv6
     c                   eval      expayr = bbpayr
     c                   eval      explan = bbplan
     c                   movel     tmpfc         exfc
     c                   eval      exrevc = newrev
     c                   eval      exCat1 = savCat1
     c                   eval      exCat2 = savCat2
     c                   exsr      bilexc
     c                   eval      first992 = *on
     c                   endif

     c                   eval      bbamnt = 0
     c                   eval      nobill = 'X'
     c                   eval      first = ' '
      *** Get diagnosis codes
     c                   call      'XFXDIAG'
     c                   parm      bbplv6        lv6#
     c                   parm      bbaccn        acct#
     c                   parm      bbtodt        rdate
     c                   parm                    diag
     c                   parm                    poaf
     c                   parm      *blanks       bodyp
     c                   parm                    icdVer
     c                   exsr      srsetupbox21
      **
     c                   eval      bbamnt = 0
     c                   eval      nobill = 'X'
     c                   eval      first = ' '
     c                   eval      counter = 0
     c                   eval      chr15new = 0
      **
     c                   if        exc212 = 'Y'
     c     chgkey        setll     hbfwkmod
     c                   else
     c     chgkey        setll     hbfchr15
     c                   endif
      **
     c                   dou       *in70 = *on
     c     nxtrd         tag
     c                   if        exc212 = 'Y'
     c     chgkey        reade     hbfwkmod                               70
     c                   if        *in70 = *off
     c     wkmdky        chain     hbfchgmm                           70
     c                   delete    hbfwkmod
     c                   endif
     c                   else
     c     chgkey        reade     hbfchr15                               70
     c                   endif
     c                   if        *in70 = *off
     c                             and bgdtsd >= bbfrdt
     c                             and bgdtsd <= bbtodt
      **
     c     nxtpag        tag
     c                   eval      action = *blanks
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c                   exsr      srcgbl
     c     action        cabeq     'N'           nxtrd
     c                   endif
      **
     c                   movel     bgdicd        svdiag
      **
     c                   select
     c                   when      bbiseq = bgdsq1
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbu1        cabeq     ' '           nxtrd
     c                   endif
     c                   if        bbpayr <> bgdub1
     c                             or bbplan <> bgdpl1
     c                             or bbplcy <> bgdpo1
     c                   if        belong = 'N'
     c                   goto      nxtrd
     c                   endif
     c                   endif
     c                   eval      wrkfld = 0
     c                   if        *in69 = *on
     c                             and bbbind = 9
     c                   eval      wrkfld = bgdci1 + bgdcp1
     c                   eval      wrkfld = wrkfld + bgddm1
     c                   if        bgdgp1 = 0
     c                             and wrkfld = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   endif
     c                   endif
     c                   exsr      srctab
     c                   movel     'BFCL'        tcode
     c                   movel     bgdfc1        ecode
     c                   exsr      srtabl
     c                   move      hmap          hcsfc
     c                   if        hcsfc = 'MC'
     c                             and bgdgp1 = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   else
     c                   if        bgdfc1 = 'MD'
     c                             and bgdgp1 = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   else
     c                   endif
     c                   endif
     c                   when      bbiseq = bgdsq2
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbu2        cabeq     ' '           nxtrd
     c                   endif
     c                   if        bbpayr <> bgdub2
     c                             or bbplan <> bgdpl2
     c                             or bbplcy <> bgdpo2
     c                   if        belong = 'N'
     c                   goto      nxtrd
     c                   endif
     c                   endif
     c                   eval      wrkfld = 0
     c                   if        *in69 = *on
     c                             and bbbind = 9
     c                   eval      wrkfld = bgdci2 + bgdcp2
     c                   eval      wrkfld = wrkfld + bgddm2
     c                   if        bgdgp2 = 0
     c                             and wrkfld = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   endif
     c                   endif
     c                   exsr      srctab
     c                   movel     'BFCL'        tcode
     c                   movel     bgdfc2        ecode
     c                   exsr      srtabl
     c                   move      hmap          hcsfc
     c                   if        hcsfc = 'MC'
     c                             and bgdgp2 <= 0
     c                             and bgdap2 <= 0
     c                             and bgdci2 <= 0
     c                             and bgdpp2 <= 0
     c                             and bgdcp2 <= 0
     c                             and bgddm2 <= 0
     c                             and bgdaj2 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   else
     c                   if        bgdfc2 = 'MD'
     c                             and bgdgp2 <= 0
     c                             and bgdap2 <= 0
     c                             and bgdci2 <= 0
     c                             and bgdpp2 <= 0
     c                             and bgdcp2 <= 0
     c                             and bgddm2 <= 0
     c                             and bgdaj2 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   else
     c                   endif
     c                   endif
     c                   when      bbiseq = bgdsq3
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbu3        cabeq     ' '           nxtrd
     c                   endif
     c                   if        bbpayr <> bgdub3
     c                             or bbplan <> bgdpl3
     c                             or bbplcy <> bgdpo3
     c                   if        belong = 'N'
     c                   goto      nxtrd
     c                   endif
     c                   endif
     c                   eval      wrkfld = 0
     c                   if        *in69 = *on
     c                             and bbbind = 9
     c                   eval      wrkfld = bgdci3 + bgdcp3
     c                   eval      wrkfld = wrkfld + bgddm3
     c                   if        bgdgp3 = 0
     c                             and wrkfld = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   endif
     c                   endif
     c                   exsr      srctab
     c                   movel     'BFCL'        tcode
     c                   movel     bgdfc3        ecode
     c                   exsr      srtabl
     c                   move      hmap          hcsfc
     c                   if        hcsfc = 'MC'
     c                             and bgdgp3 <= 0
     c                             and bgdap3 <= 0
     c                             and bgdci3 <= 0
     c                             and bgdpp3 <= 0
     c                             and bgdcp3 <= 0
     c                             and bgddm3 <= 0
     c                             and bgdaj3 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   else
     c                   if        bgdfc3 = 'MD'
     c                             and bgdgp3 <= 0
     c                             and bgdap3 <= 0
     c                             and bgdci3 <= 0
     c                             and bgdpp3 <= 0
     c                             and bgdcp3 <= 0
     c                             and bgddm3 <= 0
     c                             and bgdaj3 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd
     c                   endif
     c                   endif
     c                   when      bbiseq = 1
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbup        cabeq     ' '           nxtrd
     c                   endif
     c                   other
     c                   goto      nxtrd
     c                   endsl
      **
     c                   exsr      src2aut
     c                   if        reqtra <> *blanks
     c                   eval      prauth = *blanks
     c                   movel     reqtra        prauth
     c                   eval      bbauth = *blanks
     c                   movel     reqtra        bbauth
     c                   endif
      **
     c                   exsr      srasaut
     c                   if        reqtra <> *blanks
     c                   eval      prauth = *blanks
     c                   movel     reqtra        prauth
     c                   eval      bbauth = *blanks
     c                   movel     reqtra        bbauth
     c                   endif
      **
     c                   eval      nobill = ' '
     c                   select
     c                   when      bbiseq = bgdsq1
     c                   eval      tot(1) = tot(1) + bgdnp1
     c                   when      bbiseq = bgdsq2
     c                   eval      tot(2) = tot(2) + bgdnp2
     c                   when      bbiseq = bgdsq3
     c                   eval      tot(3) = tot(3) + bgdnp3
     c                   endsl
      **
     c                   eval      chPRsp = bgdNpt - bgdPpt
     c                   if        bgdtsd < mmaddt
     c                   eval      bgdtsd = mmaddt
     c                   else
     c                   if        mmdsdt > 0
     c                             and bgdtsd > mmdsdt
     c                   eval      bgdtsd = mmdsdt
     c                   endif
     c                   endif

     c****               eval      counter += 1
     c****               eval      bbcont = counter
     c
     c                   if        BGDTC4 = *blanks
     c                   eval      BGDTC4 = bgdtpr
     c                   endif
     c
     c                   if        enepsd <> ' '
     c                   eval      bgdfmp = enepsd
     c                   else
     c                   eval      bgdfmp = enfamp
     c                   endif
      **
        // Exception Code 910
        if exc910 and bgdtam = 0;
          chain procky xffprocc;
          if not %found(hxpprocc);
            chain bgdtpr xffprocm;
          ENDIF;

          if %found and xfpcpt <> *blanks;
            setgt (xfpcpt : bbtodt) hbfcpt4;
            readpe xfpcpt hbfcpt4;
            if not %eof(hbpcpt4) and %subst(bcfill:3:1) <> *blanks;
              bgdtam = 0.01;

              select;
              when ins# = 1 and ubisq = bgdsq1;
                bgdgp1 = bgdtam;
                bgdap1 = -bgdtam;
              when ins# = 2 and ubisq = bgdsq2;
                bgdgp2 = bgdtam;
                bgdap2 = -bgdtam;
              when ins# = 3 and ubisq = bgdsq3;
                bgdgp3 = bgdtam;
                bgdap3 = -bgdtam;
              ENDSL;

            ENDIF;
          ENDIF;
        ENDIF;
      **
     c****               if        *in37 = *on
     c****               if        bgdtsd = savdte
     c****                         and bgdtpr = savprc
     c****               if        bgdtqt <> 0
     c****               eval      qty(z) = qty(z) + bgdtqt
     c****               eval      qty4(z) = qty4(z) + bgdtqt
     c****               else
     c****               eval      qty(z) = qty(z) + 1
     c****               eval      qty4(z) = qty4(z) + 1
     c****               endif
     c****               eval      amt(z) = amt(z) + bgdtam
     c****               eval      amta(z) = amta(z) + bgdtam
     c****               eval      bbamnt = bbamnt + bgdtam
     c****               eval      totchg = totchg + bgdtam
     c****               eval      bilchg = bilchg + bgdtam
     c****               eval      rptchg = rptchg + bgdtam
     c****               eval      lv6chg = lv6chg + bgdtam
     c****               eval      paychg = paychg + bgdtam
     c****               eval      plnchg = plnchg + bgdtam
     c****               eval      accchg = accchg + bgdtam
      **
      ** Check for exception 214 specifically, will set quantity to 1 if on
      **
     c****               eval      newrev = 0
     c**** procky        chain     xffprocc
     c****               eval      newrev = xfpubc
      **
     c****               eval      hld24k = 0
     c****               eval      rqdoctor# = 0
     c****               eval      excode = 214
     c**** excode        chain     hbfbexc1
     c****               if        %found(hbpbexc)
     c****                         and exdelt <> 'X'
     c****               movel     hx6st1        exstat
     c****               eval      exlvl6 = bbplv6
     c****               eval      expayr = bbpayr
     c****               eval      explan = bbplan
     c****               movel     tmpfc         exfc
     c****               eval      exrevc = newrev
     c****               exsr      bilexc
     c****               endif
      **
     c****               goto      nextd
     c****               endif
     c****               endif
      **
     c                   exsr      clrdetl
     c                   exsr      fillpointer

     c                   eval      ydate = bgdtsd
     c                   exsr      srcymd
     c                   move      mdate         dtf(z)
     c                   eval      pbf(z) = mdate
      **
     c                   z-add     bgdtsd        mmdd6
     c                   move      dtf(z)        yy6
     c                   move      wkdat6        dtf6(z)
     c                   move      mm6           dtfm(z)
     c                   move      dd6           dtfd(z)
     c                   move      yy6           dtfy(z)
      **
     c                   eval      dtt(z) = *blanks
     c                   eval      pbt(z) = 0
     c                   eval      dtt6(z) = *blanks
     c                   eval      dttm(z) = *blanks
     c                   eval      dttd(z) = *blanks
     c                   eval      dtty(z) = *blanks
      **
     c                   eval      ydate = bgdfsd
     c                   exsr      srcymd
     c                   move      mdate         dtt(z)
     c                   eval      pbt(z) = mdate
      **
     c                   z-add     bgdfsd        mmdd6
     c                   move      dtt(z)        yy6
     c                   move      wkdat6        dtt6(z)
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
      **
     c                   eval      wkpayr = bbpayr
     c                   eval      wkplan = bbplan
      **
     c                   eval      rqport = 'B'
     c                   eval      rtplac = *blanks
     c                   eval      rttype = *blanks
     c                   exsr      srptos
     c                   if        exc343 = 'Y'
     c                   eval      pos(z) = '11'
     c                   else
     c**
     c                   if        bgdpos = *blanks
     c                   eval      pos(z) = rtplac
     c                   else
     c**
     c** look for different pos mapping code to put on bill
     c**
     c                   if        ct1flg = 'X'
     c                   eval      wrkct1 = bgdct1
     c                   else
     c                   eval      wrkct1 = *blanks
     c                   endif
     c                   if        ct2flg = 'X'
     c                   eval      wrkct2 = bgdct2
     c                   else
     c                   eval      wrkct2 = *blanks
     c                   endif
     c                   eval      wrkct3 = bgdct3
     c**
     c     retry         tag
     c                   eval      posmap = *blanks
     c     poskey        setll     hxfpomp
     c                   dou       *in79 = *on
     c     poskey        reade     hxfpomp                                79
     c                   if        *in79 = *off
     c                             and hxmefd <= bbfrdt
     c                             and bbfrdt <= hxmetd
     c                   eval      posmap = hxmpom
     c                   leave
     c                   endif
     c                   enddo
     c**
     c                   if        posmap <> *blanks
     c                   eval      bgdpos = posmap
     c                   else
     c                   if        wrkct3 <> *blanks
     c                   eval      wrkct3 = *blanks
     c                   goto      retry
     c                   endif
     c                   endif
     c**
     c                   eval      pos(z) = bgdpos
     c                   endif
     c                   endif
     c**
      **
      **
      **
     c                   if        bgdtbn = 999963
     c                   movel     bgdct3        cat2
     c                   else
     c                   movel     bgdct2        cat2
     c                   endif
      **
     c                   exsr      srptcd
      **
     c                   if        bgdmod <> *blanks
     c                   eval      %subst(mod(z):1:3) = bgdmod
     c                   endif
     c                   if        bgdmd2 <> *blanks
     c                   eval      %subst(mod(z):4:3) = bgdmd2
     c                   endif
     c                   if        bgdmd3 <> *blanks
     c                   eval      %subst(mod(z):7:3) = bgdmd3
     c                   endif
     c                   if        bgdmd4 <> *blanks
     c                   eval      %subst(mod(z):10:3) = bgdmd4
     c                   endif
      **
     c                   if        enepsd <> ' '
     c                   eval      eps(z) = enepsd
     c                   else
     c                   eval      eps(z) = enfamp
     c                   endif
      **
     c                   eval      todate = bgdtsd
     c                   eval      x = 0
     c     diagky        setgt     hmfpdiag
     c                   dou       *in79 = *on
     c     chgkey        readpe    hmfpdiag                               79
     c                   if        *in79 = *off
     c                   if        svdate = 0
     c                   eval      svdate = dxdate
     c                   leave
     c                   endif
     c                   endif
     c                   enddo

     c                   eval      ddx(z) = bgdicd
     c                   eval      amt(z) = bgdtam
     c                   eval      amta(z) = bgdtam
     c                   eval      bbamnt = bbamnt + amt(z)
      **
     c                   if        bgdtqt = 0
     c                   eval      qty(z) = 1
     c                   eval      qty4(z) = 1
     c                   eval      qty2(z) = 1
     c                   eval      qty5(z) = 1
     c                   else
     c                   eval      qty(z) = bgdtqt
     c                   eval      qty4(z) = bgdtqt
     c                   eval      qty2(z) = bgdtqt
     c                   eval      qty5(z) = bgdtqt
     c                   endif
      **
     c                   if        exc443 = 'Y'
     c                   move      'I'           omtype
     c     exokey        chain     hbfbfxo                            79
     c                   if        *in79 = *off
     c                             and bxodlt <> 'D'
     c     procky        chain     xffprocc                           79
     c                   if        *in79 = *off
     c                   eval      box19 = xfpdsc
     c                   endif
     c                   exsr      srctab
     c                   movel     'BALU'        tcode
     c                   movel     bgdtpr        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         decimal2
     c                   eval      qty(z) = decimal2
     c                   endif
     c                   endif
     c                   endif
      **  npi number
     c                   if        exc861
     c                             and snpi <> *blanks
     c                   movel     snpi          npi(z)
     c                   else
     c                   if        bgdtth <> 0
     c                   eval      rqdoctor# = bgdtth
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    npi(z)
     c                   endif
     c                   endif
      **
     c                   eval      newrev = 0
     c     procky        chain     xffprocc
     c                   eval      newrev = xfpubc
      **
     c                   eval      *in38 = *off
     c                   move      'D'           formtp
     c                   eval      hld24k = 0
     c                   eval      rqdoctor# = 0
     c     exccod        setll     hbfbexc
     c                   dou       *in71 = *on
     c     exccod        reade     hbfbexc                                71
     c                   if        *in71 = *off
     c                             and exdelt <> 'X'
     c                   movel     hx6st1        exstat
     c                   eval      exlvl6 = bbplv6
     c                   eval      expayr = bbpayr
     c                   eval      explan = bbplan
     c                   movel     tmpfc         exfc
     c                   eval      exrevc = newrev
     c                   eval      exCat1 = savCat1
     c                   eval      exCat2 = savCat2
     c                   exsr      bilexc
     c                   endif
     c                   enddo
      **
     c
     c                   if        rqdoctor# <> 0
     c                   eval      bgdtth = rqdoctor#
     c                   endif
     c                   eval      bgdpos = pos(z)
     c                   eval      bgdtc4 = cpt(z)
     c                   eval      bgdfmp = eps(z)
     c
     c                   if        ((amt(z) <= 0
     c                               and (sndizc = 'N' or grsnet <> 'G'))
     c                             or (sndizc = 'Y' and amt(z) < 0
     c                                 and grsnet = 'G'))
     c                             and cpt(z) <> 'G8553' and cpt(z) <> 'G8443'
     c****               eval      counter -= 1
     c****               eval      bbcont = counter
     c                   else
     c                   eval      counter += 1
     c                   eval      bbcont = counter
     c                   if        exc452 or exc989
     c                   eval      svdtqt = bgdtqt
     c                   eval      bgdtqt = qty5(z)
     c                   eval      bgcs06 = qty2(z)
     c                   endif
     c                   eval      bg24IQ = bx24i(z)
     c                   eval      bg24JP = pin(z)
     c                   eval      bg24JN = npi(z)
     c                   write     e5fchr15
     c                   eval      chr15new += 1
     c                   if        exc452 or exc989
     c                   eval      bgdtqt = svdtqt
     c                   endif
     c                   endif
      **
     c                   eval      totchg = totchg + bgdtam
     c                   eval      bilchg = bilchg + bgdtam
     c                   eval      rptchg = rptchg + bgdtam
     c                   eval      lv6chg = lv6chg + bgdtam
     c                   eval      paychg = paychg + bgdtam
     c                   eval      plnchg = plnchg + bgdtam
     c                   eval      accchg = accchg + bgdtam
     c                   if        prtpmt = 'Y'
     c                   if        ins# = 1
     c                   eval      totpmt = totpmt - bgdpp1
     c                   endif
     c                   if        ins# = 2
     c                   eval      totpmt = totpmt - bgdpp1
     c                   eval      totpmt = totpmt - bgdpp2
     c                   endif
     c                   if        ins# = 3
     c                   eval      totpmt = totpmt - bgdpp1
     c                   eval      totpmt = totpmt - bgdpp2
     c                   eval      totpmt = totpmt - bgdpp3
     c                   endif
     c                   else
     c                   if        exc10 = 'N'
     c                   if        ins# = 2
     c                   eval      totpmt = totpmt - bgdpp1
     c                   endif
     c                   if        ins# = 3
     c                   eval      totpmt = totpmt - bgdpp1
     c                   eval      totpmt = totpmt - bgdpp2
     c                   endif
     c                   endif
     c                   endif
      **
     c                   if        (exc527)
     c     pntrKy2       setgt     hafpntrk
     c     demoKy        readpe    hafpntrk
     c                   eval      totpmt = bpppp1
     c                   endif

     c                   eval      bilpmt = bilpmt + totpmt
     c                   eval      rptpmt = rptpmt + totpmt
     c                   eval      lv6pmt = lv6pmt + totpmt
     c                   eval      paypmt = paypmt + totpmt
     c                   eval      plnpmt = plnpmt + totpmt
     c                   eval      accpmt = accpmt + totpmt
      **
     c                   if        first = ' '                                  =if bill exception
     c                             and exc05 = *off                              5 and 12 not set
     c                             and exc12 = *off                              up, then
     c     procky        chain     xffprocc                           79         retrieve provider
     c                   if        *in79 = *off                                  number based on
      **
     c     mmpct1        chain     hxfctg1
     c                   if        %found(hxpctg1)
     c                             and (hxprty='O' or hxprty='E' or hxptyp='S')
     c                             and xfpnub <> 0
     c                   eval      xfpubc = xfpnub
     c                   endif
      **
     c                   eval      prmprc = xfproc                               procedure code/
     c                   eval      svproc = xfproc                               revenue code of
     c                   eval      prmrev = xfpubc                               first charge on
     c                   eval      savrev = xfpubc                               bill
     c                   else
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   endif
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
     c                   movel     'PRVDR'       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   movel     prvvar        prprov
     c                   eval      first = 'X'
     c                   if        exc176 = *on                                 =if bill exception
     c                   eval      prgrp# = prprov                               176 is set up
     c                   eval      prprov = *blanks
     c                   endif
     c                   if        exc401 = *on
     c                             and exc885 = *off
     c                   movel     prprov        prtprv
     c                   endif
     c                   endif
      **
     c                   endif
     c     nextd         tag
     c                   enddo
     **********
     ** box32a
     **********
     **********
     ** box32
     **********
     c**                   if        exc186 = *on
      ** Move above before detail is done
     c***                if        exc665 = *on
     c***                eval      payor = bbpayr
     c***                eval      plan = bbplan
     c***                eval      prmrev = 0
     c***                eval      prmprc = *blanks
     c***                eval      prmct1 = mmpct1
     c***                eval      prmct2 = mmpct2
     c***                eval      prvid = 'ALTNM'
     c***                eval      prmlv6 = bbplv6
     c***                exsr      srprv
     c***                if        prvvar <> *blanks
     c***                movel     prvvar        box321
     c***                endif
     c***                endif
      **
     c*****              if        exc186 = *on
     c*****              eval      payor = bbpayr
     c*****              eval      plan = bbplan
     c*****              eval      prmrev = 0
     c*****              eval      prmprc = *blanks
     c*****              eval      prmct1 = mmpct1
     c*****              eval      prmct2 = mmpct2
     c*****              eval      prvid = 'ALTAD'
     c*****              eval      prmlv6 = bbplv6
     c*****              exsr      srprv
     c*****              if        prvvar <> *blanks
     c*****              movel     prvvar        rtnaddress
     c*****              movel     rtnadr        box322
     c*****              movel     rtncty        box323
     c*****              movel     rtnst         box324
     c*****              movel     rtnzp1        box325
     c*****              movel     rtnzp2        box325e
     c*****              endif
     c*****              endif
     **********
     ** box33a
     **********
     c                   eval      box33a = *blanks
     c                   eval      chkprv = sndprv
     c                   eval      chkcnt = 0
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = savrev
     c                   eval      prmprc = svproc
     c**
     c     findprovd2    tag
      **
     c                   select
     c*****************************************************************
     c** code D - facility NPI
     c*****************************************************************
     c                   when      chkprv = 'D'
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   if        prprov = *blanks
     c                   movel     prvvar        prprov
     c                   endif
     c                   movel     prvvar        box33a
     c                   endif
     c*****************************************************************
     c** code C - facility provider ID
     c*****************************************************************
     c                   when      chkprv = 'C'
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
     c                   movel     'PRVDR'       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   if        prprov = *blanks
     c                   movel     prvvar        prprov
     c                   endif
     c                   endif
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c*****************************************************************
     c** code U - therapist NPI
     c*****************************************************************
     c                   when      chkprv = 'U'
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        prprov = *blanks
     c                   movel     rtnlicense    prprov
     c                   endif
     c                   movel     rtnlicense    box33a
     c*****************************************************************
     c** code T - therapist provider ID
     c*****************************************************************
     c                   when      chkprv = 'T'
     c                   eval      level6 = bbplv6                              =check by
     c     thprky        chain     hmfplnpr                           79         level 6, payor/plan
     c                   if        *in79 = *off
     c                   if        prprov = *blanks
     c                   movel     hmtpr#        prprov
     c                   endif
     c                   else
     c                   eval      level6 = 0                                   =check by
     c     thprky        chain     hmfplnpr                           79         payor/plan alone
     c                   if        *in79 = *off
     c                   if        prprov = *blanks
     c                   movel     hmtpr#        prprov
     c                   endif
     c                   endif
     c                   endif
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        prprov = *blanks
     c                   movel     rtnlicense    prprov
     c                   endif
     c                   movel     rtnlicense    box33a
     c*****************************************************************
     c** code X - use tax id
     c*****************************************************************
     c                   when      chkprv = 'X'
     c                   if        prprov = *blanks
     c                   movel     taxid         prprov
     c                   endif
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c                   endsl
     c*****************************************************************
     c** if no provider variable found for method, try another
     c*****************************************************************
     c                   if        prprov = *blanks and chkcnt < 6
     c                   eval      chkcnt = chkcnt + 1
     c                   select
     c                   when      chkprv = 'D'
     c                   eval      chkprv = 'C'
     c                   goto      findprovd2
     c                   when      chkprv = 'C'
     c                   eval      chkprv = 'U'
     c                   goto      findprovd2
     c                   when      chkprv = 'U'
     c                   eval      chkprv = 'T'
     c                   goto      findprovd2
     c                   when      chkprv = 'T'
     c                   eval      chkprv = 'X'
     c                   goto      findprovd2
     c                   when      chkprv = 'X'
     c                   eval      chkprv = 'D'
     c                   goto      findprovd2
     c                   endsl
     c                   endif

        // If exception code is on and value exists for Alternate NPI, ignore
        // the contract set up and use provider variable.
     c                   if        exc417
     c                   movel     bgdtpr        prmprc
     c                   movel     'ALTNP'       prvid
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prmrev = 0
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c                   endif
      **
     c                   if        exc543 = *on
     c                   eval      prmlv6 = bbplv6
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prmrev = *zeros
     c                   eval      prmdte = bbtodt
     c                   eval      prvvar = *blanks
     c                   eval      prvid = 'PRVDR'
     c                   eval      prmprc = bgdtpr
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      box32a = prvvar
     c                   eval      box33a = prvvar
     c                   endif
     c                   endif
      **
     c                   if        exc833 and box33a = *blanks
     c                   eval      box33a = 'ATYPICAL'
     c                   endif
      **
     c                   if        z > 0
     c                   if        ((amt(z) <= 0
     c                               and (sndizc = 'N' or grsnet <> 'G'))
     c                             or (sndizc = 'Y' and amt(z) < 0
     c                                 and grsnet = 'G'))
     c                             and cpt(z) <> 'G8553' and cpt(z) <> 'G8443'
     c                   eval      dtf(z) = *blanks
     c                   eval      dtt(z) = *blanks
     c                   eval      pbf(z) = 0
     c                   eval      pbt(z) = 0
     c                   eval      pos(z) = *blanks
     c                   eval      eps(z) = *blanks
     c                   eval      cpt(z) = *blanks
     c                   eval      ddx(z) = *blanks
     c                   eval      mod(z) = *blanks
     c                   eval      cds(z) = *blanks
     c                   eval      pdsc(z) = *blanks
     c                   eval      amt(z) = 0
     c                   eval      amta(z) = 0
     c                   eval      qty(z) = 0
     c                   eval      qty4(z) = 0
     c                   eval      qty2(z) = 0
     c                   eval      qty5(z) = 0
     c                   eval      dix(z) = *blanks
     c                   eval      z = z - 1
     c                   endif
      **
     c     z             cabeq     0             enddtl
      **
     c                   eval      baldue = totchg - totpmt
     c                   eval      bildue = bildue + baldue
     c                   eval      rptdue = rptdue + baldue
     c                   eval      lv6due = lv6due + baldue
     c                   eval      paydue = paydue + baldue
     c                   eval      plndue = plndue + baldue
     c                   eval      accdue = accdue + baldue
     c                   if        exc753 = *on
     c                   eval      baldue = totChgBill
     c                   elseif    not *in30
     c                   eval      baldue = 0                                   02-12 not used
     c                   endif

      *****  The real box 14 - Date of current injury
     c                   if        exc224
     c                   eval      injdte = mmpidd
     c                   elseif    mmpcmp = 'Y' and mmpidd <> 0
     c                   eval      injdte = mmpidd
     c                   else
     c                   eval      injdte = dxdate
     c                   endif
     c                   eval      box14q = '431'                               Onset of illness
     c                   if        %found(hmpenct) and chnenc = *on and
     c                             not exc224
     c                   eval      injdte = enpidd
     c                   if        enpreg = 'Y'
     c                   eval      box14q = '484'                               Last LMP
     c                   endif
     c                   endif
      **
     c                   if        exc978
     c                   eval      box15dte = mmpidd
     c                   if        mmpidd <> 0
     c                   eval      *in28 = *on
     c                   eval      box15q = '439'
     c                   endif
     c                   endif
      **
     c                   if        exc979
     c                   exsr      getIniTreatDt
     c                   if        initTrtDt <> 0
     c                   eval      box15dte = initTrtDt
     c                   eval      *in28 = *on
     c                   eval      box15q = '454'
     c                   endif
     c                   endif
      **
     c                   if        exc983 and (bbtOrb = '6' or bbtOrb = '7'
     c                             or bbtOrb = '8')
     c                   eval      box22a = bbtOrb
     c                   endif
      **
     c                   if        (exc994 and not(bbtOrb = '6' or bbtOrb = '7'
     c                             or bbtOrb = '8'))
     c                   eval      bbtOrb = '1'
     c                   eval      box22a = ''
     c                   endif
      **
     c                   if        onetrk = '0'
     c                   move      '1'           onetrk
     c                   endif
     c                   eval      ydate = bbfrdt
     c                   exsr      srcymd
     c                   eval      frmdte = mdate
     c                   eval      ydate = bbtodt
     c                   exsr      srcymd
     c                   eval      todate = mdate
      **
     c****               eval      rw = 0
     c****               eval      cl = 0
     c****               exsr      setptr
      **
     c                   eval      *in74 = *off
     c     1             do        z             xxy
     c                   if        amt(xxy) = 0
     c                   eval      *in74 = *on
     c                   endif
     c                   enddo
      **
     c                   eval      pageno = pageno + 1
     c****               if        sndf10 = 'Y'                                 =overlay
     c                   if        sndf10 <> 'N' and singlePrint <> 'Y'         =overlay
     c                   eval      rw = rowO
     c                   eval      cl = colO
     c                   eval      inc = incO
     c                   write     bill15
     c                   if        sndf10 = 'A'                                 =overlay to archive
     c****               except    billpg                                       =but not to print
     c                   eval      rw = rowN
     c                   eval      cl = colN
     c                   eval      inc = incN
     c                   write     billnoovl
     c                   endif
     c                   else                                                   =no overlay
     c****               except    billpg
     c                   eval      rw = rowN
     c                   eval      cl = colN
     c                   eval      inc = incN
     c                   write     billnoovl
     c                   endif
      **
     c                   if        elecwrite = *off
     c                   exsr      fillelecdata
     c                   eval      elecwrite = *on
     c                   endif
      **
     c                   except    detail
     c                   eval      pageno2 = pageno2 + 1
     c****               eval      pageno = pageno + 1
      **
     c                   if        sndf10 = 'A' and singlePrint <> 'Y'
     c                   move      'A'           crtspl
     c                   else
     c                   move      'Y'           crtspl
     c                   endif
      **
     c                   eval      rptbls = rptbls + 1
     c                   eval      bilpgs = bilpgs + 1
     c                   eval      rptpgs = rptpgs + 1
     c                   eval      lv6pgs = lv6pgs + 1
     c                   eval      paypgs = paypgs + 1
     c                   eval      plnpgs = plnpgs + 1
     c                   eval      accpgs = accpgs + 1
      **
     c                   endif
     c                   endif
      **
     c     enddtl        tag
      **
     c                   callp     HBXSYSRBL(bbplv6 : bbaccn : bbtrak : sysrbl)

     c                   if        nobill = 'X'                                 =remove from
     c****                         and bbaafl = ' '                              approval if no
     c                             and (bbaafl = ' '                              approval if no
     c                             or (bbaafl ='R' and bbbsts='R')
     c                             or (bbaafl ='S' and sysrbl = *on))
     c                   eval      bbtran = 'G'                                  charges
     c                   endif
      **
     c                   movel     prprov        bbprov
      **
     c                   if        exc992 and z = 0
     c                   eval      bbForm = 'ZB'
     c                   endif
      **
     c                   if        bbproc = *blanks
      **
     c                   if        exc759 = *on
     c                   eval      svbild = 0
     c                   eval      svtrak = *blanks

     c                   eval      splitUB15 = *off
     c     bbtrko        setll     hbfbilot
     c                   dou       %eof(hblbilot)
     c     bbtrko        reade     hbfbilot
     c                   if        not %eof(hblbilot)
     c                             and fbocst = 'C'
     c                   if        fbform = 'UR' or fbform = 'UC'
     c                   eval      splitUB15 = *on
     c                   endif
     c                   if        svbild = 0 or fbbild >= svbild
     c                             and fbform = '15'
     c                   eval      svbild = fbbild
     c                   eval      svtrak = fbtrak
     c                   endif
     c                   endif
     c                   enddo

     c                   if        splitUB15 = *on and svtrak <> *blanks
     c                   eval      chr15old = 0                                 =count detail on
     c     svtrak        setll     e5fchr15l                                     original bill
     c                   dou       %eof(e5lchr15)
     c     svtrak        reade     e5fchr15l
     c                   if        not %eof(e5lchr15)
     c                   eval      chr15old += 1
     c                   endif
     c                   enddo

     c                   if        chr15new <> chr15old
     c                   eval      splitUB15 = *off
     c                   endif
     c                   endif

     c                   if        splitUB15 = *on and svtrak <> *blanks

     c     bbtrak        setll     e5fchr15                                     =check detail
     c                   dou       %eof(e5pchr15)                                on current bill
     c     bbtrak        reade     e5fchr15
     c                   if        not %eof(e5pchr15)

     c                   eval      dupDetail = *off                             =look for duplicate
     c     svtrak        setll     e5fchr15l                                     detail on original
     c                   dou       %eof(e5lchr15)                                bill
     c     svtrak        reade     e5fchr15l
     c                   if        not %eof(e5lchr15)
     c                             and BBCONT = ZBCONT
     c                             and BGDTAC = ZGDTAC
     c                             and BGDFPI = ZGDFPI
     c                             and BGDTPR = ZGDTPR
     c                             and BGDCT# = ZGDCT#
     c                             and BGDFC1 = ZGDFC1
     c                             and BGDUB1 = ZGDUB1
     c                             and BGDPL1 = ZGDPL1
     c                             and BGDPO1 = ZGDPO1
     c                             and BGDSQ1 = ZGDSQ1
     c                             and BGDBU1 = ZGDBU1
     c                             and BGDFC2 = ZGDFC2
     c                             and BGDUB2 = ZGDUB2
     c                             and BGDPL2 = ZGDPL2
     c                             and BGDPO2 = ZGDPO2
     c                             and BGDSQ2 = ZGDSQ2
     c                             and BGDBU2 = ZGDBU2
     c                             and BGDFC3 = ZGDFC3
     c                             and BGDUB3 = ZGDUB3
     c                             and BGDPL3 = ZGDPL3
     c                             and BGDPO3 = ZGDPO3
     c                             and BGDSQ3 = ZGDSQ3
     c                             and BGDBU3 = ZGDBU3
     c                             and BGDBUP = ZGDBUP
     c                             and BGDPBL = ZGDPBL
     c                             and BGDTSD = ZGDTSD
     c                             and BGDTPD = ZGDTPD
     c                             and BGDCT1 = ZGDCT1
     c                             and BGDCT2 = ZGDCT2
     c                             and BGDCT3 = ZGDCT3
     c                             and BGDLV6 = ZGDLV6
     c                             and BGDICD = ZGDICD
     c                             and BGDTC4 = ZGDTC4
     c                             and BGDTQT = ZGDTQT
     c                             and BGDTAM = ZGDTAM
     c                             and BGDGP1 = ZGDGP1
     c                             and BGDGP2 = ZGDGP2
     c                             and BGDGP3 = ZGDGP3
     c                             and BGDGPT = ZGDGPT
     c                             and BGDAP1 = ZGDAP1
     c                             and BGDAP2 = ZGDAP2
     c                             and BGDAP3 = ZGDAP3
     c                             and BGDAPT = ZGDAPT
     c                             and BGDCI1 = ZGDCI1
     c                             and BGDCI2 = ZGDCI2
     c                             and BGDCI3 = ZGDCI3
     c                             and BGDCIP = ZGDCIP
     c                             and BGDPP1 = ZGDPP1
     c                             and BGDPP2 = ZGDPP2
     c                             and BGDPP3 = ZGDPP3
     c                             and BGDPPT = ZGDPPT
     c                             and BGDCP1 = ZGDCP1
     c                             and BGDCP2 = ZGDCP2
     c                             and BGDCP3 = ZGDCP3
     c                             and BGDCPP = ZGDCPP
     c                             and BGDDM1 = ZGDDM1
     c                             and BGDDM2 = ZGDDM2
     c                             and BGDDM3 = ZGDDM3
     c                             and BGDDMP = ZGDDMP
     c                             and BGDAJ1 = ZGDAJ1
     c                             and BGDAJ2 = ZGDAJ2
     c                             and BGDAJ3 = ZGDAJ3
     c                             and BGDAJP = ZGDAJP
     c                             and BGDOV1 = ZGDOV1
     c                             and BGDOV2 = ZGDOV2
     c                             and BGDOV3 = ZGDOV3
     c                             and BGDOVP = ZGDOVP
     c                             and BGDNA1 = ZGDNA1
     c                             and BGDNA2 = ZGDNA2
     c                             and BGDNA3 = ZGDNA3
     c                             and BGDNAP = ZGDNAP
     c                             and BGDNP1 = ZGDNP1
     c                             and BGDNP2 = ZGDNP2
     c                             and BGDNP3 = ZGDNP3
     c                             and BGDNPT = ZGDNPT
     c                             and BGDNET = ZGDNET
     c                             and BGDTAU = ZGDTAU
     c                             and BGDTBN = ZGDTBN
     c                             and BGDUBN = ZGDUBN
     c                             and BGDTTH = ZGDTTH
     c                             and BGDSEQ = ZGDSEQ
     c                             and BGDNVR = ZGDNVR
     c                             and BGDOVR = ZGDOVR
     c                             and BGDRMN = ZGDRMN
     c                             and BGDRMC = ZGDRMC
     c                             and BGDHST = ZGDHST
     c                             and BGDCRT = ZGDCRT
     c                             and BGDMOD = ZGDMOD
     c                             and BGDMD2 = ZGDMD2
     c                             and BGDMD3 = ZGDMD3
     c                             and BGDMD4 = ZGDMD4
     c                             and BGDAPP = ZGDAPP
     c                             and BGDARL = ZGDARL
     c                             and BGDFIL = ZGDFIL
     c                             and BGDPRV = ZGDPRV
     c                             and BGDFSD = ZGDFSD
     c                             and BGDPOS = ZGDPOS
     c                             and BGDTOS = ZGDTOS
     c                             and BGDPTR = ZGDPTR
     c                             and BGDPT2 = ZGDPT2
     c                             and BGDPT3 = ZGDPT3
     c                             and BGDPT4 = ZGDPT4
     c                             and BGDPST = ZGDPST
     c                             and BGDPET = ZGDPET
     c                             and BGNDC# = ZGNDC#
     c                             and BGITMQ = ZGITMQ
     c                             and BGITMU = ZGITMU
     c                             and BGNTET = ZGNTET
     c                             and BGNTED = ZGNTED
     c                             and BGCST1 = ZGCST1
     c                             and BGCST2 = ZGCST2
     c                             and BGCST3 = ZGCST3
     c                             and BGCST4 = ZGCST4
     c                             and BGCS01 = ZGCS01
     c                             and BGCS02 = ZGCS02
     c                             and BGCS03 = ZGCS03
     c                             and BGCS04 = ZGCS04
     c                             and BGCS05 = ZGCS05
     c                             and BGCS06 = ZGCS06
     c                             and BGCS07 = ZGCS07
     c                             and BGCS08 = ZGCS08
     c                             and BGCS09 = ZGCS09
     c                             and BGCS10 = ZGCS10
     c                             and BGCS11 = ZGCS11
     c                             and BGCS12 = ZGCS12
     c                             and BGCS13 = ZGCS13
     c                             and BGCS14 = ZGCS14
     c                             and BGCS15 = ZGCS15
     c                             and BGCS16 = ZGCS16
     c                             and BGCS17 = ZGCS17
     c                             and BGCS18 = ZGCS18
     c                             and BGCS19 = ZGCS19
     c                             and BGDFMP = ZGDFMP
     c                             and BGDTQ2 = ZGDTQ2
     c                   eval      dupDetail = *on
     c                   leave
     c                   endif
     c                   enddo

     c                   if        dupDetail = *off
     c                   eval      splitUB15 = *off
     c                   leave
     c                   endif

     c                   endif
     c                   enddo

     c                   endif

     c                   if        splitUB15 = *on
     c                   eval      bbtran = 'G'
     c                   endif

     c                   endif                                                  =end ex759
      **
     c                   if        bbtype <> *blanks
     c                   move      'Y'           bbelct
     c                   endif
      **
     c                   if        bbtran = 'D'
     c                   movel     'M'           bbtran
     c                   endif
      **
     c                   if        reqlv6 = 0
     c                   move      'C'           bbproc
     c                   else
     c                   move      'R'           bbproc
     c                   endif
      **
     c                   if        rtnflg = 'Y'
     c                   eval      gtbedp = pageno2
     c****               eval      gtbedp = pageno
     c                   movel     bbtrak        gtbitn
     c                   movel     prmmbr        gtbmbr
     c                   movel     prmfle        gtbfle
     c                   write     hbfgtob
     c                   endif
      **
     c                   if        (not exc994)
     c                   exsr      updtfreq
     c                   endif
      **
     c                   if        bbform = 'ZB'
     c                   eval      reqdat = bbtodt
     c                   exsr      srppd
     c                   endif
      **
     c                   if        bbform <> 'ZB' or sndbfq <> 'D'
     c                   write     hbfbillh
     c                   endif
     c                   write     hbfbills
     c                   delete    hbfbill
      **
     c                   else
      **
     c                   if        *inu1 = *on
     c                             and *inu7 = *on
     c                   move      'X'           bbscrb
     c                   endif
      **
     c                   if        *inu8 = *on
     c                   move      'Y'           bbelct
     c                   endif
      **
     c                   if        *inu7 = *off
     c                             and bbtype = *blanks
     c                             or *inu8 = *on
     c****               if        bbaafl <> *blanks

     c                   callp     HBXSYSRBL(bbplv6 : bbaccn : bbtrak : sysrbl)

     c                   if        bbaafl = 'X'
     c                             or (bbaafl ='R' and bbbsts <> 'R')
     c                             or (bbaafl ='S' and sysrbl = *off)
     c                   movel     '*SYSTEM'     bbtsnm
     c                   else
     c                   movel     ldausr        bbtsnm
     c                   endif
     c                   time                    tmptim
     c                   movel     tmptim        bbtstm
     c                   eval      mdate = *date
     c                   exsr      srcmdy
     c                   eval      bbtsdt = ydate
     c                   endif
      **
     c                   if        rtnflg = 'Y'
     c                             and *inu8 = *off
     c                   call      'HBXDGTOB'
     c                   parm                    bbtrak
     c                   eval      gtbedp = pageno2
     c****               eval      gtbedp = pageno
     c                   movel     bbtrak        gtbitn
     c                   movel     prmmbr        gtbmbr
     c                   movel     prmfle        gtbfle
     c                   write     hbfgtob
     c                   endif
      **
     c                   if        (not exc994)
     c                   exsr      updtfreq
     c                   endif
      **
     c                   update    hbfbill
     c                   endif
      **
     c                   if        bilpgs <> 0
     c                   except    biltot
     c                   endif
      **
     c     skip          tag
      *****************************************************************
     cl1                 if        accpgs <> 0
     cl1                 except    acctot
     cl1                 eval      accchg = 0
     cl1                 eval      accpmt = 0
     cl1                 eval      accdue = 0
     cl1                 eval      accpgs = 0
     cl1                 endif
      *****************************************************************
     cl3                 if        plnpgs <> 0
     cl3                 except    plntot
     cl3                 eval      plnchg = 0
     cl3                 eval      plnpmt = 0
     cl3                 eval      plndue = 0
     cl3                 eval      plnpgs = 0
     cl3                 endif
      *****************************************************************
     cl4                 if        paypgs <> 0
     cl4                 except    paytot
     cl4                 eval      paychg = 0
     cl4                 eval      paypmt = 0
     cl4                 eval      paydue = 0
     cl4                 eval      paypgs = 0
     cl4                 endif
      *****************************************************************
     cl5                 if        lv6pgs <> 0
     cl5                 except    lv6tot
     cl5                 eval      lv6chg = 0
     cl5                 eval      lv6pmt = 0
     cl5                 eval      lv6due = 0
     cl5                 eval      lv6pgs = 0
     cl5                 endif
      *****************************************************************
     clr                 if        open = 'Y'
     clr                 except    totals
     clr                 endif
      **************************************************************************
      **                         Fill and Map BBTORB                          **
      **************************************************************************
     c     updtfreq      begsr

      ** Verify That Rebills are of Previously Transmitted Bills **
     c                   if        bbtorb = '7' or bbtorb = 'X'
     c                   eval      bbtorb = *blanks                             =blank initially
     c     bbtrko        setll     hbfbiltr                                     =if exists trans'd
     c                   dou       %eof(hblbiltr)                                w/ same original.
     c     bbtrko        reade     hbfbiltr                                      set back to 7
     c                   if        not%eof and tbtrdt <> 0
     c                   eval      bbtorb = '7'
     c                   leave
     c                   endif
     c                   enddo
     c                   if        bbtorb = *blanks
     c     origClaim     chain     hbfbillt
     c                   if        %found(hblbillt) and tbtrdt <> 0
     c                   eval      bbtorb = '7'
     c                   endif
     c                   endif
     c                   endif

      ** Fill in if Blank **
     c                   if        bbtorb = *blanks

     c                   if        bbieff > mmaddt
     c                   eval      frstdt = bbieff
     c                   else
     c                   eval      frstdt = mmaddt
     c                   endif

     c                   if        bbiedt < mmdsdt or mmdsdt = 0
     c                   eval      lastdt = bbiedt
     c                   else
     c                   eval      lastdt = mmdsdt
     c                   endif

     c                   select
     c                   when      bbbsts = 'N'                                 =No-Pay (Medicare)
     c                   eval      bbtorb = '0'
     c                   when      bbbsts = 'V'                                 =Cancel Claim
     c                   eval      bbtorb = '8'
     c                   when      bbrbmt = '2' or bbrbmt = '4' or bbrbmt = '7' =Late Charge Only
     c                   eval      bbtorb = '5'
     c                   when      frstdt >= bbfrdt and lastdt <= bbtodt        =Admit to Discharge
     c                   eval      bbtorb = '1'
     c                   when      frstdt >= bbfrdt and lastdt > bbtodt         =First Claim
     C                   eval      bbtorb = '2'
     c                   when      frstdt < bbfrdt and lastdt > bbtodt          =Continuing Claim
     c                   eval      bbtorb = '3'
     c                   when      frstdt < bbfrdt and lastdt <= bbtodt         =Last Claim
     c                   eval      bbtorb = '4'
     c                   endsl
     c                   endif

      ** Map Frequency Code **
     c                   eval      wkcat1 = mmpct1
     c                   eval      wkcat2 = mmpct2
     c                   eval      wkcat3 = mmpct3
     c                   eval      workfreq = bbtorb
     c     billtp        chain     hbftobf
     c                   if        not%found
     c                   eval      wkcat3 = *blanks
     c     billtp        chain     hbftobf
     c                   if        not%found
     c                   eval      wkcat2 = *blanks
     c     billtp        chain     hbftobf
     c                   if        not%found
     c                   eval      wkcat1 = *blanks
     c     billtp        chain     hbftobf
     c                   endif
     c                   endif
     c                   endif
     c                   if        %found(hbptobf)
     c                   eval      bbtorb = tobfvl
     c                   endif
     c                   endsr
      *****************************************************************
     c     chkmod        begsr
      **
     c                   move      'N'           modlty
      **
     c                   z-add     0             modsdt
     c                   z-add     bbpayr        modubn
     c                   z-add     bbplan        modpln
      **
      *----------------------------------------------------------------*
      ** SPIN THROUGH CHARGE FILE - LV6/ACCT BY SVC DATE
      *----------------------------------------------------------------*
      **
     c     chgkey        setll     hbfchgmm
     c                   dou       *in86 = *on
     c     chgkey        reade     hbfchgmm                               86
     c                   if        *in86 = *off
     c                             and bgdtsd >= bbfrdt
     c                             and bgdtsd <= bbtodt
      **
      *----------------------------------------------------------------*
      * IF NEW SVC DATE: DETERMINE IF/HOW MODALITY DEFINED FOR P/P
      *----------------------------------------------------------------*
      **
     c                   if        bgdtsd <> modsdt
     c                   z-add     bgdtsd        modsdt
      **
     c                   movel     bgdct1        modct1
     c                   movel     bgdct2        modct2
     c     mmodky        setll     hbfmmod
     c     mmodk2        reade     hbfmmod                                87
     c                   if        *in87 = *on
     c                   move      *blanks       modct1
     c     mmodky        setll     hbfmmod
     c     mmodk2        reade     hbfmmod                                87
     c                   if        *in87 = *on
     c                   movel     bgdct1        modct1
     c                   move      *blanks       modct2
     c     mmodky        setll     hbfmmod
     c     mmodk2        reade     hbfmmod                                87
     c                   if        *in87 = *on
     c                   move      *blanks       modct1
     c                   move      *blanks       modct2
     c     mmodky        setll     hbfmmod
     c     mmodk2        reade     hbfmmod                                87
     c                   endif
     c                   endif
     c                   endif
      **
      *----------------------------------------------------------------*
      * IF MODALITY DEFINED FOR SVC DATE - WRITE TO WORK FILE
      *----------------------------------------------------------------*
      **
     c                   if        *in87 = *off
     c                   if        modsdt >= xmmfdt
     c                             and modsdt <= xmmtdt
     c                   move      'Y'           modlty
     c     modwky        chain     hbfmodwf                           88
     c                   if        *in88 = *on
     c                   z-add     bbplv6        wrklv6
     c                   z-add     bbaccn        wrkacn
     c                   z-add     modsdt        wrksdt
     c                   z-add     modubn        wrkubn
     c                   z-add     modpln        wrkpln
     c                   movel     modct1        wrkct1
     c                   movel     modct2        wrkct2
     c                   write     hbfmodwf
     c                   leavesr
     c                   endif
     c                   endif
     c                   endif
      **
     c                   endif
      **
     c                   endif
     c                   enddo
      **
     c                   endsr
      *****************************************************************
     c     mltmod        begsr
      **
      *----------------------------------------------------------------*
      * SPIN THROUGH MODALITY WORK FILE - LV6/ACCT BY SVC DT
      *----------------------------------------------------------------*
      **
     c     modwk2        setll     hbfmodwf
     c                   dou       *in86 = *on
     c     modwk2        reade     hbfmodwf                               86
     c                   if        *in86 = *off
      **
      *----------------------------------------------------------------*
      * CALCULATE NET RATES AND BUILD WKMOD DESCENDING RATE FILE
      *----------------------------------------------------------------*
      **
     c     mmchky        setll     hbfchgmm
     c                   dou       *in87 = *on
     c     mmchky        reade     hbfchgmm                               87
     c                   if        *in87 = *off
      **
     c                   if        bgdtqt = 0
     c                   iter
     c                   endif
      **
     c                   select
     c                   when      bgdub1 = wrkubn
     c                             and bgdpl1 = wrkpln
     c                   movel     bgdtpr        wrkcpr
     c                   z-add     bgdseq        wrkcps
     c                   z-add     bgdtsd        wrksdt
     c                   z-add     bgdnp1        wrkrte
     c                   sub       bgdci1        wrkrte
     c                   sub       bgdpp1        wrkrte
     c                   sub       bgdcp1        wrkrte
     c                   sub       bgddm1        wrkrte
     c                   exsr      submod
     c                   write     hbfwkmod
      **
     c                   when      bgdub2 = wrkubn
     c                             and bgdpl2 = wrkpln
     c                   movel     bgdtpr        wrkcpr
     c                   z-add     bgdseq        wrkcps
     c                   z-add     bgdtsd        wrksdt
     c                   z-add     bgdnp2        wrkrte
     c                   sub       bgdci2        wrkrte
     c                   sub       bgdpp2        wrkrte
     c                   sub       bgdcp2        wrkrte
     c                   sub       bgddm2        wrkrte
     c                   exsr      submod
     c                   write     hbfwkmod
      **
     c                   when      bgdub3 = wrkubn
     c                             and bgdpl3 = wrkpln
     c                   movel     bgdtpr        wrkcpr
     c                   z-add     bgdseq        wrkcps
     c                   z-add     bgdtsd        wrksdt
     c                   z-add     bgdnp3        wrkrte
     c                   sub       bgdci3        wrkrte
     c                   sub       bgdpp3        wrkrte
     c                   sub       bgdcp3        wrkrte
     c                   sub       bgddm3        wrkrte
     c                   exsr      submod
     c                   write     hbfwkmod
      **
     c                   other
     c                   movel     bgdtpr        wrkcpr
     c                   z-add     bgdseq        wrkcps
     c                   z-add     bgdtsd        wrksdt
     c                   z-add     bgdnpt        wrkrte
     c                   sub       bgdcip        wrkrte
     c                   sub       bgdppt        wrkrte
     c                   sub       bgdcpp        wrkrte
     c                   sub       bgddmp        wrkrte
     c                   exsr      submod
     c                   write     hbfwkmod
     c                   endsl
      **
     c                   endif
     c                   enddo
      **
     c                   endif
     c                   enddo
      **
     c                   endsr
      *****************************************************************
     c     submod        begsr
      **
      *----------------------------------------------------------------*
      * DETERMINE MODALITY ALLOWANCE AMOUNT TO BE BACKED OUT
      *----------------------------------------------------------------*
      **
     c                   eval      code = 4
     c     instky        chain     hxfinst                            79        =get modality
     c                   if        *in79 = *off                                  allowance
     c                   movel     xfiprc        modprc                          procedure
     c                   endif
      **
      *----------------------------------------------------------------*
      * BACK OUT MODALITY ALLOWANCE TO GET ORIGINAL RATE
      *----------------------------------------------------------------*
      **
     c     alwmky        setll     hbfalwsm
     c                   dou       *in88 = *on
     c     alwmky        reade     hbfalwsm                               88
     c                   if        *in88 = *off
     c                   sub       bgatam        wrkrte
     c                   endif
     c                   enddo
      **
     c                   endsr
      *****************************************************************
      **   DETERMINE IF TO BILL AT NET (GROSS - SYSTEM GEN ALWS)     **
      *****************************************************************
     c     billat        begsr
      **
     c     grsnet        cabeq     'G'           endnet
      **
     c                   if        grsnet = 'A'
     c     procky        chain     xffprocc                           79
     c                   if        *in79 = *off
     c****                             and (xfpubc < 110 or xfpubc > 180)
     c                   callp     HBXBREV( xfpubc : revDS )
     c****               if        not isRmAndBoard
     c                   if        not revDS.isRmAndBoard
     c                   goto      endnet
     c                   endif
     c                   endif
     c                   endif
      **
     c     alwkey        setll     hbfalws
     c                   dou       *in79 = *on
     c     alwkey        reade     hbfalws                                79
     c                   if        *in79 = *off
     c                   if        bgaflg = 'S'
     c                   eval      bgdtam = bgdtam + bgatam
     c                   endif
     c                   endif
     c                   enddo
      **
     c     endnet        endsr
      *****************************************************************
     c     fillelecdata  begsr

      ** cleared above
     c*******************clear                   e5f1500
     c                   eval      B5TRAK = bbtrak
     c                   eval      B501A  = box01a
     c                   eval      B502   = bgname
     c                   eval      mdate = prbirt
     c                   exsr      srcmdy
     c                   eval      B503A  = ydate
     c                   if        box03m = 'X'
     c                   eval      B503BC = 'M'
     c                   else
     c                   eval      B503BC = 'F'
     c                   endif
     c                   eval      B504   = box04
     c                   eval      B505A  = bdpad1
     c                   eval      B505B  = bdpcty
     c                   eval      B505C  = bdpsta
     c                   eval      B505D  = bdpzp1
     c                   eval      B505E  = %char(bdphtl)
     c                   eval      B506   = rlcode
     c                   if        box07a = 'SAME'
     c                   eval      B507A  = bdpad1
     c                   eval      B507B  = bdpcty
     c                   eval      B507C  = bdpsta
     c                   eval      B507D  = bdpzp1
     c                   eval      B507E  = %char(bdphtl)
     c                   else
     c                   eval      B507A  = box07a
     c                   eval      B507B  = box07b
     c                   eval      B507C  = box07c
     c                   eval      B507D  = box07d
     c                   eval      B507E  = %char(box07e)
     c                   endif
     c                   eval      B508A  = bdpmst
     c                   eval      B508B  = mmpesi
     c                   eval      B509   = prisd2
     c                   eval      B509A  = prtpol
     c                   eval      B509B1 = sb2bdt
     c*****              eval      B509C  = box09c
     c                   eval      B509D  = prinm2
     c                   if        box10ay = 'X'
     c                   eval      B510A  = 'Y'
     c                   else
     c                   eval      B510A  = 'N'
     c                   endif
     c                   if        box10by = 'X'
     c                   eval      B510B1 = 'Y'
     c                   else
     c                   eval      B510B1 = 'N'
     c                   endif
     c                   eval      B510B2 = box10bs
     c                   if        box10cy = 'X'
     c                   eval      B510C  = 'Y'
     c                   else
     c                   eval      B510C  = 'N'
     c                   endif
     c                   eval      B510D  = box10d
     c                   eval      B511   = prgrpx
     c                   eval      B511A1 = sbbdte
     c                   if        box11am = 'X'
     c                   eval      B511A2 = 'M'
     c                   else
     c                   eval      B511A2 = 'F'
     c                   endif
     c*****              eval      B511B  = box11b
     c                   eval      B511C  = box11c
     c                   eval      B514   = injdte
     c                   eval      B515Q  = box15q
     c                   eval      B515   = box15dte
     c                   eval      B516A  = 0
     c                   eval      B516B  = 0
     c                   eval      B517   = prrphy
     c                   eval      B517A  = prrupn
     c                   eval      B517B  = prrnpi
     c****               eval      B518A  = 0
     c****               eval      B518B  = 0
     c                   eval      B518A  = box18a
     c                   eval      B518B  = box18b
     c                   eval      B519   = box19
     c                   if        box20y = 'X'
     c                   eval      B520   = 'Y'
     c                   else
     c                   eval      B520   = 'N'
     c                   endif
     c                   eval      B520B  = 0
     c****               eval      B521A  = dxv(1)
     c****               eval      B521B  = dxv(2)
     c****               eval      B521C  = dxv(3)
     c****               eval      B521D  = dxv(4)
     c                   eval      B521A  = ediag(1)
     c                   eval      B521B  = ediag(2)
     c                   eval      B521C  = ediag(3)
     c                   eval      B521D  = ediag(4)
     c                   eval      B522A  = *blanks
     c****               eval      B522B  = *blanks
     c                   eval      B522B  = box22b
     c                   eval      B523   = prauth
     c                   eval      B5251  = taxid
     c                   eval      B5252  = box25e
     c                   eval      B526   = box26
     c                   if        box27y = 'X'
     c                   eval      B527   = 'Y'
     c                   else
     c                   eval      B527   = 'N'
     c                   endif
     c                   eval      B528   = totchg
     c                   eval      B529   = totpmt
     c                   eval      B530   = baldue
     c                   eval      B531A  = physicianName
     c                   eval      mdate = rptdte
     c                   exsr      srcmdy
     c                   eval      B531B  = ydate
     c                   eval      B532A1 = box321
     c                   eval      B532A2 = box322
     c                   eval      B532A3 = box323
     c                   eval      B532A4 = box324
     c                   eval      B532A5 = box325
     c                   eval      B532A6 = box325e
     c                   eval      B532A  = box32a
     c                   eval      B532B  = box32b
     c                   eval      B533   = %char(lvl2tl)
     c                   eval      B533A1 = lvl2nm
     c                   eval      B533A2 = lvl2ad
     c                   eval      B533A3 = lvl2ct
     c                   eval      B533A4 = lvl2st
     c                   eval      B533A5 = lvl2zp
     c                   eval      B533A6 = lvl2z2
     c                   eval      B533A  = box33a
     c                   eval      B533B  = prtprv
     c**
     c                   if        exc935
     c                   eval      B533   = wx33tl
     c                   eval      B533A1 = wx33nm
     c                   eval      B533A2 = %trim(wx33a1)+' '+%trim(wx33a2)
     c                   eval      B533A3 = wx33ct
     c                   eval      B533A4 = wx33st
     c                   eval      B533A5 = wx33z5
     c                   eval      B533A6 = wx33z4

     c                   eval      b5FfTl = wxFfTl
     c                   eval      b5FfA1 = wxFfNm
     c                   eval      b5FfA2 = %trim(wxFfA1)+' '+%trim(wxFfA2)
     c                   eval      b5FfCt = wxFfCt
     c                   eval      b5FfSt = wxFfSt
     c                   eval      b5FfZp = wxFfZp
     c                   endif

      *** Get diagnosis codes
     c                   call      'XFXDIAG'
     c                   parm      bbplv6        lv6#
     c                   parm      bbaccn        acct#
     c                   parm      bbtodt        rdate
     c                   parm                    diag
     c                   parm                    poaf
     c                   parm      *blanks       bodyp
     c                   parm                    icdVer

     c                   eval      b21icd = icdind

     c                   write     e5f1500

     c                   endsr

      **********************************************************************
      ** SRSETUPBOX21 - Setup the diagnoses in box21 (DXV) from hblchg15 file
      **                also set up the 5010 diagnoses (EDIAG)
      **                DXV will have the . inserted by XFXDXPER
      **                EDIAG will be sent through to 5010
      **********************************************************************
      /free
        begsr srsetupbox21;

          foundPrcrt = *off;
          dxv = *blanks;
          ediag = *blanks;
          foundURDiag = *off;
          if icdVer = '09';
          icdind = '9';
          else;
          icdind = '0';
          endif;

          if exc959 = *on;
            setgt (bbplv6 : bbaccn : bbiseq : bbfrdt) hmfuraut;
            dou %eof(hmlurautb);
              readpe (bbplv6 : bbaccn : bbiseq) hmfuraut;
              if not %eof(hmlurautb);
                if uradlt <> ' '
                   or urasts = 'P'
                   or urasts = 'L'
                   or urasts = 'D';
                  iter;
                endif;

                if uraicd <> ' ';
                  dxv(1) = uraicd;
                  ediag(1) = uraicd;
                  foundURDiag = *on;
                endif;

                leave;

              endif;
            enddo;

            if foundURDiag = *on;
              leavesr;
            endif;
          endif;

         // when BE 992 and BE 996, check if any procedure has
         // stepdown by minutes reimbursement as PRCRT.
         if exc992 and exc996;
           setll chgKey hbfchr15;
           dou %eof(hblchg15);
             reade chgKey hbfchr15;
             if not %eof(hblchg15);
               if bgReim = 'PRCRT';
                 foundPrcrt = *on;
                 leave;
               endif;
             endif;
           enddo;
         endif;

         if exc763 = *off;
          setll chgkey hbfchr15;
          dou %eof(hblchg15);
            reade chgkey hbfchr15;
            if not %eof(hblchg15);

              action = *blanks;
              exsr srcgbl;
              if action = 'N';
                iter;
              endif;

              chain cdsckey hbfcdgn;
              if not %found(hbpcdgn);
                bgcdg2 = *blanks;
                bgcdg3 = *blanks;
                bgcdg4 = *blanks;
              endif;

              // BE 992 and BE 996 are on, and procedure has Stepdown
              // reimbursement PRCRT, then skip below to populate
              // treatment diagnosis codes
              if not foundPrcrt;
                if exc960 = *on and bgdicd = *blanks;
                  bgdicd = dxs(1);
                  bgcdg2 = dxs(2);
                  bgcdg3 = dxs(3);
                  bgcdg4 = dxs(4);
                endif;

                if (exc981 = *on);
                  bgdicd = dxs(1);
                  bgcdg2 = dxs(2);
                  bgcdg3 = dxs(3);
                  bgcdg4 = dxs(4);
                endif;
              endif;

              exsr srsetupchrgdxs;

              for xx = 1 to 4;
                if chrgdxs(xx) <> *blanks and %lookup(chrgdxs(xx) : ediag) = 0;
                  dc = %lookup(*blanks : dxv);
                  if dc <> 0;
                                      ///xfxdxper (chrgdxs(xx) : returndx);///
                    if exc714 = *off;
                      xfxdxper (chrgdxs(xx) : icdver : returndx);
                      dxv(dc) = returndx;
                    else;
                      dxv(dc) = chrgdxs(xx);
                    endif;
                    ediag(dc) = chrgdxs(xx);
                  endif;
                endif;
              endfor;

            endif;
          enddo;
         else;
          exsr except763;
         endif;
        endsr;
      /end-free

      **------------------------------------------------------------------------
      // FILLPOINTER - fill in the diagnosis pointers for paper (A-L) and 5010 (1-12)
      //  The z variable is the current line it is working on...
      **------------------------------------------------------------------------
      /free
        begsr fillpointer;

          ebillptr = *blanks;

          // For exception 959, set all charges to UR diagnosis code
          if exc959 = *on and foundURDiag = *on;
            dc = 1;
            dix(z) = %trim(dix(z)) + %subst(alphabet : dc : 1);  //Fill in A-L for 1500 (02-12)
            xy = %lookup(*blanks : ebillptr);                    //Find blank position in 5010 ptr
            ebillptr(xy) = %char(dc);                            //Fill in 1-12 for 5010 billing
            leavesr;
          endif;

          // Not exception 959, get diagnosis codes from charges and set pointers
          chain cdsckey hbfcdgn;
          if not %found(hbpcdgn);
            bgcdg2 = *blanks;
            bgcdg3 = *blanks;
            bgcdg4 = *blanks;
          endif;

          // BE 992 and BE 996 are on, and procedure has Stepdown
          // reimbursement PRCRT, then skip below to populate
          // treatment diagnosis codes
          if not foundPrcrt;
            // Exception 960, get diagnosis codes from account
            if exc960 = *on and bgdicd = *blanks;
              bgdicd = dxs(1);
              bgcdg2 = dxs(2);
              bgcdg3 = dxs(3);
              bgcdg4 = dxs(4);
            endif;

            // Pull the account diagnosis codes regardless of the existence
            // of charge diagnosis codes.
            if (exc981 = *on);
              bgdicd = dxs(1);
              bgcdg2 = dxs(2);
              bgcdg3 = dxs(3);
              bgcdg4 = dxs(4);
            endif;
          endif;

          exsr srsetupchrgdxs;

         //Set up the diagnoses and the diagnosis pointers (1-12 for 5010, A-L for 02-12)
          for xx = 1 to 4;
            if chrgdxs(xx) <> *blanks;
              dc = %lookup(chrgdxs(xx) : ediag);                   //Find same diagnosis to point to
              if dc > 0;
                dix(z) = %trim(dix(z)) + %subst(alphabet : dc : 1);  //Fill in A-L for 1500 (02-12)
                xy = %lookup(*blanks : ebillptr);                    //Find blank position in 5010 p
                ebillptr(xy) = %char(dc);                            //Fill in 1-12 for 5010 billing
              endif;
            endif;
          endfor;
        endsr;
      /end-free

      **------------------------------------------------------------------------
      // SRSETUPCHRGDXS - setup the chrgdxs array
      **------------------------------------------------------------------------
      /free
        begsr srsetupchrgdxs;
          if exc763 = *off;
            chrgdxs(1) = bgdicd;
            chrgdxs(2) = bgcdg2;
            chrgdxs(3) = bgcdg3;
            chrgdxs(4) = bgcdg4;
          else;
            for i = 1 to ex763Ct;
              if ds763(i).chgSeq = bgdseq;
                chrgdxs(1) = ds763d(i).dg1;
                chrgdxs(2) = ds763d(i).dg2;
                chrgdxs(3) = ds763d(i).dg3;
                chrgdxs(4) = ds763d(i).dg4;
                leave;
              endif;
            endfor;
          endif;
        endsr;
      /end-free
      *****************************************************************
      *****************************************************************
      /EJECT
      *****************************************************************
      **   DETERMINE IF THERE ARE ANY BILL FORM PRINTING EXCEPTIONS  **
      *****************************************************************
      **
      **  CODEDESCRIPTION
      **
      **    01   PRINT SOCIAL SECURITY NUMBER IN BOX 1A
      **
      **    02   PRINT EMPLOYER NAME IN BOX 4
      **
      **    03   PRINT EMPLOYER ADDRESS, CITY, STATE, ZIP AND PHONE #
      **         IN BOXES 7 A,B,C,D,E
      **
      **    04   INITIALIZE DETAIL"TO DATE" FROM DETAIL "FROM DATE"
      **
      **    05   PRINT CATEGORY TWO WITH LICENSE NUMBER
      **
      **    06   PRINT LEVEL 6 ADDRESS IN BOX 32
      **
      **    07   PRINT EIGHT DIGIT DATES - DETAIL LINES
      **
      **    08   PRINT DIAGS ON DETAIL LINES, BOX 21 BLANK
      **
      **    09   PRINT "PFTH" IN BOX 11C
      **
      **    10   DO NOT PRINT PAYMENTS IN BOX 29
      **
      **    11   PRINT STAFF MEMBER PROVIDER NUMBER IN BOX 24K
      **
      **    12   PRINT STAFF MEMBER PROVIDER NUMBER IN BOX 33 PIN#
      **
      **    13   PRINT LOCATION PROVIDER NUMBER BOX 33 GROUP#
      **
      **    14   PRINT PAYOR RESOURCE NUMBER IN BOX 17
      **
      **    15   PRINT TAX-ID IN BOX24K
      **
      **    16   PRINT STAFF MEMBER UPIN NUMBER IN BOX 24K
      **
      **    17   PRINT STAFF MEMBER MEDICAID LICENSE IN BOX 24K
      **
      **    18   PRINT DEFAULT BC/BS LICENSE CODE IN BOX 24K
      **
      **    19   PRINT DEFAULT HEALTH PLUS LICENSE IN BOX 24K
      **
      **    20   ALWAYS PRINT 'X' IN BOX 20 OUTSIDE LAB
      **
      **    21   PRINT SIX DIGIT BIRTH DATE (*IN62)
      **
      **    22   PRINT SIX DIGIT SUBSCRIBER BIRTH DATE (*IN63)
      **
      **    23   PRINT SIX DIGIT SUBSCRIBER 2 BIRTH DATE (*IN64)
      **
      **    24   PRINT SIX DIGIT ADMIT DATE (*IN65)
      **
      **    25   PRINT SIX DIGIT DIAGNOSIS DATE (*IN66)
      **
      **    26   PRINT SIX DIGIT REPORT/BILL DATE (*IN67)
      **
      **    27   PRINT ALTERNATE # OF DETAIL LINES (NOT 6)
      **         (# OF LINES IS IN BOX # FIELD)
      **
      **    28   PRINT TOTAL CHARGES ON EACH PAGE FOR NON-PRIMARY
      **         BILLS
      **
      **    31   DON'T PRINT DETAIL ON CHARGES THAT HAVE BEEN PAID
      **         (*IN69)
      **
      **    34   PRINT ONLY THE LAST 8 DIGITS OF THE BILL TRACKING
      **         NUMBER
      **
      **    48   PRINT SEPARATE BILL FOR EACH DIAGNOSIS CODE
      **
      **    59   SUMMARIZE CHARGES BY PROC FOR CONSECUTIVE DATES OF SERVICE
      **
      **    61   PRINTS DETAIL DATES AS MM DD YY
      **
      **    62   STOPS HEADING PRINTING (PRIMARY, SECONDARY ETC AS WELL
      **          INITIAL, REBILL ETC)
      **
      **    63   PRINT CASE MANAGER ID NUMBER IN BOX 17A AND NAME IN 17
      **
      **    64   PRINT CLINIC PROVIDER# IN BOX 24K
      **
      **    65   PRINT RESOURCE FIELD IN BOX 26
      **
      **    117  DON'T PRINT BOX 14
      **
      **    120  PRINT MG BEFORE POLICY NUMBER (BOX 9A)
      **
      **    130  PRINT CO-CODE UNLESS RESIDENT HAS LAB CHARGES (BOX 9D)
      **
      **    131  PRINT SECONDARY CHARGE DESCRIPTION IN BOX 24D
      **
      **    158  PRINT BLANKS 9-10D COMPLIMENTARY CROSSOVER PATIENTS
      **
      **    159  PRINT CLIA NUMBER IN BOX 23
      **
      **    370  PRINT NURSE PRACTITIONER STATE# IN BOX24
      **
      **    371  USE FIRST FIVE CHARACTERS OF NUSRE PRACTITIONER
      **         STATE# AS PIN# IN BOX 33
      **
      **    545  Print Level 6 Street Address In Box 32
      **
      **    834  USE PAYID, then NPI instead of PNPI then PAYID for
      **         ins 1, 2, and 3 NPI
      **
      **    854  Print Physician on Charge in Box 31
      **
      **    907  Print Box 18 using six digit date (MM DD YY)
      **
      **    910  Print 1 penny for G code when no gross amount
      **
      *****************************************************************
     c     bilexc        begsr
      **         ======    =====
     c                   eval      svpayr = expayr
     c                   eval      svplan = explan
     c                   move      exfc          svfc
     c                   eval      svrevc = exrevc
     c                   eval      svlvl6 = exlvl6
     c                   eval      svCat1 = exCat1
     c                   eval      svCat2 = exCat2
     c                   movel     exstat        svstat
     c                   eval      svRule = exRule
      **
     c                   move      'N'           found
     c****               eval      count = 63
     c****               eval      count = 127
     c                   eval      count = 511
      **
     c     again         tag
      **
     c     excpky        chain     hbfbfex                            78
     c                   if        *in78 = *off
     c                             and hedelt = ' '
      **
     c                   move      'Y'           found
     c                   select
      **
     c                   when      excode = 1
     c                   movel(p)  bdpss#        box01a
      **
     c                   when      excode = 2
     c     empkey        chain     xffcdfle                           77
     c                   if        *in77 = *off
     c                   movel(p)  xfcdsc        box04
     c                   endif
      **
     c                   when      excode = 3
     c     empkey        chain     xffcdfle                           77
     c                   if        *in77 = *off
     c                   movel(p)  xfcad1        box07a
     c                   movel(p)  xfccty        box07b
     c                   movel(p)  xfcsta        box07c
     c                   movel(p)  xfczip        box07d
     c                   eval      box07e = xfcbtl
     c                   endif
      **
     c                   when      excode = 4
     c                   eval      exc04 = *on
     c                   eval      omtype = 'P'
     c     exokey        chain     hbfbfxo
     c                   if        not %found(hbpbfxo) or bxodlt = 'D'
     c                   move      dtf(z)        dtt(z)
     c                   eval      pbt(z) = pbf(z)
     c                   move      dtf6(z)       dtt6(z)
     c                   move      dtf6(z)       wkdat6
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
     c                   else
     c                   eval      ydate = bbToDt
     c                   exsr      srcymd
     c                   move      mdate         dtt(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtt6(z)
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
     c                   endif
      **
     c                   when      excode = 5
     c     thrct2        cat(p)    prrg#:0       prprov
     c                   eval      exc05 = *on
      **
     c                   when      excode = 6
     c****               movel(p)  hx6a21        box321
     c****               movel(p)  hx6a22        box322
     c                   movel(p)  hx6a21        box322
     c                   movel(p)  hx6ct2        box323
     c                   movel(p)  hx6st2        box324
     c                   movel(p)  hx6z21        box325
     c                   movel(p)  hx6z22        box325e
      **
     c                   when      excode = 7
     c                             and runidx <> '2'
     c                   move      *on           *in58
      **
     c                   when      excode = 8
     c                   move      *on           *in59
      **
     c                   when      excode = 9
     c                   movel(p)  'PFTH'        box11c
      **
     c                   when      excode = 10
     c                   move      'Y'           exc10
      **
     c                   when      excode = 11
     c                   move      *on           *in11
     c                   eval      level6 = bbplv6
     c                   eval      therap = bgdtth
     c     thprk2        chain     hmfplnpr
     c                   if        %found(hmlplnp3)
     c                   movel     hmtpr#        pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
     c                   eval      hld24k = hmtth#
     c                   else
     c                   eval      level6 = 0
     c     thprk2        chain     hmfplnpr
     c                   if        %found(hmlplnp3)
     c                   movel     hmtpr#        pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
     c                   eval      hld24k = hmtth#
     c                   endif
     c                   endif
     c                   if        exc179 = 'Y'
     c                   eval      hld24k = bgdtth
     c                   exsr      sre179
     c                   endif
      **
     c                   when      excode = 12
     c                   eval      level6 = bbplv6
     c     thprky        chain     hmfplnpr                           77
     c                   if        *in77 = *on
     c                   eval      level6 = 0
     c     thprky        chain     hmfplnpr                           77
     c                   if        *in77 = *on
     c                   eval      hmtpr# = *blanks
     c                   endif
     c                   endif
     c                   movel(p)  hmtpr#        prprov
     c                   eval      exc12 = *on
      **
     c                   when      excode = 13
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   if        sndprv = 'D' or sndprv = 'U'
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   eval      prvid = 'NPI'
     c                   else
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
 001 c                   eval      prvid = 'PRVDR'
     c                   endif
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
 001 c     prvvar        ifne      *blanks
 001 c                   movel     prvvar        prgrp#
     c                   else
     c                   eval      prgrp# = *blanks
     c*>>                   eval      prmct1 = ' '
     c*>>                   eval      prmct2 = ' '
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
 001 c                   movel     'PRVNM'       prvid
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
     c                   cat       prvvar:1      lvl2nm
     c                   endif
      **
     c                   when      excode = 14
     c                   movel     prresc        prrupn
     c                   eval      prrphy = *blanks
      **
     c                   when      excode = 15
     c                   movel(p)  taxid         pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = 'EI'
     c                   endif
      **
     c                   when      excode = 16
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   eval      rqdoctor# = bgdtth
     c                   eval      rqlictype = 'UPIN'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = *blanks
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
      **
     c                   when      excode = 17
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   eval      rqdoctor# = bgdtth
     c                   eval      rqlictype = 'MCAID'
     c                   eval      rqstate = hx6st1
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = bgdct2
     c                   exsr      srlicense
     c                   movel     rtnlicense    pin(z)
     c                   if        pin(z) <> *blanks
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm                    rqlictype
     c                   parm                    bx24i(z)
     c                   if        bx24i(z) = *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
     c                   endif
      **
     c                   when      excode = 18
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   exsr      srctab
     c                   movel     'BLIC'        tcode
     c                   movel     bgdct1        ecode
     c                   exsr      srtabl
     c                   movel     ldesc         pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
      **
     c                   when      excode = 19
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   exsr      srctab
     c                   movel     'BLI2'        tcode
     c                   movel     bgdct1        ecode
     c                   exsr      srtabl
     c                   movel     ldesc         pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
      **
     c                   when      excode = 20
     c                   eval      box20y = *blanks
     c                   move      'X'           box20n
      **
     c                   when      excode = 21
     c                             and runidx <> '2'
     c                   move      *on           *in62
      **                                                    BIRTH DATE
     c                   when      excode = 22
     c                             and runidx <> '2'
     c                   move      *on           *in63
      **                                                    BIRTH DATE
     c                   when      excode = 23
     c                             and runidx <> '2'
     c                   move      *on           *in64
      **                                                    2 BIRTH DATE
     c                   when      excode = 24
     c                             and runidx <> '2'
     c                   move      *on           *in65
      **                                                    ADMIT DATE
     c                   when      excode = 25
     c                             and runidx <> '2'
     c                   move      *on           *in66
      **                                                    DIAGNOSIS DATE
     c                   when      excode = 26
     c                             and runidx <> '2'
     c                   move      *on           *in67
      **                                                    BILL DATE
     c                   when      excode = 27
     c                   move      exbox#        dtline
     c                   if        dtline <= 0
     c                             or dtline > 6
     c                   eval      dtline = 6
     c                   endif
      **
     c                   when      excode = 28
     c                   move      *on           *in68
      **
     c                   when      excode = 31
     c                   move      *on           *in69
      **
     c                   when      excode = 34
     c                   move      bbtrak        eight
     c                   movel(p)  eight         box26
      **
     c                   when      excode = 35
     c                   move      *off          *in24
     c                   eval      box11dn = ' '
     c                   eval      box11dy = ' '
      **
     c                   when      excode = 38
     c                   eval      box04 = *blanks
     c                   eval      box07a = *blanks
     c                   eval      box07b = *blanks
     c                   eval      box07c = *blanks
     c                   eval      box07d = *blanks
     c                   eval      box07e = 0
     c                   eval      wklvl6 = 0
     c                   eval      prgrpx = *blanks
     c                   eval      sbbdte = 0
     c                   eval      box11af = *blanks
     c                   eval      box11am = *blanks
     c                   eval      box11b = *blanks
     c                   eval      box11bq = *blanks
     c                   eval      box11c = *blanks
     c                   eval      box11dy = *blanks
     c                   eval      box11dn = *blanks
     c                   eval      box13 = *blanks
     c                   eval      exc38 = *on
      **
     c                   when      excode = 39
     c                   move      *on           *in40
     c                   eval      box20y = *blanks
     c                   eval      box20n = *blanks
     c                   movea     *blanks       dxd
      **
     c                   when      excode = 40
      **
     c                   when      excode = 41
     c                   movea     *blanks       dix
     c                   movea     *blanks       ddx
      **
     c                   when      excode = 42
     c                   eval      prinam = *blanks
     c                   eval      wrkpyr = 0
     c                   eval      wrkpln = 0
     c                   eval      priadr = *blanks
     c                   eval      priad2 = *blanks
     c                   eval      pricty = *blanks
     c                   eval      prista = *blanks
     c                   eval      prizip = *blanks
     c                   move      *on           *in13
      **
     c                   when      excode = 48
     c                   move      *on           *in14
      **
     c                   when      excode = 50
     c                             or excode = 59
     c                   if        not exc04
     c                   eval      ydate = bgdfsd
     c                   exsr      srcymd
     c                   move      mdate         dtt(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtt6(z)
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
     c                   endif
      **
     c****               when      excode = 60
     c****               move      'P'           omtype
     c**** exokey        chain     hbfbfxo                            79
     c****               if        *in79 = *on
     c****                         or bxodlt = 'D'
     c****               move      *on           *in37
     c****               endif
      **
     c                   when      excode = 61
     c                   move      *on           *in60
      **
     c                   when      excode = 62
     c                   eval      insbil = *blanks
     c                   eval      tobill = *blanks
      **
     c                   when      excode = 63
      **
     c                   eval      prrupn = *blanks
     c                   eval      prrphy = *blanks
     c                   clear                   hmfasgn
     c                   move      'CM'          status
     c     physky        setgt     hmfasgn
     c     tryagn3       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c     msgdlt        cabeq     'D'           tryagn3
     c     msgdr#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      box17q = 'DQ'
     c                   movel     hmdnam        prrphy
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'STATE'
     c                   eval      rqstate = hx6st1
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   movel     rtnlicense    prrupn
     c                   endif
     c                   endif
      **
     c                   when      excode = 64
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
 001 c                   eval      prvid = 'PRVDR'
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
 001 c     prvvar        ifne      *blanks
 001 c                   movel     prvvar        pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
     c                   else
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
 001 c                   movel     'PRVNM'       prvid
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
     c                   cat       prvvar:1      lvl2nm
     c                   endif
      **
     c                   when      excode = 65
     c                   movel(p)  prresc        box26
      **
     c                   when      excode = 71
     c                   eval      *in30 = *on
     c                   eval      amt(z) -= chPRsp
     c                   eval      bbAmnt -= chPRsp
     c                   eval      totChg -= chPRsp
      **
     c                   when      excode = 72
     c                   eval      *in49 = *on
      **
     c                   when      excode = 73
     c                   movel(p)  bbaccn        box26
      **
     c                   when      excode = 74
     c                   eval      box12  = *blanks
     c                   eval      praddt = 0
     c                   eval      pradt6 = 0
      **
     c                   when      excode = 75
     c                   exsr      excode75
      **
     c                   when      excode = 76
     c                   eval      box27y = ' '
     c                   eval      box27n = ' '
      **
     c                   when      excode = 77
     c                   eval      box321 = *blanks
     c                   eval      box322 = *blanks
     c                   eval      box323 = *blanks
     c                   eval      box324 = *blanks
     c                   eval      box325 = *blanks
     c                   eval      box325e = *blanks
     c                   eval      exc77 = *on
      **
     c                   when      excode = 78
     c                   eval      box25e = ' '
      **
     c                   when      excode = 79
          if %subst(prProv:3:1) <> ' ';
             prvId = 'PRVDR';
             XFXBQ15 (bbPayr:prvId:qua33b);
             prProv = qua33B + ' ' + prProv;
             prtPrv = prProv;
          endif;
      **
     c                   when      excode = 112
     c                   move      *on           *in27
      **
     c                   when      excode = 104
     c                   eval      dtline = 12
      **
     c                   when      excode = 105
     c****                   z-add     6             levl#
     c****                   eval      title = 'SUP'
     c****     repkey        chain     hxftitl                            79
     c****                   if        *in79 = *off
     c****                             and hxldlt <> 'D'
     c****                   movel     hxlnam        prthnm
     c                   callp     HXXTITLE( 6 : bbplv6 : 'SUP' : titleDS )
     c                   if        ttlName <> *blanks
     c                   eval      prthnm = ttlName
     c                   eval      prrg# = *blanks
     c                   endif
      **
     c                   when      excode = 113
                         clear resAd1;
                         clear resCty;
                         clear bdpAd1;
                         clear bdpCty;
                         clear bdpSta;
                         clear bdpZp1;
                         clear bdphTl;
      **
     c                   when      excode = 114
                         clear box06A;
                         clear box06B;
                         clear box06C;
                         clear box06D;
      **
     c                   when      excode = 116
                         clear box10ay;
                         clear box10an;
                         clear box10by;
                         clear box10bn;
                         clear box10cy;
                         clear box10cn;
                         clear box10bs;
      **
     c                   when      excode = 117
     c                   move      *on           *in29
      **
     c                   when      excode = 118
     c                   eval      prAuth = *blanks
     c                   eval      bbAuth = *blanks
      **
     c                   when      excode = 121
     c                   eval      prtpol = bbplcy
     c                   move      *off          *in24
     c                   eval      box11dn = ' '
     c                   eval      box11dy = ' '
      **
     c                   when      excode = 126
     c                   eval      box10D = *blanks
      **
     c                   when      excode = 131
     c     cdsckey       chain     hbfcdsc                            79
     c                   if        *in79 = *off
     c                   movel(p)  bgcdsc        cds(z)
     c                   eval      *in38 = *on
     c                   else
     c                   eval      *in38 = *off
     c                   endif
      **
     c                   when      excode = 152
     c                   eval      prgrpx = *blanks
     c                   eval      sbbdte = 0
     c                   eval      box11af = *blanks
     c                   eval      box11am = *blanks
     c                   eval      box11b = *blanks
     c                   eval      box11bq = *blanks
     c                   eval      box11c = *blanks
     c                   eval      box11dy = *blanks
     c                   eval      box11dn = *blanks
     c                   eval      exc152 = *on
      **
     c                   when      excode = 158
     c                             and *in24 = *on
     c                   select
     c                   when      ins# = 1
     c**
     c                   if        aub(2) <> 0
     c                   eval      cpayor = aub(2)
     c                   eval      cplan = apl(2)
     c     cpyrkey       chain     hbfcompc                           79
     c                   if        *in79 = *off
     c                   eval      *in24 = *off
     c                   eval      box11dn = ' '
     c                   eval      box11dy = ' '
     c                   endif
     c                   endif
     c                   if        aub(3) <> 0
     c                   eval      cpayor = aub(3)
     c                   eval      cplan = apl(3)
     c     cpyrkey       chain     hbfcompc                           79
     c                   if        *in79 = *off
     c                   eval      *in24 = *off
     c                   eval      box11dn = ' '
     c                   eval      box11dy = ' '
     c                   endif
     c                   endif
      **
     c                   when      ins# = 2
     c                   if        aub(3) <> 0
     c                   eval      cpayor = aub(3)
     c                   eval      cplan = apl(3)
     c     cpyrkey       chain     hbfcompc                           79
     c                   if        *in79 = *off
     c                   eval      *in24 = *off
     c                   eval      box11dn = ' '
     c                   eval      box11dy = ' '
     c                   endif
     c                   endif
     c                   endsl
      **
     c                   when      excode = 159
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
 001 c                   movel     'CLIA'        prvid
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
 001 c     prvvar        ifne      *blanks
 001 c                   movel(p)  prvvar        prauth
     c                   else
     c                   eval      payor = 0
     c                   eval      plan = 0
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
 001 c     prvvar        ifne      *blanks
 001 c                   movel(p)  prvvar        prauth
     c                   endif
     c                   endif
      **
      **
     c                   when      excode = 171
     c                   select
     c                   when      afc(1) = 'MD'
     c     'MCD'         cat(p)    apo(1):1      box10d
     c                   when      afc(2) = 'MD'
     c     'MCD'         cat(p)    apo(2):1      box10d
     c                   when      afc(3) = 'MD'
     c     'MCD'         cat(p)    apo(3):1      box10d
     c                   endsl
      **
     c                   when      excode = 172
     c     1             do        999           y
     c                   eval      ydate1 = bbfrdt
     c                   eval      ydays = y - 1
     c                   exsr      sraddd
     c                   eval      rqdate = ydate2
     c                   if        rqdate > bbtodt
     c                   leave
     c                   endif
     c                   exsr      srinfo
     c                   if        afcx(1) = 'MA'
     c                             or afcx(1) = 'MB'
     c                   movel(p)  'NONE'        prgrpx
     c                   eval      box11b = *blanks
     c                   eval      box11bq = *blanks
     c                   eval      box11c = *blanks
     c                   leave
     c                   endif
     c                   enddo
      **
     c                   eval      rqdate = bbtodt
     c                   exsr      srinfo
      **
      **                                      ** BOX 9 = 'SAME'
     c                   when      excode = 175
     c                   eval      prisd2 = 'SAME'
      **
     c                   when      excode = 176
     c                   eval      prgrp# = prprov
     c                   eval      prprov = *blanks
     c                   eval      exc176 = *on
      **
     c                   when      excode = 177
     c                   eval      lstdte = lsdate
     c                   eval      lic# = licnum
      **
     c                   when      excode = 179
     c                   if        hld24k <> 0
     c                   exsr      sre179
     c                   else
     c                   eval      exc179 = 'Y'
     c                   endif
      **
 002 c** PUT IN ATTENDING PHYSICIAN FROM INPATIENT ACCOUNT
 002 c                   when      excode = 181
 001 c                   if        lic# = *blanks
     c                   call      'XFXCHKMRN'
     c                   parm      bbmrno        medrec            9 0
     c                   parm      0             begdat            8 0
     c                   parm      99999999      enddat            8 0
     c                   parm      0             levl6             6 0
     c                   parm      0             actnum           12 0
     c**
     c                   if        sndprv = 'D' or sndprv = 'U'
     c                   eval      rqlictype = 'NPI'
     c                   else
B001 c                   if        fincls = 'MD'
 001 c                   eval      rqlictype = 'MCAID'
 001 c                   eval      rqstate = hx6st1
X001 c                   else
 001 c                   eval      rqlictype = 'UPIN'
 001 c                   eval      rqstate = *blanks
E001 c                   endif
E001 c                   endif
     c**
     c                   eval      status = *blanks
     c                   eval      srlv6 = levl6
     c                   eval      sract = actnum
     c                   exsr      srasgn
     c     doc#          chain     hmfmams                            79
B001 c                   if        *in79 = *off
     c                   eval      rqdoctor# = doc#
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    lic#
E001 c                   endif
     c                   endif
      **
     c                   when      excode = 186
     c                   eval      exc186 = *on
      **
     c                   when      excode = 189
     c                   eval      exc189 = *on
      **
     c                   when      excode = 665
     c                   eval      exc665 = *on
      **
     c                   when      excode = 190
     c                   eval      prmprc = *blanks
     c                   movel(p)  'ALTTX'       prvid
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      taxid = prvvar
     c                   movel(p)  prvvar        HX6TID
     c                   endif
      **
     c                   when      excode = 193
     c                   eval      dattmp = bbtodt
     c                   eval      lv6tmp = bbplv6
     c                   eval      acttmp = bbaccn
     c                   exsr      srtrfr
     c     rmadky        chain     hxfrmad                            77
     c                   if        *in77 = *off
     c                   movel(p)  rmaad1        bdpad1
     c                   movel(p)  rmacty        bdpcty
     c                   movel(p)  rmaad1        resad1
     c                   movel(p)  rmacty        rescty
     c                   movel(p)  rmasta        bdpsta
     c                   movel(p)  rmazp1        bdpzp1
     c                   else
      **
     c                   call      'XFXCHKMRN'
     c                   parm                    mmmrno
     c                   parm                    bbfrdt
     c                   parm                    bbtodt
     c                   parm      0             lv6tmp
     c                   parm      0             acttmp
      **
     c                   exsr      srtrfr
     c     rmadky        chain     hxfrmad                            77
     c                   if        *in77 = *off
     c                   movel(p)  rmaad1        bdpad1
     c                   movel(p)  rmacty        bdpcty
     c                   movel(p)  rmaad1        resad1
     c                   movel(p)  rmacty        rescty
     c                   movel(p)  rmasta        bdpsta
     c                   movel(p)  rmazp1        bdpzp1
     c                   endif
     c                   endif
      **
     c                   when      excode = 195
     c                   eval      prmprc = *blanks
     c                   movel(p)  'ALTTX'       prvid
     c                   exsr      srprv
     c                   if        prvvar <> *BLANKS
     c                   movel(p)  prvvar        hx6tid
     c                   movel(p)  prvvar        BOX26
     c                   endif
      **
     C                   WHEN      EXCODE = 196
     c                             and exc837 = *off
     c                   eval      exc196 = *on
     c                   eval      xfpubc = 0
     c     procky        chain     xffprocc
     c                   if        %found(hxpprocc)
     c     mmpct1        chain     hxfctg1
     c                   if        %found(hxpctg1)
     c                             and xfpnub <> 0
     c                             and (hxprty='O'
     c                               or hxprty='E')
     c                   eval      xfpubc = xfpnub
     c                   endif
     c                   endif
     C     IVSKEY        SETLL     HAFIVSVC
     C                   DOU       %eof(halivsvc)
     C     IVSKEY        READE     HAFIVSVC
     C                   IF        not %eof(halivsvc)
     C                             AND BSVDLT = *BLANKS
     C                             and bsvioa <> 'Y'
     C                             AND BSVEFF <= BGDTSD
     C                             AND BGDTSD <= BSVEND
     c                             and ((bsvprc = bgdtpr and bsvprc <> *blanks)
     c                              or (bsvrvc = xfpubc and bsvrvc <> 0)
     c                              or (bsvptd = 'Y' and
     c                               (xfpubc >= 420 and xfpubc <= 429))
     c                              or (bsvotd = 'Y' and
     c                               (xfpubc >= 430 and xfpubc <= 439))
     c                              or (bsvspd = 'Y' and
     c                               (xfpubc >= 440 and xfpubc <= 449))
     c                              or (bsvswd = 'Y' and
     c                               (xfpubc >= 560 and xfpubc <= 569))
     c                              or (bsvbhd = 'Y' and
     c                               (xfpubc >= 900 and xfpubc <= 918))
     c                              or (bsvacd = 'Y' and xfpubc = 2101)
     c                              or (bsvtrd = 'Y' and xfpubc = 941)
     c                              or (bsvnrd = 'Y' and xfpubc = 230)
     c                              or (bsvcmd = 'Y' and
     c                               (xfpubc >= 942 and xfpubc <= 944))
     c                              or (bsvvcd = 'Y' and xfpubc = 948)
     c                              or (bsvphd = 'Y' and xfpubc = 960)
     c                              or (bsvtcd = 'Y' and xfpubc = 940))
     C                   IF        BSVAUT <> *BLANKS
     C                   MOVEL(P)  BSVAUT        PRAUTH
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     **
     c                   when      excode = 200
     c                   clear                   hmfasgn
     c                   move      'RF'          status
     c     physky        setgt     hmfasgn
     c     tryagn4       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c     msgdlt        cabeq     'D'           tryagn4
     c     msgdr#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   movel     hmdnam        prthnm
     c                   endif
     c                   endif
      **
     c                   when      excode = 209
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
 001 c                   movel     'RAIL'        prvid
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
 001 c     prvvar        ifne      *blanks
 001 c                   movel(p)  prvvar        pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
     c                   else
     c                   eval      payor = 0
     c                   eval      plan = 0
     c                   eval      prmlv6 = bbplv6
 001 c                   exsr      srprv
 001 c     prvvar        ifne      *blanks
 001 c                   movel(p)  prvvar        pin(z)
     c                   if        pin(z) <> *blanks
     c                   eval      bx24i(z) = '1C'
     c                   endif
     c                   endif
     c                   endif
      **
     c                   when      excode = 212                                 =descend by rate
     c                   exsr      chkmod
     c                   if        modlty = 'Y'
     c                   exsr      mltmod
     c                   move      'Y'           exc212
     c                   endif
      **
     c                   when      excode = 214
     c                   move      'I'           omtype
     c     exokey        chain     hbfbfxo                            79
     c                   if        *in79 = *off
     c                             and bxodlt <> 'D'
     c                   z-add     1             qty(z)
     c                   z-add     1             qty4(z)
     c                   z-add     1             qty2(z)
     c                   z-add     1             qty5(z)
     c                   eval      bgdtqt = 1
     c                   endif
      **
     c                   when      excode = 220
     c                   eval      box27y = ' '
     c                   eval      box27n = 'X'
      **
     c                   when      excode = 224
     c                   eval      exc224 = *on
      **
     c                   when      excode = 225
     c                   eval      priatn = wrkatn
      **
     c                   when      excode = 227
     c                   move      'I'           omtype                         =include procs
     c     exokey        chain     hbfbfxo                            76
     c                   if        *in76 = *off
     c                             and bxodlt <> 'D'
     c                   move      'CM'          status
     c                   exsr      srasgn
     c                   if        msgdr# <> 0
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'MCAID'
     c                   eval      rqstate = hx6st1
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   if        prprov = *blanks
     c                   movel     rtnlicense    prprov
     c                   endif
     c                   movel     rtnlicense    pin(z)                         =box24k
     c                   eval      bx24i(z) = *blanks
     c                   if        pin(z) <> *blanks
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm                    rqlictype
     c                   parm                    bx24i(z)
     c                   endif
     c                   else
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   endif
     c                   endif
     c                   else
     c                   movel     bgdtpr        prmprc
     c                   movel     'OPVDR'       prvid
     c                   exsr      srprv
     c                   movel     prvvar        pin(z)
     c                   eval      bx24i(z) = *blanks
     c                   if        pin(z) <> *blanks
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm      'PRVDR'       prvid
     c                   parm                    bx24i(z)
     c                   endif
     c                   endif
      **
     c                   when      excode = 287
     c                   move      'P'           omtype                         =proc include
     c     exokey        chain     hbfbfxo
     c                   if        not %found
     c                             or bxodlt = 'D'
     c                   movel(p)  bgdtpr        prmprc
     c                   if        bgdtqt > 999
     c                   eval      prmInu = bgdtqt
     c                   else
     c                   eval      prmInu = qty(z)
     c                   endif
     c                   exsr      srucnv
     c                   z-add     prmInu        qty(z)
     c                   z-add     prmInu        qty4(z)
     c                   z-add     prmInu        qty2(z)
     c                   z-add     prmInu        qty5(z)
     c                   eval      *in75 = *on
     c                   endif
      **
     c                   when      excode = 301
     c                   eval      mod(z) = *blanks
      **
     c                   when      excode = 317
     c                   eval      *in73 = *on
      **
     c                   when      excode = 319
     c                   callp     HXXTITLE( 5 : bbpLv5 : 'REP' : titleDS )
     c                   if        ttlName <> *blanks
     c                   clear                   nameOut
     c                   callp     XFXSNAM( ttlName : nameOut )
     c     namF          CAT       namL:1        prtCod
     c                   endif
      **
     c                   when      excode = 325
     c                   eval      box01x1 = ' '
     c                   eval      box01x2 = 'X'
     c                   eval      box01x3 = ' '
     c                   eval      box01x4 = ' '
     c                   eval      box01x5 = ' '
     c                   eval      box01x6 = ' '
      **
     c                   when      excode = 328
           box321 = mmName;
           box322 = bdpAd1;
           box323 = bdpCty;
           box324 = bdpSta;
           box325 = bdpZp1;
           if bdpZp2 = *blanks;
             box325e = '0000';
           else;
             box325e = bdpZp2;
           endif;
      **
     c                   when      excode = 336
     c     mastky        chain     hmfmast                            79
     c                   if        *in79 = *off
     c                             and mmprfd <> 0
     c                   z-add     mmprfd        ydate
     c                   exsr      srcymd
     c                   move      mdate         lstdte
     c                   else
     c                   eval      lstdte = *blanks
     c                   endif
      **
     c                   when      excode = 343
     c                   move      'Y'           exc343
      **
     C                   when      excode = 348
     C                   move      'Y'           prtpmt
      **
     c                   when      excode = 349
     c                   if        %trim(prthnm) = sign
     c                   clear                   prthnm
     c                   endif
      **
     c                   when      excode = 370
     c                   move      'NP'          status
     c     physky        setgt     hmfasgn
     c     readag3       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c                   if        msgdlt = 'D'
     c                   clear                   hmfasgn
     c                   goto      readag3
     c                   else
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'STATE'
     c                   eval      rqstate = hx6st1
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   movel     rtnlicense    pin(z)
     c                   eval      bx24i(z) = '1C'
     c                   else
     c                   eval      rqstate = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   movel     rtnlicense    pin(z)
     c                   eval      bx24i(z) = '1C'
     c                   else
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      **
     c                   when      excode = 371
     c                   movel     pin(z)        tmp5char
     c                   movel(p)  tmp5char      prprov
      **
     c                   when      excode = 399
      **
     c                   eval      prrupn = *blanks
     c                   clear                   hmfasgn
     c                   move      'RF'          status
     c     physky        setgt     hmfasgn
     c     tryagn5       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c     msgdlt        cabeq     'D'           tryagn5
     c     msgdr#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'UPIN'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    prrupn
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm                    rqlictype
     c                   parm                    qua17a
     c                   endif
     c                   endif
      **
     c                   when      excode = 400
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = savrev
     c                   eval      prmprc = svproc
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
     c                   movel     'PRVDR'       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box32b
     c     '1C'          cat       box32b:1      box32b
     c                   endif
      **
     c                   when      excode = 401
     c                   movel     prprov        prtprv
     c                   eval      exc401 = *on
      **
     c                   when      excode = 417
     c                   eval      exc417 = *on
      **
     c                   when      excode = 419
     c                   movel     bgdtpr        prmprc
     c                   movel     'OPVDR'       prvid
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      prprov = prvvar
     c                   movel(p)  prvvar        pin(z)
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm      'PRVDR'       prvid
     c                   parm                    bx24i(z)
     c                   movel(p)  prvvar        bbprov
     c                   else
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   endif
      **
     c                   when      excode = 420
     c                   movel     bgdtpr        prmprc
     c                   movel     'ALTGP'       prvid
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel(p)  prvvar        pin(z)
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm      'PRVDR'       prvid
     c                   parm                    bx24i(z)
     c                   else
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   endif
      **
     c                   when      excode = 423
     c                   movel     bgdtpr        prmprc
     c                   movel     'ALTGP'       prvid
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel(p)  prvvar        prtprv
     c                   endif
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm      'PRVDR'       prvid
     c                   parm                    qua33b
      **
     c                   when      excode = 425
     c                   eval      box32A = *blanks
     c                   eval      exc425 = *on
      **
     c                   when      excode = 440
     c                   eval      npi(z) = *blanks
      **
     c                   when      excode = 441
     c                   movel     hx1nam        prthnm
      **
     c                   when      excode = 442
     c                   eval      lvl2tl = hx1phn
     c                   eval      lvl2nm = hx1nam
     c                   eval      lvl2ad = hx1ad1
     c                   eval      lvl2ct = hx1ct1
     c                   eval      lvl2st = hx1st1
     c                   movel     hx1zp1        lvl2zp
     c                   movel     hx1zp2        lvl2z2
      **
     c                   when      excode = 443
     c                   move      'Y'           exc443
      **
     c                   when      excode = 444
     c                   eval      payor = 0
     c                   eval      plan = 0
     c                   eval      prmrev = 0
     c                   eval      prmprc = ' '
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        prrnpi
     c                   endif
      **
     c                   when      excode = 445
     c                   eval      payor = 0
     c                   eval      plan = 0
     c                   eval      prmrev = 0
     c                   eval      prmprc = ' '
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'NPI  '       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        npi(z)
     c                   endif
     c                   if        exc179 = 'Y'
     c                   eval      hld24k = bgdtth
     c                   exsr      sre179
     c                   endif
      **
     c                   when      excode = 446
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = ' '
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   movel     'PRVDR'       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        prtprv
     c                   endif
      **
     c                   when      excode = 447
     c                   movel     hx1nam        box321
     c                   movel     hx1nam        lvl2nm
      **
     c                   when      excode = 449
     c                   eval      lvl2tl = hx1phn
     **
     c                   when      excode = 452 or excode = 463
     c                   eval      include = *on
     c                   if        excode = 452
     c                   eval      omtype = 'P'
     c     exokey        chain     hbpbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D'
     c                   eval      include = *off
     c                   endif
     c                   endif
     c**
     c                   if        include
     c                   call      'HBBMNGC'
     c                   parm                    mmplv6
     c                   parm                    mmacct
     c                   parm      *blanks       mthlvl
     c                   parm      *blanks       chkmth
     c                   parm      0             rate
     c                   parm      0             coins
     c                   parm      bgctsd        xftdt
     c                   parm      bgcseq        chgseq
     c                   parm      *blanks       capalw
     c                   parm      ' '           rpaflg
     c                   parm      0             pctalw
     c                   parm      ' '           alwboa
     c                   parm      0             feeschd
     c                   parm      ' '           bnftexhst
     c                   parm      0             fspamt
     c                   parm      0             imeamt
     c                   parm      0             dshamt
     c                   parm      0             otlamt
     c                   parm      0             hrramt
     c                   parm      0             vbpamt
     c                   parm      0             bpmamt
     c                   parm      0             hacamt
     c                   parm      bgcct1        mmnct1
     c                   parm      bgcct2        mmnct2
     c                   parm      bgctc4        mmncpt
     c                   parm      *blanks       mmndag
     c                   parm      *blanks       mmnprc
     c                   parm      bgdtpr        mmncde
     c                   parm      *blanks       mmnvtp
     c                   parm      xfpubc        mmnrev
     c                   parm      0             mmndrg
     c                   parm      bgcrmc        mmnrmc
     c                   parm      bgcct3        mmnct3
     c                   parm      bgctqt        mmnqty
     c                   parm      bgctam        mmnamtp
     c                   parm      bbpayr        mmnubc
     c                   parm      bbplan        mmnpln
     c                   parm      bbplcy        mmnpol
     c                   parm      bbiseq        mmnisq
     c                   parm      bgctsd        mmnsvdp
     c                   parm                    mmnbdt
     c                   parm      bgcmod        mmnmod
     c                   parm      bgcct#        mmnct#
     c                   parm      bgctth        mmndoc
     c                   parm                    mmnpos
     c                   parm      *blanks       cycleflag
     c                   parm                    sndbfq
     c                   parm                    mmmrno
     c                   parm                    mmaddt
     c                   parm                    mmdsdt
     c                   parm      0             rtnmppr
     c                   parm      0             ancallow
     c                   parm      'X'           bypassPercent
     c                   parm      ' '           SNFVBPinfo
     c                   parm      *blanks       rtnReimType
     c**
     c                   if        rtnReimType = 'H'
     c                             or (rtnReimType = 'Z' and sndizc = 'Y')
     c     procky        chain     hxpprocc
     c                   if        not %found(hxpprocc)
     c     bgdtpr        chain     hxpprocm
     c                   endif
     c                   if        %found and xfptim <> 0
     c                   eval      wrktim = xfptim
     c     xfphr         mult      60            xfptme
     c                   add       xfpmn         xfptme
     c     xfptme        div(h)    60            factor
     c                   else
     c                   z-add     1             factor
     c                   endif
     c     factor        mult      bgdtqt        ex452q
     c                   z-add     ex452q        qty(z)
     c                   if        excode = 452
     c                   z-add     ex452q        qty2(z)
     c                   elseif    excode = 463
     c                   eval      qty2(Z) = %dech(ex452q:4:0)
     c                   endif
     c                   z-add     ex452q        qty4(z)
     c                   z-add     ex452q        qty5(z)
     c                   move      *on           *in75
     c                   eval      exc452 = *on
     c                   endif
     c                   endif
     **
     c                   when      excode = 466
     c                             and not exc885
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = bgdtpr
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'TAXON'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm                    prvid
     c                   parm                    qua33b
     c                   eval      prprov = qua33b + ' ' + prvvar
     c                   if        exc401 = *on
     c                   movel     prprov        prtprv
     c                   endif
     c                   else
     c                   eval      prprov = *blanks
     c                   eval      qua33b = *blanks
     c                   endif

     c                   when      excode = 470
     c                   eval      prmlv6 = bbplv6
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'TAXON'
     c                   eval      prmprc = bgdtpr
     c                   eval      prmrev = *zeros
     c                   eval      prmdte = *zeros
     c                   eval      prvvar = *blanks
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvid
     C                   parm                    bx24i(z)
     c                   eval      pin(z) = prvvar
     c                   endif
      **
     c                   when      excode = 527
     c                   eval      exc527 = *on
      **
     c                   when      excode = 532
     c                   eval      exc532 = *on

     c                   when      excode = 543
     c                   eval      exc543 = *on

     c                   when      excode = 545
     c                   if        hx6a11 <> *blanks
     c                   movel(p)  hx6nam        box321
     c                   movel(p)  hx6a11        box322
     c                   movel(p)  hx6ct1        box323
     c                   movel(p)  hx6st1        box324
     c                   if        hx6z12 <> *blanks
     c                   eval      box325 = hx6z11 + '-' + hx6z12
     c                   else
     c                   eval      box325 = hx6z11 + '-0000'
     c                   endif
     c                   endif
     **
     c                   when      excode = 549
     c                   eval      exc549 = *on
     **
     c                   when      excode = 552
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      status = 'AT'
     c                   eval      srlv6 = bbplv6
     c                   eval      sract = bbaccn
     c                   exsr      srasgn
     c     doc#          chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      rqdoctor# = doc#
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    npi(z)
     c                   movel     rtnlicense    npi(z)
     c                   endif
     **
     c                   when      excode = 559
     c                   move      'I'           omtype                         =proc include
     c     exokey        chain     hbfbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D'
     c                   eval      wrkymd = bgdtsd
     c                   eval      wrkdd = 1
     c                   eval      ydate = wrkymd
     c                   if        ydate < mmaddt
     c                   eval      ydate = mmaddt
     c                   endif
     c                   exsr      srcymd
     c                   move      mdate         dtf(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtf6(z)
     c                   move      mm6           dtfm(z)
     c                   move      dd6           dtfd(z)
     c                   move      yy6           dtfy(z)
      **
     c                   eval      wrkymd = bgdtsd                              =use eom date
     c                   eval      cmoyr = 0                                     for service date
     c                   move      wrkyy         cmoyr
     c                   movel     wrkmm         cmoyr
     c                   exsr      gteom
     c                   exsr      srcmdy
     c                   if        ydate > mmdsdt and mmdsdt <> 0
     c                   eval      ydate = mmdsdt
     c                   endif
     c                   exsr      srcymd
     c                   move      mdate         dtt(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtt6(z)
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
     c                   endif
      **
     c                   when      excode = 584
     c                   eval      omtype = 'P'
     c     exokey        chain     hbfbfxo
     c                   if        not %found(hbpbfxo) or bxodlt = 'D'
     c                   if        mmdsdt < bbtodt and mmdsdt <> 0
     c                   eval      ydate = mmdsdt
     c                   else
     c                   eval      ydate = bbtodt
     c                   endif
     c                   exsr      srcymd
     c                   move      mdate         dtt(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtt6(z)
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
     c                   endif

     c                   when      excode = 585
     c                   eval      box01x1 = ' '
     c                   eval      box01x2 = ' '
     c                   eval      box01x3 = ' '
     c                   eval      box01x4 = ' '
     c                   eval      box01x5 = ' '
     c                   eval      box01x6 = ' '

     c                   when      excode = 586 and
     c                             exc38 = *off and
     c                             exc152 = *off
     c                   eval      prgrpx = bbplcy
      **
     c                   when      excode = 591
     c                   eval      *in72 = *off
     c     procky        chain     xffprocc
     c                   if        not %found(hxpprocc)
     c     bgdtpr        chain     xffprocm
     c                   endif
     c                   if        %found
     c                   eval      pdsc(z) = xfpdsc
     c                   eval      *in72 = *on
     c                   endif
      **
     c                   when      excode = 649
     c                   eval      omtype = 'I'                                 EX649 proc include
     c     exokey        chain     hbfbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D'
     c                   movel     BGDTPR        prmprc
     c                   movel     'ALTGP'       prvid
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        prtprv
     c                   endif
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm      'PRVDR'       prvid
     c                   parm                    qua33b
     c                   else
     c                   move      *blanks       prtprv
     c                   endif
      **
     c                   when      excode = 681
      **
 004 c                   move      *blanks       prrupn
 004 c                   move      *blanks       prrnpi
 003 c                   movel     *blanks       prrphy
     c                   clear                   hmfasgn
     c                   move      'AT'          status
     c     physky        setgt     hmfasgn
     c     tryagn6       tag
     c     physk2        readpe    hmfasgn                                79
     c                   if        *in79 = *off
     c     msgdlt        cabeq     'D'           tryagn6
     c     msgdr#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   movel     hmdnam        prrphy
     c                   eval      rqdoctor# = msgdr#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   movel     rtnlicense    prrnpi
     c                   endif
     c                   endif
     c                   endif
      **
     c                   when      excode = 682
     c                   eval      exc682 = *on
      **
     c                   when      excode = 714
     c                   eval      exc714 = *on
      **
     c                   when      excode = 715
     c                   eval      box32b = 'G2 ' + %trim(taxid)
      **
     c                   when      excode = 741
     c                   eval      modVerCount = 1
     c                   dow       modVerCount < 12
     c                   if        %subst(mod(z):modVerCount:3) = '59 '
     c                   eval      %subst(mod(z):modVerCount) =
     c                                %subst(mod(z):(modVerCount + 3))
     c                   else
     c                   eval      modVerCount += 3
     c                   endif
     c                   enddo
      **
     c                   when      excode = 752
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = savrev
     c                   eval      prmprc = svproc
     c                   eval      prmct1 = ' '
     c                   eval      prmct2 = ' '
     c                   movel     'PRVDR'       prvid
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box32b
     c     'LU'          cat       box32b:1      box32b
     c                   endif
      **
     c                   when      excode = 753
     c                   eval      exc753 = *on
     c                   eval      *in35 = *on
      **
     c                   when      excode = 755
     c                   eval      prmlv6 = bbplv6
     c                   eval      payor = bbpayr
     c                   eval      plan  = bbplan
     c                   eval      prvid = 'ALPNM'
     c                   eval      prmdte = bbtodt
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = *blanks
     c                   eval      prmct2 = *blanks
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      box11c = prvvar
     C                   endif
      **
     c                   when      excode = 759
     c                   eval      exc759 = *on
      **
     c                   when      excode = 763
     c                   eval      exc763 = *on
      **
     c                   when      excode = 789
     c                   movel     hx6nam        prthnm
      **
     c                   when      excode = 833
     c                   eval      exc833 = *on
      **
     c                   when      excode = 834
     c                   eval      exc834 = *on
      **
     c                   when      excode = 837
     c                   eval      prmprc = *blanks
     c                   eval      prvid  = 'VASID'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      prauth = prvvar
     c                   endif
     c                   eval      bdpad1 = priadr
     c                   eval      bdpcty = pricty
     c                   eval      bdpsta = prista
     c                   eval      bdpzp1 = prizip
     c                   eval      box07a = priadr
     c                   eval      box07b = pricty
     c                   eval      box07c = prista
     c                   eval      box07d = prizip
     c                   eval      exc837 = *on
      **
     c                   when      excode = 854
     c     theronfile    chain     hmfmams
     c                   if        %found(hmpmams)
     c                   movel     hmdnam        prthnm
     c                   endif
      **
     c                   when      excode = 859
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = prcct1
     c                   eval      prmct2 = prcct2
     c                   eval      prvid = 'TAXON'
     c                   eval      prmlv6 = bbplv6
     c                   exsr      srprv
     c                   movel     prvvar        prtprv
     c                   eval      exc859 = *on
      **
     c                   when      excode = 861
     c                   eval      rqdoctor# = bgdtth
     c                   eval      rqlictype = 'SNPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   eval      snpi = rtnlicense
     c                   eval      exc861 = *on
     c                   exec sql
     c+                    SELECT NPNAME
     c+                    INTO :physname
     c+                    FROM TABLE(NPI_HMPNPI(:snpi,DEFAULT,DEFAULT,
     c+                                          DEFAULT,DEFAULT,DEFAULT));
     c                   if        physname <> *blanks
     c                   eval      prthnm = physname
     c                   endif
     c                   endif
      **
     c                   when      excode = 869
     c                   if        bgdtth = 0
     c                   call      'XFXMASGN'
     c                   parm      bbplv6        lv6#
     c                   parm      bbaccn        acct#
     c                   parm      'AT'          status
     c                   parm      bbtodt        rqsdte
     c                   parm                    doc#
     c                   if        doc# <> 0
     c                   eval      rqdoctor# = doc#
     c                   eval      rqlictype = 'NPI'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    npi(z)
     c                   endif
     c                   endif
     **
     c                   when      excode = 885
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = bgdtpr
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'TAXON'
     c                   eval      prmlv6 = bbplv6
     c                   eval      qua33b = *blanks
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      exc885 = *on
     c                   eval      prprov = prvvar
     c                   if        exc401 = *on
     c                   movel     prprov        prtprv
     c                   endif
     c                   endif
      **
     c                   when      excode = 888
     c                   eval      in1typ = 'P'
     c                   eval      IN2TYP = *blanks
     c                   eval      IN2SS# = 0
     c                   eval      IN2NAM = *blanks
     c                   eval      IN2AD1 = *blanks
     c                   eval      IN2AD2 = *blanks
     c                   eval      IN2CTY = *blanks
     c                   eval      IN2STA = *blanks
     c                   eval      IN2ZIP = *blanks
     c                   eval      IN2FCL = *blanks
     c                   eval      IN2NPI = *blanks
     c                   eval      IN2POL = *blanks
     c                   eval      IN2REL = *blanks
     c                   eval      IN2GP# = *blanks
     c                   eval      IN2GPN = *blanks
     c                   eval      IN2INM = *blanks
     c                   eval      IN2SEX = *blanks
     c                   eval      IN2BDT = 0
     c                   eval      IN2SBR = *blanks
     c                   eval      IN3TYP = *blanks
     c                   eval      IN3SS# = 0
     c                   eval      IN3NAM = *blanks
     c                   eval      IN3AD1 = *blanks
     c                   eval      IN3AD2 = *blanks
     c                   eval      IN3CTY = *blanks
     c                   eval      IN3STA = *blanks
     c                   eval      IN3ZIP = *blanks
     c                   eval      IN3FCL = *blanks
     c                   eval      IN3NPI = *blanks
     c                   eval      IN3POL = *blanks
     c                   eval      IN3REL = *blanks
     c                   eval      IN3GP# = *blanks
     c                   eval      IN3GPN = *blanks
     c                   eval      IN3INM = *blanks
     c                   eval      IN3SEX = *blanks
     c                   eval      IN3BDT = 0
     c                   eval      IN3SBR = *blanks
      **
     c                   when      excode = 907
     c                   move      *on           *in52
      **
     c                   when      excode = 909
     c                   eval      prmprc = *blanks
     c                   eval      prvid  = 'VASID'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      vasid = prvvar
     c     bbtrak        chain     hbfchgbl
     c                   if        %found
     c     chgkeysq      chain     hbfchrg
     c                   if        %found
     c                   eval      omtype = 'P'
     c                   eval      bgdtpr = bgctpr
     c                   eval      xfpubc = 0
     c     procky        chain     xffprocc
     c                   if        %found(hxpprocc)
     c     mmpct1        chain     hxfctg1
     c                   if        %found(hxpctg1)
     c                             and xfpnub <> 0
     c                             and (hxprty='O'
     c                               or hxprty='E')
     c                   eval      xfpubc = xfpnub
     c                   endif
     c                   endif
     c     exokey        chain     hbfbfxo
     c                   if        not %found(hbpbfxo) or bxodlt = 'D'
     c     ivskey        setll     hafivsvc
     c                   dou       %eof(halivsvc)
     c     ivskey        reade     hafivsvc
     c                   if        not %eof(halivsvc)
     c                             and bsvdlt = *blanks
     c                             and bsvioa <> 'Y'
     c                             and bsveff <= bgdtsd
     c                             and bgdtsd <= bsvend
     c                             and ((bsvprc = bgdtpr and bsvprc <> *blanks)
     c                              or (bsvrvc = xfpubc and bsvrvc <> 0)
     c                              or (bsvptd = 'Y' and
     c                               (xfpubc >= 420 and xfpubc <= 429))
     c                              or (bsvotd = 'Y' and
     c                               (xfpubc >= 430 and xfpubc <= 439))
     c                              or (bsvspd = 'Y' and
     c                               (xfpubc >= 440 and xfpubc <= 449))
     c                              or (bsvswd = 'Y' and
     c                               (xfpubc >= 560 and xfpubc <= 569))
     c                              or (bsvbhd = 'Y' and
     c                               (xfpubc >= 900 and xfpubc <= 918))
     c                              or (bsvacd = 'Y' and xfpubc = 2101)
     c                              or (bsvtrd = 'Y' and xfpubc = 941)
     c                              or (bsvnrd = 'Y' and xfpubc = 230)
     c                              or (bsvcmd = 'Y' and
     c                               (xfpubc >= 942 and xfpubc <= 944))
     c                              or (bsvvcd = 'Y' and xfpubc = 948)
     c                              or (bsvphd = 'Y' and xfpubc = 960)
     c                              or (bsvtcd = 'Y' and xfpubc = 940))
     c                   if        bsvaut <> *blanks
     c                   eval      prvid  = 'PAYID'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   exsr      srctab
     c                   eval      tcode = 'B909'
     c                   eval      ecode = prvvar
     c                   exsr      srtabl
     c                   if        tind = ' '
     c                   eval      prauth = %trim(vasid) + '-' + %trim(bsvaut)
     c                   leave
     c                   else
     c                   eval      prauth = %trim(bsvaut)
     c                   leave
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   enddo
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      **
     c                   when      excode = 910
     c                   eval      exc910 = *on
      **
     c                   when      excode = 918
     c                   eval      exc918 = *on
      **
     c                   when      excode = 919
     c                   eval      exc919 = *on
      **
     c                   when      excode = 935
     c                   exsr      getBillingInfo
     c                   exsr      getPayToInfo
      **
     c                   when      excode = 947
     c     bbtrak        setll     hbfchgbl
     c                   dou       %eof(hbpchgbl)
     c     bbtrak        reade     hbfchgbl
     c                   if        not %eof(hbpchgbl)
     c     chgkeysq      chain     hbfchrg
     c                   if        %found(hbpchrg)
     c     bgctth        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      prthnm = hmdnam
     c                   eval      physicianName = hmdnam
     c                   leave
     c                   endif
     c                   endif
     c                   endif
     c                   enddo
      **
     c                   when      excode = 948
     c                   eval      *in72 = *off
     c     cdsckey       chain     hbfcdsc
     c                   if        %found(hbpcdsc)
     c                   eval      pdsc(z) = bgcdsc
     c                   eval      *in72 = *on
     c                   else
     c     procky        chain     xffprocc
     c                   if        not %found(hxpprocc)
     c     bgdtpr        chain     xffprocm
     c                   endif
     c                   if        %found
     c                   eval      pdsc(z) = xfpdsc
     c                   eval      *in72 = *on
     c                   endif
     c                   endif
      **
     c                   when      excode = 953
     c                   eval      exc953 = *on
      **
     c                   when      excode = 959
     c                   eval      exc959 = *on
      **
     c                   when      excode = 960
     c                   eval      exc960 = *on
      **
     c                   when      excode = 964
     c                   eval      exc964 = *on
      **
     c                   when      excode = 965
     c                   if        bgdtth <> 0
     c                   eval      pin(z) = *blanks
     c     bgdtth        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      rqdoctor# = bgdtth
     c                   eval      rqlictype = 'TAXON'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    pin(z)
     c                   endif
     c                   endif
     **
     c                   when      excode = 966
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'FALOC'
     c                   eval      prmlv6 = bbplv6
     c                   eval      qua33b = *blanks
     c                   exsr      srprv
     c                   eval      box32b = prvvar
      **
     c                   when      excode = 968
     c                   call      'XFXMASGN'
     c                   parm      bbplv6        lv6#
     c                   parm      bbaccn        acct#
     c                   parm      'AT'          status
     c                   parm      bbtodt        rqsdte
     c                   parm                    doc#
     c                   if        doc# <> 0
     c     doc#          chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      prthnm = hmdnam
     c                   eval      physicianName = hmdnam
     c                   endif
     c                   endif
      **
     c                   when      excode = 970
     c                   eval      box01x1 = ' '
     c                   eval      box01x2 = ' '
     c                   eval      box01x3 = ' '
     c                   eval      box01x4 = 'X'
     c                   eval      box01x5 = ' '
     c                   eval      box01x6 = ' '
     c
      **
     c                   when      excode = 971
     c                   eval      box01x1 = ' '
     c                   eval      box01x2 = ' '
     c                   eval      box01x3 = ' '
     c                   eval      box01x4 = ' '
     c                   eval      box01x5 = 'X'
     c                   eval      box01x6 = ' '
     c
      **
     c                   when      excode = 972
     c                   eval      box01x1 = ' '
     c                   eval      box01x2 = ' '
     c                   eval      box01x3 = ' '
     c                   eval      box01x4 = ' '
     c                   eval      box01x5 = ' '
     c                   eval      box01x6 = 'X'
     c
      **
     c                   when      excode = 973
     c                   eval      box11bq = 'Y4'
     c                   eval      box11b  = bbplcy
     c                   eval      B511B   = box11b
      **
     c                   when      excode = 974
      * get additional claim information
     c                   eval      prmlv6 = bbplv6
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   eval      prvid = 'NTEAD'
     c                   eval      prmprc = bgdtpr
     c                   eval      prmrev = *zeros
     c                   eval      prvvar = *blanks
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      box19 = prvvar
     c                   endif
      **
     c                   when      excode = 978
     c                   eval      exc978 = *on
      **
     c                   when      excode = 979
     c                   eval      exc979 = *on
      **
     c                   when      excode = 981
     c                   eval      exc981 = *on
      **
     c                   when      excode = 983
     c                   eval      exc983 = *on
      **
     c                   when      excode = 989
      **
          if ct1flg = 'X';
            wrkct1 = bgdct1;
          else;
            wrkct1 = *blanks;
          endif;
          if ct2flg = 'X';
            wrkct2 = bgdct2;
          else;
            wrkct2 = *blanks;
          endif;

          minsPerUnit = getMinutesPerUnit(bbpayr : bbplan : wrkct1 :
                                          wrkct2 : bgdtsd : bgdtpr);

          if minsPerUnit <> 0 and bgdmii <> 0;
            ex989q = bgdmii / minsPerUnit;
          else;
            ex989q = bgdtqt;
          endif;
          qty(z) = ex989q;
          qty2(z) = ex989q;
          qty4(z) = ex989q;
          qty5(z) = ex989q;
          *in75 = *on;
          exc989 = *on;
     **
     c                   when      excode = 990
          omtype = 'I';
          chain exokey hbpbfxo;
          if %found(hbpbfxo) and bxodlt <> 'D';
            qty(z) = 1;
            qty4(z) = 1;
            qty2(z) = 1;
            qty5(z) = 1;
            bgdtqt = 1;
          endif;
     c                   when      excode = 992
     c                   eval      exc992 = *on
       if (bgdTq2 <> 0) and first992;
         qty(z) = bgdTq2;
         qty2(z) = bgdTq2;
         qty4(z) = bgdTq2;
         qty5(z) = bgdTq2;
         *in75 = *on;
       endif;

     c                   when      excode = 994
     c                   eval      exc994 = *on

     c                   when      excode = 995
     c                   if        (bgdTth <> 0)
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c     bgdtth        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      rqdoctor# = bgdtth
     c                   eval      rqlictype = 'TAXON'
     c                   eval      rqstate = *blanks
     c                   eval      rqstatechk = 'N'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    pin(z)
     c                   endif

     c                   if        (pin(z) <> *blanks)
     c                   call      'XFXBQ15'
     c                   parm                    bbPayr
     c                   parm                    rqLicType
     c                   parm                    bx24i(z)
     c                   endif
     c                   endif

     c                   when      excode = 996
     c                   eval      exc996 = *on

     c                   when      excode = 999
     c                   eval      box321 = hx6a21
     c                   eval      box322 = hx6a22
     c                   eval      box323 = hx6ct2
     c                   eval      box324 = hx6st2
     c                   movel(p)  hx6z21        box325
     c                   if        hx6z22 = *blanks
     c                   eval      box325e = '0000'
     c                   else
     c                   movel(p)  hx6z22        box325e
     c                   endif

     c                   when      excode = 1000
          rqLicType = 'NPI';
          clear rqState;
          status = 'AT';
          srLv6 = bbpLv6;
          srAct = bbAccn;
          exsr srAsgn;
          chain doc# hmpmams;
          if %found(hmpmams);
            physicianName = hmdNam;
            rqDoctor# = doc#;
            rqStateChk = 'N';
            clear rqCat2;
            exsr srLicense;
            npi(z) = rtnLicense;
          endif;

     c                   when      excode = 1001
          prtpol = bbplcy;
          *in24 = *off;
          box11dn = 'X';
          box11dy = ' ';

     c                   when      excode = 1002
          prmRev = savRev;
          payor = bbPayr;
          plan = bbPlan;
          prmprc = svproc;
          prvid = 'PRVDR';
          prmLv6 = bbpLv6;
          clear prmct1;
          clear prmct2;
          exsr srPrv;
          if prvVar <> *blanks;
             box32b = prvVar;
             XFXBQ15 (bbPayr:prvId:qua32b);
             box32b = qua32b + ' ' + box32b;
          endif;
     **
      *******************
      ** end of bill excpt
      *******************
     c                   endsl
      **
     c                   endif
      **
     c                   if        found = 'N'
     c                             and count > 0
      **
     c                   eval      count = count - 1
     c                   eval      binary = count
      **
     c                   eval      expayr = 0
     c                   eval      explan = 0
     c                   move      '  '          exfc
     c                   eval      exrevc = 0
     c                   eval      exlvl6 = 0
     c                   eval      exCat1 = *blanks
     c                   eval      exCat2 = *blanks
     c                   movel     '  '          exstat
     c                   eval      exRule = *blanks
      **
     c                   testb     '7'           valuel               09
     c                   if        *in09 = *off
     c                   eval      exRule = svRule
     c                   endif
      **
     c                   testb     '6'           valuel               08
     c                   if        *in08 = *off
     c                   eval      exrevc = svrevc
     c                   endif
      **
     c                   testb     '5'           valuel               07
     c                   if        *in07 = *off
     c                   eval      expayr = svpayr
     c                   eval      explan = svplan
     c                   endif
      **
     c                   testb     '4'           valuel               06
     c                   if        *in06 = *off
     c                   movel     svpayr        expayr
     c                   endif
      **
     c                   testb     '3'           valuel               05
     c                   if        *in05 = *off
     c                   movel     svfc          exfc
     c                   endif
      **
     c                   testb     '2'           valuel               04
     c                   if        *in04 = *off
     c                   movel     svCat2        exCat2
     c                   endif
      **
     c                   testb     '1'           valuel               03
     c                   if        *in03 = *off
     c                   movel     svCat1        exCat1
     c                   endif
      **
     c                   testb     '0'           valuel               02
     c                   if        *in02 = *off
     c                   eval      exlvl6 = svlvl6
     c                   endif
      **
     c                   testb     '7'           valueh               01
     c                   if        *in01 = *off
     c                   movel     svstat        exstat
     c                   endif
      **
     c                   goto      again
      **
     c                   endif
      **
     c                   eval      exRule = svRule
      **
     c                   endsr
      ********************************************************************
      **      SUBROUTINE TO PRINT CURRENT PAGE AND ADVANCE TO NEW PAGE        **
      **************************************************************************
     c     newpag        begsr
      **
     c                   if        onetrk = '0'
     c                   move      '1'           onetrk
     c                   endif
      **
     c                   if        *in68 = *off
     c                   move      *on           *in50
     c                   else
     c                   eval      baldue = totchg - totpmt
     c                   eval      totpmt = 0
     c                   eval      bildue = bildue + baldue
     c                   eval      rptdue = rptdue + baldue
     c                   eval      lv6due = lv6due + baldue
     c                   eval      paydue = paydue + baldue
     c                   eval      plndue = plndue + baldue
     c                   eval      accdue = accdue + baldue
     c                   if        exc753 = *on
     c                   eval      baldue = totChgBill
     c                   elseif    not *in30
     c                   eval      baldue = 0                                   02-12 not used
     c                   endif
     c                   endif
      **
     c                   if        exc224
     c                   eval      injdte = mmpidd
     c                   else
     c                   eval      injdte = dxdate
     c                   endif
     c                   eval      box14q = '431'                               Onset of illness
     c                   if        %found(hmpenct) and chnenc = *on and
     c                             not exc224
     c                   eval      injdte = enpidd
     c                   if        enpreg = 'Y'
     c                   eval      box14q = '484'                               Last LMP
     c                   endif
     c                   endif
      **
     c                   if        exc978
     c                   eval      box15dte = mmpidd
     c                   if        mmpidd <> 0
     c                   eval      *in28 = *on
     c                   eval      box15q = '439'
     c                   endif
     c                   endif
      **
     c                   if        exc979
     c                   exsr      getIniTreatDt
     c                   if        initTrtDt <> 0
     c                   eval      box15dte = initTrtDt
     c                   eval      *in28 = *on
     c                   eval      box15q = '454'
     c                   endif
     c                   endif
      **
     c                   if        exc983 and (bbtOrb = '6' or bbtOrb = '7'
     c                             or bbtOrb = '8')
     c                   eval      box22a = bbtOrb
     c                   endif
      **
     c                   if        (exc994 and not(bbtOrb = '6' or bbtOrb = '7'
     c                             or bbtOrb = '8'))
     c                   eval      bbtOrb = '1'
     c                   eval      box22a = ''
     c                   endif
      **
     c****               eval      rw = 0
     c****               eval      cl = 0
     c****               exsr      setptr
      **
     c                   eval      pageno = pageno + 1
     c                   if        sndf10 <> 'N' and singlePrint <> 'Y'         =overlay
     c                   eval      rw = rowO
     c                   eval      cl = colO
     c                   eval      inc = incO
     c                   write     bill15
     c                   if        sndf10 = 'A'                                 =overlay to archive
     c****               except    billpg                                       =but not print
     c                   eval      rw = rowN
     c                   eval      cl = colN
     c                   eval      inc = incN
     c                   write     billnoovl
     c                   endif
     c                   else                                                   =no overlay
     c****               except    billpg
     c                   eval      rw = rowN
     c                   eval      cl = colN
     c                   eval      inc = incN
     c                   write     billnoovl
     c                   endif
      **
     c                   if        elecwrite = *off
     c                   exsr      fillelecdata
     c                   eval      elecwrite = *on
     c                   endif
      **
     c                   except    detail
     c                   eval      pageno2 = pageno2 + 1
     c****               eval      pageno = pageno + 1
      **
     c                   if        sndf10 = 'A' and singlePrint <> 'Y'
     c                   move      'A'           crtspl
     c                   else
     c                   move      'Y'           crtspl
     c                   endif
      **
     c                   eval      rptbls = rptbls + 1
     c                   eval      bilpgs = bilpgs + 1
     c                   eval      rptpgs = rptpgs + 1
     c                   eval      lv6pgs = lv6pgs + 1
     c                   eval      paypgs = paypgs + 1
     c                   eval      plnpgs = plnpgs + 1
     c                   eval      accpgs = accpgs + 1
      **
     c                   move      *off          *in50
     c                   eval      dtf = *blanks
     c                   eval      dtt = *blanks
     c                   eval      dtf6 = *blanks
     c                   eval      dtt6 = *blanks
     c                   eval      dtfm = *blanks
     c                   eval      dttm = *blanks
     c                   eval      dtfd = *blanks
     c                   eval      dttd = *blanks
     c                   eval      dtfy = *blanks
     c                   eval      dtty = *blanks
     c                   eval      pbf = 0
     c                   eval      pbt = 0
     c                   eval      pos = *blanks
     c                   eval      eps = *blanks
     c                   eval      cpt = *blanks
     c                   eval      ddx = *blanks
     c                   eval      mod = *blanks
     c                   eval      cds = *blanks
     c                   eval      pdsc = *blanks
     c                   eval      amt = 0
     c                   eval      amta = 0
     c                   eval      qty = 0
     c                   eval      qty4 = 0
     c                   eval      qty2 = 0
     c                   eval      qty5 = 0
     c                   eval      dix = *blanks
     c                   eval      pin = *blanks
     c                   eval      bx24i = *blanks
     c                   eval      npi = *blanks
     c                   eval      z = 0
     c                   if        *in68 = *on
     c                   eval      totchg = 0
     c                   eval      totpmt = 0
     c                   endif
      **
     c                   endsr
      ********************************************************************
      /EJECT
     c     secindc       begsr
      **
     c                   eval      secind2 = 0
      **
     c                   select
     c                   when      secind = 1
     c                   eval      cpayor = aub(1)
     c                   eval      cplan = apl(1)
     c                   when      secind = 2
     c                   eval      cpayor = aub(1)
     c                   eval      cplan = apl(1)
     c                   when      secind = 3
     c                   eval      cpayor = aub(2)
     c                   eval      cplan = apl(2)
     c                   endsl
      **
     c     cpyrkey       setll     hbfdiem                                      =GET DIEM
     c                   dou       *in77 = *on                                   CLASSIFICATION
     c     cpyrkey       reade     hbfdiem                                79     CONTRACT VALUES
     c                   if        *in79 = *off
     c                   if        bbfrdt >= xffdt
     c                             and bbfrdt <= xftdt
     c                             and xfdlt = ' '
     c                   if        xfct1 = 'X'                                  =BY CLASS 1
     c                   movel     mmpct1        wrkct1
     c                   else
     c                   eval      wrkct1 = *blanks
     c                   endif
     c                   if        xfct2 = 'X'                                  =BY CLASS 2
     c                   movel     mmpct2        wrkct2
     c                   else
     c                   eval      wrkct2 = *blanks
     c                   endif
     c                   leave
     c                   endif
     c                   else
     c                   goto      end20                                        =NO CONTRACT
     c                   endif
     c                   enddo
      **
     c     cpyrkey       setll     hbfpdiem
     c                   dou       *in79 = *on
     c     cpyrkey       reade     hbfpdiem                               79    =CAT2 PDIEM
     c                   if        *in77 = *off                                    FILE
     c                             and xfpdlt = ' '
     c                             and wrkct1 = xfpct1
     c                             and wrkct2 = xfpct2
     c                   if        bbfrdt >= xfpfdt
     c                             and bbfrdt <= xfptdt
     c                   if        xfpprm = 'Y'
     c                   eval      secind2 = 1
     c                   endif
     c                   else
     c                   goto      end20                                        =NO valid CONTRACT
     c                   endif
     c                   else
     c                   goto      end20                                        =NO CONTRACT
     c                   endif
     c                   enddo
      **
     c                   if        secind2 = 1
     c                   select
     c                   when      secind = 1
     c                   eval      cpayor = aub(2)
     c                   eval      cplan = apl(2)
     c                   when      secind = 2
     c                   eval      cpayor = aub(3)
     c                   eval      cplan = apl(3)
     c                   when      secind = 3
     c                   eval      cpayor = aub(3)
     c                   eval      cplan = apl(3)
     c                   endsl
      **
     c     cpyrkey       setll     hbfdiem                                      =GET DIEM
     c                   dou       *in77 = *on                                   CLASSIFICATION
     c     cpyrkey       reade     hbfdiem                                79     CONTRACT VALUES
     c                   if        *in79 = *off
     c                   if        bbfrdt >= xffdt
     c                             and bbfrdt <= xftdt
     c                             and xfdlt = ' '
     c                   if        xfct1 = 'X'                                  =BY CLASS 1
     c                   movel     mmpct1        wrkct1
     c                   else
     c                   eval      wrkct1 = *blanks
     c                   endif
     c                   if        xfct2 = 'X'                                  =BY CLASS 2
     c                   movel     mmpct2        wrkct2
     c                   else
     c                   eval      wrkct2 = *blanks
     c                   endif
     c                   leave
     c                   endif
     c                   else
     c                   goto      end20                                        =NO CONTRACT
     c                   endif
     c                   enddo
      **
     c     cpyrkey       setll     hbfpdiem
     c                   dou       *in79 = *on
     c     cpyrkey       reade     hbfpdiem                               79    =CAT2 PDIEM
     c                   if        *in77 = *off                                    FILE
     c                             and xfpdlt = ' '
     c                             and wrkct1 = xfpct1
     c                             and wrkct2 = xfpct2
     c                   if        bbfrdt >= xfpfdt
     c                             and bbfrdt <= xfptdt
     c                   if        xfpcic = 'Y' or xfpcpc = 'Y'
     c                             or xfpdec = 'Y'
     c                   eval      secind = 9
     c                   endif
     c                   else
     c                   goto      end20                                        =NO valid CONTRACT
     c                   endif
     c                   else
     c                   goto      end20                                        =NO CONTRACT
     c                   endif
     c                   enddo
      **
     c                   endif
      **
     c     end20         tag
      **
     c                   endsr
      **************************************************************************
     c     sractv        begsr
     c**
     c                   eval      rqdate = bbtodt                              =get ins info
     c                   exsr      srinfo
     c**
     c                   call      'HBX3INS'
     c                   parm      bbplv6        rqlvl6
     c                   parm      bbaccn        rqacct
     c                   parm      bbfrdt        rqfrdt
     c                   parm      bbtodt        rqtodt
     c                   parm      bbtrak        rqtrak
     c                   parm      bbiseq        rqiseq
     c                   parm      0             rqisq1
     c                   parm      0             rqisq2
     c                   parm      0             rqisq3
     c**
     c                   eval      wrkrnk = 0
     c                   eval      afc = *blanks
     c                   eval      aub = 0
     c                   eval      apl = 0
     c                   eval      apo = *blanks
     c                   eval      isq = 0
     c                   eval      rk = 1
     c**
     c                   if        rqisq1 <> 0
     c                   eval      rk = 1
     c     rqisq1        lookup    isqx(rk)                               79
     c                   if        *in79 = *on
     c                   eval      afc(1) = afcx(rk)
     c                   eval      aub(1) = aubx(rk)
     c                   eval      apl(1) = aplx(rk)
     c                   eval      apo(1) = apox(rk)
     c                   eval      isq(1) = isqx(rk)
     c                   endif
     c                   endif
     c**
     c                   if        rqisq2 <> 0
     c                   eval      rk = 1
     c     rqisq2        lookup    isqx(rk)                               79
     c                   if        *in79 = *on
     c                   eval      afc(2) = afcx(rk)
     c                   eval      aub(2) = aubx(rk)
     c                   eval      apl(2) = aplx(rk)
     c                   eval      apo(2) = apox(rk)
     c                   eval      isq(2) = isqx(rk)
     c                   endif
     c                   else
     c** If no insurance returned, default to next rank
     c                   eval      rk = rk + 1
     c                   eval      afc(2) = afcx(rk)
     c                   eval      aub(2) = aubx(rk)
     c                   eval      apl(2) = aplx(rk)
     c                   eval      apo(2) = apox(rk)
     c                   eval      isq(2) = isqx(rk)
     c                   endif
     c**
     c                   if        rqisq3 <> 0
     c                   eval      rk = 1
     c     rqisq3        lookup    isqx(rk)                               79
     c                   if        *in79 = *on
     c                   eval      afc(3) = afcx(rk)
     c                   eval      aub(3) = aubx(rk)
     c                   eval      apl(3) = aplx(rk)
     c                   eval      apo(3) = apox(rk)
     c                   eval      isq(3) = isqx(rk)
     c                   endif
     c                   else
     c** If no insurance returned, default to next rank
     c                   eval      rk = rk + 1
     c                   eval      afc(3) = afcx(rk)
     c                   eval      aub(3) = aubx(rk)
     c                   eval      apl(3) = aplx(rk)
     c                   eval      apo(3) = apox(rk)
     c                   eval      isq(3) = isqx(rk)
     c                   endif
     c**
     c                   select
     c                   when      bbiseq = rqisq1
     c                   eval      wrkrnk = 1
     c                   when      bbiseq = rqisq2
     c                   eval      wrkrnk = 2
     c                   when      bbiseq = rqisq3
     c                   eval      wrkrnk = 3
     c                   endsl
     c**
     c                   endsr
      ********************************************************************
      /EJECT
     c     srinfo        begsr
      **
     c                   call      'XFXINSIN'
     c                   parm      'B'           reqsys
     c                   parm      'A'           reqitp
     c                   parm      mmplv6        rqlvl6
     c                   parm      mmacct        rqacct
     c                   parm      mmmrno        rqmrno
     c                   parm                    rqdate
     c                   parm      *blanks       rqfcl
     c                   parm      *blanks       rqubc
     c                   parm      *blanks       rqplc
     c                   parm      *blanks       rqpol
     c                   parm      *blanks       rqgrp
     c                   parm      *blanks       rqabl
     c                   parm      *blanks       rqvis
     c                   parm      *blanks       rqefd
     c                   parm      *blanks       rqetd
     c                   parm      *blanks       rqpro
     c                   parm      *blanks       rqisq
     c                   endsr
      **************************************************************************
     C     srtrfr        begsr

     c                   call      'XFXTRNSF'
     c                   parm                    lv6tmp
     c                   parm                    acttmp
     c                   parm      dattmp        rqdate
     c                   parm      *blanks       rqcat1
     c                   parm      *blanks       rqcat2
     c                   parm      *blanks       rqcat3
     c                   parm      *blanks       rqcat4
     c                   parm      *blanks       rqroom
     c                   parm      *blanks       rqrcls
     c                   parm      *blanks       rqproc
     c                   parm      0             rqoamt
     c                   parm      *blanks       rqrtyp
     c                   parm                    rqchng

     C                   ENDSR
      **************************************************************************
     c     get3ins       begsr

     c     benkey        klist
     c                   kfld                    epayor
     c                   kfld                    eplan

     c     demoikeyE     klist
     c                   kfld                    asglv6
     c                   kfld                    bbmrno
     c                   kfld                    epayor
     c                   kfld                    eplan
     c                   kfld                    epol

     c                   eval      epayor = ubcode
     c                   eval      eplan  =  ubplan
     c                   eval      epol   = policy
     c     benkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   eval      payor = ubcode
     c                   eval      plan  = ubplan
     c                   eval      prtplcy = policy
     c                   eval      rqiseq = ubisq
     c                   exsr      pyradr
     c                   movel     insbil        in1typ
     c                   eval      in1nam = xfbnam
     c                   eval      in1ad1 = xfbadr
     c                   eval      in1ad2 = xfbad2
     c                   eval      in1cty = xfbcty
     c                   eval      in1sta = xfbsta
     c                   eval      in1zip = xfbzip
     c                   exsr      srctab
     c                   eval      in1fcl = *blanks
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = XFBFCL
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in1fcl
     c                   endif

     c                   if        in1ad1 = *blanks
     c     prmicovkey1   setgt     haficov
     c     prmicovkey2   readpe    haficov
     c                   if        not %eof(hapicov)
     c                   movel     bcvga1        in1ad1
     c                   movel     bcvga2        in1ad2
     c                   movel     bcvgct        in1cty
     c                   movel     bcvgst        in1sta
     c                   movel     bcvgz1        in1zip
     c                   endif
     c                   endif

     c                   eval      payor = bbpayr
     c                   eval      plan  = bbplan

     c                   if        exc834 = *on
     c                   eval      prvid = 'PAYID'
     c                   else
     c                   eval      prvid = 'PNPI '                              =Payor National
     c                   endif

     c                   eval      prmdte = bbtodt
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c****               eval      prmct1 = prcct1
     c****               eval      prmct2 = prcct2
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in1npi = prvvar
     C                   else

     c                   if        exc834 = *on
     c                   eval      prvid = 'NPI  '
     c                   else
     c                   eval      prvid = 'PAYID'                              =Payor National
     c                   endif

     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in1npi = prvvar
     c                   endif
     c                   endif

     c     demoikeyE     chain     hafdemoi                           79
     c                   if        *in79 = *off
     c                   eval      in1pol = BDIPLY
     c                   eval      in1inm = BDISNM
     c                   eval      in1sex = BDISSX
     c                   eval      in1bdt = BDISBD
     c                   if        in1bdt = 0
     c                   eval      in1bdt = BDPBDT
     c                   endif
     c                   eval      IN1GP# = BDIGRP
     c                   eval      IN1GPN = BDIGNM
     c                   eval      in1rel = *blanks
     c                   eval      in1ss# = bdpss#
     c                   movel     *blanks       in1rel
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      %subst(ecode:1:2) = bdirlc
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in1rel
     c                   else
     c                   eval      in1rel = 'G8'
     c                   endif
     c                   endif
     c                   endif
     c*******************************
     c** secondary insurance (b)   **
     c*******************************
     c     1             do        3             e                 3 0
     c                   if        (ubcode <> aub(e)
     c                             or ubplan <> apl(e)
     c                             or policy <> apo(e))
     c                   eval      epayor = aub(e)
     c                   eval      eplan = apl(e)
     c                   eval      epol = apo(e)
     c                   eval      Spayor = aub(e)
     c                   eval      Splan = apl(e)
     c                   eval      Spol = apo(e)
     c     benkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   select
     c                   when      e = 1
     c                   eval      in2typ = 'P'
     c                   when      e = 2
     c                   eval      in2typ = 'S'
     c                   when      e = 3
     c                   eval      in2typ = 'T'
     c                   endsl
     c                   eval      payor = aub(e)
     c                   eval      plan  = apl(e)
     c****               eval      policy = apo(e)
     c                   eval      prtplcy = apo(e)
     c                   eval      rqiseq = isq(e)
     c                   exsr      pyradr
     c                   eval      in2nam = xfbnam
     c                   eval      in2ad1 = xfbadr
     c                   eval      in2ad2 = xfbad2
     c                   eval      in2cty = xfbcty
     c                   eval      in2sta = xfbsta
     c                   eval      in2zip = xfbzip
     c                   exsr      srctab
     c                   eval      in2fcl = *blanks
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = XFBFCL
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in2fcl
     c                   endif
     c                   eval      payor = epayor
     c                   eval      plan = eplan

     c                   if        exc834 = *on
     c                   eval      prvid = 'PAYID'
     c                   else
     c                   eval      prvid = 'PNPI '                              =Payor National
     c                   endif

     c                   eval      prmdte = bbtodt
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c****               eval      prmct1 = prcct1
     c****               eval      prmct2 = prcct2
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in2npi = prvvar
     C                   else

     c                   if        exc834 = *on
     c                   eval      prvid = 'NPI  '
     c                   else
     c                   eval      prvid = 'PAYID'                              =Payor National
     c                   endif

     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in2npi = prvvar
     c                   else
     c                   eval      in2npi = apo(e)
     c                   endif
     c                   endif
     c                   endif
     c     demoikeyE     chain     hafdemoi                           79
     c                   if        *in79 = *off
     c                   eval      in2pol = BDIPLY
     c                   eval      in2inm = BDISNM
     c                   eval      in2sex = BDISSX
     c                   eval      in2bdt = BDISBD
     c                   eval      IN2GP# = BDIGRP
     c                   eval      IN2GPN = BDIGNM
     c                   eval      in2rel = *blanks
     c                   eval      in2ss# = bdpss#
     c                   movel     *blanks       in2rel
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      %subst(ecode:1:2) = bdirlc
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in2rel
     c                   else
     c                   eval      in2rel = 'G8'
     c                   endif
     c                   endif

     c                   leave
     c                   endif
     c                   enddo
     c*******************************
     c** tertiary insurance (c)    **
     c*******************************
     c     1             do        3             e                 3 0
     c                   if        (ubcode <> aub(e)
     c                             or ubplan <> apl(e)
     c                             or policy <> apo(e))  and
     c                             (  Spayor <> aub(e)
     c                             or Splan  <> apl(e)
     c                             or Spol  <> apo(e))
     c                   eval      epayor = aub(e)
     c                   eval      eplan = apl(e)
     c                   eval      epol = apo(e)
     c                   select
     c                   when      e = 1
     c                   eval      in3typ = 'P'
     c                   when      e = 2
     c                   eval      in3typ = 'S'
     c                   when      e = 3
     c                   eval      in3typ = 'T'
     c                   endsl
     c     benkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   eval      payor = aub(e)
     c                   eval      plan  = apl(e)
     c****               eval      policy = apo(e)
     c                   eval      prtplcy = apo(e)
     c                   eval      rqiseq = isq(e)
     c                   exsr      pyradr
     c                   eval      in3nam = xfbnam
     c                   eval      in3ad1 = xfbadr
     c                   eval      in3ad2 = xfbad2
     c                   eval      in3cty = xfbcty
     c                   eval      in3sta = xfbsta
     c                   eval      in3zip = xfbzip
     c                   exsr      srctab
     c                   eval      in3fcl = *blanks
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = XFBFCL
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in3fcl
     c                   endif
     c                   eval      payor = epayor
     c                   eval      plan = eplan

     c                   if        exc834 = *on
     c                   eval      prvid = 'PAYID'
     c                   else
     c                   eval      prvid = 'PNPI '                              =Payor National
     c                   endif

     c                   eval      prmdte = bbtodt
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c****               eval      prmct1 = prcct1
     c****               eval      prmct2 = prcct2
     c                   eval      prmct1 = mmpct1
     c                   eval      prmct2 = mmpct2
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in3npi = prvvar
     C                   else

     c                   if        exc834 = *on
     c                   eval      prvid = 'NPI  '
     c                   else
     c                   eval      prvid = 'PAYID'                              =Payor National
     c                   endif

     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in3npi = prvvar
     c                   else
     c                   eval      in3npi = apo(e)
     c                   endif
     c                   endif
     c                   endif
     c     demoikeyE     chain     hafdemoi                           79
     c                   if        *in79 = *off
     c                   eval      in3pol = BDIPLY
     c                   eval      in3inm = BDISNM
     c                   eval      in3sex = BDISSX
     c                   eval      in3bdt = BDISBD
     c                   eval      IN3GP# = BDIGRP
     c                   eval      IN3GPN = BDIGNM
     c                   eval      in3rel = *blanks
     c                   eval      in3ss# = bdpss#
     c                   movel     *blanks       in3rel
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      %subst(ecode:1:2) = bdirlc
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in3rel
     c                   else
     c                   eval      in3rel = 'G8'
     c                   endif
     c                   endif

     c                   endif
     c                   enddo

      *** If Medicare is secondary, we need a code to put in SBR05 based on primary info
     c**** aub(1)        chain     xffinsd
     c****               if        %found(hxpinsd)                              =get primary F/C
     c                   eval      epayor = aub(1)
     c                   eval      eplan  = apl(1)
     c     benkey        chain     xffbnfit
     c                   if        %found(hxpbnfit)                             =get primary F/C
     c
     c                   select                                                 =determine primary
     c                   when      in1typ = 'P'                                 relationship code
     c                   eval      prmrel = in1rel
     c                   when      in2typ = 'P'
     c                   eval      prmrel = in2rel
     c                   when      in3typ = 'P'
     c                   eval      prmrel = in3rel
     c                   other
     c                   eval      prmrel = *blanks
     c                   endsl

     c                   if        (in1fcl = 'MA' or in1fcl = 'MB')
     c                             and in1typ <> 'P'
     c                   if        prmrel <> '18'
     c                   eval      in1sbr = '12'
     c                   else
     c                   exsr      srctab
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = xfbfcl
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      in1sbr = sdesc
     c                   endif
     c                   endif
     c                   endif

     c                   if        (in2fcl = 'MA' or in2fcl = 'MB')
     c                             and in2typ <> 'P'
     c                   if        prmrel <> '18'
     c                   eval      in2sbr = '12'
     c                   else
     c                   exsr      srctab
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = xfbfcl
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      in2sbr = sdesc
     c                   endif
     c                   endif
     c                   endif

     c                   if        (in3fcl = 'MA' or in3fcl = 'MB')
     c                             and in3typ <> 'P'
     c                   if        prmrel <> '18'
     c                   eval      in3sbr = '12'
     c                   else
     c                   exsr      srctab
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = xfbfcl
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      in3sbr = sdesc
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   endsr
      **************************************************************************
      /EJECT
      **************************************************************************
      **         GET SERVICE DATE VERIFICATION COVERAGE SUBROUTINE            **
      **************************************************************************
     c**                   provider number lookup                    **
     c*****************************************************************
     c     srprv         begsr
     c**
     c                   if        prvid = '15FRM'
     c                   eval      prmdte = bbbild
     c                   else
     c                   eval      prmdte = bbtodt
     c                   endif
      **
     c                   call      'XFXPRV'
     c                   parm                    prmlv6
     c                   parm                    bbaccn
     c                   parm                    payor
     c                   parm                    plan
     c                   parm                    prmct1
     c                   parm                    prmct2
     c                   parm                    prvid
     c                   parm                    prmprc
     c                   parm                    prmrev
     c                   parm                    prmdte
     c                   parm      *blanks       prvvar
     c**
     c                   endsr
     c*****************************************************************
     c     srcovg        begsr
      **
     c                   call      'HAXCOVG'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm      bbfrdt        cursvd
     c                   parm                    rqiseq
     c                   parm                    covgds

     c                   endsr
      **************************************************************************
      **      SUBROUTINE TO RETRIEVE PAYOR/PLAN DEFAULT INFORMATION           **
      **************************************************************************
     c     srppd         begsr
      **
     c                   call      'XFXPPD'
     c                   parm      bbplv6        level6
     c                   parm      bbaccn        acct#
     c                   parm                    reqdat
     c                   parm      bbpayr        payor
     c                   parm      bbplan        plan
     c                   parm                    ppdds
     c                   endsr
      *****************************************************************
      /EJECT
     c     srptcd        begsr
      **
     c                   call      'XFXPRTCD'
     c                   parm      bbplv6        level6
     c                   parm      bgdtsd        rqssvd
     c                   parm      bbpayr        rqspyr
     c                   parm      bbplan        rqspln
     c                   parm      bgdct1        rqsct1
     c                   parm      cat2          rqsct2
     c                   parm      bgdct3        rqsct3
     c                   parm      bgdtpr        rqsprc
     c                   parm      bgdvtp        rqsvtp
     c                   parm      'C'           rqsval
     c                   parm      'Y'           dftflg
     c                   parm                    rqvds1
     c                   parm                    cpt(z)
     c                   parm                    prtmd1
     c                   parm                    prtmd2
     c                   parm                    prtmd3
     c                   parm                    prtmd4
      **
     c                   if        prtmd1 <> *blanks
     c                   eval      bgdmod = prtmd1
     c                   eval      bgdmd2 = prtmd2
     c                   eval      bgdmd3 = prtmd3
     c                   eval      bgdmd4 = prtmd4
     c                   endif
      **
     c                   endsr
      **************************************************************************
      /SPACE 3
      **************************************************************************
      **               FILE TO CALENDAR DATE CONVERSION              **
      *****************************************************************
     c     srcymd        begsr
     c                   call      'XFXCYMD'
     c                   parm                    ydate
     c                   parm                    mdate
     c                   endsr
      *****************************************************************
     c     srcmdy        begsr
     c                   call      'XFXCMDY'
     c                   parm                    mdate
     c                   parm                    ydate
     c                   endsr
      *****************************************************************
     c     srmind        begsr
     c                   call      'XFXMIND'
     c                   parm                    ydate1
     c                   parm                    ydays
     c                   parm                    ydate2
     c                   endsr
     c*****************************************************************
     c     sraddd        begsr
     c                   call      'XFXADDD'
     c                   parm                    ydate1
     c                   parm                    ydays
     c                   parm                    ydate2
     c                   endsr
      *****************************************************************
      **                     CLEAR TABLE FIELDS                      **
      *****************************************************************
     c     srctab        begsr
     c                   eval      tcode = *blanks
     c                   eval      ecode = *blanks
     c                   eval      hmap = *blanks
     c                   eval      edate = 0
     c                   eval      sdesc = *blanks
     c                   eval      ldesc = *blanks
     c                   eval      tind = *blanks
     c                   endsr
      *****************************************************************
      **                        TABLE SEARCH                         **
      *****************************************************************
     c     srtabl        begsr
     c                   call      'XFXTABL'
     c                   parm                    tcode
     c                   parm                    ecode
     c                   parm                    hmap
     c                   parm                    edate
     c                   parm                    sdesc
     c                   parm                    ldesc
     c                   parm                    tind
      **
     c                   endsr
      *****************************************************************
      /EJECT
      *****************************************************************
     c     chkchg        begsr
     c                   call      'XFXCHKB2'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm                    bbtrak
     c                   parm                    rtnflg
     c                   endsr
      *****************************************************************
     c     srcgbl        begsr
     c                   call      'XFXCKBLS'
     c                   parm                    bgdlv6
     c                   parm                    bgdtac
     c                   parm                    bgdseq
     c                   parm      bbtrak        biltrk
     c                   parm                    action
     c                   endsr
      *****************************************************************
      **        PHONE NUMBER LOOKUP SUBROUTINE                       **
      *****************************************************************
     c     srphnl        begsr
     c                   call      'HXXPHNL'
     c                   parm                    levl6p
     c                   parm                    reqnum
     c                   parm                    reqnm2
     c                   parm                    reqsrc
     c                   parm                    reqtyp
     c                   parm      0             rtnphn
     c                   parm      *blanks       rtnxtn
     c                   endsr
      *****************************************************************
     c     srptos        begsr
     c                   call      'HBXPTOS'
     c                   parm                    wkpayr
     c                   parm                    wkplan
     c                   parm                    bgdct1
     c                   parm                    bgdct2
     c                   parm                    bgdct3
     c                   parm                    rqport
     c                   parm                    rtplac
     c                   parm                    rttype
     c                   endsr
      *****************************************************************
     c     clrdetl       begsr
     c                   if        z > 0
     c                   if        ((amt(z) <= 0
     c                               and (sndizc = 'N' or grsnet <> 'G'))
     c                             or (sndizc = 'Y' and amt(z) < 0
     c                                 and grsnet = 'G'))
     c                             and cpt(z) <> 'G8553' and cpt(z) <> 'G8443'
     c                   eval      dtf(z) = *blanks
     c                   eval      dtt(z) = *blanks
     c                   eval      dtf6(z) = *blanks
     c                   eval      dtt6(z) = *blanks
     c                   eval      dttm(z) = *blanks
     c                   eval      dtfm(z) = *blanks
     c                   eval      dttd(z) = *blanks
     c                   eval      dtfd(z) = *blanks
     c                   eval      dtty(z) = *blanks
     c                   eval      dtfy(z) = *blanks
     c                   eval      pbf(z) = 0
     c                   eval      pbt(z) = 0
     c                   eval      pos(z) = *blanks
     c                   eval      eps(z) = *blanks
     c                   eval      cpt(z) = *blanks
     c                   eval      ddx(z) = *blanks
     c                   eval      mod(z) = *blanks
     c                   eval      cds(z) = *blanks
     c                   eval      pdsc(z) = *blanks
     c                   eval      amt(z) = 0
     c                   eval      amta(z) = 0
     c                   eval      qty(z) = 0
     c                   eval      qty4(z) = 0
     c                   eval      qty2(z) = 0
     c                   eval      qty5(z) = 0
     c                   eval      dix(z) = *blanks
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   eval      npi(z) = *blanks
     c                   eval      z = z - 1
     c                   endif
     c                   endif
      **
     c                   if        z >= dtline                                  =IF PG FULL,
     c** print 0.00 in box 24f instead of blanks
     c                   eval      *in74 = *off
     c     1             do        z             xxy
     c                   if        amt(xxy) = 0
     c                   eval      *in74 = *on
     c                   endif
     c                   enddo
     c**
     c                   exsr      newpag                                        PRINT AND ADVANCE
     c                   endif
      **
     c                   eval      savdte = bgdtsd
     c                   movel     bgdtpr        savprc
     C                   IF        DTLINE > 6
     c                   eval      z = z + 1
     C                   ELSE
     C                   EVAL      Z= Z + 1
     c                   eval      dtf(z) = *blanks
     c                   eval      dtt(z) = *blanks
     c                   eval      dtf6(z) = *blanks
     c                   eval      dtt6(z) = *blanks
     c                   eval      dttm(z) = *blanks
     c                   eval      dtfm(z) = *blanks
     c                   eval      dttd(z) = *blanks
     c                   eval      dtfd(z) = *blanks
     c                   eval      dtty(z) = *blanks
     c                   eval      dtfy(z) = *blanks
     c                   eval      pbf(z) = 0
     c                   eval      pbt(z) = 0
     c                   eval      pos(z) = *blanks
     c                   eval      eps(z) = *blanks
     c                   eval      cpt(z) = *blanks
     c                   eval      ddx(z) = *blanks
     c                   eval      mod(z) = *blanks
     c                   eval      cds(z) = *blanks
     c                   eval      pdsc(z) = *blanks
     c                   eval      amt(z) = 0
     c                   eval      amta(z) = 0
     c                   eval      qty(z) = 0
     c                   eval      qty4(z) = 0
     c                   eval      qty2(z) = 0
     c                   eval      qty5(z) = 0
     c                   eval      dix(z) = *blanks
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   eval      npi(z) = *blanks
     C                   ENDIF
      **
     c                   endsr
     c**********************************************************************
     c/space 3
     c     srasgn        begsr
     c**
B001 c                   if        status = *blanks
 001 c                   eval      status = 'AT'
E001 c                   endif
     c**
     c                   call      'XFXMASGN'
     c                   parm      srlv6         lv6#
     c                   parm      sract         acct#
     c                   parm                    status
     c                   parm      bbtodt        rqsdte
     c                   parm                    doc#
     c**
B001 c                   if        doc# = 0
 001 c                   eval      status = 'AD'
 001 c                   call      'XFXMASGN'
 001 c                   parm      srlv6         lv6#
 001 c                   parm      sract         acct#
 001 c                   parm                    status
 001 c                   parm      bbtodt        rqsdte
 001 c                   parm                    doc#
E001 c                   endif
     c**
     c                   endsr
     c*****************************************************************
     c**  Bill Exception 179                                         **
     c*****************************************************************
     c/space 3
     c     sre179        begsr
     c**
     c     hld24k        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      prthnm = hmdnam
     c                   eval      prttyp = hmdtyp
     c*****              eval      rqdoctor# = msgdr#
     c                   eval      rqdoctor# = hmddr#
     c                   eval      rqlictype = 'STATE'
     c                   eval      rqstate = hx6st1
     c                   eval      rqstatechk = 'Y'
     c                   eval      rqcat2 = *blanks
     c                   exsr      srlicense
     c                   movel     rtnlicense    prrg#
     c                   movel     rtntherapist  prtcod
     c                   movel     rtncat2       thrct2
     c                   endif
     c**
     c                   endsr
      *****************************************************************
      **             (SETPTR) SETUP PRINTER FILE ARRAYS              **
      *****************************************************************
B0   c     setptr        begsr
      **
     c                   movel(p)  ldafpr        prtr
     c****               movel(p)  'HBR1500C'    prtf
      **
      /free
       clear rw;
       clear cl;
       clear inc;
      /end-free
     c     prtfky        klist
     c                   kfld                    prtr
     c                   kfld                    prtf
      **
     c     prtfky        chain     hxfprrc                            76
B1   c                   if        *in76 = *off
     c     prtfky        setll     hxfprrc
B2   c                   dou       %eof(hxpprrc)
     c     prtfky        reade     hxfprrc
B3   c                   if        not %eof(hxpprrc)
     c                   eval      ct = hxseqn
     c                   eval      rw(ct) = hxrow
     c                   eval      cl(ct) = hxcol
     c                   eval      inc(ct) = hxincr
E3   c                   endif
E2   c                   enddo
X1   c                   else
     c     prtf          setll     hxfprrcd
B2   c                   dou       %eof(hxlprrcd)
     c     prtf          reade     hxfprrcd
B3   c                   if        not %eof(hxlprrcd)
     c                   eval      ct = hxseqn
     c                   eval      rw(ct) = hxrow
     c                   eval      cl(ct) = hxcol
     c                   eval      inc(ct) = hxincr
E3   c                   endif
E2   c                   enddo
E1   c                   endif
      **
E0   c                   endsr
     c*****************************************************************
     c     src2aut       begsr
      **
     c                   call      'HBXC2AUT'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm                    rqiseq
     c                   parm                    bgdct2
     c                   parm      bbfrdt        cursvd
     c                   parm      *blanks       reqtra
      **
     c                   endsr
     c*****************************************************************
     c     srasaut       begsr
      **
      ** See if there is a specific authorization for the doctor on the charge
      **
     c                   call      'HBXASAUT'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm                    mmmrno
     c                   parm                    rqiseq
     c                   parm      bgdtth        reqcod
     c                   parm      ' '           reqprc
     c                   parm      bgdtsd        cursvd
     c                   parm      *blanks       reqtra
     c                   parm      ' '           reqexp
      **
      ** See if there is any referral for a visit on this date
      **
     c                   if        reqtra = ' '
     c                   call      'HBXASAUT'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm                    mmmrno
     c                   parm                    rqiseq
     c                   parm      0             reqcod
     c                   parm      ' '           reqprc
     c                   parm      bgdtsd        cursvd
     c                   parm      *blanks       reqtra
     c                   parm      'V'           reqexp
     c                   endif
      **
     c                   endsr
      **************************************************************************
     c     srlicense     begsr
      **
     c                   call      'XFXLICE'
     c                   parm      bbplv6        rqlevel6
     c                   parm      bbaccn        rqaccount
     c                   parm                    rqdoctor#
     c                   parm                    rqlictype
     c                   parm                    rqstate
     c                   parm                    rqstatechk
     c                   parm                    rqcat2
     c                   parm                    rtnlicense
     c                   parm                    rtntherapist
     c                   parm                    rtncat2
     c                   parm                    rtnexpire
      **
     c                   endsr
     c**************************************************************************
     c     pyradr        begsr
     c                   call      'HXXPYRADR'
     c                   parm                    payor
     c                   parm                    plan
     c****               parm                    policy
     c                   parm                    prtplcy
     c                   parm                    bbplv6
     c                   parm                    bbmrno
     c                   parm                    bbaccn
     c                   parm                    rqiseq
     c                   parm      bbtodt        reqdat
     c                   parm                    xfbadr
     c                   parm                    xfbad2
     c                   parm                    xfbcty
     c                   parm                    xfbsta
     c                   parm                    xfbzip
     c                   parm                    xfbzp2
     c                   parm                    xfbtel
     c                   parm                    xfbfax
     c                   parm                    xfbatn
     c                   parm                    rtnprn
     c**
     c                   endsr
     *****************************************************************
     C     gtEom         begSr
     C                   call      'XFXGTEOM'
     C                   parm                    cmoyr
     C                   parm                    mDate
     C                   endSr
      **********************************************************************
      ** This routine reads through all charges that will be on the bill  **
      ** before they're processed.  Originally it was just used for       **
      ** service dates but has been expanded to calculate other values    **
      ** that need to be calculated before charges are processed          **
      **********************************************************************
     c     getDates      begsr
      **
     c                   eval      firstDate = 0
     c                   eval      lastDate = 0
     c                   eval      totChgBill = 0
      **
     c     chgkey        setll     hbfchr15
     c                   dou       *in70 = *on
     c     nxtrd2        tag
     c     chgkey        reade     hbfchr15                               70
     c                   if        *in70 = *off
     c                             and bgdtsd >= bbfrdt
     c                             and bgdtsd <= bbtodt
      **
     c                   eval      action = *blanks
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c                   exsr      srcgbl
     c     action        cabeq     'N'           nxtrd2
     c                   endif
      **
     c                   select
     c                   when      bbiseq = bgdsq1
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbu1        cabeq     ' '           nxtrd2
     c                   endif
     c                   if        bbpayr <> bgdub1
     c                             or bbplan <> bgdpl1
     c                             or bbplcy <> bgdpo1
     c                   if        belong = 'N'
     c                   goto      nxtrd2
     c                   endif
     c                   endif
     c                   eval      wrkfld = 0
     c                   if        *in69 = *on
     c                             and bbbind = 9
     c                   eval      wrkfld = bgdci1 + bgdcp1
     c                   eval      wrkfld = wrkfld + bgddm1
     c                   if        bgdgp1 = 0
     c                             and wrkfld = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   endif
     c                   endif
     c                   exsr      srctab
     c                   movel     'BFCL'        tcode
     c                   movel     bgdfc1        ecode
     c                   exsr      srtabl
     c                   move      hmap          hcsfc
     c                   if        hcsfc = 'MC'
     c                             and bgdgp1 = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   else
     c                   if        bgdfc1 = 'MD'
     c                             and bgdgp1 = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   else
     c                   endif
     c                   endif
     c                   when      bbiseq = bgdsq2
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbu2        cabeq     ' '           nxtrd2
     c                   endif
     c                   if        bbpayr <> bgdub2
     c                             or bbplan <> bgdpl2
     c                             or bbplcy <> bgdpo2
     c                   if        belong = 'N'
     c                   goto      nxtrd2
     c                   endif
     c                   endif
     c                   eval      wrkfld = 0
     c                   if        *in69 = *on
     c                             and bbbind = 9
     c                   eval      wrkfld = bgdci2 + bgdcp2
     c                   eval      wrkfld = wrkfld + bgddm2
     c                   if        bgdgp2 = 0
     c                             and wrkfld = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   endif
     c                   endif
     c                   exsr      srctab
     c                   movel     'BFCL'        tcode
     c                   movel     bgdfc2        ecode
     c                   exsr      srtabl
     c                   move      hmap          hcsfc
     c                   if        hcsfc = 'MC'
     c                             and bgdgp2 <= 0
     c                             and bgdap2 <= 0
     c                             and bgdci2 <= 0
     c                             and bgdpp2 <= 0
     c                             and bgdcp2 <= 0
     c                             and bgddm2 <= 0
     c                             and bgdaj2 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   else
     c                   if        bgdfc2 = 'MD'
     c                             and bgdgp2 <= 0
     c                             and bgdap2 <= 0
     c                             and bgdci2 <= 0
     c                             and bgdpp2 <= 0
     c                             and bgdcp2 <= 0
     c                             and bgddm2 <= 0
     c                             and bgdaj2 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   else
     c                   endif
     c                   endif
     c                   when      bbiseq = bgdsq3
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbu3        cabeq     ' '           nxtrd2
     c                   endif
     c                   if        bbpayr <> bgdub3
     c                             or bbplan <> bgdpl3
     c                             or bbplcy <> bgdpo3
     c                   if        belong = 'N'
     c                   goto      nxtrd2
     c                   endif
     c                   endif
     c                   eval      wrkfld = 0
     c                   if        *in69 = *on
     c                             and bbbind = 9
     c                   eval      wrkfld = bgdci3 + bgdcp3
     c                   eval      wrkfld = wrkfld + bgddm3
     c                   if        bgdgp3 = 0
     c                             and wrkfld = 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   endif
     c                   endif
     c                   exsr      srctab
     c                   movel     'BFCL'        tcode
     c                   movel     bgdfc3        ecode
     c                   exsr      srtabl
     c                   move      hmap          hcsfc
     c                   if        hcsfc = 'MC'
     c                             and bgdgp3 <= 0
     c                             and bgdap3 <= 0
     c                             and bgdci3 <= 0
     c                             and bgdpp3 <= 0
     c                             and bgdcp3 <= 0
     c                             and bgddm3 <= 0
     c                             and bgdaj3 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   else
     c                   if        bgdfc3 = 'MD'
     c                             and bgdgp3 <= 0
     c                             and bgdap3 <= 0
     c                             and bgdci3 <= 0
     c                             and bgdpp3 <= 0
     c                             and bgdcp3 <= 0
     c                             and bgddm3 <= 0
     c                             and bgdaj3 <= 0
     c                             and bgdtpr <> 'G8443' and bgdtpr <> 'G8553'
     c                   goto      nxtrd2
     c                   endif
     c                   endif
     c                   when      bbiseq = 1
     c                   if        bgdtpr <> 'G8553' and bgdtpr <> 'G8443'
     c     bgdbup        cabeq     ' '           nxtrd2
     c                   endif
     c                   other
     c                   goto      nxtrd2
     c                   endsl
      **
     c                   if        bgdtsd < mmaddt
     c                   eval      bgdtsd = mmaddt
     c                   else
     c                   if        mmdsdt > 0
     c                             and bgdtsd > mmdsdt
     c                   eval      bgdtsd = mmdsdt
     c                   endif
     c                   endif
      **
     c                   if        bgdtam > 0
     c                             or bgdtc4 = 'G8553' or bgdtc4 = 'G8443'

     c                   if        firstDate = 0 or bgdtsd < firstDate
     c                   eval      firstDate = bgdtsd
     c                   endif
     c                   if        lastDate = 0
     c                   eval      lastDate = bgdtsd
     c                   endif
     c                   if        bgdtsd > lastDate
     c                   eval      lastDate = bgdtsd
     c                   endif
     c                   if        bgdfsd > lastDate
     c                   eval      lastDate = bgdtsd
     c                   endif
     c                   endif
      **
     c                   eval      totChgBill += bgdtam
      **
     c                   endif
     c                   enddo
      **
     c                   if        exc682 = *on
     c                   eval      ydate = firstDate
     c                   exsr      srcymd
     c                   eval      box18a = mdate
      **
     c                   if        mdate <> 0
     c                   move      mdate         dtf18
     c                   z-add     firstDate     mmdd6
     c                   move      dtf18         yy6
     c                   move      wkdat6        dtf618
     c                   move      mm6           dtfm18
     c                   move      dd6           dtfd18
     c                   move      yy6           dtfy18
     c                   endif
      **
     c                   eval      ydate = lastDate
     c                   exsr      srcymd
     c                   eval      box18b = mdate
      **
     c                   if        mdate <> 0
     c                   move      mdate         dtt18
     c                   z-add     lastDate      mmdd6
     c                   move      dtt18         yy6
     c                   move      wkdat6        dtt618
     c                   move      mm6           dttm18
     c                   move      dd6           dttd18
     c                   move      yy6           dtty18
     c                   endif
     c                   endif

     c                   if        exc953 = *on
     c                   eval      box18a = 0
     c                   eval      box18b = 0
     c                   eval      dtf18 = *blanks
     c                   eval      dtf618 = *blanks
     c                   eval      dtfm18 = *blanks
     c                   eval      dtfd18 = *blanks
     c                   eval      dtfy18 = *blanks
     c                   eval      dtt18 = *blanks
     c                   eval      dtt618 = *blanks
     c                   eval      dttm18 = *blanks
     c                   eval      dttd18 = *blanks
     c                   eval      dtty18 = *blanks
     c                   endif
      **
     c                   endsr
      **********************************************************************
      ** EXCEPT763 - Build box21 as well as up to 4 diagnosis per charge
      **                based of care date diagnosis
      **********************************************************************
      /free
        begsr except763;

          clear ds763;
          chain mmpct1 hxfctg1;
          if %found(hxpctg1) and (hxprty='O' or hxprty='E');
            usenub = *on;
          else;
            usenub = *off;
          endif;

          setll bbtrak hbfchgbl;
          dou %eof(hbpchgbl);
            reade bbtrak hbfchgbl;
            if not %eof(hbpchgbl);
              chain chgkeysq hbfchrg;
              if %found(hbpchrg) and bgctqt <> 0;
                addedCareDG = *off;
                bgdlv6 = bgclv6;
                bgdtpr = bgctpr;
                chain procky xffprocc;
                if %found(hxpprocc);
                  if usenub = *on and xfpnub <> 0;
                    xfpubc = xfpnub;
                  endif;

                  select;
                  when xfpubc >= 420 and xfpubc <= 429;
                    btsthr = 'PT';
                  when      xfpubc >= 430 and xfpubc <= 439;
                    btsthr = 'OT';
                  when xfpubc >= 440 and xfpubc <= 449;
                    btsthr = 'SP';
                  other;
                    iter;
                  endsl;

                  setgt tscdkey hbftscd;
                  dou %eof(hbltscdd);
                    readpe tscdky2 hbftscd;
                    if not %eof(hbltscdd)
                           and bgctsd >= btsstd and bgctsd <= btsend;
                      for i = 1 to 14;
                        chain mdiagky hmfmdiag;
                        if not %found(hmpmdiag);
                          tscdDiag(x) = *blanks;
                        else;
                          addedCareDG = *on;
                        endif;
                      endfor;
                      if addedCareDG = *on;
                        ex763Ct += 1;
                        ds763(ex763Ct).chgSeq  = bgcbsq;
                        ds763(ex763Ct).chgdg1  = btsdia;
                        ds763(ex763Ct).chgdg2  = btsdx2;
                        ds763(ex763Ct).chgdg3  = btsdx3;
                        ds763(ex763Ct).chgdg4  = btsdx4;
                        ds763(ex763Ct).chgdg5  = btsdx5;
                        ds763(ex763Ct).chgdg6  = btsdx6;
                        ds763(ex763Ct).chgdg7  = btsdx7;
                        ds763(ex763Ct).chgdg8  = btsdx8;
                        ds763(ex763Ct).chgdg9  = btsdx9;
                        ds763(ex763Ct).chgdg10 = btsdx0;
                        ds763(ex763Ct).chgdg11 = btsmx1;
                        ds763(ex763Ct).chgdg12 = btsmx2;
                        ds763(ex763Ct).chgdg13 = btsmx3;
                        ds763(ex763Ct).chgdg14 = btsmx4;
                        leave;
                      endif;
                    endif;
                  enddo;

                endif;
                if not addedCareDG;
                  ex763Ct += 1;
                  ds763(ex763Ct).chgSeq  = bgcbsq;
                endif;
              endif;
            endif;
          enddo;

          //spin through first time and to get at least 1 diag per charge in list
          for i = 1 to ex763Ct;
            ds763d(i).seq = ds763(i).chgSeq;
            if ds763(i).chgdg1 <> *blanks;
              wrkDiag = ds763(i).chgdg1;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg2 <> *blanks;
              wrkDiag = ds763(i).chgdg2;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg3 <> *blanks;
              wrkDiag = ds763(i).chgdg3;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg4 <> *blanks;
              wrkDiag = ds763(i).chgdg4;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg5 <> *blanks;
              wrkDiag = ds763(i).chgdg5;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg6 <> *blanks;
              wrkDiag = ds763(i).chgdg6;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg7 <> *blanks;
              wrkDiag = ds763(i).chgdg7;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg8 <> *blanks;
              wrkDiag = ds763(i).chgdg8;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg9 <> *blanks;
              wrkDiag = ds763(i).chgdg9;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg10 <> *blanks;
              wrkDiag = ds763(i).chgdg10;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg11 <> *blanks;
              wrkDiag = ds763(i).chgdg11;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg12 <> *blanks;
              wrkDiag = ds763(i).chgdg12;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg13 <> *blanks;
              wrkDiag = ds763(i).chgdg13;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
            if ds763(i).chgdg14 <> *blanks;
              wrkDiag = ds763(i).chgdg14;
              exsr tryToAddDg;
              if dgCnt = 1;
                iter;
              endif;
            endif;
          endfor;


          //spin through second time and fill in remaining diagnosis for each charge
          for i = 1 to ex763Ct;
            if ds763(i).chgdg1 <> *blanks;
              wrkDiag = ds763(i).chgdg1;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg2 <> *blanks;
              wrkDiag = ds763(i).chgdg2;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg3 <> *blanks;
              wrkDiag = ds763(i).chgdg3;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg4 <> *blanks;
              wrkDiag = ds763(i).chgdg4;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg5 <> *blanks;
              wrkDiag = ds763(i).chgdg5;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg6 <> *blanks;
              wrkDiag = ds763(i).chgdg6;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg7 <> *blanks;
              wrkDiag = ds763(i).chgdg7;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg8 <> *blanks;
              wrkDiag = ds763(i).chgdg8;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg9 <> *blanks;
              wrkDiag = ds763(i).chgdg9;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg10 <> *blanks;
              wrkDiag = ds763(i).chgdg10;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg11 <> *blanks;
              wrkDiag = ds763(i).chgdg11;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg12 <> *blanks;
              wrkDiag = ds763(i).chgdg12;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg13 <> *blanks;
              wrkDiag = ds763(i).chgdg13;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
            if ds763(i).chgdg14 <> *blanks;
              wrkDiag = ds763(i).chgdg14;
              exsr tryToAddDg;
              if dgCnt = 4;
                iter;
              endif;
            endif;
          endfor;

        endsr;

        //-------------------------
        // set dates for ExCode 75
        //-------------------------
        begsr excode75;

          omtype = 'P';
          chain exokey hbpbfxo;
          if not %found(hbpbfxo)
             or (%found(hbpbfxo) and bxodlt = 'D');
            ydate = bbfrdt;
            exsr srcymd;
            dtf(z) = %editc(mdate:'X');
            dtf6(z) = %subst(dtf(z):1:4) + %subst(dtf(z):7:2);
            dtfm(z) = %subst(dtf(z):1:2);
            dtfd(z) = %subst(dtf(z):3:2);
            dtfy(z) = %subst(dtf(z):7:2);
          endif;

        endsr;

        //-------------------------------------------------
        // get Billing proivder info to be used in EDI 837
        //-------------------------------------------------
        begsr getBillingInfo;

           exc935 = *on;

           // get billing provider name to populate loop 2010AA
           prmlv6 = bbplv6;
           payor = bbpayr;
           plan = bbplan;
           prmct1 = mmpct1;
           prmct2 = mmpct2;
           prvid = 'BN83N';
           prmprc = *blanks;
           prmrev = *zeros;
           prmdte = *zeros;
           prvvar = *blanks;
           exsr srprv;
           if prvvar <> *blanks;
              wx33nm = prvvar;
           endif;

           //get billing provider address to populate loop 2010AA
           prmlv6 = bbplv6;
           payor = bbpayr;
           plan =  bbplan;
           prmct1 = mmpct1;
           prmct2 = mmpct2;
           prvid = 'BN837';
           prmprc = *blanks;
           prmrev = *zeros;
           prmdte = *zeros;
           prvvar = *blanks;
           exsr srprv;
           if prvvar <> *blanks;
              rtnaddress = prvvar;
              wx33a1 = rtnadr;
              wx33a2 = rtnad2;
              wx33ct = rtncty;
              wx33st = rtnst ;
              wx33z5 = rtnzp1;
              wx33z4 = rtnzp2;
           endif;

           // get billing provider phone number to populate loop 2010AA
           prmlv6 = bbplv6;
           payor = bbpayr;
           plan = bbplan;
           prmct1 = mmpct1;
           prmct2 = mmpct2;
           prvid = 'BN83P';
           prmprc = *blanks;
           prmrev = *zeros;
           prmdte = *zeros;
           prvvar = *blanks;
           exsr srprv;
           if prvvar <> *blanks;
              wx33tl = prvvar;
           endif;

        endsr;

        //------------------------------------------
        // get PayTo Provider info to populate Box33
        //------------------------------------------
        begsr getPayToInfo;

           //get pay-to provider name
           prmlv6 = bbplv6;
           payor = bbpayr;
           plan = bbplan;
           prmct1 = mmpct1;
           prmct2 = mmpct2;
           prvid = 'PAYTN';
           prmprc = *blanks;
           prmrev = *zeros;
           prmdte = *zeros;
           prvvar = *blanks;
           exsr srprv;
           if prvvar <> *blanks;
              lvl2nm = prvvar;
              wxFfNm = prvVar;
           endif;

           //get pay-to provider address
           prmlv6 = bbplv6;
           payor = bbpayr;
           plan =  bbplan;
           prmct1 = mmpct1;
           prmct2 = mmpct2;
           prvid = 'PAYTO';
           prmprc = *blanks;
           prmrev = *zeros;
           prmdte = *zeros;
           prvvar = *blanks;
           exsr srprv;
           if prvvar <> *blanks;
              rtnaddress = prvvar;
              lvl2ad = %trim(rtnadr) + ' ' + %trim(rtnad2);
              lvl2ct = rtncty;
              lvl2st = rtnst;
              lvl2zp = rtnzp1;
              lvl2z2 = rtnzp2;

              wxFfA1 = rtnAdr;
              wxFfA2 = rtnAd2;
              wxFfCt = rtnCty;
              wxFfSt = rtnSt;
              if (rtnZp2 <> ' ');
                wxFfZp = rtnZp1 + '-' + rtnZp2;
              else;
                wxFfZp = rtnZp1 + '-0000';
              endif;


           endif;

           //get pay-to provider phone number
           payor = bbpayr;
           plan = bbplan;
           prmct1 = mmpct1;
           prmct2 = mmpct2;
           prmlv6 = bbplv6;
           prvid = 'PAYTP';
           prmprc = *blanks;
           prmrev = 0;
           prmdte = *zeros;
           prvvar = *blanks;
           exsr srprv;
           if prvvar <> *blanks;
              monitor;
                 lvl2tl = %dec(%trim(prvvar):10:0);
              on-error;
                 clear lvl2tl;
              endmon;
              wxFfTl = prvVar;
           endif;

        endsr;

      /end-free

      **********************************************************************
      ** tryToAddDg - try to add a diag to dxv for exeption 763
      **                also set up the 5010 diagnoses (EDIAG)
      **********************************************************************
      /free
        begsr tryToAddDg;

          dgCnt = 0;
          if ds763d(i).dg1 <> *blanks;
            dgCnt = 1;
          endif;
          if ds763d(i).dg2 <> *blanks;
            dgCnt = 2;
          endif;
          if ds763d(i).dg3 <> *blanks;
            dgCnt = 3;
          endif;
          if ds763d(i).dg4 <> *blanks;
            dgCnt = 4;
          endif;

          if ds763d(i).dg1 = wrkDiag or
             ds763d(i).dg2 = wrkDiag or
             ds763d(i).dg3 = wrkDiag or
             ds763d(i).dg4 = wrkDiag;
            leavesr;
          endif;

          dc = %lookup(wrkDiag : ediag);
          if dc = 0;
            dc = %lookup(*blanks : dxv);
            if dc <> 0;
              if exc714 = *off;
                xfxdxper (wrkDiag : icdver : returndx);
                dxv(dc) = returndx;
              else;
                dxv(dc) = wrkDiag;
              endif;
              ediag(dc) = wrkDiag;
            else;
              dgCnt = 4;
              leavesr;
            endif;
          endif;
            select;
            when dgCnt = 0;
              ds763d(i).dg1 = ediag(dc);
            when dgCnt = 1;
              ds763d(i).dg2 = ediag(dc);
            when dgCnt = 2;
              ds763d(i).dg3 = ediag(dc);
            when dgCnt = 3;
              ds763d(i).dg4 = ediag(dc);
            endsl;
            dgCnt +=  1;

        endsr;

      **********************************************************************
      ** getInitialTreatmentDate: pull initial treatment date from TMPMASTTR
      **                table
      **********************************************************************
        begSr getIniTreatDt;

           // get initial treatment date from TMPMASTTR
           // if not found, pull from HBPCHRG file

           clear initTrtDt;
           EXEC SQL SELECT trITDt INTO :initTrtDt
                      FROM tmpMasttr T
                     INNER JOIN tmpMast M
                        ON m.patient_Id = t.trPatId
                     WHERE mmpLv6 = :bbpLv6 AND mmAcct = :bbAccn
                     FETCH FIRST 1 ROW ONLY;
           if sqlcode = 100 or (sqlCode = 0 and initTrtDt = 0);
              EXEC SQL SELECT bgcTSd INTO :initTrtDt
                         FROM hbpChrg
                        WHERE bgcLv6 = :bbpLv6 AND bgcTAc = :bbAccn
                          AND bgcTQt > 0
                        ORDER BY bgcTSd
                        FETCH FIRST 1 ROW ONLY;
           endif;

        endSr;

      **********************************************************************
      ** srucnv - Convert units based on the Time Conversion Code
      **********************************************************************
        begsr srucnv;
          xfpcnv = *blanks;
          xfxproc( bbplv6 : prmprc : bgdtsd : xfpdsc : xfpamt : xfptxa :
                   xfprev : xfpubc : xfpnub : xfpcpt : xfpovr : xfppfg :
                   xfpnch : xfpbid : xfpvrp : xfpmod : xfpmd2 : xfpmd2 :
                   xfpmd4 : xfpcnv : xfpgfg : xfptim : xfpctx );

          if xfpcnv <> *blanks;
            xfxcvtim( prmInu : xfpcnv : '2' : rtncod );
          endif;

        endsr;

      /end-free
      **********************************************************************
      /EJECT
      *----------------------------------------------------------------
     o****printr3   e            alignPage      1 08
     O****                                            5 'X'
     O****                                           12 'X'
     O****                                           19 'X'
     O****                                           28 'X'
     O****                                           35 'X'
     O****                                           43 'X'
     O****                                           49 'X'
      *----------------------------------------------------------------
     o****          e            billpg           01
     o****                       insbil              24
     o****                       tobill              35
     o****               39                          49 'ATTACHMENT'
     o****                       wrkpyr        4     58
     o****              n13                          59 '/'
     o****                       wrkpln        4     64
     o****                   page          4     83
     o****                       pageno        4     83
      *----------------------------------------------------------------
     o****          e            billpg           02
     o****                       bbplv6        4     35
     o****                       bbmrno        4     45
     o****                       prinam              80
      *----------------------------------------------------------------
     o****          e            billpg           03
     o****                       priadr              72
      *----------------------------------------------------------------
     o****          e            billpg           04
     o****                       priad2              72
      *----------------------------------------------------------------
     o****          e            billpg           05
     o****                       pricty              67
     o****                       prista              70
     o****                       prizip              76
      *----------------------------------------------------------------
     o****          e            billpg           06
     o****                       priatn              37
      *----------------------------------------------------------------
     o****          e            billpg           08
     o****                       box01x1              4
     o****                       box01x2             11
jmd  o****                       box01x3             48
     o****                       box01a              73
      *----------------------------------------------------------------
     o****          e            billpg           10
     o****                       bgname              30
     o****                 n27   prbdmm              35
     o****                 n27   prbddd              38
     o****              n27n62   prbdyy              43
     o****              n27 62   prbdyr              42
     o****                 n27   box03m              45
     o****                 n27   box03f              50
     o****                       box04               79
      *----------------------------------------------------------------
     o****          e            billpg           12
     o****                   bdpad1              24
     o****                       resad1              24
     o****                       box06a              36
     o****                       box06b              41
     o****                       box06c              45
     o****                       box06d              50
     o****                       box07a              73
      *----------------------------------------------------------------
     o****          e            billpg           14
     o****                   bdpcty              19
     o****                       rescty              19
     o****                       bdpsta              31
     o****                       box07b              68
     o****                       box07c              78
      *----------------------------------------------------------------
     o****          e            billpg           16
     o****                  23   bdpzp1               9
     o****                       bdphtl              29 '   -   -    '
     o****                                           21 ' '
     o****                       box07d              58
     o****                       box07e              79 '   -   -    '
     o****                                           71 ' '
      *----------------------------------------------------------------
     o****          e            billpg           18
     o****                  24   prisd2              30
     o****                       prgrpx              70
      *----------------------------------------------------------------
     o****          e            billpg           20
     o****                  24   prinn2              18
     o****                 n24   prtpol              24
     o****                       box10ay             38
     o****                       box10an             44
     o****                       prs1mm              57
     o****                       prs1dd              60
     o****                 n63   prs1yy              66
     o****                  63   prs1yr              64
     o****                       box11am             71
     o****                       box11af             78
      *----------------------------------------------------------------
     o****          e            billpg           22
     o****                       box10by             38
     o****                       box10bn             44
     o****                       box10bs             49
     o****                       box11bq             54
     o****                       box11b              81
      *----------------------------------------------------------------
     o****          e            billpg           24
     o****                       box10cy             38
     o****                       box10cn             44
     o****                       box11c              81
      *----------------------------------------------------------------
     o****          e            billpg           26
     o****                  24   prinm2              32
     o****                       box10d              51
     o****                       box11dy             55
     o****                       box11dn             60
      *----------------------------------------------------------------
     o****          e            billpg           30
     o****                       box12               26
     o****                 n65   praddt              51 '  /  /    '
     o****                  65   pradt6              51 '  /  /  '
     o****                       box13               76
      *----------------------------------------------------------------
     o****          e            billpg           32
     o****              n40n29   indtmm               6
     o****              n40n29   indtdd               9
     o****              n66n40n29indtyy              14
     o****               66n40n29indtyr              12
     o****              n29      box14q              22
     o****                       box15q              36
     o****               28      bx15mm              41
     o****               28      bx15dd              44
     o****               28 66   bx15yy              49
     o****               28n66   bx15yr              47
     o****                       box16a        4     65
     o****                       box16b        4     78
      *----------------------------------------------------------------
     o****          e            billpg           33
     o****                       prrupn              47
      *----------------------------------------------------------------
     o****          e            billpg           34
     o****                       box17q               6
     o****                       prrphy              30
     o****                       prrnpi              45
     o****                       box18a        4     65
     o****                       box18b        4     78
      *----------------------------------------------------------------
     o****          e            billpg           36
     o****                       box19               51
     o****                       box20y              55
     o****                       box20n              60
     o****                       box20a        z     73
      *----------------------------------------------------------------
     o****          e            billpg           37
     o****                       icdind              45
      *----------------------------------------------------------------
     o****          e            billpg           38
     o****              n59      dxv(1)              12
     o****              n59      dxv(2)              25
     o****              n59      dxv(3)              39
     o****              n59      dxv(4)              51
     o****                       box22a              63
     o****                       box22b              82
      *----------------------------------------------------------------
     o****          e            billpg           39
     o****              n59      dxv(5)              12
     o****              n59      dxv(6)              25
     o****              n59      dxv(7)              39
     o****              n59      dxv(8)              51
      *----------------------------------------------------------------
     o****          e            billpg           40
     o****              n59      dxv(9)              12
     o****              n59      dxv(10)             25
     o****              n59      dxv(11)             39
     o****              n59      dxv(12)             51
     o****                       prauth              70
      *----------------------------------------------------------------
     o****          e            billpg           43
     o****               38      cds(1)              55
     o****                       bx24i1              70
     o****                       pin(1)              80
      *----------------------------------------------------------------
     o****          e            billpg           44
     o****              n58n60   dtf(1)              12
     o****              n58n60   dtt(1)              21
     o****               58n60   dtf6(1)             11
     o****               58n60   dtt6(1)             20
     o****              n58 60   dtfm(1)             06
     o****              n58 60   dttm(1)             15
     o****              n58 60   dtfd(1)             09
     o****              n58 60   dttd(1)             18
     o****              n58 60   dtfy(1)             11
     o****              n58 60   dtty(1)             20
     o****                       pos(1)              24
     o****                       cpt(1)              36
     o****                       mod011              38
     o****                       mod012              41
     o****                       mod013              44
     o****                       mod014              47
     o****              n59      dix(1)              57
     o****               59      ddx(1)              60
     o****              n74      amt(1)        m     62
     o****               74      amta(1)             61 '     0 .  '
     o****              n73      qty(1)        m     65
     o****               73      qty4(1)       m     65
     o****                       eps(1)              68
     o****                       npi(1)              80
      *----------------------------------------------------------------
     o****          e            billpg           45
     o****               38      cds(2)              55
     o****                       bx24i2              70
     o****                       pin(2)              80
      *----------------------------------------------------------------
     o****          e            billpg           46
     o****              n58n60   dtf(2)              12
     o****              n58n60   dtt(2)              21
     o****               58n60   dtf6(2)             11
     o****               58n60   dtt6(2)             20
     o****              n58 60   dtfm(2)             06
     o****              n58 60   dttm(2)             15
     o****              n58 60   dtfd(2)             09
     o****              n58 60   dttd(2)             18
     o****              n58 60   dtfy(2)             11
     o****              n58 60   dtty(2)             20
     o****                       pos(2)              24
     o****                       cpt(2)              36
     o****                       mod021              38
     o****                       mod022              41
     o****                       mod023              44
     o****                       mod024              47
     o****              n59      dix(2)              57
     o****               59      ddx(2)              60
     o****              n74      amt(2)        m     62
     o****               74      amta(2)             61 '     0 .  '
     o****              n73      qty(2)        m     65
     o****               73      qty4(2)       m     65
     o****                       eps(2)              68
     o****                       npi(2)              80
      *----------------------------------------------------------------
     o****          e            billpg           47
     o****               38      cds(3)              55
     o****                       bx24i3              70
     o****                       pin(3)              80
      *----------------------------------------------------------------
     o****          e            billpg           48
     o****              n58n60   dtf(3)              12
     o****              n58n60   dtt(3)              21
     o****               58n60   dtf6(3)             11
     o****               58n60   dtt6(3)             20
     o****              n58 60   dtfm(3)             06
     o****              n58 60   dttm(3)             15
     o****              n58 60   dtfd(3)             09
     o****              n58 60   dttd(3)             18
     o****              n58 60   dtfy(3)             11
     o****              n58 60   dtty(3)             20
     o****                       pos(3)              24
     o****                       cpt(3)              36
     o****                       mod031              38
     o****                       mod032              41
     o****                       mod033              44
     o****                       mod034              47
     o****              n59      dix(3)              57
     o****               59      ddx(3)              60
     o****              n74      amt(3)        m     62
     o****               74      amta(3)             61 '     0 .  '
     o****              n73      qty(3)        m     65
     o****               73      qty4(3)       m     65
     o****                       eps(3)              68
     o****                       npi(3)              80
      *----------------------------------------------------------------
     o****          e            billpg           49
     o****               38      cds(4)              55
     o****                       bx24i4              70
     o****                       pin(4)              80
      *----------------------------------------------------------------
     o****          e            billpg           50
     o****              n58n60   dtf(4)              12
     o****              n58n60   dtt(4)              21
     o****               58n60   dtf6(4)             11
     o****               58n60   dtt6(4)             20
     o****              n58 60   dtfm(4)             06
     o****              n58 60   dttm(4)             15
     o****              n58 60   dtfd(4)             09
     o****              n58 60   dttd(4)             18
     o****              n58 60   dtfy(4)             11
     o****              n58 60   dtty(4)             20
     o****                       pos(4)              24
     o****                       cpt(4)              36
     o****                       mod041              38
     o****                       mod042              41
     o****                       mod043              44
     o****                       mod044              47
     o****              n59      dix(4)              57
     o****               59      ddx(4)              60
     o****              n74      amt(4)        m     62
     o****               74      amta(4)             61 '     0 .  '
     o****              n73      qty(4)        m     65
     o****               73      qty4(4)       m     65
     o****                       eps(4)              68
     o****                       npi(4)              80
      *----------------------------------------------------------------
     o****          e            billpg           51
     o****               38      cds(5)              55
     o****                       bx24i5              70
     o****                       pin(5)              80
      *----------------------------------------------------------------
     o****          e            billpg           52
     o****              n58n60   dtf(5)              12
     o****              n58n60   dtt(5)              21
     o****               58n60   dtf6(5)             11
     o****               58n60   dtt6(5)             20
     o****              n58 60   dtfm(5)             06
     o****              n58 60   dttm(5)             15
     o****              n58 60   dtfd(5)             09
     o****              n58 60   dttd(5)             18
     o****              n58 60   dtfy(5)             11
     o****              n58 60   dtty(5)             20
     o****                       pos(5)              24
     o****                       cpt(5)              36
     o****                       mod051              38
     o****                       mod052              41
     o****                       mod053              44
     o****                       mod054              47
     o****              n59      dix(5)              57
     o****               59      ddx(5)              60
     o****              n74      amt(5)        m     62
     o****               74      amta(5)             61 '     0 .  '
     o****              n73      qty(5)        m     65
     o****               73      qty4(5)       m     65
     o****                       eps(5)              68
     o****                       npi(5)              80
      *----------------------------------------------------------------
     o****          e            billpg           53
     o****               38      cds(6)              55
     o****                       bx24i6              70
     o****                       pin(6)              80
      *----------------------------------------------------------------
     o****          e            billpg           54
     o****              n58n60   dtf(6)              12
     o****              n58n60   dtt(6)              21
     o****               58n60   dtf6(6)             11
     o****               58n60   dtt6(6)             20
     o****              n58 60   dtfm(6)             06
     o****              n58 60   dttm(6)             15
     o****              n58 60   dtfd(6)             09
     o****              n58 60   dttd(6)             18
     o****              n58 60   dtfy(6)             11
     o****              n58 60   dtty(6)             20
     o****                       pos(6)              24
     o****                       cpt(6)              36
     o****                       mod061              38
     o****                       mod062              41
     o****                       mod063              44
     o****                       mod064              47
     o****              n59      dix(6)              57
     o****               59      ddx(6)              60
     o****              n74      amt(6)        m     62
     o****               74      amta(6)             61 '     0 .  '
     o****              n73      qty(6)        m     65
     o****               73      qty4(6)       m     65
     o****                       eps(6)              68
     o****                       npi(6)              80
      *----------------------------------------------------------------
     o****          e            billpg           56
     o****                       taxid               19
     o****                       box25e              23
     o****                       box26               40
     o****                       box27y              42
     o****                       box27n              47
     o****              n50      totchg        l     64
     o****              n50      totpmt        l     74
     o*****         n50      baldue        l     83
     o****               35      baldue        l     83
     o****               50      continue            82
      *----------------------------------------------------------------
     o****          e            billpg           57
     o****                       lvl2tl              81 '   -   -    '
      *----------------------------------------------------------------
     o****          e            billpg           58
     o****                       box321              52
     o****                       lvl2nm              83
      *----------------------------------------------------------------
     o****          e            billpg           59
     o****              n67      rptdte              24 '  /  /    '
     o****               67      rptdt6              24 '  /  /  '
     o****                       box322              52
     o****                       lvl2ad              79
      *----------------------------------------------------------------
     o****          e            billpg           60
     o****                       prthnm              25
     o****                       box323              41
     o****                       box324              44
     o****                       box325              50
     o****                       lvl2ct              68
     o****                       lvl2st              71
     o****                       lvl2zp              77
      *----------------------------------------------------------------
     o****          e            billpg           61
     o****                       prtcod              17
     o****                       box32a              36
     o****                       box32b              53
     o****                       box33a              63
     o****                       prtprv              81
      *----------------------------------------------------------------
     o****          e            billpg           62
     o****               53      lmn                 32
     o****               54      prg                 36
     o****               55      rxr                 40
     o****               56      phr                 44
      *****************************************************************
      /EJECT
     oprintr2   h    oa                     1 02
     o                                           12 'REPORT DATE:'
     o                       rundte              24 '  /  /    '
     o                       l1name             112
     o                                          193 'PAGE:'
     o                       page2         4    198
      *---------------------------------------------------------------*
     o          h    oa                     1
     o                                           12 'REPORT TIME:'
     o                       runtme              22 '  :  :  '
     o               82                          98 'APPROVED ELECTRONIC'
     o               82                         105 ' CLAIMS'
     o               83                         102 'APPROVED MANUAL CLAIMS'
     o               84                          98 'MANUAL CLAIMS NEEDING'
     o               84                         107 ' APPROVAL'
     o               85                          99 'ELECTRONIC CLAIMS NEEDIN'
     o               85                         109 'G APPROVAL'
     o                       pgmnam             198
      *---------------------------------------------------------------*
     o          h    oa                     2
     o                       l6name             112
      *---------------------------------------------------------------*
     o          h    oa                     2
     o               u7                         100 '***** SCRUB *****'
     o              nu7                         100 '***** FINAL *****'
      *---------------------------------------------------------------*
     o          h    oa                     1
     o                                           72 'FROM       TO'
     o                                          174 ' MEDICAID  PRIOR'
     o                                          196 'DIAGNOSIS'
      *---------------------------------------------------------------*
     o          h    oa                     0
     o                                           16 '   _________  __'
     o                                           40 '__                      '
     o                                           63 '  _______________  ____'
     o                                           89 '     ____        _______'
     o                                          111 '______  ______________'
     o                                          131 ' ___________  _____'
     o                                          151 '____   __________'
     o                                          182 '________  _____________'
     o                                          196 '_________'
      *---------------------------------------------------------------*
     o          h    oa                     1
     o***                                           16 '   ACCOUNT #  NA'
     o***                                           40 'ME                      '
     o                       prtnumh             12
     o                                           18 'NAME'
     o                                           63 '  TRACKING NUMBER  DATE'
     o                                           89 '     DATE        TOTAL C'
     o                                          111 'HARGES  TOTAL PAYMENTS'
     o                                          131 ' BALANCE DUE  PAYOR'
     o                                          151 'PLAN   PROVIDER #'
     o                                          182 'RESOURCE  AUTHORIZATION'
     o                                          191 'CODE'
      *---------------------------------------------------------------*
     o          ef           detail         1
     o***                       bbaccn        4     12
     o                       prtnum        4     12
     o                       bbname              40
     o                       bbtrak              56
     o                       bbfrdt              67
     o                       bbtodt              78
     o                       totchg        l     93
     o                       totpmt        l    109
     o                       baldue        l    122
     o                       bbpayr        4    132
     o                       bbplan        4    139
     o                       bbprov             156
     o                       prrupn             169
     o                       prauth             186
     o                       dxc(1)             193
      *---------------------------------------------------------------*
     o          ef           biltot         2
     o                                           11 'BILL TOTAL:'
     o                       bilpgs        3     30
     o                       bilchg        l     93
     o                       bilpmt        l    109
     o                       bildue        l    122
      *---------------------------------------------------------------*
     o          ef           acctot         0
     o                                            5 '____ '
      *---------------------------------------------------------------*
     o          ef           acctot         2
     o***                                           5 'ACCT '
     o***                      bbaccn        4     17
     o                       prtnums              4
     o                       prtnum        4     17
     o                                           24 'TOTAL:'
     o                       accpgs        3     30
     o                       accchg        l     93
     o                       accpmt        l    109
     o                       accdue        l    122
      *---------------------------------------------------------------*
     o          ef           plntot         0
     o                                            5 '____ '
      *---------------------------------------------------------------*
     o          ef           plntot         2
     o                                            5 'PLAN '
     o                       bbplan        4     10
     o                                           24 'TOTAL:'
     o                       plnpgs        3     30
     o                       plnchg        l     93
     o                       plnpmt        l    109
     o                       plndue        l    122
      *---------------------------------------------------------------*
     o          ef           paytot         0
     o                                            5 '____ '
      *---------------------------------------------------------------*
     o          ef           paytot         2
     o                                            5 'PAYR '
     o                       bbpayr        4     11
     o                                           24 'TOTAL:'
     o                       paypgs        3     30
     o                       paychg        l     93
     o                       paypmt        l    109
     o                       paydue        l    122
      *---------------------------------------------------------------*
     o          ef           lv6tot         0
     o                                            5 '____ '
      *---------------------------------------------------------------*
     o          ef           lv6tot         2
     o                                            5 'SITE '
     o                       bbplv6        4     11
     o                                           24 'TOTAL:'
     o                       lv6pgs        3     30
     o                       lv6chg        l     93
     o                       lv6pmt        l    109
     o                       lv6due        l    122
      *---------------------------------------------------------------*
     o          ef           totals      1  1
     o                                           13 'TOTAL BILLS: '
     o                       rptbls        4     30
     o                       rptchg        l     93
     o                       rptpmt        l    109
     o                       rptdue        l    122
      *---------------------------------------------------------------*
      *****************************************************************

      **************************************************************************
      *  GetMinutesToUnits - Get minutes to units for contract                 *
      **************************************************************************
       dcl-proc getMinutesPerUnit;

         dcl-pi *n packed(5:0);
           iPayor packed(6:0) const;
           iPlan packed(5:0) const;
           iCategory1 char(3) const;
           iCategory2 char(3) const;
           iServiceDate packed(8:0) const;
           iProcedure char(8) const;
         end-pi;

         // Declare local variables
         dcl-s minutesPerUnit packed(5:0) inz(0);

         // Get the minutes per unit value
         exec sql
           Select mnumpu
           into   :minutesPerUnit
           from   tbminunt
           where  mnuubn = :iPayor
           and    mnupln = :iPlan
           and    mnuct1 = :iCategory1
           and    mnuct2 = :iCategory2
           and   (mnustd <= :iServiceDate and mnuend >= :iServiceDate)
           and    mnuprc = :iProcedure
           and    mnudlf = ' '
           fetch first row only;

         if sqlCode <> 0;
           minutesPerUnit = 0;
         endif;

         return minutesPerUnit;

       end-proc;
