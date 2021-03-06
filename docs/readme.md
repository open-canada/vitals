<!-- Open Canada Vital Statistics Application Resources
Analysis of Official Data Related to COVID-19 Vaccine Efficacy and Safety
-->

Analysis of Official Data Related to COVID-19 Vaccine Efficacy and Safety
==================================================
[open-canada.github.io/vitals](https://open-canada.github.io/vitals) 


Disclaimer: The authoritative source for COVID-19 information is [Canada.ca/coronavirus](https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19.html). The results and views presented here are those by the authors and are not to be misconstrued as representing  views of
any Canadian Agency, University, or Department. They were developed and gathered by the [R4GC Data Science community](https://open-canada.github.io/r4gc/index.html#r4gc-community) as part of the community training and socializing, mostly on authors' own time and using authors' own resources.
All codes behind the analysis and supporting documents are free, open and available in [GitHub](https://github.com/open-canada/vitals/).  Comments and corrections are welcome.




### Talking about "The Elephant in the Room". 
<!-- How to defend the constitutional freedom and scientific integgity in the times of crisis-->

<!-- by a Senior Government of Canada Data Scientist, Dr. Dmitry Gorodnichy, the lead and moderator of the [R4GC Data Science community](https://open-canada.github.io/r4gc/index.html#r4gc-community)  group, -
, which started on January 29, 2021 and which continues till present,
-->

<!--   , all bound by the [Code of Conduct](conduct_code_conduite_eng.pdf) which instructs employees to refrain from making  criticisms of  the Government
of Canada, 
We refrain from making public criticisms of the CBSA and/or the Government
of Canada including posting critical comments on social media fora. -->

The following presentation was given to the Government of Canada employees, who expressed interest in learning about the  latest Government of Canada data related to COVID-19 vaccination. 
A considerable portion of this presentation was about how to create a safe and respectful environment within Government of Canada where Government of Canada employees can discuss this topic ("The Elephant in the Room"), which is already causing much of polarization within Canadian public, including among the Government of Canada employees, and which has  led to  the largest in the Canadian history demonstrations of protests  of Canadians against the government.
government, demanding it to revoke its "Mandatory Vaccination Policies" <!-- as being scientifically, ethically,  legally, and economically unjustified. -->
The solution to  this problem  was seen in keeping the discussion focused entirely on  Canada's own sources and open data, much of which -- thanks to the [Open Goverment and Scientific Integrity](https://open-canada.github.io/r4gc/open-policies.html#open-policies) policies introduced by the  Liberal Government several years ago -- have now become available, including  statistics of the number of deaths by cause and by vaccination status, which can be analyzed  and effectively visualized using contemporary  open source data science tools such as [R](https://cran.r-project.org/web/packages/cansim/index.html) and [RStudio](https://shiny.rstudio.com/gallery/).  
<!-- Another part of the presenation -->

<!-- A[open-canada.github.io/vitals](https://open-canada.github.io/vitals)  lot of effort in this presentation was given to applaud many of the Government of Canada efforts -->


- ["One Year since Vaccination. What we have learnt - using Open Canada Data and Data Science"](https://open-canada.github.io/vitals/OneYearOfVaccineInCanada-deck-meetup-2022-02-04.pdf), 
Special Edition presentation for the R4GC Community 'Lunch and Learn' Meetup  by the [R4GC Data Science community](https://open-canada.github.io/r4gc/index.html#r4gc-community)  lead, Senior Data Scientist, Dr. Dmitry Gorodnichy (4 February 2022).

<!--
- Shorter version of the above deck, modified for larger audience is [here]()

-->

<a href="https://open-canada.github.io/vitals/OneYearOfVaccineInCanada-deck-meetup-2022-02-04.pdf" style="text-align: center">
<img src="https://open-canada.github.io/vitals/OneYearOfVaccineInCanada-s1.png" width="500" align="center"></a>


### White Papers and Tools:

- [Guide on how to report COVID-19 vaccine side-effects in Canada as a consumer](https://open-canada.github.io/vitals/report-side-effect)
- [Interactive *Open Canada Vitals Statitistcs (Deaths) Tracker* App](https://open-canada.github.io/Apps/vitals) 
<!-- - ["One Year since Vaccination. What we have learnt - using Open Canada Data and Data Science"](https://github.com/open-canada/vitals/blob/main/docs/r4gc-meetu[open-canada.github.io/vitals](https://open-canada.github.io/vitals) p-2022-02-04-SpecialEdition(OneYearOfVaccineInCanada).pdf), 
Special Edition presentation for the R4GC Community 'Lunch and Learn' Meetup --> 
<!-- by the community lead, Senior Data Scientist, Dr. Dmitry Gorodnichy (4 February 2022) -->

-  ["Some observations from Canadian Vital Statistics Death (CVSD) Database related to
    the 'Elephant in the Room'"](https://open-canada.github.io/vitals/comment.pdf), Comment submission to The Lancet Journal (6 January 2022)
-  ["Additional  observations related to the 'Elephant in the Room' ???  from the Government of Canada COVID-19 daily epidemiology updates.  Time to act!"](https://open-canada.github.io/vitals/comment2.pdf), White paper (10 January 2022)  
  
  
<a href="https://open-canada.github.io/vitals/OneYearOfVaccineInCanada-s19.png">
<img src="https://open-canada.github.io/vitals/OneYearOfVaccineInCanada-s19.png" width="500">
</a>


-   ["Statistical analysis of official data sources related to vaccine efficacy and  safety (NEW EVIDENCE SINCE OCTOBER
    2021)"](https://open-canada.github.io/vitals/analysis)



### Sources and Findings

Last Updated: 2022-02-10 



[gc1] "Cases* following vaccination, COVID-19 Daily Epidemiology Update"*, Public Health Agency of Canada,  <https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html#a9> ([archived reports](https://github.com/open-canada/vitals/tree/main/docs/COVID-19%20epidemiology%20updates))

NB: The statistics reported in this source are computed using  death data since December 2020, when  there were no fully vaccinated Canadians yet, but when many cases happened (More exactly: There were 0 fully-vaccinated in December, < 1% for most of January,  <3%  until May). This is known as *algorithmic bias*, which skews results favourably towards fully-vaccinated cases, and which can be removed using  technique described in [this paper](https://open-canada.github.io/vitals/comment2.pdf). The numbers below are quoted directrly from the source.
  
  


 
<!-- 



- *Data extracted on January 28, 2022 for cases <u>from December 14, 2020</u>* <br>
  Unvaccinated 70.3%% vs. fully vaccinated **16.8%** (+3% increase since last week, +7.3% increase since last month) <p>
    

Fully vaccinated among COVID-19 deaths

Report Date (total cases)| Period reported for     | Reported deaths (with bias*)| Actual deaths (bias removed)   per week 
2022-02-10 (n=1,670,682) | 2020.12.14 - 2022.01.22 | 2,490 (19.5%) |  458  (80%)
2022-02-04 (n=1,458,433)| 2020.12.14 - 2022.01.15 | 2,032 (16.8%) |  


Unvaccinated among COVID-19 deaths

Report Date (total cases)| Period reported for     | Reported deaths (with bias*)| Actual deaths (bias removed)   per week 
2022-02-10 (n=1,670,682)| 2020.12.14 - 2022.01.22 | 2,490 (19.5%) |  458  (80%)
2022-02-04 (n=1,458,433)| 2020.12.14 - 2022.01.15 | 2,032 (16.8%) |  


* Significant bias is introduced in the reporting of Fully-vaccinated deaths by counting  Deaths since December 14, 2020 when there were very few fully-vaccinated  people back between than May.

-->

<!--


Proportion of Unvaccinated among COVID-19 deaths


[gc3]  **Deaths of COVID-19 by vaccination status** (Source:
    [[canada.ca](https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html#a9), [archived reports](https://github.com/open-canada/vitals/tree/main/docs/Epidemiological-summary-of-COVID-19-cases-in-Canada-Canada.ca)] -
    Table 2):


-->

###### COVID-19 deaths by Vaccination Status

 
- *on February 10, 2022 for cases from December 14, 2020 up until January 15, 2022 (n=1,670,682)* <br> 
unvaccinated 68.1%% vs. fully vaccinated **19.5%** (+2.7% increase since last week)
- *on February 04,  2022 for cases from December 14, 2020 up until January 15, 2022 (n=1,458,433)* <br> 
unvaccinated 70.3% vs. fully vaccinated **16.8%** (+3% increase since last week)
- *on January 21, 2022 for cases from December 14, 2020 up until January 08, 2022 (n=1,341,192)*: <br> 
unvaccinated 72.8% vs. fully vaccinated **13.8%** (+4.3% increase since December 14)
- *on December 15, 2021 for cases from December 14, 2020 up until November 27, 2021 (n=882,988)*: <br> 
unvaccinated 76.1% vs. fully vaccinated **9.5%**

<hr>

[ gc2a ] Hospitalizations by vaccination status, COVID-19 (coronavirus) in Ontario, <https://covid-19.ontario.ca/data/hospitalizations#hospitalizationsByVaccinationStatus>

[ gc2b ] For more information on cases following vaccination, please see the Weekly epidemiology report (PDF) available on the [Government of Canada???s COVID-19 data trends](https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19/epidemiological-economic-research-data.html) page

<!-- 

[gc1] **Hospitalizations by vaccination status** (Sources:
[canada.ca](https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19/epidemiological-economic-research-data.html) - weekly report (PDF), [ontario.ca](https://covid-19.ontario.ca/data/hospitalizations#hospitalizationsByVaccinationStatus))

-->

-  *???Incidence [of COVID-19 cases] among fully vaccinated cases has surpassed those among unvaccinated since mid-December 2021???*. 
-  In January 2022, close to 3/4 of hospitalized COVID patients in Ontario were fully vaccinated

<!-- -  The proportion of fully-vaccinated hospitalizations has been increasing since December 2021.-->
<!-- - and continues* to increase -->

<!-- ONTARIO
Last updated: February 3, 2022 at 10:31 a.m. (EST): Hosp. 536 vs 1383, ICU 179 vs 211
Last updated: February 6, 2022 at 10:31 a.m. (EST): 484 + 100, vs. 1264, 158 + 10 vs. 174
-->

<hr>

[gc3]: ???Reported side effects following COVID-19 vaccination in Canada???, Canadian COVID-19 vaccination safety report, Public Health Agency of Canada, <https://health-infobase.canada.ca/covid-19/vaccine-safety>
 
<!-- 
[gc4]  **COVID-19 vaccine adverse reactions (side-effects)** (Source: 
    [[canada.ca](https://health-infobase.canada.ca/covid-19/vaccine-safety/)] - Figure 1, Table 1)):
    
    -->


- Total (since Jan 2021) "Serious": *	7,678.  Weekly: 118
- Total (since Jan 2021) "Not serious":    28,486. 


- Between *May 2021 and Oct 2021* "Serious" side-effects: **5200** (increased from 5038 posted in last month report)
-  Of these: 1,111 heart diseases (including 64 Cardiac arrests, 77 Heart Attacks, 970
    Myocarditis1/Pericarditis), 947 thrombosis, 781 central nervous system failures
    (including 594 Bell's Palsy), 487 Anaphylaxis, 166 Auto-immune diseases, 37 Acute
    kidney injury, 25 Liver injury, and 208 deaths (some under investigation).
    

<!-- 
- Many are not reported or published yet, like the one  [here](https://open-canada.github.io/vitals/SideEffectReporting-example-1.pdf)
 - Adverse reaction, not reported yet in Canada but reported in US: -->

<hr>

[gc4] 
Recalls and safety alerts, Government of Canada: <https://recalls-rappels.canada.ca/>
(for 
[[Moderna](https://recalls-rappels.canada.ca/en/search/site?search_api_fulltext=moderna)], 
[[Pfizer](https://recalls-rappels.canada.ca/en/search/site?search_api_fulltext=pfizer)], 
[[Astrazeneca](https://recalls-rappels.canada.ca/en/search/site?search_api_fulltext=astrazeneca)]
):

- Risk of Facial paralysis/Bell???s Palsy. Posted: 2021-08-06 
- Risk of Myocarditis and pericarditis. Posted: 2021-06-30
- Risk of Capillary Leak Syndrome. Posted: 2021-06-29
- Risk of Thrombosis with Thrombocytopenia. Posted: 2021-03-24


<hr>

[ gc5 ] Statistics Canada, Canadian Vital Statistics - Death database . Provisional weekly death counts, by selected grouped causes of death.  <https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001>  (Dashboard: <https://open-canada.github.io/vitals>) 

<!-- 
[gc4]: **Deaths of COVID-19** (Source: GC
    [statcan.gc.ca](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001), [App
    Calculator](https://o-canada.shinyapps.io/vitals/#section-statistics)) - (Last record: 6 NOV 2021):
-->


- From December 14, 2020 to  April 30, 2021: 13075
- From May 1,  2021 to  October 31, 2021*: **2975** 
- Of these, deaths of COVID-19 without comorbidities: **404** (recomputed using [ukg1])
- Of these, deaths of COVID-19 without comorbidities for \<65 : **89** (recomputed  using [ukg1])

<!-- 
- For comparison (in the same period): Cancer -- , ...

- All COVID-19 since the start of pandemic: with comorbidities --, without comorbidities -- , without for \<65 years old -- 
- For comparison, since the start of pandemic other caueses of death: Cancer -- , ...
- For comparison, Deaths from Flu without comorbidities (10-year historical average): 
-->


<hr>

[gcA]  **Vaccination rates** (Source:
    [canada.ca](https://health-infobase.canada.ca/covid-19/vaccination-coverage/)):
    
- Dec 2020 - Apr 2021: High-risk population vaccination (rate: 0 -- 2%)
- May 2021 - Oct 2021: General public  vaccination (rate: **2% -- 75%**)



[gcB] **COVID-19 cases vs. COVID-19 deaths** (source: [google1](https://www.google.com/search?q=canada+covid+cases&oq=canada+covid+cases)):

- Canada population: 38.246.108
- Total COVID Cases (since March 2020): 3.150.000
- Total COVID Deaths (since March 2020): 35.00 or 1.1% Cases
- Of these, Most are comorbidities (>90% based on UKG and USG data)
- Of these, Most are in people 65+ (>90% based on GC, UKG, USG data)
- COVID Deaths for people younger than 65 with no health problems: < 0.01% Cases

<!-- 
- About 0.0xxx% of COVID-19 cases result in hospitalizations, 0.00yy% result in ICU, and 0.000z% in deaths, about 10% of which  COVID-deaths only - TBC
-->

<hr>

#### Data from other governments:
 
 
[ukg1] **COVID deaths vs. COVID alone deaths** (Source: [gov.uk](https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/deathsfromcovid19withnootherunderlyingcauses) - [Explanation](https://www.youtube.com/watch?v=9UHvwWWcjYw)):

- Of all reported in UK COVID deaths, only 13.6%  are due to COVID alone. 
- 78% of all COVID alone deaths occurred in people with 65+ of age.
- Average age of death from COVID is 82.5 years, which is statistically equal to the average life expectancy in UK.

 
[usg1] **Natural immunity vs. vaccine-induced immunity** (Source: [cdc.gov](https://www.cdc.gov/mmwr/volumes/71/wr/mm7104e1.htm) - [Explanation](https://www.youtube.com/watch?v=eK83QqbNOmU)):


- COVID vaccine reduces the risk of hospitalization considerably (by x10-x20)
- However, natural immunity  reduces the risk of hospitalization even more (by x20-x50),  while the chance of having severe complication from COVID infection for people younger than 65 and in good health is very low (<0.01% Cases based on [ukg1])  and for  children under 12 even lower (<0.0001% Cases based on [ukg2]).


<a href="https://open-canada.github.io/vitals/OneYearOfVaccineInCanada-s13.png" >
<img src="https://open-canada.github.io/vitals/OneYearOfVaccineInCanada-s13.png" width="500" >
</a>


    
<!-- 
#### CONCLUSION: Open Canada data that has become available *after* the start of general public vaccination  <u>do not</u> support the COVID-19  vaccine manufacturers\' claims about  vaccine efficacy and safety.
-->


<!-- 
#### Additional  references:  

 
- [Justice Centre for Constitutional Freedoms](https://www.jccf.ca)

-->

<!--  
Any Canadian can report vaccine side-effect, as a consumer, i.e., without a doctor, at the 
Heath Canada portal. This is how you do it:

-   Check if your symptoms are reported in the US COVID Vaccine Adverse Event Reports
    (VAERS): [vaers.hhs.gov](https://openvaers.com). 
-   Go to the Health Canada Voluntary Side Effect Reporting portal :
    <https://hpr-rps.hres.ca/side-effects-reporting-form.php?form=voluntary>
-   Submit electronic form there as shown in this 
    [Example](https://open-canada.github.io/vitals/SideEffectReporting-example-1.pdf). (
    There are three places there you need to know how to fill out: in Section B: Patient
    ID write your initials; in Section D: Product DIN and Product Name - paste the
    vaccine names (and batch/lot number, if available), as printed on your receipt. )
-   Call Heath Canada Voluntary Reporting support line: Tel. 866-234-2345, if you still
    have problems submitting the form, leave a message, they should respond within 24
    hours. )
    
-->

<!-- 
 -   Check if your symptoms are reported in the US COVID Vaccine Adverse Event Reports
    (VAERS): [vaers.hhs.gov](https://openvaers.com). 
-   Check if your vaccine batch (aka lot), which is printed on your vaccination receipt, is among the ["bad ones"](https://www.howbad.info), according to VAERS data. -->    
<!--   

-->

<hr>


 Corrections/Comments: [ open-canada-data-analysis @ ivim . ca ]     
 


