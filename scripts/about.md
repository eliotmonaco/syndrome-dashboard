# Purpose

This dashboard uses data downloaded from ESSENCE to track daily incidence of selected syndromes (e.g., specific diseases, broad disease categories, symptoms, or other health-related queries) in the Kansas City metro region. SaTScan software is then used to detect clusters of any syndromes that may occur.

# ESSENCE data

- Records of emergency room visits
- Missouri hospitals report within three business days
- Out of state hospitals report slower
- Data can be downloaded from the ESSENCE system based on either the location of the patient or the location of the hospital in the record.
    + ER visits by patient location: This dataset consists of records in which the patient's residential ZIP code is at least partly within Cass, Clay, Jackson, or Platte County. It includes ER visits to hospitals anywhere in the country by residents of these local ZIP codes. However, most of the visits occur at local hospitals. This is the dataset that most closely represents the stable population over time. (Assignment of ZIP code if a residential ZIP code isn't available?)
    + ER visits by hospital location: This dataset consists of records in which the hospital where the patient was seen is located in Cass, Clay, Jackson, or Platte County. It includes ER visits by local residents and non-residents. This dataset should be responsive to changes in the non-resident population, e.g., due to an increase of visitors during the World Cup event series.
- Counts of ER visits do not represent diagnoses for any condition (do they?). They represent...
- Data may contain duplicates. An effort is made to deduplicate, but there is a potential duplicate error rate (table).

# SaTScan analysis

- Describe software
- Describe analysis used
- Provide parameters, start/end dates, etc.






