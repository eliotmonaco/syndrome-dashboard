# Essence syndrome queries for API

syn <- list(
  hepb = list(
    queryname = "Acute Hepatitis B v1",
    apistring = "ccddCategory=acute%20hepatitis%20b%20v1"
  ),
  resp = list(
    queryname = "CDC Broad Acute Respiratory DD v1",
    apistring = "ccddCategory=cdc%20broad%20acute%20respiratory%20dd%20v1"
  ),
  cpox = list(
    queryname = "CDC Chickenpox v2",
    apistring = "ccddCategory=cdc%20chickenpox%20v2"
  ),
  covid = list(
    queryname = "CDC COVID-Specific DD v1",
    apistring = "ccddCategory=cdc%20covid-specific%20dd%20v1"
  ),
  food = list(
    queryname = "CDC Food Poisoning v1",
    apistring = "ccddCategory=cdc%20food%20poisoning%20v1"
  ),
  hepa = list(
    queryname = "CDC Hepatitis A v1",
    apistring = "ccddCategory=cdc%20hepatitis%20a%20v1"
  ),
  ili = list(
    queryname = "ILI CCDD v1",
    apistring = "ccddCategory=ili%20ccdd%20v1"
  ),
  meas = list(
    queryname = "CDC Measles CCDD v1",
    apistring = "ccddCategory=cdc%20measles%20ccdd%20v1"
  ),
  mpox = list(
    queryname = "CDC Monkeypox DD v1",
    apistring = "ccddCategory=cdc%20monkeypox%20dd%20v1"
  ),
  pert = list(
    queryname = "Pertussis v2 CCDD Parsed",
    apistring = "ccddCategory=pertussis%20v2%20ccdd%20parsed"
  ),
  rsv = list(
    queryname = "CDC Respiratory Syncytial Virus v1",
    apistring = "ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1"
  ),
  shig = list(
    queryname = "CDC Shigella v1",
    apistring = "ccddCategory=cdc%20shigella%20v1"
  ),
  spox = list(
    queryname = "CDC Small Pox v1",
    apistring = "ccddCategory=cdc%20small%20pox%20v1"
  ),
  hus = list(
    queryname = "Hemolytic Uremic Syndrome (HUS) v1",
    apistring = "ccddCategory=hemolytic%20uremic%20syndrome%20(hus)%20v1"
  ),
  men = list(
    queryname = "Meningococcal Disease v1",
    apistring = "ccddCategory=meningococcal%20disease%20v1"
  ),
  mump = list(
    queryname = "Mumps v1",
    apistring = "ccddCategory=mumps%20v1"
  ),
  noro = list(
    queryname = "Norovirus v1",
    apistring = "ccddCategory=norovirus%20v1"
  ),
  drug = list(
    queryname = "CDC All Drug Overdose v3 Parsed",
    apistring = "ccddCategory=cdc%20all%20drug%20overdose%20v3%20parsed"
  ),
  alc = list(
    queryname = "CDC Alcohol v1",
    apistring = "ccddCategory=cdc%20alcohol%20v1"
  ),
  fent = list(
    queryname = "CDC Fentanyl Overdose v2 Parsed",
    apistring = "ccddCategory=cdc%20fentanyl%20overdose%20v2%20parsed"
  ),
  opd = list(
    queryname = "CDC Opioid Overdose v4 Parsed",
    apistring = "ccddCategory=cdc%20opioid%20overdose%20v4%20parsed"
  ),
  gun1 = list(
    queryname = "CDC Assault Firearm Injury v1",
    apistring = "ccddCategory=cdc%20assault%20firearm%20injury%20v1"
  ),
  gun2 = list(
    queryname = "CDC Firearm Injury v2",
    apistring = "ccddCategory=cdc%20firearm%20injury%20v2"
  ),
  traf = list(
    queryname = "CDC Pedestrian Motor Vehicle Traffic Injury v1",
    apistring = "ccddCategory=cdc%20pedestrian%20motor%20vehicle%20traffic%20injury%20v1"
  ),
  heat = list(
    queryname = "Heat Related Illness v2",
    apistring = "ccddCategory=heat%20related%20illness%20v2"
  )
)

syn <- lapply(syn, \(ls) {
  ls$apistring <- paste(
    ls$apistring,
    "medicalGroupingSystem=essencesyndromes",
    sep = "&"
  )

  ls
})

