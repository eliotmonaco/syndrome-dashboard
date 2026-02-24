# Essence syndrome queries for API

syn <- list(
  hepb = list(
    name = "hepatitis B",
    queryname = "Acute Hepatitis B v1",
    apistring = "ccddCategory=acute%20hepatitis%20b%20v1"
  ),
  resp = list(
    name = "respiratory conditions",
    queryname = "CDC Broad Acute Respiratory DD v1",
    apistring = "ccddCategory=cdc%20broad%20acute%20respiratory%20dd%20v1"
  ),
  cpox = list(
    name = "chickenpox",
    queryname = "CDC Chickenpox v2",
    apistring = "ccddCategory=cdc%20chickenpox%20v2"
  ),
  covid = list(
    name = "COVID-19",
    queryname = "CDC COVID-Specific DD v1",
    apistring = "ccddCategory=cdc%20covid-specific%20dd%20v1"
  ),
  food = list(
    name = "food poisoning",
    queryname = "CDC Food Poisoning v1",
    apistring = "ccddCategory=cdc%20food%20poisoning%20v1"
  ),
  hepa = list(
    name = "hepatitis A",
    queryname = "CDC Hepatitis A v1",
    apistring = "ccddCategory=cdc%20hepatitis%20a%20v1"
  ),
  ili = list(
    name = "influenza-like illness (ILI)",
    queryname = "ILI CCDD v1",
    apistring = "ccddCategory=ili%20ccdd%20v1"
  ),
  meas = list(
    name = "measles",
    queryname = "CDC Measles CCDD v1",
    apistring = "ccddCategory=cdc%20measles%20ccdd%20v1"
  ),
  mpox = list(
    name = "mpox",
    queryname = "CDC Monkeypox DD v1",
    apistring = "ccddCategory=cdc%20monkeypox%20dd%20v1"
  ),
  pert = list(
    name = "pertussis",
    queryname = "Pertussis v2 CCDD Parsed",
    apistring = "ccddCategory=pertussis%20v2%20ccdd%20parsed"
  ),
  rsv = list(
    name = "respiratory syncytial virus (RSV)",
    queryname = "CDC Respiratory Syncytial Virus v1",
    apistring = "ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1"
  ),
  shig = list(
    name = "shigella",
    queryname = "CDC Shigella v1",
    apistring = "ccddCategory=cdc%20shigella%20v1"
  ),
  spox = list(
    name = "smallpox",
    queryname = "CDC Small Pox v1",
    apistring = "ccddCategory=cdc%20small%20pox%20v1"
  ),
  hus = list(
    name = "hemolytic uremic syndrome (HUS)",
    queryname = "Hemolytic Uremic Syndrome (HUS) v1",
    apistring = "ccddCategory=hemolytic%20uremic%20syndrome%20(hus)%20v1"
  ),
  men = list(
    name = "meningococcal disease",
    queryname = "Meningococcal Disease v1",
    apistring = "ccddCategory=meningococcal%20disease%20v1"
  ),
  mump = list(
    name = "Mumps",
    queryname = "Mumps v1",
    apistring = "ccddCategory=mumps%20v1"
  ),
  noro = list(
    name = "norovirus",
    queryname = "Norovirus v1",
    apistring = "ccddCategory=norovirus%20v1"
  ),
  drug = list(
    name = "drug overdose",
    queryname = "CDC All Drug Overdose v3 Parsed",
    apistring = "ccddCategory=cdc%20all%20drug%20overdose%20v3%20parsed"
  ),
  alc = list(
    name = "alcohol overdose",
    queryname = "CDC Alcohol v1",
    apistring = "ccddCategory=cdc%20alcohol%20v1"
  ),
  fent = list(
    name = "fentanyl overdose",
    queryname = "CDC Fentanyl Overdose v2 Parsed",
    apistring = "ccddCategory=cdc%20fentanyl%20overdose%20v2%20parsed"
  ),
  opd = list(
    name = "opioid overdose",
    queryname = "CDC Opioid Overdose v4 Parsed",
    apistring = "ccddCategory=cdc%20opioid%20overdose%20v4%20parsed"
  ),
  gun1 = list(
    name = "assault firearm injury",
    queryname = "CDC Assault Firearm Injury v1",
    apistring = "ccddCategory=cdc%20assault%20firearm%20injury%20v1"
  ),
  gun2 = list(
    name = "firearm injury",
    queryname = "CDC Firearm Injury v2",
    apistring = "ccddCategory=cdc%20firearm%20injury%20v2"
  ),
  traf = list(
    name = "pedestrian injury by a motor vehicle",
    queryname = "CDC Pedestrian Motor Vehicle Traffic Injury v1",
    apistring = "ccddCategory=cdc%20pedestrian%20motor%20vehicle%20traffic%20injury%20v1"
  ),
  heat = list(
    name = "heat illness",
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

