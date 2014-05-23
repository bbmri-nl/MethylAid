headers = list(
  ##controls used for filtering of bad quality samples
  MU = "Median Methylated versus Unmethylated .",
  OP = "Overall probe quality control using non-polymorphic controls.",
  BS = "Bisulphite conversion .",
  ##sample-dependent controls
  BSI = "Bisulphite conversion controls (Type I): These controls use the Infinium I probe design and allele-specific single base extension
to monitor efficiency of bisulfite conversion.",
  BSII = "Bisulphite conversion controls (TypeII): These controls use Infinium II probe design and single base extension to monitor the
efficiency of bisulfite conversion.",
  SPI = "Specificity controls I: These controls are designed to monitor allele-specific extension for Infinium I probes.",
  SPII = "Specificity controls II: These controls are designed to monitor extension specificity for Infinium II probes and
check for potential non-specific detection of methylation signal over unmethylated
background.",
  NP = "Non-polymorphic controls: Non-polymorphic controls test the overall performance of the assay, from amplification
to detection, by querying a particular base in a non-polymorphic region of the bisulfite
genome.",
  NC = "Negative controls: Negative control probes are randomly permutated sequences that should not hybridize
to the DNA template.",
  ##sample-independent controls
  SC = "Staining controls: Staining controls are used to examine the efficiency of the staining step in both the red and green channels.",
  EC = "Extension controls: Extension controls test the extension efficiency of A, T, C, and G nucleotides from a hairpin probe, and are
therefore sample-independent. Both red (A,T) and green (C,G) channels are monitored.",
  TR = "Target removal: Target removal controls test the efficiency of the stripping step after the extension reaction.",
  HYB = "Hybridization controls: The hybridization controls test the overall performance of the entire assay using synthetic targets instead of
amplified DNA."
  )

qcProbes = list(
  BSI = "^BISULFITE CONVERSION I$",
  BSII = "^BISULFITE CONVERSION II$",
  SPI = "^SPECIFICITY I$",
  SPII = "^SPECIFICITY II$",
  NP = "^NON-POLYMORPHIC$",
  NC = "^NEGATIVE$",
  SC = "^STAINING$",
  EC = "^EXTENSION$",
  TR = "^TARGET REMOVAL$",
  HYB = "^HYBRIDIZATION$"
  ) ## we don't use the normalization controls NORM_A, NORM_G, NORM_C or NORM_T
