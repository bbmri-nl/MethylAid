
width = "auto"
height = "400px"

start = "<br> <p style=\'width:800px;text-align:justify\'><span style=\'color:#000000;font-size:16px\'>"
  end = "</span></p><br><br>"

hdrUI <- paste0("MethylAid:",'<sup>', packageVersion("MethylAid"),'</sup>' ," Interactive Visualization of Illumina Human DNA Methylation array data")

disclaimer <- paste("These controls are used to detect bad quality samples.",
                    "The default 450k/EPIC specific thresholds are used.", collapse="")

htSDC <- paste("The sample-dependent controls can be used to evaluate",
           "performance across samples. These control oligos are",
           "designed for bisulfite-converted human genomic DNA",
           "sequences. Because target sequences do not contain CpG",
           "dinucleotides, the performance of the control oligos does",
           "not depend on the methylation status of the template DNA.", collapse="")

htSIC <- paste("Sample-independent controls evaluate the performance of",
           "specific steps in the process flow.", collapse="")

hdrFC = list(
  MU = "Median Methylated vs Unmethylated.",
  OP = "Overall quality control.",
  BS = "Bisulphite Conversion.",
  HC = "Hybridization",
  DP= "Detection P-values")

hdrSDC <- list(
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
to the DNA template.")

hdrSIC <- list(
  SC = "Staining controls: Staining controls are used to examine the efficiency of the staining step in both the red and green channels.",
  EC = "Extension controls: Extension controls test the extension efficiency of A, T, C, and G nucleotides from a hairpin probe, and are
therefore sample-independent. Both red (A,T) and green (C,G) channels are monitored.",
                  TR = "Target removal: Target removal controls test the efficiency of the stripping step after the extension reaction.",
                  HYB = "Hybridization controls: The hybridization controls test the overall performance of the entire assay using synthetic targets instead of
amplified DNA.")

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
