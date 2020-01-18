# map AHS
setwd("c:\\presentations_papers\\paper2")
source("ahsMap.R")
patchMap(wdCrit=0.01, vCrit=0.33, 
         vx.path  = "c:\\doktor\\proof_of_concept\\mod_lr\\results",
         vy.path  = "c:\\doktor\\proof_of_concept\\mod_lr\\results",
         wd.path  = "c:\\doktor\\proof_of_concept\\mod_lr\\results",
         s.file   = "c:\\presentations_papers\\paper2\\lr\\sr.asc",
         out.path = "c:\\presentations_papers\\paper2\\lr\\patchMaps"
)


# track AHS availability in time
setwd("c:\\presentations_papers\\paper2")
source("ahsTrack.R")
ahsTrack(s.file     = "c:\\presentations_papers\\paper2\\lr\\sr.asc",
         patch.path = "c:\\presentations_papers\\paper2\\lr\\patchMaps\\",
         out.ahsAv  = "c:\\presentations_papers\\paper2\\lr\\ahsAv.asc"
)


# assess AHS connectivity
setwd("c:\\presentations_papers\\paper2")
source("ahsConn_t.R")
ahsConn_t(vx.path        = "c:\\doktor\\proof_of_concept\\mod_cr\\results",
          vy.path        = "c:\\doktor\\proof_of_concept\\mod_cr\\results",
          wd.path        = "c:\\doktor\\proof_of_concept\\mod_cr\\results", 
          s.file         = "c:\\presentations_papers\\paper2\\cr\\sr.asc", 
          patch.path     = "c:\\presentations_papers\\paper2\\cr\\patchMaps",
          ctresults.file = "c:\\presentations_papers\\paper2\\cr\\c_t.txt",
          dtc            = 5,
          lambda         = 0.1,
          tend           = 365,
          seed.dens      = 1/10
)



