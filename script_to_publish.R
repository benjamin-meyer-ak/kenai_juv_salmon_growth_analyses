# SYNCH/PUBLISH BOOK TO BOOKDOWN.ORG (created this becuase the 'publish' button in corner of html interface window after render does not work)
library(rsconnect)

# clean out old versions
bookdown::clean_book(TRUE)

# render new version
bookdown:: render_book()

# publish book to https://bookdown.org/alaskasaurusrex_ak/kenai_juv_salmon_growth_analyses/
bookdown::publish_book(name = "kenai_juv_salmon_growth_analyses", account = "alaskasaurusrex_ak")
Y
