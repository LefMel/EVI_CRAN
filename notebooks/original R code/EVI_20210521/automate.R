library(cronR)

cmd <- cron_rscript("Downloads/World_data_for_shiny_update_all.R")

cron_add(command = cmd, frequency = 'daily', at = "6:00" , 
         id = 'update_COVID')

cmd2 <- cron_rscript("Downloads/World_data_for_shiny_update_all_US.R")

cron_add(command = cmd2, frequency = 'daily', at = "11:00" , 
         id = 'update_COVID_US')