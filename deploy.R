
# Function to deploy app & data to shiny-server
#
# Copy & overwrite existing deployment

deployApp <- function(deploy = "app")
{
    if (deploy=="all"){
        # Deploy app to shiny-server
        file.copy(from = "ui.R", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
        file.copy(from = "server.R", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
        # Deploy source data & template to shiny-server
        file.copy(from = "./source/", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
        file.copy(from = "./template/", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
    } else if (deploy=="app"){
        # Deploy app to shiny-server
        file.copy(from = "ui.R", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
        file.copy(from = "server.R", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
    } else if (deploy=="data"){
        # Deploy source data & template to shiny-server
        file.copy(from = "./source/", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
        file.copy(from = "./template/", to = "/home/gord/shiny-server/ShinyKPI/", recursive = TRUE)
    } else {
        # Show usage
        print("Usage: all, app or data")
    }
}