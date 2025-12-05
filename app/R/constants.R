sysdrive <- system("wmic logicaldisk get name", intern = TRUE)
drives <- substr(sysdrive[-c(1, length(sysdrive))], 1, 1)
named_paths <- setNames(paste0(drives, ":/"), paste0(drives, ":"))

volumes <- c(Home = fs::path_home(), 
             named_paths, 
             "My Computer" = "/")  # Customize as needed

limits <- list(Minimum = 1, Likely = 1, Maximum = 1)
default_sim <- list(n_sim = 50000, seed = 1234, lambda = 1, w1 = 0.5, w2 = 0.5)