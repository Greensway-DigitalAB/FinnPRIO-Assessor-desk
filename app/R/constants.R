volumes <- c(Home = fs::path_home(), 
             "R:" = "R:/", 
             # "C:" = "C:/",
             "My Computer" = "/")  # Customize as needed

limits <- list(Minimum = 1, Likely = 1, Maximum = 1)
default_sim <- list(n_sim = 1000, seed = 1234, lambda = 1, w1 = 0.5, w2 = 0.5)