library(tidyverse)

science_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

(science <- science_raw  %>%
  select(-references, -links))

# Check which columns have missing data
science %>% 
  apply(2, anyNA)
# name                      birth                      death               occupation_s inventions_accomplishments 
# FALSE                       TRUE                       TRUE                       TRUE                       TRUE 


# identify those with missing occupation or invention/accomplishments
science %>% 
  filter(is.na(science$occupation_s) | is.na(science$inventions_accomplishments)) %>% 
  select(name, occupation_s, inventions_accomplishments)
# A tibble: 10 x 3
# name                occupation_s                inventions_accomplishments                                                                          
# <chr>               <chr>                       <chr>                                                                                               
# 1 Graves, Joseph L.   Evolutionary biologist      NA                                                                                                  
# 2 Greenaugh, Kevin    Nuclear engineer            NA                                                                                                  
# 3 Hall, Lloyd         Chemist                     NA                                                                                                  
# 4 Harris, James A.    NA                          Co-discovered Rutherfordium (element 104) and Dubnium (element 105) at Lawrence Livermore Laboratory
# 5 Hodge, John E.      Chemist                     NA                                                                                                  
# 6 McLurkin, James     Roboticist                  NA                                                                                                  
# 7 Mensah, Thomas      Inventor                    NA                                                                                                  
# 8 Renfroe, Earl       Orthodontist                NA                                                                                                  
# 9 Williams, Scott W.  Mathematician               NA                                                                                                  
# 10 Williams, Walter E. Economist; social scientist NA

# replace NA values based on information found on wikipedia page
science <- science %>%
  # add missing occupation for Harris, James A.
  mutate(occupation_s = replace_na(occupation_s, "Nuclear Chemist"))

  # inventions and accomplishments
  graves_JL <- "Evolutionary theory of aging; Myths and theories on race in American society"
  greenaugh_K <- "First African-American to receive a Ph.D. in nuclear engineering at the University of Maryland"
  hall_L <- "Multiple contributions in food preservation; Inducted into National Inventors Hall of Fame"
  hodge_J <- "Designer of the Hodge Scheme of Maillard reaction"
  mclurkin_J <- "Winner of Lemelson-MIT Student Prize; Co-creator of Honeybee Adventure project"
  mensah_T <- "Inducted into US National Academy of Inventors; Fiber optics and nanotechnology; President and CEO"
  renfroe_E <- "First African-American Department Chair at University of Illinois at Chicago College of Dentistry"
  williams_S <- "Founded the predecessor to the National Association of Mathematicians; Contributions to topology"
  williams_w <- "John M. Olin Distinguished Professor of Economics; Syndicated libertarian columnist and author"
  
  accomplishments <- list(graves_JL, 
                          greenaugh_K, 
                          hall_L, 
                          hodge_J, 
                          mclurkin_J, 
                          mensah_T, 
                          renfroe_E, 
                          williams_S, 
                          williams_w)
  
  (science_acc <- science %>%
    filter(is.na(inventions_accomplishments)) %>% 
    mutate(inventions_accomplishments = unlist(accomplishments))) %>% 
    select(-2:-4)
  # A tibble: 9 x 2
  # name                inventions_accomplishments                                                                        
  # <chr>               <chr>                                                                                             
  # 1 Graves, Joseph L.   Evolutionary theory of aging; Myths and theories on race in American society                      
  # 2 Greenaugh, Kevin    First African-American to receive a Ph.D. in nuclear engineering at the University of Maryland    
  # 3 Hall, Lloyd         Multiple contributions in food preservation; Inducted into National Inventors Hall of Fame        
  # 4 Hodge, John E.      Designer of the Hodge Scheme of Maillard reaction                                                 
  # 5 McLurkin, James     Winner of Lemelson-MIT Student Prize; Co-creator of Honeybee Adventure project                    
  # 6 Mensah, Thomas      Inducted into US National Academy of Inventors; Fiber optics and nanotechnology; President and CEO
  # 7 Renfroe, Earl       First African-American Department Chair at University of Illinois at Chicago College of Dentistry 
  # 8 Williams, Scott W.  Founded the predecessor to the National Association of Mathematicians; Contributions to topology  
  # 9 Williams, Walter E. John M. Olin Distinguished Professor of Economics; Syndicated libertarian columnist and author    
  
  # add missing inventions and/or accomplishments by joining
  (science <- left_join(science, science_acc, by = "name") %>% 
    mutate(inventions_accomplishments = coalesce(inventions_accomplishments.x, inventions_accomplishments.y), 
           birth = as.double(str_remove(birth.x, "\\.[[:alpha:]]?")),
           death = as.double(str_remove(death.x, "\\.[[:alpha:]]?")),
           occupation_s = str_remove(occupation_s.x, "\\.[[:alpha:]]?")) %>% 
    select(name,
           birth,
           death,
           occupation_s,
           inventions_accomplishments))
  
# A tibble: 120 x 5
# name              birth death occupation_s                     inventions_accomplishments                                                                    
#   <chr>             <dbl> <dbl> <chr>                            <chr>                                                                                         
# 1 Amos, Harold      1918  2003  Microbiologist                   First African-American department chair at Harvard Medical School                             
# 2 Alcorn, George E~ 1940    NA  Physicist; inventor              Invented a method of fabricating an imaging X-ray spectrometer                                
# 3 Andrews, James J. 1930  1998  Mathematician                    Put forth the Andrews–Curtis conjecture in group theory with Morton L. Curtis, still unsolved 
# 4 Alexander, Archie 1888  1958  Civil engineer                   Responsible for the construction of many roads and bridges, including the Whitehurst Freeway,~
# 5 Bailey, Leonard ~ 1825  1918  Inventor                         Folding bed                                                                                   
# 6 Ball, Alice Augu~ 1892  1916  Chemist                          Extracted chaulmoogra oil for the treatment of Hansen's disease (leprosy)                     
# 7 Banneker, Benjam~ 1731  1806  Almanac author; surveyor; farmer Constructed wooden clock; astronomer; mathematician; assisted in the survey of the original b~
# 8 Banyaga, Augustin 1947    NA  Mathematician                    Work on diffeomorphisms and symplectomorphisms                                                
# 9 Bashen, Janet     1957    NA  Inventor; entrepreneur; profess~ First African-American woman to receive a patent for a web-based software invention, LinkLine~
# 10 Bath, Patricia   1942  2019  Ophthalmologist                  First African-American female physician to receive a patent for a medical invention; inventio~
# # ... with 110 more rows

# Check again which columns have missing data
science %>% 
  apply(2, anyNA)

# name                      birth                      death               occupation_s inventions_accomplishments 
# FALSE                       TRUE                       TRUE                      FALSE                      FALSE

# re-add wikipedia page links then save .csv file
wiki <- science_raw %>% 
  select(name, links)

science_complete <- left_join(science, wiki, by = "name")

write_csv(science_complete, path = "./aa_science_complete.csv")