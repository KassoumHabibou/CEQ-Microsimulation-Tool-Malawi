
################################################################################
######### CEQ initial data wrangling file for MALAWI ###########################
################################################################################


HH_MOD_E <- read_dta(paste0(here(), hh_data_path, "/HH_MOD_E.dta"))

################### Household characteristics ##################################
basicvars_ihs5 <- read_dta(paste0(here(), hh_data_path, "/basicvars_ihs5.dta"))
indivivars_ihs5 <- read_dta(paste0(here(), data_folder,"/Household Surveys/IHS5 2019-20/Intermediate/indivivars_ihs5.dta"))
yd <- read_dta(paste0(here(), data_folder,"/Output/yd.dta"))
################### Individual Characteristics #################################

# --- Merge household and individual characteristics
curr_df <- HH_MOD_E %>%
  left_join(basicvars_ihs5, by = "HHID") %>%
  left_join(indivivars_ihs5, by = c("HHID", "PID")) %>%
  mutate(
    active = ifelse((15 <= age) & (age >= 64), 1, 0)
  )

# --- Binarize activity participation: hh_e06_1a to hh_e06_6
act_vars <- c("1a", "1b", "1c", "2", "3", "4", "5", "6")
for (act in act_vars) {
  var <- paste0("hh_e06_", act)
  if (var %in% names(curr_df)) {
    curr_df <- curr_df %>% 
      mutate(!!sym(var) := ifelse(!!sym(var)==1, 1, 0) %>% structure(label = attr(!!sym(var), "label")))
  }
}

# Rename hh_e06_8a and 8b
curr_df <- curr_df %>%
  rename(
    act_1 = hh_e06_8a,
    act_2 = hh_e06_8b
  )

# --- Compute agri and activities totals
curr_df <- curr_df %>%
  mutate(
    agri = rowSums(select(., starts_with("hh_e06_1")), na.rm = TRUE),
    activities = rowSums(select(., starts_with("hh_e06_")), na.rm = TRUE)
  )

# --- Labor profile
curr_df <- curr_df %>%
  mutate(
    labor = case_when(
      activities == 0 ~ 0,
      activities == 1 & agri == 1 ~ 1,
      (activities == 2 & agri == 2) | (activities == 3 & agri == 3) ~ 2,
      agri > 0 & activities > agri & hh_e06_4 < 1 ~ 3,
      agri > 0 & activities > agri & hh_e06_4 == 1 ~ 4,
      agri == 0 & hh_e06_4 == 1 ~ 5,
      agri == 0 & activities > agri & hh_e06_4 == 0 ~ 6
    ),
    i_profile = factor(labor, levels = 0:6,
                       labels = c(
                         "Not working", "Non-diversified farming", "Diversified farming",
                         "Farming + informal NF", "Farming + formal NF", "Formal NF", "Informal NF"
                       )
    )
  )

# --- Time expenditure: winsorize, recode 0 to NA, rename
vals <- c("05", "06", "07a", "07b", "07c", "08", "09", "10", "11", "12")

for (val in vals) {
  old_var <- paste0("hh_e", val)
  new_var <- paste0("i_time", val)
  
  if (old_var %in% names(curr_df)) {
    # Recode 0 to NA
    curr_df <- curr_df %>% 
      mutate(!!sym(old_var) := ifelse(!!sym(old_var)==0, NA, !!sym(old_var)) %>% structure(label = attr(!!sym(old_var), "label")))
    
    
    # Winsorize
    q <- quantile(curr_df %>% select(!!sym(old_var)) %>% pull(), probs = c(0.05, 0.95), na.rm = TRUE)
    
    curr_df <- curr_df %>% 
      mutate(!!sym(old_var) := ifelse(!!sym(old_var) > q[2], q[2], ifelse(!!sym(old_var) < q[1], q[1],!!sym(old_var))) %>% structure(label = attr(!!sym(old_var), "label")))
    
    
    # Rename
    curr_df <- curr_df %>% rename(!!sym(new_var) := !!sym(old_var))
  }
}

# --- Total time and shares
curr_df <- curr_df %>%
  mutate(i_time0 = rowSums(select(., starts_with("i_time")), na.rm = TRUE))

for (val in vals) {
  base_var <- paste0("i_time", val)
  share_var <- paste0("i_timeshare", val)
  
  curr_df <- curr_df %>% 
    mutate(!!sym(share_var) := ifelse(i_time0 == 0, 0, !!sym(base_var) / i_time0) %>% structure(label = paste0('Share of',attr(!!sym(base_var), "label"))))
  
}


curr_df <- curr_df %>%
  mutate(
    # Recode values
    hh_e07a_1 = case_when(
      hh_e07a_1 %in% c(1, 2) ~ 0,
      hh_e07a_1 %in% c(3, 4) ~ 1,
      is.na(hh_e07a_1) ~ 2,
      TRUE ~ hh_e07a_1
    ),
    # Rename and convert to labelled factor
    i11_profile = labelled(
      hh_e07a_1,
      labels = c(
        "Farm products for sale" = 0,
        "Farm products for own consumption" = 1,
        "Don't farm/produce" = 2
      )
    )
  )


# Assume your dataset is called df and already loaded

# Convert hh_e19a to lowercase
curr_df <- curr_df %>%
  mutate(hh_e19a = tolower(hh_e19a))

# Map hh_e19b codes to occupation labels
# occupation_labels <- c(
#  
# )

# Create the hh_e19a_activity column
curr_df <- curr_df %>%
  mutate(hh_e19a_activity = recode(as.character(hh_e19b), 
                                   `1` = "Physical Scientists and related technicians",
                                   `2` = "Architects, Surveyors and related workers",
                                   `3` = "Engineers and related workers",
                                   `4` = "Aircraft's and ships' officers",
                                   `5` = "Life scientists and related technicians",
                                   `6` = "Medical, dental and related workers",
                                   `7` = "Veterinary and related workers",
                                   `8` = "Statisticians, mathematicians, systems analysts",
                                   `9` = "Economist",
                                   `11` = "Accountants",
                                   `12` = "Jurists",
                                   `13` = "Teachers",
                                   `14` = "Workers in Religion",
                                   `15` = "Writers",
                                   `16` = "Artists",
                                   `17` = "Composers and Performing artists",
                                   `18` = "Athletics, sportsmen and related workers",
                                   `19` = "Professional and technical workers not elsewhere classified",
                                   `20` = "Legislative Officials and government senior administrators",
                                   `21` = "Managers",
                                   `22` = "Traditional Leaders",
                                   `30` = "Clerical supervisors",
                                   `31` = "Government administrative/secretarial officials",
                                   `32` = "Stenographers and related workers",
                                   `33` = "Book-keepers, cashiers and related workers",
                                   `34` = "Computing and machine operators of book-keeping machines, calculators and automatic data processing machines (computers)",
                                   `35` = "Transport and communication supervisors",
                                   `36` = "Transport conductors",
                                   `37` = "Mail distribution clerks",
                                   `38` = "Telephone and telegram operators Including switchboard (PBX) operators",
                                   `39` = "Clerical and related workers not elsewhere classified",
                                   `40` = "Managers (wholesale & retail trade)",
                                   `41` = "Working proprietors (wholesale and retail trade)",
                                   `42` = "Sales supervisors and buyers",
                                   `43` = "Technical salesmen, commercial travellers, manufactures agency",
                                   `44` = "Auctioneers and salesmen of insurance, real estate, securities, and business services",
                                   `45` = "Salesmen and shop assistants, and related workers",
                                   `49` = "Sales workers not elsewhere classified",
                                   `50` = "Managers (catering &lodging services)",
                                   `51` = "Working proprietors (catering & lodging services)",
                                   `52` = "Housekeeping and related service supervisors (Excluding housewives)",
                                   `53` = "Cooks, waiters, bartenders and related workers",
                                   `54` = "Maids and related housekeeping service workers not elsewhere classified, house girls, houseboys, garden boys",
                                   `55` = "Buildings caretakers, watch guards, char workers, cleaners and related workers.",
                                   `56` = "Launderers, dry-cleaners and pressers",
                                   `57` = "Hairdressers, barbers, beauticians and related workers",
                                   `58` = "Protective service workers",
                                   `59` = "Service workers not elsewhere classified",
                                   `60` = "Farm managers and supervisors",
                                   `61` = "Farmers (general farm owner/operators and specialised farmers)",
                                   `62` = "Agricultural and animal husbandry workers",
                                   `63` = "Forestry workers",
                                   `64` = "Fishermen, hunters and related workers",
                                   `70` = "General foreman and production supervisors",
                                   `71` = "Miners, Quarrymen, well drillers",
                                   `72` = "Metal processors",
                                   `73` = "Wood preparation and workers and paper makers",
                                   `74` = "Chemical processors and related workers",
                                   `75` = "Spinners, weavers, dyers, fibre preparers",
                                   `76` = "Tanners, skin preparers and pelt dressers",
                                   `77` = "Food and beverage processors",
                                   `78` = "Tobacco preparers and product makers",
                                   `79` = "Tailors, dressmakers, sewers, upholsters",
                                   `80` = "Shoemakers and leather goods makers",
                                   `81` = "Cabinet makers and related wood workers",
                                   `82` = "Stone cutters and carvers",
                                   `83` = "Blacksmith, toolmakers & machine tool operators",
                                   `84` = "Machinery fitters, machine assemblers",
                                   `85` = "Electrical fitters and related electrical workers",
                                   `86` = "Broadcasting station operators and cinema projectionists",
                                   `87` = "Plumbers, welders, sheet metal workers",
                                   `88` = "Jewellery and precious metal workers",
                                   `89` = "Potters, glass formers and related workers",
                                   `90` = "Rubber and plastic product makers",
                                   `91` = "Paper and paper-board product makers",
                                   `92` = "Printers and related workers.",
                                   `93` = "Painters",
                                   `94` = "Production and related workers",
                                   `95` = "Bricklayers, carpenters and other bricklayers",
                                   `96` = "Operators of stationery engines and power generating machines",
                                   `97` = "Material handling and related equipment operators",
                                   `98` = "Transport equipment operators",
                                   `99` = "Labourers not elsewhere classified."
                                   ))

# Generate job class using group index of activity labels
curr_df <- curr_df %>%
  mutate(job_class = as.integer(factor(hh_e19a_activity)))

# Rename hh_e21 to employer
curr_df <- curr_df %>%
  rename(employer = hh_e21)

# Hourly wage conversion based on payment frequency
curr_df <- curr_df %>%
  mutate(wage = case_when(
    hh_e26b == 3 ~ hh_e25,
    hh_e26b == 4 ~ hh_e25 / 7,
    hh_e26b == 5 ~ hh_e25 / 30.5,
    TRUE ~ NA
  ))


# Winsorize function wrapper
winsor <- function(x, probs = c(0.05, 0.95)) {
  DescTools::Winsorize(x, val = quantile(x, probs = probs, na.rm = TRUE))
}

# Apply winsorization to the wage-related variables
curr_df <- curr_df %>%
  mutate(across(c(hh_e22, hh_e23, hh_e24_1, hh_e24), winsor)) %>%
  mutate(
    hh_e24_1 = ifelse(hh_e24_1 == 0 & hh_e24 > 0, hh_e24, hh_e24_1)
  ) %>%
  mutate(
    h_return = wage * 7 / hh_e24_1,
    h_return = winsor(h_return)
  ) %>% 
  mutate(d_hours = round(hh_e24 / 7, 1),
         a_hours = hh_e24 * hh_e23 * hh_e22,
         a_return = h_return * hh_e24 * hh_e23 * hh_e22)

# Gratuity calculations
curr_df <- curr_df %>%
  mutate(
    hh_e27 = winsor(hh_e27)
  ) %>% 
  mutate(
    gratuity = case_when(
      hh_e28b == 3 ~ hh_e27,
      hh_e28b == 4 ~ hh_e27 / 7,
      hh_e28b == 5 ~ hh_e27 / 30.5,
      TRUE ~ NA
    )
  ) %>% 
  mutate(
    gratuity = winsor(gratuity)
  ) %>% 
  mutate(h_gratuity = gratuity * 7 / hh_e24) %>% 
  mutate(
         h_gratuity = ifelse(is.na(h_gratuity) & !is.na(h_return), 0, h_gratuity),
         a_gratuity = h_gratuity * hh_e24 * hh_e23 * hh_e22)

# Ganyu (casual labor) calculations
curr_df <- curr_df %>%
  mutate(
    hh_e59 = winsor(hh_e59),
    hh_e58 = ifelse(hh_e58 == 54, 7, hh_e58),
    a_ganyu = hh_e56 * hh_e57 * hh_e58 * hh_e59,
    d_ganyu = hh_e59,
    ganyu_d = hh_e56 * hh_e57 * hh_e58
  )

# Renaming to match Stata output
curr_df <- curr_df %>%
  rename(
    i22_class1 = job_class,
    i22_employer = employer,
    i22_return_h = h_return,
    i22_return_a = a_return,
    i22_hours_d = d_hours,
    i22_hours_a = a_hours,
    i22_return2_h = h_gratuity,
    i22_return2_a = a_gratuity,
    i23_return_a = a_ganyu,
    i23_return_d = d_ganyu,
    i23_days = ganyu_d,
    
    i_time = i_time0,
    i99_time = i_time05,
    i99_time2 = i_time06,
    i11_time = i_time07a,
    i12_time = i_time07b,
    i14_time = i_time07c,
    i21_time = i_time08,
    i21_time2 = i_time09,
    i23_time = i_time10,
    i22_time = i_time11,
    i23_time2 = i_time12,
    
    i99_times = i_timeshare05,
    i99_times2 = i_timeshare06,
    i11_times = i_timeshare07a,
    i12_times = i_timeshare07b,
    i14_times = i_timeshare07c,
    i21_times = i_timeshare08,
    i21_times2 = i_timeshare09,
    i23_times = i_timeshare10,
    i22_times = i_timeshare11,
    i23_times2 = i_timeshare12
  )


curr_df <- curr_df %>%
  select(HHID, PID, starts_with("i")) %>% 
  mutate(across(starts_with("i11"), ~ structure(.x, label = "Agriculture:"), .names = "{.col}")) %>% 
  mutate(across(starts_with("i12"), ~ structure(.x, label = "Livestock:"), .names = "{.col}")) %>% 
  mutate(across(starts_with("i14"), ~ structure(.x, label = "Fisheries:"), .names = "{.col}")) %>% 
  mutate(across(starts_with("i21"), ~ structure(.x, label = "HH Business:"), .names = "{.col}")) %>% 
  mutate(across(starts_with("i22"), ~ structure(.x, label = "Payed job:"), .names = "{.col}")) %>% 
  mutate(across(starts_with("i23"), ~ structure(.x, label = "Ganyu:"), .names = "{.col}")) %>% 
  mutate(across(starts_with("i99"), ~ structure(.x, label = "Other:"), .names = "{.col}"))


# Time spent (last week) for i*_time variables
for (h in c(11, 12, 14, 21, 22, 23, 99)) {
  time_var <- paste0("i", h, "_time")
  timeshare_var <- paste0("i", h, "_times")
  if (time_var %in% names(curr_df)) {
    curr_df <- curr_df %>%
      mutate(!!sym(time_var) := !!sym(time_var) %>% structure(label = paste0(labelled::var_label(!!sym(time_var)), " Time spent (last week)")))
  }
  if (timeshare_var %in% names(curr_df)) {
    curr_df <- curr_df %>%
      mutate(!!sym(timeshare_var) := !!sym(timeshare_var) %>% structure(label = paste0(labelled::var_label(!!sym(timeshare_var)), " Time spent (last week)")))
  }
}

# Time2 and Times2 variables (for 21, 23, 99 only)

for (h in c(21, 23, 99)) {
  time2_var <- paste0("i", h, "_time2")
  timeshare2_var <- paste0("i", h, "_times2")
  if (time2_var %in% names(curr_df)) {
    curr_df <- curr_df %>%
      mutate(!!sym(time2_var) := !!sym(time2_var) %>% structure(label = paste0(labelled::var_label(!!sym(time2_var)), " Time spent (last week)")))
  }
  if (timeshare2_var %in% names(data)) {
    curr_df <- curr_df %>%
      mutate(!!sym(timeshare2_var) := !!sym(timeshare2_var) %>% structure(label = paste0(labelled::var_label(!!sym(timeshare2_var)), " Time spent (share)")))
  }
}


# Manual labels for individual variables
var_label(curr_df$i11_profile)   <- "Agriculture: Profile"
var_label(curr_df$i22_class1)    <- "Paid job: Two digits OTI job classification"
var_label(curr_df$i22_employer)  <- "Paidjob: Emplyer"
var_label(curr_df$i22_return_h)  <- "Paid job: Hour return"
var_label(curr_df$i22_return_a)  <- "Paid job: Annual return"
var_label(curr_df$i22_hours_d)   <- "Paid job: Daily work hours"
var_label(curr_df$i22_hours_a)   <- "Paid job: Annual work hours"
var_label(curr_df$i22_return2_h) <- "Paid job: Hour Gratuity"
var_label(curr_df$i22_return2_a) <- "Paid job: Annual Gratuity"
var_label(curr_df$i23_return_a)  <- "Ganyu: Annual return"
var_label(curr_df$i23_return_d)  <- "Ganyu: Daily return"
var_label(curr_df$i23_days)      <- "Ganyu: work days per year"
var_label(curr_df$i_profile)     <- "Profile"
var_label(curr_df$i_time)        <- "Total time use (Last week)"

# Ensure i22_class1 is numeric
curr_df <- curr_df %>%
  mutate(i22_class1 = as.numeric(i22_class1),
         i22_return_a = ifelse(is.na(i22_return_a), 0, i22_return_a))

# Apply payroll tax logic
curr_df <- curr_df %>%
  mutate(
    p_tax = case_when(
      i22_return_a <= 1800000 ~ 0,
      i22_return_a > 1800000 & i22_return_a <= 6000000 ~ i22_return_a * 0.25,
      i22_return_a > 6000000 & i22_return_a <= 30600000 ~ i22_return_a * 0.30,
      i22_return_a > 30600000 ~ i22_return_a * 0.35,
      TRUE ~ NA
    ),
    i_tax = case_when(
      i22_return_a <= 1800000 ~ 0,
      i22_return_a > 1800000 & i22_return_a <= 6000000 ~ 25,
      i22_return_a > 6000000 & i22_return_a <= 30600000 ~ 30,
      i22_return_a > 30600000 ~ 35
    )
  ) %>% 
  mutate(    
    p_tax_m = p_tax / 12,
    p_tax = ifelse(is.na(p_tax), 0, p_tax),
    pp_tax = p_tax / i22_return_a,
    pp2_tax = ifelse(p_tax > 0, p_tax / i22_return_a, NA),
    i_ptax = ifelse(p_tax > 0, 1, 0))

# Label new variables
var_label(curr_df$p_tax) <- "Payroll tax"
var_label(curr_df$pp_tax) <- "Percent Payroll Tax"

# Rename ID variables to match merge key (optional)
curr_df <- curr_df %>%
  rename(hhid = HHID, pid = PID)

# Adding weight data
curr_df <- curr_df %>%
  left_join(yd, by=c("hhid"="hhid", "pid"="pid"))


# Summary statistics
# Total payroll tax (weighted)
total_p_tax <- weighted.mean(curr_df$p_tax, round(curr_df$weight, 0), na.rm = TRUE)
mean_p_tax <- weighted.mean(curr_df$p_tax[curr_df$p_tax > 0], round(curr_df$weight[curr_df$p_tax > 0], 0), na.rm = TRUE)

# Frequency of PAYE payers
table_ptax <- table(curr_df$i_ptax, round(curr_df$weight))
table_ptax_pid1 <- table(curr_df$i_ptax[curr_df$pid == 1], round(curr_df$weight[curr_df$pid == 1]))

# Print summary
cat("Total payroll tax (weighted mean):", total_p_tax, "\n")
cat("Mean payroll tax among taxpayers (weighted):", mean_p_tax, "\n")


# Create decile for yd_pc using weights, only for head (pid == 1)
curr_df <- curr_df %>%
  mutate(
    decile = ifelse(pid == 1,
                     ntile(yd_pc, 10),
                     NA)
  )

# Fill in missing deciles for other members of same household
curr_df <- curr_df %>%
  group_by(hhid) %>%
  fill(decile, .direction = "downup") %>%
  ungroup()
