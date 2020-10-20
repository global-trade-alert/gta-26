# setup

cutoff.date <- c("2020-10-23")     ##cutoff for GTA 26
cutoff.prior <- c("2019-11-15")     ## cut-off for GTA 25
cutoff.gta24 <- c("2019-04-15") ## cut-off for GTA 24
cutoff.gta23=c("2018-10-31")  ## cut-off for GTA 23
cutoff.gta22=c("2018-04-24")  ## cut-off for GTA 22

# break.date<-c("2016-12-31")   ## first day of populist-era
# manually.removed.interventions=c(18891,70350,16819,71578,58794,18254,13633,15366,19899,13512,14328,
#                                  18602,14104,17285,18601,19351,19347,15100,18638,57474,14017,20375,
#                                  57843,57619,62121,70692,72278,60042,13631,72137,18795,71645,13707,
#                                  19425,70751,15747,58726,18897,18649,72800,72384,69601, 70466)


# sectors <- c(28, 29, 43, 44, 46, 47, 49) # Sectors for sectoral chapters

pop.shade = "#FFE400" # Colour for populist era shading and text
pop.text = "#E5AE1C"

# g20.members=c("32", "36", "76", "124", "156", "251", "276", "699", "360", "381", "392", "484", "410", "643", "682", "710", "792", "826", "840")
# g20.member.names <-  c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "South Korea", "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", "United States of America")
# 
# g7.members <- c(124, 251, 276, 381, 392, 826, 840)
# g7.member.names <- c("Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States of America")
# 
# eu.members <- c(40, 56, 100, 191, 196, 203, 208, 233, 246, 251, 276, 300, 348, 372, 381, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752, 826)
# eu.member.names <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")
# 
# brics.members <- c(76, 643, 699, 156, 710)
# brics.member.names <- c("Brazil","Russia","India","China","South Africa")
# 
# au.members <- c(12, 24, 72, 108, 120, 132, 140, 148, 174, 178, 180, 204, 226, 231, 232, 262, 266, 270, 288, 324, 384, 404, 426, 430, 434, 450, 454, 466, 478, 480, 504, 508, 516, 562, 566, 624, 646, 678, 686, 690, 694, 706, 710, 716, 728, 729, 732, 748, 768, 788, 800, 818, 834, 854, 894)
# au.member.name <- c("Algeria", "Angola", "Botswana", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo", "DR Congo", "Benin", "Equatorial Guinea", "Ethiopia", "Eritrea", "Djibouti", "Gabon", "Gambia", "Ghana", "Guinea", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Guinea-Bissau", "Rwanda", "Sao Tome & Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "Zimbabwe", "South Sudan", "Republic of the Sudan", "Western Sahara", "Swaziland", "Togo", "Tunisia", "Uganda", "Egypt", "Tanzania", "Burkina Faso", "Zambia")
# 
# ldc.countries <- c(4, 24, 50, 64, 90, 104, 108, 116, 140, 148, 174, 180, 204, 226, 231, 232, 262, 270, 296, 324, 332, 418, 426, 430, 450, 454, 466, 478, 508, 524, 548, 562, 624, 626, 646, 678, 686, 694, 706, 728, 729, 768, 798, 800, 834, 854, 887, 894)
# ldc.country.names <- c("Afghanistan", "Angola", "Bangladesh", "Bhutan", "Solomon Islands", "Myanmar", "Burundi", "Cambodia", "Central African Republic", "Chad", "Comoros", "DR Congo", "Benin", "Equatorial Guinea", "Ethiopia", "Eritrea", "Djibouti", "Gambia", "Kiribati", "Guinea", "Haiti", "Lao", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Nepal", "Vanuatu", "Niger", "Guinea-Bissau", "Timor-Leste", "Rwanda", "Sao Tome & Principe", "Senegal", "Sierra Leone", "Somalia", "South Sudan", "Republic of the Sudan", "Togo", "Tuvalu", "Uganda", "Tanzania", "Burkina Faso", "Yemen", "Zambia")
# 
# oecd.members <- c(36, 40, 56, 124, 152, 203, 208, 233, 246, 251, 276, 300, 348, 352, 372, 376, 381, 392, 410, 428, 440, 442, 484, 528, 554, 578, 616, 620, 703, 705, 724, 752, 756, 792, 826, 840)
# oecd.member.names <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")
# 
# acp.members <- c(24, 28, 44, 52, 72, 84, 90, 108, 120, 132, 140, 148, 174, 178, 184, 192, 204, 212, 214, 226, 231, 232, 242, 262, 266, 270, 288, 296, 308, 324, 328, 332, 384, 388, 404, 426, 430, 450, 454, 466, 478, 480, 508, 516, 520, 548, 562, 566, 570, 583, 584, 585, 598, 624, 626, 646, 659, 662, 670, 678, 686, 690, 694, 706, 710, 716, 729, 740, 748, 768, 776, 780, 798, 800, 834, 854, 882, 894)
# acp.member.names <- c("Angola", "Antigua & Barbuda", "Bahamas", "Barbados", "Belize", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo", "Cook Islands", "Ivory Coast", "Cuba", "Djibouti", "Dominica", "Dominican Republic", "Timor-Leste", "Equatorial Guinea", "Eritrea", "Ethiopia", "Fiji", "Gabon", "Gambia", "Ghana", "Grenada", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Jamaica", "Kenya", "Kiribati", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Marshall Islands", "Mauritania", "Mauritius", "Micronesia", "Mozambique", "Namibia", "Nauru", "Niger", "Nigeria", "Niue", "Palau", "Papua New Guinea", "Rwanda", "Saint Kitts & Nevis", "Saint Lucia", "Saint Vincent & the Grenadines", "Samoa", "Sao Tome & Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa", "Republic of the Sudan", "Suriname", "Swaziland", "Tanzania", "Togo", "Tonga", "Trinidad & Tobago", "Tuvalu", "Uganda", "Vanuatu", "Zambia", "Zimbabwe")
# 
# wb.low.mid.ctry <- c(24, 50, 68, 64, 384, 120, 178, 174, 132, 262, 818, 583, 288, 340, 360, 699, 404, 417, 116, 296, 418, 426, 504, 498, 104, 496, 478, 566, 558, 586, 608, 598, 275, 729, 686, 90, 222, 678, 748, 626, 788, 804, 860, 704, 548, 894, 716)
# wb.upp.mid.ctry <- c(8, 32, 51, 16, 31, 100, 70, 112, 84, 76, 72, 156, 170, 188, 192, 212, 214, 12, 218, 242, 266, 268, 226, 308, 320, 328, 364, 368, 388, 400, 398, 422, 434, 662, 144, 462, 484, 584, 807, 499, 480, 458, 516, 520, 604, 600, 642, 643, 688, 740, 764, 795, 776, 792, 798, 670, 862, 882, 710)
# 
# trade.war.us <- c(56890, 56823, 63051, 57917, 62073)
# trade.war.chn <- c(63064, 62226, 62411)
# trade.war.intervention.ids <- c(56890, 56823, 63051, 63064, 57917, 62073, 62226, 62411, 61213, 71656, 71661, 71655, 71660)
# 
# china.us.trade.war.act.ids = c(27158,27214,30443,27906,20878,27906,31381,27906,31839,27906,35573,35573)
# china.us.trade.war.intervention.ids = c(63051)
# eu.sg.steel.act.ids = c(30461,30461,30461,36367,36794,36807)
# new.actions.intervention.ids = c(71656,71655,69341,71661,71660)
# 
# jumbo.threshold.1 = 10e9
# jumbo.threshold.2 = 100e9
# 
# ec.revoked.gsp.state.act.id = 30038 
# indian.2.3.exp.id = 70350

## FROM GTA23, maybe useful here:
## GTA trade coverage instrument groups

intervention.groups=rbind(data.frame(trade.flow="inward",
                                     group.name="Tariffs and trade defence",
                                     intervention.type=c("Import tariff",
                                                         "Import tariff quota",
                                                         "Anti-dumping",
                                                         "Anti-subsidy",
                                                         "Anti-circumvention",
                                                         "Safeguard",
                                                         "Import monitoring",
                                                         "Special safeguard")),
                          data.frame(trade.flow="inward",
                                     group.name="Import NTBs",
                                     intervention.type=c("Import licensing requirement",
                                                         "Import ban",
                                                         "Import quota",
                                                         "Import-related non-tariff measure, nes",
                                                         "Capital injection and equity stakes (including bailouts)",
                                                         "Consumption subsidy",
                                                         "Financial grant",
                                                         "In-kind grant",
                                                         "Interest payment subsidy",
                                                         "Internal taxation of imports",
                                                         "Loan guarantee",
                                                         "Local labour",
                                                         "Local operations",
                                                         "Local sourcing",
                                                         "Localisation incentive",
                                                         "Price stabilisation",
                                                         "Production subsidy",
                                                         "Public procurement access",
                                                         "Public procurement localisation",
                                                         "Public procurement preference margin",
                                                         "Public procurement, nes",
                                                         "State aid, nes",
                                                         "State loan",
                                                         "Tax or social insurance relief",
                                                         "Trade payment measure",
                                                         "Trade balancing measure")),
                          data.frame(trade.flow="outward",
                                     group.name="Restraints on exports",
                                     intervention.type=c("Export tax",
                                                         "Export tariff quota",
                                                         "Export ban",
                                                         "Export quota",
                                                         "Export licensing requirement",
                                                         "Export-related non-tariff measure, nes")),
                          data.frame(trade.flow="outward subsidy",
                                     group.name="Incentives",
                                     intervention.type=c("Export subsidy",
                                                         "Other export incentive",
                                                         "Tax-based export incentive",
                                                         "Trade finance"))
                          )


