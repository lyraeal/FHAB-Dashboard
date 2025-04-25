
dat = read.csv("files/dashboard-dat.csv")

# landlord specific data
metadat = dat %>%
  select(
    Landlord_Group,
    Ranking,
    Aliases_of_Group,
    Total.Code.Violations.for.Landlord.Group,
    Total.Complaints.for.Landlord.Group,
    Total_Filings_by_Landlord_Group_.Numeric.,
    Total_Number_of_Filings_by_Landlord_Group_.Text.,
    Yearly_Filings_by_Landlord_Group_.Numeric.
  ) %>% unique() %>% 
  # ask about these, because they seem to have inconsistent results? 
  slice(c(-2, -10, -14))

colnames(metadat) = c("landlord", "ranking", "aliases", "code_violations", "complaints", "filings", "filingsdated", "yearlyfilings")

# this is directly from the FOIL data, but idk if this is the full dataset so i'll just leave it off
# metadat = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1AbBGWD1PXBRljakQmTwGKJ3MjBKJE2gXGKAAH_1gagk/edit?gid=711963840#gid=711963840", 
#                                     sheet = 11)
# 
# colnames(metadat) = colnames(metadat) %>% str_replace(., "[ \n]+", "")
# 
# metadat %>%
#   mutate(Landlord = Landlord %>% str_replace(., "[\n]+", " ")) %>% 
#   group_by(Landlord) %>%
#   summarise(violations = sum(TotalViolations),
#             complaints = n())

housingdat = dat %>% 
  select(
    -c(
    Ranking,
    Aliases_of_Group,
    Total.Code.Violations.for.Landlord.Group,
    Total.Complaints.for.Landlord.Group,
    Total_Filings_by_Landlord_Group_.Numeric.,
    Total_Number_of_Filings_by_Landlord_Group_.Text.,
    Yearly_Filings_by_Landlord_Group_.Numeric.)
  )

write.csv(housingdat, "files/housing-dat.csv")
write.csv(metadat, "files/landlord-dat.csv")





# to create the formatted addresses. 

# dat$actaddress = NA
# for (i in 1:nrow(dat)){
#   dat$actaddress[i] = paste0(
#     str_to_title(dat$Complaint_Address[i]),
#     ", ",
#     reverse_zipcode(dat$Parcel_Zip_Code)[which(reverse_zipcode(dat$Parcel_Zip_Code)[1] == dat$Parcel_Zip_Code[i]), 3][[1]],
#     ", NY ",
#     dat$Parcel_Zip_Code[i]
#   )
# }
# 



















































