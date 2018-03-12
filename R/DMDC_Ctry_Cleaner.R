DMDC_Standardizer <- function(data,ctrycol,branch){
  data[,ctrycol][data[,ctrycol]==""] <- ""
  #########################################
  
  #Merge Afghanistan
  data[,ctrycol][data[,ctrycol]=="Afghanistan (In/Around Not Available)"] <- "Afghanistan"
  data[,ctrycol][data[,ctrycol]=="Afghanistan (Not Available)"] <- "Afghanistan"
  
  #Merge American Samoa
  data[,ctrycol][data[,ctrycol]=="Samoan Islands"] <- "American Samoa"
  data[,ctrycol][data[,ctrycol]=="Samoa (American)"] <- "American Samoa"
  
  #Merge Antarctica
  data[,ctrycol][data[,ctrycol]=="Antarctic Region"] <- "Antarctica"
  data[,ctrycol][data[,ctrycol]=="Project Deep Freeze (Antarctica)"] <- "Antarctica"
  data[,ctrycol][data[,ctrycol]=="Project Deep Freeze (Antartica)"] <- "Antarctica"
  
  #Merge Bahrain
  data[,ctrycol][data[,ctrycol]=="Bahrein"] <- "Bahrain"
  data[,ctrycol][data[,ctrycol]=="Bahrein Island"] <- "Bahrain"
  
  #Merge Belgium
  data[,ctrycol][data[,ctrycol]=="Belgium*"] <- "Belgium"
  
  #Merge Burkina Faso
  data[,ctrycol][data[,ctrycol]=="Burkina"] <- "Burkina Faso"
  
  #Merge British Caribbean Countries
  data[,ctrycol][data[,ctrycol]=="Trinidad Bwi"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="British West Indies (Edata[,ctrycol]cluding Jamaica & Trinidad)"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Trinidad And Tobago"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Jamaica"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Grenada"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Barbados"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Bahamas, The"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Antigua And Barbuda"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Winward Islands (St. Lucia)"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="St. Christopher-Nevis-Anguilla"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="British Virgin Islands"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Virgin Islands, British"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Turks And Caicos Islands"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="British West Indies"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="British West Indies Federation"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Antigua"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Leeward Islands (Antigua)"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Leward Islands"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Leeward Islands"] <- "British Caribbean"
  data[,ctrycol][data[,ctrycol]=="Antigua"] <- "British Caribbean"
  
  #Merge Cameroon
  data[,ctrycol][data[,ctrycol]=="Cameroon, United Republic Of"] <- "Cameroon"
  data[,ctrycol][data[,ctrycol]=="Cameroun"] <- "Cameroon"
  
  #Merge Canada
  data[,ctrycol][data[,ctrycol]=="Canada (Including Newfoundland & Labrador)"] <- "Canada"
  data[,ctrycol][data[,ctrycol]=="Canada (Including Newfoundland)"] <- "Canada"
  data[,ctrycol][data[,ctrycol]=="Newfoundland (& Canada)"] <- "Canada"
  
  #Merge Caroline Islands
  data[,ctrycol][data[,ctrycol]=="Caroline Islands (Truk, Palau)"] <- "Caroline Islands"
  
  #Merge China
  data[,ctrycol][data[,ctrycol]=="Hong Kong"] <- "China"
  data[,ctrycol][data[,ctrycol]=="Taiwan"] <- "China"
  data[,ctrycol][data[,ctrycol]=="China (Includes Hong Kong)"] <- "China"
  data[,ctrycol][data[,ctrycol]=="China, Republic Of (Taiwan)"] <- "China"
  data[,ctrycol][data[,ctrycol]=="Hong Kong (& China)"] <- "China"
  
  #Merge Colombia
  data[,ctrycol][data[,ctrycol]=="Columbia"] <- "Colombia"
  
  #Merge Congo
  data[,ctrycol][data[,ctrycol]=="Congo (Brazzaville)"] <- "Congo"
  data[,ctrycol][data[,ctrycol]=="Congo(Brazzaville)"] <- "Congo"
  data[,ctrycol][data[,ctrycol]=="Congo (Kinshasa)"] <- "Congo"
  data[,ctrycol][data[,ctrycol]=="Congo(Kinshasa)"] <- "Congo"
  data[,ctrycol][data[,ctrycol]=="Congo (Leopoldville)"] <- "Congo"
  data[,ctrycol][data[,ctrycol]=="Zaire"] <- "Congo"
  data[,ctrycol][data[,ctrycol]=="Zaire (Congo)"] <- "Congo"
  
  #Merge Costa Rica
  data[,ctrycol][data[,ctrycol]=="Costa Rico"] <- "Costa Rica"
  
  #Merge Cote D'Ivoire
  data[,ctrycol][data[,ctrycol]=="Cote D'ivoire"] <- "Cote Divoire"
  data[,ctrycol][data[,ctrycol]=="Ivory Coast"] <- "Cote Divoire"
  
  #Merge Cuba
  data[,ctrycol][data[,ctrycol]=="Cuba (Guantanamo)"] <- "Cuba"
  data[,ctrycol][data[,ctrycol]=="Guantanamo Naval Base (Cuba)"] <- "Cuba"
  
  #Merge Czech Republic
  data[,ctrycol][data[,ctrycol]=="Czechoslovakia"] <- "Czech Republic"
  data[,ctrycol][data[,ctrycol]=="Czech Republic*"] <- "Czech Republic"
  
  #Merge Denmark
  data[,ctrycol][data[,ctrycol]=="Denmark*"] <- "Denmark"
  
  #Merge Egypt
  data[,ctrycol][data[,ctrycol]=="United Arab Republic (Egypt)"] <- "Egypt"
  data[,ctrycol][data[,ctrycol]=="United Arab Rep. (Egypt)"] <- "Egypt"
  
  #Merge Ethiopia
  data[,ctrycol][data[,ctrycol]=="Ethiopia (Inc. Eritrea)"] <- "Ethiopia"
  data[,ctrycol][data[,ctrycol]=="Ethiopia (Incl Eritrea)"] <- "Ethiopia"
  data[,ctrycol][data[,ctrycol]=="Ethiopia And Eritrea"] <- "Ethiopia"
  
  #Merge Fiji
  data[,ctrycol][data[,ctrycol]=="Fiji And Tonga"] <- "Fiji"
  
  #Merge France
  data[,ctrycol][data[,ctrycol]=="France (Interior & Atlantic)"] <- "France"
  data[,ctrycol][data[,ctrycol]=="France (Mediterranean)"] <- "France"
  data[,ctrycol][data[,ctrycol]=="France*"] <- "France"
  
  #Merge Germany
  data[,ctrycol][data[,ctrycol]=="German Democratic Republic"] <- "Germany"
  data[,ctrycol][data[,ctrycol]=="Germany (Inc. W. Berlin)"] <- "Germany"
  data[,ctrycol][data[,ctrycol]=="Germany *"] <- "Germany"
  data[,ctrycol][data[,ctrycol]=="Germany, Federal Republic Of"] <- "Germany"
  data[,ctrycol][data[,ctrycol]=="Germany*"] <- "Germany"
  data[,ctrycol][data[,ctrycol]=="Germany (Federal Republic & West Berlin)*"] <- "Germany"
  
  #Merge Gibraltar
  data[,ctrycol][data[,ctrycol]=="Gilbraltar"] <- "Gibraltar"
  data[,ctrycol][data[,ctrycol]=="Gibralter"] <- "Gibraltar"
  
  #Merge Greece
  data[,ctrycol][data[,ctrycol]=="Greece (& Crete)"] <- "Greece"
  data[,ctrycol][data[,ctrycol]=="Greece (Inc. Crete)"] <- "Greece"
  data[,ctrycol][data[,ctrycol]=="Greece (Incl Crete)"] <- "Greece"
  data[,ctrycol][data[,ctrycol]=="Greece (Including Crete)"] <- "Greece"
  data[,ctrycol][data[,ctrycol]=="Greece*"] <- "Greece"
  
  #Merge Greenland
  data[,ctrycol][data[,ctrycol]=="Greenland*"] <- "Greenland"
  
  #Merge Guam
  data[,ctrycol][data[,ctrycol]=="Mariana Islands (Guam)"] <- "Guam"
  data[,ctrycol][data[,ctrycol]=="Mariana Islands (Including Guam)"] <- "Guam"
  data[,ctrycol][data[,ctrycol]=="Northern Mariana Islands"] <- "Guam"
  
  #Merge Hungary
  data[,ctrycol][data[,ctrycol]=="Hungary*"] <- "Hungary"
  
  #Merge Iceland
  data[,ctrycol][data[,ctrycol]=="Iceland*"] <- "Iceland"
  
  #Merge Indonesia
  #The Democratic Republic of Timor-Leste came to be established in 2002, therefore prior data of troop strength in Timor was aggregated as part of Indonesia.
  data[,ctrycol][data[,ctrycol]=="Indonesia (Includes Timor)"] <- "Indonesia"
  
  #Merge Iraq
  data[,ctrycol][data[,ctrycol]=="Iraq (See Oif Table)"] <- "Iraq"
  
  #Merge Ireland
  data[,ctrycol][data[,ctrycol]=="Ireland (Eire)"] <- "Ireland"
  
  #Merge Israel
  data[,ctrycol][data[,ctrycol]=="Israel (Incl. Jerusalem)"] <- "Israel"
  data[,ctrycol][data[,ctrycol]=="Israel (Including Jerusalem)"] <- "Israel"
  data[,ctrycol][data[,ctrycol]=="Israel (Palestine)"] <- "Israel"
  data[,ctrycol][data[,ctrycol]=="Israel (Inc. Jerusalem)"] <- "Israel"
  data[,ctrycol][data[,ctrycol]=="Jerusalem"] <- "Israel"
  
  #Merge Italy
  data[,ctrycol][data[,ctrycol]=="Italy (& Sicily)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy (Inc. Sicily & Sardina)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy (Incl Siciliy & Sardinia)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy (Including Sardinia & Siciliy)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy (Including Sardinia & Sicily)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy (Including Sicily)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy (Incl Sicily & Sardinia)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy (Inc. Sicily & Sardinia)"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy*"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Italy *"] <- "Italy"
  data[,ctrycol][data[,ctrycol]=="Trieste"] <- "Italy"
  
  #Merge Japan
  data[,ctrycol][data[,ctrycol]=="Ryukyu Islands  E/"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Ryukyu Islands"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Ryukyus"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Ryukyus (Okinawa)"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Ryukyus(Okinawa)"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Volcano Islands (Iwo Jima)"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Iwo Jima (Volcano Islands)"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Volcano Islands (Including Iwo Jima)"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Japan (Incl Okinawa Prefecture)"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Japan (Incl Okinawa Prefecture) B/"] <- "Japan"
  data[,ctrycol][data[,ctrycol]=="Japan *"] <- "Japan"
  
  #Merge Korea, South ***NOTE THAT PERSONNEL IN N KOREA ADDS UP TO <30 (Assigning to Korea, South)**
  data[,ctrycol][data[,ctrycol]=="Korea"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="South Korea"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="South Korea D/"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="Korea, Republic Of"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="Korea, Democratic Peoples Republic Of"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="Korea, Republic Of *"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="Korea, North"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="South Korea D/"] <- "Korea, South"
  data[,ctrycol][data[,ctrycol]=="South Korea  D/"] <- "Korea, South"
  
  #Merge Kuwait
  data[,ctrycol][data[,ctrycol]=="Kuwait (See Oif Table)"] <- "Kuwait"
  
  #Merge Libya
  data[,ctrycol][data[,ctrycol]=="Libya (Tripoli)"] <- "Libya"
  
  #Merge Ludata[,ctrycol]embourg
  data[,ctrycol][data[,ctrycol]=="Ludata[,ctrycol]embourg*"] <- "Ludata[,ctrycol]embourg"
  data[,ctrycol][data[,ctrycol]=="Ludata[,ctrycol]embourgh"] <- "Ludata[,ctrycol]embourg"
  
  #Merge Macedonia
  data[,ctrycol][data[,ctrycol]=="Macedonia, The Former Yugoslav Republic Of"] <- "Macedonia"
  data[,ctrycol][data[,ctrycol]=="Macedonia, Former Yugoslav Republic Of"] <- "Macedonia"
  
  #Merge Madagascar (Questionable as DOD is reporting on Bassas Da India and Madagascar; however, they are on the same Island...but only in 2016 and only 7 MC personnel)
  data[,ctrycol][data[,ctrycol]=="Bassas Da India"] <- "Madagascar"
  data[,ctrycol][data[,ctrycol]=="Malagasy"] <- "Madagascar"
  data[,ctrycol][data[,ctrycol]=="Malagasy Republic (Madagascar)"] <- "Madagascar"
  data[,ctrycol][data[,ctrycol]=="Malagasy Republic"] <- "Madagascar"
  
  #Merge Midway
  data[,ctrycol][data[,ctrycol]=="Midway"] <- "Midway Islands"
  data[,ctrycol][data[,ctrycol]=="Midway Island"] <- "Midway Islands"
  
  #Merge Montenegro
  data[,ctrycol][data[,ctrycol]=="Montenegro (1992 - 2001)"] <- "Montenegro (2006 - 2008)"
  data[,ctrycol][data[,ctrycol]=="Montenegro"] <- "Montenegro (2006 - 2008)"
  
  #Merge Morocco
  data[,ctrycol][data[,ctrycol]=="Morocco (Including Tangier)"] <- "Morocco"
  data[,ctrycol][data[,ctrycol]=="Tangier"] <- "Morocco"
  data[,ctrycol][data[,ctrycol]=="French Morocco"] <- "Morocco"
  
  #Merge Netherlands
  data[,ctrycol][data[,ctrycol]=="Netherlands*"] <- "Netherlands"
  
  #Merge Netherlands Antilles (1991-2010)
  data[,ctrycol][data[,ctrycol]=="Netherlands Antilles"] <- "Netherlands Antilles (1991 - 2010)"
  data[,ctrycol][data[,ctrycol]=="Netherlands West Indies (Aruba)"] <- "Netherlands Antilles (1991 - 2010)"
  data[,ctrycol][data[,ctrycol]=="Aruba"] <- "Netherlands Antilles (1991 - 2010)"
  data[,ctrycol][data[,ctrycol]=="Netherlands West Indies"] <- "Netherlands Antilles (1991 - 2010)"
  
  #Merge Norway
  data[,ctrycol][data[,ctrycol]=="Norway*"] <- "Norway"
  
  #Merge Panama
  data[,ctrycol][data[,ctrycol]=="Panama"] <- "Panama (1980 - Present)"
  data[,ctrycol][data[,ctrycol]=="Panama (Republic Of)"] <- "Panama (1980 - Present)"
  data[,ctrycol][data[,ctrycol]=="Panama, Republic Of"] <- "Panama (1980 - Present)"
  data[,ctrycol][data[,ctrycol]=="Panama Canal Zone"] <- "Panama (1980 - Present)"
  data[,ctrycol][data[,ctrycol]=="Canal Zone"] <- "Panama (1980 - Present)"
  
  #Merge Poland
  data[,ctrycol][data[,ctrycol]=="Poland*"] <- "Poland"
  
  #Merge Portugal
  data[,ctrycol][data[,ctrycol]=="Portugal (Incl Azores)"] <- "Portugal"
  data[,ctrycol][data[,ctrycol]=="Portugal*"] <- "Portugal"
  data[,ctrycol][data[,ctrycol]=="Portugal (Includes Azores)"] <- "Portugal"
  data[,ctrycol][data[,ctrycol]=="Azores"] <- "Portugal"
  data[,ctrycol][data[,ctrycol]=="Azore Islands"] <- "Portugal"
  
  #Merge Romania
  data[,ctrycol][data[,ctrycol]=="Rumania"] <- "Romania"
  data[,ctrycol][data[,ctrycol]=="Roumania"] <- "Romania"
  
  #Merge Russia
  data[,ctrycol][data[,ctrycol]=="Ussr (Russia)"] <- "Russia"
  data[,ctrycol][data[,ctrycol]=="Soviet Union"] <- "Russia"
  data[,ctrycol][data[,ctrycol]=="Ussr (Soviet Union)"] <- "Russia"
  data[,ctrycol][data[,ctrycol]=="Union Of Soviet Socialist Republics"] <- "Russia"
  
  #Merge Serbia
  data[,ctrycol][data[,ctrycol]=="Serbia (1992 - 2004)"] <- "Serbia"
  data[,ctrycol][data[,ctrycol]=="Serbia (2006 - 2008)"] <- "Serbia"
  data[,ctrycol][data[,ctrycol]=="Serbia (2008 - Present)"] <- "Serbia"
  data[,ctrycol][data[,ctrycol]=="Serbia (Includes Kosovo)"] <- "Serbia"
  
  #Merge Seychelles Island
  data[,ctrycol][data[,ctrycol]=="Seychelles Island"] <- "Seychelles"
  data[,ctrycol][data[,ctrycol]=="Seychelles Islands"] <- "Seychelles"
  
  #Merge Spain
  data[,ctrycol][data[,ctrycol]=="Spain*"] <- "Spain"
  data[,ctrycol][data[,ctrycol]=="Spain (Atlantic)"] <- "Spain"
  data[,ctrycol][data[,ctrycol]=="Spain (Interior & Mediterranean)"] <- "Spain"
  data[,ctrycol][data[,ctrycol]=="Spain (Inc. Belearic Is.)"] <- "Spain"
  
  #Merge Somalia
  data[,ctrycol][data[,ctrycol]=="Somalia Republic"] <- "Somalia"
  data[,ctrycol][data[,ctrycol]=="Somali Republic"] <- "Somalia"
  data[,ctrycol][data[,ctrycol]=="French Somaliland"] <- "Somalia"
  
  #Merge South Africa
  data[,ctrycol][data[,ctrycol]=="South Africa, Union Of"] <- "South Africa"
  data[,ctrycol][data[,ctrycol]=="South Africa, Republic Of"] <- "South Africa"
  data[,ctrycol][data[,ctrycol]=="Union of South Africa"] <- "South Africa"
  
  #Merge St. Helena (Includes Ascension Island)
  data[,ctrycol][data[,ctrycol]=="St. Helena (Inc. Ascension)"] <- "St. Helena (Includes Ascension Island)"
  data[,ctrycol][data[,ctrycol]=="St. Helena (Incl. Ascension Is.)"] <- "St. Helena (Includes Ascension Island)"
  data[,ctrycol][data[,ctrycol]=="St Helena (Incl. Ascension Is.)"] <- "St. Helena (Includes Ascension Island)"
  data[,ctrycol][data[,ctrycol]=="St Helena (Includes Ascension Is.)"] <- "St. Helena (Includes Ascension Island)"
  data[,ctrycol][data[,ctrycol]=="Ascension Island"] <- "St. Helena (Includes Ascension Island)"
  
  #Merge Sri Lanka
  data[,ctrycol][data[,ctrycol]=="Sri Lanka (Ceylon)"] <- "Sri Lanka"
  
  #Merge Suriname
  data[,ctrycol][data[,ctrycol]=="Surinam (Netherlands Guiana)"] <- "Suriname" 
  
  #Merge Syria
  data[,ctrycol][data[,ctrycol]=="Syrian Arab Republic (Syria)"] <- "Syria"
  
  #Merge Tanzania
  data[,ctrycol][data[,ctrycol]=="Tanzania, United Republic Of"] <- "Tanzania"
  
  #Merge Thailand
  data[,ctrycol][data[,ctrycol]=="Thailand  D/"] <- "Thailand"
  data[,ctrycol][data[,ctrycol]=="Thailand  E/"] <- "Thailand"
  data[,ctrycol][data[,ctrycol]=="Thailand B/"] <- "Thailand"
  data[,ctrycol][data[,ctrycol]=="Thailand D/"] <- "Thailand"
  data[,ctrycol][data[,ctrycol]=="Thailand E/"] <- "Thailand"
  
  #Merge Turkey
  data[,ctrycol][data[,ctrycol]=="Turkey *"] <- "Turkey"
  data[,ctrycol][data[,ctrycol]=="Turkey*"] <- "Turkey"
  
  #Merge United Kingdom
  data[,ctrycol][data[,ctrycol]=="England (& Wales)"] <- "United Kingdom"
  data[,ctrycol][data[,ctrycol]=="United Kingdom *"] <- "United Kingdom"
  data[,ctrycol][data[,ctrycol]=="United Kingdom*"] <- "United Kingdom"
  
  #Merge Virgin Islands, U.s.
  data[,ctrycol][data[,ctrycol]=="Virgin Islands (U.s.)"] <- "Virgin Islands, U.s."
  data[,ctrycol][data[,ctrycol]=="U.S. Virgin Islands"] <- "Virgin Islands, U.s."
  data[,ctrycol][data[,ctrycol]=="U. S. Virgin Islands"] <- "Virgin Islands, U.s." 
  
  #Merge Vietnam
  data[,ctrycol][data[,ctrycol]=="Viet Nam"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="Viet-Nam"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="South Viet-Nam"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="South Viet-Nam E/"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="South Viet-Nam E/  F/"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="South Vietnam"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="South Vietnam  D/ E/"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="South Vietnam  E/ F/"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="South Vietnam  G/"] <- "Vietnam"
  data[,ctrycol][data[,ctrycol]=="Viet Nam"] <- "Vietnam"
  
  #Merge Yemen
  data[,ctrycol][data[,ctrycol]=="Yemen (Sanaa)"] <- "Yemen"
  
  data[,ctrycol] <- data[,ctrycol] %>% 
    str_to_title()
  data[,branch] <- data[,ctrycol] %>% 
    str_to_title()
  
  
  return(data)
}

cleaned_data <- DMDC_Country_Standardizer(data=Troop_Data,ctrycol=2)

