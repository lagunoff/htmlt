module Assets where

import GHC.Generics
import Data.Text

data Country = Country
  { title :: Text
  , wiki_href :: Text
  , population :: Int
  , flag_icon :: Maybe Text
  , region :: Text
  , subregion :: Text
  } deriving Generic

countries :: [Country]
countries =
  [ Country "China" "https://en.wikipedia.org/wiki/China" 1433783686 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_the_People%27s_Republic_of_China.svg/23px-Flag_of_the_People%27s_Republic_of_China.svg.png") "Asia" "Eastern Asia"
  , Country "India" "https://en.wikipedia.org/wiki/India" 1366417754 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/4/41/Flag_of_India.svg/23px-Flag_of_India.svg.png") "Asia" "Southern Asia"
  , Country "United States" "https://en.wikipedia.org/wiki/United_States" 329064917 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/a/a4/Flag_of_the_United_States.svg/23px-Flag_of_the_United_States.svg.png") "Americas" "Northern America"
  , Country "Indonesia" "https://en.wikipedia.org/wiki/Indonesia" 270625568 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Flag_of_Indonesia.svg/23px-Flag_of_Indonesia.svg.png") "Asia" "South-eastern Asia"
  , Country "Pakistan" "https://en.wikipedia.org/wiki/Pakistan" 216565318 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/Flag_of_Pakistan.svg/23px-Flag_of_Pakistan.svg.png") "Asia" "Southern Asia"
  , Country "Brazil" "https://en.wikipedia.org/wiki/Brazil" 211049527 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/0/05/Flag_of_Brazil.svg/22px-Flag_of_Brazil.svg.png") "Americas" "South America"
  , Country "Nigeria" "https://en.wikipedia.org/wiki/Nigeria" 200963599 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Flag_of_Nigeria.svg/23px-Flag_of_Nigeria.svg.png") "Africa" "Western Africa"
  , Country "Bangladesh" "https://en.wikipedia.org/wiki/Bangladesh" 163046161 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Flag_of_Bangladesh.svg/23px-Flag_of_Bangladesh.svg.png") "Asia" "Southern Asia"
  , Country "Russia" "https://en.wikipedia.org/wiki/Russia" 145872256 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/f/f3/Flag_of_Russia.svg/23px-Flag_of_Russia.svg.png") "Europe" "Eastern Europe"
  , Country "Mexico" "https://en.wikipedia.org/wiki/Mexico" 127575529 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Flag_of_Mexico.svg/23px-Flag_of_Mexico.svg.png") "Americas" "Central America"
  , Country "Japan" "https://en.wikipedia.org/wiki/Japan" 126860301 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/9/9e/Flag_of_Japan.svg/23px-Flag_of_Japan.svg.png") "Asia" "Eastern Asia"
  , Country "Ethiopia" "https://en.wikipedia.org/wiki/Ethiopia" 112078730 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/71/Flag_of_Ethiopia.svg/23px-Flag_of_Ethiopia.svg.png") "Africa" "Eastern Africa"
  , Country "Philippines" "https://en.wikipedia.org/wiki/Philippines" 108116615 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Flag_of_the_Philippines.svg/23px-Flag_of_the_Philippines.svg.png") "Asia" "South-eastern Asia"
  , Country "Egypt" "https://en.wikipedia.org/wiki/Egypt" 100388073 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Egypt.svg/23px-Flag_of_Egypt.svg.png") "Africa" "Northern Africa"
  , Country "Vietnam" "https://en.wikipedia.org/wiki/Vietnam" 96462106 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Flag_of_Vietnam.svg/23px-Flag_of_Vietnam.svg.png") "Asia" "South-eastern Asia"
  , Country "DR Congo" "https://en.wikipedia.org/wiki/Democratic_Republic_of_the_Congo" 86790567 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6f/Flag_of_the_Democratic_Republic_of_the_Congo.svg/20px-Flag_of_the_Democratic_Republic_of_the_Congo.svg.png") "Africa" "Middle Africa"
  , Country "Germany" "https://en.wikipedia.org/wiki/Germany" 83517045 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/23px-Flag_of_Germany.svg.png") "Europe" "Western Europe"
  , Country "Turkey" "https://en.wikipedia.org/wiki/Turkey" 83429615 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b4/Flag_of_Turkey.svg/23px-Flag_of_Turkey.svg.png") "Asia" "Western Asia"
  , Country "Iran" "https://en.wikipedia.org/wiki/Iran" 82913906 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/Flag_of_Iran.svg/23px-Flag_of_Iran.svg.png") "Asia" "Southern Asia"
  , Country "Thailand" "https://en.wikipedia.org/wiki/Thailand" 69037513 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a9/Flag_of_Thailand.svg/23px-Flag_of_Thailand.svg.png") "Asia" "South-eastern Asia"
  , Country "United Kingdom" "https://en.wikipedia.org/wiki/United_Kingdom" 67530172 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/a/ae/Flag_of_the_United_Kingdom.svg/23px-Flag_of_the_United_Kingdom.svg.png") "Europe" "Northern Europe"
  , Country "France" "https://en.wikipedia.org/wiki/France" 65129728 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/c/c3/Flag_of_France.svg/23px-Flag_of_France.svg.png") "Europe" "Western Europe"
  , Country "Italy" "https://en.wikipedia.org/wiki/Italy" 60550075 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/0/03/Flag_of_Italy.svg/23px-Flag_of_Italy.svg.png") "Europe" "Southern Europe"
  , Country "South Africa" "https://en.wikipedia.org/wiki/South_Africa" 58558270 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/a/af/Flag_of_South_Africa.svg/23px-Flag_of_South_Africa.svg.png") "Africa" "Southern Africa"
  , Country "Tanzania" "https://en.wikipedia.org/wiki/Tanzania" 58005463 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Flag_of_Tanzania.svg/23px-Flag_of_Tanzania.svg.png") "Africa" "Eastern Africa"
  , Country "Myanmar" "https://en.wikipedia.org/wiki/Myanmar" 54045420 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/Flag_of_Myanmar.svg/23px-Flag_of_Myanmar.svg.png") "Asia" "South-eastern Asia"
  , Country "Kenya" "https://en.wikipedia.org/wiki/Kenya" 52573973 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Flag_of_Kenya.svg/23px-Flag_of_Kenya.svg.png") "Africa" "Eastern Africa"
  , Country "South Korea" "https://en.wikipedia.org/wiki/South_Korea" 51225308 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Flag_of_South_Korea.svg/23px-Flag_of_South_Korea.svg.png") "Asia" "Eastern Asia"
  , Country "Colombia" "https://en.wikipedia.org/wiki/Colombia" 50339443 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Flag_of_Colombia.svg/23px-Flag_of_Colombia.svg.png") "Americas" "South America"
  , Country "Spain" "https://en.wikipedia.org/wiki/Spain" 46736776 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/9/9a/Flag_of_Spain.svg/23px-Flag_of_Spain.svg.png") "Europe" "Southern Europe"
  , Country "Argentina" "https://en.wikipedia.org/wiki/Argentina" 44780677 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Flag_of_Argentina.svg/23px-Flag_of_Argentina.svg.png") "Americas" "South America"
  , Country "Uganda" "https://en.wikipedia.org/wiki/Uganda" 44269594 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4e/Flag_of_Uganda.svg/23px-Flag_of_Uganda.svg.png") "Africa" "Eastern Africa"
  , Country "Ukraine" "https://en.wikipedia.org/wiki/Ukraine" 43993638 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Flag_of_Ukraine.svg/23px-Flag_of_Ukraine.svg.png") "Europe" "Eastern Europe"
  , Country "Algeria" "https://en.wikipedia.org/wiki/Algeria" 43053054 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Flag_of_Algeria.svg/23px-Flag_of_Algeria.svg.png") "Africa" "Northern Africa"
  , Country "Sudan" "https://en.wikipedia.org/wiki/Sudan" 42813238 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/01/Flag_of_Sudan.svg/23px-Flag_of_Sudan.svg.png") "Africa" "Northern Africa"
  , Country "Iraq" "https://en.wikipedia.org/wiki/Iraq" 39309783 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f6/Flag_of_Iraq.svg/23px-Flag_of_Iraq.svg.png") "Asia" "Western Asia"
  , Country "Afghanistan" "https://en.wikipedia.org/wiki/Afghanistan" 38041754 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_the_Taliban.svg/23px-Flag_of_the_Taliban.svg.png") "Asia" "Southern Asia"
  , Country "Poland" "https://en.wikipedia.org/wiki/Poland" 37887768 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/1/12/Flag_of_Poland.svg/23px-Flag_of_Poland.svg.png") "Europe" "Eastern Europe"
  , Country "Canada" "https://en.wikipedia.org/wiki/Canada" 37411047 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Canada_%28Pantone%29.svg/23px-Flag_of_Canada_%28Pantone%29.svg.png") "Americas" "Northern America"
  , Country "Morocco" "https://en.wikipedia.org/wiki/Morocco" 36471769 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2c/Flag_of_Morocco.svg/23px-Flag_of_Morocco.svg.png") "Africa" "Northern Africa"
  , Country "Saudi Arabia" "https://en.wikipedia.org/wiki/Saudi_Arabia" 34268528 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0d/Flag_of_Saudi_Arabia.svg/23px-Flag_of_Saudi_Arabia.svg.png") "Asia" "Western Asia"
  , Country "Uzbekistan" "https://en.wikipedia.org/wiki/Uzbekistan" 32981716 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Flag_of_Uzbekistan.svg/23px-Flag_of_Uzbekistan.svg.png") "Asia" "Central Asia"
  , Country "Peru" "https://en.wikipedia.org/wiki/Peru" 32510453 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cf/Flag_of_Peru.svg/23px-Flag_of_Peru.svg.png") "Americas" "South America"
  , Country "Malaysia" "https://en.wikipedia.org/wiki/Malaysia" 31949777 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Flag_of_Malaysia.svg/23px-Flag_of_Malaysia.svg.png") "Asia" "South-eastern Asia"
  , Country "Angola" "https://en.wikipedia.org/wiki/Angola" 31825295 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/Flag_of_Angola.svg/23px-Flag_of_Angola.svg.png") "Africa" "Middle Africa"
  , Country "Mozambique" "https://en.wikipedia.org/wiki/Mozambique" 30366036 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Flag_of_Mozambique.svg/23px-Flag_of_Mozambique.svg.png") "Africa" "Eastern Africa"
  , Country "Yemen" "https://en.wikipedia.org/wiki/Yemen" 29161922 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/Flag_of_Yemen.svg/23px-Flag_of_Yemen.svg.png") "Asia" "Western Asia"
  , Country "Ghana" "https://en.wikipedia.org/wiki/Ghana" 28833629 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Ghana.svg/23px-Flag_of_Ghana.svg.png") "Africa" "Western Africa"
  , Country "Nepal" "https://en.wikipedia.org/wiki/Nepal" 28608710 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9b/Flag_of_Nepal.svg/16px-Flag_of_Nepal.svg.png") "Asia" "Southern Asia"
  , Country "Venezuela" "https://en.wikipedia.org/wiki/Venezuela" 28515829 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Flag_of_Venezuela.svg/23px-Flag_of_Venezuela.svg.png") "Americas" "South America"
  , Country "Madagascar" "https://en.wikipedia.org/wiki/Madagascar" 26969307 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Madagascar.svg/23px-Flag_of_Madagascar.svg.png") "Africa" "Eastern Africa"
  , Country "North Korea" "https://en.wikipedia.org/wiki/North_Korea" 25666161 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/51/Flag_of_North_Korea.svg/23px-Flag_of_North_Korea.svg.png") "Asia" "Eastern Asia"
  , Country "Ivory Coast" "https://en.wikipedia.org/wiki/Ivory_Coast" 25716544 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_C%C3%B4te_d%27Ivoire.svg/23px-Flag_of_C%C3%B4te_d%27Ivoire.svg.png") "Africa" "Western Africa"
  , Country "Cameroon" "https://en.wikipedia.org/wiki/Cameroon" 25876380 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4f/Flag_of_Cameroon.svg/23px-Flag_of_Cameroon.svg.png") "Africa" "Middle Africa"
  , Country "Australia" "https://en.wikipedia.org/wiki/Australia" 25203198 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Flag_of_Australia_%28converted%29.svg/23px-Flag_of_Australia_%28converted%29.svg.png") "Oceania" "Australia and New Zealand"
  , Country "Taiwan" "https://en.wikipedia.org/wiki/Taiwan" 23773876 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/72/Flag_of_the_Republic_of_China.svg/23px-Flag_of_the_Republic_of_China.svg.png") "Asia" "Eastern Asia"
  , Country "Niger" "https://en.wikipedia.org/wiki/Niger" 23310715 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Flag_of_Niger.svg/18px-Flag_of_Niger.svg.png") "Africa" "Western Africa"
  , Country "Sri Lanka" "https://en.wikipedia.org/wiki/Sri_Lanka" 21323733 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/11/Flag_of_Sri_Lanka.svg/23px-Flag_of_Sri_Lanka.svg.png") "Asia" "Southern Asia"
  , Country "Burkina Faso" "https://en.wikipedia.org/wiki/Burkina_Faso" 20321378 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Flag_of_Burkina_Faso.svg/23px-Flag_of_Burkina_Faso.svg.png") "Africa" "Western Africa"
  , Country "Mali" "https://en.wikipedia.org/wiki/Mali" 19658031 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_Mali.svg/23px-Flag_of_Mali.svg.png") "Africa" "Western Africa"
  , Country "Romania" "https://en.wikipedia.org/wiki/Romania" 19364557 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Flag_of_Romania.svg/23px-Flag_of_Romania.svg.png") "Europe" "Eastern Europe"
  , Country "Malawi" "https://en.wikipedia.org/wiki/Malawi" 18628747 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d1/Flag_of_Malawi.svg/23px-Flag_of_Malawi.svg.png") "Africa" "Eastern Africa"
  , Country "Chile" "https://en.wikipedia.org/wiki/Chile" 18952038 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/78/Flag_of_Chile.svg/23px-Flag_of_Chile.svg.png") "Americas" "South America"
  , Country "Kazakhstan" "https://en.wikipedia.org/wiki/Kazakhstan" 18551427 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/Flag_of_Kazakhstan.svg/23px-Flag_of_Kazakhstan.svg.png") "Asia" "Central Asia"
  , Country "Zambia" "https://en.wikipedia.org/wiki/Zambia" 17861030 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Flag_of_Zambia.svg/23px-Flag_of_Zambia.svg.png") "Africa" "Eastern Africa"
  , Country "Guatemala" "https://en.wikipedia.org/wiki/Guatemala" 17581472 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ec/Flag_of_Guatemala.svg/23px-Flag_of_Guatemala.svg.png") "Americas" "Central America"
  , Country "Ecuador" "https://en.wikipedia.org/wiki/Ecuador" 17373662 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e8/Flag_of_Ecuador.svg/23px-Flag_of_Ecuador.svg.png") "Americas" "South America"
  , Country "Netherlands" "https://en.wikipedia.org/wiki/Netherlands" 17097130 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/23px-Flag_of_the_Netherlands.svg.png") "Europe" "Western Europe"
  , Country "Syria" "https://en.wikipedia.org/wiki/Syria" 17070135 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Flag_of_Syria.svg/23px-Flag_of_Syria.svg.png") "Asia" "Western Asia"
  , Country "Cambodia" "https://en.wikipedia.org/wiki/Cambodia" 16486542 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Flag_of_Cambodia.svg/23px-Flag_of_Cambodia.svg.png") "Asia" "South-eastern Asia"
  , Country "Senegal" "https://en.wikipedia.org/wiki/Senegal" 16296364 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fd/Flag_of_Senegal.svg/23px-Flag_of_Senegal.svg.png") "Africa" "Western Africa"
  , Country "Chad" "https://en.wikipedia.org/wiki/Chad" 15946876 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Flag_of_Chad.svg/23px-Flag_of_Chad.svg.png") "Africa" "Middle Africa"
  , Country "Somalia" "https://en.wikipedia.org/wiki/Somalia" 15442905 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/Flag_of_Somalia.svg/23px-Flag_of_Somalia.svg.png") "Africa" "Eastern Africa"
  , Country "Zimbabwe" "https://en.wikipedia.org/wiki/Zimbabwe" 14645468 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6a/Flag_of_Zimbabwe.svg/23px-Flag_of_Zimbabwe.svg.png") "Africa" "Eastern Africa"
  , Country "Guinea" "https://en.wikipedia.org/wiki/Guinea" 12771246 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ed/Flag_of_Guinea.svg/23px-Flag_of_Guinea.svg.png") "Africa" "Western Africa"
  , Country "Rwanda" "https://en.wikipedia.org/wiki/Rwanda" 12626950 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/17/Flag_of_Rwanda.svg/23px-Flag_of_Rwanda.svg.png") "Africa" "Eastern Africa"
  , Country "Benin" "https://en.wikipedia.org/wiki/Benin" 11801151 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0a/Flag_of_Benin.svg/23px-Flag_of_Benin.svg.png") "Africa" "Western Africa"
  , Country "Tunisia" "https://en.wikipedia.org/wiki/Tunisia" 11694719 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Flag_of_Tunisia.svg/23px-Flag_of_Tunisia.svg.png") "Africa" "Northern Africa"
  , Country "Belgium" "https://en.wikipedia.org/wiki/Belgium" 11539328 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_Belgium_%28civil%29.svg/23px-Flag_of_Belgium_%28civil%29.svg.png") "Europe" "Western Europe"
  , Country "Bolivia" "https://en.wikipedia.org/wiki/Bolivia" 11513100 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/Bandera_de_Bolivia_%28Estado%29.svg/22px-Bandera_de_Bolivia_%28Estado%29.svg.png") "Americas" "South America"
  , Country "Cuba" "https://en.wikipedia.org/wiki/Cuba" 11333483 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Flag_of_Cuba.svg/23px-Flag_of_Cuba.svg.png") "Americas" "Caribbean"
  , Country "Haiti" "https://en.wikipedia.org/wiki/Haiti" 11263770 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Flag_of_Haiti.svg/23px-Flag_of_Haiti.svg.png") "Americas" "Caribbean"
  , Country "South Sudan" "https://en.wikipedia.org/wiki/South_Sudan" 11062113 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7a/Flag_of_South_Sudan.svg/23px-Flag_of_South_Sudan.svg.png") "Africa" "Eastern Africa"
  , Country "Burundi" "https://en.wikipedia.org/wiki/Burundi" 10864245 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Flag_of_Burundi.svg/23px-Flag_of_Burundi.svg.png") "Africa" "Eastern Africa"
  , Country "Dominican Republic" "https://en.wikipedia.org/wiki/Dominican_Republic" 10738958 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Flag_of_the_Dominican_Republic.svg/23px-Flag_of_the_Dominican_Republic.svg.png") "Americas" "Caribbean"
  , Country "Czech Republic" "https://en.wikipedia.org/wiki/Czech_Republic" 10689209 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Flag_of_the_Czech_Republic.svg/23px-Flag_of_the_Czech_Republic.svg.png") "Europe" "Eastern Europe"
  , Country "Greece" "https://en.wikipedia.org/wiki/Greece" 10473455 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_Greece.svg/23px-Flag_of_Greece.svg.png") "Europe" "Southern Europe"
  , Country "Portugal" "https://en.wikipedia.org/wiki/Portugal" 10226187 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_Portugal.svg/23px-Flag_of_Portugal.svg.png") "Europe" "Southern Europe"
  , Country "Jordan" "https://en.wikipedia.org/wiki/Jordan" 10101694 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c0/Flag_of_Jordan.svg/23px-Flag_of_Jordan.svg.png") "Asia" "Western Asia"
  , Country "Azerbaijan" "https://en.wikipedia.org/wiki/Azerbaijan" 10047718 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Flag_of_Azerbaijan.svg/23px-Flag_of_Azerbaijan.svg.png") "Asia" "Western Asia"
  , Country "Sweden" "https://en.wikipedia.org/wiki/Sweden" 10036379 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/4/4c/Flag_of_Sweden.svg/23px-Flag_of_Sweden.svg.png") "Europe" "Northern Europe"
  , Country "United Arab Emirates" "https://en.wikipedia.org/wiki/United_Arab_Emirates" 9770529 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Flag_of_the_United_Arab_Emirates.svg/23px-Flag_of_the_United_Arab_Emirates.svg.png") "Asia" "Western Asia"
  , Country "Honduras" "https://en.wikipedia.org/wiki/Honduras" 9746117 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Flag_of_Honduras.svg/23px-Flag_of_Honduras.svg.png") "Americas" "Central America"
  , Country "Hungary" "https://en.wikipedia.org/wiki/Hungary" 9684679 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/Flag_of_Hungary.svg/23px-Flag_of_Hungary.svg.png") "Europe" "Eastern Europe"
  , Country "Belarus" "https://en.wikipedia.org/wiki/Belarus" 9452411 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Flag_of_Belarus.svg/23px-Flag_of_Belarus.svg.png") "Europe" "Eastern Europe"
  , Country "Tajikistan" "https://en.wikipedia.org/wiki/Tajikistan" 9321018 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Flag_of_Tajikistan.svg/23px-Flag_of_Tajikistan.svg.png") "Asia" "Central Asia"
  , Country "Austria" "https://en.wikipedia.org/wiki/Austria" 8955102 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Flag_of_Austria.svg/23px-Flag_of_Austria.svg.png") "Europe" "Western Europe"
  , Country "Papua New Guinea" "https://en.wikipedia.org/wiki/Papua_New_Guinea" 8776109 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/Flag_of_Papua_New_Guinea.svg/20px-Flag_of_Papua_New_Guinea.svg.png") "Oceania" "Melanesia"
  , Country "Serbia" "https://en.wikipedia.org/wiki/Serbia" 8772235 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/ff/Flag_of_Serbia.svg/23px-Flag_of_Serbia.svg.png") "Europe" "Southern Europe"
  , Country "Switzerland" "https://en.wikipedia.org/wiki/Switzerland" 8591365 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Flag_of_Switzerland.svg/16px-Flag_of_Switzerland.svg.png") "Europe" "Western Europe"
  , Country "Israel" "https://en.wikipedia.org/wiki/Israel" 8519377 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/Flag_of_Israel.svg/21px-Flag_of_Israel.svg.png") "Asia" "Western Asia"
  , Country "Togo" "https://en.wikipedia.org/wiki/Togo" 8082366 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/68/Flag_of_Togo.svg/23px-Flag_of_Togo.svg.png") "Africa" "Western Africa"
  , Country "Sierra Leone" "https://en.wikipedia.org/wiki/Sierra_Leone" 7813215 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/17/Flag_of_Sierra_Leone.svg/23px-Flag_of_Sierra_Leone.svg.png") "Africa" "Western Africa"
  , Country "Hong Kong" "https://en.wikipedia.org/wiki/Hong_Kong" 7436154 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5b/Flag_of_Hong_Kong.svg/23px-Flag_of_Hong_Kong.svg.png") "Asia" "Eastern Asia"
  , Country "Laos" "https://en.wikipedia.org/wiki/Laos" 7169455 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Flag_of_Laos.svg/23px-Flag_of_Laos.svg.png") "Asia" "South-eastern Asia"
  , Country "Paraguay" "https://en.wikipedia.org/wiki/Paraguay" 7044636 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/Flag_of_Paraguay.svg/23px-Flag_of_Paraguay.svg.png") "Americas" "South America"
  , Country "Bulgaria" "https://en.wikipedia.org/wiki/Bulgaria" 7000119 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Bulgaria.svg/23px-Flag_of_Bulgaria.svg.png") "Europe" "Eastern Europe"
  , Country "Lebanon" "https://en.wikipedia.org/wiki/Lebanon" 6855713 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/5/59/Flag_of_Lebanon.svg/23px-Flag_of_Lebanon.svg.png") "Asia" "Western Asia"
  , Country "Libya" "https://en.wikipedia.org/wiki/Libya" 6777452 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Flag_of_Libya.svg/23px-Flag_of_Libya.svg.png") "Africa" "Northern Africa"
  , Country "Nicaragua" "https://en.wikipedia.org/wiki/Nicaragua" 6545502 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Nicaragua.svg/23px-Flag_of_Nicaragua.svg.png") "Americas" "Central America"
  , Country "El Salvador" "https://en.wikipedia.org/wiki/El_Salvador" 6453553 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Flag_of_El_Salvador.svg/23px-Flag_of_El_Salvador.svg.png") "Americas" "Central America"
  , Country "Kyrgyzstan" "https://en.wikipedia.org/wiki/Kyrgyzstan" 6415850 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Flag_of_Kyrgyzstan.svg/23px-Flag_of_Kyrgyzstan.svg.png") "Asia" "Central Asia"
  , Country "Turkmenistan" "https://en.wikipedia.org/wiki/Turkmenistan" 5942089 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Flag_of_Turkmenistan.svg/23px-Flag_of_Turkmenistan.svg.png") "Asia" "Central Asia"
  , Country "Singapore" "https://en.wikipedia.org/wiki/Singapore" 5804337 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Flag_of_Singapore.svg/23px-Flag_of_Singapore.svg.png") "Asia" "South-eastern Asia"
  , Country "Denmark" "https://en.wikipedia.org/wiki/Denmark" 5771876 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/Flag_of_Denmark.svg/20px-Flag_of_Denmark.svg.png") "Europe" "Northern Europe"
  , Country "Finland" "https://en.wikipedia.org/wiki/Finland" 5532156 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Finland.svg/23px-Flag_of_Finland.svg.png") "Europe" "Northern Europe"
  , Country "Slovakia" "https://en.wikipedia.org/wiki/Slovakia" 5457013 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Flag_of_Slovakia.svg/23px-Flag_of_Slovakia.svg.png") "Europe" "Eastern Europe"
  , Country "Congo" "https://en.wikipedia.org/wiki/Republic_of_the_Congo" 5380508 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_the_Republic_of_the_Congo.svg/23px-Flag_of_the_Republic_of_the_Congo.svg.png") "Africa" "Middle Africa"
  , Country "Norway" "https://en.wikipedia.org/wiki/Norway" 5378857 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Norway.svg/21px-Flag_of_Norway.svg.png") "Europe" "Northern Europe"
  , Country "Costa Rica" "https://en.wikipedia.org/wiki/Costa_Rica" 5047561 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f2/Flag_of_Costa_Rica.svg/23px-Flag_of_Costa_Rica.svg.png") "Americas" "Central America"
  , Country "Palestine" "https://en.wikipedia.org/wiki/State_of_Palestine" 4981420 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Flag_of_Palestine.svg/23px-Flag_of_Palestine.svg.png") "Asia" "Western Asia"
  , Country "Oman" "https://en.wikipedia.org/wiki/Oman" 4974986 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Flag_of_Oman.svg/23px-Flag_of_Oman.svg.png") "Asia" "Western Asia"
  , Country "Liberia" "https://en.wikipedia.org/wiki/Liberia" 4937374 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b8/Flag_of_Liberia.svg/23px-Flag_of_Liberia.svg.png") "Africa" "Western Africa"
  , Country "Ireland" "https://en.wikipedia.org/wiki/Republic_of_Ireland" 4882495 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Flag_of_Ireland.svg/23px-Flag_of_Ireland.svg.png") "Europe" "Northern Europe"
  , Country "New Zealand" "https://en.wikipedia.org/wiki/New_Zealand" 4783063 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3e/Flag_of_New_Zealand.svg/23px-Flag_of_New_Zealand.svg.png") "Oceania" "Australia and New Zealand"
  , Country "Central African Republic" "https://en.wikipedia.org/wiki/Central_African_Republic" 4745185 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6f/Flag_of_the_Central_African_Republic.svg/23px-Flag_of_the_Central_African_Republic.svg.png") "Africa" "Middle Africa"
  , Country "Mauritania" "https://en.wikipedia.org/wiki/Mauritania" 4525696 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/43/Flag_of_Mauritania.svg/23px-Flag_of_Mauritania.svg.png") "Africa" "Western Africa"
  , Country "Panama" "https://en.wikipedia.org/wiki/Panama" 4246439 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Flag_of_Panama.svg/23px-Flag_of_Panama.svg.png") "Americas" "Central America"
  , Country "Kuwait" "https://en.wikipedia.org/wiki/Kuwait" 4207083 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/Flag_of_Kuwait.svg/23px-Flag_of_Kuwait.svg.png") "Asia" "Western Asia"
  , Country "Croatia" "https://en.wikipedia.org/wiki/Croatia" 4130304 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Flag_of_Croatia.svg/23px-Flag_of_Croatia.svg.png") "Europe" "Southern Europe"
  , Country "Moldova" "https://en.wikipedia.org/wiki/Moldova" 4043263 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/Flag_of_Moldova.svg/23px-Flag_of_Moldova.svg.png") "Europe" "Eastern Europe"
  , Country "Georgia" "https://en.wikipedia.org/wiki/Georgia_(country)" 3996765 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Flag_of_Georgia.svg/23px-Flag_of_Georgia.svg.png") "Asia" "Western Asia"
  , Country "Eritrea" "https://en.wikipedia.org/wiki/Eritrea" 3497117 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Flag_of_Eritrea.svg/23px-Flag_of_Eritrea.svg.png") "Africa" "Eastern Africa"
  , Country "Uruguay" "https://en.wikipedia.org/wiki/Uruguay" 3461734 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Uruguay.svg/23px-Flag_of_Uruguay.svg.png") "Americas" "South America"
  , Country "Bosnia and Herzegovina" "https://en.wikipedia.org/wiki/Bosnia_and_Herzegovina" 3301000 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Flag_of_Bosnia_and_Herzegovina.svg/23px-Flag_of_Bosnia_and_Herzegovina.svg.png") "Europe" "Southern Europe"
  , Country "Mongolia" "https://en.wikipedia.org/wiki/Mongolia" 3225167 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Flag_of_Mongolia.svg/23px-Flag_of_Mongolia.svg.png") "Asia" "Eastern Asia"
  , Country "Armenia" "https://en.wikipedia.org/wiki/Armenia" 2957731 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Flag_of_Armenia.svg/23px-Flag_of_Armenia.svg.png") "Asia" "Western Asia"
  , Country "Jamaica" "https://en.wikipedia.org/wiki/Jamaica" 2948279 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0a/Flag_of_Jamaica.svg/23px-Flag_of_Jamaica.svg.png") "Americas" "Caribbean"
  , Country "Puerto Rico" "https://en.wikipedia.org/wiki/Puerto_Rico" 2933408 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/28/Flag_of_Puerto_Rico.svg/23px-Flag_of_Puerto_Rico.svg.png") "Americas" "Caribbean"
  , Country "Albania" "https://en.wikipedia.org/wiki/Albania" 2880917 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/36/Flag_of_Albania.svg/21px-Flag_of_Albania.svg.png") "Europe" "Southern Europe"
  , Country "Qatar" "https://en.wikipedia.org/wiki/Qatar" 2832067 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/65/Flag_of_Qatar.svg/23px-Flag_of_Qatar.svg.png") "Asia" "Western Asia"
  , Country "Lithuania" "https://en.wikipedia.org/wiki/Lithuania" 2759627 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/11/Flag_of_Lithuania.svg/23px-Flag_of_Lithuania.svg.png") "Europe" "Northern Europe"
  , Country "Namibia" "https://en.wikipedia.org/wiki/Namibia" 2494530 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Flag_of_Namibia.svg/23px-Flag_of_Namibia.svg.png") "Africa" "Southern Africa"
  , Country "Gambia" "https://en.wikipedia.org/wiki/The_Gambia" 2347706 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Flag_of_The_Gambia.svg/23px-Flag_of_The_Gambia.svg.png") "Africa" "Western Africa"
  , Country "Botswana" "https://en.wikipedia.org/wiki/Botswana" 2303697 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_Botswana.svg/23px-Flag_of_Botswana.svg.png") "Africa" "Southern Africa"
  , Country "Gabon" "https://en.wikipedia.org/wiki/Gabon" 2172579 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Flag_of_Gabon.svg/20px-Flag_of_Gabon.svg.png") "Africa" "Middle Africa"
  , Country "Lesotho" "https://en.wikipedia.org/wiki/Lesotho" 2125268 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Flag_of_Lesotho.svg/23px-Flag_of_Lesotho.svg.png") "Africa" "Southern Africa"
  , Country "North Macedonia" "https://en.wikipedia.org/wiki/North_Macedonia" 2083459 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Flag_of_North_Macedonia.svg/23px-Flag_of_North_Macedonia.svg.png") "Europe" "Southern Europe"
  , Country "Slovenia" "https://en.wikipedia.org/wiki/Slovenia" 2078654 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f0/Flag_of_Slovenia.svg/23px-Flag_of_Slovenia.svg.png") "Europe" "Southern Europe"
  , Country "Guinea-Bissau" "https://en.wikipedia.org/wiki/Guinea-Bissau" 1920922 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/01/Flag_of_Guinea-Bissau.svg/23px-Flag_of_Guinea-Bissau.svg.png") "Africa" "Western Africa"
  , Country "Latvia" "https://en.wikipedia.org/wiki/Latvia" 1906743 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Flag_of_Latvia.svg/23px-Flag_of_Latvia.svg.png") "Europe" "Northern Europe"
  , Country "Bahrain" "https://en.wikipedia.org/wiki/Bahrain" 1641172 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2c/Flag_of_Bahrain.svg/23px-Flag_of_Bahrain.svg.png") "Asia" "Western Asia"
  , Country "Trinidad and Tobago" "https://en.wikipedia.org/wiki/Trinidad_and_Tobago" 1394973 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Flag_of_Trinidad_and_Tobago.svg/23px-Flag_of_Trinidad_and_Tobago.svg.png") "Americas" "Caribbean"
  , Country "Equatorial Guinea" "https://en.wikipedia.org/wiki/Equatorial_Guinea" 1355986 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Flag_of_Equatorial_Guinea.svg/23px-Flag_of_Equatorial_Guinea.svg.png") "Africa" "Middle Africa"
  , Country "Estonia" "https://en.wikipedia.org/wiki/Estonia" 1325648 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8f/Flag_of_Estonia.svg/23px-Flag_of_Estonia.svg.png") "Europe" "Northern Europe"
  , Country "East Timor" "https://en.wikipedia.org/wiki/East_Timor" 1293119 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Flag_of_East_Timor.svg/23px-Flag_of_East_Timor.svg.png") "Asia" "South-eastern Asia"
  , Country "Mauritius" "https://en.wikipedia.org/wiki/Mauritius" 1198575 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Flag_of_Mauritius.svg/23px-Flag_of_Mauritius.svg.png") "Africa" "Eastern Africa"
  , Country "Cyprus" "https://en.wikipedia.org/wiki/Cyprus" 1179551 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/Flag_of_Cyprus.svg/23px-Flag_of_Cyprus.svg.png") "Asia" "Western Asia"
  , Country "Eswatini" "https://en.wikipedia.org/wiki/Eswatini" 1148130 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fb/Flag_of_Eswatini.svg/23px-Flag_of_Eswatini.svg.png") "Africa" "Southern Africa"
  , Country "Djibouti" "https://en.wikipedia.org/wiki/Djibouti" 973560 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Flag_of_Djibouti.svg/23px-Flag_of_Djibouti.svg.png") "Africa" "Eastern Africa"
  , Country "Fiji" "https://en.wikipedia.org/wiki/Fiji" 889953 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Flag_of_Fiji.svg/23px-Flag_of_Fiji.svg.png") "Oceania" "Melanesia"
  , Country "Réunion" "https://en.wikipedia.org/wiki/R%C3%A9union" 888927 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8e/Proposed_flag_of_R%C3%A9union_%28VAR%29.svg/23px-Proposed_flag_of_R%C3%A9union_%28VAR%29.svg.png") "Africa" "Eastern Africa"
  , Country "Comoros" "https://en.wikipedia.org/wiki/Comoros" 850886 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/94/Flag_of_the_Comoros.svg/23px-Flag_of_the_Comoros.svg.png") "Africa" "Eastern Africa"
  , Country "Guyana" "https://en.wikipedia.org/wiki/Guyana" 782766 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Flag_of_Guyana.svg/23px-Flag_of_Guyana.svg.png") "Americas" "South America"
  , Country "Bhutan" "https://en.wikipedia.org/wiki/Bhutan" 763092 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Flag_of_Bhutan.svg/23px-Flag_of_Bhutan.svg.png") "Asia" "Southern Asia"
  , Country "Solomon Islands" "https://en.wikipedia.org/wiki/Solomon_Islands" 669823 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Flag_of_the_Solomon_Islands.svg/23px-Flag_of_the_Solomon_Islands.svg.png") "Oceania" "Melanesia"
  , Country "Macau" "https://en.wikipedia.org/wiki/Macau" 640445 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/63/Flag_of_Macau.svg/23px-Flag_of_Macau.svg.png") "Asia" "Eastern Asia"
  , Country "Montenegro" "https://en.wikipedia.org/wiki/Montenegro" 627987 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Flag_of_Montenegro.svg/23px-Flag_of_Montenegro.svg.png") "Europe" "Southern Europe"
  , Country "Luxembourg" "https://en.wikipedia.org/wiki/Luxembourg" 615729 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Flag_of_Luxembourg.svg/23px-Flag_of_Luxembourg.svg.png") "Europe" "Western Europe"
  , Country "Western Sahara" "https://en.wikipedia.org/wiki/Western_Sahara" 582463 Nothing "Africa" "Northern Africa"
  , Country "Suriname" "https://en.wikipedia.org/wiki/Suriname" 581372 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/Flag_of_Suriname.svg/23px-Flag_of_Suriname.svg.png") "Americas" "South America"
  , Country "Cape Verde" "https://en.wikipedia.org/wiki/Cape_Verde" 549935 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Flag_of_Cape_Verde.svg/23px-Flag_of_Cape_Verde.svg.png") "Africa" "Western Africa"
  , Country "Maldives" "https://en.wikipedia.org/wiki/Maldives" 530953 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Flag_of_Maldives.svg/23px-Flag_of_Maldives.svg.png") "Asia" "Southern Asia"
  , Country "Guadeloupe" "https://en.wikipedia.org/wiki/Guadeloupe" 447905 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Unofficial_flag_of_Guadeloupe_%28local%29.svg/23px-Unofficial_flag_of_Guadeloupe_%28local%29.svg.png") "Americas" "Caribbean"
  , Country "Malta" "https://en.wikipedia.org/wiki/Malta" 440372 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Flag_of_Malta.svg/23px-Flag_of_Malta.svg.png") "Europe" "Southern Europe"
  , Country "Brunei" "https://en.wikipedia.org/wiki/Brunei" 433285 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/Flag_of_Brunei.svg/23px-Flag_of_Brunei.svg.png") "Asia" "South-eastern Asia"
  , Country "Belize" "https://en.wikipedia.org/wiki/Belize" 390353 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Flag_of_Belize.svg/23px-Flag_of_Belize.svg.png") "Americas" "Central America"
  , Country "Bahamas" "https://en.wikipedia.org/wiki/The_Bahamas" 389482 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/93/Flag_of_the_Bahamas.svg/23px-Flag_of_the_Bahamas.svg.png") "Americas" "Caribbean"
  , Country "Martinique" "https://en.wikipedia.org/wiki/Martinique" 375554 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Snake_Flag_of_Martinique.svg/23px-Snake_Flag_of_Martinique.svg.png") "Americas" "Caribbean"
  , Country "Iceland" "https://en.wikipedia.org/wiki/Iceland" 339031 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Flag_of_Iceland.svg/21px-Flag_of_Iceland.svg.png") "Europe" "Northern Europe"
  , Country "Vanuatu" "https://en.wikipedia.org/wiki/Vanuatu" 299882 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Vanuatu.svg/23px-Flag_of_Vanuatu.svg.png") "Oceania" "Melanesia"
  , Country "Barbados" "https://en.wikipedia.org/wiki/Barbados" 287025 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/Flag_of_Barbados.svg/23px-Flag_of_Barbados.svg.png") "Americas" "Caribbean"
  , Country "New Caledonia" "https://en.wikipedia.org/wiki/New_Caledonia" 282750 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Flag_of_FLNKS.svg/23px-Flag_of_FLNKS.svg.png") "Oceania" "Melanesia"
  , Country "French Guiana" "https://en.wikipedia.org/wiki/French_Guiana" 282731 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Flag_of_French_Guiana.svg/23px-Flag_of_French_Guiana.svg.png") "Americas" "South America"
  , Country "French Polynesia" "https://en.wikipedia.org/wiki/French_Polynesia" 279287 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Flag_of_French_Polynesia.svg/23px-Flag_of_French_Polynesia.svg.png") "Oceania" "Polynesia"
  , Country "Mayotte" "https://en.wikipedia.org/wiki/Mayotte" 266150 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4f/Flag_of_Mayotte_%28Local%29.svg/23px-Flag_of_Mayotte_%28Local%29.svg.png") "Africa" "Eastern Africa"
  , Country "São Tomé and Príncipe" "https://en.wikipedia.org/wiki/S%C3%A3o_Tom%C3%A9_and_Pr%C3%ADncipe" 215056 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4f/Flag_of_Sao_Tome_and_Principe.svg/23px-Flag_of_Sao_Tome_and_Principe.svg.png") "Africa" "Middle Africa"
  , Country "Samoa" "https://en.wikipedia.org/wiki/Samoa" 197097 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Flag_of_Samoa.svg/23px-Flag_of_Samoa.svg.png") "Oceania" "Polynesia"
  , Country "Saint Lucia" "https://en.wikipedia.org/wiki/Saint_Lucia" 182790 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Flag_of_Saint_Lucia.svg/23px-Flag_of_Saint_Lucia.svg.png") "Americas" "Caribbean"
  , Country "Channel Islands" "https://en.wikipedia.org/wiki/Channel_Islands" 172259 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/a/ae/Flag_of_the_United_Kingdom.svg/23px-Flag_of_the_United_Kingdom.svg.png") "Europe" "Northern Europe"
  , Country "Guam" "https://en.wikipedia.org/wiki/Guam" 167294 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/07/Flag_of_Guam.svg/23px-Flag_of_Guam.svg.png") "Oceania" "Micronesia"
  , Country "Curaçao" "https://en.wikipedia.org/wiki/Cura%C3%A7ao" 163424 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b1/Flag_of_Cura%C3%A7ao.svg/23px-Flag_of_Cura%C3%A7ao.svg.png") "Americas" "Caribbean"
  , Country "Kiribati" "https://en.wikipedia.org/wiki/Kiribati" 117606 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/Flag_of_Kiribati.svg/23px-Flag_of_Kiribati.svg.png") "Oceania" "Micronesia"
  , Country "Micronesia" "https://en.wikipedia.org/wiki/Federated_States_of_Micronesia" 113815 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e4/Flag_of_the_Federated_States_of_Micronesia.svg/23px-Flag_of_the_Federated_States_of_Micronesia.svg.png") "Oceania" "Micronesia"
  , Country "Grenada" "https://en.wikipedia.org/wiki/Grenada" 112003 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Grenada.svg/23px-Flag_of_Grenada.svg.png") "Americas" "Caribbean"
  , Country "Tonga" "https://en.wikipedia.org/wiki/Tonga" 110940 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Tonga.svg/23px-Flag_of_Tonga.svg.png") "Oceania" "Polynesia"
  , Country "Saint Vincent and the Grenadines" "https://en.wikipedia.org/wiki/Saint_Vincent_and_the_Grenadines" 110589 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Flag_of_Saint_Vincent_and_the_Grenadines.svg/23px-Flag_of_Saint_Vincent_and_the_Grenadines.svg.png") "Americas" "Caribbean"
  , Country "Aruba" "https://en.wikipedia.org/wiki/Aruba" 106314 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f6/Flag_of_Aruba.svg/23px-Flag_of_Aruba.svg.png") "Americas" "Caribbean"
  , Country "U.S. Virgin Islands" "https://en.wikipedia.org/wiki/United_States_Virgin_Islands" 104578 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f8/Flag_of_the_United_States_Virgin_Islands.svg/23px-Flag_of_the_United_States_Virgin_Islands.svg.png") "Americas" "Caribbean"
  , Country "Seychelles" "https://en.wikipedia.org/wiki/Seychelles" 97739 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Flag_of_Seychelles.svg/23px-Flag_of_Seychelles.svg.png") "Africa" "Eastern Africa"
  , Country "Antigua and Barbuda" "https://en.wikipedia.org/wiki/Antigua_and_Barbuda" 97118 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/Flag_of_Antigua_and_Barbuda.svg/23px-Flag_of_Antigua_and_Barbuda.svg.png") "Americas" "Caribbean"
  , Country "Isle of Man" "https://en.wikipedia.org/wiki/Isle_of_Man" 84584 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_the_Isle_of_Man.svg/23px-Flag_of_the_Isle_of_Man.svg.png") "Europe" "Northern Europe"
  , Country "Andorra" "https://en.wikipedia.org/wiki/Andorra" 77142 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Andorra.svg/22px-Flag_of_Andorra.svg.png") "Europe" "Southern Europe"
  , Country "Dominica" "https://en.wikipedia.org/wiki/Dominica" 71808 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c4/Flag_of_Dominica.svg/23px-Flag_of_Dominica.svg.png") "Americas" "Caribbean"
  , Country "Cayman Islands" "https://en.wikipedia.org/wiki/Cayman_Islands" 64948 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Flag_of_the_Cayman_Islands.svg/23px-Flag_of_the_Cayman_Islands.svg.png") "Americas" "Caribbean"
  , Country "Bermuda" "https://en.wikipedia.org/wiki/Bermuda" 62506 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Flag_of_Bermuda.svg/23px-Flag_of_Bermuda.svg.png") "Americas" "Northern America"
  , Country "Marshall Islands" "https://en.wikipedia.org/wiki/Marshall_Islands" 58791 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2e/Flag_of_the_Marshall_Islands.svg/23px-Flag_of_the_Marshall_Islands.svg.png") "Oceania" "Micronesia"
  , Country "Greenland" "https://en.wikipedia.org/wiki/Greenland" 56672 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Flag_of_Greenland.svg/23px-Flag_of_Greenland.svg.png") "Americas" "Northern America"
  , Country "Northern Mariana Islands" "https://en.wikipedia.org/wiki/Northern_Mariana_Islands" 56188 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Flag_of_the_Northern_Mariana_Islands.svg/23px-Flag_of_the_Northern_Mariana_Islands.svg.png") "Oceania" "Micronesia"
  , Country "American Samoa" "https://en.wikipedia.org/wiki/American_Samoa" 55312 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/87/Flag_of_American_Samoa.svg/23px-Flag_of_American_Samoa.svg.png") "Oceania" "Polynesia"
  , Country "Saint Kitts and Nevis" "https://en.wikipedia.org/wiki/Saint_Kitts_and_Nevis" 52823 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Saint_Kitts_and_Nevis.svg/23px-Flag_of_Saint_Kitts_and_Nevis.svg.png") "Americas" "Caribbean"
  , Country "Faroe Islands" "https://en.wikipedia.org/wiki/Faroe_Islands" 48678 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Flag_of_the_Faroe_Islands.svg/21px-Flag_of_the_Faroe_Islands.svg.png") "Europe" "Northern Europe"
  , Country "Sint Maarten" "https://en.wikipedia.org/wiki/Sint_Maarten" 42388 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/Flag_of_Sint_Maarten.svg/23px-Flag_of_Sint_Maarten.svg.png") "Americas" "Caribbean"
  , Country "Monaco" "https://en.wikipedia.org/wiki/Monaco" 38964 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Flag_of_Monaco.svg/19px-Flag_of_Monaco.svg.png") "Europe" "Western Europe"
  , Country "Turks and Caicos Islands" "https://en.wikipedia.org/wiki/Turks_and_Caicos_Islands" 38191 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/Flag_of_the_Turks_and_Caicos_Islands.svg/23px-Flag_of_the_Turks_and_Caicos_Islands.svg.png") "Americas" "Caribbean"
  , Country "Liechtenstein" "https://en.wikipedia.org/wiki/Liechtenstein" 38019 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/Flag_of_Liechtenstein.svg/23px-Flag_of_Liechtenstein.svg.png") "Europe" "Western Europe"
  , Country "San Marino" "https://en.wikipedia.org/wiki/San_Marino" 33860 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b1/Flag_of_San_Marino.svg/20px-Flag_of_San_Marino.svg.png") "Europe" "Southern Europe"
  , Country "Gibraltar" "https://en.wikipedia.org/wiki/Gibraltar" 33701 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/Flag_of_Gibraltar.svg/23px-Flag_of_Gibraltar.svg.png") "Europe" "Southern Europe"
  , Country "British Virgin Islands" "https://en.wikipedia.org/wiki/British_Virgin_Islands" 30030 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Flag_of_the_British_Virgin_Islands.svg/23px-Flag_of_the_British_Virgin_Islands.svg.png") "Americas" "Caribbean"
  , Country "Caribbean Netherlands" "https://en.wikipedia.org/wiki/Caribbean_Netherlands" 25979 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/23px-Flag_of_the_Netherlands.svg.png") "Americas" "Caribbean"
  , Country "Palau" "https://en.wikipedia.org/wiki/Palau" 18008 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Flag_of_Palau.svg/23px-Flag_of_Palau.svg.png") "Oceania" "Micronesia"
  , Country "Cook Islands" "https://en.wikipedia.org/wiki/Cook_Islands" 17548 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/35/Flag_of_the_Cook_Islands.svg/23px-Flag_of_the_Cook_Islands.svg.png") "Oceania" "Polynesia"
  , Country "Anguilla" "https://en.wikipedia.org/wiki/Anguilla" 14869 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b4/Flag_of_Anguilla.svg/23px-Flag_of_Anguilla.svg.png") "Americas" "Caribbean"
  , Country "Tuvalu" "https://en.wikipedia.org/wiki/Tuvalu" 11646 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Flag_of_Tuvalu.svg/23px-Flag_of_Tuvalu.svg.png") "Oceania" "Polynesia"
  , Country "Wallis and Futuna" "https://en.wikipedia.org/wiki/Wallis_and_Futuna" 11432 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Flag_of_Wallis_and_Futuna.svg/23px-Flag_of_Wallis_and_Futuna.svg.png") "Oceania" "Polynesia"
  , Country "Nauru" "https://en.wikipedia.org/wiki/Nauru" 10756 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/3/30/Flag_of_Nauru.svg/23px-Flag_of_Nauru.svg.png") "Oceania" "Micronesia"
  , Country "Saint Helena" "https://en.wikipedia.org/wiki/Saint_Helena,_Ascension_and_Tristan_da_Cunha" 6059 (Just "https://upload.wikimedia.org/wikipedia/en/thumb/a/ae/Flag_of_the_United_Kingdom.svg/23px-Flag_of_the_United_Kingdom.svg.png") "Africa" "Western Africa"
  , Country "Saint Pierre and Miquelon" "https://en.wikipedia.org/wiki/Saint_Pierre_and_Miquelon" 5822 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Flag_of_Saint-Pierre_and_Miquelon.svg/23px-Flag_of_Saint-Pierre_and_Miquelon.svg.png") "Americas" "Northern America"
  , Country "Montserrat" "https://en.wikipedia.org/wiki/Montserrat" 4989 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Flag_of_Montserrat.svg/23px-Flag_of_Montserrat.svg.png") "Americas" "Caribbean"
  , Country "Falkland Islands" "https://en.wikipedia.org/wiki/Falkland_Islands" 3377 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Flag_of_the_Falkland_Islands.svg/23px-Flag_of_the_Falkland_Islands.svg.png") "Americas" "South America"
  , Country "Niue" "https://en.wikipedia.org/wiki/Niue" 1615 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/01/Flag_of_Niue.svg/23px-Flag_of_Niue.svg.png") "Oceania" "Polynesia"
  , Country "Tokelau" "https://en.wikipedia.org/wiki/Tokelau" 1340 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8e/Flag_of_Tokelau.svg/23px-Flag_of_Tokelau.svg.png") "Oceania" "Polynesia"
  , Country "Vatican City" "https://en.wikipedia.org/wiki/Vatican_City" 799 (Just "https://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Flag_of_the_Vatican_City.svg/16px-Flag_of_the_Vatican_City.svg.png") "Europe" "Southern Europe"
  ]

awsmCss :: Text
awsmCss = "\
  \@charset \"UTF-8\";\
  \/*!\
  \ * awsm.css v3.0.7 (https://igoradamenko.github.io/awsm.css/)\
  \ * Copyright 2015 Igor Adamenko <mail@igoradamenko.com> (https://igoradamenko.com)\
  \ * Licensed under MIT (https://github.com/igoradamenko/awsm.css/blob/master/LICENSE.md)\
  \ */\
  \html{\
  \  font-family:system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell, \"PT Sans\", \"Open Sans\", \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Helvetica, Arial, sans-serif;\
  \  font-size:100%;\
  \  line-height:1.4;\
  \  background:white;\
  \  color:black;\
  \  -webkit-overflow-scrolling:touch;\
  \}\
  \\
  \body{\
  \  margin:1.2em;\
  \  font-size:1rem;\
  \}\
  \@media (min-width: 20rem){\
  \  body{\
  \    font-size:calc(1rem + 0.00625 * (100vw - 20rem));\
  \  }\
  \}\
  \@media (min-width: 40rem){\
  \  body{\
  \    font-size:1.125rem;\
  \  }\
  \}\
  \body header,\
  \body main,\
  \body footer,\
  \body article{\
  \  position:relative;\
  \  max-width:40rem;\
  \  margin:0 auto;\
  \}\
  \body > header{\
  \  margin-bottom:3.5em;\
  \}\
  \body > header h1{\
  \  margin:0;\
  \  font-size:1.5em;\
  \}\
  \body > header p{\
  \  margin:0;\
  \  font-size:0.85em;\
  \}\
  \body > footer{\
  \  margin-top:6em;\
  \  padding-bottom:1.5em;\
  \  text-align:center;\
  \  font-size:0.8rem;\
  \  color:#aaaaaa;\
  \}\
  \\
  \nav{\
  \  margin:1em 0;\
  \}\
  \nav ul{\
  \  list-style:none;\
  \  margin:0;\
  \  padding:0;\
  \}\
  \nav li{\
  \  display:inline-block;\
  \  margin-right:1em;\
  \  margin-bottom:0.25em;\
  \}\
  \nav li:last-child{\
  \  margin-right:0;\
  \}\
  \nav a:visited{\
  \  color:#0064c1;\
  \}\
  \nav a:hover{\
  \  color:#f00000;\
  \}\
  \\
  \ul, ol{\
  \  margin-top:0;\
  \  padding-top:0;\
  \  padding-left:2.5em;\
  \}\
  \ul li + li, ol li + li{\
  \  margin-top:0.25em;\
  \}\
  \ul li > details, ol li > details{\
  \  margin:0;\
  \}\
  \\
  \p{\
  \  margin:1em 0;\
  \  -webkit-hyphens:auto;\
  \      -ms-hyphens:auto;\
  \          hyphens:auto;\
  \}\
  \p:first-child{\
  \  margin-top:0;\
  \}\
  \p:last-child{\
  \  margin-bottom:0;\
  \}\
  \p + ul, p + ol{\
  \  margin-top:-0.75em;\
  \}\
  \p img, p picture{\
  \  float:right;\
  \  margin-bottom:0.5em;\
  \  margin-left:0.5em;\
  \}\
  \p picture img{\
  \  float:none;\
  \  margin:0;\
  \}\
  \\
  \dd{\
  \  margin-bottom:1em;\
  \  margin-left:0;\
  \  padding-left:2.5em;\
  \}\
  \\
  \dt{\
  \  font-weight:700;\
  \}\
  \\
  \blockquote{\
  \  margin:0;\
  \  padding-left:2.5em;\
  \}\
  \\
  \aside{\
  \  margin:0.5em 0;\
  \  font-style:italic;\
  \  color:#aaaaaa;\
  \}\
  \@media (min-width: 65rem){\
  \  aside{\
  \    position:absolute;\
  \    right:-12.5rem;\
  \    width:9.375rem;\
  \    max-width:9.375rem;\
  \    margin:0;\
  \    padding-left:0.5em;\
  \    font-size:0.8em;\
  \    border-left:1px solid #f2f2f2;\
  \  }\
  \}\
  \aside:first-child{\
  \  margin-top:0;\
  \}\
  \aside:last-child{\
  \  margin-bottom:0;\
  \}\
  \\
  \section + section{\
  \  margin-top:2em;\
  \}\
  \\
  \h1, h2, h3, h4, h5, h6{\
  \  margin:1.25em 0 0;\
  \  line-height:1.2;\
  \}\
  \h1:hover > a[href^=\"#\"][id]:empty, h1:focus > a[href^=\"#\"][id]:empty, h2:hover > a[href^=\"#\"][id]:empty, h2:focus > a[href^=\"#\"][id]:empty, h3:hover > a[href^=\"#\"][id]:empty, h3:focus > a[href^=\"#\"][id]:empty, h4:hover > a[href^=\"#\"][id]:empty, h4:focus > a[href^=\"#\"][id]:empty, h5:hover > a[href^=\"#\"][id]:empty, h5:focus > a[href^=\"#\"][id]:empty, h6:hover > a[href^=\"#\"][id]:empty, h6:focus > a[href^=\"#\"][id]:empty{\
  \  opacity:1;\
  \}\
  \h1 + p, h1 + details, h2 + p, h2 + details, h3 + p, h3 + details, h4 + p, h4 + details, h5 + p, h5 + details, h6 + p, h6 + details{\
  \  margin-top:0.5em;\
  \}\
  \h1 > a[href^=\"#\"][id]:empty, h2 > a[href^=\"#\"][id]:empty, h3 > a[href^=\"#\"][id]:empty, h4 > a[href^=\"#\"][id]:empty, h5 > a[href^=\"#\"][id]:empty, h6 > a[href^=\"#\"][id]:empty{\
  \  position:absolute;\
  \  left:-0.65em;\
  \  opacity:0;\
  \  text-decoration:none;\
  \  font-weight:400;\
  \  line-height:1;\
  \  color:#aaaaaa;\
  \}\
  \@media (min-width: 40rem){\
  \  h1 > a[href^=\"#\"][id]:empty, h2 > a[href^=\"#\"][id]:empty, h3 > a[href^=\"#\"][id]:empty, h4 > a[href^=\"#\"][id]:empty, h5 > a[href^=\"#\"][id]:empty, h6 > a[href^=\"#\"][id]:empty{\
  \    left:-0.8em;\
  \  }\
  \}\
  \h1 > a[href^=\"#\"][id]:empty:target, h1 > a[href^=\"#\"][id]:empty:hover, h1 > a[href^=\"#\"][id]:empty:focus, h2 > a[href^=\"#\"][id]:empty:target, h2 > a[href^=\"#\"][id]:empty:hover, h2 > a[href^=\"#\"][id]:empty:focus, h3 > a[href^=\"#\"][id]:empty:target, h3 > a[href^=\"#\"][id]:empty:hover, h3 > a[href^=\"#\"][id]:empty:focus, h4 > a[href^=\"#\"][id]:empty:target, h4 > a[href^=\"#\"][id]:empty:hover, h4 > a[href^=\"#\"][id]:empty:focus, h5 > a[href^=\"#\"][id]:empty:target, h5 > a[href^=\"#\"][id]:empty:hover, h5 > a[href^=\"#\"][id]:empty:focus, h6 > a[href^=\"#\"][id]:empty:target, h6 > a[href^=\"#\"][id]:empty:hover, h6 > a[href^=\"#\"][id]:empty:focus{\
  \  opacity:1;\
  \  box-shadow:none;\
  \  color:black;\
  \}\
  \h1 > a[href^=\"#\"][id]:empty:target:focus, h2 > a[href^=\"#\"][id]:empty:target:focus, h3 > a[href^=\"#\"][id]:empty:target:focus, h4 > a[href^=\"#\"][id]:empty:target:focus, h5 > a[href^=\"#\"][id]:empty:target:focus, h6 > a[href^=\"#\"][id]:empty:target:focus{\
  \  outline:none;\
  \}\
  \h1 > a[href^=\"#\"][id]:empty::before, h2 > a[href^=\"#\"][id]:empty::before, h3 > a[href^=\"#\"][id]:empty::before, h4 > a[href^=\"#\"][id]:empty::before, h5 > a[href^=\"#\"][id]:empty::before, h6 > a[href^=\"#\"][id]:empty::before{\
  \  content:\"§ \";\
  \}\
  \\
  \h1{\
  \  font-size:2.5em;\
  \}\
  \\
  \h2{\
  \  font-size:1.75em;\
  \}\
  \\
  \h3{\
  \  font-size:1.25em;\
  \}\
  \\
  \h4{\
  \  font-size:1.15em;\
  \}\
  \\
  \h5{\
  \  font-size:1em;\
  \}\
  \\
  \h6{\
  \  margin-top:1em;\
  \  font-size:1em;\
  \  color:#aaaaaa;\
  \}\
  \\
  \article + article{\
  \  margin-top:4em;\
  \}\
  \article header p{\
  \  font-size:0.6em;\
  \  color:#aaaaaa;\
  \}\
  \article header p + h1, article header p + h2{\
  \  margin-top:-0.25em;\
  \}\
  \article header h1 + p, article header h2 + p{\
  \  margin-top:0.25em;\
  \}\
  \article header h1 a, article header h2 a{\
  \  color:black;\
  \}\
  \article header h1 a:visited, article header h2 a:visited{\
  \  color:#aaaaaa;\
  \}\
  \article header h1 a:visited:hover, article header h2 a:visited:hover{\
  \  color:#f00000;\
  \}\
  \article > footer{\
  \  margin-top:1.5em;\
  \  font-size:0.85em;\
  \}\
  \\
  \a{\
  \  color:#0064c1;\
  \}\
  \a:visited{\
  \  color:#8d39d0;\
  \}\
  \a:hover, a:active{\
  \  outline-width:0;\
  \}\
  \a:hover{\
  \  color:#f00000;\
  \}\
  \a abbr{\
  \  font-size:1em;\
  \}\
  \\
  \abbr{\
  \  margin-right:-0.075em;\
  \  text-decoration:none;\
  \  -webkit-hyphens:none;\
  \      -ms-hyphens:none;\
  \          hyphens:none;\
  \  letter-spacing:0.075em;\
  \  font-size:0.9em;\
  \}\
  \\
  \img, picture{\
  \  display:block;\
  \  max-width:100%;\
  \  margin:0 auto;\
  \}\
  \\
  \audio, video{\
  \  width:100%;\
  \  max-width:100%;\
  \}\
  \\
  \figure{\
  \  margin:1em 0 0.5em;\
  \  padding:0;\
  \}\
  \figure + p{\
  \  margin-top:0.5em;\
  \}\
  \figure figcaption{\
  \  opacity:0.65;\
  \  font-size:0.85em;\
  \}\
  \\
  \table{\
  \  display:inline-block;\
  \  border-spacing:0;\
  \  border-collapse:collapse;\
  \  overflow-x:auto;\
  \  max-width:100%;\
  \  text-align:left;\
  \  vertical-align:top;\
  \  background:linear-gradient(rgba(0, 0, 0, 0.15) 0%, rgba(0, 0, 0, 0.15) 100%) 0 0, linear-gradient(rgba(0, 0, 0, 0.15) 0%, rgba(0, 0, 0, 0.15) 100%) 100% 0;\
  \  background-attachment:scroll, scroll;\
  \  background-size:1px 100%, 1px 100%;\
  \  background-repeat:no-repeat, no-repeat;\
  \}\
  \table caption{\
  \  font-size:0.9em;\
  \  background:white;\
  \}\
  \table td, table th{\
  \  padding:0.35em 0.75em;\
  \  vertical-align:top;\
  \  font-size:0.9em;\
  \  border:1px solid #f2f2f2;\
  \  border-top:0;\
  \  border-left:0;\
  \}\
  \table td:first-child, table th:first-child{\
  \  padding-left:0;\
  \  background-image:linear-gradient(to right, white 50%, rgba(255, 255, 255, 0) 100%);\
  \  background-size:2px 100%;\
  \  background-repeat:no-repeat;\
  \}\
  \table td:last-child, table th:last-child{\
  \  padding-right:0;\
  \  border-right:0;\
  \  background-image:linear-gradient(to left, white 50%, rgba(255, 255, 255, 0) 100%);\
  \  background-position:100% 0;\
  \  background-size:2px 100%;\
  \  background-repeat:no-repeat;\
  \}\
  \table td:only-child, table th:only-child{\
  \  background-image:linear-gradient(to right, white 50%, rgba(255, 255, 255, 0) 100%), linear-gradient(to left, white 50%, rgba(255, 255, 255, 0) 100%);\
  \  background-position:0 0, 100% 0;\
  \  background-size:2px 100%, 2px 100%;\
  \  background-repeat:no-repeat, no-repeat;\
  \}\
  \table th{\
  \  line-height:1.2;\
  \}\
  \\
  \form{\
  \  margin-right:auto;\
  \  margin-left:auto;\
  \}\
  \@media (min-width: 40rem){\
  \  form{\
  \    max-width:80%;\
  \  }\
  \}\
  \form select, form label{\
  \  display:block;\
  \}\
  \form label:not(:first-child){\
  \  margin-top:1em;\
  \}\
  \form p label{\
  \  display:inline;\
  \}\
  \form p label + label{\
  \  margin-left:1em;\
  \}\
  \form legend:first-child + label{\
  \  margin-top:0;\
  \}\
  \form select, form input[type], form textarea{\
  \  margin-bottom:1em;\
  \}\
  \form input[type=checkbox], form input[type=radio]{\
  \  margin-bottom:0;\
  \}\
  \\
  \fieldset{\
  \  margin:0;\
  \  padding:0.5em 1em;\
  \  border:1px solid #aaaaaa;\
  \}\
  \\
  \legend{\
  \  color:#aaaaaa;\
  \}\
  \\
  \button{\
  \  outline:none;\
  \  box-sizing:border-box;\
  \  height:2em;\
  \  margin:0;\
  \  padding:calc(.25em - 1px) 0.5em;\
  \  font-family:inherit;\
  \  font-size:1em;\
  \  border:1px solid #aaaaaa;\
  \  border-radius:2px;\
  \  background:white;\
  \  color:black;\
  \  display:inline-block;\
  \  width:auto;\
  \  background:#f2f2f2;\
  \  color:black;\
  \  cursor:pointer;\
  \}\
  \button:focus{\
  \  border:1px solid black;\
  \}\
  \button:not([disabled]):hover{\
  \  border:1px solid black;\
  \}\
  \button:active{\
  \  background-color:#aaaaaa;\
  \}\
  \button[disabled]{\
  \  color:#aaaaaa;\
  \  cursor:not-allowed;\
  \}\
  \\
  \select{\
  \  outline:none;\
  \  box-sizing:border-box;\
  \  height:2em;\
  \  margin:0;\
  \  padding:calc(.25em - 1px) 0.5em;\
  \  font-family:inherit;\
  \  font-size:1em;\
  \  border:1px solid #aaaaaa;\
  \  border-radius:2px;\
  \  background:white;\
  \  color:black;\
  \  display:inline-block;\
  \  width:auto;\
  \  background:#f2f2f2;\
  \  color:black;\
  \  cursor:pointer;\
  \  padding-right:1.2em;\
  \  background-position:top 55% right 0.35em;\
  \  background-size:0.5em;\
  \  background-repeat:no-repeat;\
  \  -webkit-appearance:none;\
  \     -moz-appearance:none;\
  \          appearance:none;\
  \  background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 3 2'%3E%3Cpath fill='rgb(170, 170, 170)' fill-rule='nonzero' d='M1.5 2L3 0H0z'/%3E%3C/svg%3E\");\
  \}\
  \select:focus{\
  \  border:1px solid black;\
  \}\
  \select:not([disabled]):hover{\
  \  border:1px solid black;\
  \}\
  \select:active{\
  \  background-color:#aaaaaa;\
  \}\
  \select[disabled]{\
  \  color:#aaaaaa;\
  \  cursor:not-allowed;\
  \}\
  \select:not([disabled]):focus, select:not([disabled]):hover{\
  \  background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 3 2'%3E%3Cpath fill='rgb(0, 0, 0)' fill-rule='nonzero' d='M1.5 2L3 0H0z'/%3E%3C/svg%3E\");\
  \}\
  \\
  \input[type=text], input[type=password], input[type^=date], input[type=email], input[type=number], input[type=search], input[type=tel], input[type=time], input[type=month], input[type=week], input[type=url]{\
  \  outline:none;\
  \  box-sizing:border-box;\
  \  height:2em;\
  \  margin:0;\
  \  padding:calc(.25em - 1px) 0.5em;\
  \  font-family:inherit;\
  \  font-size:1em;\
  \  border:1px solid #aaaaaa;\
  \  border-radius:2px;\
  \  background:white;\
  \  color:black;\
  \  display:block;\
  \  width:100%;\
  \  line-height:calc(2em - 1px * 2 - (.25em - 1px) * 2);\
  \  -webkit-appearance:none;\
  \     -moz-appearance:none;\
  \          appearance:none;\
  \}\
  \input[type=text]:focus, input[type=password]:focus, input[type^=date]:focus, input[type=email]:focus, input[type=number]:focus, input[type=search]:focus, input[type=tel]:focus, input[type=time]:focus, input[type=month]:focus, input[type=week]:focus, input[type=url]:focus{\
  \  border:1px solid black;\
  \}\
  \input[type=text]::-moz-placeholder, input[type=password]::-moz-placeholder, input[type^=date]::-moz-placeholder, input[type=email]::-moz-placeholder, input[type=number]::-moz-placeholder, input[type=search]::-moz-placeholder, input[type=tel]::-moz-placeholder, input[type=time]::-moz-placeholder, input[type=month]::-moz-placeholder, input[type=week]::-moz-placeholder, input[type=url]::-moz-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \input[type=text]::-webkit-input-placeholder, input[type=password]::-webkit-input-placeholder, input[type^=date]::-webkit-input-placeholder, input[type=email]::-webkit-input-placeholder, input[type=number]::-webkit-input-placeholder, input[type=search]::-webkit-input-placeholder, input[type=tel]::-webkit-input-placeholder, input[type=time]::-webkit-input-placeholder, input[type=month]::-webkit-input-placeholder, input[type=week]::-webkit-input-placeholder, input[type=url]::-webkit-input-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \input[type=text]:-ms-input-placeholder, input[type=password]:-ms-input-placeholder, input[type^=date]:-ms-input-placeholder, input[type=email]:-ms-input-placeholder, input[type=number]:-ms-input-placeholder, input[type=search]:-ms-input-placeholder, input[type=tel]:-ms-input-placeholder, input[type=time]:-ms-input-placeholder, input[type=month]:-ms-input-placeholder, input[type=week]:-ms-input-placeholder, input[type=url]:-ms-input-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \input[type=submit], input[type=button], input[type=reset]{\
  \  outline:none;\
  \  box-sizing:border-box;\
  \  height:2em;\
  \  margin:0;\
  \  padding:calc(.25em - 1px) 0.5em;\
  \  font-family:inherit;\
  \  font-size:1em;\
  \  border:1px solid #aaaaaa;\
  \  border-radius:2px;\
  \  background:white;\
  \  color:black;\
  \  display:inline-block;\
  \  width:auto;\
  \  background:#f2f2f2;\
  \  color:black;\
  \  cursor:pointer;\
  \  -webkit-appearance:none;\
  \     -moz-appearance:none;\
  \          appearance:none;\
  \}\
  \input[type=submit]:focus, input[type=button]:focus, input[type=reset]:focus{\
  \  border:1px solid black;\
  \}\
  \input[type=submit]:not([disabled]):hover, input[type=button]:not([disabled]):hover, input[type=reset]:not([disabled]):hover{\
  \  border:1px solid black;\
  \}\
  \input[type=submit]:active, input[type=button]:active, input[type=reset]:active{\
  \  background-color:#aaaaaa;\
  \}\
  \input[type=submit][disabled], input[type=button][disabled], input[type=reset][disabled]{\
  \  color:#aaaaaa;\
  \  cursor:not-allowed;\
  \}\
  \input[type=color]{\
  \  outline:none;\
  \  box-sizing:border-box;\
  \  height:2em;\
  \  margin:0;\
  \  padding:calc(.25em - 1px) 0.5em;\
  \  font-family:inherit;\
  \  font-size:1em;\
  \  border:1px solid #aaaaaa;\
  \  border-radius:2px;\
  \  background:white;\
  \  color:black;\
  \  display:block;\
  \  width:100%;\
  \  line-height:calc(2em - 1px * 2 - (.25em - 1px) * 2);\
  \  -webkit-appearance:none;\
  \     -moz-appearance:none;\
  \          appearance:none;\
  \  width:6em;\
  \}\
  \input[type=color]:focus{\
  \  border:1px solid black;\
  \}\
  \input[type=color]::-moz-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \input[type=color]::-webkit-input-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \input[type=color]:-ms-input-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \input[type=color]:hover{\
  \  border:1px solid black;\
  \}\
  \input[type=file]{\
  \  outline:none;\
  \  box-sizing:border-box;\
  \  height:2em;\
  \  margin:0;\
  \  padding:calc(.25em - 1px) 0.5em;\
  \  font-family:inherit;\
  \  font-size:1em;\
  \  border:1px solid #aaaaaa;\
  \  border-radius:2px;\
  \  background:white;\
  \  color:black;\
  \  display:inline-block;\
  \  width:auto;\
  \  background:#f2f2f2;\
  \  color:black;\
  \  cursor:pointer;\
  \  display:block;\
  \  width:100%;\
  \  height:auto;\
  \  padding:0.75em 0.5em;\
  \  font-size:12px;\
  \  line-height:1;\
  \}\
  \input[type=file]:focus{\
  \  border:1px solid black;\
  \}\
  \input[type=file]:not([disabled]):hover{\
  \  border:1px solid black;\
  \}\
  \input[type=file]:active{\
  \  background-color:#aaaaaa;\
  \}\
  \input[type=file][disabled]{\
  \  color:#aaaaaa;\
  \  cursor:not-allowed;\
  \}\
  \input[type=checkbox], input[type=radio]{\
  \  margin:-0.2em 0.75em 0 0;\
  \  vertical-align:middle;\
  \}\
  \\
  \textarea{\
  \  outline:none;\
  \  box-sizing:border-box;\
  \  height:2em;\
  \  margin:0;\
  \  padding:calc(.25em - 1px) 0.5em;\
  \  font-family:inherit;\
  \  font-size:1em;\
  \  border:1px solid #aaaaaa;\
  \  border-radius:2px;\
  \  background:white;\
  \  color:black;\
  \  display:block;\
  \  width:100%;\
  \  line-height:calc(2em - 1px * 2 - (.25em - 1px) * 2);\
  \  -webkit-appearance:none;\
  \     -moz-appearance:none;\
  \          appearance:none;\
  \  height:4.5em;\
  \  resize:vertical;\
  \  padding-top:0.5em;\
  \  padding-bottom:0.5em;\
  \}\
  \textarea:focus{\
  \  border:1px solid black;\
  \}\
  \textarea::-moz-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \textarea::-webkit-input-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \textarea:-ms-input-placeholder{\
  \  color:#aaaaaa;\
  \}\
  \\
  \output{\
  \  display:block;\
  \}\
  \\
  \code, kbd, var, samp{\
  \  font-family:Consolas, \"Lucida Console\", Monaco, monospace;\
  \  font-style:normal;\
  \}\
  \\
  \pre{\
  \  overflow-x:auto;\
  \  font-size:0.8em;\
  \  background:linear-gradient(rgba(0, 0, 0, 0.15) 0%, rgba(0, 0, 0, 0.15) 100%) 0 0, linear-gradient(rgba(0, 0, 0, 0.15) 0%, rgba(0, 0, 0, 0.15) 100%) 100% 0;\
  \  background-attachment:scroll, scroll;\
  \  background-size:1px 100%, 1px 100%;\
  \  background-repeat:no-repeat, no-repeat;\
  \}\
  \pre > code{\
  \  display:inline-block;\
  \  overflow-x:visible;\
  \  box-sizing:border-box;\
  \  min-width:100%;\
  \  border-right:3px solid white;\
  \  border-left:1px solid white;\
  \}\
  \\
  \hr{\
  \  height:1px;\
  \  margin:2em 0;\
  \  border:0;\
  \  background:#f2f2f2;\
  \}\
  \\
  \details{\
  \  margin:1em 0;\
  \}\
  \details[open]{\
  \  padding-bottom:0.5em;\
  \  border-bottom:1px solid #f2f2f2;\
  \}\
  \\
  \summary{\
  \  display:inline-block;\
  \  font-weight:700;\
  \  border-bottom:1px dashed;\
  \  cursor:pointer;\
  \}\
  \summary::-webkit-details-marker{\
  \  display:none;\
  \}\
  \\
  \noscript{\
  \  color:#d00000;\
  \}\
  \\
  \::selection{\
  \  background:rgba(0, 100, 193, 0.25);\
  \}"
