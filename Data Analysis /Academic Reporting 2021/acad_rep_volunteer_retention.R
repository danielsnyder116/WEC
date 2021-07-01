#-------------------------------------------
# ACADEMIC REPORTING: VOLUNTEER RECRUITMENT
#-------------------------------------------

#M 9:40 - 5:40 = 8 hours
#Tu 10:00 - 3:00 = 5
#W  5:30 - 7:30 = 2
#   7:45 -



# Increase year-over-year retention rate to 50%
# Note: Pandemic is wildcard - this might not be reasonable  

library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)
library(tibble)
library(glue)
library(ggplot2)

library(viridis)




setwd("/Users/Daniel/Desktop/WEC/Data/All (2006 - Present)")


df <- read.csv("all_volunteers_2006_2021_final.csv", stringsAsFactors = FALSE) %>% 
                mutate(semester=factor(semester, levels = c("SUMMER", "FALL", "WINTER", "SPRING")),
                       year = as.double(year)) %>%
                filter(year != 2006) %>% 
                arrange(across(c(year, semester))) %>% 
  
                #To create FY and get rid of two semester from partial FY 2006
                mutate(fy = case_when((semester %in% c("WINTER", "SPRING")) ~ (year - 1), TRUE ~ year)) %>%
                select(-year) %>%
                rename(year = fy) %>%
                filter(year != 2006)

               

#Recode Year so we are working with FY and not CY
# df <- df %>% mutate(fy = case_when((semester %in% c("WINTER", "SPRING")) ~ (year - 1),
#                                    TRUE ~ year)
#                     ) %>% distinct(year, semester, fy)
verify <- df %>% distinct(semester, year)

length(unique(df$name))

#Manual correcting of name variation for same individual argh
#-------------------
df <- df %>% mutate(name = case_when(str_detect(name, "PIOCH") ~ "ABIGAIL (ABBY) PIOCH",
                                     str_detect(name, "ABBY THOMSEN") ~ "ABBY THOMSEN VARGAS",
                                     str_detect(name, "ALBERT WAT") ~ "ALBERT WATT",
                                     str_detect(name, "ALEX NAMETH|ALEXANDRA NEMETH") ~ "ALEXANDRA (ALEX) NEMETH",
                                     str_detect(name, "ALEXANDRA BUDZ") ~ "ALEXANDRA (ALEX) BUDZ",
                                     str_detect(name, "ALEXANNEDRA TAYLOR") ~ "ALEXANDRA TAYLOR",
                                     str_detect(name, "ALISON PANSA") ~ "ALISON PANZA",
                                     str_detect(name, "ALLISON KIRSH") ~ "ALLISON KIRSCH",
                                     str_detect(name, "ALLISON SMITH") ~ "ALISON SMITH",
                                     str_detect(name, "AMY LILLS|AMY LILLAS") ~ "AMY LILLIS",
                                     str_detect(name, "AMY PECK$") ~ "AMY PECK ABRAHAM",
                                     str_detect(name, "ANASTACIO ANDRES RAMOS") ~ "ANASTACIO RAMOS",
                                     str_detect(name, "ANDREA ANDY BROWN|ANDY BROWN|ANDIE BROWN") ~ "	ANDREA (ANDY) BROWN",
                                     str_detect(name, "ANDREW MCKENZIE|ANDREW R\\. MACKENZIE") ~ "ANDREW MACKENZIE",
                                     str_detect(name, "ANGIE DEVEREAUX|ANGELA DEVEREAUX") ~ "ANGELA (ANGIE) DEVEREAUX",
                                     str_detect(name, "ANI BENESON") ~ "ANI BENENSON",
                                     str_detect(name, "ANN WORTH|ANN WROTH") ~ "ANN WORTH/WROTH?",
                                     str_detect(name, "CELIE YANN|ANNA \\(CELIE\\) YANN") ~ "ANNA (CELIE) YANN",
                                     str_detect(name, "ANNA EAPON") ~ "ANNA EAPEN",
                                     str_detect(name, "ANNA LUKAS") ~ "ANNA LUKACS",
                                     str_detect(name, "ANNA R. MILLER") ~ "ANNA MILLER",
                                     str_detect(name, "ANNA STINCHOMB") ~ "ANNA STINCHCOMB",
                                     str_detect(name, "ANNE BOWMAN HAYES") ~ "ANNE BOWMAN",
                                     str_detect(name, "ANNE MACASKILL|ANNE MACKASKIL") ~ "ANNE MACKASKIL/MACASKILL?",
                                     str_detect(name, "ANNE MARIE CREIGHTON") ~ "ANNE CREIGHTON",
                                     str_detect(name, "ANNE PARTIKA") ~ "ANNIE PARTIKA",
                                     str_detect(name, "ARIANA SPILOTES") ~ "ARIANA SPILIOTES",
                                     str_detect(name, "ARLEEN RITCHTER|ARLEEN RITCHER") ~ "ARLEEN RICHTER",
                                     str_detect(name, "ARTHUR GOLDBERG|ART GOLDBERG") ~ "	ARTHUR (ART) GOLDBERG",
                                     str_detect(name, "ASHELY LOVELACE") ~ "ASHLEY LOVELACE",
                                     str_detect(name, "ASHWIN THOMAS EAPEN") ~ "ASHWIN EAPEN",
                                     str_detect(name, "AUDREY TAFOYA") ~ "AUDRY TAFOYA",
                                     str_detect(name, "BARRY GOTTFRIELD") ~ "BARRY GOTTFRIED",
                                     str_detect(name, "ELIZABETH BETSY DANELLO|BETSY DANELLO") ~ "ELIZABETH (BETSY) DANELLO",
                                     str_detect(name, "BETSY DONAGHUE") ~ "BETSY DONAHOE",
                                     str_detect(name, "BILLY KLAY") ~ "BILL KLAY",
                                     str_detect(name, "BOB KANCHUGAR") ~ "BOB KANCHUGER",
                                     str_detect(name, "BRENDEN RYAN") ~ "BRENDAN RYAN",
                                     str_detect(name, "BRENNA MANION|BRENNA$") ~ "BRENNA MANNION",
                                     str_detect(name, "BRETT FORTMAN") ~ "BRETT FORTNAM",
                                     str_detect(name, "BRETT LADIUNSKY") ~ "BRETT LADUZINSKY",
                                     str_detect(name, "BRIAN DAMERON") ~ "BRIAN DAMRON",
                                     str_detect(name, "BRAIN NEW") ~ "BRIAN NEW",
                                     str_detect(name, "BRIANA JOSLYN|BRIANNA JOSSEYLN") ~ "BRIANNA JOSLYN",
                                     str_detect(name, "BRUCE FRISHMEN	") ~ "BRUCE FRISHMAN",
                                     str_detect(name, "BRUCE JOHSON") ~ "BRUCE JOHNSON",
                                     str_detect(name, "CAILIN GARLOW") ~ "CAITLIN GARLOW",
                                     str_detect(name, "CAROL CHICOWSKI") ~ "CAROL CICHOWSKI",
                                     str_detect(name, "CAROL SIMMONS") ~ "CAROL SIMONS",
                                     str_detect(name, "CAROLINE RENZOULI") ~ "CAROLINE RENZULLI",
                                     str_detect(name, "CARRIE PARROTT MONAHAN") ~ "CARRIE MONAHAN",
                                     str_detect(name, "CATHERINE CRUMM") ~ "CATHERINE CRUM",
                                     str_detect(name, "CATHERINE GOODE|CAT GOODE") ~ "CATHERINE (CAT) GOODE",
                                     str_detect(name, "CATHY LION") ~ "CATHY KLION",
                                     str_detect(name, "CHAR SLAUGHTER|CHARMELL SLAUGHTER") ~ "CHARMELL (CHAR) SLAUGHTER",
                                     str_detect(name, "CHELSEA PRAX") ~ "CHELSEA RAE PRAX",
                                     str_detect(name, "CHIP BRIAN$") ~ "CHIP BRIAN-HORBERG",
                                     str_detect(name, "CHRISTIAN CHRIS HOWLETT") ~ "CHRISTIAN (CHRIS) HOWLETT",
                                     str_detect(name, "CHRISTINA CHEUNG|CHRISSIE CHEUNG") ~ "CHRISTINA (CHRISSIE) CHEUNG",
                                     str_detect(name, "CHRISTOPHER HEART") ~ "CHRISTOPHER HART",
                                     str_detect(name, "CHRISTY WOOD|CHRISTIAN WOOD|CHRISTINA WOOD") ~ "CHRISTINA (CHRISTY) WOOD",
                                     str_detect(name, "CLAIRE KEVILLE") ~ "CLAIRE KEVILL",
                                     str_detect(name, "CLARE BRENSNAHAN|CLARE BRESNAHAM") ~ "CLARE BRESNAHAN",
                                     str_detect(name, "CONVERSATION CLASS WITH ELENA BEISEL") ~ "ELENA BEISEL",
                                     str_detect(name, "CONVERSATION CLASS WITH STEVE SEIDEL") ~ "STEVE SEIDEL",
                                     str_detect(name, "CONVERSATION CLASS WITH YOUMNA SIRGI") ~ "YOUMNA SIRGI",
                                     str_detect(name, "COURTNEY MCCARTNEY") ~ "COURTNEY MCCARTY",
                                     str_detect(name, "COURTNEY SCHETT|COURTNEY SCHETTO") ~ "",
                                     str_detect(name, "CYTHNIA CHASE") ~ "CYNTHIA CHASE",
                                     str_detect(name, "D'VERA COHN") ~ "D'VERA (DEE) COHN",
                                     str_detect(name, "DALE KAUFFMAN") ~ "	DALE KAUFMAN",
                                     str_detect(name, "DANA JU BYKOWSKI") ~ "DANA BYKOWSKI",
                                     str_detect(name, "DANA PERONNE") ~ "DANA PERRONE",
                                     str_detect(name, "DANNY TANG|DANIEL TANG") ~ "DANIEL (DANNY) TANG",
                                     str_detect(name, "DANNY BROOKMEYER") ~ "	DANNY BROOKMYER",
                                     str_detect(name, "DAVID GARLOCK WEDENSDAY 10:00 AM - 11:00 AM") ~ "DAVID GARLOCK",
                                     str_detect(name, "DAVID THUONG") ~ "DAVID TRUONG",
                                     str_detect(name, "DAVON$") ~ "DAVON COLLINS",
                                     str_detect(name, "DEANNA DURRET") ~ "DEANNA DURRETT",
                                     str_detect(name, "DEBRRA DRUMHELLER") ~ "DEBRA DRUMHELLER",
                                     str_detect(name, "DEIDRE DLUGOLESKI") ~ "DEIRDRE DLUGOLESKI",
                                     str_detect(name, "DEIRDRA DONAHUE") ~ "DEIRDRE DONAHUE",
                                     str_detect(name, "DENIS ILLIGE-S") ~ "DENIS ILLIGE-SAUCIER",
                                     str_detect(name, "DENISE GAUF|DENISE$") ~ "DENISE GAUFF",
                                     str_detect(name, "DENNIS MERSMAN") ~ "DENNIS MERSMANN",
                                     str_detect(name, "DIAN SEIDAL") ~ "DIAN SEIDEL",
                                     str_detect(name, "DIANNA$") ~ "DIANNA FREY",
                                     str_detect(name, "DINA LASOW") ~ "DINA LASSOW",
                                     str_detect(name, "DONNA MARIE THOMPSON") ~ "DONNA-MARIE THOMPSON",
                                     str_detect(name, "EDWARD PERUGIA|ED PERUGIA") ~ "EDWARD (ED) PERUGIA",
                                     str_detect(name, "EILEEN LAFLEUR") ~ "EILEEN LA FLEUR",
                                     str_detect(name, "EILEEN BAUMGARTNER") ~ "EILENE BAUMGARTNER",
                                     str_detect(name, "ELEANOR ELLIE SHIRLEY|ELEANOR SHIRLEY") ~ "ELEANOR (ELLIE) SHIRLEY",
                                     str_detect(name, "ELISSA LEIBOWITZ POMA|ELISSA LEIBOWITZ-POMA") ~ "ELISSA POMA",
                                     str_detect(name, "ELIZABETH$|ELIZABETH KONISCHOW|ELIZABETH KONISCHOCOW") ~ "ELIZABETH KONIUSZKOW",
                                     str_detect(name, "ELIZABETH G\\.") ~ "ELISABETH GRONINGER",
                                     str_detect(name, "ELIZABETH MARY PECKHAM") ~ "ELIZABETH PECKHAM",
                                     str_detect(name, "ELIZABETHANNBLAKE@GMAIL.COM") ~ "ELIZABETH BLAKE",
                                     str_detect(name, "ELLEN VARGYAS") ~ "ELLEN VARGAS",
                                     str_detect(name, "ELLIE$") ~ " ELLIE ?",
                                     str_detect(name, "ELLY PEARL") ~ "ELLY PERL",
                                     str_detect(name, "EMILIE SCHMIDLER") ~ "EMILIE SCHMEIDLER",
                                     str_detect(name, "EMILY A BURLINGHAUS") ~ "EMILY BURLINGHAUS",
                                     str_detect(name, "EMILY DORSO") ~ "EMILY DURSO",
                                     str_detect(name, "EMILY ROTHROCK|EMILY ROTHROCK KASTLER") ~ "EMILY ROTHROCK-KASTLER",
                                     str_detect(name, "ERIC MON$") ~ "ERIC SIGMON",
                                     str_detect(name, "ERIN D\\. DUMBACHER") ~ "ERIN DUMBACHER",
                                     str_detect(name, "ERIN MADSEN") ~ "ERIN MASDEN",
                                     str_detect(name, "ERIN RESTIG") ~ "ERIN RISTIG",
                                     str_detect(name, "ESHITA AZZUMEE") ~ "ESHITA AZZMEE",
                                     str_detect(name, "FRANCES \\\"CLAIRE\\\" SAYLER|FRANCES SAYLER") ~ "FRANCES (CLAIRE) SAYLER",
                                     str_detect(name, "FRANCISCO APOCACA") ~ "FRANCISCO APODACA",
                                     str_detect(name, "GAMBLE HAYDAN") ~ "GAMBLE HAYDEN",
                                     str_detect(name, "GAVINMCGOVERN") ~ "GAVIN MCGOVERN",
                                     str_detect(name, "GEORGE PENNY|GEORGE WILSON PENNY") ~ "GEORGE PENNY",
                                     str_detect(name, "GEORGE POLIZOS") ~ "GEORGEA POLIZOS",
                                     str_detect(name, "GLORIA J\\.") ~ "GLORIA JURADO",
                                     str_detect(name, "GREG WEISMANN") ~ "	GREG WEISMAN",
                                     str_detect(name, "GREG RHODES") ~ "GREG RHOADS",
                                     str_detect(name, "GRISSEL CACHICATARI") ~ "GRISSELL CACHICATARI",
                                     str_detect(name, "GUILLERMO DIAZ OLA|	GUILLERMO DÍAZ") ~ "GUILLERMO DIAZ",
                                     str_detect(name, "GULNARA MURZAGIDINA|GULNARA MURZAGILDINA") ~ "GULNARA MURZAGILDINA/MURZAGIDINA?",
                                     str_detect(name, "GULNEET KHOLI") ~ "GULNEET KOHLI",
                                     str_detect(name, "HAILY SIPLE") ~ "HAILEY SIPLE",
                                     str_detect(name, "HANNATU TARFA") ~ "HANNAH TARFA",
                                     str_detect(name, "HARRIET TRITTEL") ~ "HARRIET TRITTELL",
                                     str_detect(name, "HELEN WALSCH") ~ "HELEN WALSH",
                                     str_detect(name, "HELENE LEWINSKI") ~ "HELENE LIWINSKI",
                                     str_detect(name, "HENRY$") ~ "HENRY ?",
                                     str_detect(name, "HOLLY SAURE") ~ "HOLLY SAUER",
                                     str_detect(name, "ILSABEL URBAN|LLSABE URBAN|ILASABE URBANE") ~ "ILSABE URBAN",
                                     str_detect(name, "IOAN SUCIO") ~ "IOAN SUCIU",
                                     str_detect(name, "JAD K\\. ATALLAH") ~ "JAD ATALLAH",
                                     str_detect(name, "JAKELINE BARCELOS|JAKIE BARCELOS") ~ "JAKELINE (JAKIE) BARCELOS",
                                     str_detect(name, "JAN$") ~ "JAN ?",
                                     str_detect(name, "JANANI LYENGAR	") ~ "JANANI IYENGAR",
                                     str_detect(name, "JANDAN AYSE KIRISCI") ~ "JANDAN KIRISCI",
                                     str_detect(name, "JANET BARTLET") ~ "JANET BARLETT",
                                     str_detect(name, "JEFF KELLY|JEFFREY KELLY|JEFF KELLEY") ~ "JEFFREY (JEFF) KELLEY",
                                     str_detect(name, "JENNIFER$") ~ "JENNIFER ?",
                                     str_detect(name, "JENIFFER GRIFFITHS") ~ "JENNIFER GRIFFITHS",
                                     str_detect(name, "JENNY C\\. HODGES|JENNY CLARINDA HODGES") ~ "JENNY HODGES",
                                     str_detect(name, "JENNY O'CONNEL") ~ "JENNY O'CONNELL",
                                     str_detect(name, "JHONNATHAN MENDOZA$") ~ "JHONNATHAN MENDOZA TORO",
                                     str_detect(name, "JILLIAN VAN ELLIS") ~ "JILLIAN VAN ELLS",
                                     str_detect(name, "JOANNEVINE") ~ "JOANNE VINE",
                                     str_detect(name, "JOCELYNE OLGA DO SACREMENTO") ~ "JOCELYNE DO SACREMENTO",
                                     str_detect(name, "JOE CROCKET") ~ "	JOE CROCKETT",
                                     str_detect(name, "JON APFLBAUM") ~ "JON APFELBAUM",
                                     str_detect(name, "JORDAN BEARDEN") ~ "JORDAN BEARDIN",
                                     str_detect(name, "JORGE POSADOS") ~ "JORGE POSADAS",
                                     str_detect(name, "JOSE LUIS FLORES") ~ "JOSE FLORES",
                                     str_detect(name, "JOSH HINMAN|JOSHUA HINMAN") ~ "JOSHUA (JOSH) HINMAN",
                                     str_detect(name, "JR DRYER") ~ "JR DREYER",
                                     str_detect(name, "JUDITH OVERBY") ~ "JUDITH OVERBEY",
                                     str_detect(name, "JUDY GUY") ~ "JUDIE GUY",
                                     str_detect(name, "JULIE$") ~ "JULIE ?",
                                     str_detect(name, "JULIET A\\. SABLOSKY|JULIET SOBLOSKY") ~ "JULIET SABLOSKY",
                                     str_detect(name, "KATE LEBUA|KATIE LEUBA") ~ "KATE LEUBA",
                                     str_detect(name, "KATE MALONEY") ~ "KATE MALONE",
                                     str_detect(name, "KATE NOEL WINTER|KATIE WINTER") ~ "KATE WINTER",
                                     str_detect(name, "KATHLEEN$") ~ "KATHLEEN ?",
                                     str_detect(name, "KATHLEEN L CALCERANO") ~ "KATHLEEN CALCERANO",
                                     str_detect(name, "KATHY O'NIEL|KATHY ONIELL") ~ "KATHY O'NIELL",
                                     str_detect(name, "KATHYRN CHIASSON") ~ "KATHRYN CHIASSON",
                                     str_detect(name, "KATIE FIN PALMER|KATE FINN PALMER") ~ "KATIE PALMER FINN",
                                     str_detect(name, "KATRINA CHESSMEN") ~ "KATRINA CHEESMAN",
                                     str_detect(name, "KEHAN$|KEHAN DESOUSSA|KEHAN DESOUZA|KEHAN DESUZA") ~ "KEHAN DESOUSA",
                                     str_detect(name, "KELLY E\\. GRIFFIN") ~ "KELLY GRIFFIN",
                                     str_detect(name, "KEVAN VANCE") ~ "KEVEN VANCE",
                                     str_detect(name, "KIRSTEN BAUER KIRSTENB@USC.EDU") ~ "KIRSTEN BAUER",
                                     str_detect(name, "KIRSTEN SCHMITZ|KIKI SCHMITZ|KIRSTEN KIKI SCHMITZ") ~ "KIRSTEN (KIKI) SCHMITZ",
                                     str_detect(name, "KRISTEN SYZNAL") ~ "KRISTEN SZYNAL",
                                     str_detect(name, "LALRINPARI SAILO") ~ "LALRIN SAILO",
                                     str_detect(name, "LANNING MOLDAUER") ~ "LANNY MOLDAUER",
                                     str_detect(name, "LARA E$") ~ "LARA EHRENHOFER",
                                     str_detect(name, "LARA FREDERICKSON") ~ "LARA FREDRICKSON",
                                     str_detect(name, "LARRISA KWENDE") ~ "LARISSA KWENDE",
                                     str_detect(name, "LARRY$") ~ "LARRY ?",
                                     str_detect(name, "LARRY RAUCH") ~ "LARRY RAUSCH",
                                     str_detect(name, "LARUEN MOSTELLER	") ~ "LAUREN MOSTELLER	",
                                     str_detect(name, "LAURA$") ~ "LAURA MAKL",
                                     str_detect(name, "LAURA IGLESAIS|LAURA IGLESIA") ~ "LAURA IGLESIAS",
                                     str_detect(name, "LAUREN COLLETI|LAUREN C\\.") ~ "LAUREN COLLETTI",
                                     str_detect(name, "LAURENCE BEAZY") ~ "LAURENCE BEZY",
                                     str_detect(name, "AURIE SEDLMAYR-CUMMING") ~ "AURIE SEDLMAYR",
                                     str_detect(name, "LES MCBEE|LESLIE MCBEE") ~ "LESLIE (LES) MCBEE",
                                     str_detect(name, "LIEZL P\\. SCHEWE") ~ "LIEZL PEREZ",
                                     str_detect(name, "LILLIAN HANGAR") ~ "LILLIAN HANGER",
                                     str_detect(name, "LINCOLN H\\. GROVES") ~ "LINCOLN GROVES",
                                     str_detect(name, "LINDA R\\.") ~ "LINDA RUBENSTEIN",
                                     str_detect(name, "LINMENGYU ZHAO|LINMENGYU ZHOU") ~ "LINMENGYU ZHOU/ZHAO?",
                                     str_detect(name, "LIZ FIGIOLA|LIZ FIGLIOLA|LIZ FIGIOLIA|LIZ FIGIOLI") ~ "LIZ FIGIOLA",
                                     str_detect(name, "LIZ TISDALE|ELIZABETH TISDALE") ~ "ELIZABETH (LIZ) TISDALE",
                                     str_detect(name, "LORA GOLAN") ~ "LORA GOLANN",
                                     str_detect(name, "LOUELIN DWYER") ~ "LOU ELIN DWYER",
                                     str_detect(name, "LOUIS LTARESCU") ~ "LOUIS ALTARESCU",
                                     str_detect(name, "LOUISE MAILLET") ~ "LOUISE MAILLETT",
                                     str_detect(name, "LUCRETIA$") ~ "LUECRETIA MASON",
                                     str_detect(name, "LUZ MARINA LOPEZ SALAS") ~ "	LUZ MARINA LOPEZ",
                                     str_detect(name, "MARA POUTAIN") ~ "MARA POUNTAIN",
                                     str_detect(name, "MARGARET MCLEOD|MAGGIE MCLEOD") ~ "MARGARET (MAGGIE) MCLEOD",
                                     str_detect(name, "MARGO PEET") ~ "MARGOT PEET",
                                     str_detect(name, "MARIA GORGEOUS") ~ "MARIA GERGOUDIS",
                                     str_detect(name, "MARIE LE BLANC") ~ "	MARIE LEBLANC",
                                     str_detect(name, "MARK RIESENFIELD") ~ "MARK RIESENFELD",
                                     str_detect(name, "MARRY BRITT|MARRY BRITTINGHAM") ~ "MARY BRITTINGHAM",
                                     str_detect(name, "MARSHALL$") ~ "MARSHALL SCHREIER",
                                     str_detect(name, "MARY ALICE LAMBOLEY") ~ "MARY AGNES LAMBOLEY",
                                     str_detect(name, "MARY ELLEN$|MARY ELLEN GUERA") ~ "MARY ELLEN GUERRA",
                                     str_detect(name, "MARY FRAN$|MARY FRAN KIRSCHNER") ~ "MARY FRAN KIRCHNER",
                                     str_detect(name, "MARY GAWRONKSI|MARY GOWRONSKI") ~ "MARY GAWRONSKI",
                                     str_detect(name, "MARY JANICE$") ~ "MARY JANICE DIO",
                                     str_detect(name, "MARY TRINH WEDENSDAY") ~ "MARY TRINH",
                                     str_detect(name, "MARYJO BAXTOR") ~ "MARYJO BAXTER",
                                     str_detect(name, "MATHA COHEN") ~ "MARTHA COHEN",
                                     str_detect(name, "MATTHEW GREENSPUN|MATT GREENSPUN	") ~ "MATTHEW (MATT) GREENSPUN",
                                     str_detect(name, "MATTHEW LA COTE & PRIYA KVAM|MATTHEW LA CORTE") ~ "MATTHEW LACORTE",
                                     str_detect(name, "MATTHEW RHODAS|MATT RHOADES|MATTHEW RHOADES") ~ "MATTHEW (MATT) RHOADES",
                                     str_detect(name, "MAUREEN HENRICKSON|MAUREEN HERIKSON") ~ "MAUREEN HENRIKSON",
                                     str_detect(name, "MAYA SMITH WED") ~ "MAYA SMITH",
                                     str_detect(name, "MEINRADO SAMALA") ~ "MEIN SAMALA",
                                     str_detect(name, "MERILEE JANSSENN|MERILEE JANSEN|MERILEE JANSON") ~ "MERILEE JANSSEN",
                                     str_detect(name, "MICHEL PAGAN") ~ "MICHELE PAGAN",
                                     str_detect(name, "	MICHELLE$|MICHELLE GARAFALO") ~ "	MICHELLE GARAFAL",
                                     str_detect(name, "MICHELLE MCNAMARA") ~ "MICHELE MCNAMARA",
                                     str_detect(name, "MICHELLE RODRIQUEZ") ~ "MICHELLE RODRIGUEZ",
                                     str_detect(name, "MICHIEL DEPOOTER") ~ "MICHIEL DE POOTER",
                                     str_detect(name, "MIKE PENA") ~ "MIKE PINA",
                                     str_detect(name, "MINERVA MACNAB|MINI MACNAB") ~ "MINERVA (MINI) MACNAB",
                                     str_detect(name, "MIROSLAVA MIMROVA") ~ "MIRKA MIMROVA	",
                                     str_detect(name, "MITHILA SAMAK ") ~ "MITHILA SAMAK",
                                     str_detect(name, "MONA$") ~ "MONA BIN SAMIH",
                                     str_detect(name, "MONICA BOROKOWSKI") ~ "MONICA BORKOWSKI",
                                     str_detect(name, "MONIKA BROZNYA") ~ "MONIKA BROZYNA",
                                     str_detect(name, "MUGABY BYENKYA") ~ "MUGABI BYENKYA",
                                     str_detect(name, "MUSTAFABAHAR@GMAIL.COM") ~ "BAHAR MUSTAFA",
                                     str_detect(name, "NANCY ERICKSON WED 6-7PM") ~ "NANCY ERICKSON",
                                     str_detect(name, "NATALIA ERMICIOI\\s+") ~ "NATALIA ERMICIOI",
                                     str_detect(name, "NATHALIE KIRBY|NATHALIE KERBY") ~ "NATHALIE KIRBY/KERBY?",
                                     str_detect(name, "NATHAN GREEN") ~ "NATE GREEN",
                                     str_detect(name, "NATHAN HIRSCH|NATE HIRSCH") ~ "NATHAN (NATE) HIRSCH",
                                     str_detect(name, "NATHAN YOUNGBLOOD|NATE YOUNGBLOOD") ~ "NATHAN (NATE) YOUNGBLOOD",
                                     str_detect(name, "NATLIE BARBIERI") ~ "NATALIE BARBIERI",
                                     str_detect(name, "NICK BRITTEN|NICHOLAS BRITTEN") ~ "NICHOLAS (NICK) BRITTEN",
                                     str_detect(name, "NICOLAS ARNAL") ~ "NICOLÁS ARNAL",
                                     str_detect(name, "OBI NNEBUNDO WEDENSDAY 1:00 PM - 2:00 PM") ~ "OBI NNEBUNDO",
                                     str_detect(name, "OSCAR QUINONES") ~ "OSCAR QUIÑONES",
                                     str_detect(name, "PAMELA DAVIS|PAM DAVIS") ~ "PAMELA (PAM) DAVIS",
                                     str_detect(name, "PAMELA ROBERTS$") ~ "PAMELA ROBERTS MEJIA",
                                     str_detect(name, "PAOLA$") ~ "PAOLA PASCUAL-FERRA",
                                     str_detect(name, "PARAMITA SEN") ~ "PARAMITRA SEN",
                                     str_detect(name, "PATRICIA COLL|PAT COLL") ~ "PATRICIA (PAT) COLL",
                                     str_detect(name, "PATRICIA NYHAN|PAT NYHAN") ~ "PATRICIA (PAT) NYHAN",
                                     str_detect(name, "PATRICIA YVONNE WED") ~ "PATRICIA YVONNE",
                                     str_detect(name, "PATRICK GARCIA|PAT GARCIA") ~ "PATRICK (PAT) GARCIA",
                                     str_detect(name, "PAT URDA") ~ "PATRICK URDA",
                                     str_detect(name, "PAUL STRUM") ~ "PAUL STURM",
                                     str_detect(name, "PAULA BILLINGSLY") ~ "PAULA BILLINGSLEY",
                                     str_detect(name, "PAYAL VACCHANI") ~ "PAYAL VACHHANI",
                                     str_detect(name, "PHILLIP ZHENG") ~ "PHILIP ZHENG",
                                     str_detect(name, "PHILOMENA OJEDA|PENNIE OJEDA|PENNY OJEDA") ~ "PHILOMENA (PENNIE) OJEDA",
                                     str_detect(name, "R SHAPIRO") ~ "ROBERTA SHAPIRO",
                                     str_detect(name, "RACHEL CENTARICZIKI") ~ "RACHEL CENTARICZIKI",
                                     str_detect(name, "RACHEL SCHLINDER") ~ "RACHEL SCHINDLER",
                                     str_detect(name, "RANDI RUBOVITS-SEITZ") ~ "RANDI RUBOVITS",
                                     str_detect(name, "RAQUEL SANTA CRUZ|RAQUEL SANTANA DA CRUZ") ~ "RAQUEL SANTA CRUZ/SANTANA DA CRUZ?",
                                     str_detect(name, "REBECCA EARNST") ~ "REBECCA ERNEST",
                                     str_detect(name, "REBECCA OLIVEIRA ESTEVES") ~ "REBECCA ESTEVES",
                                     str_detect(name, "BECKY BURKE|REBECCA BURKE") ~ "REBECCA (BECKY) BURKE",
                                     str_detect(name, "RENE SERVIN") ~ "RENÈ SERVIN",
                                     str_detect(name, "RHIANNON BLIM") ~ "RHIANNON BLIIM",
                                     str_detect(name, "RHONDA M\\. CARTER|RHONDA MARIE CARTER") ~ "RHONDA CARTER",
                                     str_detect(name, "RICARDO R\\. CASTANO|RICARDO RESTREPO CASTANO|RICARDO RESTREPO-CASTANO") ~ "RICARDO CASTAÑO",
                                     str_detect(name, "RISHAN G HABTE") ~ "RISHAN HABTE",
                                     str_detect(name, "ROB LANDINIO") ~ "ROB LANDINO",
                                     str_detect(name, "ROBERT MCGOVERN|BOB MCGOVERN") ~ "ROBERT (BOB) MCGOVERN",
                                     str_detect(name, "ROBERTA SHAPRIO") ~ "ROBERTA SHAPIRO",
                                     str_detect(name, "ROBIN HANERFIELD") ~ "ROBIN HANERFELD",
                                     str_detect(name, "RON CASTALDI-") ~ "RON CASTALDI",
                                     str_detect(name, "ROXANNE OROXAM") ~ "ROXANNE OROX0M",
                                     str_detect(name, "RUTH KZBAUER") ~ "RUTH KURZBAUER",
                                     str_detect(name, "RYAN STASNY|RYAN STASRY") ~ "RYAN STASTNY",
                                     str_detect(name, "SALLY VON SUMMERS") ~ "SALLY VON SUMMER",
                                     str_detect(name, "SANDRA CUMMINGHAM") ~ "SANDRA CUNNINGHAM",
                                     str_detect(name, "SARAH$") ~ "SARAH ?",
                                     str_detect(name, "SARAH BOITTEN") ~ "SARAH BOITTIN",
                                     str_detect(name, "SARAH SHAPRIO") ~ "SARAH SHAPIRO",
                                     str_detect(name, "SARAH TRACY") ~ "SARAH TRACEY",
                                     str_detect(name, "SHANHAI JARRET") ~ "SHANHAI JARRETT",
                                     str_detect(name, "SHARRON CANNISTRA") ~ "SHARON CANNISTRA",
                                     str_detect(name, "SHIELA KAPLAN") ~ "SHEILA KAPLAN",
                                     str_detect(name, "SHIRLEY$") ~ "SHIRLEY TO?",
                                     str_detect(name, "STATON PROVENCE") ~ "STANTON PROVENCE",
                                     str_detect(name, "STEPHEN FUMA") ~ "STEFAN FUMA",
                                     str_detect(name, "STEPHEN SEIDEL|STEVE SEIDEL|STEVE SIEDEL") ~ "STEPHEN (STEVE) SEIDEL",
                                     str_detect(name, "STEPHEN GIBSON|STEVE GIBSON") ~ "STEPHEN (STEVE) GIBSON",
                                     str_detect(name, "STEPHEN SKUBEL|STEVE SKUBEL") ~ "STEPHEN (STEVE) SKUBEL",
                                     str_detect(name, "STEVE SWETT|STEPHEN SWETT") ~ "STEPHEN (STEVE) SWETT",
                                     str_detect(name, "SUE COHEN") ~ "SUE COHN",
                                     str_detect(name, "SUE ELEMENDORF|SUSAN ELMENDORF|SUE ELMENDORF") ~ "SUSAN (SUE) ELMENDORF",
                                     str_detect(name, "SUE HASTEN|SUSAN HASTAN") ~ "SUSAN (SUE) HASTEN",
                                     str_detect(name, "SUSAN HATTAN|SUE HATTAN|SUSAN K\\. HATTAN") ~ "SUSAN (SUE) HATTAN",
                                     str_detect(name, "SUSAN JOSEPH|SUSAN JOSEPHS") ~ "SUSAN JOSEF",
                                     str_detect(name, "SUSAN RAUCH") ~ "SUSAN RAUSCH",
                                     str_detect(name, "SUSAN SCHNIEDER") ~ "SUSAN SCHNEIDER",
                                     str_detect(name, "SUSAN MEDINA-DAY|SUSANA MEDINA DAY") ~ "SUSANA MEDINA-DAY",
                                     str_detect(name, "SUSIE ZIMMERMAN") ~ "SUSIE ZIMMERMANN",
                                     str_detect(name, "SUZANNE MILLER-MCFEELEY") ~ "SUZANNE MILLER",
                                     str_detect(name, "SUZANNE STEVENS") ~ "SUZANNE STEPHENS",
                                     str_detect(name, "TAESA MON") ~ "TAESA SOLOMON",
                                     str_detect(name, "TERE MARTINEZ VERGNE") ~ "TERE MARTINEZ",
                                     str_detect(name, "TERI MCRAE|TERI MCRAE \\(BREZNER\\)|TERI MCRAE BREZNER") ~ "TERI MCRAE-BREZNER",
                                     str_detect(name, "TERWASETOR-AGBIDYE") ~ "TERWASE TOR-AGBIDYE",
                                     str_detect(name, "THOMAS FLANAGAN|TOMMY FLANAGAN") ~ "THOMAS (THOMMY) FLANAGAN",
                                     str_detect(name, "TIMOTHY AGUIRRE|TIM AGUIRRE") ~ "TIMOTHY (TIM) AGUIRRE",
                                     str_detect(name, "TIMOTHY J\\. DAY") ~ "TIM DAY",
                                     str_detect(name, "TJ$") ~ "TODD PIANTEDOSI",
                                     str_detect(name, "THOMAS HUTCHESON|TOM HUTCHESON") ~ "THOMAS (TOM) HUTCHESON",
                                     str_detect(name, "THOMAS STURMA|TOM STURMA") ~ "THOMAS (TOM) STURMA",
                                     str_detect(name, "TONI LEWIS|TONISHA LEWIS") ~ "TONISHA (TONI) LEWIS",
                                     str_detect(name, "TRAVIS BAILY") ~ "TRAVIS BAILEY",
                                     str_detect(name, "UNA HILDEBRANT") ~ "UNA HILDEBRANDT",
                                     str_detect(name, "URSULA FOX-KOOR") ~ "URSULA FOX KOOR",
                                     str_detect(name, "VERENICE PAUCER") ~ "VERENICE PAUCAR",
                                     str_detect(name, "VERONICA TRIMBLAY") ~ "VERONICA TREMBLAY",
                                     str_detect(name, "VIVIANE BENCIC") ~ "VIVIANE BENCIE",
                                     str_detect(name, "VRINDA GUPTA YOST") ~ "VRINDA GUPTA",
                                     str_detect(name, "WILLIAM KLAY KLAY") ~ "	WILLIAM KLAY",
                                     str_detect(name, "WILLIAM G MATCHIN") ~ "WILLIAM MATCHIN",
                                     str_detect(name, "ZACH LUCK ET AL") ~ "ZACH LUCK",
                                     str_detect(name, "ZENAN \\\"VERA\\\" SONG") ~ "ZENAN (VERA) SONG",
                                     str_detect(name, "ZERSHA MUNIR|ZERSHA MURNIR") ~ "ZERSHA MUNIR/MURNIR?",
                                     str_detect(email, "court\\.schett@gmail\\.com") ~ "COURT SCHETT",
                                     TRUE ~ name)
                    )
#----------------------
#CY: 4592/4943 (improved by 7%)
#FY: 4446/4790 (improved by 7%)
unique_volunteers <- df %>% distinct(name)
nrow(unique_volunteers)


#For each year
#get all unique volunteers
get_retention_rates <- function(dataframe) {
  
  for (i in c(2007:2019)) {
    #i = 2007
    
    df_vols <- dataframe %>% filter(year == i) %>% distinct(name) 
    df_vols_next <- dataframe %>% filter(year == i+1) %>% distinct(name) 
    
    #Get volunteers that went into next cy
    df_multi_year_vols <- df_vols$name[df_vols$name %in% df_vols_next$name]
    
    retention_stat <- round((length(df_multi_year_vols) / nrow(df_vols)) * 100, 0)
    
    if (i == 2007) {
      
      df_base <- tibble(year = i,
                        year_range = glue::glue("{i}-{i+1}"), 
                        year_one_num_vols = nrow(df_vols),
                        year_one_num_vols_in_year_two = length(df_multi_year_vols),
                        retention_rate = retention_stat)
    }
    
    else {
      
      df_retention_row <- tibble(year = i,
                                 year_range = glue::glue("{i}-{i+1}"), 
                                 year_one_num_vols = nrow(df_vols),
                                 year_one_num_vols_in_year_two = length(df_multi_year_vols),
                                 retention_rate = retention_stat)
      
      df_base <- bind_rows(df_base, df_retention_row)
    }
  }
  return(df_base)
}

df_fy_retention <- get_retention_rates(df)

df_five_year_rate_row <- df_fy_retention %>% filter(year >= 2017) %>% summarize(across(3:5, mean)) %>% 
              add_column(year = NA, .before = "year_one_num_vols") %>%
              add_column(year_range = "2017-2020 Average Retention:", .before = "year_one_num_vols") %>%
              mutate(across(3:5, ~round(., 0))) %>%
              mutate(retention_rate = paste0(retention_rate, "%"))

df_overall_rate_row <- df_fy_retention %>% summarize(across(3:5, mean)) %>%
              add_column(year = NA, .before = "year_one_num_vols") %>%
              add_column(year_range = "2007-2020 Average Retention:", .before = "year_one_num_vols") %>%
              mutate(across(3:5, ~round(., 0))) %>%
              mutate(retention_rate = paste0(retention_rate, "%"))

df_fy_retention <- df_fy_retention %>% mutate(retention_rate = paste0(retention_rate, "%"))

#---------------------------------------
#         DELIVERABLE DATA SET #1
#---------------------------------------
df_fy_retention <- bind_rows(df_fy_retention, df_five_year_rate_row, df_overall_rate_row) %>% select(-c(year)) %>%
                            rename(`Year Range` = year_range,
                                   
                                   )


# Establish a baseline for the average # of terms volunteers serve within one cy
get_avg_terms_per_fy <- function(dataframe){

  for (i in c(2007:2020)) {
    #i = 2007
    
    for (j in c("WINTER", "SPRING", "SUMMER", "FALL")) {
      #j = "WINTER"
      
      df_sem <- dataframe %>% filter(year == i & semester == j) %>% distinct(year, semester, name)
      
      if ((i == 2007 & j == "WINTER")){
        
        df_final <- df_sem
      }
      else {
        
        df_final <- bind_rows(df_final, df_sem)
      }
    }
  }
  
  #Aggregate up to the year level in terms of frequency 
  df_final <- df_final %>% group_by(year, name) %>% summarize(n = n())
  return(df_final) 
}   
    
df_avg_granular <- get_avg_terms_per_fy(df) 
    

#So, OF the individuals who volunteered during a year, this is the average

#For each year, the average number of terms
df_avg_fy <- df_avg_granular %>% group_by(year) %>% summarize(avg_term = round(mean(n), 1), 
                                                              median_term = median(n),
                                                              max_term = max(n)) %>%
                                                     mutate(year = as.character(year))


df_avg_all <- df_avg_fy %>% summarize(avg_term = round(mean(avg_term), 1)) %>% 
                    add_column(year = "2007-2020 Average Number of Terms") %>%
                    add_column(median_term = NA) %>%
                    add_column(max_term = NA)


#---------------------------------------
#       DELIVERABLE DATA SET #2
#---------------------------------------
df_avg_fy <- bind_rows(df_avg_fy, df_avg_all) %>%
               rename(`Fiscal Year`= year, 
                      `Average Number of Terms Served by Volunteers` = avg_term,
                      `Median Number of Terms Served by Volunteers` = median_term,
                      `Max Number of Terms Served by Volunteers` = max_term)


#HISTOGRAM OVER TIME - AREA CHART IS MORE EFFECTIVE
#-------------------------------------------------
# B <- ggplot(data=df_avg_granular, aes(x=n)) + 
#         geom_histogram(aes(y = ..density..), bins = 4, color='black', fill='lightblue') +
#         facet_grid(.~year) + #semester~year to get every case
#         scale_y_continuous(breaks = seq(.1, .9, by=.1), 
#                            labels = scales::percent_format(accuracy = 1L),
#                            expand = c(0,0)) +
# 
#         labs(title="Distribution of the Number of Terms Volunteers Serve per cy", x="Number of Terms Served", y="Percentage of Volunteers Who Served During one cy") +
#         theme(axis.title.y = element_text(margin = unit(c(0,4,0,0), "mm")),
#               axis.ticks.x=element_blank(),
#               panel.spacing = unit(.6, "lines"))
# 
# B
#-------------------------------------------------

#AREA CHART OVER TIME
#--------------------------------------------------
df_area_1 <- df_avg_granular %>% mutate(num_terms = factor(as.character(n), 
                                                       levels = c("1","2", "3", "4"))) %>% 
                                 group_by(year, num_terms, .drop = FALSE) %>% summarize(num_volunteers = n())
                              
df_area_2 <- df_area_1 %>% group_by(year) %>% summarize(total_volunteers = sum(num_volunteers))

df_area <- left_join(df_area_1, df_area_2, by=c("year")) %>% mutate(percent_vols = (num_volunteers / total_volunteers))

df_area_graph <- df_area %>% mutate(num_terms = factor(as.character(num_terms), levels = c("4", "3", "2", "1"))
                                    )

#---------------------------------------
#       DELIVERABLE DATA SET #3
#---------------------------------------
df_area_print <- df_area %>% mutate(percent_vols = paste0(round(percent_vols, 3) * 100, "%")) %>%
                             rename(`FY`= year,
                                    `Number of Terms Served in one FY` = num_terms,
                                    `Number of Volunteers Who Served This Number of Terms` = num_volunteers,
                                    `Total Number of Volunteers in FY` = total_volunteers, 
                                    `Percent of All Volunteers Who Served at Least Once During FY` = percent_vols)

#GRAPH CODE
C <- ggplot(data=df_area_graph, aes(x=year, y=percent_vols, fill=num_terms)) + 
       geom_area(alpha=.65) +
        scale_y_continuous(breaks = seq(.1, 1, by=.1),
                           labels = scales::percent_format(accuracy = 1L),
                           expand = c(0,0)) +
  
       scale_x_continuous(breaks = c(seq(2007, 2020, by=2), 2020)) +
       scale_fill_viridis(discrete = TRUE) +

        labs(title="Distribution of the Number of Terms Volunteers Serve per FY", 
             x="Fiscal Year", 
             y="% Volunteers Who Served During One FY",
             fill="Number of Terms") +
        theme(axis.title.y = element_text(margin = unit(c(0,4,0,0), "mm")),
              axis.ticks.x= element_blank(),
              axis.text.x = element_text(vjust = .75, size = 10, angle = 30),
              panel.spacing = unit(.6, "lines"))


  
C

list_of_tables <- list("Retention" = df_fy_retention,
                       "Average # Terms" = df_avg_fy,
                       "Distribution by # Terms" = df_area_print)
write.xlsx(list_of_tables, file = "./Output/WEC Volunteer Retention & Average Service: FY2007-FY2020.xlsx", row.names = FALSE)




