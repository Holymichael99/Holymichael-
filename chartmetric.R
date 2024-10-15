#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(factoextra)
library(FactoMineR)
library(cluster)
library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rio)
library(plotly)
library(stringr)
library(SmartEDA)
library(explore)
library(ggraph)
library(igraph)





MusicFolder_path <- "C:/Users/USER/Downloads/music for Africa countries"
# Get a list of all Excel files in the directory
MusicFile_list <- list.files(path = MusicFolder_path, pattern = "*.xlsx", full.names = TRUE)

# Create an empty list to store the imported data
Music_list <- list()

# Loop through the files and import them one by one, using their file names as the list keys
for (file_path in MusicFile_list) {
  # Extract the file name without the path or extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Import the Excel file
  Musicdata <- import(file_path)
  
  # Add the imported data to the list with the file name as the list key
  Music_list[[file_name]] <- Musicdata
}

Angola <- Music_list$`ANGOLA CHATMETRIC`
Benin <- Music_list$`BENIN CHART METRICS`
Burkina_faso <- Music_list$`BURKINA FASO CHARTMETRIC`
Burundi <- Music_list$`BURUNDI CHARTMETRIC`
Cape_verde <- Music_list$`CAPE VERDE CHAERTMETRICS`
Central_Africa_Republic <- Music_list$`CENTRAL AFRICA REP`
Chad <- Music_list$`CHAD CHARTMETRIC`
Comoros <- Music_list$COMOROS.CHARTMETRIC
Cote_di_voire <- Music_list$`COTE D'IVOIRE CHARTMETRIC`
Djibouti <- Music_list$`DJBOUTI CHATMETRIC`
Dr_Congo <- Music_list$`DR CONGO CHARTMETRICS`
Egypt <- Music_list$`EGYPT CHARTMETRIC`
Equitorial_Guinea <- Music_list$`EQUITORIAL GUINEA CHARTMETRIC`
Gabon <- Music_list$`GABON CHATMETRIC`
Gambia <- Music_list$`GANBIA CHATMETRIC`
Ghana <- Music_list$`GHANA CHARTMETRIC`
Guinea_Bissau <- Music_list$`GUINEA BISSAU CHATMETRIC`
Kenya <- Music_list$KENYA.CHARTMETRIC
Lesotho <- Music_list$`LESOTHO CHARTMETRIC`
Liberia <- Music_list$LIBERIA.CHARTMETRIC
Lybia <- Music_list$`LIBYA CHARTMETRICS`
Madagascar <- Music_list$`MADAGASCAR CHATMETRIC`
Malawi <- Music_list$`MALAWI CHATMETRIC`
Mali <- Music_list$`MALI CHARTMETRIC`
Morocco <- Music_list$`MOROCCO CHARTMETRICS`
Mauritania <- Music_list$`MAURITANIA CHARTMETRICS`
Mozambique <- Music_list$MOZAMBIQUE.CHARTMETRIC
Niger <- Music_list$`NIGER CHARTMETRICS`
Namibia <- Music_list$`NAMIBIA CHARTMETRICS`
Nigeria <- Music_list$`NIGERIA CHARTMETRIC`
Sao_Tome_Principe <- Music_list$`SAO TOME CHARTMETRIC`
Senegal <- Music_list$`SENEGAL CHARTMETRICS`
Seychelles <- Music_list$`SEYCHELLES CHATMETRIC`
Somalia <- Music_list$`SOMALIA CHARTMETRIC`
South_Africa <- Music_list$`SOUTH-AFRICA`
Swaziland <- Music_list$`SWAZILAND CHARTMETRIC`
Togo <- Music_list$TOGO.CHARTMETRIC
Tunisia <- Music_list$`TUNISIA CHATMETRIC`
Uganda <- Music_list$`UGANDA CHARTMETRICS`
Zambia <- Music_list$`ZAMBIA CHATMETRIC`


CountryMusic <- rbind(Angola, Benin, Burkina_faso, Burundi,Cape_verde,Central_Africa_Republic,
                      Chad, Comoros, Cote_di_voire, Djibouti, Dr_Congo, Egypt, Equitorial_Guinea, Gabon,
                      Gambia, Ghana, Guinea_Bissau, Kenya, Lesotho, Liberia, Lybia, Madagascar, Malawi,
                      Mali, Mauritania, Morocco, Mozambique, Namibia, Niger, Nigeria, Sao_Tome_Principe,
                      Senegal, Seychelles, Somalia, South_Africa, Swaziland, Togo, Tunisia, Uganda, Zambia)

CountryMusic$ARTISTS_COUNTRY <- CountryMusic$`ARTISTS COUNTRY`
CountryMusic$LANGUAGE_OF_THE_SONG <- CountryMusic$`LANGUAGE OF THE SONG`
CountryMusic$WEEKLY_VIEWS <- CountryMusic$`WEEKLY VIEWS`
CountryMusic <- CountryMusic%>%
  select(-c(`ARTISTS COUNTRY`,`LANGUAGE OF THE SONG`,`WEEKLY VIEWS`))
MusicClusteringWork <- readxl::read_xlsx("C:/Users/USER/Downloads/creativeDOC-20231216-WA0038.xlsx")
MusicClusteringWork <- MusicClusteringWork%>%
  separate(`OTHER LANGUAGES`,into = c( "OTHER_LANGUAGE1", "OTHER_LANGUAGE2", "OTHERLANGUAGE3"),sep=",")

lang <- CountryMusic%>%
  group_by(COUNTRY)%>%
  count(Song_Language=LANGUAGE_OF_THE_SONG)%>%
  arrange(COUNTRY)
lang_wider <- pivot_wider(lang, names_from = Song_Language, values_from = n)
Artist_country <- CountryMusic%>%
  group_by(COUNTRY)%>%
  count(ArtistCountry =ARTISTS_COUNTRY)%>%
  arrange(COUNTRY)
Artist_countryWider <-pivot_wider(Artist_country, names_from = ArtistCountry, values_from = n)%>%
  select(-COUNTRY) 
genreW <- CountryMusic%>%
  group_by(COUNTRY)%>%
  count(Genre =GENRE)%>%
  arrange(COUNTRY)
GenreWider <-pivot_wider(genreW, names_from = Genre, values_from = n)%>%
  select(-COUNTRY)

songs <- CountryMusic%>%
  group_by(COUNTRY, SONGS)%>%
  summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))%>%
  arrange(COUNTRY)
SongsWider <-pivot_wider(songs, names_from = SONGS, values_from = views)%>%
  select(-COUNTRY)

unique(MusicClusteringWork$OTHER_LANGUAGE1)
MusicClusteringWork_longer <- pivot_longer(MusicClusteringWork,cols = c(OTHER_LANGUAGE1,OTHER_LANGUAGE2,OTHERLANGUAGE3),
                                           names_to = "OtherLanguages", values_to = "Language")
langua <- MusicClusteringWork_longer%>%
  group_by(COUNTRY)%>%
  count(Language)%>%
  arrange(COUNTRY)
languaWider <-pivot_wider(langua, names_from = Language, values_from = n)%>%
  select(-COUNTRY)

languaOf <- MusicClusteringWork%>%
  group_by(COUNTRY)%>%
  count(LinguaFranca=`LINGUA FRANCA`)%>%
  arrange(COUNTRY)
languaOFWider <-pivot_wider(languaOf, names_from = LinguaFranca, values_from = n)%>%
  select(-COUNTRY)

Region<- MusicClusteringWork%>%
  group_by(COUNTRY)%>%
  count(REGION)%>%
  arrange(COUNTRY)
RegionWider <-pivot_wider(Region, names_from = REGION, values_from = n)%>%
  select(-COUNTRY)
MusicClusteringWork <- MusicClusteringWork%>%
  arrange(COUNTRY)

MusicClustering <- cbind(MusicClusteringWork,Artist_countryWider,GenreWider,lang_wider,
                         languaOFWider,languaWider,RegionWider,SongsWider)
names(MusicClustering)
rownames(MusicClustering) <- MusicClustering$COUNTRY
names(MusicClustering) <- make.unique(names(MusicClustering))
MusicClustering <- MusicClustering %>%
  select_if(is.numeric)
MusicClustering <- MusicClustering%>%
  replace(is.na(.),0)


music.res <- PCA(MusicClustering,scale.unit = TRUE, ncp = 10)
eigen_values <- get_eigenvalue(music.res)
fviz_eig(music.res, addlabels = TRUE,ylim=c(0,50))
var <- get_pca_var(music.res)
var$coord
var$cor
var$cos2
var$contrib
fviz_pca_var(music.res,col.var = "red")
library("corrplot")
corrplot(var$cos2, is.corr = TRUE)
fviz_cos2(music.res, choice = "var", axes = 1:10)
fviz_cos2(music.res, choice = "var", axes = 1, top = 400)
fviz_cos2(music.res, choice = "var", axes = 2, top = 400)

fviz_pca_var(music.res, col.var = "cos2",
             gradient.cols = c("green", "blue", "red"),
             repel = TRUE # Avoid text overlapping
)

fviz_pca_var(music.res,alpha.var = "cos2")
var$contrib
corrplot(var$contrib, is.corr = FALSE)
fviz_contrib(music.res, choice = "var", axes = 1, top = 10)
fviz_contrib(music.res, choice = "var", axes = 2, top = 10)
fviz_contrib(music.res, choice = "var", axes = 3, top = 10)
fviz_contrib(music.res, choice = "var", axes = 4, top = 10)
fviz_contrib(music.res, choice = "var", axes = 5, top = 10)
fviz_contrib(music.res, choice = "var", axes = 1:2,  top = 10)
fviz_pca_var(music.res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "blue", "brown")
)
fviz_nbclust(MusicClustering,kmeans,method = "wss", k.max = 30)
fviz_nbclust(MusicClustering, clara ,method = "silhouette", k.max = 30)

df_dist45 <- scale(MusicClustering)
df_dist1 <- get_dist(df_dist45, method = "manhattan")
df_dist1 <- as.matrix(df_dist1)
ggplotly(fviz_nbclust(MusicClustering,FUNcluster =clara, method = "silhouette", k.max = 20))
ggplotly(fviz_nbclust(MusicClustering, clara, method = "wss", k.max = 30))
res_music <- clara(df_dist1,8,metric = "manhattan", stand = TRUE)
fviz_cluster(res_music, data = MusicClustering,
             palette = c("jco"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
) + theme(panel.background = element_rect(fill = "darkblue"))+theme_classic()
res_music$i.med
medi <- MusicClustering[c(11, 13, 23, 12, 19, 33, 39, 37),]
aggregate(df_dist1, by= list(cluster = res_music$cluster), mean)
dc <- cbind( MusicClustering, cluster= res_music$cluster)
dc%>%arrange(cluster)
df_dist2 <- get_dist(df_dist45, method = "spearman")
df_dist2 <- as.matrix(df_dist2)
ggplotly(fviz_nbclust(df_dist2, clara, method = "silhouette", k.max = 20))
ggplotly(fviz_nbclust(df_dist2, clara, method = "wss", k.max = 20))
res_music1<- clara(df_dist2,13, metric="manhattan", stand = TRUE)
fviz_cluster(res_music1, data = MusicClustering,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
) + theme(panel.background = element_rect(fill = "darkblue"))+theme_classic()
res_music1$i.med
MusicClustering[c(1,  3 , 8, 10,  6, 14, 24, 32, 16, 23, 35, 21, 27),]
df_dist45[c(1,  3 , 8, 10,  6, 14, 24, 32, 16, 23, 35, 21, 27),]
aggregate(df_dist2, by= list(cluster = res_music1$cluster), mean)
dc1 <- cbind( MusicClustering, cluster= res_music1$cluster)
dc1%>%arrange(cluster)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme= bslib::bs_theme(bootswatch = "flatly"),
 fluidRow(
  column(1, titlePanel(icon("music", class = "btn-lg btn-primary"))), 
  column(11, titlePanel("Melochain Africa"))),
tabsetPanel(
  tabPanel("Home",icon = icon("home", class = "btn-sm btn-primary")),
  tabPanel("General Exploration", icon = icon("globe", class = "btn-sm btn-primary"),
           fluidRow(
             column(4, selectInput("variables","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "COUNTRY" ),
                    selectInput("variables1","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "ARTISTS_COUNTRY" ),
                    submitButton("Get Result", icon = icon("check-square", class = "btn-sm btn-info") ), imageOutput("image")),
           column(8, plotlyOutput("plot", height= "4500px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text")), column(8, dataTableOutput("table"))
           ),
           fluidRow(
             column(4, selectInput("variables2","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "COUNTRY" ),
                    selectInput("variables3","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "ARTISTS_COUNTRY" ),
                    submitButton("view", icon = icon("check-square", class = "btn-sm btn-info") ), imageOutput("image1")),
             column(8, plotlyOutput("plot1", height= "4500px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text1")), column(8, dataTableOutput("table1"))
           ),
           fluidRow(
             column(4, selectInput("variables4","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "ARTISTS_COUNTRY" ),
                    submitButton("Visualize", icon = icon("check-square", class = "btn-sm btn-info") ), imageOutput("image2")),
             column(8, plotlyOutput("plot2", height= "4500px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text2")), column(8, dataTableOutput("table2"))
           ),
           fluidRow(
             column(4, selectInput("variables5","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "ARTISTS_COUNTRY" ),
                    submitButton("Explore", icon = icon("check-square", class = "btn-sm btn-info") ), imageOutput("image3")),
             column(8, plotlyOutput("plot3", height= "4500px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text3")), column(8, dataTableOutput("table3"))
           ),
           ),
  tabPanel("Country Exploration", icon = icon("flag", class = "btn-sm btn-primary"),
           fluidRow(
             column(4, selectInput("country","Select Country", choices = unique(CountryMusic$COUNTRY), selected = "NIGERIA" ),
                    selectInput("variables6","Select Variable Column", choices = names(CountryMusic)[-c(1,6,9)], selected = "ARTISTS_COUNTRY" ),
                    submitButton("View Analysis", icon = icon("check-square", class = "btn-sm btn-info") ), imageOutput("image4")),
           column(8, plotlyOutput("plot4", height= "800px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text4")), column(8, dataTableOutput("table4"))
           ),
           fluidRow(
             column(4, selectInput("variables7","Select Variable Column", choices = names(CountryMusic)[-c(1,6,9)], selected = "ARTISTS_COUNTRY" ),
                    submitButton("click to view", icon = icon("arrow-right", class = "btn-sm btn-info") ), imageOutput("image5")),
             column(8, plotlyOutput("plot5", height= "800px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text5")), column(8, dataTableOutput("table5"))
           ),
           fluidRow(
             column(4, selectInput("Artist_country","Select Artists Country", choices = unique(CountryMusic$ARTISTS_COUNTRY), selected = "NIGERIA" ),
                    selectInput("variables8","Select Variable Column", choices = names(CountryMusic)[-c(6,7,9)], selected = "ARTISTS" ),
                    submitButton("View Analysis", icon = icon("check-square", class = "btn-sm btn-info") ), imageOutput("image6")),
             column(8, plotlyOutput("plot6", height= "800px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text6")), column(8, dataTableOutput("table6"))
           ),
           fluidRow(
             column(4, selectInput("variables9","Select Variable Column", choices = names(CountryMusic)[-c(6,7,9)], selected = "ARTISTS" ),
                    submitButton("click to view", icon = icon("arrow-right", class = "btn-sm btn-info") ), imageOutput("image7")),
             column(8, plotlyOutput("plot7", height= "800px", width= "100%"))),
           fluidRow(
             column(4, textOutput("text7")), column(8, dataTableOutput("table7"))
           ),fluidRow(
             column(4, selectInput("variables10","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "SONGS" ),
                    selectInput("variables11","Select Variable Column", choices = names(CountryMusic)[-c(6,9)], selected = "ARTISTS_COUNTRY" ),
                    submitButton("View Analysis", icon = icon("check-square", class = "btn-sm btn-info") ), imageOutput("image8")),
             column(8, textOutput("text8"),textOutput("text8a"),textOutput("text8b"),textOutput("text8c"))
           ),
           fluidRow(plotOutput("plot8", height= "5000px", width= "100%"),
                    plotOutput("plot9", height= "5000px", width= "100%")),
           fluidRow(
             column(12, dataTableOutput("table_network_metrics"))
           ),
  ),
  tabPanel("Analysis", icon = icon("bar-chart",class = "btn-sm btn-info"),
           titlePanel("Segmentation of African Countries Based on Similarities and Musical Connectivity "),
           fluidRow(
             column(3, textOutput("static21")),
             column(9, dataTableOutput("dynamic21"))
           ),
           fluidRow(
             plotOutput("plot21", height = "1000px")
           ),
           fluidRow(
             column(6, verbatimTextOutput("summary21")),
             column(6, textOutput("static22"), verbatimTextOutput("summary22"),
                    verbatimTextOutput("summary23"), imageOutput("image3s"),textOutput("over"), imageOutput("image10s"))
           ),
           fluidRow(
             dataTableOutput("dynamic22")
             
           ),
           fluidRow(
             plotOutput("plot22", height="1000px")
           ),
           fluidRow(
             column(6, verbatimTextOutput("summary24")),
             column(6, textOutput("static23"), textOutput("static24"),imageOutput("image4s"), imageOutput("image5s"),
                    imageOutput("image6s"), imageOutput("image7s"), imageOutput("image8s"),imageOutput("image9s"))
           )
           
  )
  
) 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlotly({
    ggplotly(CountryMusic %>%
               group_by(!!sym(input$variables)) %>%
               count(!!sym(input$variables1)) %>%
               mutate(percentage = n / sum(n) * 100)%>%
               ggplot(aes_string(x = input$variables, y = "n", fill = input$variables1)) +
               geom_col(show.legend = TRUE) +
               ylab("count") +
               geom_text(aes(label = paste0(round(percentage, 1), "%, ")),position = position_stack(vjust = 0.5))+
               xlab("Variable of Interest") +
               coord_flip() +
               ggtitle("Exploring the count of a column in respect to another") +
               theme_classic() +
               theme(axis.text.x = element_text(angle = 90, hjust = 0))
    )
  })
  
  output$table <- renderDataTable({
    CountryMusic %>%
      group_by(!!sym(input$variables)) %>%
      count(!!sym(input$variables1))
  })
output$text <- renderText({
  "In this comprehensive analysis, our primary objective is to unravel the intricate connections that exist between various variables within our extensive dataset. The methodology we employ centers around the meticulous examination of the occurrences of a specific variable, as enumerated in the second column, in relation to the corresponding values present in the first column.

To elucidate this approach, let's take a concrete example. We will focus on scrutinizing the distribution of artist countries, a variable housed in the second column, concerning the countries listed in the first column. This examination is pivotal as it enables us to derive meaningful insights into the prevalence and distribution of artist countries within the ambit of different countries. The resulting data will cast a spotlight on the nuanced relationships and patterns that underlie the interaction between these two variables.

As we embark on this analytical journey, it's essential to recognize the inherent flexibility of our methodology. The adaptability of our approach is a key feature, allowing us to extend our investigation beyond the artist country-country relationship. We can seamlessly apply this analytical framework to explore and understand the interplay between any other pair of variables present in our expansive dataset.

The fundamental step in our analysis involves the systematic tallying of occurrences. For the case of artist countries, we meticulously count and document the instances where a particular artist country is associated with each unique country in the first column. This process unveils a quantitative representation of the prevalence of artist countries within the context of individual countries, providing a robust foundation for insights.

The richness of insights derived from this analysis is not confined to a mere numerical representation. Beyond the raw counts, we delve into the qualitative aspects that accompany these associations. Factors such as cultural influences, artistic collaborations, and global trends come into play, contributing to a holistic understanding of the intricate relationships between variables.

Furthermore, the adaptability of our approach extends beyond exploring the artist country-country relationship. We can seamlessly pivot to investigating other variable pairs within our dataset. Whether it's genre preferences, listener demographics, or any other variable at our disposal, the same analytical rigor can be applied, uncovering a tapestry of connections that collectively contribute to a comprehensive understanding of our dataset.

In essence, our analytical framework is not just a methodological tool; it's a gateway to unlocking the narrative embedded within our data. It's a journey that transcends mere statistical analysis, offering a profound exploration of the multifaceted relationships that define the landscape of our dataset. As we navigate through the intricate web of variables, we are not just counting occurrences; we are deciphering a story—one that is waiting to be told through the lens of rigorous analysis and insightful interpretation.
"
})
output$plot1 <- renderPlotly({
  ggplotly(CountryMusic%>%
             group_by(!!sym(input$variables2), !!sym(input$variables3))%>%
             summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))%>%
             mutate(percentage = views / sum(views) * 100)%>%
             ggplot(aes_string(x = input$variables2, y = "views", fill = input$variables3))+
             geom_col(show.legend =TRUE) +
             ylab("VIEWS") +
             geom_text(aes(label = paste0(round(percentage, 1), "%, ")),position = position_stack(vjust = 0.5))+
             xlab("Column of Interest") +
             coord_flip() +
             ggtitle("music penetration in terms of views")+
             theme_classic()+
             theme(axis.text.x = element_text(angle = 90, hjust = 0))
  )
})

output$table1 <- renderDataTable({
  CountryMusic%>%
    group_by(!!sym(input$variables2), !!sym(input$variables3))%>%
    summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))
})
output$text1 <- renderText({
  "In the realm of data analysis, our mission is to embark on a comprehensive exploration of music penetration, unraveling the intricate relationships woven within the fabric of our dataset. Our focal point lies in understanding the distribution and prevalence of artist countries, a variable nestled in the second column, in tandem with countries listed in the first column. Through a meticulous tallying process, we endeavor to shed light on the nuanced relationships between these variables, providing a panoramic view of music penetration across different regions.

Our analytical journey commences with a meticulous examination of the counts associated with artist countries within each unique country. This process is not merely a numerical exercise; rather, it serves as a gateway to a deeper comprehension of the prevalence of artists in diverse geographical contexts. As we unravel the numerical tapestry, patterns emerge, offering glimpses into the underlying dynamics that govern the global landscape of music.

At its core, this analysis transcends numerical counts; it is an exploration into the rich tapestry of cultural influences, artistic collaborations, and global trends that shape the distribution of artists. Beyond the raw numbers, we delve into the qualitative aspects that make this relationship inherently dynamic. Understanding the prevalence of artist countries within each country is not just about quantity; it's about deciphering the qualitative dimensions that contribute to the vibrant mosaic of the music industry.

Moreover, the versatility of our methodology stands out as a key feature. While we delve into the artist country-country relationship as a focal point, the same analytical rigor can seamlessly be applied to other variables within our dataset. This adaptability ensures that our exploration is not confined to a singular facet of music penetration; rather, it serves as a comprehensive lens through which we can scrutinize various dimensions of the music landscape.

Consider, for instance, the potential to extend this analysis to genres, listener demographics, or any other variable at our disposal. The same meticulous approach can uncover hidden patterns, offering a holistic understanding of the multifaceted layers that constitute music penetration. This versatility transforms our methodology into a dynamic tool, capable of unraveling diverse aspects of our dataset, contributing to a more profound comprehension of the music ecosystem.

In essence, what we embark upon is not just a numerical journey but a narrative exploration. As we navigate the expansive landscape of our dataset, we are deciphering a story—one that unfolds through the lens of rigorous analysis and insightful interpretation. The relationship between artist countries and countries is a chapter in this story, revealing not just counts but the intricate threads that weave together to create the vibrant tapestry of music penetration in our data.
"
})
output$plot2 <- renderPlotly({
  ggplotly(CountryMusic %>%
             count(!!sym(input$variables4)) %>%
             ggplot(aes_string(x = input$variables4, y = "n", fill = input$variables4)) +
             geom_col(show.legend = TRUE) +
             ylab("count") +
             xlab("Variable of Interest") +
             geom_text(aes(label = paste0(round(n / sum(n) * 100, 1), "%, ")),position = position_stack(vjust = 0.5))+
             coord_flip() +
             ggtitle("Exploring the count of a variable for all") +
             theme_classic() +
             theme(axis.text.x = element_text(angle = 90, hjust = 0))
  )
})
output$table2 <- renderDataTable({
  CountryMusic %>%
    count(!!sym(input$variables4))
})
output$text2 <- renderText({
  "In the intricate realm of data analysis, our expedition revolves around a meticulous exploration of the distribution of a pivotal variable within its own domain. A profound case study forms the nucleus of our inquiry, wherein we set our sights on unraveling the intricate tapestry of occurrences within the variable denoted as artist country. Our modus operandi involves a granular investigation, aiming to decipher the counts of occurrences of each artist country with a discerning focus on its internal representation within the expansive dataset.

Our analytical odyssey commences with a nuanced understanding of the inherent intricacies encapsulated within the artist country variable. Far from a mere quantitative exercise, our approach transforms each count into a narrative thread, weaving together a story of representation and prevalence. The artist country variable, a vibrant protagonist in this narrative, takes center stage as we unravel its multifaceted presence within the expansive landscape of the music dataset.

As we navigate through the data, our focal point becomes the artist country variable's self-representation, offering a panoramic view of its frequency and distribution. This endeavor is not a mere numerical exploration; rather, it's a journey into the heart of musical diversity, encapsulated within the representation of different countries. Each count becomes a testament to the global symphony, revealing not just the frequency but the nuanced nuances that distinguish one artist country from another.

In delving into the counts of artist countries within itself, we embark on a quest to decode the narrative of musical representation. The numbers cease to be mere statistics; instead, they metamorphose into keys that unlock the doors to cultural diversity, artistic expression, and global resonance. The artist country variable, in this context, transforms into a vibrant palette, painting the canvas of our analysis with the myriad hues of nations and their unique contributions to the musical landscape.

Furthermore, our methodology isn't confined to a singular exploration; it unfolds as a versatile tool, adaptable to unravel the complexities of other variables within our dataset. The same meticulous scrutiny can be applied to genres, time periods, or any other facet that our dataset encapsulates. This adaptability transcends the boundaries of a singular variable, paving the way for a comprehensive understanding of the intricate relationships that define the musical ecosystem.

As we extend our exploration beyond the artist country variable, we recognize the profound narrative potential embedded within our analysis. It's not merely about counts; it's about deciphering the stories that unfold as we traverse the expansive terrain of data. The self-representation of artist countries becomes a chapter in this narrative, revealing not just the frequency of occurrences but the cultural narratives, collaborative synergies, and global resonances that reverberate within the musical corridors of our dataset.

In conclusion, what we embark upon is a narrative journey, decoding the intricate stories within the numerical fabric of our data. The artist country variable, examined within the context of self-representation, becomes a lens through which we gain profound insights into the rich and diverse tapestry of music within our dataset. Each count, each occurrence, is a brushstroke on the canvas of our analysis, contributing to the vibrant masterpiece that is the representation of artist countries within the musical symphony of our data.
"
})
output$plot3 <- renderPlotly({
  ggplotly(CountryMusic%>%
             group_by(!!sym(input$variables5))%>%
             summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))%>%
             ggplot(aes_string(x = input$variables5, y = "views", fill = input$variables5))+
             geom_col(show.legend =TRUE) +
             ylab("VIEWS") +
             xlab("Column of Interest") +
             geom_text(aes(label = paste0(round(views / sum(views) * 100, 1), "%, ")),position = position_stack(vjust = 0.5))+
             coord_flip() +
             ggtitle("music penetration in terms of views")+
             theme_classic()+
             theme(axis.text.x = element_text(angle = 90, hjust = 0))
  )
})
output$table3 <- renderDataTable({
  CountryMusic%>%
    group_by(!!sym(input$variables5))%>%
    summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))
})
output$text3 <- renderText({
  "In this comprehensive exploration of our dataset, we embark on a journey to unravel the intricate dynamics of music penetration, employing a nuanced lens that focuses on the variable representing artist countries. Our analytical spotlight is directed towards understanding the self-representation of the artist country variable, offering a unique perspective on the prevalence and impact of each country within the expansive musical landscape encapsulated in our dataset.

The crux of our methodology lies in deciphering the music penetration, specifically in terms of views, as we delve into the self-representation of the artist country variable. Our quest is to meticulously tally the counts of occurrences of each artist country within itself, unveiling not just the numerical frequencies but also the underlying narratives, cultural resonances, and collaborative patterns that define the musical ecosystem.

As we traverse through the expansive dataset, each count of artist countries within itself becomes a pivotal data point, a gateway to understanding the diverse and multifaceted nature of musical representation. This exploration is not confined to numerical values alone; it transforms into a narrative journey, unraveling stories of global connectivity, artistic diversity, and the symbiotic relationships that shape the musical landscape.

The artist country variable, in this context, becomes a central protagonist, and its self-representation emerges as a testament to the multifaceted nature of music within our dataset. It is not merely about counting occurrences; it's about decoding the richness of each representation, discerning the unique qualities that each artist country brings to the musical tableau.

Furthermore, our methodology is not limited to a singular variable; it serves as a versatile tool that can be applied to probe the depths of other variables within our dataset. Whether it's genres, time periods, or any other facet of the musical landscape, our analytical framework adapts, providing a holistic understanding of the intricate relationships that define music penetration within our dataset.

In essence, what unfolds is a narrative expedition, where numerical counts transform into chapters of a compelling story. The self-representation of artist countries serves as a captivating narrative arc, revealing not only the statistical aspects of music penetration but also the cultural narratives, collaborative endeavors, and global resonances that reverberate within the dataset's musical corridors.

In conclusion, our analytical endeavor transcends the realms of mere counting; it is an exploration of the narratives woven within the fabric of our data. The self-representation of artist countries becomes a lens through which we gain profound insights into the diverse and vibrant tapestry of music penetration, offering a comprehensive understanding of the global symphony encapsulated within our dataset. Each count, each representation, contributes to the intricate masterpiece that is the self-portrait of artist countries within the music data landscape.
"
})
output$plot4 <- renderPlotly({
  ggplotly(
    CountryMusic%>%
      filter(COUNTRY==input$country)%>%
      count(!!sym(input$variables6))%>%
      ggplot(aes_string(x = input$variables6, y = "n", fill = input$variables6)) +
      geom_col(show.legend = TRUE) +
      ylab("count") +
      geom_text(aes(label = paste0(round(n / sum(n) * 100, 1), "%, ")),position = position_stack(vjust = 0.5))+
      xlab("Variable of Interest") +
      coord_flip() +
      ggtitle("Exploring the Count of Variable for the Chosen Country") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))
  )
})
output$table4 <- renderDataTable({
  CountryMusic%>%
    filter(COUNTRY==input$country)%>%
    count(!!sym(input$variables6))
})
output$text4 <- renderText({
  "In this analytical expedition, our dataset unfolds as a dynamic tableau where the first column proudly showcases a comprehensive list of countries, and the second column, equally diverse, presents an array of variables contributing to the rich tapestry of our data. Our quest revolves around unraveling the intricate relationships between these columns, specifically focusing on the count of a chosen variable from the second column in association with a designated country from the first column.

For our illustrative case study, we shine a spotlight on the vibrant and culturally diverse country of Nigeria, positioned in the esteemed first column. The variable that takes center stage in our exploration, residing in the second column, is the captivating realm of 'artist.' As we embark on this analytical journey, our goal is to meticulously tally the occurrences of the 'artist' variable within the context of Nigeria, shedding light on the prevalence and distribution of artists within this particular country.

This analysis transcends mere enumeration; it transforms into a narrative that encapsulates the musical landscape of Nigeria. Each count becomes a testament to the artistic vibrancy, collaborative endeavors, and cultural richness that characterize the intersection of the chosen variable and the selected country. The rhythm of Nigeria's music scene, as reflected in the variable 'artist,' unfolds before us, revealing not only numerical counts but the nuanced stories and synergies that define the relationship between the two columns.

Moreover, the methodology employed in this exploration is not confined to Nigeria or the 'artist' variable alone; it serves as a versatile tool ready to unveil insights for any chosen country and variable pairing within our dataset. Whether exploring genres, time periods, or any other facet of our expansive musical landscape, this analytical framework adapts, offering a holistic view of the dynamic interplay between countries and variables.

In essence, our analytical voyage is a celebration of the diverse symphony that emanates from the fusion of countries and variables within our dataset. The chosen country, Nigeria, and the selected variable, 'artist,' become protagonists in this narrative, contributing to the larger story of global musical connectivity. The counts, intricately woven into the fabric of this exploration, transform into chapters, each revealing a unique facet of the musical journey within Nigeria's borders.

As we navigate this exploration, it becomes clear that it is not merely a statistical exercise; it is a journey into the heart of music, where numbers echo the harmonies, melodies, and collaborations that define the intricate relationship between countries and variables. The analytical canvas we paint is a vibrant mosaic, with Nigeria and the 'artist'' variable adding their unique hues to the overarching masterpiece of our dataset.
"
})
output$plot5 <- renderPlotly({
  ggplotly(
    CountryMusic%>%
      filter(COUNTRY==input$country)%>%
      group_by(!!sym(input$variables7))%>%
      summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))%>%
      ggplot(aes_string(x = input$variables7, y = "views", fill = input$variables7)) +
      geom_col(show.legend = TRUE) +
      ylab("views") +
      geom_text(aes(label = paste0(round(views / sum(views) * 100, 1), "%, ")),position = position_stack(vjust = 0.5))+
      xlab("Variable of Interest") +
      coord_flip() +
      ggtitle("Exploring the Views of Variable for the Chosen Country") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))
  )
})
output$table5 <- renderDataTable({
  CountryMusic%>%
    filter(COUNTRY==input$country)%>%
    group_by(!!sym(input$variables7))%>%
    summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))
})
output$text5<- renderText({
  "In this investigative journey, our focus shifts to the column representing a plethora of variables within our dataset. Each variable encapsulates a distinct aspect of the musical landscape, contributing to the vibrant tapestry of our data. Our mission, in this exploration, centers on gauging the music penetration in terms of views, with a specific emphasis on the relationship between the chosen variable and the country designated in the first column of our previous analysis.

As a continuation of our earlier exploration where Nigeria stood as the focal point in the first column, we maintain our allegiance to this culturally rich nation. Nigeria, with its pulsating rhythms and diverse musical heritage, serves as an emblematic representation of the global music scene.

Now, the spotlight shifts to the variable realm, where the term 'songs' emerges as the protagonist of our analysis. In this context, 'songs' symbolize the quintessential unit of musical expression, encapsulating a myriad of genres, themes, and artistic visions.

Our analytical quest takes on a new dimension as we delve into the realm of music penetration, where the variable 'songs' becomes our guiding beacon. Through meticulous examination, we seek to unravel the depth of engagement and resonance that songs from our dataset achieve within the confines of Nigeria's borders.

Each view garnered by a song becomes a testament to its impact, reaching audiences, and resonating with listeners in Nigeria. The numerical count of views serves not only as a metric of popularity but also as a reflection of cultural influence and artistic significance within this vibrant musical ecosystem.

Moreover, our choice of Nigeria as the focal point imbues this exploration with cultural richness and historical depth. As a powerhouse of musical innovation and creativity, Nigeria's influence extends far beyond its geographical boundaries, shaping trends and setting precedents across the global music landscape.

Furthermore, the versatility of our analytical approach allows for seamless adaptation to explore other countries and variables within our dataset. Whether delving into the music penetration of genres, artists, or other dimensions of musical expression, our methodology remains robust, offering a comprehensive lens through which to understand the intricate interplay between variables and countries.

In essence, our exploration transcends the confines of mere data analysis; it becomes a celebration of music's universal language and its profound ability to transcend borders and boundaries. Through the lens of Nigeria and the variable 'songs,' we embark on a journey of discovery, uncovering the multi-faceted tapestry of musical penetration and cultural resonance within our dataset.
"
})
output$plot6 <- renderPlotly({
  ggplotly(
    CountryMusic%>%
      filter(ARTISTS_COUNTRY==input$Artist_country)%>%
      count(!!sym(input$variables8))%>%
      ggplot(aes_string(x = input$variables8, y = "n", fill = input$variables8)) +
      geom_col(show.legend = TRUE) +
      ylab("count") +
      geom_text(aes(label = paste0(round(n / sum(n) * 100, 1), "%, ")),position = position_stack(vjust = 0.5))+
      xlab("Variable of Interest") +
      coord_flip() +
      ggtitle("Exploring the Count of Variable for the Chosen Artist Country") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))
  )
})
output$table6 <- renderDataTable({
  CountryMusic%>%
    filter(ARTISTS_COUNTRY==input$Artist_country)%>%
    count(!!sym(input$variables8))
})
output$text6 <- renderText({
  "In this analytical voyage, our journey begins with the first column, which stands as a compendium of artist countries, each representing a unique cultural backdrop and musical heritage. Across this expansive landscape, nations intertwine, fostering a rich tapestry of artistic expression and creative diversity.
  
  Simultaneously, the second column emerges as a repository of various variables within our dataset, each holding a distinct facet of musical exploration. Among these variables, the spotlight now turns to 'songs,' a fundamental unit of musical expression that transcends linguistic and cultural barriers, serving as a universal medium of human connection.
  
  Our mission is clear: to dissect the intricate relationship between the chosen variable and the artist country, with Nigeria serving as our vantage point. As the heartbeat of Africa's musical renaissance, Nigeria pulsates with rhythm and vitality, its cultural resonance reverberating across continents.

With Nigeria firmly established as our anchor in the first column, we pivot our gaze to the variable realm, where 'songs' beckon us to unravel their mysteries. In this exploration, we seek not only to quantify the prevalence of songs within Nigeria but also to glean insights into their cultural impact and resonance within this vibrant artistic landscape.

Through meticulous analysis and statistical scrutiny, we endeavor to unveil the numerical count of songs associated with Nigeria, shedding light on their distribution and popularity within the country's borders. Each count serves as a testament to the enduring influence of music in shaping societal discourse, fostering unity, and celebrating diversity.
  
  Moreover, our choice of Nigeria as the focal point imbues this exploration with historical significance and cultural depth, underscoring the nation's pivotal role in shaping the trajectory of African music and influencing global trends.

As we traverse the intricate terrain of data analysis, our methodology remains flexible and adaptable, capable of accommodating diverse variables and countries within our dataset. Whether exploring the prevalence of genres, artists, or other dimensions of musical expression, our approach remains steadfast in its commitment to unraveling the complexities of music's enduring allure.
  
  In essence, our exploration transcends the confines of mere statistical analysis; it becomes a celebration of Nigeria's rich musical heritage and its profound impact on the global cultural landscape. Through the lens of Nigeria and the variable 'songs,' we embark on a journey of discovery, unearthing the transformative power of music to transcend boundaries, unite communities, and ignite the human spirit."

})
output$plot7 <- renderPlotly({
  ggplotly(
    CountryMusic%>%
      filter(ARTISTS_COUNTRY==input$Artist_country)%>%
      group_by(!!sym(input$variables9))%>%
      summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))%>%
      ggplot(aes_string(x = input$variables9, y = "views", fill = input$variables9)) +
      geom_col(show.legend = TRUE) +
      ylab("views") +
      geom_text(aes(label = paste0(round(views / sum(views) * 100, 1), "%, ")),position = position_stack(vjust = 0.5))+
      xlab("Variable of Interest") +
      coord_flip() +
      ggtitle("Exploring the Views of Variable for the Chosen Artiste Country") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0))
  )
})
output$table7 <- renderDataTable({
  CountryMusic%>%
    filter(ARTISTS_COUNTRY==input$Artist_country)%>%
    group_by(!!sym(input$variables9))%>%
    summarise(views= sum(WEEKLY_VIEWS, na.rm = TRUE))
})
output$text7<- renderText({
  "In this analytical endeavor, we delve into the matrix of variables that populate our dataset, each representing a unique facet of the musical landscape. Our focus now shifts to the measurement of music penetration, gauged in terms of views, with a keen eye on the relationship between the artist's country and the variable of interest.

As we navigate through the columns of our dataset, we encounter a diverse array of variables, each holding a key to unlocking insights into the multifaceted realm of music. In this exploration, our attention is drawn to the pivotal role played by the artist's country, serving as a crucible for cultural expression and artistic innovation.

With Nigeria designated as our chosen artist country, we embark on a journey to elucidate its influence on the variable of 'Countries' within our dataset. This selection holds particular significance, considering Nigeria's status as a beacon of musical excellence and innovation, not only within Africa but on the global stage.

By examining the interplay between Nigeria and the variable of 'Countries,' we aim to uncover patterns of music penetration and viewership across different geographical regions. Through this lens, we seek to discern the extent to which Nigerian music resonates with audiences worldwide, transcending borders and cultural barriers.

Our methodology hinges on meticulous data analysis, wherein we quantify the prevalence of views attributed to Nigerian artists across various countries. Each count serves as a testament to the cross-cultural appeal of Nigerian music, highlighting its ability to captivate audiences across continents.

Moreover, our choice of Nigeria as the focal point underscores its significance as a cultural powerhouse, shaping the musical landscape not only within its borders but far beyond. From Afrobeat to Highlife, Nigerian music encompasses a rich tapestry of genres and influences, reflecting the nation's cultural diversity and dynamism.

Through this exploration, we aim not only to quantify the impact of Nigerian music on a global scale but also to celebrate its role in fostering cultural exchange and mutual understanding. As we traverse the data landscape, our analysis remains grounded in a spirit of curiosity and discovery, seeking to unravel the intricate threads that bind music, culture, and identity.

In conclusion, our exploration of music penetration in relation to the artist's country offers a glimpse into the transcendent power of music to unite, inspire, and transform. With Nigeria as our guide and 'Countries' as our variable of interest, we embark on a journey of exploration and enlightenment, driven by a passion for understanding the profound influence of music on the human experience.
"
})
network_data <- reactive({
  re1 <- CountryMusic %>%
    select(node = !!sym(input$variables10))
  re2 <- CountryMusic %>%
    select(node = !!sym(input$variables11))
  CountryMusic_Node <- rbind(re1, re2)
  Node <- data.frame(id = unique(CountryMusic_Node),
                     color = c("darkred"))
  Edge <- data.frame(from = re1$node, to = re2$node,
                     length = c(100, 500),
                     arrows = c("to", "from", "middle", "middle;to"),
                     dashes = c(TRUE, FALSE),
                     title = paste("Edge", 1:20),
                     smooth = c(FALSE, TRUE),
                     shadow = c(FALSE, TRUE, FALSE, TRUE))
  net <- graph_from_data_frame(d = Edge, vertices = Node, directed = TRUE)
  return(net)
})

layout_custom <- reactive({
  net <- network_data()
  layout_custom <- layout_nicely(net)
  return(layout_custom)
})
communities <- reactive({
  net <- network_data()
  communities <- cluster_walktrap(net)$membership
  return(communities)
})
output$text8 <- renderText({
  "Network analysis (relationship/community)"
})
output$text8a <- renderText({
  "In this exploratory analysis, we dive into the intricate web of connections within our dataset, employing network analysis to unveil the underlying communities shaped by the chosen variables. The focal points of our investigation are the variables residing in the first and second columns, with a specific focus on songs and artist countries, respectively.

Imagine our dataset as a vast network where each node represents a unique entity, and the edges between them signify relationships. In this case, the nodes are the songs, and the edges connect them to the corresponding artist countries. Our objective is to unravel the communities within this musical network, shedding light on how songs and artist countries coalesce to form interconnected clusters.

Taking songs as our point of origin in the first column, we trace the threads that bind them to the artist countries listed in the second column. This network-based approach allows us to discern patterns of association, highlighting songs that share common ground in terms of the artist's country.

The crux of our analysis lies in community detection, a technique that identifies groups of nodes (songs) densely interconnected with each other while having fewer connections to nodes outside the group. This methodology is akin to uncovering musical tribes within our dataset, where songs with similar artist countries form cohesive clusters.

As we apply this network lens to the case study of songs and artist countries, we gain insights into the collaborative and genre-specific ties that bind them. For instance, we may discover that songs from a particular genre or era are closely linked to artist countries that played a pivotal role in shaping that musical landscape.

Moreover, our exploration extends beyond mere counting and delves into the qualitative aspects of these connections. By scrutinizing the network structure, we discern not only the prevalence of certain artist countries within the songs but also the nuances of influence, collaboration, and cross-cultural fertilization.

This network analysis approach is a dynamic tool, adaptable to different variable pairs within our dataset. By substituting songs and artist countries with other variables, we can unravel distinct communities, uncover hidden patterns, and deepen our understanding of the intricate relationships that underpin the musical fabric captured in our data.

In essence, this exploration goes beyond traditional analytics, offering a visually compelling narrative of the interconnectedness within our dataset. Through the lens of network analysis, we embark on a journey of discovery, unveiling the communities that emerge when songs and artist countries converge, each note resonating with the echoes of cultural exchange, artistic collaboration, and the rich tapestry of the global music landscape.
"
})
output$text8b <- renderText({
  "Network analysis (centralization)
"
})
output$text8c <- renderText({
  "In this analytical expedition, we embark on a journey through network analysis to unravel the essence of centrality, exposing the intricate relationships between variables chosen in each column. Building upon the foundation laid in the previous analysis, where we explored the connection between countries and songs, our focus now shifts to understanding the centrality of songs in relation to the countries.

Central to this exploration is the concept of centrality itself, a measure that signifies the significance of a variable within the network formed by the chosen variables. Our case study unfolds with countries taking the lead in the first column of the previous analysis, and songs emerging as the variable of interest in this column.

Picture our dataset as a dynamic network, where each node represents a country, and the edges denote relationships with songs. Centrality, in this context, becomes a beacon illuminating the prominence of songs within this intricate web of musical connections.

Our approach involves dissecting the relationships established in the previous analysis, where countries played a pivotal role. By focusing on songs in this exploration, we seek to understand not only the prevalence of songs associated with specific countries but also their centrality within the broader musical landscape.

Centrality can manifest in various forms—be it through songs that are widely embraced across diverse countries, becoming central nodes in the global musical network, or through songs that hold a unique and central position within specific country-centric clusters. The nuances of these centrality patterns provide rich insights into the dynamics of musical influence, cross-cultural appeal, and the global dissemination of artistic expressions.

To measure centrality, we employ metrics such as degree centrality, which gauges the number of connections a song has, and betweenness centrality, which identifies songs that act as crucial bridges between different countries. By analyzing these metrics, we unravel the web of influence woven by songs across the spectrum of countries, offering a nuanced understanding of their centrality in the musical tapestry.

Furthermore, this exploration is not confined to a static analysis; rather, it provides a dynamic lens through which we can adapt our focus to different variable pairs within the dataset. The synergy between countries and songs serves as a case study, a gateway to understanding how the centrality dynamics unfold across diverse dimensions of our data.

In essence, this exploration transcends traditional analytics, delving into the pulsating heart of our dataset's network structure. Through the lens of centrality analysis, we decipher the significance of songs within the vast musical network, revealing the nodes that radiate influence, connect disparate musical realms, and shape the ever-evolving narrative of global music culture."
})



  output$plot8 <- renderPlot({
    ggraph_obj <- ggraph(network_data(), layout_custom()) +
      geom_edge_link2(edge_endcap = "round", edge_linetype = "solid", edge_color = "lightblue",
                      arrow = grid::arrow(angle = 30, length = unit(0.15, "inches"),
                                          ends = "last", type = "open")) +
      geom_node_point(aes(color = as.factor(communities())), size = 7) +
      geom_node_text(aes(label = name), vjust = -1, size = 5, color = "black") +
      theme_classic() +
      ggtitle("Harmony Unveiled: Exploring Interconnected Communities in a Network Layout")+
      scale_color_discrete(name = "Community") +
      guides(color = guide_legend(title = "Community"))
    
    print(ggraph_obj)  # Use print to force the rendering in Shiny
  })
  
  degree_centrality <- reactive({
    net <- network_data()
    degree_centrality <- degree(net)
    return(degree_centrality)
  })
  output$plot9 <- renderPlot({
    req(network_data(), layout_custom(), degree_centrality())  # Ensure required data is available
    
    net <- network_data()
    layout_custom <- layout_nicely(net)
    degree_centrality <- degree(net)
    
    centrality_values <- degree_centrality  # Store degree centrality values
    top_central_nodes <- which(centrality_values > quantile(centrality_values, 0.9))
    
    Node <- data.frame(id = V(net)$name,
                       color = as.factor(communities()))
    Node_top <- Node[Node$id %in% V(net)$name[top_central_nodes], ]
    
    ggraph_obj <- ggraph(net, layout = layout_custom) +
      geom_edge_link2(edge_linetype = "solid", edge_color = "green",
                      arrow = grid::arrow(angle = 30, length = unit(0.15, "inches"),
                                          ends = "last", type = "open")) +
      geom_node_point(aes(x= layout_custom[,1], y= layout_custom[,2], color = centrality_values), size = 7) +
      geom_node_point(data = Node_top, aes(x = layout_custom[,1][Node_top$id], y = layout_custom[,2][Node_top$id], color = centrality_values[Node_top$id]), size = 7) +
      geom_node_text(aes(label = name), vjust = -1, size = 5, color = "black") +
      theme_classic() +
      ggtitle("Custom Layout Network Visualization with Degree Centrality Highlighting")+
      scale_color_viridis_c(name = "Degree Centrality", option = "turbo")
    
    
    print(ggraph_obj)
  })
  
  output$dynamic21 <- renderDataTable({
    dc%>%arrange(cluster)
  })
  output$dynamic22 <- renderDataTable({
    dc1%>%arrange(cluster)
  })
  output$static22 <- renderText({
    "In this analytical endeavor, our focus shifts to the dynamic clustering of countries based on their respective numbers of views, employing a nuanced approach that aligns countries sharing similar ranges of views into cohesive clusters.

Imagine our dataset as a vibrant tapestry of diverse countries, each woven into the fabric of global music culture, and their numbers of views serving as the threads that bind them together. The objective here is not merely to dissect individual countries but to discern patterns and affinities among them, unveiling a mosaic of viewing preferences that transcend geographical boundaries.

The clustering methodology adopted follows a thoughtful strategy—countries are grouped together when their numbers of views fall within specific ranges. This deliberate approach allows us to identify clusters of countries that exhibit similarities in the magnitude of their music consumption, creating a more nuanced understanding of global viewership dynamics.

As we delve into this clustering analysis, a symphony of connections emerges, echoing the shared viewing habits of countries within each cluster. Whether it's a cluster characterized by countries with high numbers of views, signifying a collective affinity for particular genres or artists, or a cluster representing nations with moderate views, indicative of diverse musical tastes, each grouping tells a unique story within the overarching narrative of global music consumption.

To implement this clustering, sophisticated algorithms and statistical techniques are harnessed to discern meaningful patterns in the distribution of views across countries. This process involves considering not only the absolute numbers of views but also the relative positions of countries in the viewing spectrum, ensuring a comprehensive and accurate clustering representation.

The results of this analysis go beyond a static categorization; they offer a dynamic lens through which we can observe how clusters evolve over time or adapt when different variables are considered. The clusters become dynamic entities, reflecting the ever-changing landscape of global music preferences and consumption.

In conclusion, this clustering analysis ventures beyond the conventional exploration of countries, unraveling a tapestry where similarities in viewing habits weave intricate connections between nations. The clustered countries, bound by their shared ranges of views, beckon us to explore the rich diversity of global music culture and appreciate the nuanced patterns that emerge when viewed through the lens of collective viewership dynamics.
"
  })
  output$static23 <- renderText({
    "In this insightful analysis, our focus shifts to the clustering of countries based on their distinct musical preferences or genres, employing a meticulous approach that unites countries with similar genre inclinations into coherent clusters.

Imagine our dataset as a vibrant tapestry of global music diversity, with each country contributing its unique thread in the form of preferred musical genres. The objective here is not merely to categorize countries by geographic location but to unravel the intricate tapestry of musical tastes, creating clusters that showcase the harmonious blending of nations with comparable genre predilections.

The clustering methodology adopted is a thoughtful orchestration—countries find their place in clusters when their musical preferences align within specific genre ranges. This deliberate approach allows us to identify clusters of countries that share common ground in their musical choices, offering a nuanced perspective on the global landscape of musical diversity.

As we delve into this clustering analysis, a symphony of connections emerges, echoing the shared musical affinities of countries within each cluster. Picture a cluster characterized by countries with a penchant for rock or a cluster representing nations inclined towards hip-hop. Each grouping within these clusters tells a unique story, reflecting the rich mosaic of global music preferences.

To implement this clustering, advanced algorithms and statistical techniques are harnessed to discern meaningful patterns in the distribution of musical genres across countries. This involves considering not only the prevalence of genres but also the relative positions of countries within the musical spectrum, ensuring a comprehensive and accurate representation of clustering dynamics.

The results of this analysis transcend mere categorization; they offer a dynamic lens through which we can observe how clusters evolve over time or adapt when different variables, such as views or artist preferences, are considered. The clusters become dynamic entities, reflecting the ever-changing landscape of global music preferences and the fascinating interplay of nations within these musical constellations.

In conclusion, this clustering analysis ventures beyond traditional country-centric perspectives, unraveling a tapestry where shared musical preferences weave intricate connections between nations. The clustered countries, bound by their harmonious genre ranges, invite us to explore the rich diversity of global music culture and appreciate the nuanced patterns that emerge when viewed through the lens of collective musical preference dynamics."
  })
  output$summary21 <- renderPrint({
    summary(res_music)
  })
  output$summary24 <- renderPrint({
    summary(res_music1)
  })
  output$summary22<- renderPrint({
    summary(dc)
  })
  output$summary23 <- renderPrint({
    summary(dc1)
  })
  output$plot21 <- renderPlot({
    fviz_cluster(res_music, data = MusicClustering,
                 ellipse.type = "euclid", # Concentration ellipse
                 star.plot = TRUE, # Add segments from centroids to items
                 repel = TRUE, # Avoid label overplotting (slow)
                 ggtheme = theme_minimal()
    ) + theme(panel.background = element_rect(fill = "lightblue"))+theme_classic()
  })
  output$plot22 <- renderPlot({
    fviz_cluster(res_music1, data = MusicClustering,
                 ellipse.type = "euclid", # Concentration ellipse
                 star.plot = TRUE, # Add segments from centroids to items
                 repel = TRUE, # Avoid label overplotting (slow)
                 ggtheme = theme_minimal()
    ) + theme(panel.background = element_rect(fill = "darkblue"))+theme_classic()
  })
  
  
  
  
  
  



}

 #Run the application 
shinyApp(ui = ui, server = server)
