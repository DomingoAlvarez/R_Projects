#############################################
#             Install Packages              # 
#############################################


list.of.packages <- c('tidyverse','lubridate','stringr','forecast','gridExtra','corrplot')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) {  
  install.packages(new.packages)
}

#############################################
#             Reading Data                  # 
#############################################

sales <- read.csv2(file ="C:\\Users\\domingo.alvarez\\Downloads\\Roche\\Sales_Expense_InputFile.csv",
                   sep="," , dec = ".")

account <- read.csv2(file ="C:\\Users\\domingo.alvarez\\Downloads\\Roche\\Account_Potential_Data.csv",
                     sep="," )

########################################################
#     Adding account potential to sales                #
########################################################

sales <- left_join(sales, account, by.x=sales$ACCOUNT_ID, by.y=account$ACCOUNT_ID)

head(sales)

summary(sales)

########################################################
#     MONTH should be a date                           #
########################################################

class(sales$MONTH)

sales$MONTH<- as.Date(sales$MONTH, "%m/%d/%Y")

########################################################
#     Renaming variables                               #
########################################################

sales<- sales %>% rename(DATE=MONTH, 
                         UNIT_SALES=UNIT_SALES..in.Million...,
                         TOTAL_EXPENSE=TOTAL_EXPENSE..in.Million...,
                         SALESFORCE_EXPENSE=SALESFORCE_EXPENSE..in.Million...,
                         DIGITAL_EXPENSE=DIGITAL_EXPENSE..in.Million...)

####################################################
#  searching for missing data and there's none     #
####################################################

any(is.na(sales))



        ##################################################
        #                                                #
        # Let's do some Exploratory Data Analysis        #
        #                                                #
        ##################################################



#####################################################################################
#                                                                                   #      
# Plotting sales and expenses for the differents drugs, countries and accounts      #
#    Looks like there's a correlation between expenses and sales, as expected.      #
#                                                                                   #  
#####################################################################################

countries <- as.vector(unique(sales$COUNTRY))
accounts <- as.vector(unique(sales$ACCOUNT_ID))
drugs <- as.vector(unique(sales$PRODUCT))

for(h in 1:length(drugs)){
  
  for (j in 1:length(countries)) {
    
    for (i in 1:length(accounts)) {
      
      datos <- filter(sales,PRODUCT==drugs[h],COUNTRY==countries[j],ACCOUNT_ID==accounts[i])
      if (dim(datos)[1]==0){ 
        next
      }
      else{
        plot <- ggplot(datos, aes(x = DATE)) +
          geom_line(aes(y = UNIT_SALES),col = "#000000") +
          geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
          geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
          geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
          scale_x_date(date_labels = "%y %b", date_breaks = "1 month") +
          theme_bw() + theme(legend.title = element_blank(),
                             axis.text.x  = element_text(angle=45, vjust=0.5)) +
          ggtitle(paste(drugs[h],countries[j],"ID_ACCOUNT",accounts[i]), ' \n')
        
        print(plot)
        cor(datos[,(5:8)])
      }
    }
  }
}

#####################################################################################
#                                                                                   #      
# Let's plot agregated sales for the differents drugs  and compare them             #
#                                                                                   # 
#####################################################################################

          ###############################
          #                             #
          #          Drug 1             #
          ###############################

drug1 <- sales %>% filter(PRODUCT==drugs[2])

sales_drug1 <- aggregate(x= drug1[c("UNIT_SALES", "TOTAL_EXPENSE", "SALESFORCE_EXPENSE", "DIGITAL_EXPENSE")],
                         FUN = sum,
                         by = list(Group.date = drug1$DATE))

sales_drug1$PRODUCT <- "Drug_1"

plot_drug1 <- ggplot(sales_drug1, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES),col = "#000000") +
  geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
  geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
  geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
  scale_x_date(date_labels = "%y %b", date_breaks = "3 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG1"), ' \n')

print(plot_drug1)


            ###############################
            #                             #
            #  Drug 1 by Potential        #
            ###############################


#HIGH POTENTIAL
drug1_high <- sales %>% filter(PRODUCT==drugs[2], ACCOUNT_POTENTIAL=="High")

sales_drug1_high <- aggregate(x= drug1_high[c("UNIT_SALES", "TOTAL_EXPENSE", "SALESFORCE_EXPENSE", "DIGITAL_EXPENSE")],
                              FUN = sum,
                              by = list(Group.date = drug1_high$DATE))

sales_drug1_high$PRODUCT <- "Drug_1"

plot1 <- ggplot(sales_drug1_high, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES),col = "#000000") +
  geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
  geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
  geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
  scale_x_date(date_labels = "%y %b", date_breaks = "1 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG1 HIGH POTENTIAL"), ' \n')

print(plot1)

#MEDIUM POTENTIAL

drug1_medium <- sales %>% filter(PRODUCT==drugs[2], ACCOUNT_POTENTIAL=="Medium")

sales_drug1_medium <- aggregate(x= drug1_medium[c("UNIT_SALES", "TOTAL_EXPENSE", "SALESFORCE_EXPENSE", "DIGITAL_EXPENSE")],
                                FUN = sum,
                                by = list(Group.date = drug1_medium$DATE))

sales_drug1_medium$PRODUCT <- "Drug_1"

plot2 <- ggplot(sales_drug1_medium, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES),col = "#000000") +
  geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
  geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
  geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
  scale_x_date(date_labels = "%y %b", date_breaks = "1 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG1 MEDIUM POTENTIAL"), ' \n')

print(plot2)


#LOW POTENTIAL

drug1_low <- sales %>% filter(PRODUCT==drugs[2], ACCOUNT_POTENTIAL=="Low")

sales_drug1_low <- aggregate(x= drug1_low [c("UNIT_SALES", "TOTAL_EXPENSE", "SALESFORCE_EXPENSE", "DIGITAL_EXPENSE")],
                             FUN = sum,
                             by = list(Group.date = drug1_low $DATE))

sales_drug1_low$PRODUCT <- "Drug_1"

plot3 <- ggplot(sales_drug1_low, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES),col = "#000000") +
  geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
  geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
  geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
  scale_x_date(date_labels = "%y %b", date_breaks = "1 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG1 LOW POTENTIAL"), ' \n')

print(plot3)

            #######################
            #                     #
            #   Drug 1  by year   #
            #                     #
            #######################

yeear <-as.vector(unique(year(sales_drug1$Group.date)))

for (i in 1:length(yeear)) {
  sales_drug1_year <- sales_drug1 %>% filter(year(sales_drug1$Group.date)==yeear[i])
  
  plot1 <- ggplot(sales_drug1_year, aes(x = Group.date)) +
    geom_line(aes(y = UNIT_SALES),col = "#000000") +
    geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
    geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
    geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
    scale_x_date(date_labels = "%y %b", date_breaks = "1 month")+
    theme_bw() + 
    theme(legend.title = element_blank(),
          axis.text.x  = element_text(angle=45, vjust=0.5))+
    ggtitle(paste("DRUG1", yeear[i]), ' \n')
  
  print(plot1)
  
  
}

        #######################
        #                     #
        #   Drug 2            #
        #                     #
        #######################


drug2 <- sales %>% filter(PRODUCT==drugs[1])

sales_drug2 <- aggregate(x= drug2[c("UNIT_SALES", "TOTAL_EXPENSE", "SALESFORCE_EXPENSE", "DIGITAL_EXPENSE")],
                         FUN = sum,
                         by = list(Group.date = drug2$DATE))

sales_drug2$PRODUCT <- "Drug_2"

plot_drug2 <- ggplot(sales_drug2, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES),col = "#000000") +
  geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
  geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
  geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
  scale_x_date(date_labels = "%y %b", date_breaks = "3 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG2"), ' \n')

print(plot_drug2)

          #######################
          #                     #
          #   Drug 2  by year   #
          #                     #
          #######################

yeear <-as.vector(unique(year(sales_drug2$Group.date)))

for (i in 1:length(yeear)) {
  sales_drug2_year <- sales_drug2 %>% filter(year(sales_drug2$Group.date)==yeear[i])
  
  plot1 <- ggplot(sales_drug2_year, aes(x = Group.date)) +
    geom_line(aes(y = UNIT_SALES),col = "#000000") +
    geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
    geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
    geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
    scale_x_date(date_labels = "%y %b", date_breaks = "1 month")+
    theme_bw() + 
    theme(legend.title = element_blank(),
          axis.text.x  = element_text(angle=45, vjust=0.5))+
    ggtitle(paste("DRUG2", yeear[i]), ' \n')
  
  print(plot1)
  
  
}


          ###################
          #                 #
          #   Drug 3        #
          ###################

drug3 <- sales %>% filter(PRODUCT==drugs[3])

sales_drug3 <- aggregate(x= drug3[c("UNIT_SALES", "TOTAL_EXPENSE", "SALESFORCE_EXPENSE", "DIGITAL_EXPENSE")],
                         FUN = sum,
                         by = list(Group.date = drug3$DATE))

sales_drug3$PRODUCT <- "Drug_3"

plot3 <- ggplot(sales_drug3, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES),col = "#000000") +
  geom_line(aes(y = TOTAL_EXPENSE),col = "red")+
  geom_line(aes(y = SALESFORCE_EXPENSE),col = "hotpink")+
  geom_line(aes(y = DIGITAL_EXPENSE),col = "steelblue")+
  scale_x_date(date_labels = "%y %b", date_breaks = "3 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG3"), ' \n')

print(plot3)

        ##########################
        #                        #
        #   All drugs together   #
        ##########################


grid.arrange(plot_drug1, plot_drug2,nrow=2)

names(sales_drug1) <- c("Group.date","UNIT_SALES_1", "TOTAL_EXPENSE_1", "SALESFORCE_EXPENSE_1",
                        "DIGITAL_EXPENSE_1", "PRODUCT_1" )
names(sales_drug2) <- c("Group.date","UNIT_SALES_2", "TOTAL_EXPENSE_2", "SALESFORCE_EXPENSE_2",
                        "DIGITAL_EXPENSE_2", "PRODUCT_2" )
names(sales_drug3) <- c("Group.date","UNIT_SALES_3", "TOTAL_EXPENSE_3", "SALESFORCE_EXPENSE_3",
                        "DIGITAL_EXPENSE_3", "PRODUCT_3" )


sales_1_2 <- left_join(sales_drug1,sales_drug2,by.x=sales_drug1$"Group.date", by.y=sales_drug2$"Group.date")


plot1_2 <- ggplot(sales_1_2, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES_1),col = "#000000") +
  geom_line(aes(y = UNIT_SALES_2),col = "red")+
  scale_x_date(date_labels = "%y %b", date_breaks = "3 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG 1 and 2"), ' \n')

print(plot1_2)

sales_1_2_3 <- left_join(sales_1_2,sales_drug3,by.x=sales_1_2$"Group.date", by.y=sales_drug3$"Group.date")

sales_1_2_3[is.na(sales_1_2_3)]<- 0
sales_1_2_3$PRODUCT_3  <- "Drug_3"
any(is.na(sales_1_2_3))

plot1_2_3 <- ggplot(sales_1_2_3, aes(x = Group.date)) +
  geom_line(aes(y = UNIT_SALES_1),col = "#000000") +
  geom_line(aes(y = UNIT_SALES_2),col = "red")+
  geom_line(aes(y = UNIT_SALES_3),col = "blue")+
  scale_x_date(date_labels = "%y %b", date_breaks = "2 month")+
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.text.x  = element_text(angle=45, vjust=0.5))+
  ggtitle(paste("DRUG 1, 2 and 3"), ' \n')

print(plot1_2_3)

grid.arrange(plot1_2, plot3,nrow=2)

