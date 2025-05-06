MalnutrtionKDE = function(malnutriton, s.owin, whichone){

    # cols <- brewer.pal(3, "BuRd")
    mycols <- colors()[c(124,138,555)]# Blue , White, Red
    pal1 <- colorRampPalette(mycols)
    
    if (whichone == "India"){
    
      if(malnutriton == "Stunting"){
        
        df_stunt <- read.csv("01_Data/df_stunt_Yes_No.csv")
        df_stunt <- df_stunt[df_stunt$HW70_yes_no == 'Yes',]
        data.ppp <- ppp(x = df_stunt$Longitude, y = df_stunt$Latitude, window = s.owin, marks = df_stunt$HW70_yes_no)
        # plot(density(stunt.ppp, sigma = 1), col=colors, breaks = breakpoints, border="black")
        # contour(density(stunt.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      } else if(malnutriton == "Underweight"){
        
        df_underweight <- read.csv("01_Data/df_underweight_Yes_No.csv")
        df_underweight <- df_underweight[df_underweight$HW71_yes_no == 'Yes',]
        data.ppp <- ppp(x = df_underweight$Longitude, y = df_underweight$Latitude, window = s.owin, marks = df_underweight$HW71_yes_no)
        # plot(density(underweight.ppp, sigma = 1),col=colors, breaks = breakpoints)
        # contour(density(underweight.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      }else if(malnutriton == "Wasting"){
        
        df_wast <- read.csv("01_Data/df_wast_Yes_No.csv")
        df_wast <- df_wast[df_wast$HW72_yes_no == 'Yes',]
        data.ppp <- ppp(x = df_wast$Longitude, y = df_wast$Latitude, window = s.owin, marks = df_wast$HW72_yes_no)
        # plot(density(wast.ppp, sigma = 1), col=colors, breaks = breakpoints)
        # contour(density(wast.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      }else if(malnutriton == "Overweight"){
        
        df_over <- read.csv("01_Data/df_overweight_Yes_No.csv")
        df_over <- df_over[df_over$Overweight == 'Yes',]
        data.ppp <- ppp(x = df_over$Longitude, y = df_over$Latitude, window = s.owin, marks = df_over$Overweight)
        # plot(density(over.ppp, sigma = 1), col=colors, breaks = breakpoints)
        # contour(density(over.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      } else if(malnutriton == "Obese"){
        
        df_obese <- read.csv("01_Data/df_obese_Yes_No.csv")
        df_obese <- df_obese[df_obese$Obese == 'Yes',]
        data.ppp <- ppp(x = df_obese$Longitude, y = df_obese$Latitude, window = s.owin, marks = df_obese$Obese)
        # plot(density(obese.ppp, sigma = 1), col=colors, breaks = breakpoints)
        # contour(density(obese.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      }
    }
    if (whichone == "Karnataka"){
      
      if(malnutriton == "Stunting"){
        
        df_stunt <- read.csv("01_Data/df_stunt_Yes_No.csv")
        df_stunt <- df_stunt%>%
          filter(HW70_yes_no == 'Yes' & V024 == "Karnataka")
        data.ppp <- ppp(x = df_stunt$Longitude, y = df_stunt$Latitude, window = s.owin, marks = df_stunt$HW70_yes_no)
        # plot(density(stunt.ppp, sigma = 1), col=colors, breaks = breakpoints, border="black")
        # contour(density(stunt.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      } else if(malnutriton == "Underweight"){
        
        df_underweight <- read.csv("01_Data/df_underweight_Yes_No.csv")
        df_underweight <- df_underweight%>%
          filter(HW71_yes_no == 'Yes' & V024 == "Karnataka")
        data.ppp <- ppp(x = df_underweight$Longitude, y = df_underweight$Latitude, window = s.owin, marks = df_underweight$HW71_yes_no)
        # plot(density(underweight.ppp, sigma = 1),col=colors, breaks = breakpoints)
        # contour(density(underweight.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      }else if(malnutriton == "Wasting"){
        
        df_wast <- read.csv("01_Data/df_wast_Yes_No.csv")
        df_wast <- df_wast%>%
          filter(HW72_yes_no == 'Yes' & V024 == "Karnataka")
        data.ppp <- ppp(x = df_wast$Longitude, y = df_wast$Latitude, window = s.owin, marks = df_wast$HW72_yes_no)
        # plot(density(wast.ppp, sigma = 1), col=colors, breaks = breakpoints)
        # contour(density(wast.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      }else if(malnutriton == "Overweight"){
        
        df_over <- read.csv("01_Data/df_overweight_Yes_No.csv")
        df_over <- df_over%>%
          filter(Overweight == 'Yes' & V024 == "Karnataka")
        data.ppp <- ppp(x = df_over$Longitude, y = df_over$Latitude, window = s.owin, marks = df_over$Overweight)
        # plot(density(over.ppp, sigma = 1), col=colors, breaks = breakpoints)
        # contour(density(over.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      } else if(malnutriton == "Obese"){
        
        df_obese <- read.csv("01_Data/df_obese_Yes_No.csv")
        df_obese <- df_obese%>%
          filter(Obese == 'Yes' & V024 == "Karnataka")
        data.ppp <- ppp(x = df_obese$Longitude, y = df_obese$Latitude, window = s.owin, marks = df_obese$Obese)
        # plot(density(obese.ppp, sigma = 1), col=colors, breaks = breakpoints)
        # contour(density(obese.ppp, 1), axes = F, add=TRUE)
        # plot(s.owin, add=TRUE, las=1)
        
      }
    }
    
    return (data.ppp)
}

















