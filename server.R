# server.R

#----------------------------------------
# Functions
#----------------------------------------

# Dividing data into time periods

timeGroups<-
  function (x, period="Month", format = "%Y-%m-%d", origin = as.Date("1970-01-01"), 
            min.date, max.date, sunday = TRUE){
    if(period=="Month"){
      before=31
      after=0
      am<-as.month(x, format=format, min.date=min.date, max.date, before=before, after=after)
      rr <- list(dates = am$stratum2, cdates= paste(am$cmonth, am$cyear))
    } else if(period=="Week"){
      before=7
      after=0
      aw <- as.week(x, format = format, min.date = min.date, max.date = max.date, 
                    before = before, after = after, sunday = sunday, origin = origin)
      if (sunday) {
        firstday <- "Sunday"
      }
      else {
        firstday <- "Monday"
      }
      rr <- list(dates = aw$stratum2, cdates=aw$cweek)
      
    } else if(period=="Day"){
      before=7
      after=0
      dates0 <- as.Date(x, format = format)
      if (missing(min.date)) {
        min.date <- min(dates0, na.rm = TRUE) - before
      }
      if (missing(max.date)) {
        max.date <- max(dates0, na.rm = TRUE) + after
      }
      cdates <- seq(min.date, max.date, by = 1)
      dates <- factor(dates0, levels = as.character(cdates))
      rr <- list(dates=dates, cdates=cdates)
    } else if(period=="Quarter"){
      before=90
      after=0
      quarters0<-as.yearqtr(x)
      min.date<-min(x)-before
      max.date<-max(x)+after
      cquarters<-unique(as.yearqtr(seq(min.date, max.date, by=1)))
      quarters=factor(quarters0, levels=as.character(cquarters))
      rr <- list(dates=quarters, cdates=cquarters)      
    }else if(period=="Year"){
      before=1
      after=0
      years0<-year(x)
      min.date<-year(min(x))-before
      max.date<-year(max(x))+after
      cyears<-seq(min.date, max.date)
      years<-factor(years0, levels=cyears)
      rr<-list(dates=years, cdates=cyears)
    }
  } 

#----------------------------------------
# Server
#----------------------------------------

shinyServer(function(input, output, session) {
  values<-reactiveValues(starting=TRUE)
  session$onFlushed(function(){
    values$starting<-FALSE
  })
  
  
  #----------------------------------------
  # Input tab
  #----------------------------------------

  # Trigger display of variable definition dropdowns when data uploaded
  
  ## Cases
  caseDatUser<-reactive({
    if(is.null(input$fileCase)){
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  output$caseFileUploaded <- reactive({
    return(!is.null(caseDatUser()))
  })
  outputOptions(output, 'caseFileUploaded', suspendWhenHidden=FALSE)
  
  ## Venues 
  venueDatUser<-reactive({
    if(is.null(input$fileVenue)){
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  output$venueFileUploaded <- reactive({
    return(!is.null(venueDatUser()))
  })
  outputOptions(output, 'venueFileUploaded', suspendWhenHidden=FALSE)
  
  # Define data
  
  ## Cases
  caseDat<-reactiveValues()
  caseDat$data<-reactive({
    inFile <- input$fileCase
    if(is.null(inFile)){
      return(NULL) 
    } else {
      as.data.frame(read.csv(inFile$datapath, header=T, stringsAsFactors=F))
    }

  })
  
  ## Venues
  venueDat<-reactiveValues()
  venueDat$data<-reactive({
    inFile <- input$fileVenue
    if(is.null(inFile)){
      return(NULL) 
    } else {
      as.data.frame(read.csv(inFile$datapath, header=T, stringsAsFactors=F))
    }
  })
  
  # Display preview of data
  
  ## Cases
  output$previewCase<-DT::renderDataTable(
    if(!(is.null(input$fileCase))){
      DT::datatable(caseDat$data(), 
                    options=list(paging=FALSE, searching=FALSE))
    }
  )
  
  ## Venues
  output$previewVenue<-DT::renderDataTable(
    if(!(is.null(input$fileVenue))){
      DT::datatable(venueDat$data(), 
                    options=list(paging=FALSE, searching=FALSE))
    }
  )
  
  # Set columns with each data item
  
  ## Cases
  ### ID
  output$caseidUi<-renderUI({
      selectizeInput('caseid', label='Unique case identifier', 
                     choices=names(caseDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'caseidUi', suspendWhenHidden=FALSE)
  
  ### Date
  output$casedateUi<-renderUI({
    selectizeInput('casedate', label='Case date', 
                   choices=names(caseDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'casedateUi', suspendWhenHidden=FALSE)
  
  ### Cluster ID
  output$clusteridUi<-renderUI({
    selectizeInput('clusterid', label='Cluster identifier', 
                   choices=names(caseDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'clusteridUi', suspendWhenHidden=FALSE)
  
  ### Categorical variables  
  output$catvarUi<-renderUI({
    selectizeInput('catvars', label='Categorical variables (optional)', 
                   choices=names(caseDat$data()), multiple=T,
                   options=list(
                     placeholder="Select one or more variables",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, "catvarUi", suspendWhenHidden = FALSE)
  
  ### Latitude
  output$caselatUi<-renderUI({
    selectizeInput('caselat', label='Latitude', 
                   choices=names(caseDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'caselatUi', suspendWhenHidden=FALSE)
  
  ### Longitude
  output$caselonUi<-renderUI({
    selectizeInput('caselon', label='Longitude', 
                   choices=names(caseDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'caselonUi', suspendWhenHidden=FALSE)
  
  ## Venues
  ### ID
  output$venueidUi<-renderUI({
    selectizeInput('venueid', label='Unique venue identifier', 
                   choices=names(venueDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'venueidUi', suspendWhenHidden=FALSE)
  
  ### Type
  output$venuetypeUi<-renderUI({
    selectizeInput('venuetype', label='Type', 
                   choices=names(venueDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'venuetypeUi', suspendWhenHidden=FALSE)
  
  ### Name
  output$venuenameUi<-renderUI({
    selectizeInput('venuename', label='Name', 
                   choices=names(venueDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'venuenameUi', suspendWhenHidden=FALSE)
  
  ### Latitude
  output$venuelatUi<-renderUI({
    selectizeInput('venuelat', label='Latitude', 
                   choices=names(venueDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'venuelatUi', suspendWhenHidden=FALSE)
  
  ### Longitude
  output$venuelonUi<-renderUI({
    selectizeInput('venuelon', label='Longitude', 
                   choices=names(venueDat$data()),
                   options=list(
                     placeholder="Select variable",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  outputOptions(output, 'venuelonUi', suspendWhenHidden=FALSE)

  # Validate inputs - generate error messages
  ## Cases
  output$warn<-renderText({
    if(is.null(input$fileCase)){
      "Please select data"
    } else if(!is.null(input$fileCase)){
      validate(
        ### Case variables not selected
        need(input$caseid!="" & input$casedate!="" & input$caselat!="" &  input$caselon!="",
             "Please select variables"),
        ### Case same variable selected for >1 field
        if(input$caseid!="" & input$casedate!=""  & input$caselat!="" & input$caselon!=""){
          need(
            anyDuplicated(c(
              input$caseid, input$casedate, input$caselat, input$caselon, input$catvars
            )
            )==0, "Please select each variable only once")
        }
      )
      ## Venues
      if(!is.null(venueDatUser())){
        validate(
          ### Venues variables not selected
          need(input$venueid!="" & input$venuetype!="" & input$venuename!="" & input$venuelat!=""  & input$venuelon!="",
               "Please select venue variables"),
          
          ### Venues variable selected for >1 field 
          if(input$venueid!="" & input$venuetype!="" & input$venuename!="" & input$venuelat!=""  & input$venuelon!=""){
            need(
              anyDuplicated(c(
                input$venueid, input$venuetype, input$venuename, input$venuelat, input$venuelon
              ))==0, "Please select each variable only once")
          }
        )
      }
    } else { NULL}
  })
  
  # Disable generate plan button if validations not met
  ### this is ugly but toggleState can only be based on inputs so needs to react directly to the inputs
  ### rather than eg to a reactive value, as far as I can tell. Anyway it works!
  
  ## Cases
  observe({
    toggleState("gen", condition=
                  !is.null(input$fileCase)
    )
  })
  observe({
    if(!is.null(input$fileCase)){
      toggleState("gen", condition=
                    input$caseid!="" & input$casedate!="" & input$caselat!="" &  input$caselon!="" &
                    anyDuplicated(c(
                      input$caseid, input$casedate, input$caselat, input$caselon, input$catvars
                    )
                    )==0
      )
    }
  })
  
  ## Venues
  observe({
    if(!is.null(venueDatUser())){
      toggleState("gen", condition=
                    input$venueid!="" & input$venuetype!="" & input$venuename!="" & input$venuelat!="" & input$venuelon!="" &
                    anyDuplicated(c(
                      input$venueid, input$venuetype, input$venuename, input$venuelat, input$venuelon
                    ))==0
      )
    }
  })
  
  # Check that lat and lon are valid - if not warning that rows will be removed
  
  ## Cases
  output$caselatlon<-renderText({
    if(!is.null(input$caselat) & !is.null(input$caselon)){
      if(input$caselat!="" & input$caselon!=""){
        validate(
          need(all(caseDat$data()[,input$caselon]>=-180, na.rm=T) & all(caseDat$data()[,input$caselon]<=180, na.rm=T) &
                 all(caseDat$data()[,input$caselat]>=-90, na.rm=T) & all(caseDat$data()[,input$caselat]<=90, na.rm=T) &
                 any(is.na(caseDat$data()[,input$caselon]))==FALSE & any(is.na(caseDat$data()[,input$caselat]))==FALSE,
               "One or more rows have invalid latitude or longitude and will not be plotted on map"
          )
        )
      } else {NULL}
    } else {NULL}
  })
  
  ## Venues
  output$venuelatlon<-renderText({
    if(!is.null(input$venuelat) & !is.null(input$venuelon)){
      if(input$venuelat!="" & input$venuelon!=""){
        validate(
          need(all(venueDat$data()[,input$venuelon]>=-180, na.rm=T) & all(venueDat$data()[,input$venuelon]<=180, na.rm=T) &
                 all(venueDat$data()[,input$venuelat]>=-90, na.rm=T) & all(venueDat$data()[,input$venuelat]<=90, na.rm=T) &
                 any(is.na(venueDat$data()[,input$venuelon]))==FALSE & any(is.na(venueDat$data()[,input$venuelat]))==FALSE,
               "One or more rows have invalid latitude or longitude and will not be plotted on map"
          )
        )
      } else {NULL}
    } else {NULL}
  })
  

  #----------------------------------------
  # Go button
  #----------------------------------------
  
  # Disable other tabs until go button is pressed
  observe({
    toggleState(selector="#nav li a[data-value=navmap]", condition=input$gen!=0 )
    toggleState(selector="#nav li a[data-value=navtable]", condition=input$gen!=0 )
    toggleState(selector="#nav li a[data-value=navbar]", condition=input$gen!=0 )
  })
  
  # Observe event - go button 
  observeEvent({
    input$gen}, {
      
      # Move focus to map tab 
      updateTabsetPanel(session, "nav", selected = "navmap")
      
      # Rename and format variables for assigned columns
      ## Cases 
      caseDat$caseDatNam<-caseDat$data()
      colnames(caseDat$caseDatNam)[which(colnames(caseDat$caseDatNam)==input$caseid)]<-"id"
      colnames(caseDat$caseDatNam)[which(colnames(caseDat$caseDatNam)==input$casedate)]<-"date"
      colnames(caseDat$caseDatNam)[which(colnames(caseDat$caseDatNam)==input$caselat)]<-"lat" 
      colnames(caseDat$caseDatNam)[which(colnames(caseDat$caseDatNam)==input$caselon)]<-"lon" 
      
      caseDat$caseDatNam$date<-dmy(caseDat$caseDatNam$date)
      
      if(length(input$catvars)>=1){
        lapply(input$catvars, function(i){
          caseDat$caseDatNam[is.na(caseDat$caseDatNam[,i]),i]<<-"NA"
          caseDat$caseDatNam[,i]<<-as.factor(caseDat$caseDatNam[,i])
        })
      }
      
      ## Venues 
      if(!is.null(venueDatUser())){
        venueDat$venueDatNam<-venueDat$data()
        colnames(venueDat$venueDatNam)[which(colnames(venueDat$venueDatNam)==input$venueid)]<-"id"
        colnames(venueDat$venueDatNam)[which(colnames(venueDat$venueDatNam)==input$venuetype)]<-"type"
        colnames(venueDat$venueDatNam)[which(colnames(venueDat$venueDatNam)==input$venuename)]<-"name"  
        colnames(venueDat$venueDatNam)[which(colnames(venueDat$venueDatNam)==input$venuelat)]<-"lat" 
        colnames(venueDat$venueDatNam)[which(colnames(venueDat$venueDatNam)==input$venuelon)]<-"lon" 
        
        ## add string to venue id to make sure different to case
        venueDat$venueDatNam$id<-paste0("venueid", venueDat$venueDatNam$id)
        
      }
      
      # Update inputs
      ## Date slider
      output$dateRangeUi<-renderUI({
        sliderInput("dateRange", "Dates",
                    min=min(caseDat$caseDatNam$date),
                    max=max(caseDat$caseDatNam$date),
                    value=c(min(caseDat$caseDatNam$date),max(caseDat$caseDatNam$date)),
                    dragRange=T)
      })
      outputOptions(output, "dateRangeUi", suspendWhenHidden = FALSE)
      
      ## Colour variable
      output$varcolUi<-renderUI({
        selectInput("varcol", "Colour variable", choices=c(input$durChoice, input$catvars), selected=input$durChoice)
      })
      outputOptions(output, "varcolUi", suspendWhenHidden = FALSE)
      
      ## Filter variables
      output$filUi<-renderUI({
        if(length(input$catvars)>=1){
          lapply(input$catvars, function(i) {
            selectInput(
              inputId=paste0("grp",i), 
              label=i,
              choices=levels(caseDat$caseDatNam[,i]), 
              selected=levels(caseDat$caseDatNam[,i]), 
              multiple=T)
          })   
        } else {NULL}
      })
      outputOptions(output, "filUi", suspendWhenHidden = FALSE)
    })
  
  #----------------------------------------
  # Update filters if reset pressed 
  #----------------------------------------
  
  observeEvent(input$reset,{
    lapply(input$catvars, function(i) {
      updateSelectInput(session,
                        inputId=paste0("grp",i),
                        selected=levels(caseDat$caseDatNam[,i]))
    })
  })
  
  #----------------------------------------
  # Plotting data
  #----------------------------------------
  
  # Calc time interval groups
  caseTime<-reactive({
    caseTime1<-
      caseDat$caseDatNam %>%
      mutate(Year=timeGroups(date,period="Year")$dates, 
             Quarter=timeGroups(date,period="Quarter")$dates, 
             Month=timeGroups(date,period="Month")$dates, 
             Day=timeGroups(date,period="Day")$dates)
    
    levels(caseTime1$Month)<-timeGroups(caseTime1$date, period="Month")$cdates
    
    caseTime1
  })
  
  # Set colours
  caseCol<-reactive({
    caseCol1<-caseTime()
      suppressWarnings(
        colRamp<-colorRampPalette(brewer.pal(length(levels(caseCol1[,input$varcol])), "Set1"))
      )
      cols<-colRamp(length(levels(caseCol1[,input$varcol])))
      factpal <- colorFactor(cols, caseCol1[,input$varcol])
      caseCol1$col<- factpal(caseCol1[,input$varcol])
    caseCol1
  })
  
  # Filter data
  ## Groups
  caseFil<-reactive({
    caseFil1<-caseCol()
    if(length(input$catvars>=1)){
      filGrps<-lapply(input$catvars, function(j){
        caseFil1[,j]%in%input[[paste0("grp",j)]]           
      })
      filGrps<-Reduce("&", filGrps)  
      caseFil1<-caseFil1[which(filGrps),]
    }
    caseFil1<-caseFil1[which(caseFil1$date>=input$dateRange[1] &
                               caseFil1$date<=input$dateRange[2]),]
    as.data.frame(caseFil1)
  })
  
  ## Remove if no lat/ lon
  caseMap<-reactive({
    caseMap1<-caseFil()
    caseMap1 %>%
      filter(lat>=-90 & lat<=90 & lon>=-180 & lon<=180)
  })
 
  #----------------------------------------
  # Map
  #----------------------------------------
  
  # Render base map
  
  output$datamap<-renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
#      setView(lat=median(caseMap()$lat),lng=median(caseMap()$lon), zoom=9, options=list(maxZoom=11))
      setView(lat=median(caseDat$caseDatNam$lat),lng=median(caseDat$caseDatNam$lon), zoom=9, options=list(maxZoom=11))
  })
  
  # Add features to map
  
  observe({
    if(input$nav=="navmap"){
    datamap<-leafletProxy("datamap")
    ## Cases
    datamap %>%
      clearMarkers() %>%
      removeControl("plLeg") %>%
      addCircleMarkers(data=caseFil(), lng=~lon, lat=~lat, layerId=~id,
                       fillColor=~col, radius=8, color="black", 
                       weight=1, fillOpacity=1)

    ## Venues
    if(!is.null(venueDatUser()) & input$disContext=="Yes"){
      datamap %>% 
        addMarkers(data=venueDat$venueDatNam, lat=~lat, lng=~lon, layerId=~id)
    }
    ## legend
    if(length(input$catvars)>=1){
      if(input$varcol%in%c("Year", "Quarter", "Month", "Day")){
        datamap %>%
          addLegend(position="bottomright", colors=unique(caseCol()$col),
                    labels=unique(caseCol()[,input$varcol]), title=input$varcol, 
                    opacity=1, layerId="plLeg")
      } else {
        datamap %>%
          addLegend(position="bottomright", colors=unique(caseCol()$col),
                    labels=levels(caseCol()[,input$varcol]), title=input$varcol, 
                    opacity=1, layerId="plLeg")
      }
    }
    }
  })
  
  popContent<-function(ID){
    if(ID%in%caseFil()$id){
    datPop<-caseFil()
    selectedID <- datPop[datPop$id == ID,]
            conList<-
              lapply(input$catvars, function(l){
                list(tags$strong(l),         
                     selectedID[,l],
                     tags$br())
              })
    content<-as.character(
      tagList(
        tags$strong("ID"), selectedID$id, tags$br(),
        tags$strong("Date"), selectedID$date, tags$br(),
                    conList
      )
    )
    } else {
      selectedID <- venueDat$venueDatNam[venueDat$venueDatNam$id==ID,]
      content <- as.character(tagList(
        tags$strong("Name"), selectedID$name, tags$br(),
        tags$strong("Type"), selectedID$type, tags$br()
      ))
    }
    
    return(content) 
  }
  ## Add to map
  ### Also add to text box (index selector)
  observe({ 
    datamap<-leafletProxy("datamap")
    datamap %>% clearPopups()
    event<-input$datamap_marker_click
    if (is.null(event)){
      return()
    } else{
      isolate({
        datamap %>% addPopups(lng=event$lng, lat=event$lat, 
                          popup=popContent(event$id),
                          options=popupOptions(closeOnClick=T))
        
      })
    }
  })
  
  #----------------------------------------
  # Table
  #----------------------------------------
  
  # Summary of selected data
  
  sumDat<-reactive({
    sumDat1<-caseFil()
    if(length(input$catvars>=1)){
      suppressWarnings(
      sumDat1<-
        sumDat1 %>%
        select(input$durChoice, input$catvars) %>%
        gather(key=Variable, value=Group, na.rm=F) %>%
        group_by(Variable, Group) %>%
        summarise(Selected_cases_n=n(), Selected_cases_percent=round(Selected_cases_n/nrow(sumDat1)*100,2))
      )
    } else {
      suppressWarnings(
      sumDat1<-
        sumDat1 %>%
        select(input$durChoice) %>%
        gather(key=Variable, value=Group, na.rm=F) %>%
        group_by(Variable, Group) %>%
        summarise(Number_of_cases=n(), Cases_percent=round(Number_of_cases/nrow(sumDat1)*100,2))
      )
    }
    
    sumDat1
  })

  output$sumTable<-DT::renderDataTable({
    DT::datatable(sumDat(), 
                  options=list(paging=FALSE, searching=FALSE))
    })
  
  
  #----------------------------------------
  # Bar chart
  #----------------------------------------
  
  # Bar chart size
  output$epiUi<-renderUI({
    plotOutput("epi", width=input$plWid, height=input$plHt)
    
  })
  
  
  # Bar chart data
  
  barDat<-reactive({
    if(nrow(caseFil())>=1){
      data.frame(dates=caseFil()[,input$durChoice]) %>%
        group_by(dates) %>%
        summarise(N=length(dates)) %>%
        complete(dates) %>%
        mutate(N=ifelse(is.na(N),0,N), 
               lab=as.character(timeGroups(caseDat$caseDatNam$date,period=input$durChoice)$cdates))
    } else {NULL}
   
  })
  
  # Plot bar chart
  
  output$epi<-renderPlot({
    validate(need(nrow(barDat())>=1, "No data selected - check filters"))
    ## Use rotated labels if Day or Month or Quarter selected
    if(input$durChoice%in%c("Day", "Month", "Quarter")){
      pl<-
        ggplot()+
        geom_bar(data=barDat(),
                 aes(x=dates, y=N), 
                 stat="identity", width=1, fill="#3c8dbc", col="black")+
        scale_y_continuous(name="N", expand=c(0,0))+
        scale_x_discrete(expand=c(0,0), labels=barDat()$lab) +
        theme_bw()+
        theme(
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=8, angle=90))
    } else {
      
      ## Otherwise non-rotated
      pl<-
        ggplot()+
        geom_bar(data=barDat(),
                 aes(x=dates, y=N), 
                 stat="identity", width=1, fill="#3c8dbc", col="black")+
        scale_y_continuous(name="N", expand=c(0,0))+
        scale_x_discrete(expand=c(0,0), labels=barDat()$lab) +
        theme_bw()+
        theme(
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=8))
    }
    pl
#  })
  }, height=exprToFunction(input$plHt), width=exprToFunction(input$plWid))
})








