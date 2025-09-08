library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(dplyr)

# Initialize data storage (in production, use a proper database)
users_data <- data.frame(
  id = 1:5,
  username = c("alice.smith", "bob.jones", "carol.white", "david.brown", "eve.davis"),
  display_name = c("Alice Smith", "Bob Jones", "Carol White", "David Brown", "Eve Davis"),
  status = c("online", "online", "away", "busy", "offline"),
  avatar = c("👩‍💼", "👨‍💻", "👩‍🎨", "👨‍🔬", "👩‍⚕️"),
  stringsAsFactors = FALSE
)

teams_data <- data.frame(
  id = 1:3,
  name = c("Development Team", "Marketing Team", "HR Department"),
  description = c("Software development discussions", "Marketing campaigns and strategies", "Human resources matters"),
  members = c("1,2,4", "2,3,5", "1,3,5"),
  stringsAsFactors = FALSE
)

# Initialize message storage
messages_data <- data.frame(
  id = integer(),
  sender_id = integer(),
  recipient_type = character(), # 'team' or 'individual'
  recipient_id = integer(),
  message = character(),
  timestamp = character(),
  read_by = character(),
  stringsAsFactors = FALSE
)

notifications_data <- data.frame(
  id = integer(),
  user_id = integer(),
  message = character(),
  timestamp = character(),
  read = logical(),
  stringsAsFactors = FALSE
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Internal Communication Hub",
    dropdownMenu(
      type = "notifications",
      headerText = "Notifications",
      icon = icon("bell"),
      badgeStatus = "warning",
      notificationItem(
        text = "New message from Alice",
        icon = icon("envelope")
      ),
      notificationItem(
        text = "Team meeting in 30 mins",
        icon = icon("calendar")
      )
    )
  ),
  
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      # User Profile Section
      div(
        style = "padding: 15px; border-bottom: 1px solid #ddd; margin-bottom: 10px;",
        div(
          style = "text-align: center;",
          span("👤", style = "font-size: 30px;"),
          br(),
          strong("Current User"),
          br(),
          span("alice.smith", style = "color: #666; font-size: 12px;"),
          br(),
          div(
            style = "margin-top: 8px;",
            selectInput("user_status", NULL, 
                        choices = c("🟢 Available" = "online", 
                                    "🟡 Away" = "away", 
                                    "🔴 Busy" = "busy", 
                                    "⚫ Invisible" = "offline"),
                        selected = "online", width = "100%")
          )
        )
      ),
      
      menuItem("Teams", tabName = "teams", icon = icon("users")),
      menuItem("Direct Messages", tabName = "direct", icon = icon("comment")),
      menuItem("Notifications", tabName = "notifications", icon = icon("bell"),
               badgeLabel = "3", badgeColor = "red")
    ),
    
    # Teams List
    div(
      id = "teams_sidebar",
      style = "padding: 10px;",
      h5("Teams", style = "color: #666; margin-bottom: 10px;"),
      actionButton("team_1", "💻 Development Team", 
                   style = "width: 100%; text-align: left; margin-bottom: 5px; background: none; border: none;"),
      actionButton("team_2", "📈 Marketing Team", 
                   style = "width: 100%; text-align: left; margin-bottom: 5px; background: none; border: none;"),
      actionButton("team_3", "👥 HR Department", 
                   style = "width: 100%; text-align: left; margin-bottom: 5px; background: none; border: none;")
    ),
    
    # Online Users
    div(
      style = "padding: 10px; border-top: 1px solid #ddd;",
      h5("Online Now", style = "color: #666; margin-bottom: 10px;"),
      div(id = "online_users",
          actionButton("user_2", "👨‍💻 Bob Jones", 
                       style = "width: 100%; text-align: left; margin-bottom: 5px; background: none; border: none;"),
          actionButton("user_3", "👩‍🎨 Carol White", 
                       style = "width: 100%; text-align: left; margin-bottom: 5px; background: none; border: none;")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f8f9fa; }
        .chat-container { 
          height: 500px; 
          overflow-y: auto; 
          background: white; 
          border: 1px solid #ddd; 
          border-radius: 5px; 
          padding: 15px; 
          margin-bottom: 10px;
        }
        .message { 
          margin-bottom: 15px; 
          padding: 10px; 
          border-radius: 8px; 
          max-width: 70%; 
        }
        .message-sent { 
          background: #0078d4; 
          color: white; 
          margin-left: auto; 
          text-align: right; 
        }
        .message-received { 
          background: #f3f2f1; 
          color: #323130; 
        }
        .message-header { 
          font-weight: bold; 
          margin-bottom: 5px; 
          font-size: 12px; 
        }
        .message-time { 
          font-size: 11px; 
          opacity: 0.7; 
          margin-top: 5px; 
        }
        .typing-indicator { 
          font-style: italic; 
          color: #666; 
          margin-bottom: 10px; 
        }
        .notification-item {
          background: white;
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 10px;
          margin-bottom: 10px;
        }
        .notification-unread {
          border-left: 4px solid #0078d4;
          background: #f8f9ff;
        }
      "))
    ),
    
    tabItems(
      # Teams Tab
      tabItem(
        tabName = "teams",
        fluidRow(
          column(8,
                 box(
                   title = "Team Chat", status = "primary", solidHeader = TRUE,
                   width = NULL, height = "600px",
                   
                   div(
                     style = "margin-bottom: 10px;",
                     selectInput("selected_team", "Select Team:", 
                                 choices = setNames(1:3, c("💻 Development Team", "📈 Marketing Team", "👥 HR Department")),
                                 selected = 1, width = "300px")
                   ),
                   
                   div(id = "team_chat", class = "chat-container",
                       div(class = "message message-received",
                           div(class = "message-header", "👨‍💻 Bob Jones"),
                           "Hey team! How's the new feature coming along?",
                           div(class = "message-time", "10:30 AM")
                       ),
                       div(class = "message message-sent",
                           div(class = "message-header", "You"),
                           "Making good progress! Should be ready for testing by tomorrow.",
                           div(class = "message-time", "10:32 AM")
                       ),
                       div(class = "message message-received",
                           div(class = "message-header", "👨‍🔬 David Brown"),
                           "Great! Let me know if you need any help with the backend integration.",
                           div(class = "message-time", "10:35 AM")
                       )
                   ),
                   
                   div(
                     style = "display: flex; gap: 10px;",
                     textAreaInput("team_message", NULL, 
                                   placeholder = "Type your message...", 
                                   rows = 2, width = "100%"),
                     div(
                       style = "display: flex; flex-direction: column; gap: 5px;",
                       actionButton("send_team_message", "Send", 
                                    class = "btn-primary", style = "width: 80px;"),
                       actionButton("attach_file", "📎", 
                                    style = "width: 40px; height: 35px;")
                     )
                   )
                 )
          ),
          
          column(4,
                 box(
                   title = "Team Members", status = "info", solidHeader = TRUE,
                   width = NULL,
                   
                   div(id = "team_members",
                       div(style = "padding: 10px; border-bottom: 1px solid #eee;",
                           span("👤", style = "margin-right: 10px; font-size: 20px;"),
                           strong("Alice Smith (You)"),
                           span("🟢", style = "float: right;")
                       ),
                       div(style = "padding: 10px; border-bottom: 1px solid #eee;",
                           span("👨‍💻", style = "margin-right: 10px; font-size: 20px;"),
                           strong("Bob Jones"),
                           span("🟢", style = "float: right;")
                       ),
                       div(style = "padding: 10px; border-bottom: 1px solid #eee;",
                           span("👨‍🔬", style = "margin-right: 10px; font-size: 20px;"),
                           strong("David Brown"),
                           span("🔴", style = "float: right;")
                       )
                   )
                 ),
                 
                 box(
                   title = "Quick Actions", status = "warning", solidHeader = TRUE,
                   width = NULL,
                   
                   actionButton("schedule_meeting", "📅 Schedule Meeting", 
                                class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
                   actionButton("share_screen", "🖥️ Share Screen", 
                                class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
                   actionButton("team_call", "📞 Start Team Call", 
                                class = "btn-warning", style = "width: 100%;")
                 )
          )
        )
      ),
      
      # Direct Messages Tab
      tabItem(
        tabName = "direct",
        fluidRow(
          column(4,
                 box(
                   title = "Conversations", status = "primary", solidHeader = TRUE,
                   width = NULL, height = "600px",
                   
                   div(id = "conversations_list",
                       actionButton("conv_bob", 
                                    div(
                                      div(style = "display: flex; align-items: center;",
                                          span("👨‍💻", style = "margin-right: 10px; font-size: 20px;"),
                                          div(
                                            strong("Bob Jones"),
                                            br(),
                                            span("Thanks for the help!", style = "color: #666; font-size: 12px;")
                                          ),
                                          span("🟢", style = "margin-left: auto;")
                                      )
                                    ),
                                    style = "width: 100%; text-align: left; padding: 15px; margin-bottom: 10px; background: white; border: 1px solid #ddd;"),
                       
                       actionButton("conv_carol", 
                                    div(
                                      div(style = "display: flex; align-items: center;",
                                          span("👩‍🎨", style = "margin-right: 10px; font-size: 20px;"),
                                          div(
                                            strong("Carol White"),
                                            br(),
                                            span("Can we review the designs?", style = "color: #666; font-size: 12px;")
                                          ),
                                          span("🟡", style = "margin-left: auto;")
                                      )
                                    ),
                                    style = "width: 100%; text-align: left; padding: 15px; margin-bottom: 10px; background: white; border: 1px solid #ddd;"),
                       
                       actionButton("conv_david", 
                                    div(
                                      div(style = "display: flex; align-items: center;",
                                          span("👨‍🔬", style = "margin-right: 10px; font-size: 20px;"),
                                          div(
                                            strong("David Brown"),
                                            br(),
                                            span("Database is ready", style = "color: #666; font-size: 12px;")
                                          ),
                                          span("🔴", style = "margin-left: auto;")
                                      )
                                    ),
                                    style = "width: 100%; text-align: left; padding: 15px; background: white; border: 1px solid #ddd;")
                   )
                 )
          ),
          
          column(8,
                 box(
                   title = textOutput("dm_chat_title"), status = "info", solidHeader = TRUE,
                   width = NULL, height = "600px",
                   
                   div(id = "dm_chat", class = "chat-container",
                       div(class = "message message-received",
                           div(class = "message-header", "👨‍💻 Bob Jones"),
                           "Hi Alice! Could you help me with the API integration?",
                           div(class = "message-time", "2:15 PM")
                       ),
                       div(class = "message message-sent",
                           div(class = "message-header", "You"),
                           "Of course! What specific issue are you running into?",
                           div(class = "message-time", "2:16 PM")
                       ),
                       div(class = "message message-received",
                           div(class = "message-header", "👨‍💻 Bob Jones"),
                           "I'm getting a 401 error when trying to authenticate. Here's the error message...",
                           div(class = "message-time", "2:18 PM")
                       ),
                       div(class = "message message-sent",
                           div(class = "message-header", "You"),
                           "Thanks for the help! That solved the issue.",
                           div(class = "message-time", "3:45 PM")
                       ),
                       div(class = "typing-indicator", "👨‍💻 Bob is typing...")
                   ),
                   
                   div(
                     style = "display: flex; gap: 10px;",
                     textAreaInput("dm_message", NULL, 
                                   placeholder = "Type a message...", 
                                   rows = 2, width = "100%"),
                     div(
                       style = "display: flex; flex-direction: column; gap: 5px;",
                       actionButton("send_dm_message", "Send", 
                                    class = "btn-primary", style = "width: 80px;"),
                       actionButton("attach_dm_file", "📎", 
                                    style = "width: 40px; height: 35px;")
                     )
                   )
                 )
          )
        )
      ),
      
      # Notifications Tab
      tabItem(
        tabName = "notifications",
        fluidRow(
          column(12,
                 box(
                   title = "Notifications Center", status = "warning", solidHeader = TRUE,
                   width = NULL,
                   
                   div(
                     style = "margin-bottom: 20px;",
                     actionButton("mark_all_read", "Mark All as Read", class = "btn-info"),
                     actionButton("clear_notifications", "Clear All", class = "btn-danger", style = "margin-left: 10px;")
                   ),
                   
                   div(id = "notifications_list",
                       div(class = "notification-item notification-unread",
                           div(style = "display: flex; align-items: center;",
                               span("💬", style = "margin-right: 10px; font-size: 20px;"),
                               div(
                                 strong("New message from Bob Jones"),
                                 br(),
                                 span("In Development Team: 'Thanks for the help!'", style = "color: #666;"),
                                 br(),
                                 span("5 minutes ago", style = "color: #999; font-size: 12px;")
                               ),
                               actionButton("notif_1", "View", class = "btn-sm btn-primary", style = "margin-left: auto;")
                           )
                       ),
                       
                       div(class = "notification-item notification-unread",
                           div(style = "display: flex; align-items: center;",
                               span("📅", style = "margin-right: 10px; font-size: 20px;"),
                               div(
                                 strong("Meeting Reminder"),
                                 br(),
                                 span("Weekly standup in 30 minutes", style = "color: #666;"),
                                 br(),
                                 span("25 minutes ago", style = "color: #999; font-size: 12px;")
                               ),
                               actionButton("notif_2", "Join", class = "btn-sm btn-success", style = "margin-left: auto;")
                           )
                       ),
                       
                       div(class = "notification-item notification-unread",
                           div(style = "display: flex; align-items: center;",
                               span("👥", style = "margin-right: 10px; font-size: 20px;"),
                               div(
                                 strong("Carol White mentioned you"),
                                 br(),
                                 span("In Marketing Team: '@alice Can you review this?'", style = "color: #666;"),
                                 br(),
                                 span("1 hour ago", style = "color: #999; font-size: 12px;")
                               ),
                               actionButton("notif_3", "View", class = "btn-sm btn-primary", style = "margin-left: auto;")
                           )
                       ),
                       
                       div(class = "notification-item",
                           div(style = "display: flex; align-items: center;",
                               span("📁", style = "margin-right: 10px; font-size: 20px;"),
                               div(
                                 strong("File shared by David Brown"),
                                 br(),
                                 span("database_schema.sql uploaded to Development Team", style = "color: #666;"),
                                 br(),
                                 span("2 hours ago", style = "color: #999; font-size: 12px;")
                               ),
                               actionButton("notif_4", "Download", class = "btn-sm btn-info", style = "margin-left: auto;")
                           )
                       )
                   )
                 )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store app state
  values <- reactiveValues(
    current_team = 1,
    current_dm_user = "Bob Jones",
    messages = messages_data,
    notifications = notifications_data,
    typing_users = character()
  )
  
  # Dynamic chat title for DM
  output$dm_chat_title <- renderText({
    paste("Chat with", values$current_dm_user)
  })
  
  # Team message sending
  observeEvent(input$send_team_message, {
    if(input$team_message != "") {
      # Add message to chat (in production, save to database)
      new_message <- data.frame(
        id = nrow(values$messages) + 1,
        sender_id = 1, # Current user ID
        recipient_type = "team",
        recipient_id = values$current_team,
        message = input$team_message,
        timestamp = format(Sys.time(), "%I:%M %p"),
        read_by = "1",
        stringsAsFactors = FALSE
      )
      values$messages <- rbind(values$messages, new_message)
      
      # Clear input
      updateTextAreaInput(session, "team_message", value = "")
      
      # Add message to UI (simplified)
      insertUI(
        selector = "#team_chat",
        where = "beforeEnd",
        ui = div(class = "message message-sent",
                 div(class = "message-header", "You"),
                 input$team_message,
                 div(class = "message-time", format(Sys.time(), "%I:%M %p"))
        )
      )
      
      # Send notifications to team members
      team_members <- c("Bob Jones", "David Brown") # Simplified
      for(member in team_members) {
        new_notif <- data.frame(
          id = nrow(values$notifications) + 1,
          user_id = which(users_data$display_name == member),
          message = paste("New message from Alice in", teams_data$name[values$current_team]),
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          read = FALSE,
          stringsAsFactors = FALSE
        )
        values$notifications <- rbind(values$notifications, new_notif)
      }
    }
  })
  
  # DM message sending
  observeEvent(input$send_dm_message, {
    if(input$dm_message != "") {
      # Add message to chat
      insertUI(
        selector = "#dm_chat",
        where = "beforeEnd",
        ui = div(class = "message message-sent",
                 div(class = "message-header", "You"),
                 input$dm_message,
                 div(class = "message-time", format(Sys.time(), "%I:%M %p"))
        )
      )
      
      # Clear input
      updateTextAreaInput(session, "dm_message", value = "")
      
      # Send notification to recipient
      recipient_id <- which(users_data$display_name == values$current_dm_user)
      new_notif <- data.frame(
        id = nrow(values$notifications) + 1,
        user_id = recipient_id,
        message = paste("New direct message from Alice"),
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        read = FALSE,
        stringsAsFactors = FALSE
      )
      values$notifications <- rbind(values$notifications, new_notif)
    }
  })
  
  # Team selection
  observeEvent(input$selected_team, {
    values$current_team <- as.numeric(input$selected_team)
  })
  
  # Direct message conversation selection
  observeEvent(input$conv_bob, {
    values$current_dm_user <- "Bob Jones"
  })
  
  observeEvent(input$conv_carol, {
    values$current_dm_user <- "Carol White"
  })
  
  observeEvent(input$conv_david, {
    values$current_dm_user <- "David Brown"
  })
  
  # Notification actions
  observeEvent(input$mark_all_read, {
    # Mark all notifications as read
    runjs("$('.notification-unread').removeClass('notification-unread');")
  })
  
  observeEvent(input$clear_notifications, {
    # Clear all notifications
    runjs("$('#notifications_list').html('<p>No notifications</p>');")
  })
  
  # Quick actions
  observeEvent(input$schedule_meeting, {
    showModal(modalDialog(
      title = "Schedule Meeting",
      "Meeting scheduler would open here (integrate with calendar system)",
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$team_call, {
    showModal(modalDialog(
      title = "Start Team Call",
      "Video call interface would launch here (integrate with video conferencing)",
      footer = modalButton("Close")
    ))
  })
  
  # File attachment handlers
  observeEvent(input$attach_file, {
    showModal(modalDialog(
      title = "Attach File",
      fileInput("file_upload", "Choose file to upload"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("upload_confirm", "Upload", class = "btn-primary")
      )
    ))
  })
  
  # Simulate typing indicators (in production, use WebSocket or polling)
  observe({
    invalidateLater(10000) # Update every 10 seconds
    # Randomly show/hide typing indicators for demo purposes
  })
}

shinyApp(ui = ui, server = server)