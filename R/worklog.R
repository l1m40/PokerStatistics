# File:   
# 

# INSTALL AND LOAD PACKAGES ################################
#sessionInfo()
#Sys.getlocale()

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,tidyverse,rio,lubridate,grid,gridExtra) 








poker_cards <- factor(c("A","K","Q","10","9","8","7","6","5","4","3","2"),ordered=T)
assign_card_values <- function(card) { return(1-(match(card,poker_cards)-1)/(length(poker_cards)-1)) }

initial_poker_hands <- expand.grid(card_1=poker_cards,card_2=poker_cards) %>% as_tibble()
initial_poker_hands$card_1 <- factor(initial_poker_hands$card_1,levels=poker_cards)
initial_poker_hands$card_2 <- factor(initial_poker_hands$card_2,levels=poker_cards)
initial_poker_hands$card_1_value <- sapply(initial_poker_hands$card_1,assign_card_values)
initial_poker_hands$card_2_value <- sapply(initial_poker_hands$card_2,assign_card_values)
initial_poker_hands$hand_value <- initial_poker_hands$card_1_value*initial_poker_hands$card_2_value
initial_poker_hands$hand_value <- ifelse(initial_poker_hands$card_1_value==initial_poker_hands$card_2_value,(initial_poker_hands$hand_value+.5),initial_poker_hands$hand_value)
initial_poker_hands$hand_value <- ifelse(initial_poker_hands$card_1_value>initial_poker_hands$card_2_value,(initial_poker_hands$hand_value+.3),initial_poker_hands$hand_value)
#initial_poker_hands$hand_label <- str_pad(paste0(initial_poker_hands$card_1,initial_poker_hands$card_2,ifelse()),width=4,side="both")

initial_poker_hands <- initial_poker_hands %>% 
  mutate(hand_label=str_pad(paste0(card_1,card_2,ifelse(card_1==card_2,"",ifelse(card_1>card_2,"o","s"))),width=12,side="both"))

initial_poker_hands <- initial_poker_hands %>% 
  mutate(card_diff=abs(match(card_1,poker_cards)-match(card_2,poker_cards))) %>% 
  mutate(card_diff=ifelse(card_1=="A",pmin(card_diff,abs(13-match(card_2,poker_cards))),card_diff)) %>% 
  mutate(card_diff=ifelse(card_2=="A",pmin(card_diff,abs(13-match(card_1,poker_cards))),card_diff)) %>% 
  mutate(card_diff_value=ifelse(card_diff>5,0,.4-card_diff*0.05)) %>% 
  mutate(hand_value=hand_value+card_diff_value)

initial_poker_hands %>% 
  ggplot(aes(card_1,card_2))+geom_label(aes(label=hand_label,fill=hand_value),color="gray55",label.size=NA,label.r=unit(0,"lines"))+
  scale_fill_gradient2(midpoint=.5,high="green",mid="yellow3",low="red4")+
  labs(title="Initial Poker Hand Value")+
  theme_void()+theme(legend.position="none",plot.title=element_text(hjust=.5))


expand.grid(card_1=poker_cards,card_2=poker_cards) %>% as_tibble() %>% 
  mutate(hand=str_pad(paste0(card_1,card_2),width=3)) %>% 
  mutate(card_diff=abs(match(card_1,poker_cards)-match(card_2,poker_cards))) %>% 
  mutate(card_diff=ifelse(card_1=="A",pmin(card_diff,abs(13-match(card_2,poker_cards))),card_diff)) %>% 
  mutate(card_diff=ifelse(card_2=="A",pmin(card_diff,abs(13-match(card_1,poker_cards))),card_diff)) %>% 
  mutate(card_diff_value=ifelse(card_diff==0 | card_diff>5,0,.4-card_diff*0.05))

card="K"
# Function to assign values to cards
  # Define card ranks
  card_ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  
  # Linearly interpolate card value between 0 and 1
  card_value <- (match(card, card_ranks) - 1) / (length(card_ranks) - 1)
  
  return(card_value)
}

# Assign numerical values to each card
card_numerical_values <- sapply(poker_cards, assign_card_values)



x





# CLEAN UP #################################################
# sessionInfo()
# Clear environment
rm(list = ls()) 
# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base
# Clear plots
dev.off()  # But only if there IS a plot
# Clear console
cat("\014")  # ctrl+L

# Clear mind :)



