# Programming

# "While" loop ----

i <- 3
while(i < 12){
  print(i)
  i <- i + 1
}


# "For" loop ----

vector <- c(2, 3, 5, 7, 11, 13)

for (gezen in c(3,5)) {
  print(vector[gezen])
}


df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df[2]

df[[2]]

df[[2]][4]


# median(df[["a"]])
# median(df[["b"]])
# median(df[["c"]])
# median(df[["d"]])

colnames <- names(df)

for (i in colnames) {
  print(i)
  print(median(df[[i]]))
}


# "If" statement ----

x = 2
y = 3

if (x > y){
  print('x is greater than y')
} else if (x < y) {
  print('y is greater than x')
} else{
  print('x is equal to y')
}



for (r in 1:nrow(df)) {
  if(df[[1]][r]>0){
    print(r) # print(paste0(r," - row"))
    print(df[[1]][r])
  }
}


for (c in 1:ncol(df)) {
  for (r in 1:nrow(df)) {
    if(df[[c]][r]>0){
      print(paste(c,"- column"))
      print(paste(r,"- row"))
      print(df[[c]][r])
    }
  }
}


df$new <- ifelse(df$d>0,"+","-")

df$new <- ifelse(df$d>0,"+",ifelse(df$d==0,"0","-"))


# Write own function ----

# Function components
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

avg(1:100)


# First-class functions
avg1 <- function(y, calc = TRUE){
  n <- length(y)
  ifelse(calc, sum(y)/n, sum(y)+350)
}

y <- 1:100
avg1(y,calc = T)
avg1(y,calc = F)


condition <- function(x,y){
  if (x > y){
    print('x is greater than y')
  }
  else if (x < y) {
    print('y is greater than x')
  }
  else{
    print('x is equal to y')
  }
}

condition(2,3)


formals(condition)
body(condition)



# Implicit versus explicit returns
t01 <- function(x) {
  if (x < 10) {
    return(0)
  } else {
    return(10)
  }
}


t02 <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}


t01(5)
t02(15)
