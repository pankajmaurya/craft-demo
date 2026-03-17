library(dplyr)
library(lubridate)
library(tidyr)

set.seed(42)

# -----------------------------
# 1. Define categories & products
# -----------------------------
categories <- list(
  Books = c("Novel", "Comics", "Textbook", "Biography", "Magazine", "Poetry"),
  Gadgets = c("Phone", "Laptop", "Tablet", "Headphones", "Smartwatch", "Camera", "Speaker"),
  Kitchen = c("Mixer", "Cooker", "KnifeSet", "Pan", "Blender", "Toaster", "Microwave")
)

product_df <- stack(categories) %>%
  rename(product = values, category = ind)

# -----------------------------
# 2. Create date range (2 years)
# -----------------------------
dates <- seq.Date(from = as.Date("2023-01-01"),
                  to = as.Date("2024-12-31"),
                  by = "day")

data <- expand.grid(date = dates, product = product_df$product) %>%
  left_join(product_df, by = "product")

# -----------------------------
# 3. Add base demand per product
# -----------------------------
base_demand <- data.frame(
  product = product_df$product,
  base = runif(nrow(product_df), 20, 200)
)

data <- data %>%
  left_join(base_demand, by = "product")

# -----------------------------
# 4. Add seasonality & trend
# -----------------------------
data <- data %>%
  mutate(
    t = as.numeric(date - min(date)),
    
    # Trend (slight growth)
    trend = 0.01 * t,
    
    # Weekly seasonality (weekends higher)
    weekly = ifelse(wday(date) %in% c(1,7), 1.3, 1.0),
    
    # Yearly seasonality (festive spikes)
    yearly = 1 + 0.2 * sin(2 * pi * yday(date) / 365),
    
    # Random promotions (10% days)
    promo = rbinom(n(), 1, 0.1),
    
    promo_effect = ifelse(promo == 1, runif(n(), 1.2, 1.8), 1),
    
    # Price variation
    price = case_when(
      category == "Books" ~ runif(n(), 200, 800),
      category == "Gadgets" ~ runif(n(), 5000, 50000),
      category == "Kitchen" ~ runif(n(), 500, 5000)
    ),
    
    # Noise
    noise = rnorm(n(), 0, 10)
  )

# -----------------------------
# 5. Generate final sales
# -----------------------------
data <- data %>%
  mutate(
    sales = base * weekly * yearly * promo_effect + trend + noise,
    sales = pmax(round(sales), 0)  # no negative sales
  )

# -----------------------------
# 6. Add category-level shocks
# -----------------------------
data <- data %>%
  mutate(
    shock = case_when(
      category == "Gadgets" & month(date) %in% c(10,11) ~ 1.5,  # festive spike
      category == "Kitchen" & month(date) == 5 ~ 1.3,           # summer demand
      TRUE ~ 1
    ),
    sales = round(sales * shock)
  )

# -----------------------------
# 7. Final dataset
# -----------------------------
final_data <- data %>%
  select(date, category, product, sales, price, promo)

# Save
write.csv(final_data, "simple_synthetic_sales_data.csv", row.names = FALSE)

# Preview
head(final_data)