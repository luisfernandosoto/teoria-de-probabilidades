setwd("C:/Users/prestamo/Documents/TALLER R")
datos <- read.csv("C:/Users/prestamo/Documents/TALLER R/MobilesDataSet.csv")
View(datos)
library(ggplot2)
iphone <- data.frame(
  Precio_USD = c(799, 849, 899, 899, 949, 999, 999, 1049, 1099, 1099, 1199, 1299)
)
ggplot(iphone, aes(x = Precio_USD)) +
  geom_bar(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Precios iPhone 16", x = "USD", y = "Frecuencia") +
  theme_minimal()






datos <- data.frame(
  Serie = c("Estándar", "Plus", "Pro", "Pro Max"),
  Precio = c(849, 949, 1049, 1199)
)
ggplot(datos, aes(x = Serie, y = Precio, fill = Serie)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", Precio)), vjust = -0.5) +
  labs(title = "Precio Promedio por Serie", y = "USD") +
  theme_minimal() +
  theme(legend.position = "none")

datos_telefonos <- data.frame(
  CompanyName = c(rep("Apple", 15)),
  ModelName = c("iPhone 16", "iPhone 16 256", "iPhone 16 512", 
                "iPhone 16 Plus", "iPhone 16 Plus 256", "iPhone 16 Plus 512",
                "iPhone 16 Pro", "iPhone 16 Pro 256", "iPhone 16 Pro 512",
                "iPhone 16 Pro 221", "iPhone 16 Pro 221 256", "iPhone 16 Pro 221 512",
                "iPhone 15", "iPhone 15 256", "iPhone 15 512"),
  Weight = c(174, 174, 174, 203, 203, 203, 206, 206, 206, 221, 221, 221, 171, 171, 171),
  RAM = c(6, 6, 6, 6, 6, 6, 6, 8, 8, 6, 8, 8, 6, 6, 6),
  FrontCamera = c(rep("12MP", 6), rep("12MP / 4K", 6), rep("12MP", 3)),
  BackCamera = c(rep("48MP", 6), 
                 "50MP + 12MP", "50MP + 12MP", "50MP + 12MP", 
                 "48MP + 12MP", "48MP + 12MP", "48MP + 12MP",
                 "48MP", "48MP", "48MP"),
  Processor = c(rep("A17 Bionic", 6), rep("A17 Pro", 6), rep("A16 Bionic", 3)),
  BatteryCapacity = c(rep(3600, 3), rep(4200, 3), rep(4400, 3), rep(4500, 3), rep(3200, 3)),
  ScreenSize = c(rep("6.1 inches", 3), rep("6.7 inches", 3), rep("6.1 inches", 3), rep("6.7 inches", 3), rep("6.1 inches", 3)),
  PricePKR = c(224999, 234999, 244999, 249999, 259999, 274999, 284999, 294999, 314999, 314999, 324999, 344999, 199999, 209999, 219999),
  PriceINR = c(79999, 84999, 89999, 89999, 94999, 104999, 99999, 104999, 114999, 109999, 114999, 124999, 69999, 74999, 79999),
  PriceCNY = c(5799, 6099, 6499, 6199, 6499, 6999, 6999, 7099, 7499, 7499, 7799, 8199, 5299, 5599, 5999),
  PriceUSD = c(799, 849, 899, 899, 949, 999, 999, 1049, 1099, 1099, 1199, 1299, 699, 749, 799),
  PriceAED = c(2799, 2999, 3199, 3199, 3399, 3599, 3499, 3699, 3899, 3799, 3999, 4199, 2499, 2699, 2899),
  LaunchedYear = rep(2024, 15),
  Storage = c("128GB", "256GB", "512GB", "128GB", "256GB", "512GB", "128GB", "256GB", "512GB", "128GB", "256GB", "512GB", "128GB", "256GB", "512GB")
)

datos_telefonos$Storage <- factor(datos_telefonos$Storage, levels = c("128GB", "256GB", "512GB"))

ggplot(datos_telefonos, aes(x = Storage, y = PriceUSD, color = ModelName, group = ModelName)) +
  geom_line() +
  geom_point() +
  labs(title = "Precio por Almacenamiento",
       x = "Almacenamiento", y = "USD") +
  theme_minimal()



telefonos =`names<-`( Fabricante = "CompanyName",
    Modelo = "ModelName")


samsung <- telefonos +
  filter(CompanyName == "Samsung") +
  arrange(desc(`Launched Price USD`)) +
  head(10)  


ggplot(samsung, aes(x = reorder(Modelo, -`Launched Price USD`), y = `Launched Price USD`)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +
  coord_flip() +
  labs(title = "Top 10 Modelos Samsung por Precio",
       x = "",
       y = "Precio (USD)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format())telefonos <- telefonos %>%
  rename(
    Fabricante = `CompanyName`,
    Modelo = `ModelName`
  )


samsung <- data.frame((
  Serie = c("Z Fold", "Z Flip", "S22", "A5x", "A3x", "A2x", "A1x"),
  Precio = c(1850, 1050, 900, 425, 375, 325, 275))

ggplot(samsung, aes(x = reorder(Serie, -Precio), y = Precio)) +
  geom_col(fill = "#1428A0") +  
  geom_text(aes(label = paste0("$", Precio)), vjust = -0.5) +
  labs(title = "Precios promedio por serie de Samsung", x = "", y = "") +
  theme_minimal()


almacenamiento <- data.frame(
  Capacidad = c("128GB", "256GB", "512GB"),
  Precio = c(650, 850, 1400)
)

ggplot(almacenamiento, aes(x = Capacidad, y = Precio)) +
  geom_bar(stat = "identity", fill = "#5CC7F8") +
  geom_text(aes(label = paste0("$", Precio)), vjust = -0.5) +
  labs(title = "Precio promedio por capacidad de almacenamiento", x = "", y = "") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1600))


